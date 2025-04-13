;Asm Bible Read v6
;
; done a fancy double menu
; got 70 bytes free (technically) 4 bytes now... CFFB
;  i can shift word banks around for more space at $c000
;  also some upper mem free, and some in casette buffer
;
; did a little dramatic intro effect, cycling the colors 
; 12 bytes free! :-O
;
; how to compile this
; first do the upper.asm program for the upper banks
; compile this program from CBM studio to get the PRG file
;
; run exomizer: exomizer.exe sfx $c9d7 asmbibleread.prg
; copy the output file from exomizer into a disk with the upper.prg
; and have a book to read on same disk. Default filename "book1"
;
;
;Asm Bible Read v5
;
; todo
;  make a keyboard pause every N line... done
;
;
;Asm Bible Read v4
;
;  fixed the case bug
;
;  todo
;  fix the space after special chars bug - done

;  handle special chars - done(?)
;
;
;Asm Bible Read v3
;  moved all variables to tapebuffer (safe)
;  still get random crashes...
;
;  Set a SEI in the start, which was wrong?
;  now i only do SEI when dealing with ram under ROM, and CLIs afterwards
;  seems to be running...forever ! :D damn bug!
;
;  todo
;  handle special chars
;
;
;Asm Bible Read v2
;
; deepfix: was messing up X in the diskread routine. FIXED :D
;
; todo
;  need to load the book file and decode, done!
;
;
;Asm Bible Read v1
;
;  fix $01 so we can read under ROM -- done
;
;  Data banks had an error on last byte of data. Not correctly flushed in
;  the basic conversion. Needed to "finish" the last byte - fixed.
;  new dataset generated.
;
;
;
;Asm Bible Read v0
; loading the banks into memory, let exomizer do the rest, saves 4kb compressed
; loading the upper bank to $E000, exomizer no good on the upper banks
; division routine needed to be 24 bit, updated.
; word lookup routine completed... i hope (seems good)
;
;
;*=$0801                                 ;2025 sys2061
;        byte $0b,$08,$e9,$07            ;line number (year)
;        byte $9e, "2","0","6","1",0,0,0 ;sys 2061

buf=$0350 ;$fe            ;buffer for word decoder , 5 bit
bitcount=$02              ;the current bit we are working on
                          ;when done 8, time to load a new byte into buf+1 from ram
;locate in casette buffer
wlen=$0340                ;word length we are working on
wrdno=$0341;and $05       ;the word number we need
thy=$0343                 ;save the Y reg
;bytespr=$0344             ;for debug... decode 255 chars
oldrom=$0345              ;save $01 setting when reading memory
bookbuf=$0346             ;book data buffer, 2 bytes 
bookbitc=$0348            ;book bit counter
bookres=$0349             ;book result 2 bytes;wordlen or wordno
wtoprint=$034b            ;string len to print
savex=$034c               ;x gets changed in diskread...

bascx=$034d
bassp=$034e
basdstr=$034f             ;string, not handled... yet
basw=$0350
basdu=$0353               ;do case?
lastspecial=$0354         ;was the last char special
linecnt=$0355             ;lines printed
printmode=$0356           ;0=page, 1=cont,
colcnt=$0357              ;color counter

;Beware
;MATH variables are from $0360-
   


;locate in zeropage-- messes something up
;wlen=$03                ;word length we are working on
;wrdno=$04;and $05       ;the word number we need
;thy=$06                 ;save the Y reg
;bytespr=$07             ;for debug... decode 255 chars
;oldrom=$08              ;save $01 setting when reading memory
;bookbuf=$09             ;book data buffer, 2 bytes 
;bookbitc=$0b            ;book bit counter
;bookres=$0c             ;book result 2 bytes;wordlen or wordno
;wtoprint=$0e
;savex=$10               ;x gets changed in diskread...

*=$0801

incbin "b02.seq"         ;include the word banks 02...
incbin "b03.seq"
incbin "b04.seq"
incbin "b05.seq"
incbin "b06.seq"
incbin "b07.seq"
incbin "b08.seq"
incbin "b09.seq"
incbin "b10.seq"         ;until bank 10, this will be at cfXX
;incbin "b11.seq"        ;no room - rest of banks in "upper.prg" $E000-

;*=$C9D7                 
        ;sei    ;dont disable interrupts, since IO needs them

        ; SETUP
        lda #12                ;set screen colors
        sta $d020
        lda #0
        sta $d021               
        lda #14                 ;set lowercase
        jsr $ffd2
        lda #147                ;clear screen
        jsr $ffd2
        lda #30                 ;green text
        jsr $ffd2

        ;show intro text

        ldx #31
startintro
        ldy #38

introloop
        lda intro,y
        sta $0400,y
        lda colortab,x
        
        sta $d800,y
        dey
        bpl introloop
        lda #250

introrasterloop
        cmp $d012
        bne introrasterloop
introraster2
        cmp $d012
        bne introraster2        ;wait twice for raster
        dex
        bpl startintro


        
        
        ;setup BASIC vars
        lda #0
        sta bascx
        sta bassp
        sta basdstr
        sta lastspecial
        sta printmode

        lda #31
        sta colcnt
        
        lda #1
        sta basdu       ;start with upper case  

        lda #23         ;lines to print before pause
        sta linecnt
        

;        sta bytespr
        ;load the upper data $e000-
        lda #1
        ldx $BA                 ;last used device number
        bne drvskip
        ldx #$08                ;default to device 8
drvskip 
        ldy #0                  ;load
        jsr $ffba               ;setlfs
        lda #fend-filename      ;filename length
        ldx #<filename          ;lo & hi adr of filename string
        ldy #>filename
        jsr $ffbd               ;setnam
        lda #0                  ;flag for 'load'
        ldx #0                  ;set load adress to $e000; under rom
        ldy #$e0
        jsr $ffd5               ;kernal load routine

        ;now open the book file
        ;sei                     ;disable interrupts

        jsr bookopen    ;get a byte from disk by opening
        sta bookbuf     ;save it
        jsr bookread    ;read a byte from disk
        sta bookbuf+1   ;buffer ready
        
;        lda bookbuf     ; debug
;        sta $0400
;        lda bookbuf+1
;        sta $0401
;        jmp bookclose

        ;clear the intro
        lda #147                ;clear screen
        jsr $ffd2

        lda #8          ;setup book buf
        sta bookbitc

        ;now lets decode
readabook

        lda lastspecial ;skip if last was a special char
        cmp #1
        beq goonreadabook

        lda bassp       ;if sp>1 ...
        cmp #1
        bmi goonreadabook
        lda bascx       ;dont print a space if bascx=0
        cmp #0
        bne dontprintspace
        jsr printaspace
dontprintspace
        lda #0
        sta bassp
        inc bascx
        jsr checkcx

       
goonreadabook
        jsr clearbookres
        ldx #1          ;get 1 bit
        jsr getbookbits
        lda bookres+1
        and #1
        cmp #1          ;its a special or its a word
        bne getspecial


        lda lastspecial ;if the last was a special, put a space
        cmp #1
        bne bookspecskip

        jsr printaspace
        lda #0          ;and reset lastspecial flag, and bassp
        sta lastspecial
        sta bassp
bookspecskip
        ;lets decode a word
        ;get word length
        jsr clearbookres
        ldx #4          ;get wordlen
        jsr getbookbits
        lda bookres+1
        and #15
        sta wlen        ;wordlen
        tax             ;set table pointer

        ;lda #8          ;setup book buf
        ;sta bookbitc


        jsr clearbookres
        lda wtabln,x    ;get bitlen of wordbank

        tax
        
        jsr getbookbits  
        lda bookres+1   ;i had swapped hi with lo byte? doh
        sta wrdno
        lda bookres
        sta wrdno+1

;        lda wrdno      ;debug
;        sta $0400
;        lda wrdno+1
;        sta $0401
;        lda wlen
;        sta $0402

        jsr locwrd      ;find the word
        
        ;print wlen+2 bytes
        lda wlen
        clc
        adc #2
        sta wtoprint
        lda bascx       ;add up wordlength to bascx
        clc
        adc wtoprint
        adc #2          ;2 spaces
        sta bascx
        jsr checkcx
        
keepprinting      
        
        jsr read5bit    ;print it
        dec wtoprint
        bne keepprinting
        jsr printaspace

        
        jmp readabook

        ;jmp endlop

;        lda bookres     ; debug
;        sta $0400
;        lda bookres+1
;        sta $0401
;        jmp bookclose



getspecial      ;getspecial char
        lda #0
        sta lastspecial         ;clear lastspecial flag

        jsr clearbookres
        ldx #5                  ;get 5 bits
        jsr getbookbits 
        lda bookres+1
        cmp #23                 ;is it an enter?
        bne chkcomma
        jsr printaenter
        jsr printaenter
        lda #0                  ;cx=0:sp=0:du=1
        sta bascx
        sta bassp
        lda #1
        sta basdu               ;do uppercase
        jmp readmorenext

chkcomma
        cmp #12                 ;is it a comma ","?
        bne chkdot
        lda strcomma
        jsr $ffd2
        lda strcomma+1
        jsr $ffd2
;        lda strcomma+2
;        jsr $ffd2
        lda bascx
        clc
        adc #2
        sta bascx       ;cx=cx+2
        jsr checkcx
        lda #0
        sta bassp
        jmp readmore

chkdot
        cmp #11         ;is it a period "."
        bne chkfora
        lda strdot
        jsr $ffd2
        lda strdot+1
        jsr $ffd2
;        lda strdot+2
;        jsr $ffd2
        lda bascx
        clc
        adc #2
        sta bascx       ;cx=cx+2
        jsr checkcx
        lda #1
        sta basdu       ;do uppercase
        lda #0
        sta bassp
        jmp readmore

chkfora       
        cmp #17         ;is it an "A"
        bne chkfori
        lda #65
        sta basdstr     ;store an A
        jsr checkcase
        lda basdstr
        jsr $ffd2
        inc bascx
        ;jsr printaspace
        jmp readmore

chkfori      
        cmp #18         ;is it an "i"
        bne chkforo
        lda #73
        sta basdstr     ;store an I
        jsr checkcase
        lda basdstr
        jsr $ffd2
        inc bascx
        ;jsr printaspace
        jmp readmore

chkforo      
        cmp #19         ;is it an "O" o as in Ole, Olé
        bne checkforquestion
        lda #79
        sta basdstr     ;store an O
        jsr checkcase
        lda basdstr
        jsr $ffd2
        inc bascx
        ;jsr printaspace
        jmp readmore
        
checkforquestion
        cmp #15         ;is it an "?"
        bne outputspec
        lda #63         ;Questionmark "?"
        jsr $ffd2
        ;jsr printaspace
        lda bascx
        clc
        adc #2
        sta bascx       ;cx=cx+2
        jsr checkcx
        lda #1          ;set uppercase
        sta basdu
        lda #0
        sta bassp
        jmp readmore

outputspec              ;this is a special
        ;ldy basw
        tay
        dey             ;-1 in array
        lda strspec,y   ;get special char
        jsr $ffd2
        inc bascx
        inc bassp       ;sp=sp+1
        lda #1
        sta lastspecial
        jmp readmorenext
        
        


readmore     
        jsr printaspace
        

readmorenext    
        jmp readabook
        ;jmp bookclose


checkcx
        lda bascx
        cmp #35
        bpl checkcxmore
        rts

checkcxmore
        jsr printaenter
        lda #0
        sta bascx
        rts


checkcase       
        lda basdu       ;check case
        cmp #0
        bne checkmorecase
        rts
checkmorecase
        lda basdstr
        clc
        adc #$80        ;do uppercase
        sta basdstr
        lda #0
        sta basdu
        rts

unshowmenu
        ldy #39         ;remove the menu line in bottom
        lda #32
unshowmenuloop
        sta $07c0,y
        dey
        bpl unshowmenuloop
        rts

showmenu
        ;show a menu in the bottom line
        ldy #38
showmenulop
        lda menu,y
        sta $07c0,y
        dey
        bpl showmenulop
        lda #160
        sta $07e7
        rts


showmenu2
        ;show a menu in the bottom line
        ldy #38
showmenulop2
        lda menu2,y
        sta $07c0,y
        dey
        bpl showmenulop2
        lda #160
        sta $07e7
        rts

        

waitforkey
        jsr showmenu

waitforkeynomenu
        lda #0  ;clear length of keyboard buffer
        sta $c6 ;#198

waitkeyloop
        ldy #$40
waitraster
        cpy $d012       ;wait raster
        bne waitraster
waitraster2
        cpy $d012       ;wait another raster
        bne waitraster2

        ldy colcnt
        lda colortab,y
        sta $dbe7       ;last screen char color, flash
        dec colcnt
        bpl colskip
        lda #31         ;reset color counter
        sta colcnt

colskip
        lda #0
        cmp $c6 ;length of keyboard buffer
        beq waitkeyloop ;wait for a key
        lda #0
        sta $c6 ;clear keyboard buffer
        lda $c5 ;what was the key
        ;sta $07e7 ;its a keyb matrix
        cmp #56 ;user pressed "1"
        bne nextkeycmp1
        lda #0
        sta printmode
        jmp endkeycmp

nextkeycmp1
        cmp #59 ;user pressed "2"
        bne nextkeycmp2
        lda #1
        sta printmode
        jmp endkeycmp

nextkeycmp2
        cmp #08 ;user pressed "3"
        bne nextkeycmp3
        inc $0286       ;inc text color
        ldy #39
        lda $0286

colchangelop
        sta $dbc0,y
        dey
        bpl colchangelop
        jmp waitforkeynomenu

nextkeycmp3
        cmp #11 ;user pressed "4"
        bne nextkeycmp4
        jsr showmenu2
        jmp waitforkeynomenu

nextkeycmp4
        cmp #16 ;user pressed "5"
        bne nextkeycmp5
        inc $d020
        jmp waitforkeynomenu

nextkeycmp5
        cmp #19 ;user pressed "6"
        bne nextkeycmp6
        inc $d021
        jmp waitforkeynomenu

nextkeycmp6
        cmp #27 ;user pressed "8"
        bne endkeycmp
        jmp bookclose


endkeycmp
        ;sta $d020
        jsr unshowmenu
        rts
        
printaenter
        lda #13         ;print a ENTER
        jsr $ffd2
        dec linecnt
        bne printaenterout      ;check linecount
        lda #23
        sta linecnt

        lda printmode   ;check printmode
        beq waitthatkey
        lda $c6
        cmp #0
        beq printaenterout      ;if key pressed show menu
waitthatkey
        jsr waitforkey
        

printaenterout
        rts

printaspace
        lda #32
        jsr $ffd2
        rts

clearbookres       
        lda #0
        sta bookres     ;clear book result
        sta bookres+1
        rts


        
       
        ;get X bits from book, load into bookres
getbookbits
        clc
        rol bookbuf+1
        rol bookbuf
        rol bookres+1     ;rol the carry if any into result
        rol bookres

        dec bookbitc
        bne bookadvnxt
        lda #8          
        sta bookbitc    ;reset bitcount
        jsr bookread    ;read a byte from disk       
        sta bookbuf+1
        ;sta $0400+39    ;show some feedback

bookadvnxt
        dex
        bne getbookbits
        rts        




;        ;now list all the words - setup data pointer start $0801
;        lda #$93
;        sta $fc
;        lda #$2a
;        sta $fd
;        lda #8
;        sta bitcount ; sta 8 in bitcount
;        ;bank #02 starts at $0801
;        ldy #0
;        ldx #0

;        ;start by filling the buf
;        lda ($fc),y
;        sta buf
;        iny
;        lda ($fc),y
;        sta buf+1
;        iny
;        sty thy

        ;lets test a word

;        lda #$0e          ;+2=bank
;        sta wlen        ;+2 ?
;        lda #$07        ;word # 100
;        sta wrdno    
;        lda #$00
;        sta wrdno+1
;        ldx wlen
;        lda wtablo,x
;        sta $fc
;        lda wtabhi,x
;        sta $fd

;        jsr locwrd                  
        
read5bit
        ;data is ready in buf, handle 5 bits
        lda buf
        jsr print5bit
        ;advance buf 5 bits
        ldx #5
        jsr advbuf
        rts

;        inc bytespr
;        bne read5bit
;        jmp bookclose


        ;jmp read5bit


print5bit              ;print a 5 bit char
        ;clc
        lsr
        lsr
        lsr
        and #31
        clc
        adc #$40        ;add #64

        ldy basdu       ;uppercase?
        cpy #0
        beq dontupper   ;was BNE... DOH
        ldy #0
        sty basdu       ;clear uppercase
        clc
        adc #$80        ;do uppercase
        
dontupper
        
        jsr $ffd2       ;print the char
        rts



locwrd                  ;locate word
        ;set zpointers to start of wordbank in mem
        ldx wlen
        lda wtablo,x
        sta $fc         ;this needs fixing
        lda wtabhi,x
        sta $fd
        ;now calc offset in bits
        ;a=wlen*wrdno*5     ;wordlen*wordnumber*5 (5bits for each letter)
        lda wlen

        clc
        adc #2  ; real word len

        sta multiplier
        lda #0
        sta multiplier+1
       
        lda wrdno
        sta multiplicand
        lda wrdno+1
        sta multiplicand+1

        jsr mymultiply  ;wlen*wrdno

        lda #5
        sta multiplier
        lda #0
        sta multiplier+1
        lda product
        sta multiplicand
        lda product+1
        sta multiplicand+1

        jsr mymultiply  ;a=wlen*wrdno*5 ; bitlocation in product

      
        lda product
        sta dividend
        lda product+1
        sta dividend+1
        lda product+2
        sta dividend+2
        lda #8
        sta divisor
        lda #0
        sta divisor+1
        sta divisor+2
        
        jsr mydivide    ;a/8; offset byte, remainder is the start bit



        
        ;add result to set offset
        ldx remainder   ; for later; bit offset
                        ;needs to reverse? 8-x.. nop!
        

        lda dividend
        sta num1lo
        lda dividend+1 ; does it exist?
        sta num1lo+1
        lda $fc
        sta num2lo
        lda $fd
        sta num2lo+1

        jsr myaddition



;        lda reslo    ;debug
;        sta $0400
;        lda reslo+1
;        sta $0401
;        stx $0402
;        jmp endlop 



        lda reslo       ;update the byte offset
        sta $fc
        lda reslo+1
        sta $fd
        

        ;start by filling the buf - look under ROM

        sei
        lda $01         ; disable rom
        sta oldrom
        lda #$35
        sta $01

        ldy #0
        lda ($fc),y     ;read data
        sta buf
        iny
        lda ($fc),y
        sta buf+1
        iny
        sty thy

        lda oldrom     ; enable rom
        sta $01
        cli
      

        ; now we need to shift bits into position from remainder
        ;ldy #1
        ;sty thy         ;set the buffer offset to 1
        lda #8  ; not 0 an 8 !!! 
        sta bitcount

        cpx #0  ;dont adv the buffer if x=0
        beq locwrdout
        jsr advbuf      ;shift the bits in X
    
locwrdout    
        rts
        
        

advbuf  ;Advance the buffer X bits. LDX the desired bits
        clc
        rol buf+1
        rol buf
        dec bitcount
        bne advbufnxt
        lda #8          
        sta bitcount    ;reset bitcount
        
        sei
        lda $01         ; disable rom
        sta oldrom
        lda #$35
        sta $01

        ldy thy
        lda ($fc),y     ;read next byte
        sta buf+1
        
        lda oldrom      ;restore rom
        sta $01
        cli

        iny
        sty thy
        bne advbufnxt
        inc $fd
        
advbufnxt
        dex
        bne advbuf
        rts



;-------open and read book from disk

bookopen
        LDA #bookname_end-bookname
        LDX #<bookname
        LDY #>bookname
        JSR $FFBD     ; call SETNAM

        LDA #$02      ; file number 2
        LDX $BA       ; last used device number
        BNE bookdrvskip
        LDX #$08      ; default to device 8
bookdrvskip    
        LDY #$02      ; secondary address 2
        JSR $FFBA     ; call SETLFS

        JSR $FFC0     ; call OPEN
        BCS bookerror    ; if carry set, the file could not be opened

        ; check drive error channel here to test for
        ; FILE NOT FOUND error etc.

        LDX #$02      ; filenumber 2
        JSR $FFC6     ; call CHKIN (file 2 now used as input)
        

;        LDA #<load_address
;        STA $AE
;        LDA #>load_address
;        STA $AF

        ;LDY #$00
bookread    
        stx savex       ; save X before we trash it
        LDX #$02      ; filenumber 2
        ldy #$02
        ;lda #$00
        JSR $FFB7     ; call READST (read status byte)
        BNE bookeof      ; either EOF or read error
        JSR $FFCF     ; call CHRIN (get a byte from file)

        ldx savex       ;restore X
        rts             ; data in A

;        STA ($AE),Y   ; write byte to memory¨

;        and #31         ;print to screen
;        clc
;        adc #$40
;        jsr $ffd2

;        INC $AE
;        BNE bookskip2
;        INC $AF
;bookskip2   
;        JMP bookread     ; next byte

bookeof
        AND #$40      ; end of file?
        BEQ bookreaderror

bookclose
        LDA #$02      ; filenumber 2
        JSR $FFC3     ; call CLOSE

        JSR $FFCC     ; call CLRCHN
        ;inc $d020
        jmp endlop
        ;rts
bookerror
        ; Akkumulator contains BASIC error code

        ; most likely errors:
        ; A = $05 (DEVICE NOT PRESENT)

        ;... error handling for open errors ...
        JMP bookclose    ; even if OPEN failed, the file has to be closed
bookreaderror
        ; for further information, the drive error channel has to be read

        ;... error handling for read errors ...
        JMP bookclose

bookname 
        byte "book1"
bookname_end




;MATH Subroutines start
;------ divide start

; new 24 bit version
dividend=        $0360 ;$22
divisor=         $0363 ;$25
remainder=       $0366 ;$28    ; remainder is in zero-page to gain some cycle/byte ($fb-$fd)
pztemp=          $0369 ;$2b

mydivide
div24   lda #0          ;preset remainder to 0
        sta remainder
        sta remainder+1
        sta remainder+2
        ldx #24         ;repeat for each bit: ...

divloop asl dividend    ;dividend lb & hb*2, msb -> Carry
        rol dividend+1  
        rol dividend+2
        rol remainder   ;remainder lb & hb * 2 + msb from carry
        rol remainder+1
        rol remainder+2
        lda remainder
        sec
        sbc divisor     ;substract divisor to see if it fits in
        tay             ;lb result -> Y, for we may need it later
        lda remainder+1
        sbc divisor+1
        sta pztemp
        lda remainder+2
        sbc divisor+2
        bcc skip        ;if carry=0 then divisor didn't fit in yet

        sta remainder+2 ;else save substraction result as new remainder,
        lda pztemp
        sta remainder+1
        sty remainder   
        inc dividend    ;and INCrement result cause divisor fit in 1 times

skip    dex
        bne divloop     
        rts
;new div end


;------ multiply start
; 16-bit multiply with 32-bit product
; https://codebase64.org/doku.php?id=base:16bit_multiplication_32-bit_product

multiplier      = $0360 ;$22 ;$f0 
multiplicand    = $0363 ;$24 ;$f2
product         = $0369 ;$26 ;$f4
 
mymultiply

mult16          
        lda     #$00
        sta     product+2       ; clear upper bits of product
        sta     product+3 
        ldx     #$10            ; set binary count to 16 
shift_r lsr     multiplier+1    ; divide multiplier by 2 
        ror     multiplier
        bcc     rotate_r 
        lda     product+2       ; get upper half of product and add multiplicand
        clc
        adc     multiplicand
        sta     product+2
        lda     product+3 
        adc     multiplicand+1
rotate_r       
        ror                     ; rotate partial product 
        sta     product+3 
        ror     product+2
        ror     product+1 
        ror     product 
        dex
        bne     shift_r 
        rts
;------------------- multiply end

;------------------- add and subtract

num1lo = $0360 ;$22 ;$62
;num1hi = $63
num2lo = $0362 ;$24 ;$64
;num2hi = $65
reslo = $0364 ;$26; $66
;reshi = $67

; adds numbers 1 and 2, writes result to separate location
;must be left out if not needed...

myaddition
        clc                     ; clear carry
        lda num1lo
        adc num2lo
        sta reslo               ; store sum of LSBs
        lda num1lo+1
        adc num2lo+1            ; add the MSBs using carry from
        sta reslo+1             ; the previous calculation
        rts

; subtracts number 2 from number 1 and writes result out
; - this i not used... yet

;mysubtraction
;        sec                      ; set carry for borrow purpose
;        lda num1lo
;        sbc num2lo               ; perform subtraction on the LSBs
;        sta reslo
;        lda num1lo+1             ; do the same for the MSBs, with carry
;        sbc num2lo+1             ; set according to the previous result
;        sta reslo+1
;        rts

;math routines end

      
        
endlop     
        dec $d020
        jmp endlop
        ;jmp startadress

filename text "upper"
fend

;table of hi and lo bytes of wordbanks start 2-10
wtablo byte $01,$3e,$87,$4e,$93,$1d,$2e,$7f,$27,$00,$91,$a2,$09,$7a,$53
wtabhi byte $08,$08,$0a,$15,$2a,$4b,$71,$94,$b3,$e0,$ed,$f4,$f8,$f9,$fa
;bitlen of each bank
wtabln byte $06,$09,$0b,$0b,$0c,$0c,$0b,$0b,$0a,$09,$08,$07,$07,$06,$03
;data 6,9,11,11,12,12,11,11,10,9,8,7,7,6,3
;
;special chars
;
strcomma  text "{left},"
strdot    text "{left}."
strspec   text "0123456789.,:;?!AIO'()-"

CHARSET 2       ;directive for CBM studio to threat letters with case

               ;1234567890123456789012345678901234567890
menu      text "1=page 2=continuous 3=text color 4=more"
menu2     text "5=background 6=foreground 7=     8=quit"
intro     text 'ASMBible reader v6 by Eyvind Ebsen 2025'

;colors sorted from high to low intensity and up again
colortab   byte 1,7,13,3,15,5,10,12,14,4,8,2,11,6,9,0,9,6,11,2,8,4,14,12,10,5,15,3,13,7


;800 s$="0123456789.,:;?!AIO'()-"+chr$(13)

;00011  0801             INCBIN     "B02.SEQ"
;00012  083E             INCBIN     "B03.SEQ"
;00013  0A87             INCBIN     "B04.SEQ"
;00014  154E             INCBIN     "B05.SEQ"
;00015  2A93             INCBIN     "B06.SEQ"
;00016  4B1D             INCBIN     "B07.SEQ"
;00017  712E             INCBIN     "B08.SEQ"
;00018  947F             INCBIN     "B09.SEQ"
;00019  B327             INCBIN     "B10.SEQ"
;00020  C9D7

;00002  E000             INCBIN     "B11.SEQ"
;00003  ED91             INCBIN     "B12.SEQ"
;00004  F4A2             INCBIN     "B13.SEQ"
;00005  F809             INCBIN     "B14.SEQ"
;00006  F97A             INCBIN     "B15.SEQ"
;00007  FA53             INCBIN     "B16.SEQ"
;00008  FAA4 

;https://codebase64.org/doku.php?id=base:reading_a_file_byte-by-byte
