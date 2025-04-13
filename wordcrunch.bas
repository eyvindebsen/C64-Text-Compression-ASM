2 rem this will convert 8 bit strings to 5 bit string-stream
3 rem use source disk in 8, destination in 9
4 rem a$="a":gosub90:a$="b":gosub90:a$="c":gosub90:a$="d":gosub90:a$="e":gosub90:end
5 input"ready";a$
6 od=9:ip=0:dp=0:rem out device, data pos
10 forx=2to16:b$=str$(x)
12 f$=right$(b$,len(b$)-1):ifx<10thenf$="0"+f$
13 w$="@:b"+f$:rem will overwrite
14 f$="w"+f$:print"read "f$" write "w$
15 open 3,od,3,w$+",s,w"
16 ds=0:el=0:bn=0:cd=0:rem input data size, elements, bit #
17 rem next:end
20 open 2,8,2,f$+",s,r"
30 b$="":forz=1tox:get#2,a$:ot=st:gosub90:b$=b$+a$:next:printb$,:ds=ds+x:el=el+1
35 ip=ip+x:rem count up input bytes
40 if ot=64 then close2:goto80
50 goto30
80 if bn>0 then gosub120:print#3,chr$(cd);:sb=sb+1:rem flush the buffer if any. cd/2 needs to undo the rol
81 print#3,chr$(0);:close3:sb=sb+1:rem add a zero
82 ?:printf$" done.":?"total size:"ds" elements: "el
84 ?"there are"ip"input bytes"
85 print"there are"sb"output bytes":?"saved %"100-(sb/ip*100)
86 next
87 ?:print"all complete":?"there was"ip"input bytes"
88 print"there was"sb"output bytes":?"saved %"100-(sb/ip*100):end
89 rem insert 5-bit
90 da=asc(a$)and31:remprint"data "da,a$
92 fort=.to4
94 if daand16thencd=cd+1
95 rem print t,cd,bn
97 bn=bn+1:ifbn=8thenprint#3,chr$(cd);:cd=0:bn=0:sb=sb+1:rem save the byte
98 cd=cd*2:da=da*2:rem shift bits
99 next:return
110 rem print#3,chr$(cd);:cd=0:bn=0:return
119 rem finish the last byte of bits
120 forq=bn+1to7:cd=cd*2:next:return