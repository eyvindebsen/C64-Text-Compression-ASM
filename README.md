# C64-Text-Compression-ASM
ASM Text decompressor using all memory

This is the assembler version of the original bible read basic program.

Entire vobulary in memory for super quick access.
Only speed limit is the kernal PRINT, and kernal IO disk operations.

This contains the first book of Moses, Genesis.
This is 206kb of raw text compressed to 78kb.
This will take about 14+ minutes to decode on a PAL system.
Or days, if you are going to read it ;)

The compression is 3:1, yielding about 3 bytes of text, by the cost of 1, in this preview disk. Does not seem like much, but the main program can decode any kind of compressed textfile once loaded.
This will really kick some a$$, when using the other side of the disk, or a 1581 disk (800kb) or several. Text adventures can be huge!

Decoder squeezed in with a little intro, and a menu to control the decode and colors.
CBM studio reports program from $0801 to $CFFB, 4 bytes left :)
Seems it time to optimize.

Here are the controls
1 : Print a page of text. Will print a page and wait.
2 : Print continuously. Will keep printing the entire book until keypress
3 : Change text color
4 : Show the 2nd menu
5 : Change background color
6 : Change foreground color
7 : Future option, search?
8 : close the book and quit
