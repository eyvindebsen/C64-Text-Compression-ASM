10 rem squeeze data - optimize word banks
15 mx=47103+1024+192:dl=14:sf=mx:dim ds(dl),bn$(dl),us(dl):rem sf=space free
20 forx=0todl:read ds(x):bn$(x)="bank "+str$(x+2):print ds(x),bn$(x):next
30 rem sort
40 ds=0
45 forx=0todl-1:ifds(x)<ds(x+1)thengosub100
50 next:ifds=1then40
55 ?:print"sorted":?
60 forx=0todl:printds(x),bn$(x):next
70 ?:print"spend":?
75 forx=0todl:ifsf>ds(x)thensf=sf-ds(x):print"used"ds(x),bn$(x):us(x)=1
80 next
82 print"there is"sf"free bytes"
84 print"code can start at"2049+mx-sf
86 print "upper mem""
88 for x=0todl:ifus(x)=0then um=um+ds(x):print"upper bank "bn$(x)
90 next
92 print"spend upper mem"um
98 end:rem data
99 data 61,585,2759,5445,8330,9745,9041,7848,5808,3473,1809,871,369,217,81
100 sw=ds(x):ds(x)=ds(x+1):ds(x+1)=sw:ds=1
110 s$=bn$(x):bn$(x)=bn$(x+1):bn$(x+1)=s$
120 return