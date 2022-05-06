/**
00 - HELP      (ctrl-H)      (0x08)
01 - UP                      (0x103)
02 - MARK      
03 -       
04 - FINISH    (END)         (0x168)
05 - PREV PAGE (Page Up)     (0x153)
06 - 
07 - CANCEL    (Ctrl-X) (0x07) (Esc) 0x
08 - BACKSPACE               (0x107)
09 - TAB                     (0x09)
0A - RETURN (NEXT)           (0x0A)
0B - DOWN                    (0x102)
0C - NEXT PAGE  (Page Down)  (0x152)
0D - BOUND      
0E - LEFT                    (0x104)
0F - MOVE       (Ctrl-O)     (0x0F) 
10 - 
11 - SCROLL UP  (ctrl-UP)    (0x235)
12 - RIGHT                   (0x105)
13 - SCROLL DOWN(ctrl-DOWN)  (0x235)
14 - COPY
15 - F1                      (0x109)
16 - F2                      (0x10A)
17 - F3                      (0x10B)
18 - F4                      (0x10C)
19 - F5                      (0x10D)
1A - F6                      (0x10E)
1B - GO
1C - F7                      (0x10F)
1D - F8                      (0x110)
1E - F9                      (0x111)
1F - F10                     (0x112)


 
<F1> thru <F10> : X”15” thru X”1F”
<GO> key : X”1B”           As the PC does not have this key we could use <Alt-Return> or <Alt-Enter>
<Code-GO> : X”9B”        As the PC does not have this key we could use <Code-Alt-Return> or <Cntrl-Alt-Return>
<Shift-F5> : X”D9”
<Code-F5>:  X”99”
<Next-page> = <PgDn> : X”0C“
<Prev-Page> = <PgUp> : X”05”
<Scroll-Up> = X”11”       As the PC does not have this key we could use <Alt-Pgdn>
<Scroll-Down> “ X”13”   As the PC does not have this key we could use <Alt-PgUp>
<Tab> : X”09”
<Cancel> = <Esc> : X”07”
<Finish> = <End> : X”04”
<Move> : X”0F”
<Code-Shift-v> : X”D6”
<Code-E> OR <Code-e> : X”E5”
<Code-B> or <Code-b>  : X”E2”
<Code-Delete> : X”FF”
<Code-Move>  : X”8F”
<Up-Arrow> : X”01”
<Down-Arrow> : X”0B”

 
Hi Max,
 
The Call X”E5” is the “Beep” call that the COBOL2 runtime had built into it – not part of Forms.
 
If you look in <Copy>FormsInfo you will find the following:
       01  F-EXITSTATE.
           03  F-EXIT-ICH          PIC 9(4) COMP.
           03  F-EXIT-CH           PIC X.
           03  FILLER                PIC X(13).
 
The F-EXIT-CH is where the return char is put by the OS when “&PerformUserFillField” is performed.
 
The values like X”01” come from the standard hex value chart found in CTOS – See attached......
 
As mentioned before we use a number of keys strokes AND combinations as found in the attached.....
 
So the few you have asked about are only 10% of what we check for.
 
In straight COBOL WITHOUT forms (as found in a number of reports) at the moment we use the following as found in WsDateInfo.
 
       01  W-DEFINE-ESCAPE.
           05  W-ESCAPE-KEY     PIC 99 COMP-X.
           05  W-ESCAPE-TABLE   PIC X(20) VALUE
                 X"010A011B01040101010B0107010C011D011F00".
      *                   1      2       3     4      5      6       7      8      9
      *  1=RETURN,    2=GO,  3=FINISH, 4=UP, 5=DOWN, 6=CANCEL,
      *  7=NEXT-PAGE, 8=F8,  9=F10
 
We then check for W-ESCAPE-KEY if it is = 1 or 2 or 3 ETC......  Would we do the same in Linux ????  or does your key checker handle it differently ???
 
Hope this answers your question.  See below in RED
 
Best regards
Steve
================= My halfdelay(1) solution ======================
        | No X11       | PuTTY         | xfce4-term
---------------------------------------|------------
F1      | 1b5b5b41     | 1b5b31317e    |
F2      | 1b5b5b42     | 1b5b31327e    | 1b4f51
F3      | 1b5b5b43     | 1b5b31337e    | 1b4f52
F4      | 1b5b5b44     | 1b5b31347e    | 1b4f53
F5      | 1b5b5b45     | 1b5b31357e    | 
F6      | 1b5b31377e   | 1b5b31377e    |
F7      | 1b5b31387e   | 1b5b31387e    |
F8      | 1b5b31397e   | 1b5b31397e    |
F9      | 1b5b32307e   | 1b5b32307e    |
F10     | 1b5b32317e   | 1b5b32317e    |
F11     | 1b5b32337e   | 1b5b32337e    |
F12     | 1b5b32347e   | 1b5b32347e    |
SF1     | 1b5b32357e   | 1b5b32337e    | 1b4f313b3250
SF2     | 1b5b32367e   | 1b5b32347e    | 1b4f313b3251
SF3     | 1b5b32387e   | 1b5b32357e    | 1b4f313b3252
SF4     | 1b5b32397e   | 1b5b32367e    | 1b4f313b3253
SF5     | 1b5b33317e   | 1b5b32387e    | 1b5b31353b327e
SF6     | 1b5b33327e   | 1b5b32397e    | 1b5b31373b327e
SF7     | 1b5b33337e   | 1b5b33317e    | 1b5b31383b327e
SF8     | 1b5b33347e   | 1b5b33327e    | 1b5b31393b327e
SF9     |              | 1b5b33337e    | 1b5b32303b327e
SF10    |              | 1b5b33347e    |
SF11    |              | NC            |
SF12    |              | NC            |
CF1     |              | NC            |
CF2     |              | NC            |
CF3     |              | NC            |
CF4     |              | NC            |
CF5     |              | NC            |
CF6     |              | NC            |
CF7     |              | NC            |
CF8     |              | NC            |
CF9     |              | NC            |
CF10    |              | NC            |
CF11    |              | NC            |
CF12    |              | NC            |
AF1     |              | 1b1b5b31317e  |
AF2     |              | 1b1b5b31327e  |
AF3     |              | 1b1b5b31337e  |
AF4     |              | 1b1b5b31347e  |
AF5     |              | 1b1b5b31357e  |
AF6     |              | 1b1b5b31377e  |
AF7     |              | 1b1b5b31387e  |
AF8     |              | 1b1b5b31397e  |
AF9     |              | 1b1b5b32307e  |
AF10    |              | 1b1b5b32317e  |
AF11    |              | 1b1b5b32337e  |
AF12    |              | 1b1b5b32347e  |
Ins     | 1b5b327e     |               |
Del     | 1b5b337e     |               |
Home    | 1b5b317e     |               |
End     | 1b5b347e     |               |
PgUp    | 1b5b357e     |               |
PgDn    | 1b5b367e     |               |
SIns    | NC           |               |
SDel    | NC           |               |
SHome   | NC           |               |
SEnd    | NC           |               |
SPgUp   |              |               |
SPgDn   |              |               |
CIns    | NC           |               |
CDel    | NC           |               |
CHome   | NC           |               |
CEnd    | NC           |               |
CPgUp   | NC           |               |
CPgDn   | NC           |               |
AIns    | NC           | 1b1b5b327e    |
ADel    | NC           | 1b1b5b337e    |
AHome   | NC           | 1b1b5b317e    |
AEnd    | NC           | 1b1b5b347e    |
APgUp   | NC           | 1b1b5b357e    |
APgDn   | NC           | 1b1b5b367e    |
Left    | 1b5b44       |               |
Right   | 1b5b43       |               |
Up      | 1b5b41       |               |
Down    | 1b5b42       |               |
SLeft   | NC           |               |
SRight  | NC           |               |
SUp     | NC           |               |
SDown   | NC           |               |
CLeft   | NC           |               |
CRight  | NC           |               |
CUp     | NC           |               |
CDown   | NC           |               |
ALeft   |              | 1b1b5b44      |
ARight  |              | 1b1b5b43      |
AUp     |              | 1b1b5b41      |
ADown   | NC           | 1b1b5b42      |
Tab     | 09           |               |
STab    | NC           |               |
CTab    | NC           |               |
Enter   | 0a           |               |
SEnter  | NC           |               |
CEnter  | NC           |               |
Bspace  | 7f           |               |
ABspace | 1b7f         |               |
SBspace | 08           |               |
ATab    | 1b09         |               |
AEnter  | 1b0a         |               |
A-a     | 1b61         |               |
A-b     | 1b62         |               |
A-c     | 1b63         |               |
A-d     | 1b64         |               |
A-e     | 1b65         |               |
A-f     | 1b66         |               |
A-g     | 1b67         |               |
A-h     | 1b68         |               |
A-i     | 1b69         |               |
A-j     | 1b6a         |               |
A-k     | 1b6b         |               |
A-l     | 1b6c         |               |
A-m     | 1b6d         |               |
A-n     | 1b6e         |               |
A-o     | 1b6f         |               |
A-p     | 1b70         |               |
A-q     | 1b71         |               |
A-r     | 1b72         |               |
A-s     | 1b73         |               |
A-t     | 1b74         |               |
A-u     | 1b75         |               |
A-v     | 1b76         |               |
A-w     | 1b77         |               |
A-x     | 1b78         |               |
A-y     | 1b79         |               |
A-z     | 1b7a         |               |
  Alt and AltShift 
  seem to escape 
  normal char here
**/   