#! /usr/bin/env python
'''
NUL 00 SOH 01 STX 02 ETX 03 EOT 04 ENQ 05 ACK 06 BEL 07
BS  08 HT  09 NL  0A VT  0B NP  0C CR  0D SO  0E SI  0F
DLE 10 DC1 11 DC2 12 DC3 13 DC4 14 NAK 15 SYN 16 ETB 17
CAN 18 EM  19 SUB 1A ESC 1B FS  1C GS  1D RS  1E US  1F
DEL 7F 

NUL null
SOH start of heading
STX start of text
ETX end of text
EOT end of transmission
ENQ enquiry
ACK acknowledge
BEL bell
BS  backspace
HT  horizontal tab
NL  new line (or LF, line feed)
VT  vertical tab
NP  new page (or FF, form feed)
CR  carriage return
SO  shift out
SI  shift in
DLE data link escape
DC1 device control 1
DC2 device control 2
DC3 device control 3
DC4 device control 4
NAK negative acknowledge
SYN synchronous idle
ETB end of transmission block
CAN cancel
EM  end of medium
SUB substitute
ESC escape
FS  file separator
GS  group separator
RS  record separator
US  unit separator
SP  space
DEL delete
'''
code = {}
# hex 20-7E used as ascii chars
# hex A0-D4 not defined here
# hex DB not defined here
# hex E1 not defined here
# hex E3-E4 not defined here
# hex E6-F0 not defined here
# hex F5-FD not used
code['1/2']='06'
code['1/4']='10'
code['ACK']='06'
code['BACKSPACE']='08'
code['BEL']='07'
code['BOUND']='0D'
code['BS']='08'
code['CAN']='18'
code['CANCEL']='07'
code['CODE-1/2']='86'
code['CODE-B']='E2'
code['CODE-BACKSPACE']='88'
code['CODE-BACKSPACE']='FE'
code['CODE-BOUND']='8D'
code['CODE-BS']='FE'
code['CODE-CANCEL']='87'
code['CODE-COPY']='94'
code['CODE-DELETE']='FF'
code['CODE-DOWN']='8B'
code['CODE-E']='E5'
code['CODE-F1']='95'
code['CODE-F10']='9F'
code['CODE-F2']='96'
code['CODE-F3']='97'
code['CODE-F4']='98'
code['CODE-F5']='99'
code['CODE-F6']='9A'
code['CODE-F7']='9C'
code['CODE-F8']='9D'
code['CODE-F9']='9E'
code['CODE-FINISH']='84'
code['CODE-GO']='9B'
code['CODE-HELP']='80'
code['CODE-LEFT']='8E'
code['CODE-LEFT']='F3'
code['CODE-MARK']='82'
code['CODE-MOVE']='8F'
code['CODE-MOVE']='8F'
code['CODE-NEXT-PAGE']='8C'
code['CODE-PREV-PAGE']='85'
code['CODE-RETURN']='8A'
code['CODE-RIGHT']='92'
code['CODE-RIGHT']='F4'
code['CODE-SCR-DN']='93'
code['CODE-SCR-UP']='91'
code['CODE-SCROLL-DOWN']='93'
code['CODE-SCROLL-UP']='91'
code['CODE-SHIFT-1/2']='90'
code['CODE-SHIFT-6']='83'
code['CODE-SHIFT-V']='D6'
code['CODE-TAB']='89'
code['CODE-UP']='81'
code['COPY']='14'
code['CR']='0D'
code['DC1']='11'
code['DC2']='12'
code['DC3']='13'
code['DC4']='14'
code['DEL']='7F' 
code['DEL-CHAR']='F2'
code['DELETE']='7F'
code['DLE']='10'
code['DOWN']='0B'
code['EM']='19'
code['ENQ']='05'
code['EOT']='04'
code['ESC']='1B'
code['ETB']='17'
code['ETX']='03'
code['F1']='15'
code['F10']='1F'
code['F2']='16'
code['F3']='17'
code['F4']='18'
code['F5']='19'
code['F6']='1A'
code['F7']='1C'
code['F8']='1D'
code['F9']='1E'
code['FINISH']='04'
code['FS']='1C'
code['GO']='1B'
code['GS']='1D'
code['HELP']='00'
code['HT']='09'
code['INS-TOGGLE']='F1'
code['LEFT']='0E'
code['MARK']='02'
code['MOVE']='0F'
code['NAK']='15'
code['NEXT']='0A'
code['NEXT-PAGE']='0C'
code['NL']='0A'
code['NP']='0C'
code['NUL']='00'
code['PREV-PAGE']='05'
code['RETURN']='0A'
code['RIGHT']='12'
code['RS']='1E'
code['SCROLL-DN']='13'
code['SCROLL-DOWN']='13'
code['SCROLL-UP']='11'
code['SHIFT-1/2']='10'
code['SHIFT-6']='03'
code['SHIFT-F1']='D5'
code['SHIFT-F10']='DF'
code['SHIFT-F2']='D6'
code['SHIFT-F3']='D7'
code['SHIFT-F4']='D8'
code['SHIFT-F5']='D9'
code['SHIFT-F6']='DA'
code['SHIFT-F7']='DC'
code['SHIFT-F8']='DD'
code['SHIFT-F9']='DE'
code['SI']='0F'
code['SO']='0E'
code['SOH']='01'
code['STX']='02'
code['SUB']='1A'
code['SYN']='16'
code['TAB']='09'
code['UP']='01'
code['US']='1F'
code['VT']='0B'

for entry in sorted(code):
  print ', {"%s", 0x%s}' % (entry, code[entry])

