#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      vince
#
# Created:     24/06/2014
# Copyright:   (c) vince 2014
# Licence:     <your licence>
#-------------------------------------------------------------------------------

field = {}
field[0] = 'HELP'
field[1] = 'UP'
field[2] = 'MARK'
field[4] = 'FINISH'
field[5] = 'PREV-PAGE'
field[7] = 'CANCEL'
field[8] = 'BACKSPACE'
field[9] = 'TAB'
field[10] = 'RETURN'
field[11] = 'DOWN'
field[12] = 'NEXT-PAGE'
field[13] = 'BOUND'
field[14] = 'LEFT'
field[15] = 'MOVE'
field[17] = 'SCROLL-UP'
field[20] = 'COPY'
field[21] = 'F1'
field[22] = 'F2'
field[23] = 'F3'
field[24] = 'F4'
field[25] = 'F5'
field[26] = 'F6'
field[27] = 'F7'
field[28] = 'GO'
field[29] = 'F8'
field[30] = 'F9'
field[31] = 'F10'
field[0x8f] = 'CODE-MOVE'
field[0x99] = 'CODE-F5'
field[0x9b] = 'CODE-GO'
field[0xd6] = 'CODE-SHIFT-V'
field[0xe2] = 'CODE-B'
field[0xe5] = 'CODE-E'
field[0xff] = 'CODE-DELETE'


def main():
  codes = sorted(field.keys())
  for code in codes:
    print '  add_field(0x%x, "%s");' % (code, field[code])


if __name__ == '__main__':
    main()
