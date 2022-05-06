        IDENTIFICATION DIVISION.
        PROGRAM-ID. CoPrinter.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
      *
       DATA DIVISION.
       FILE SECTION.

      *
       WORKING-STORAGE SECTION.
       77  WS-DATA              PIC X(60) VALUE " ".
       77  WS-ACCEPT            PIC XX VALUE " ".
       77  F-FIELDNAME          PIC X(25) VALUE " ".
       77  WS-PRINTANSWER       PIC 99.
       77  WS-PRINTERDISPLAY    PIC Z9.
       01  F-ERROR1             BINARY-SHORT.
       01  WS-PRINTER-STATUS.
           03  WS-PRNT-ST1      PIC 99.
       01  WS-OWN-PRINT-FILE.
           03  WS-OWN-DIR       PIC X(12) VALUE "/ctools/spl/".
           03  WS-OWN-FILE-NAME PIC X(13) VALUE " ".
       01  PRINT-LINE.
            03  FILLER             PIC X(11).
            03  W-SUB1             PIC Z9.
            03  FILLER             PIC X(2).
            03  W-PRINTER          PIC X(25).
            03  FILLER             PIC X(1).
            03  W-LOCATION         PIC X(20).
            03  FILLER             PIC X(2).
       Copy "WsDateInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       PROCEDURE DIVISION USING Ws-Linkage.
       CONT-PARAGRAPH SECTION.
       CONT-000.
           IF WS-PRINTERTYPE (21) = 3 OR = 4 OR = 9
              MOVE WS-PRINTERNUMBER (21) TO WS-PRINTERSELECTED
              PERFORM CHECK-PRINTER
              EXIT PROGRAM.
           PERFORM CLEAR-010 THRU CLEAR-015.
       CONT-001.
           MOVE 510 TO POS
           DISPLAY
           "You Must Select Which Printer You Want to Print To.        "
                  AT POS.
           Move 718 to pos
           Display "Option  Queue Name                Location" At Pos
           MOVE 711 TO POS
           MOVE 1 TO SUB-1.
       CONT-010.
           IF WS-PRINTERNAME (SUB-1) = " "
              GO TO CONT-020.
           IF WS-PRINTERTYPE (21) = 1
            IF WS-PRINTERTYPE (SUB-1) = 1 OR = 2
              GO TO CONT-011.
           IF WS-PRINTERTYPE (SUB-1) NOT = WS-PRINTERTYPE (21)
              GO TO CONT-012.
       CONT-011.
           MOVE WS-PRINTERNUMBER (SUB-1)   TO W-SUB1
           MOVE WS-PRINTERNAME (SUB-1)     TO W-PRINTER
           MOVE WS-PRINTERLOCATION (SUB-1) TO W-LOCATION
           ADD 100 TO POS
           DISPLAY PRINT-LINE AT POS.
       CONT-012.
           IF SUB-1 < 20
              ADD 1 TO SUB-1
              GO TO CONT-010.
       CONT-015.
           GO TO CONT-020.
      ******************************************************************
      * CONT-015 HAS BEEN OMITTED FROM DISPLAYING SO THAT STAFF DON'T  *
      * PLAY THE FOOL IN CHECKING BRANCH PRINTERS.  IT DOES WORK THOUGH*
      ******************************************************************
           ADD 200 TO POS
           DISPLAY "Enter 'X' To Print To Your Own Device Name" AT POS.
           IF WS-ACCEPT NOT = "B"
            ADD 100 TO POS
            DISPLAY "   OR 'B' To Display Branch Device Names," AT POS.
           IF WS-ACCEPT = "B"
            ADD 100 TO POS
            DISPLAY "   OR 'R' To RE-Display Device Names Here." AT POS.
       CONT-020.
           MOVE " " TO WS-ACCEPT.
           MOVE 610 TO POS
           DISPLAY 
           "Your Selection is : [  ]                                   "
              AT POS
           ADD 21 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 3         TO CDA-ROW.
           MOVE 30        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

           IF W-ESCAPE-KEY = 3
               MOVE 99 TO WS-PRINTANSWER
               GO TO CONT-900.
           IF WS-ACCEPT = "B" OR = "R"
               GO TO CONT-000.
           IF WS-ACCEPT = "X" OR = "x" OR = "XX" OR = "xx"
             PERFORM ENTER-OWN-FILE
            IF W-ESCAPE-KEY = 3
             GO TO CONT-020
            ELSE
             GO TO CONT-950.
               
           IF WS-ACCEPT NOT > " "
               GO TO CONT-020.
           MOVE SPACES       TO ALPHA-RATE.
           MOVE WS-ACCEPT    TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-PRINTANSWER WS-PRINTERDISPLAY.
           DISPLAY WS-PRINTERDISPLAY AT POS.
       CONT-900.
           If WS-PRINTANSWER = 99
               MOVE WS-PRINTANSWER TO WS-PRINTERSELECTED
               GO TO CONT-950.
           MOVE WS-PRINTANSWER TO WS-PRINTERSELECTED
           PERFORM CHECK-PRINTER.
           IF WS-PRNT-ST1 = 8
              MOVE "You Must Select a Printer from those Displayed."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO CONT-020.
           PERFORM CLEAR-010 THRU CLEAR-015.
       CONT-950.
           EXIT PROGRAM.
       CONT-999.
           EXIT.
      *
       CHECK-PRINTER SECTION.
       CP-005.
           MOVE 00 TO WS-PRNT-ST1.
           MOVE 1 TO SUB-1.
       CP-010.
           IF WS-PRINTERNUMBER (SUB-1) NOT = WS-PRINTERSELECTED
            IF SUB-1 < 20
                ADD 1 TO SUB-1
                GO TO CP-010.
           IF WS-PRINTERSELECTED NOT = WS-PRINTERNUMBER (SUB-1)
                MOVE 8 TO WS-PRNT-ST1.
           MOVE WS-PRINTERNUMBER   (SUB-1) TO WS-PRINTERNUMBER (21)
           MOVE WS-PRINTERNAME     (SUB-1) TO WS-PRINTERNAME (21)
           MOVE WS-PRINTERTYPE     (SUB-1) TO WS-PRINTERTYPE (21)
           MOVE WS-PRINTERLOCATION (SUB-1) TO WS-PRINTERLOCATION (21)
           MOVE WS-PRINTERPROMPT   (SUB-1) TO WS-PRINTERPROMPT (21)
           MOVE WS-PRINTERCHARS    (SUB-1) TO WS-PRINTERCHARS (21).
       CP-999.
           EXIT.
      *
       ENTER-OWN-FILE SECTION.
       EOF-010.
           MOVE 610 TO POS
           DISPLAY
          "Enter Your Own File Name : [                         ]"
                  AT POS
           ADD 28 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 25        TO CDA-DATALEN.
           MOVE 3         TO CDA-ROW.
           MOVE 37        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE FUNCTION LOWER-CASE(CDA-DATA) TO F-FIELDNAME.
           
           IF W-ESCAPE-KEY = 3
               GO TO EOF-999.
           IF F-FIELDNAME = " "
                DISPLAY " " AT 3079 WITH BELL
                GO TO EOF-010.
       EOF-020.
           IF WS-ACCEPT = "X" OR = "x"
               MOVE F-FIELDNAME        TO WS-OWN-FILE-NAME
                                          WS-PRINTERSELECTED.
           IF WS-ACCEPT = "XX" OR = "xx"
               MOVE F-FIELDNAME        TO WS-OWN-PRINT-FILE
               MOVE 88                 TO WS-PRINTERSELECTED.

           MOVE 0                      TO WS-PRINTERNUMBER (21)
           MOVE WS-OWN-PRINT-FILE      TO WS-PRINTERNAME (21)
           Move "*Selected Disk File*" TO WS-PRINTERLOCATION (21).
           MOVE WS-PRINTERCHARS    (1) TO WS-PRINTERCHARS (21)
           MOVE 1                      TO WS-PRINTERTYPE (21).
           PERFORM CLEAR-010 THRU CLEAR-015.
       EOF-999.
           EXIT.
      *
       Copy "ClearScreen".
       Copy "DecimaliseRate".
       Copy "ErrorMessage".
       Copy "CTOSCobolAccept".
      * END-OF-JOB
