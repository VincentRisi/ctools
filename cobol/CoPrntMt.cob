        IDENTIFICATION DIVISION.
        PROGRAM-ID. CoPrntMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT PRINTER-MASTER ASSIGN TO Ws-CoPrinters
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-COPRINTERS-STATUS
               RECORD KEY IS PRNT-KEY.
           SELECT PRINTER-REMOTE ASSIGN TO Ws-CoPrintersRemote
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-COPRINTERREMOTE-STATUS
               RECORD KEY IS PRNREM-KEY.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCoPrinters.
           COPY ChlfdCoPrintersRemote.
           
       WORKING-STORAGE SECTION.
       77  NEW-NO             PIC X VALUE " ".
       77  WS-END             PIC X VALUE " ".
       77  WS-ACCEPT          PIC 9 VALUE 0.
       77  WS-NUMBER          PIC 99 VALUE 0.
       77  WS-SAVE            PIC 99 VALUE 0.
       01  WS-COPRINTERS-STATUS.
           03  WS-PRNT-ST1    PIC 99.
       01  WS-COPRINTERREMOTE-STATUS.
           03  WS-PRNREM-ST1  PIC 99.
       Copy "WsDateInfo".
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 0410 TO POS
           DISPLAY "*** PRINTER ENTRY / EDIT PROGRAM ***" AT POS
           MOVE 0610 TO POS
           DISPLAY 
           "ENTER 1=SERVER PRINTERS; 3, 7, 8,=REMOTE PRINTERS [ ]"
            AT POS
           MOVE 0661 TO POS
           
           
           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 03        TO CDA-ROW.
           MOVE 60        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

           IF WS-ACCEPT NOT = 1 AND NOT = 3
                    AND NOT = 7 AND NOT = 8
              DISPLAY " " AT 3079 WITH BELL
              GO TO CONTROL-000.

           IF WS-CO-NUMBER NOT = 1
            IF WS-ACCEPT NOT = 1 
               PERFORM WORK-OUT-REMOTE-PRINTER-FILE.
               
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN.
       CONT-010.
           PERFORM DISPLAY-FORM.
           MOVE 0425 TO POS
           IF WS-ACCEPT = 1
              DISPLAY "** SERVER PRINTERS **" AT POS
           ELSE
              DISPLAY "** REMOTE PRINTERS ** = " AT POS
              ADD 25 TO POS 
              DISPLAY WS-ACCEPT AT POS.
           IF WS-ACCEPT = 1
              PERFORM GET-DATA
              PERFORM FILL-DATA
              GO TO CONT-010.
           IF WS-ACCEPT = 3 OR 7 OR 8
              PERFORM GET-REM-DATA
              PERFORM FILL-REM-DATA
              GO TO CONT-010.
      *
       WORK-OUT-REMOTE-PRINTER-FILE SECTION.
       WORPF-000.
      * "CoPrintersRemote"
            MOVE WS-COPRINTERSREMOTE TO ALPHA-RATE.

            MOVE WS-COPRINTERSREMOTE TO WS-MESSAGE
            PERFORM ERROR-MESSAGE.
       WORPF-010.
            MOVE WS-ACCEPT    TO DATA-RATE.
            MOVE DAT-RATE (1) TO AL-RATE (32).
            MOVE DAT-RATE (2) TO AL-RATE (33).
            
            MOVE ALPHA-RATE TO WS-COPRINTERSREMOTE.

            MOVE WS-COPRINTERSREMOTE TO WS-MESSAGE
            PERFORM ERROR-MESSAGE.
       WORPF-999.
            EXIT.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE "N" TO NEW-NO
                       WS-END.
           MOVE 6 TO PRNT-TYPE.
      *     MOVE "IN GET SECTION." TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       GET-001.
           MOVE "                   " TO F-NAMEFIELD.
           MOVE "NUMBER" TO F-FIELDNAME.
           MOVE 6 TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           MOVE 2 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO PRNT-NUMBER
           IF F-EXIT-CH = X"0C"
                 MOVE WS-SAVE TO PRNT-NUMBER
                 PERFORM START-RECORD
                 PERFORM READ-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-003 THRU GET-005
                 GO TO GET-999.
           IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
           IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
           IF F-EXIT-CH NOT = X"0A" AND NOT = X"1A"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-000.
           PERFORM READ-RECORD.
           IF NEW-NO = "Y"
              PERFORM GET-003
              GO TO GET-999.
      *      GO TO GET-005.
       GET-003.
           MOVE "NUMBER"    TO F-FIELDNAME
           MOVE 6           TO F-CBFIELDNAME
           MOVE PRNT-NUMBER TO F-EDNAMEFIELDANAL
           MOVE 2           TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ANALYSIS.
       GET-005.
           MOVE "NAME"    TO F-FIELDNAME
           MOVE 4         TO F-CBFIELDNAME
           MOVE PRNT-NAME TO F-NAMEFIELD
           MOVE 20        TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "TYPE"    TO F-FIELDNAME
           MOVE 4         TO F-CBFIELDNAME
           MOVE PRNT-TYPE TO F-EDNAMEFIELDANAL
           MOVE 2         TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ANALYSIS.

           MOVE "DESC"     TO F-FIELDNAME
           MOVE 4          TO F-CBFIELDNAME
           MOVE PRNT-DESC  TO F-NAMEFIELD
           MOVE 20         TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "PROMT-PAPER"     TO F-FIELDNAME
           MOVE 11                TO F-CBFIELDNAME
           MOVE PRNT-Prompt-PAPER TO F-NAMEFIELD
           MOVE 1                 TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "COMP"     TO F-FIELDNAME
           MOVE 4          TO F-CBFIELDNAME
           MOVE PRNT-COMP  TO F-NAMEFIELD
           MOVE 2          TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "BOLD"     TO F-FIELDNAME
           MOVE 4          TO F-CBFIELDNAME
           MOVE PRNT-BOLD  TO F-NAMEFIELD
           MOVE 2          TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "UNBOLD"    TO F-FIELDNAME
           MOVE 6           TO F-CBFIELDNAME
           MOVE PRNT-UNBOLD TO F-NAMEFIELD
           MOVE 2           TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "NORMAL"     TO F-FIELDNAME
           MOVE 6            TO F-CBFIELDNAME
           MOVE PRNT-NORMAL  TO F-NAMEFIELD
           MOVE 2            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "EIGHT"     TO F-FIELDNAME
           MOVE 5           TO F-CBFIELDNAME
           MOVE PRNT-EIGHT  TO F-NAMEFIELD
           MOVE 8           TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "ELEVEN"     TO F-FIELDNAME
           MOVE 6            TO F-CBFIELDNAME
           MOVE PRNT-ELEVEN  TO F-NAMEFIELD
           MOVE 8            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "FOUR"       TO F-FIELDNAME
           MOVE 4            TO F-CBFIELDNAME
           MOVE PRNT-FOUR    TO F-NAMEFIELD
           MOVE 8            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       GET-999.
           EXIT.
      *
       GET-REM-DATA SECTION.
       GET-REM-000.
           MOVE "N" TO NEW-NO
                       WS-END.
           MOVE 6 TO PRNREM-TYPE.
      *     MOVE "IN GET-REM SECTION." TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       GET-REM-001.
           MOVE "                   " TO F-NAMEFIELD.
           MOVE "NUMBER" TO F-FIELDNAME.
           MOVE 6 TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           MOVE 2 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO PRNREM-NUMBER
           IF F-EXIT-CH = X"0C"
                 MOVE WS-SAVE TO PRNREM-NUMBER
                 PERFORM START-REM-RECORD
                 PERFORM READ-REM-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-REM-003
              ELSE
                 PERFORM CLEAR-REM-FORM
                 PERFORM GET-REM-003 THRU GET-REM-005
                 GO TO GET-REM-999.
           IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
           IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-REM-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-REM-001.
           IF F-EXIT-CH NOT = X"0A" AND NOT = X"1A"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-REM-000.
           PERFORM READ-REM-RECORD.
           IF NEW-NO = "Y"
              PERFORM GET-REM-003
              GO TO GET-REM-999.
       GET-REM-003.
           MOVE "NUMBER"    TO F-FIELDNAME
           MOVE 6           TO F-CBFIELDNAME
           MOVE PRNREM-NUMBER TO F-EDNAMEFIELDANAL
           MOVE 2           TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ANALYSIS.
       GET-REM-005.
           MOVE "NAME"    TO F-FIELDNAME
           MOVE 4         TO F-CBFIELDNAME
           MOVE PRNREM-NAME TO F-NAMEFIELD
           MOVE 20        TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "TYPE"    TO F-FIELDNAME
           MOVE 4         TO F-CBFIELDNAME
           MOVE PRNREM-TYPE TO F-EDNAMEFIELDANAL
           MOVE 2         TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ANALYSIS.

           MOVE "DESC"     TO F-FIELDNAME
           MOVE 4          TO F-CBFIELDNAME
           MOVE PRNREM-DESC  TO F-NAMEFIELD
           MOVE 20         TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "PROMT-PAPER"     TO F-FIELDNAME
           MOVE 11                TO F-CBFIELDNAME
           MOVE PRNREM-Prompt-PAPER TO F-NAMEFIELD
           MOVE 1                 TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "COMP"     TO F-FIELDNAME
           MOVE 4          TO F-CBFIELDNAME
           MOVE PRNREM-COMP  TO F-NAMEFIELD
           MOVE 2          TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "BOLD"     TO F-FIELDNAME
           MOVE 4          TO F-CBFIELDNAME
           MOVE PRNREM-BOLD  TO F-NAMEFIELD
           MOVE 2          TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "UNBOLD"    TO F-FIELDNAME
           MOVE 6           TO F-CBFIELDNAME
           MOVE PRNREM-UNBOLD TO F-NAMEFIELD
           MOVE 2           TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "NORMAL"     TO F-FIELDNAME
           MOVE 6            TO F-CBFIELDNAME
           MOVE PRNREM-NORMAL  TO F-NAMEFIELD
           MOVE 2            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "EIGHT"     TO F-FIELDNAME
           MOVE 5           TO F-CBFIELDNAME
           MOVE PRNREM-EIGHT  TO F-NAMEFIELD
           MOVE 8           TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "ELEVEN"     TO F-FIELDNAME
           MOVE 6            TO F-CBFIELDNAME
           MOVE PRNREM-ELEVEN  TO F-NAMEFIELD
           MOVE 8            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "FOUR"       TO F-FIELDNAME
           MOVE 4            TO F-CBFIELDNAME
           MOVE PRNREM-FOUR    TO F-NAMEFIELD
           MOVE 8            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       GET-REM-999.
           EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNT-NAME.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-050.
            IF PRNT-NUMBER NOT > " "
               MOVE "A PRINTER NAME MUST BE ENTERED, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
       FILL-005.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TYPE" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2     TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO PRNT-TYPE F-EDNAMEFIELDANAL.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF PRNT-TYPE NOT > 0
               MOVE "THIS TYPE MUST BE > ZERO, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-005.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-005.
       FILL-008.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DESC" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20     TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNT-DESC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-005.
            IF PRNT-DESC NOT > " "
               MOVE "THIS DESC MUST BE ENTERED, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-008.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-008.
       FILL-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PROMT-PAPER" TO F-FIELDNAME.
            MOVE 11  TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1   TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNT-Prompt-PAPER.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-008.
            IF PRNT-Prompt-PAPER NOT = "S" AND NOT = "Y"
               MOVE "THIS FIELD MUST BE S OR Y, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
       FILL-020.
            IF F-EXIT-CH NOT = X"1D"
                GO TO FILL-001.
            IF F-EXIT-CH = X"1D"
                 PERFORM SET-UP-PRN-PARAMS
                 PERFORM GET-005
                 GO TO FILL-001.
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COMP" TO F-FIELDNAME.
            MOVE 4  TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2   TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNT-COMP.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
      *      IF PRNT-COMP NOT > " "
      *         MOVE "THIS FIELD MUST BE HEXADECIMAL, PLEASE RE-ENTER."
      *         TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         GO TO FILL-030.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-030.
       FILL-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BOLD" TO F-FIELDNAME.
            MOVE 4  TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2   TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNT-BOLD.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
      *      IF PRNT-BOLD NOT > " "
      *         MOVE "THIS FIELD MUST BE HEXADECIMAL, PLEASE RE-ENTER."
      *         TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         GO TO FILL-040.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-040.
       FILL-045.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "UNBOLD" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2        TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNT-UNBOLD.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
      *      IF PRNT-UNBOLD NOT > " "
      *         MOVE "THIS FIELD MUST BE HEXADECIMAL, PLEASE RE-ENTER."
      *         TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         GO TO FILL-045.
            IF F-EXIT-CH = X"01"
               GO TO FILL-040.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-045.
       FILL-050.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NORMAL" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2        TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNT-NORMAL.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
      *      IF PRNT-NORMAL NOT > " "
      *         MOVE "THIS FIELD MUST BE HEXADECIMAL, PLEASE RE-ENTER."
      *         TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         GO TO FILL-050.
            IF F-EXIT-CH = X"01"
               GO TO FILL-045.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-050.
       FILL-055.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "EIGHT"  TO F-FIELDNAME.
            MOVE 5        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 8        TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNT-EIGHT.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
      *      IF PRNT-EIGHT NOT > " "
      *         MOVE "THIS FIELD MUST BE HEXADECIMAL, PLEASE RE-ENTER."
      *         TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         GO TO FILL-055.
            IF F-EXIT-CH = X"01"
               GO TO FILL-050.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-055.
       FILL-060.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ELEVEN" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 8        TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNT-ELEVEN.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
      *      IF PRNT-ELEVEN NOT > " "
      *         MOVE "THIS FIELD MUST BE HEXADECIMAL, PLEASE RE-ENTER."
      *         TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         GO TO FILL-060.
            IF F-EXIT-CH = X"01"
               GO TO FILL-055.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-060.
       FILL-065.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "FOUR" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 8      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNT-FOUR.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
      *      IF PRNT-FOUR NOT > " "
      *         MOVE "THIS FIELD MUST BE HEXADECIMAL, PLEASE RE-ENTER."
      *         TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         GO TO FILL-065.
            IF F-EXIT-CH = X"01"
               GO TO FILL-055.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-065.

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       FILL-REM-DATA SECTION.
       FILL-REM-001.
      *     MOVE "IN FILL-REM SECTION." TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
            IF WS-END = "Y"
                GO TO FILL-REM-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNREM-NAME.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-REM-050.
            IF PRNREM-NUMBER NOT > " "
               MOVE "A PRINTER NAME MUST BE ENTERED, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-001.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-REM-RECORD
               PERFORM READ-REM-NEXT
               PERFORM GET-REM-003 THRU GET-REM-005
               GO TO FILL-REM-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-001.
       FILL-REM-005.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TYPE" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2     TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO PRNREM-TYPE F-EDNAMEFIELDANAL.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-REM-001.
            IF PRNREM-TYPE NOT > 0
               MOVE "THIS TYPE MUST BE > ZERO, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-005.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-REM-RECORD
               PERFORM READ-REM-NEXT
               PERFORM GET-REM-003 THRU GET-REM-005
               GO TO FILL-REM-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-005.
       FILL-REM-008.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DESC" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20     TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNREM-DESC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-REM-005.
            IF PRNREM-DESC NOT > " "
               MOVE "THIS DESC MUST BE ENTERED, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-008.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-REM-RECORD
               PERFORM READ-REM-NEXT
               PERFORM GET-REM-003 THRU GET-REM-005
               GO TO FILL-REM-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-008.
       FILL-REM-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PROMT-PAPER" TO F-FIELDNAME.
            MOVE 11  TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1   TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNREM-Prompt-PAPER.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-REM-008.
            IF PRNREM-Prompt-PAPER NOT = "S" AND NOT = "Y"
               MOVE "THIS FIELD MUST BE S OR Y, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-REM-RECORD
               PERFORM READ-REM-NEXT
               PERFORM GET-REM-003 THRU GET-REM-005
               GO TO FILL-REM-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-010.
       FILL-REM-020.
            IF F-EXIT-CH NOT = X"1D"
                GO TO FILL-REM-001.
            IF F-EXIT-CH = X"1D"
                 PERFORM SET-UP-PRN-PARAMS
                 PERFORM GET-REM-005
                 GO TO FILL-REM-001.
       FILL-REM-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COMP" TO F-FIELDNAME.
            MOVE 4  TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2   TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNREM-COMP.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF PRNREM-COMP NOT > " "
               MOVE "THIS FIELD MUST BE HEXADECIMAL, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-030.
            IF F-EXIT-CH = X"01"
               GO TO FILL-REM-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-REM-RECORD
               PERFORM READ-REM-NEXT
               PERFORM GET-REM-003 THRU GET-REM-005
               GO TO FILL-REM-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-030.
       FILL-REM-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BOLD" TO F-FIELDNAME.
            MOVE 4  TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2   TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNREM-BOLD.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF PRNREM-BOLD NOT > " "
               MOVE "THIS FIELD MUST BE HEXADECIMAL, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-040.
            IF F-EXIT-CH = X"01"
               GO TO FILL-REM-030.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-REM-RECORD
               PERFORM READ-REM-NEXT
               PERFORM GET-REM-003 THRU GET-REM-005
               GO TO FILL-REM-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-040.
       FILL-REM-045.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "UNBOLD" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2        TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNREM-UNBOLD.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF PRNREM-UNBOLD NOT > " "
               MOVE "THIS FIELD MUST BE HEXADECIMAL, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-045.
            IF F-EXIT-CH = X"01"
               GO TO FILL-REM-040.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-REM-RECORD
               PERFORM READ-REM-NEXT
               PERFORM GET-REM-003 THRU GET-REM-005
               GO TO FILL-REM-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-045.
       FILL-REM-050.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NORMAL" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2        TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNREM-NORMAL.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF PRNREM-NORMAL NOT > " "
               MOVE "THIS FIELD MUST BE HEXADECIMAL, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-050.
            IF F-EXIT-CH = X"01"
               GO TO FILL-REM-045.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-REM-RECORD
               PERFORM READ-REM-NEXT
               PERFORM GET-REM-003 THRU GET-REM-005
               GO TO FILL-REM-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-050.
       FILL-REM-055.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "EIGHT"  TO F-FIELDNAME.
            MOVE 5        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 8        TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNREM-EIGHT.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF PRNREM-EIGHT NOT > " "
               MOVE "THIS FIELD MUST BE HEXADECIMAL, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-055.
            IF F-EXIT-CH = X"01"
               GO TO FILL-REM-050.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-REM-RECORD
               PERFORM READ-REM-NEXT
               PERFORM GET-REM-003 THRU GET-REM-005
               GO TO FILL-REM-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-055.
       FILL-REM-060.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ELEVEN" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 8        TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNREM-ELEVEN.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF PRNREM-ELEVEN NOT > " "
               MOVE "THIS FIELD MUST BE HEXADECIMAL, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-060.
            IF F-EXIT-CH = X"01"
               GO TO FILL-REM-055.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-REM-RECORD
               PERFORM READ-REM-NEXT
               PERFORM GET-REM-003 THRU GET-REM-005
               GO TO FILL-REM-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-060.
       FILL-REM-065.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "FOUR" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 8      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO PRNREM-FOUR.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF PRNREM-FOUR NOT > " "
               MOVE "THIS FIELD MUST BE HEXADECIMAL, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-065.
            IF F-EXIT-CH = X"01"
               GO TO FILL-REM-055.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-REM-RECORD
               PERFORM READ-REM-NEXT
               PERFORM GET-REM-003 THRU GET-REM-005
               GO TO FILL-REM-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-REM-RECORD
               PERFORM CLEAR-REM-FORM
               GO TO FILL-REM-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-REM-065.

            GO TO FILL-REM-001.
       FILL-REM-999.
            EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO PRNT-NAME
                         PRNT-DESC
                         PRNT-PROMPT-PAPER.
       CLSC-010.
             MOVE SPACES TO PRNT-COMP
                            PRNT-BOLD
                            PRNT-UNBOLD
                            PRNT-NORMAL
                            PRNT-EIGHT
                            PRNT-ELEVEN
                            PRNT-FOUR.
       CLSC-050.
             MOVE 0         TO PRNT-TYPE.
             MOVE WS-NUMBER TO PRNT-NUMBER.
       CLSC-999.
             EXIT.      
      *
       SET-UP-PRN-PARAMS SECTION.
       SUPP-000.
             IF PRNT-TYPE = 6 OR 7
                PERFORM CLSC-010
                GO TO SUPP-999.
             MOVE X"0F"        TO PRNT-COMP.
             MOVE X"0E"        TO PRNT-BOLD.
             MOVE X"14"        TO PRNT-UNBOLD.
             MOVE X"12"        TO PRNT-NORMAL.
             MOVE X"1B430008"  TO PRNT-EIGHT.
             MOVE X"1B430011"  TO PRNT-ELEVEN.
             MOVE X"1B430004"  TO PRNT-FOUR.
       SUPP-999.
             EXIT.      
      *
       SET-UP-PRNREM-PARAMS SECTION.
       SUPP-REM-000.
             IF PRNT-TYPE = 6 OR 7
                PERFORM CLSCR-010
                GO TO SUPP-REM-999.
             MOVE X"0F"        TO PRNREM-COMP.
             MOVE X"0E"        TO PRNREM-BOLD.
             MOVE X"14"        TO PRNREM-UNBOLD.
             MOVE X"12"        TO PRNREM-NORMAL.
             MOVE X"1B430008"  TO PRNREM-EIGHT.
             MOVE X"1B430011"  TO PRNREM-ELEVEN.
             MOVE X"1B430004"  TO PRNREM-FOUR.
       SUPP-REM-999.
             EXIT.      
      *
       CLEAR-REM-FORM SECTION.
       CLSCR-000.
             MOVE " " TO PRNREM-NAME
                         PRNREM-DESC
                         PRNREM-PROMPT-PAPER.
       CLSCR-010.
             MOVE SPACES TO PRNREM-COMP
                            PRNREM-BOLD
                            PRNREM-UNBOLD
                            PRNREM-NORMAL
                            PRNREM-EIGHT
                            PRNREM-ELEVEN
                            PRNREM-FOUR.
       CLSCR-050.
             MOVE 0   TO PRNREM-TYPE.
             MOVE WS-NUMBER TO PRNREM-NUMBER.
       CLSCR-999.
             EXIT.      
      *
       DELETE-RECORD SECTION.
       DDR-000.
            IF NEW-NO = "Y"
                GO TO DDR-999.
       DDR-010.
            DELETE PRINTER-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-PRNT-ST1 NOT = 0
               MOVE 0 TO WS-PRNT-ST1
                MOVE "PRINTER BUSY ON DELETE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
               GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       DELETE-REM-RECORD SECTION.
       DRR-000.
            IF NEW-NO = "Y"
                GO TO DRR-999.
       DRR-010.
            DELETE PRINTER-REMOTE
               INVALID KEY NEXT SENTENCE.
            IF WS-PRNREM-ST1 NOT = 0
               MOVE 0 TO WS-PRNREM-ST1
                MOVE "PRINTER REMOTE BUSY ON DELETE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
               GO TO DRR-010.
       DRR-999.
           EXIT.
      *
       REWRITE-RECORD SECTION.
       RDR-005.
            IF NEW-NO = "Y"
               GO TO RDR-020.
       RDR-010.
            REWRITE PRINTER-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-PRNT-ST1 NOT = 0
                MOVE 0 TO WS-PRNT-ST1
                MOVE "PRINTER BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-010.
            GO TO RDR-999.
       RDR-020.
            WRITE PRINTER-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-PRNT-ST1 NOT = 0
                MOVE 0 TO WS-PRNT-ST1
                MOVE "PRINTER BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-010.
       RDR-999.
            EXIT.
      *
       REWRITE-REM-RECORD SECTION.
       RWRR-005.
            IF NEW-NO = "Y"
               GO TO RWRR-020.
       RWRR-010.
            REWRITE PRINTERREMOTE-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-PRNREM-ST1 NOT = 0
                MOVE 0 TO WS-PRNREM-ST1
                MOVE "PRINTER REMOTE BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RWRR-010.
            GO TO RWRR-999.
       RWRR-020.
            WRITE PRINTERREMOTE-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-PRNREM-ST1 NOT = 0
                MOVE 0 TO WS-PRNREM-ST1
                MOVE "PRINTER REMOTE BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RWRR-010.
       RWRR-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE PRNT-NUMBER TO WS-NUMBER.
           START PRINTER-MASTER KEY NOT < PRNT-KEY
             INVALID KEY NEXT SENTENCE.
        RD-010.
           READ PRINTER-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-PRNT-ST1 = 23 OR 35 OR 47 OR 49
                MOVE 0 TO WS-PRNT-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-NO
                MOVE WS-NUMBER TO PRNT-NUMBER
                GO TO RD-999.
           IF WS-PRNT-ST1 NOT = 0
                MOVE "PRINTER BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE WS-PRNT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE 0 TO WS-PRNT-ST1
                GO TO RD-010.
           MOVE "N" TO NEW-NO.
           MOVE PRNT-NUMBER TO WS-SAVE.
       RD-999.
             EXIT.
      *
       READ-REM-RECORD SECTION.
       RDR-000.
           MOVE PRNREM-NUMBER TO WS-NUMBER.
           START PRINTER-REMOTE KEY NOT < PRNREM-KEY
             INVALID KEY NEXT SENTENCE.
       RDR-010.
           READ PRINTER-REMOTE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-PRNREM-ST1 = 23 OR 35 OR 47 OR 49
                MOVE 0 TO WS-PRNREM-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-NO
                MOVE WS-NUMBER TO PRNREM-NUMBER
                GO TO RDR-999.
           IF WS-PRNREM-ST1 NOT = 0
                MOVE "PRINTER REMOTE BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE WS-PRNREM-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE 0 TO WS-PRNREM-ST1
                GO TO RDR-010.
           MOVE "N" TO NEW-NO.
           MOVE PRNREM-NUMBER TO WS-SAVE.
       RDR-999.
             EXIT.
      *
       START-RECORD SECTION.
       STR-000.
           MOVE WS-NUMBER TO PRNT-NUMBER.
           START PRINTER-MASTER KEY NOT < PRNT-KEY
              INVALID KEY NEXT SENTENCE.
       STR-999.
             EXIT.
      *
       START-REM-RECORD SECTION.
       STRR-000.
           MOVE WS-NUMBER TO PRNREM-NUMBER.
           START PRINTER-REMOTE KEY NOT < PRNREM-KEY
              INVALID KEY NEXT SENTENCE.
       STRR-999.
             EXIT.
      *
       READ-NEXT SECTION.
       RNX-001.
           MOVE 0 TO WS-PRNT-ST1.
       RNX-005.
           IF PRNT-NAME = " "
               PERFORM START-RECORD.
           READ PRINTER-MASTER NEXT WITH LOCK
             AT END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO PRNT-NUMBER
                           WS-NUMBER
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF WS-PRNT-ST1 = 23 OR 35 OR 49 OR 51 
               MOVE 0 TO WS-PRNT-ST1
               MOVE "PRINTER BUSY ON READ-NEXT, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNX-005.
           IF WS-PRNT-ST1 NOT = 0
               MOVE 0 TO WS-PRNT-ST1
               PERFORM START-RECORD
               GO TO RNX-005.
           MOVE PRNT-NUMBER TO WS-NUMBER
                               WS-SAVE.
           MOVE "N" TO NEW-NO.
       RNX-999.
           EXIT.
      *
       READ-REM-NEXT SECTION.
       RNXR-001.
           MOVE 0 TO WS-PRNREM-ST1.
       RNXR-005.
           IF PRNREM-NAME = " "
               PERFORM START-REM-RECORD.
           READ PRINTER-REMOTE NEXT WITH LOCK
             AT END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO PRNREM-NUMBER
                           WS-NUMBER
               MOVE "Y" TO WS-END
               GO TO RNXR-999.
           IF WS-PRNREM-ST1 = 23 OR 35 OR 49 OR 51 
               MOVE 0 TO WS-PRNREM-ST1
               MOVE "PRINTER REMOTE BUSY ON READ-NEXT, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNXR-005.
           IF WS-PRNREM-ST1 NOT = 0
               MOVE 0 TO WS-PRNREM-ST1
               PERFORM START-RECORD
               GO TO RNXR-005.
           MOVE PRNREM-NUMBER TO WS-NUMBER
                               WS-SAVE.
           MOVE "N" TO NEW-NO.
       RNXR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            IF WS-ACCEPT = "3" OR = "7" OR = "8"
               GO TO OPEN-005.
            OPEN I-O PRINTER-MASTER.
            IF WS-PRNT-ST1 NOT = 0
               MOVE 0 TO WS-PRNT-ST1
               MOVE "PRINTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-005.
            IF WS-ACCEPT = "1"
               GO TO OPEN-010.
            OPEN I-O PRINTER-REMOTE.
            IF WS-PRNREM-ST1 NOT = 0
               MOVE 0 TO WS-PRNREM-ST1
               MOVE "PRINTER REMOTE BUSY ON OPEN I-O, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-006.
             GO TO OPEN-010.
       OPEN-006.
            OPEN OUTPUT PRINTER-REMOTE.
            IF WS-PRNREM-ST1 NOT = 0
               MOVE 0 TO WS-PRNREM-ST1
               MOVE "PRINTER REMOTE BUSY ON OPEN OUTPUT,'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CoPrntMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            IF WS-ACCEPT = "1"
               CLOSE PRINTER-MASTER.
            IF WS-ACCEPT = "2"
               CLOSE PRINTER-REMOTE.
           EXIT PROGRAM.
      *      STOP RUN.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAnalysis".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "CTOSCobolAccept".
       Copy "ClearScreen".
       Copy "ErrorMessage".
      *
      * END-OF-JOB
