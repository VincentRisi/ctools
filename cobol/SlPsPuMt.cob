        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlPsPuMt.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectSlRegister".
          Copy "SelectCoPullers".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdRegister.
           COPY ChlfdPullers.
      *
       WORKING-STORAGE SECTION.
       77  WS-INQ-PROGRAM       PIC X(8) VALUE "SlInOrIq".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-PSDIS             PIC Z9.
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-PRINTER           PIC X(12) VALUE " ".
       77  WS-TYPE-OF-KEY       PIC X(20) VALUE " ".
       77  WS-TYPE              PIC 9 VALUE 0.
       77  WS-KEY               PIC X VALUE " ".
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-INVOICE           PIC 9(6).
       77  WS-NO-CH             PIC 9 VALUE 0.
       77  W-INVALID-TYPE       PIC X VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       01  WS-PB-FIELDS-NAMES.
         02  WS-PB-FIELDS OCCURS 15.
           03  WS-PSLIP        PIC 9(6).
           03  WS-INITIAL      PIC X(2).
           03  WS-BIN          PIC X(6).
           03  WS-TRAN-DATE    PIC 9(8).
           03  WS-ACCOUNT      PIC 9(7).
           03  WS-NAME         PIC X(20).
           03  WS-PS-AMT       PIC 9(8)V99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1     PIC 99.
       01  WS-PULLERS-STATUS.
           03  WS-PULLERS-ST1  PIC 99.
       Copy "WsDateInfo".
      *
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-020.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-020.
      *
       GET-DATA SECTION.
       GET-000.
      ***********************
      *TYPE 4=P/SLIP        *
      *     7=B/M           *
      ***********************
            MOVE 4 TO INCR-TRANS
                      WS-TYPE.
       GET-001.
            MOVE "TYPE"  TO F-FIELDNAME.
            MOVE 4       TO F-CBFIELDNAME.
            MOVE WS-TYPE TO F-NAMEFIELD
            MOVE 1       TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            GO TO GET-005.
       GET-002. 
            MOVE "TYPE" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            MOVE 1            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO INCR-TRANS
                                   WS-TYPE.
            IF WS-TYPE NOT = 4 AND NOT = 7
                MOVE "THE ONLY VALID ENTRY IS 4 OR 7, RE-ENTER"
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-002.
       GET-005.
            PERFORM ERROR-020.
            MOVE 2610 TO POS
            DISPLAY
           "ENTER THE P/SLIP # AND <Pgdn> TO READ NUMBERS ONWARDS."
              AT POS.
            MOVE "N" TO WS-ANSWER
                        W-INVALID-TYPE.
            MOVE " " TO WS-NEWINPUT.
            MOVE 1 TO F-INDEX.
            MOVE "TRANS" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-002.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            MOVE 6            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO INCR-INVOICE
                                 WS-INVOICE.
            IF INCR-INVOICE = " " OR = "0" OR = 0
                CLOSE INCR-REGISTER
                CALL WS-INQ-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQ-PROGRAM
                PERFORM CLEAR-SCREEN
                OPEN I-O INCR-REGISTER
                PERFORM DISPLAY-FORM
                GO TO GET-005.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0C"
               MOVE "YOU MAY ONLY PRESS <RETURN> OR <Pgdn> to Continue."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-005.
            IF F-EXIT-CH = X"0A"
               MOVE "1" TO WS-NEWINPUT.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            MOVE 2610 TO POS
            DISPLAY WS-MESSAGE AT POS.
       GET-900.
            PERFORM READ-TRANSACTIONS.
            IF WS-NEWINPUT = "8"
                GO TO GET-950.
            PERFORM ERROR1-020.
            MOVE SUB-1 TO SUB-2.
            IF SUB-2 NOT > 1
              MOVE "NO FURTHAR TRANSACTIONS, 'ESC' TO END."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO GET-999.
            PERFORM SCROLLING.
            PERFORM FILL-DATA.
       GET-950.
            IF WS-PSLIP (15) > 0
               MOVE WS-PSLIP (15) TO WS-INVOICE.
            PERFORM CLEAR-TRANSACTIONS.
            IF WS-ABOVE-BODY = "8"
               GO TO GET-900.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-000.
           MOVE 1 TO SUB-1 F-INDEX.
           MOVE " " TO WS-ABOVE-BODY.
       FILL-005.
           IF SUB-1 < 1
              MOVE 1 TO SUB-1 F-INDEX.
          IF WS-PSLIP (SUB-1) = 0
              GO TO FILL-060.
       FILL-010.
           MOVE "                    " TO F-NAMEFIELD.
           MOVE "BIN" TO F-FIELDNAME
           MOVE 3     TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           MOVE 6     TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA
           MOVE F-NAMEFIELD TO WS-BIN (SUB-1).
           PERFORM ERROR-020.
       FILL-013.
           MOVE "                    " TO F-NAMEFIELD.
           MOVE "PBINITIAL" TO F-FIELDNAME
           MOVE 9           TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           MOVE 2           TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA
           MOVE F-NAMEFIELD TO WS-INITIAL (SUB-1) PU-INITIAL.
           IF F-NAMEFIELD = " "
              GO TO FILL-014.
           
           PERFORM READ-PULLERS.
           IF PU-INITIAL = " "
              MOVE "THIS IS NOT A VALID STOCK PULLER, RE-ENTER."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO FILL-013.
           IF PU-INITIAL > " "
            IF WS-BIN (SUB-1) = " "
              MOVE "A VALID BIN NUMBER MUST BE ENTERED, PLEASE RE-ENTER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO FILL-010.
           PERFORM ERROR-020.
       FILL-014.
           IF F-EXIT-CH = X"01" AND F-INDEX = 1
              MOVE "1" TO WS-ABOVE-BODY
              GO TO FILL-999.
           IF F-EXIT-CH = X"01" AND F-INDEX > 1
              SUBTRACT 1 FROM F-INDEX SUB-1 SUB-2
              GO TO FILL-005.
      *****************
      *TAB KEY = X"09"*
      *****************
           IF F-EXIT-CH = X"09"
              GO TO FILL-060.
              
           IF F-EXIT-CH = X"12" AND F-INDEX < 15
              ADD 1 TO F-INDEX SUB-1 SUB-2
              GO TO FILL-005.
           IF F-EXIT-CH = X"0B" AND F-INDEX < 15
              ADD 1 TO F-INDEX SUB-1 SUB-2
              GO TO FILL-005.
           IF F-EXIT-CH = X"0A" AND F-INDEX < 15
              ADD 1 TO F-INDEX SUB-1 SUB-2
              GO TO FILL-005.
           IF F-EXIT-CH = X"0B" AND F-INDEX = 15
              MOVE "8" TO WS-ABOVE-BODY
              GO TO FILL-060.
           IF F-EXIT-CH = X"0A" AND F-INDEX = 15
              MOVE "8" TO WS-ABOVE-BODY
              GO TO FILL-060.
           IF F-EXIT-CH = X"12" AND F-INDEX = 15
              MOVE "8" TO WS-ABOVE-BODY
              GO TO FILL-060.
           IF F-EXIT-CH = X"07"
              PERFORM FILL-060
              GO TO FILL-999.
       FILL-050.
            IF SUB-1 > 14
               GO TO FILL-060.
            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 > SUB-2
               MOVE SUB-1 TO SUB-2.
            GO TO FILL-005.
       FILL-060.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
            MOVE 2910 TO POS
            DISPLAY "WRITING CHANGES TO P/SLIP-FILE....." AT POS
            PERFORM REWRITE-REGISTER.
       FILL-999.
            EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-005.
           MOVE WS-INVOICE TO INCR-INVOICE.
      *    MOVE 4          TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
                INVALID KEY NEXT SENTENCE.
           MOVE 1 TO SUB-1 F-INDEX.
       RDTR-010.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10 OR = 91
               GO TO RDTR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RDTR-010.
               
           IF WS-NEWINPUT = "1"
            IF INCR-PRINTED NOT = "P" AND NOT = "N" AND NOT = "S"
                        AND NOT = "L" AND NOT = "Y"
               MOVE
              "INVALID P/SLIP NUMBER ENTERED, 'ESC' TO RE-ENTER."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
      *         MOVE INCR-PRINTED TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         MOVE INCR-INVOICE TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               MOVE "8" TO WS-NEWINPUT
               GO TO RDTR-999.
               
           IF WS-NEWINPUT = "1"
            IF INCR-PULLBY NOT = "  "
             IF INCR-PULL-DATE NOT = 0
               MOVE
              "THIS P/SLIP NUMBER HAS ALREADY BEEN ALLOCATED, RE-ENTER."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE INCR-PULLBY TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE INCR-PULL-DATE TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "8" TO WS-NEWINPUT
               GO TO RDTR-999.
            
           IF WS-TYPE = 4
            IF INCR-TRANS NOT = 4 
               GO TO RDTR-999.
           IF WS-TYPE = 7
            IF INCR-TRANS NOT = 7
               GO TO RDTR-999.

           IF INCR-PRINTED NOT = "P" AND NOT = "S" AND NOT = "N" 
                       AND NOT = "L" AND NOT = "Y"
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next P/SLIP...." AT POS
               GO TO RDTR-010.
           IF INCR-PULL-DATE > 0
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next P/SLIP...." AT POS
               GO TO RDTR-010.
               
           MOVE INCR-INVOICE     TO WS-PSLIP (SUB-1)
           MOVE INCR-DATE        TO WS-TRAN-DATE (SUB-1)
           MOVE INCR-ACCOUNT     TO WS-ACCOUNT (SUB-1)
           MOVE INCR-NAME        TO WS-NAME (SUB-1)
           MOVE INCR-PULLBY      TO WS-INITIAL (SUB-1)
           MOVE INCR-INVCRED-AMT TO WS-PS-AMT (SUB-1).
           
           
           IF WS-NEWINPUT = "1"
              ADD 1 TO SUB-1
              GO TO RDTR-999.
           IF SUB-1 < 15
              ADD 1 TO SUB-1
              MOVE 2610 TO POS
              DISPLAY "Number Of P/Slips Found :" AT POS
              ADD 26 TO POS
              MOVE SUB-1 TO WS-PSDIS
              DISPLAY WS-PSDIS AT POS
              GO TO RDTR-010.
           PERFORM ERROR-020.
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
       RDTR-999.
           EXIT.
      *
       REWRITE-REGISTER SECTION.
       RWR-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE 1 TO SUB-1.
           MOVE WS-TYPE TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
                INVALID KEY NEXT SENTENCE.
       RWR-008.
           IF WS-INITIAL (SUB-1) = "  "
                GO TO RWR-070.
           MOVE WS-PSLIP (SUB-1) TO INCR-INVOICE.
       RWR-010.
           READ INCR-REGISTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49 OR 91
               GO TO RWR-999.
           IF WS-INCR-ST1 NOT = 0
             MOVE "REGISTER FILE BUSY ON READ-LOCK, 'ESC' TO RETRY."
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-INCR-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             MOVE 0 TO WS-INCR-ST1
             GO TO RWR-010.
           ACCEPT WS-TIME FROM TIME.
           MOVE WS-INITIAL (SUB-1) TO INCR-PULLBY
           MOVE WS-BIN (SUB-1)     TO INCR-BIN
           MOVE WS-DATE            TO INCR-PULL-DATE
           MOVE WS-TIME            TO INCR-PULL-TIME.
           IF INCR-PRINTED NOT = "L" AND NOT = "Y"
              MOVE "P" TO INCR-PRINTED.
      *THE ABOVE TWO LINES ADDED TO MAKE SURE THAT AT THE TIME OF
      *FLAGGING RECORD AS PULLED THE FIELD WILL BE P FOR FUTURE REPORTS
       RWR-060.
           REWRITE INCR-REC
                INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
             MOVE "REGISTER BUSY ON REWRITE, 'ESC' TO RETRY."
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-INCR-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             MOVE 0 TO WS-INCR-ST1
             GO TO RWR-060.
       RWR-070.
           IF SUB-1 < 15
              ADD 1 TO SUB-1
            IF WS-PSLIP (SUB-1) NOT = 0
              GO TO RWR-008.
       RWR-999.
           EXIT.
      *
       READ-PULLERS SECTION.
       RPULL-000.
           START PULLER-MASTER KEY NOT < PU-KEY.
       RPULL-010.
           READ PULLER-MASTER
                 INVALID KEY NEXT SENTENCE.
           IF WS-PULLERS-ST1 = 23 OR 35 OR 49
                MOVE " " TO PU-INITIAL
                GO TO RPULL-999.
           IF WS-PULLERS-ST1 NOT = 0
             MOVE "PULLER RECORD BUSY ON READ, 'ESC' TO RETRY"
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-PULLERS-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             MOVE 0 TO WS-PULLERS-ST1
             GO TO RPULL-010.
           MOVE PU-INITIAL TO WS-INITIAL (SUB-1).
       RPULL-999.
             EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            MOVE 1 TO SUB-1 F-INDEX.
       SCROLL-005.
            IF WS-PSLIP (SUB-1) = 0
                GO TO SCROLL-999.
                
            MOVE "PSLIP"          TO F-FIELDNAME.
            MOVE 5                TO F-CBFIELDNAME.
            MOVE WS-PSLIP (SUB-1) TO F-EDNAMEFIELDNUM.
            MOVE 6                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "PBINITIAL"        TO F-FIELDNAME
            MOVE 9                  TO F-CBFIELDNAME
            MOVE WS-INITIAL (SUB-1) TO F-NAMEFIELD
            MOVE 2                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BIN" TO F-FIELDNAME.
            MOVE 3     TO F-CBFIELDNAME.
            MOVE " "   TO F-NAMEFIELD.
            MOVE 2     TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"               TO F-FIELDNAME
            MOVE 4                    TO F-CBFIELDNAME
            MOVE WS-TRAN-DATE (SUB-1) TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE         TO F-NAMEFIELD
            MOVE 10                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNO"            TO F-FIELDNAME.
            MOVE 5                  TO F-CBFIELDNAME.
            MOVE WS-ACCOUNT (SUB-1) TO F-NAMEFIELD.
            MOVE 7                  TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNAME"       TO F-FIELDNAME.
            MOVE 7               TO F-CBFIELDNAME.
            MOVE WS-NAME (SUB-1) TO F-NAMEFIELD.
            IF WS-TYPE = 4
                MOVE 20          TO F-CBFIELDLENGTH
            ELSE
                MOVE 15          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT"          TO F-FIELDNAME
            MOVE 6                 TO F-CBFIELDNAME
            MOVE WS-PS-AMT (SUB-1) TO F-EDNAMEFIELD99Mil
            MOVE 11                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.
            
            IF SUB-1 < 15
               ADD 1 TO SUB-1 F-INDEX
               GO TO SCROLL-005.
       SCROLL-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO SUB-1 F-INDEX.
       CLTR-010.
            MOVE "PSLIP" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            MOVE " "     TO F-NAMEFIELD.
            MOVE 6       TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PBINITIAL" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            MOVE " "         TO F-NAMEFIELD.
            MOVE 2           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BIN" TO F-FIELDNAME.
            MOVE 3     TO F-CBFIELDNAME.
            MOVE " "   TO F-NAMEFIELD.
            MOVE 2     TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            MOVE " "    TO F-NAMEFIELD.
            MOVE 10     TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            MOVE " "     TO F-NAMEFIELD.
            MOVE 7       TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNAME" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 20        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 11       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            IF SUB-1 < 15
                ADD 1 TO SUB-1 F-INDEX
                MOVE " " TO WS-PB-FIELDS (SUB-1)
                GO TO CLTR-010.
       CLTR-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-006.
            OPEN I-O PULLER-MASTER.
            IF WS-PULLERS-ST1 NOT = 0
               MOVE 0 TO WS-PULLERS-ST1
               MOVE "PULLBY FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-006.
       OPEN-001.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-008.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlPsPuMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-900.
            CLOSE INCR-REGISTER
                  PULLER-MASTER.
            EXIT PROGRAM.
      *       STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldDate".
       Copy "WriteField99Mil".
       Copy "WriteFieldQty".
       Copy "GetSystemY2KDate".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "TimeChecking".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
      *
      * END-OF-JOB.
