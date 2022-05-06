        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlAreaMt.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectSlRegister".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdRegister.
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
       01  WS-PB-FIELDS-NAME.
         02  WS-PB-FIELDS OCCURS 15.
           03  WS-PSLIP        PIC 9(6) VALUE 0.
           03  WS-AREA         PIC X(1) VALUE " ".
           03  WS-BIN          PIC X(6) VALUE " ".
           03  WS-PULLBY       PIC X(2) VALUE " ".
           03  WS-PART-ORDERS  PIC X(1) VALUE " ".
           03  WS-TRAN-DATE    PIC 9(8) VALUE 0.
           03  WS-ACCOUNT      PIC 9(7) VALUE 0.
           03  WS-NAME         PIC X(20) VALUE " ".
           03  WS-PS-AMT       PIC 9(8)V99 VALUE 0.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1     PIC 99.
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
      *TYPE 1=INVOICE       *
      *     4=PSLIP         *
      ***********************
            MOVE 4 TO INCR-TRANS
                      WS-TYPE.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            PERFORM CLEAR-MEMORY.
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
            IF WS-TYPE NOT = 1 AND NOT = 4
                MOVE "THE ONLY VALID ENTRY IS 1 OR 4, RE-ENTER"
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-002.
       GET-005.
            PERFORM ERROR-020.
            MOVE 2610 TO POS
            IF WS-TYPE = 4
               DISPLAY
           "ENTER THE P/SLIP # AND <PgDn> TO READ NUMBERS ONWARDS."
              AT POS
            ELSE
               DISPLAY
           "ENTER INVOICE # AND <PgDn> TO READ NUMBERS ONWARDS.   "
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
            MOVE 6       TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO INCR-INVOICE
                                 WS-INVOICE.
            IF INCR-INVOICE = " " OR = "0"
                CLOSE INCR-REGISTER
                CALL WS-INQ-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQ-PROGRAM
                PERFORM CLEAR-SCREEN
                OPEN I-O INCR-REGISTER
                PERFORM DISPLAY-FORM
                GO TO GET-005.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0C"
               MOVE "YOU MAY ONLY PRESS <RETURN> OR <PgDn>."
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
          IF WS-PSLIP (SUB-1) = 0 OR = " "
              GO TO FILL-060.
       FILL-010.
           MOVE 2910 TO POS
           DISPLAY "THE HEADING 'A' = AREA CODE.              " AT POS.
           
           MOVE "                    " TO F-NAMEFIELD.
           MOVE "AREA"      TO F-FIELDNAME
           MOVE 4           TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           MOVE 1           TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO WS-AREA (SUB-1).
           PERFORM ERROR-020.
       FILL-012.
           MOVE 2910 TO POS
           DISPLAY "THE HEADING 'P' = PART ORDERS ALLOWED.    " AT POS.
           
           MOVE "                    " TO F-NAMEFIELD.
           MOVE "PART-ORDERS" TO F-FIELDNAME
           MOVE 11            TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           MOVE 1             TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD   TO WS-PART-ORDERS (SUB-1).
           
           IF WS-PART-ORDERS (SUB-1) NOT = "Y" AND NOT = "N"
              MOVE "ENTRY MUST BE EITHER 'Y' OR 'N', PLEASE RE-ENTER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO FILL-012.
           
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
           MOVE 2910 TO POS.
           IF WS-TYPE = 4
               DISPLAY "WRITING CHANGES TO P/SLIP-FILE....." AT POS
           ELSE
               DISPLAY "WRITING CHANGES TO INVOICE-FILE...." AT POS.
           PERFORM REWRITE-REGISTER.
       FILL-999.
            EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-005.
           MOVE WS-INVOICE TO INCR-INVOICE.
           START INCR-REGISTER KEY NOT < INCR-KEY
                INVALID KEY NEXT SENTENCE.
           MOVE 1 TO SUB-1 F-INDEX.
       RDTR-010.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10 OR = 91
               GO TO RDTR-999.
           IF WS-INCR-ST1 NOT = 0
             MOVE "REGISTER BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RDTR-010.
               
           IF WS-NEWINPUT = "1"
            IF WS-TYPE = 4
             IF INCR-PRINTED NOT = "P" AND NOT = "N" AND NOT = "S"
               MOVE "P/SLIP IS COMPLETE, 'ESC' TO RE-ENTER."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "8" TO WS-NEWINPUT
               GO TO RDTR-999.
           IF WS-NEWINPUT = "1"
            IF WS-TYPE = 1
             IF INCR-PULL-DATE > 0
               MOVE
             "THIS INVOICE IS ALREADY DELIVERED, 'ESC' TO RE-ENTER."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "8" TO WS-NEWINPUT
               GO TO RDTR-999.
            
           IF WS-TYPE = 4
            IF INCR-TRANS NOT = 4 
               GO TO RDTR-999.
           IF WS-TYPE = 1
            IF INCR-TRANS NOT = 1
               GO TO RDTR-999.

           IF WS-TYPE = 4
            IF INCR-PRINTED NOT = "P" AND NOT = "S" AND NOT = "N"
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next P/SLIP...." AT POS
               GO TO RDTR-010.
           IF WS-TYPE = 1
            IF INCR-PULL-DATE > 0
               MOVE 2910 TO POS
               DISPLAY "Be Patient, reading next INVOICE..." AT POS
               GO TO RDTR-010.
               
           MOVE INCR-INVOICE     TO WS-PSLIP (SUB-1)
           MOVE INCR-BIN         TO WS-BIN (SUB-1)
           MOVE INCR-PULLBY      TO WS-PULLBY (SUB-1)
           MOVE INCR-AREA        TO WS-AREA (SUB-1)
           MOVE INCR-PART-ORDERS TO WS-PART-ORDERS (SUB-1)
           MOVE INCR-DATE        TO WS-TRAN-DATE (SUB-1)
           MOVE INCR-ACCOUNT     TO WS-ACCOUNT (SUB-1)
           MOVE INCR-NAME        TO WS-NAME (SUB-1)
           MOVE INCR-INVCRED-AMT TO WS-PS-AMT (SUB-1).
           
           
           IF WS-NEWINPUT = "1"
              ADD 1 TO SUB-1
              GO TO RDTR-999.
           IF SUB-1 < 15
            IF WS-TYPE = 4
              ADD 1 TO SUB-1
              MOVE 2610 TO POS
              DISPLAY "Number Of P/Slips Found :" AT POS
              ADD 26 TO POS
              MOVE SUB-1 TO WS-PSDIS
              DISPLAY WS-PSDIS AT POS
              GO TO RDTR-010.
           IF SUB-1 < 15
            IF WS-TYPE = 1
              ADD 1 TO SUB-1
              MOVE 2610 TO POS
              DISPLAY "Number Of Invoices Found:" AT POS
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
           IF WS-PULLBY (SUB-1) = " "
                GO TO RWR-070.
           MOVE WS-PSLIP (SUB-1) TO INCR-INVOICE.
       RWR-010.
           READ INCR-REGISTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               GO TO RWR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
             GO TO RWR-010.
           ACCEPT WS-TIME FROM TIME.
           MOVE WS-AREA (SUB-1)        TO INCR-AREA
           MOVE WS-PART-ORDERS (SUB-1) TO INCR-PART-ORDERS.
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
       SCROLLING SECTION.
       SCROLL-000.
            MOVE 1 TO SUB-1 F-INDEX.
       SCROLL-005.
            IF WS-PSLIP (SUB-1) = 0 OR = " "
                GO TO SCROLL-999.
                
            MOVE "PSLIP"          TO F-FIELDNAME.
            MOVE 5                TO F-CBFIELDNAME.
            MOVE WS-PSLIP (SUB-1) TO F-EDNAMEFIELDNUM.
            MOVE 6                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "PBINITIAL"       TO F-FIELDNAME.
            MOVE 9                 TO F-CBFIELDNAME.
            MOVE WS-PULLBY (SUB-1) TO F-NAMEFIELD.
            MOVE 2                 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BIN"          TO F-FIELDNAME.
            MOVE 3              TO F-CBFIELDNAME.
            MOVE WS-BIN (SUB-1) TO F-NAMEFIELD.
            MOVE 6              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AREA"          TO F-FIELDNAME.
            MOVE 4               TO F-CBFIELDNAME.
            MOVE WS-AREA (SUB-1) TO F-NAMEFIELD.
            MOVE 1               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PART-ORDERS"          TO F-FIELDNAME
            MOVE 11                     TO F-CBFIELDNAME
            MOVE WS-PART-ORDERS (SUB-1) TO F-NAMEFIELD
            MOVE 1                      TO F-CBFIELDLENGTH
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

            MOVE "BIN" TO F-FIELDNAME
            MOVE 3     TO F-CBFIELDNAME
            MOVE " "   TO F-NAMEFIELD
            MOVE 6     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AREA"  TO F-FIELDNAME
            MOVE 4       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 1       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PART-ORDERS" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE " "           TO F-NAMEFIELD
            MOVE 1             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 10     TO F-CBFIELDLENGTH
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
       CLEAR-MEMORY SECTION.
       CMS-005.
            MOVE 1 TO SUB-1.
       CMS-010.
            MOVE SPACES TO WS-PB-FIELDS (SUB-1).
            IF SUB-1 < 15
              ADD 1 TO SUB-1
              GO TO CMS-010.
            MOVE 1 TO SUB-1.
       CMS-999.
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
           MOVE "SlAreaMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-900.
            CLOSE INCR-REGISTER.
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
