        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrReFrAl.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectCrMaster".
           Copy "SelectCrTrans".
           Copy "SelectCrRemitTrans".
           Copy "SelectGlParameter".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCrTrans.
           COPY ChlfdCrRemiTrans.
           COPY ChlfdGlParam.
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "CrMastIq".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-NO-OF-TRANS       PIC 9(5) VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-CREDITORNUMBER    PIC X(15) VALUE " ".
       77  WS-PRINTER           PIC X(20) VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  LINES-DISPLAYED      PIC 9(5) VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-ONLY-UNALLOC      PIC X VALUE " ".
       77  WS-END               PIC X VALUE " ".
       77  WS-ALLOC             PIC X VALUE " ".
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-PAY-COUNT         PIC 9(3) VALUE 0.
       77  WS-PAY-FOR-AMT       PIC 9(6)9V99.
       77  WS-PAY-LOC-AMT       PIC 9(6)9V99.
       77  WS-REMIT-F-L         PIC X VALUE " ".      
       01  WS-REMIT-PERIOD.
           03  WS-REMI-YY          PIC 99.
           03  WS-REMI-MM          PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1     PIC 99.
       01  WS-CRTRANS-STATUS.
           03  WS-CRTRANS-ST1      PIC 99.
       01  WS-REMITTRANS-STATUS.
           03  WS-REMITTRANS-ST1   PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
       01  WS-PERIOD.
           03  WS-1ST-CHAR         PIC X.
           03  WS-PER              PIC 99.
       01  WS-TRANSACTIONS.
           03  FILLER              PIC X(3) VALUE "INV".
           03  FILLER              PIC X(3) VALUE "PAY".
           03  FILLER              PIC X(3) VALUE "   ".
           03  FILLER              PIC X(3) VALUE "JDR".
           03  FILLER              PIC X(3) VALUE "JCR".
           03  FILLER              PIC X(3) VALUE "CRD".
           03  FILLER              PIC X(3) VALUE "INT".
           03  FILLER              PIC X(3) VALUE "DIS".
           03  FILLER              PIC X(3) VALUE "FRX".
       01  WS-TRANS-TYPERED REDEFINES WS-TRANSACTIONS.
           03  WS-TRANS-DESC    PIC X(3) OCCURS 9.
       01  WS-ALL-LINES.
           03  WS-LINES OCCURS 500.
               05  WS-TRANS-NUM     PIC 9(6) VALUE 0.
               05  WS-TYPE-NUM      PIC 99 VALUE 0.
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 15.
               05  WS-TRANS          PIC 9(6).
               05  WS-CR-PERIOD      PIC 99.
               05  WS-CR-ACC-NUM     PIC 9(7).
               05  WS-CR-REF         PIC X(10).
               05  WS-CR-INV-NO      PIC X(10).
               05  WS-CR-INV-DATE    PIC 9(8).
               05  WS-CR-INV-FOR-AMT PIC S9(8)V99.
               05  WS-CR-INV-LOC-AMT PIC S9(8)V99.
               05  WS-CR-DIS-AMT     PIC S9(8)V99.
               05  WS-PAID           PIC X.
               05  WS-NEWLINE        PIC X.
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
           MOVE 0310 TO POS
           DISPLAY 
           "** FOREIGN CREDITOR TRANS ALLOCATION FOR REMIT **" AT POS
           MOVE 0410 TO POS
           DISPLAY 
           "*************************************************" AT POS.
       CONTROL-005.
           MOVE 0610 TO POS
           DISPLAY 
           "ENTER YY=YEAR, MM=MONTH FOR THE REMITTANCE : [    ]" AT POS
           ADD 46 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 4         TO CDA-DATALEN.
           MOVE 3         TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-REMIT-PERIOD.

      *     ACCEPT WS-REMIT-PERIOD AT POS.
           IF W-ESCAPE-KEY = 3
               PERFORM END-OFF.
       CONTROL-006.
           MOVE "F" TO WS-REMIT-F-L.
           MOVE 0810 TO POS
           DISPLAY 
           "                  ENTER L=LOCAL, F=FOREIGN : [ ]" AT POS
           ADD 46 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-REMIT-F-L.

      *     ACCEPT WS-REMIT-F-L AT POS.
           IF WS-REMIT-F-L NOT = "F" AND NOT = "L"
              MOVE "FOREIGN / LOCAL FIELD MUST BE F OR L, RE-ENTER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO CONTROL-006.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
       CONTROL-010.
           MOVE 2910 TO POS
           DISPLAY "Program now loading, please be patient..." AT POS.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-020.
           PERFORM DISPLAY-FORM.
           PERFORM DISPLAY-FORM-CR-TOP-INFO.
           PERFORM GET-DATA.
           GO TO CONTROL-020.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE SPACES  TO F-NAMEFIELD.
            MOVE "N"     TO WS-ANSWER.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 GO TO GET-999.
            IF F-EXIT-CH = X"0C"
                 PERFORM READ-CREDITOR-NEXT
                 GO TO GET-010.
            MOVE 15           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CR-ACCOUNT-NUMBER.
            IF F-NAMEFIELD = "   "
                CLOSE CREDITOR-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                OPEN I-O CREDITOR-MASTER
                PERFORM DISPLAY-FORM
                PERFORM DISPLAY-FORM-CR-TOP-INFO
                GO TO GET-000.
            PERFORM READ-CREDITOR.
            GO TO GET-020.
       GET-010.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            MOVE CR-ACCOUNT-NUMBER TO F-NAMEFIELD.
            MOVE 15      TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-020.
            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE CR-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            IF CR-NAME = "UNKNOWN"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.

            MOVE "BALANCE"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE CR-BALANCE TO F-EDNAMEFIELDSALE
            MOVE 11         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-SALE.
       GET-500.
            MOVE " " TO WS-ABOVE-BODY.
            PERFORM FILL-DATA.
            IF WS-ABOVE-BODY = "1"
               GO TO GET-000.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-000.
           MOVE 0 TO SUB-25 SUB-15
                     WS-PAY-COUNT
                     WS-PAY-FOR-AMT
                     WS-PAY-LOC-AMT.
           PERFORM READ-ALL-TRANSACTIONS.
       FILL-001.
           MOVE 1            TO SUB-1 SUB-2 F-INDEX.
           PERFORM READ-NEXT-TRANSACTIONS.
           
           MOVE "TOTTRANS" TO F-FIELDNAME
           MOVE 8          TO F-CBFIELDNAME
           MOVE SUB-15     TO F-EDNAMEFIELDQTY
           MOVE 5          TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-QTY.

           PERFORM SCROLLING.
           MOVE 1 TO SUB-1 F-INDEX.
           MOVE 2910 TO POS
           DISPLAY "                                         " AT POS.
           IF SUB-2 NOT > 1
              MOVE "NO FURTHAR TRANSACTIONS, 'ESC' TO FINISH."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO FILL-999.
       FILL-005.
           IF SUB-1 < 1
              MOVE 1 TO SUB-1 F-INDEX.
           MOVE "LINES" TO F-FIELDNAME
           MOVE 5       TO F-CBFIELDNAME
           MOVE SUB-1   TO F-EDNAMEFIELDQTY
           MOVE 5       TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-QTY.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
                
           PERFORM SCROLL-950.
           MOVE WS-PAID (SUB-1) TO WS-ALLOC.
       FILL-013.
           MOVE "                    " TO F-NAMEFIELD.
           MOVE "ALLOCATED" TO F-FIELDNAME
           MOVE 9           TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           MOVE 1           TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA
           MOVE F-NAMEFIELD TO WS-PAID (SUB-1).
           IF F-EXIT-CH = X"01" AND F-INDEX = 1
            IF SUB-2 = 1
              MOVE "1" TO WS-ABOVE-BODY
              GO TO FILL-999
            ELSE
              MOVE "YOU MUST 'TAB' BEFORE FINISHING THIS SESSION."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO FILL-005.
           IF F-EXIT-CH = X"01" AND F-INDEX > 1
              SUBTRACT 1 FROM F-INDEX SUB-1 SUB-2
              GO TO FILL-005.
           IF F-EXIT-CH = X"07"
              PERFORM CLEAR-FIELDS
              PERFORM CLEAR-TRANSACTIONS
              CLOSE CRTR-FILE
              PERFORM ERROR1-020
              PERFORM ERROR-020
      *        PERFORM FILL-060
              GO TO FILL-999.
           IF WS-PAID (SUB-1) NOT = "N" AND NOT = "Y"
               MOVE "THE FIELD MUST BE EITHER 'N' OR 'Y', RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-013.
      *****************
      *TAB KEY = X"09"*
      *****************
           IF F-EXIT-CH = X"09"
              PERFORM FILL-050
              GO TO FILL-060.
           IF F-EXIT-CH = X"12" AND F-INDEX < 15
              PERFORM FILL-050
              ADD 1 TO F-INDEX SUB-1 SUB-2
              GO TO FILL-005.
           IF F-EXIT-CH = X"0B" AND F-INDEX < 15
              ADD 1 TO F-INDEX SUB-1 SUB-2
              GO TO FILL-005.
           IF F-EXIT-CH = X"0A" AND F-INDEX < 15
              ADD 1 TO F-INDEX SUB-1 SUB-2
              GO TO FILL-005.
           IF F-EXIT-CH = X"0B" AND F-INDEX = 15
              PERFORM FILL-050
              GO TO FILL-060.
           IF F-EXIT-CH = X"0A" AND F-INDEX = 15
              PERFORM FILL-050
              GO TO FILL-060.
           IF F-EXIT-CH = X"12" AND F-INDEX = 15
              PERFORM FILL-050
              GO TO FILL-060.

      *X"12" = AUTO EXIT FROM FIELD.
           IF F-EXIT-CH NOT = X"01" AND NOT = X"0A" AND NOT = X"0B"
                    AND NOT = X"07" AND NOT = X"09" AND NOT = X"12"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO FILL-013.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
       FILL-015.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-050.
            IF WS-ALLOC NOT = WS-PAID (SUB-1)
             IF WS-ALLOC = "N"
                ADD 1                    TO WS-PAY-COUNT
                ADD WS-CR-INV-FOR-AMT (SUB-1) TO WS-PAY-FOR-AMT
                ADD WS-CR-INV-LOC-AMT (SUB-1) TO WS-PAY-LOC-AMT.
            IF WS-ALLOC NOT = WS-PAID (SUB-1)
             IF WS-ALLOC = "Y"
                SUBTRACT 1                    FROM WS-PAY-COUNT
                SUBTRACT WS-CR-INV-FOR-AMT (SUB-1) FROM WS-PAY-FOR-AMT
                SUBTRACT WS-CR-INV-LOC-AMT (SUB-1) FROM WS-PAY-LOC-AMT.

            PERFORM SCROLL-950.
       FILL-055.
            IF SUB-1 > 14
               GO TO FILL-060.
            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 > SUB-2
               MOVE SUB-1 TO SUB-2.
            GO TO FILL-005.
       FILL-060.
            PERFORM ERROR-020.
            MOVE 3010 TO POS
            DISPLAY "WRITING REMITTANCE TRANSACTIONS......" AT POS
            PERFORM WRITE-CRREMIT-TRANS
            PERFORM CLEAR-FIELDS
            PERFORM CLEAR-TRANSACTIONS.

            PERFORM ERROR1-020.
            PERFORM ERROR-020.
       FILL-070.
            IF WS-END = "Y"
               MOVE "NO FURTHAR TRANSACTIONS, 'ESC' TO FINISH."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               CLOSE CRTR-FILE
               GO TO FILL-999.
           GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       READ-ALL-TRANSACTIONS SECTION.
       RALT-000.
           OPEN I-O CRTR-FILE.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE "CRTRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              CLOSE CRTR-FILE
              GO TO RALT-000.
           MOVE 2910 TO POS.
           DISPLAY "Reading All transactions......" AT POS.
       RALT-005.
           MOVE 1 TO SUB-25.
           MOVE CR-ACCOUNT-NUMBER TO CRTR-ACC-NUMBER.
           START CRTR-FILE KEY NOT < CRTR-ACC-NUMBER
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
               GO TO RALT-900.
       RALT-010.
           READ CRTR-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 10
               SUBTRACT 1 FROM SUB-25
               GO TO RALT-900.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE "CRTRANS FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RALT-010.
               
           IF CRTR-ACC-NUMBER NOT = CR-ACCOUNT-NUMBER
               GO TO RALT-900.
           IF CRTR-TYPE NOT = 1 AND NOT = 6
               GO TO RALT-010.
       RALT-015.
           MOVE CRTR-TYPE  TO WS-TYPE-NUM (SUB-25)
           MOVE CRTR-TRANS TO WS-TRANS-NUM (SUB-25).
       RALT-020.
            IF SUB-25 < 501
               ADD 1 TO SUB-25
               GO TO RALT-010.
       RALT-900.
           PERFORM ERROR1-020
           PERFORM ERROR-020
           CLOSE CRTR-FILE.
           PERFORM OPEN-004.
       RALT-999.
           EXIT.
      *
       READ-NEXT-TRANSACTIONS SECTION.
       RONX-001.
           ADD 1 TO SUB-15.
           MOVE 2910 TO POS
           DISPLAY "READING TRANSACTIONS FOR ACCOUNT...." AT POS.
           MOVE WS-TYPE-NUM (SUB-15)  TO CRTR-TYPE
           MOVE WS-TRANS-NUM (SUB-15) TO CRTR-TRANS.
           PERFORM START-TRANS.
           IF WS-REMITTRANS-ST1 NOT = 0
              GO TO RONX-999.
       RONX-005.
           READ CRTR-FILE
              INVALID KEY NEXT SENTENCE.
           IF WS-REMITTRANS-ST1 = 23 OR 35 OR 49
              MOVE "THE CR-TRANS REC DOES NOT EXIST, 'ESC' FOR NUMBER."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
              MOVE CRTR-TRANS TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RONX-030.
           IF WS-REMITTRANS-ST1 NOT = 0
              MOVE 0 TO WS-REMITTRANS-ST1
              MOVE "CRTRANS FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-005.
       RONX-010.
            MOVE CRTR-TRANS      TO WS-TRANS (SUB-1)
            MOVE CRTR-NO         TO WS-CR-PERIOD (SUB-1)
            MOVE CRTR-ACC-NUMBER TO WS-CR-ACC-NUM (SUB-1)
            MOVE CRTR-REFERENCE  TO WS-CR-REF (SUB-1)
            MOVE CRTR-INV-NO     TO WS-CR-INV-NO (SUB-1)
            MOVE CRTR-DATE       TO WS-CR-INV-DATE (SUB-1)
            MOVE CRTR-FOR-AMT    TO WS-CR-INV-FOR-AMT (SUB-1)
            MOVE CRTR-LOC-AMT    TO WS-CR-INV-LOC-AMT (SUB-1)
            MOVE CRTR-SETT-DISC  TO WS-CR-DIS-AMT (SUB-1)
            MOVE "N"             TO WS-NEWLINE (SUB-1)
                                    WS-PAID (SUB-1).
       RONX-020.
            IF SUB-1 = 15
               GO TO RONX-900.
            ADD 1 TO SUB-1.
       RONX-030.
            IF SUB-15 < SUB-25
                ADD 1 TO SUB-15
            ELSE
                MOVE "Y" TO WS-END
                GO TO RONX-900.
            MOVE WS-TRANS-NUM (SUB-15) TO CRTR-TRANS
            GO TO RONX-005.
       RONX-900.
            MOVE SUB-1 TO SUB-2.
       RONX-999.
           EXIT.
      *
       START-TRANS SECTION.
       ST-OO-000.
           START CRTR-FILE KEY NOT < CRTR-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-REMITTRANS-ST1 NOT = 0
              MOVE "THERE ARE NO TRANSACTIONS FOR THIS PERIOD."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
       ST-OO-999.
             EXIT.
      *
       WRITE-CRREMIT-TRANS SECTION.
       WCBT-001.
           MOVE 1 TO SUB-1.
           IF WS-CR-ACC-NUM (SUB-1) = " "
              GO TO WCBT-900.
       WCBT-005.
           IF WS-PAID (SUB-1) NOT = "Y"
              GO TO WCBT-020.
              
           MOVE WS-REMI-YY                TO CRREMTR-YY
           MOVE WS-REMI-MM                TO CRREMTR-MM
           MOVE WS-CR-ACC-NUM (SUB-1)     TO CRREMTR-ACC-NUMBER
           MOVE WS-CR-INV-NO (SUB-1)      TO CRREMTR-INVNO
           MOVE WS-CR-INV-FOR-AMT (SUB-1) TO CRREMTR-INV-AMT
           MOVE WS-CR-DIS-AMT (SUB-1)     TO CRREMTR-DISC-AMT
           MOVE "N"                       TO CRREMTR-COMPLETE.
       WCBT-018.
           WRITE CRREMTR-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-REMITTRANS-ST1 NOT = 0
                MOVE "CRREMIT-TRANS BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-REMITTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO WCBT-018.
       WCBT-020.
            ADD 1 TO SUB-1.
            IF SUB-1 < 16
             IF WS-TRANS (SUB-1) = 0
                GO TO WCBT-900
             ELSE
                GO TO WCBT-005.
       WCBT-900.
            MOVE 1 TO SUB-1.
       WCBT-999.
            EXIT.
     *
       READ-CREDITOR SECTION.
       RD-010.
           START CREDITOR-MASTER KEY NOT < CR-KEY.
       RD-015.
           READ CREDITOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
               MOVE "UNKNOWN" TO CR-NAME
               GO TO RD-999.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE"CREDITOR BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RD-015.
       RD-999.
           EXIT.
      *
       START-CREDITOR SECTION.
       ST-ST-000.
              MOVE WS-CREDITORNUMBER TO CR-ACCOUNT-NUMBER.
              START CREDITOR-MASTER KEY NOT < CR-ACCOUNT-NUMBER.
       ST-ST-999.
             EXIT.
      *
       READ-CREDITOR-NEXT SECTION.
       R-ST-NX-000.
             MOVE 0 TO WS-CREDITOR-ST1.
             MOVE " " TO WS-MESSAGE
             MOVE 3010 TO POS
             DISPLAY WS-MESSAGE AT POS.
       R-ST-NX-005. 
             READ CREDITOR-MASTER NEXT
                 AT END NEXT SENTENCE.
             IF WS-CREDITOR-ST1 = 0
                 GO TO R-ST-NX-999
             ELSE
              MOVE "CREDITOR BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CREDITOR-ST1
              PERFORM START-CREDITOR
              GO TO R-ST-NX-005.
       R-ST-NX-999.
             EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            MOVE 1 TO SUB-1 F-INDEX.
       SCROLL-010.
            IF WS-TRANS (SUB-1) = 0
                 GO TO SCROLL-999.

            MOVE "PERIOD"             TO F-FIELDNAME
            MOVE 6                    TO F-CBFIELDNAME
            MOVE WS-CR-PERIOD (SUB-1) TO F-NAMEFIELD
            MOVE 2                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "JRN"             TO F-FIELDNAME
            MOVE 3                 TO F-CBFIELDNAME
            MOVE WS-CR-REF (SUB-1) TO F-NAMEFIELD
            MOVE 10                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPE"                    TO F-FIELDNAME
            MOVE 4                         TO F-CBFIELDNAME
            MOVE WS-TRANS-DESC (CRTR-TYPE) TO F-NAMEFIELD
            MOVE 3                         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICE"            TO F-FIELDNAME
            MOVE 7                    TO F-CBFIELDNAME
            MOVE WS-CR-INV-NO (SUB-1) TO F-NAMEFIELD
            MOVE 10                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVDATE"              TO F-FIELDNAME
            MOVE 7                      TO F-CBFIELDNAME
            MOVE WS-CR-INV-DATE (SUB-1) TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE           TO F-NAMEFIELD
            MOVE 10                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ALLOCATED"     TO F-FIELDNAME
            MOVE 9               TO F-CBFIELDNAME
            MOVE WS-PAID (SUB-1) TO F-NAMEFIELD
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVFORAMT"               TO F-FIELDNAME
            MOVE 9                         TO F-CBFIELDNAME
            MOVE WS-CR-INV-FOR-AMT (SUB-1) TO F-EDNAMEFIELDSALE
            MOVE 11                        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-SALE.

            MOVE "INVLOCAMT"               TO F-FIELDNAME
            MOVE 9                         TO F-CBFIELDNAME
            MOVE WS-CR-INV-LOC-AMT (SUB-1) TO F-EDNAMEFIELDSALE
            MOVE 11                        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-SALE.

            MOVE "SETTDISC"            TO F-FIELDNAME
            MOVE 8                     TO F-CBFIELDNAME
            MOVE WS-CR-DIS-AMT (SUB-1) TO F-EDNAMEFIELDADDON
            MOVE 7                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ADDON.
       SCROLL-020.
            IF SUB-1 = 15
              GO TO SCROLL-999.
            ADD 1 TO SUB-1 F-INDEX.
            GO TO SCROLL-010.
       SCROLL-950.
            MOVE 2929 TO POS
            DISPLAY "PAYMENTS     @                " AT POS
            MOVE 2938 TO POS
            MOVE WS-PAY-COUNT TO F-EDNAMEFIELDGROUP
            DISPLAY F-EDNAMEFIELDGROUP AT POS
            MOVE 2944 TO POS
            DISPLAY CR-CURRENCY AT POS
            MOVE 2948 TO POS
            MOVE WS-PAY-FOR-AMT TO F-EDNAMEFIELD9MIL
            DISPLAY F-EDNAMEFIELD9MIL AT POS
            MOVE 2960 TO POS
            MOVE WS-PAY-LOC-AMT TO F-EDNAMEFIELD9MIL
            DISPLAY F-EDNAMEFIELD9MIL AT POS.
       SCROLL-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-001.
           MOVE 1 TO SUB-1.
       CF-005.
           MOVE 0   TO WS-TRANS (SUB-1)
                       WS-CR-PERIOD (SUB-1)
                       WS-CR-ACC-NUM (SUB-1)
                       WS-CR-INV-DATE (SUB-1)
                       WS-CR-INV-FOR-AMT (SUB-1)
                       WS-CR-INV-LOC-AMT (SUB-1)
                       WS-CR-DIS-AMT (SUB-1).
           MOVE " " TO WS-CR-REF (SUB-1)
                       WS-CR-INV-NO (SUB-1)
                       WS-PAID (SUB-1)
                       WS-NEWLINE (SUB-1).
           ADD 1 TO SUB-1.
           IF SUB-1 < 16
               GO TO CF-005.
           MOVE 1 TO SUB-1.
           PERFORM CLEAR-ALL-TRANS.
       CF-999.
           EXIT.
      *
       CLEAR-ALL-TRANS SECTION.
       CAT-001.
           MOVE 1 TO SUB-1.
       CAT-005.
           IF WS-TRANS-NUM (SUB-1) = 0
               GO TO CAT-900.
           MOVE 0 TO WS-TRANS-NUM (SUB-1)
                     WS-TYPE-NUM (SUB-1).
           ADD 1 TO SUB-1.
           IF SUB-1 < 500
               GO TO CAT-005.
       CAT-900.
           MOVE 1 TO SUB-1.
       CAT-999.
           EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO SUB-1 F-INDEX.
       CLTR-010.
            IF SUB-1 > 15
                GO TO CLTR-999.
            MOVE "PERIOD" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 2        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "JRN" TO F-FIELDNAME
            MOVE 3     TO F-CBFIELDNAME
            MOVE " "   TO F-NAMEFIELD
            MOVE 10    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 3      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICE" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 10        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVDATE" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 10        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ALLOCATED"     TO F-FIELDNAME
            MOVE 9               TO F-CBFIELDNAME
            MOVE " "             TO F-NAMEFIELD
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVFORAMT" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 11          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVLOCAMT" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 11          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SETTDISC" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 11         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            ADD 1 TO SUB-1 F-INDEX.
            GO TO CLTR-010.
       CLTR-999.
            EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO GLPARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE "CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO OPEN-000.
       OPEN-003.
            OPEN I-O CRREMIT-TRANS-FILE
            IF WS-REMITTRANS-ST1 NOT = 0
               MOVE 0 TO WS-REMITTRANS-ST1
               MOVE "CR-REMITRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-003.
               
           GO TO OPEN-005.
       OPEN-004.
           OPEN I-O CRTR-FILE.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE "CRTR-FILEBUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-004.
       OPEN-005.
            OPEN I-O GLPARAMETER-FILE.
            IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-005.
           PERFORM READ-PARAMETER.
           MOVE GLPA-NAME TO CO-NAME.
           PERFORM ENTER-PERIOD-DATES.
           PERFORM OPEN-006.
           CLOSE GLPARAMETER-FILE.
       OPEN-006.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT.
      *     MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrReFrAl"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-900.
           CLOSE CRREMIT-TRANS-FILE
                 CREDITOR-MASTER.
           EXIT PROGRAM.
      *      STOP RUN.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldChange".
       Copy "WriteFieldCredit".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldQty".
       Copy "WriteFieldSale".
       Copy "WriteFieldAddOn".
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "DisplayFormCRTopInfo".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
      * Copy "PrintReportInfo".
      *
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
