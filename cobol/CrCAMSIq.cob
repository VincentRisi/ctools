        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrCAMSIq.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectCbMaster".
           Copy "SelectCrCAMSTrans".
           Copy "SelectGlParameter".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCrCAMSTrans.
           COPY ChlfdCbMast.
           COPY ChlfdGlParam.

       WORKING-STORAGE SECTION.
       77  WS-CBMAST          PIC X(12) VALUE " ".
       77  WS-REFERENCE       PIC X(10) VALUE " ".      
       77  WS-ONLY-UNALLOC    PIC X VALUE " ".
       77  WS-END             PIC X VALUE " ".
       77  WS-ALLOC           PIC X VALUE " ".
       77  WS-ABOVE-BODY      PIC X VALUE " ".
       77  WS-PAY-COUNT       PIC 9(3) VALUE 0.
       77  WS-PAY-AMT         PIC 9(6)9V99.
       01  WS-PERIOD.
           03  WS-FUTURE      PIC X.
           03  WS-NO          PIC 99.
       01  WS-CRCAMSTRANS-STATUS.
           03  WS-CRCAMSTRANS-ST1  PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
       01  WS-CB-STATUS.
           03  WS-CB-ST1           PIC 99.
       01  WS-ALL-LINES.
           03  WS-LINES OCCURS 500.
               05  WS-TRANS-NUM     PIC 9(6).
               05  WS-TYPE-NUM      PIC 99.
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 15.
               05  WS-TRANS           PIC 9(6).
               05  WS-ACC-NUM         PIC 9(7).
               05  WS-ACC-NAME        PIC X(20).
               05  WS-CRED-NUM        PIC X(10).
               05  WS-CHEQUE-NUM      PIC X(10).
               05  WS-TRANS-DATE      PIC 9(8).
               05  WS-TRANS-CRED-TYPE PIC 9.
               05  WS-TRANS-AMT       PIC S9(7)V99.
               05  WS-BANK-NUM        PIC X(11).
               05  WS-BRANCH-NUM      PIC X(6).
               05  WS-PAID            PIC X.
               05  WS-NEWLINE         PIC X.
       Copy "WsDateInfo".
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
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM DISPLAY-FORM-CR-TOP-INFO.
           PERFORM CLEAR-TRANS-IN-MEM.
           PERFORM GET-DATA.
           PERFORM CLEAR-SCREEN.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO CR-CAMS-TRANS-REC
                        WS-END.
       GET-002.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
               EXIT PROGRAM.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF ALPHA-RATE > SPACES
               PERFORM NUMBER-CHECK
               MOVE WS-GLNO-CHECK TO Ws-CbMast
            IF SIGN-FOUND = 9
               MOVE 0 TO SIGN-FOUND
               GO TO GET-002.
            MOVE Ws-CbMast TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-002.

            PERFORM READ-CB.
            IF CB-DESCRIPTION = " "
               MOVE "ENTER AN EXISTING ACCOUNT NUMBER" TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-002.

            MOVE "NAME"         TO F-FIELDNAME
            MOVE 4              TO F-CBFIELDNAME
            MOVE CB-DESCRIPTION TO F-NAMEFIELD
            MOVE 40             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       GET-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ONLY-UNALLOC" TO F-FIELDNAME
            MOVE 12             TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            MOVE 2              TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            IF F-EXIT-CH = X"01"
               GO TO GET-002.
            MOVE F-NAMEFIELD  TO WS-ONLY-UNALLOC.
            IF WS-ONLY-UNALLOC NOT = "N" AND NOT = "Y"
               MOVE "ENTER ONLY 'Y' OR 'N' - PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       GET-500.
            MOVE " " TO WS-ABOVE-BODY.
            PERFORM FILL-DATA.
            IF WS-ABOVE-BODY = "1"
               GO TO GET-002.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-000.
           MOVE 0 TO SUB-25 SUB-15
                     WS-PAY-COUNT
                     WS-PAY-AMT.
           PERFORM READ-ALL-TRANSACTIONS.
       FILL-001.
           MOVE 1            TO SUB-1 SUB-2 F-INDEX.
           PERFORM READ-NEXT-TRANSACTIONS.
           
           MOVE "TOTLINENO"  TO F-FIELDNAME
           MOVE 9            TO F-CBFIELDNAME
           MOVE SUB-15       TO F-EDNAMEFIELDNUM
           MOVE 6            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-NUMERIC.
           
           PERFORM SCROLLING.
           MOVE 1 TO SUB-1 F-INDEX.
           PERFORM ERROR1-020.
           PERFORM SCROLL-950.
           IF SUB-2 NOT > 1
              MOVE "NO FURTHAR TRANSACTIONS, 'ESC' TO FINISH."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO FILL-999.
       FILL-005.
           IF SUB-1 < 1
              MOVE 1 TO SUB-1 F-INDEX.
           MOVE "LINENO"     TO F-FIELDNAME
           MOVE 6            TO F-CBFIELDNAME
           MOVE SUB-1        TO F-EDNAMEFIELDANAL
           MOVE 2            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ANALYSIS.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
                
           PERFORM SCROLL-950.
           MOVE WS-PAID (SUB-1) TO WS-ALLOC.
           PERFORM SCROLL-900.
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
      *****************
      *TAB KEY = X"09"*
      *****************
           IF F-EXIT-CH = X"09"
              PERFORM FILL-050
              GO TO FILL-060.
           IF WS-PAID (SUB-1) NOT = "N" AND NOT = "Y"
                               AND NOT = "P"
               MOVE "THE FIELD MUST BE EITHER 'P' 'N' OR 'Y', RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-013.
           IF F-EXIT-CH = X"12" AND F-INDEX < 15
            IF WS-CRED-NUM (SUB-1) NOT = " "
              PERFORM FILL-050
              ADD 1 TO F-INDEX SUB-1 SUB-2
              GO TO FILL-005
            ELSE
              GO TO FILL-005.
           IF F-EXIT-CH = X"0B" AND F-INDEX < 15
            IF WS-CRED-NUM (SUB-1) NOT = " "
              ADD 1 TO F-INDEX SUB-1 SUB-2
              GO TO FILL-005
            ELSE
              GO TO FILL-005.
           IF F-EXIT-CH = X"0A" AND F-INDEX < 15
            IF WS-CRED-NUM (SUB-1) NOT = " "
              ADD 1 TO F-INDEX SUB-1 SUB-2
              GO TO FILL-005
            ELSE
              GO TO FILL-005.
           IF F-EXIT-CH = X"0B" AND F-INDEX = 15
            IF WS-CRED-NUM (SUB-1) NOT = " "
              PERFORM FILL-050
              GO TO FILL-060
            ELSE
              GO TO FILL-005.
           IF F-EXIT-CH = X"0A" AND F-INDEX = 15
            IF WS-CRED-NUM (SUB-1) NOT = " "
              PERFORM FILL-050
              GO TO FILL-060
            ELSE
              GO TO FILL-005.
           IF F-EXIT-CH = X"12" AND F-INDEX = 15
            IF WS-CRED-NUM (SUB-1) NOT = " "
              PERFORM FILL-050
              GO TO FILL-060
            ELSE
              GO TO FILL-005.
           IF F-EXIT-CH = X"07"
              PERFORM CLEAR-FIELDS
              PERFORM CLEAR-BODY
              PERFORM ERROR1-020
              PERFORM ERROR-020
      *        PERFORM FILL-060
              GO TO FILL-999.

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
                ADD WS-TRANS-AMT (SUB-1) TO WS-PAY-AMT.
            IF WS-ALLOC NOT = WS-PAID (SUB-1)
             IF WS-ALLOC = "P"
                SUBTRACT 1                    FROM WS-PAY-COUNT
                SUBTRACT WS-TRANS-AMT (SUB-1) FROM WS-PAY-AMT.

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
            DISPLAY "WRITING CHANGES TO CR-CAMS-TRANS-FILE....." AT POS
            PERFORM WRITE-CRCAMS-TRANS
            PERFORM CLEAR-FIELDS
            PERFORM CLEAR-BODY.

      *      PERFORM ERROR1-020.
            PERFORM ERROR-020.
       FILL-070.
            IF WS-END = "Y"
               MOVE "NO FURTHAR TRANSACTIONS, 'ESC' TO FINISH."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-999.
           GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       WRITE-CRCAMS-TRANS SECTION.
       WCBT-001.
           MOVE 1 TO SUB-1.
           IF WS-ACC-NUM (SUB-1) = " "
              GO TO WCBT-900.
       WCBT-002.
           MOVE WS-TRANS (SUB-1) TO CR-CAMS-TRANS-NUM
           START CR-CAMS-TRANS-FILE KEY NOT < CR-CAMS-TRANS-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-CRCAMSTRANS-ST1 NOT = 0
                MOVE "CRCAMS BUSY ON START, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO WCBT-002.
           GO TO WCBT-010.
       WCBT-005.
           MOVE WS-TRANS (SUB-1) TO CR-CAMS-TRANS-NUM.
       WCBT-010.
           READ CR-CAMS-TRANS-FILE WITH LOCK
                  INVALID KEY NEXT SENTENCE.
            IF WS-CRCAMSTRANS-ST1 NOT = 0
               MOVE "CRCAMS BUSY ON READ-LOCK, WCBT-010 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO WCBT-010.

           MOVE WS-PAID (SUB-1) TO CR-CAMS-TRANS-PAID.
       WCBT-018.
           REWRITE CR-CAMS-TRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CRCAMSTRANS-ST1 NOT = 0
                MOVE "CBTRANS BUSY ON REWRITE, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
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
       READ-NEXT-TRANSACTIONS SECTION.
       RONX-001.
           ADD 1 TO SUB-15.
           MOVE 2910 TO POS
           DISPLAY "READING TRANSACTIONS FOR ACCOUNT...." AT POS.
           PERFORM START-TRANS.
           MOVE WS-TRANS-NUM (SUB-15) TO CR-CAMS-TRANS-NUM.
           IF WS-CRCAMSTRANS-ST1 NOT = 0
              GO TO RONX-999.
       RONX-005.
           READ CR-CAMS-TRANS-FILE
              INVALID KEY NEXT SENTENCE.
           IF WS-CRCAMSTRANS-ST1 = 23 OR 35 OR 49
              GO TO RONX-030.
           IF WS-CRCAMSTRANS-ST1 NOT = 0
              MOVE 0 TO WS-CRCAMSTRANS-ST1
              MOVE "CRCAMS FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-005.
       RONX-010.
            MOVE CR-CAMS-TRANS-NUM        TO WS-TRANS (SUB-1)
            MOVE CR-CAMS-TRANS-ACC-NUMBER TO WS-ACC-NUM (SUB-1)
            MOVE CR-CAMS-TRANS-ACC-NAME   TO WS-ACC-NAME (SUB-1)
            MOVE CR-CAMS-TRANS-CRED-NUM   TO WS-CRED-NUM (SUB-1)
            MOVE CR-CAMS-TRANS-CHEQUE-NUM TO WS-CHEQUE-NUM (SUB-1)
            MOVE CR-CAMS-TRANS-DATE       TO WS-TRANS-DATE (SUB-1)
            MOVE CR-CAMS-TRANS-CRED-TYPE  TO WS-TRANS-CRED-TYPE (SUB-1)
            MOVE CR-CAMS-TRANS-AMOUNT     TO WS-TRANS-AMT (SUB-1)
            MOVE CR-CAMS-TRANS-BANK-NUM   TO WS-BANK-NUM (SUB-1)
            MOVE CR-CAMS-TRANS-BRANCH-NUM TO WS-BRANCH-NUM (SUB-1)
            MOVE CR-CAMS-TRANS-PAID       TO WS-PAID (SUB-1)
            MOVE "N"                      TO WS-NEWLINE (SUB-1).
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
            MOVE WS-TRANS-NUM (SUB-15) TO CR-CAMS-TRANS-NUM
            GO TO RONX-005.
       RONX-900.
            MOVE SUB-1 TO SUB-2.
       RONX-999.
           EXIT.
      *
       READ-ALL-TRANSACTIONS SECTION.
       RALL-001.
           MOVE 2910 TO POS
           DISPLAY "READING ALL TRANSACTIONS FOR ACCOUNT...." AT POS.
           IF WS-CRCAMSTRANS-ST1 NOT = 0
              GO TO RALL-999.
           MOVE 1 TO SUB-25.
           MOVE 0 TO CR-CAMS-TRANS-NUM
           PERFORM START-TRANS.
       RALL-005.
           READ CR-CAMS-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-CRCAMSTRANS-ST1 = 10
              SUBTRACT 1 FROM SUB-25
              GO TO RALL-900.
           IF WS-CRCAMSTRANS-ST1 NOT = 0
              MOVE 0 TO WS-CRCAMSTRANS-ST1
              MOVE "CR-CAMS FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RALL-005.
            IF WS-ONLY-UNALLOC = "N"
               GO TO RALL-010.
            IF WS-ONLY-UNALLOC = "Y"
             IF CR-CAMS-TRANS-PAID = "Y"
              GO TO RALL-005.
       RALL-010.
            MOVE CR-CAMS-TRANS-NUM TO WS-TRANS-NUM (SUB-25).
       RALL-020.
            IF SUB-25 < 501
               ADD 1 TO SUB-25
               GO TO RALL-005.
       RALL-900.
           CLOSE CR-CAMS-TRANS-FILE.
           PERFORM OPEN-000.
       RALL-999.
           EXIT.
      *
       DELETE-TRANS SECTION.
       DO-010.
            DELETE CR-CAMS-TRANS-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-CRCAMSTRANS-ST1 NOT = 0
                MOVE "CBTRANS BUSY ON DELETE, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE 0 TO WS-CRCAMSTRANS-ST1
                GO TO DO-010.
       DO-999.
           EXIT.
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE 0 TO WS-CRCAMSTRANS-ST1.
           START CR-CAMS-TRANS-FILE KEY NOT < CR-CAMS-TRANS-KEY
                 INVALID KEY NEXT SENTENCE.
       RO-010.
           READ CR-CAMS-TRANS-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-CRCAMSTRANS-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-CRCAMSTRANS-ST1
                GO TO RO-999.
           IF WS-CRCAMSTRANS-ST1 NOT = 0
                MOVE 0 TO WS-CRCAMSTRANS-ST1
                MOVE "CBTRANS BUSY ON READ-LOCK, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RO-010.
       RO-999.
           EXIT.
     *
       START-TRANS SECTION.
       ST-OO-000.
           MOVE 0 TO CR-CAMS-TRANS-KEY.
           START CR-CAMS-TRANS-FILE KEY NOT < CR-CAMS-TRANS-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-CRCAMSTRANS-ST1 NOT = 0
              MOVE "THERE ARE NO TRANSACTIONS TO START FOR THIS PERIOD."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
       ST-OO-999.
             EXIT.
     *
       READ-CB SECTION.
       RD-000.
           MOVE Ws-CbMast TO CB-NUMBER.
           START CB-MASTER KEY NOT < CB-KEY.
       RD-010.
           READ CB-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
                MOVE " " TO CB-DESCRIPTION
                GO TO RD-999.
           IF WS-CB-ST1 NOT = 0
                MOVE 0 TO WS-CB-ST1
                MOVE "CASH BOOK BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-010.
       RD-999.
           EXIT.
      *
       READ-GLPARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       RP-010.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE "GLPARAMETER BUSY ON READ, RP-010, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RP-010.
       RP-999.
           EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            MOVE 1 TO SUB-1 F-INDEX.
       SCROLL-010.
            IF WS-TRANS (SUB-1) = 0
                 GO TO SCROLL-999.

            MOVE "ACC-NUM"            TO F-FIELDNAME
            MOVE 7                    TO F-CBFIELDNAME
            MOVE WS-ACC-NUM (SUB-1)   TO F-NAMEFIELD
            MOVE 7                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CRD-NUM"            TO F-FIELDNAME
            MOVE 7                    TO F-CBFIELDNAME
            MOVE WS-CRED-NUM (SUB-1)  TO F-NAMEFIELD
            MOVE 10                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CHEQUE"              TO F-FIELDNAME
            MOVE 6                     TO F-CBFIELDNAME
            MOVE WS-CHEQUE-NUM (SUB-1) TO F-NAMEFIELD
            MOVE 10                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"                TO F-FIELDNAME
            MOVE 4                     TO F-CBFIELDNAME
            MOVE WS-TRANS-DATE (SUB-1) TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE          TO F-NAMEFIELD
            MOVE 10                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ALLOCATED"          TO F-FIELDNAME
            MOVE 9                    TO F-CBFIELDNAME
            MOVE WS-PAID (SUB-1)      TO F-NAMEFIELD
            MOVE 1                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT"             TO F-FIELDNAME
            MOVE 6                    TO F-CBFIELDNAME
            MOVE WS-TRANS-AMT (SUB-1) TO F-EDNAMEFIELDREC
            MOVE 12                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.

            MOVE "BANK"               TO F-FIELDNAME
            MOVE 4                    TO F-CBFIELDNAME
            MOVE WS-BANK-NUM (SUB-1)  TO F-NAMEFIELD
            MOVE 11                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BRANCH"              TO F-FIELDNAME
            MOVE 6                     TO F-CBFIELDNAME
            MOVE WS-BRANCH-NUM (SUB-1) TO F-NAMEFIELD
            MOVE 6                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-020.
            IF SUB-1 = 15
              GO TO SCROLL-999.
            ADD 1 TO SUB-1 F-INDEX.
            GO TO SCROLL-010.
       SCROLL-900.
            MOVE 2910 TO POS
            DISPLAY WS-ACC-NAME (SUB-1) AT POS.
       SCROLL-950.
            MOVE 2940 TO POS
            DISPLAY "PAYMENTS     @ R              " AT POS
            MOVE 2949 TO POS
            MOVE WS-PAY-COUNT TO F-EDNAMEFIELDGROUP
            DISPLAY F-EDNAMEFIELDGROUP AT POS
            MOVE 2955 TO POS
            MOVE WS-PAY-AMT TO F-EDNAMEFIELD9MIL
            DISPLAY F-EDNAMEFIELD9MIL AT POS.
       SCROLL-999.
             EXIT.
      *
       CLEAR-BODY SECTION.
       CLEAR-000.
            MOVE 0 TO F-INDEX.
       CLEAR-002.
            ADD 1 TO F-INDEX.
            IF F-INDEX > 15
               GO TO CLEAR-BODY-999.

            MOVE "ACC-NUM"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 7           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CRD-NUM" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 10        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CHEQUE" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 10       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 10     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ALLOCATED" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 1           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 12       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BANK"  TO F-FIELDNAME
            MOVE 4       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 11      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BRANCH" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 6        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            GO TO CLEAR-002.
       CLEAR-BODY-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
           MOVE 1 TO SUB-1.
       CF-010.
           ADD 1 TO SUB-1.
           MOVE " " TO WS-CRED-NUM (SUB-1)
                       WS-CHEQUE-NUM (SUB-1)
                       WS-BANK-NUM (SUB-1)
                       WS-BRANCH-NUM (SUB-1)
                       WS-PAID (SUB-1)
                       WS-NEWLINE (SUB-1)
           MOVE 0   TO WS-TRANS (SUB-1)
                       WS-ACC-NUM (SUB-1)
                       WS-TRANS-DATE (SUB-1)
                       WS-TRANS-AMT (SUB-1).
           IF SUB-1 < 15
                GO TO CF-010.
           MOVE 1 TO SUB-1.
       CF-999.
             EXIT.
      *
       CLEAR-TRANS-IN-MEM SECTION.
       CTIM-000.
           MOVE 1 TO SUB-1.
       CTIM-010.
           ADD 1 TO SUB-1.
           MOVE 0   TO WS-TRANS-NUM (SUB-1).
           IF SUB-1 < 500
                GO TO CTIM-010.
           MOVE 1 TO SUB-1.
       CTIM-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CR-CAMS-TRANS-FILE
            IF WS-CRCAMSTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRCAMSTRANS-ST1
               MOVE "CRCAMS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
        OPEN-008.
            OPEN I-O CB-MASTER.
            IF WS-CB-ST1 NOT = 0
               MOVE 0 TO WS-CB-ST1
               MOVE "CB-MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-008.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-012.

           PERFORM READ-GLPARAMETER.
           MOVE GLPA-CURRENT-CRPER TO WS-NO.
           PERFORM ENTER-PERIOD-DATES.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrCAMSIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CR-CAMS-TRANS-FILE
                 CB-MASTER
                 GLPARAMETER-FILE.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldDate".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldRec".
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "NumberCheck".
       Copy "DisplayFormCRTopInfo".
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
      * END-OF-JOB
