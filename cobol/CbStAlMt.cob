        IDENTIFICATION DIVISION.
        PROGRAM-ID. CbStAlMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectCbMaster".
        Copy "SelectCbTrans".
        Copy "SelectGlParameter".
        Copy "SelectGlMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCbTrans.
           COPY ChlfdCbMast.
           COPY ChlfdGlMast.
           COPY ChlfdGlParam.

       WORKING-STORAGE SECTION.
       77  Ws-CbMast          PIC X(12).
       77  WS-REFERENCE       PIC X(10) VALUE " ".      
       77  WS-ONLY-UNALLOC    PIC X VALUE " ".
       77  WS-END             PIC X VALUE " ".
       77  WS-ABOVE-BODY      PIC X VALUE " ".
       01  WS-PERIOD.
           03  WS-FUTURE      PIC X.
           03  WS-NO          PIC 99.
       01  WS-BATCH.
           03  WS-BATCH-1STCHAR  PIC X(2) VALUE "CB".
           03  WS-BATCH-REST     PIC X(8).
       01  WS-CBTRANS-STATUS.
           03  WS-CBTRANS-ST1     PIC 99.
       01  WS-CB-STATUS.
           03  WS-CB-ST1          PIC 99.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1      PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  WS-TRANS-TYPE.
           03  FILLER        PIC X(7) VALUE "CHQ PAY".
           03  FILLER        PIC X(7) VALUE "MTX PAY".
           03  FILLER        PIC X(7) VALUE "TRN PAY".
           03  FILLER        PIC X(7) VALUE "R/D DEP".
           03  FILLER        PIC X(7) VALUE "STL CHQ".
           03  FILLER        PIC X(7) VALUE "BNK CHG".
           03  FILLER        PIC X(7) VALUE "BNK INT".
           03  FILLER        PIC X(7) VALUE "TRN 1DY".
           03  FILLER        PIC X(7) VALUE "CR ADJ ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "CHQ DEP".
           03  FILLER        PIC X(7) VALUE "TRN DEP".
           03  FILLER        PIC X(7) VALUE "R/D PAY".
           03  FILLER        PIC X(7) VALUE "STL CHQ".
           03  FILLER        PIC X(7) VALUE "BNK REV".
           03  FILLER        PIC X(7) VALUE "BNK INT".
           03  FILLER        PIC X(7) VALUE "TRN 1DY".
           03  FILLER        PIC X(7) VALUE "DR ADJ ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
       01  WS-TRANS-DESCRED REDEFINES WS-TRANS-TYPE.
           03  WS-TRANS-DESC   PIC X(7) OCCURS 30.
       01  WS-ALL-LINES.
           03  WS-LINES OCCURS 500.
               05  WS-TRANS-NUM     PIC 9(6).
               05  WS-TYPE-NUM      PIC 99.
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 15.
               05  WS-TRANS          PIC 9(6).
               05  WS-TYPE           PIC 99.
               05  WS-TRANS-DATE     PIC 9(8).
               05  WS-TYPE-OF-POST   PIC X.
               05  WS-ALLOCATED      PIC X.
               05  WS-ACCOUNT-NUMBER PIC X(12).
               05  WS-TRANS-AMT      PIC S9(7)V99.
               05  WS-GLDESC         PIC X(40).
               05  WS-LINE-DESC      PIC X(25).
               05  WS-NEWLINE        PIC X.
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
           PERFORM DISPLAY-CB-NO-TOP-INFO.
           PERFORM CLEAR-TRANS-IN-MEM.
           PERFORM GET-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO CBTRANS-REC
                        WS-END.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
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
       GET-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PERIOD"      TO F-FIELDNAME
            MOVE 6             TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            MOVE 2          TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            IF F-EXIT-CH = X"01"
               GO TO GET-002.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-NO.
            IF WS-NO NOT > 0
               MOVE "A VALID PERIOD MUST BE ENTERED, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-010.
            IF WS-NO > 12
               MOVE "A VALID PERIOD MUST BE ENTERED, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
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
               GO TO GET-010.
            MOVE F-NAMEFIELD  TO WS-ONLY-UNALLOC.
            IF WS-ONLY-UNALLOC NOT = "N" AND NOT = "Y"
               MOVE "ENTER ONLY 'Y' OR 'N' - PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
            PERFORM DISPLAY-CB-NO-TOP-INFO.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       GET-500.
            MOVE " " TO WS-ABOVE-BODY.
            PERFORM FILL-DATA.
            IF WS-ABOVE-BODY = "1"
               GO TO GET-010.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-000.
           MOVE 0 TO SUB-25 SUB-15.
           PERFORM READ-ALL-TRANSACTIONS.
       FILL-001.
           MOVE 1 TO SUB-1 SUB-2 F-INDEX.
           PERFORM READ-NEXT-TRANSACTIONS.
           MOVE "TOTLINENO"  TO F-FIELDNAME
           MOVE 9            TO F-CBFIELDNAME
           MOVE SUB-15       TO F-EDNAMEFIELDNUM
           MOVE 6            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-NUMERIC.
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
           MOVE "LINENO"     TO F-FIELDNAME
           MOVE 6            TO F-CBFIELDNAME
           MOVE SUB-1        TO F-EDNAMEFIELDANAL
           MOVE 2            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ANALYSIS.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
       FILL-013.
           MOVE "                    " TO F-NAMEFIELD.
           MOVE "ALLOCATED" TO F-FIELDNAME
           MOVE 9           TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           MOVE 1 TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA
           MOVE F-NAMEFIELD TO WS-ALLOCATED (SUB-1).
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
              GO TO FILL-060.
           IF WS-ALLOCATED (SUB-1) NOT = "N" AND NOT = "Y"
                               AND NOT = "H"
               MOVE "THE FIELD MUST BE EITHER 'H' 'N' OR 'Y', RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-013.
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
              GO TO FILL-060.
           IF F-EXIT-CH = X"0A" AND F-INDEX = 15
              GO TO FILL-060.
           IF F-EXIT-CH = X"12" AND F-INDEX = 15
              GO TO FILL-060.
           IF F-EXIT-CH = X"07"
              PERFORM FILL-060
              GO TO FILL-999.

      *X"12" = AUTO EXIT FROM FIELD.
           IF F-EXIT-CH NOT = X"01" AND NOT = X"0A" AND NOT = X"0B"
                    AND NOT = X"07" AND NOT = X"09" AND NOT = X"12"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO FILL-013.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           IF WS-TYPE-OF-POST (SUB-1) = "S"
              GO TO FILL-015
           ELSE
              GO TO FILL-005.
       FILL-015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            IF SUB-1 > 1
              COMPUTE SUB-4 = SUB-1 - 1
              MOVE WS-TRANS-DATE (SUB-4) TO WS-TRANS-DATE (SUB-1)
                                            SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE WS-CONVERT-DATE TO DISPLAY-DATE
              MOVE DISPLAY-DATE    TO F-NAMEFIELD
              MOVE 10              TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-ALPHA.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO FILL-005.
            MOVE 10      TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-015.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE
            MOVE DISPLAY-DATE    TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA
            PERFORM CONVERT-SPLIT-FORMAT
            MOVE SPLIT-DATE TO WS-TRANS-DATE (SUB-1)
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-015.
            MOVE GL-BEGDATE (WS-NO) TO WS-BEG-DATE
            MOVE GL-ENDDATE (WS-NO) TO WS-END-DATE.
            IF WS-TRANS-DATE (SUB-1) > WS-END-DATE
            MOVE "THE JOURNAL DATE MUST BE < OR = THE END PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-015.
            IF WS-TRANS-DATE (SUB-1) < WS-BEG-DATE
            MOVE "THE JOURNAL DATE MUST > OR = THE BEG. PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-015.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "GLACCNO"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"8A"
             IF SUB-1 > 1
             COMPUTE SUB-4 = SUB-1 - 1
             MOVE WS-ACCOUNT-NUMBER (SUB-4) TO WS-ACCOUNT-NUMBER (SUB-1)
                                               F-NAMEFIELD
              MOVE 12 TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
            MOVE 12          TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF ALPHA-RATE > SPACES
               PERFORM NUMBER-CHECK
               MOVE WS-GLNO-CHECK TO WS-ACCOUNT-NUMBER (SUB-1)
            IF SIGN-FOUND = 9
               MOVE 0 TO SIGN-FOUND
               GO TO FILL-020.
            MOVE WS-ACCOUNT-NUMBER (SUB-1) TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM READ-GLNUMBER.
            IF GL-DESCRIPTION = " "
               MOVE "THIS MUST BE AN EXISTING GL-ACCOUNT NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-020.
 
            MOVE GL-DESCRIPTION    TO WS-GLDESC (SUB-1)
            MOVE "GLACCNAME"       TO F-FIELDNAME
            MOVE 9                 TO F-CBFIELDNAME
            MOVE WS-GLDESC (SUB-1) TO F-NAMEFIELD
            MOVE 40                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "AMOUNT"     TO F-FIELDNAME
            MOVE 6            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            MOVE 12           TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 WS-TRANS-AMT (SUB-1)
            PERFORM WRITE-FIELD-REC.
            
            IF SUB-3 < 20
             IF WS-TRANS-AMT (SUB-1) NOT < 0
               MOVE "FOR TRANS TYPE < 20, AMOUNT MUST BE < 0, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-025.
            IF SUB-3 > 19
             IF WS-TRANS-AMT (SUB-1) NOT > 0
               MOVE "FOR TRANS TYPE > 19, AMOUNT MUST BE > 0, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-025.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DESC"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            MOVE 25          TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO WS-LINE-DESC (SUB-1).
            IF F-EXIT-CH = X"8A"
               MOVE WS-BATCH TO WS-LINE-DESC (SUB-1) F-NAMEFIELD
               MOVE 25       TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               GO TO FILL-030.
            IF WS-LINE-DESC (SUB-1) = " "
               MOVE "YOU MUST ENTER A LINE DESCRIPTION, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-030.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
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
            DISPLAY "WRITING CHANGES TO CBTRANS-FILE....." AT POS
            PERFORM WRITE-CBTRANS
            PERFORM CLEAR-FIELDS
            PERFORM CLEAR-BODY.
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
       WRITE-CBTRANS SECTION.
       WCBT-001.
           MOVE 1 TO SUB-1.
           IF WS-ACCOUNT-NUMBER (SUB-1) = " "
              GO TO WCBT-900.
       WCBT-002.
           MOVE WS-TRANS (SUB-1) TO CBTRANS-TRANS
           MOVE WS-TYPE (SUB-1)  TO CBTRANS-TYPE.
           START CBTRANS-FILE KEY NOT < CBTRANS-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON START, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO WCBT-002.
           GO TO WCBT-010.
       WCBT-005.
           MOVE WS-TRANS (SUB-1) TO CBTRANS-TRANS
           MOVE WS-TYPE (SUB-1)  TO CBTRANS-TYPE.
       WCBT-010.
           READ CBTRANS-FILE WITH LOCK
                  INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON READ-WCBT-010 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO WCBT-010.
           MOVE WS-NO                     TO CBTRANS-NO
           MOVE Ws-CbMast                 TO CBTRANS-CBMASTER
           MOVE WS-TRANS-DATE (SUB-1)     TO CBTRANS-DATE
           MOVE WS-TYPE-OF-POST (SUB-1)   TO CBTRANS-TYPE-OF-POST
           MOVE WS-ALLOCATED (SUB-1)      TO CBTRANS-ALLOCATED
           MOVE WS-ACCOUNT-NUMBER (SUB-1) TO CBTRANS-ACCOUNT-NUMBER
           MOVE WS-TRANS-AMT (SUB-1)      TO CBTRANS-AMOUNT
           MOVE WS-LINE-DESC (SUB-1)      TO CBTRANS-LINE-DESC.
       WCBT-018.
           REWRITE CBTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON REWRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO WCBT-018.
       WCBT-020.
            ADD 1 TO SUB-1.
            IF SUB-1 < 16
             IF WS-ACCOUNT-NUMBER (SUB-1) = " "
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
           MOVE WS-TRANS-NUM (SUB-15) TO CBTRANS-TRANS
           MOVE WS-TYPE-NUM (SUB-15)  TO CBTRANS-TYPE.
           PERFORM START-TRANS.
           IF WS-CBTRANS-ST1 NOT = 0
              GO TO RONX-999.
       RONX-005.
           READ CBTRANS-FILE
              INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-ST1 = 23 OR 35 OR 49
              GO TO RONX-030.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS-FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO RONX-005.
       RONX-010.
            MOVE CBTRANS-TRANS             TO WS-TRANS (SUB-1)
            MOVE CBTRANS-TYPE              TO WS-TYPE (SUB-1)
            MOVE CBTRANS-DATE              TO WS-TRANS-DATE (SUB-1)
            MOVE CBTRANS-TYPE-OF-POST      TO WS-TYPE-OF-POST (SUB-1)
            MOVE CBTRANS-ALLOCATED         TO WS-ALLOCATED (SUB-1)
            MOVE CBTRANS-ACCOUNT-NUMBER    TO WS-ACCOUNT-NUMBER (SUB-1)
            MOVE CBTRANS-AMOUNT            TO WS-TRANS-AMT (SUB-1)
            MOVE CBTRANS-LINE-DESC         TO WS-LINE-DESC (SUB-1)
            MOVE "N"                       TO WS-NEWLINE (SUB-1).
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
            MOVE WS-TRANS-NUM (SUB-15) TO CBTRANS-TRANS
            MOVE WS-TYPE-NUM (SUB-15)  TO CBTRANS-TYPE.
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
           PERFORM START-TRANS-BY-DATE.
           IF WS-CBTRANS-ST1 NOT = 0
              GO TO RALL-999.
           MOVE 1 TO SUB-25.
       RALL-005.
           READ CBTRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-CBTRANS-ST1 = 10
              SUBTRACT 1 FROM SUB-25
              GO TO RALL-900.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS-FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO RALL-005.
            IF CBTRANS-NO NOT = WS-NO
              GO TO RALL-005.
            IF CBTRANS-CBMASTER NOT = Ws-CbMast
              GO TO RALL-005.
            IF WS-ONLY-UNALLOC = "Y"
             IF CBTRANS-ALLOCATED = "Y"
              GO TO RALL-005.
       RALL-010.
            MOVE CBTRANS-TRANS TO WS-TRANS-NUM (SUB-25)
            MOVE CBTRANS-TYPE  TO WS-TYPE-NUM (SUB-25).
       RALL-020.
            IF SUB-25 < 501
               ADD 1 TO SUB-25
               GO TO RALL-005.
       RALL-900.
           CLOSE CBTRANS-FILE.
           PERFORM OPEN-000.
       RALL-999.
           EXIT.
      *
       DELETE-TRANS SECTION.
       DO-010.
            DELETE CBTRANS-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CBTRANS-ST1
               GO TO DO-010.
       DO-999.
           EXIT.
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE 0 TO WS-CBTRANS-ST1.
           START CBTRANS-FILE KEY NOT < CBTRANS-KEY
                 INVALID KEY NEXT SENTENCE.
       RO-010.
           READ CBTRANS-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-CBTRANS-ST1
                GO TO RO-999.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON READ, PRESS 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO RO-010.
       RO-999.
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
              MOVE "CASH BOOK BUSY ON READ, PRESS 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CB-ST1
              GO TO RD-010.
       RD-999.
           EXIT.
      *
       START-TRANS SECTION.
       ST-OO-000.
           START CBTRANS-FILE KEY NOT < CBTRANS-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "THERE ARE NO TRANSACTIONS FOR THIS PERIOD."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
       ST-OO-999.
             EXIT.
      *
       START-TRANS-BY-DATE SECTION.
       ST-DATE-000.
           START CBTRANS-FILE KEY NOT < CBTRANS-ALT-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "THERE ARE NO TRANSACTIONS BY DATE FOR THIS PERIOD."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
       ST-DATE-999.
             EXIT.
     *
       READ-GLNUMBER SECTION.
       RD-000.
           MOVE WS-ACCOUNT-NUMBER (SUB-1) TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       RD-010.
           READ GL-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
                MOVE " " TO GL-DESCRIPTION
                GO TO RD-999.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER BUSY ON READ, PRESS 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
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
              MOVE "GLPARAMETER BUSY ON READ RP-010, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RP-010.
       RP-999.
           EXIT.
      *
       READ-GLPARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 1 TO GLPA-RECORD.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       RPL-010.
           READ GLPARAMETER-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RPL-010.
       RPL-999.
           EXIT.
      *
       REWRITE-GLPARAMETER SECTION.
       REWP-000.
           REWRITE GLPARAMETER-REC
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RP-000.
       REWP-999.
           EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            MOVE 1 TO SUB-1 F-INDEX.
       SCROLL-010.
            IF WS-TYPE (SUB-1) = 0
                 GO TO SCROLL-999.
            MOVE "TYPE"          TO F-FIELDNAME
            MOVE 4               TO F-CBFIELDNAME
            MOVE WS-TYPE (SUB-1) TO F-EDNAMEFIELDANAL
            MOVE 2               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE WS-TYPE (SUB-1)       TO SUB-3.
            MOVE "TYPEDESC"            TO F-FIELDNAME
            MOVE 8                     TO F-CBFIELDNAME
            MOVE WS-TRANS-DESC (SUB-3) TO F-NAMEFIELD
            MOVE 7                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPE-OF-POST"          TO F-FIELDNAME
            MOVE 12                      TO F-CBFIELDNAME
            MOVE WS-TYPE-OF-POST (SUB-1) TO F-NAMEFIELD
            MOVE 1                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ALLOCATED"          TO F-FIELDNAME
            MOVE 9                    TO F-CBFIELDNAME
            MOVE WS-ALLOCATED (SUB-1) TO F-NAMEFIELD
            MOVE 1                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLACCNO"                  TO F-FIELDNAME
            MOVE 7                          TO F-CBFIELDNAME
            MOVE WS-ACCOUNT-NUMBER  (SUB-1) TO F-NAMEFIELD
            MOVE 12                         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"                TO F-FIELDNAME
            MOVE 4                     TO F-CBFIELDNAME
            MOVE WS-TRANS-DATE (SUB-1) TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE          TO F-NAMEFIELD
            MOVE 10                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT"             TO F-FIELDNAME
            MOVE 6                    TO F-CBFIELDNAME
            MOVE WS-TRANS-AMT (SUB-1) TO F-EDNAMEFIELDREC
            MOVE 12                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.

            MOVE "DESC"               TO F-FIELDNAME
            MOVE 4                    TO F-CBFIELDNAME
            MOVE WS-LINE-DESC (SUB-1) TO F-NAMEFIELD
            MOVE 25                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-020.
            IF SUB-1 = 15
              GO TO SCROLL-999.
            ADD 1 TO SUB-1 F-INDEX.
            GO TO SCROLL-010.
       SCROLL-999.
             EXIT.
      *
       CLEAR-BODY SECTION.
       CLEAR-000.
            MOVE 0 TO F-INDEX.
       CLEAR-002.
            ADD 1 TO F-INDEX.
            IF F-INDEX > 15
               GO TO CLEAR-BODY-900.

            MOVE "TYPE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 2      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPEDESC" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 7          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPE-OF-POST" TO F-FIELDNAME
            MOVE 12             TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 1              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ALLOCATED" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 1           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 10     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLACCNO" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 12        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESC" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 25     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 12       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            GO TO CLEAR-002.
       CLEAR-BODY-900.
            MOVE "GLACCNAME" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       CLEAR-BODY-999.
             EXIT.
      *
       CANCEL-TRANSACTION SECTION.
       CAN-005.
           COMPUTE SUB-2 = SUB-1 + 1.
       CAN-008.
           PERFORM READ-TRANS.
           PERFORM DELETE-TRANS.
       CAN-010.
           IF SUB-2 > 15
              MOVE 15 TO SUB-1 SUB-2
              GO TO CAN-090.
           IF WS-ACCOUNT-NUMBER (SUB-2) = " "
              MOVE " " TO WS-TYPE-OF-POST (SUB-1)
                          WS-ALLOCATED (SUB-1)
                          WS-ACCOUNT-NUMBER (SUB-1)
                          WS-GLDESC (SUB-1)
                          WS-LINE-DESC (SUB-1)
                          WS-NEWLINE (SUB-1)
              MOVE 0   TO WS-TRANS (SUB-1)
                          WS-TYPE (SUB-1)
                          WS-TRANS-DATE (SUB-1)
                          WS-TRANS-AMT (SUB-1)
              GO TO CAN-090.
           MOVE BODY-LINE (SUB-2) TO BODY-LINE (SUB-1).
           ADD 1 TO SUB-1 SUB-2.
           GO TO CAN-010.
       CAN-090.
           MOVE " " TO WS-TYPE-OF-POST (SUB-1)
                       WS-ALLOCATED (SUB-1)
                       WS-ACCOUNT-NUMBER (SUB-1)
                       WS-GLDESC (SUB-1)
                       WS-LINE-DESC (SUB-1)
                       WS-NEWLINE (SUB-1)
           MOVE 0   TO WS-TRANS (SUB-1)
                       WS-TYPE (SUB-1)
                       WS-TRANS-DATE (SUB-1)
                       WS-TRANS-AMT (SUB-1).
       CAN-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
           MOVE 1 TO SUB-1.
       CF-010.
           ADD 1 TO SUB-1.
           MOVE " " TO WS-TYPE-OF-POST (SUB-1)
                       WS-ALLOCATED (SUB-1)
                       WS-ACCOUNT-NUMBER (SUB-1)
                       WS-GLDESC (SUB-1)
                       WS-LINE-DESC (SUB-1)
                       WS-NEWLINE (SUB-1)
           MOVE 0   TO WS-TRANS (SUB-1)
                       WS-TYPE (SUB-1)
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
           MOVE 0   TO WS-TRANS-NUM (SUB-1)
                       WS-TYPE-NUM (SUB-1).
           IF SUB-1 < 500
                GO TO CTIM-010.
           MOVE 1 TO SUB-1.
       CTIM-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CBTRANS-FILE
            IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CB-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO OPEN-000.
        OPEN-008.
            OPEN I-O CB-MASTER.
            IF WS-CB-ST1 NOT = 0
              MOVE "CB-MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CB-ST1
              GO TO OPEN-008.
        OPEN-009.
            OPEN I-O GL-MASTER.
            IF WS-GLMAST-ST1 NOT = 0
              MOVE "GL-MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO OPEN-009.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-012.

           PERFORM READ-GLPARAMETER.
           MOVE GLPA-CURRENT-CBPER TO WS-NO.
           PERFORM ENTER-PERIOD-DATES.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CbStAlMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CBTRANS-FILE
                 CB-MASTER
                 GLPARAMETER-FILE
                 GL-MASTER.
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
       Copy "DisplayCBNoTopInfo".
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
