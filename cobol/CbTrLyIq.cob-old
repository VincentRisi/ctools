        IDENTIFICATION DIVISION.
        PROGRAM-ID. CbTrLyIq.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectCbMasterLy".
        Copy "SelectCbTransLy".
        Copy "SelectGlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCbMastLy.
           COPY ChlfdCbTransLy.
           COPY ChlfdGlParam.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "CbMaLyIq".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-NO-OF-TRANS       PIC 9(4) VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-TRANS-AMT         PIC S9(8)V99 VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  LINES-DISPLAYED      PIC 9(5) VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       01  WS-CB-LY-STATUS.
           03  WS-CB-LY-ST1       PIC 99.
       01  WS-CBTRANS-LY-STATUS.
           03  WS-CBTRANS-LY-ST1  PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  WS-PERIOD.
           03  WS-1ST-CHAR        PIC X.
           03  WS-PER             PIC 99.
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
       01  WS-CBNUMBER.
           03  WS-HEAD-SUB.
               05  WS-CBHEADER     PIC X(2).
               05  WS-CBSUBHEADER  PIC X(4).
           03  WS-RESTOFACCOUNT    PIC X(6).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(55) VALUE
           "** CASHBOOK LAST YEAR TRANSACTION INQUIRY **".
           03  FILLER         PIC X(7) VALUE "PERIOD:".
           03  H1-PERIOD      PIC X(13).
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(20) VALUE "ACCOUNT            :".
           03  H3-ACC         PIC X(112).
       01  HEAD2-1.
           03  FILLER         PIC X(20) VALUE "NAME               :".
           03  H3-NAME        PIC X(112).
       01  HEAD2-2.
           03  H3-DESC        PIC X(20).
           03  H3-BALANCE     PIC Z(7)9.99-.
           03  FILLER         PIC X(100) VALUE " ".
       01  HEAD2-3.
           03  FILLER         PIC X(20) VALUE "PERIOD READ        :".
           03  H3-PERIOD-READ PIC X(112).
       01  HEAD3.
           03  FILLER         PIC X(11) VALUE "PRD TRANS".
           03  FILLER         PIC X(18) VALUE "TYPE  JRN No:".
           03  FILLER         PIC X(15) VALUE "TYPE     LGR".
           03  FILLER         PIC X(22) VALUE "STM  ACC NUMBER".
           03  FILLER         PIC X(12) VALUE "DATE".
           03  FILLER         PIC X(12) VALUE "DEBIT AMT".
           03  FILLER         PIC X(12) VALUE "CREDIT AMT".
           03  FILLER         PIC X(26) VALUE "DESCRIPTION".
           03  FILLER         PIC X(21) VALUE " ".
       01  DETAIL-LINE.
           03  D-PERIOD       PIC X(4).
           03  D-TRANS        PIC X(8).
           03  D-TYPE         PIC X(5).
           03  D-JRNNO        PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-TYPE-DESC    PIC X(10).
           03  FILLER         PIC X VALUE " ".
           03  D-TYPE-OF-POST PIC X(6).
           03  D-ALLOCATED    PIC X(4).
           03  D-ACCNO        PIC X(15).
           03  D-DATE         PIC X(10).
           03  FILLER         PIC X(2) VALUE " ".
           03  D-DRAMT        PIC Z(7)9.99- BLANK WHEN ZERO.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-CRAMT        PIC Z(7)9.99- BLANK WHEN ZERO.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-DESC         PIC X(25).
        01  TOTAL-LINE.
           03  FILLER         PIC X(43) VALUE " ".
           03  FILLER         PIC X(27) VALUE
             "TOTAL AMOUNT FOR PERIOD:".
           03  T-RANDAMT      PIC Z(7)9.99-.
           03  FILLER         PIC X(52) VALUE " ".
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
           DISPLAY "** CASH-BOOK L/YEAR TRANSACTION INQUIRY PROGRAM **"
           AT POS
           MOVE 0410 TO POS
           DISPLAY "**************************************************"
           AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
           MOVE 2910 TO POS
           DISPLAY "Program now loading, Please be patient.." AT POS.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM DISPLAY-FORM-CB-TOP-INFO.
           PERFORM GET-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE "                               " TO F-NAMEFIELD.
            MOVE "N" TO WS-ANSWER.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 GO TO GET-999.
            IF F-EXIT-CH = X"0C"
                 PERFORM READ-CBMASTER-NEXT
                 GO TO GET-010.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF F-NAMEFIELD = "   "
                CLOSE CB-LY-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                PERFORM DISPLAY-FORM-CB-TOP-INFO
                GO TO GET-000.
           IF ALPHA-RATE > SPACES
                PERFORM NUMBER-CHECK.
            MOVE WS-GLNO-CHECK TO WS-CBNUMBER.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE WS-CBNUMBER TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM READ-CBMASTER.
            GO TO GET-020.
       GET-010.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CB-LY-NUMBER TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-020.
            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE CB-LY-DESCRIPTION TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            IF CB-LY-DESCRIPTION = "UNKNOWN"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.

            MOVE "BALANCE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CB-LY-BALANCE TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.
        GET-030.
            MOVE "                               " TO F-NAMEFIELD.
            MOVE "READPERIOD" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-000.
            MOVE 2 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"07"
                 GO TO GET-999.
            IF F-EXIT-CH NOT = X"0A"
                  DISPLAY " " AT 3079 WITH BELL
                  GO TO GET-030.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-PER.
            IF WS-PER = 0
               MOVE "  " TO F-NAMEFIELD
            ELSE
               MOVE WS-PER TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            MOVE " " TO WS-1ST-CHAR.
       GET-040.
            MOVE " " TO F-EXIT-CH.

            PERFORM READ-ALL-TRANSACTIONS.
            MOVE "TOTTRANS" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-NO-OF-TRANS TO F-EDNAMEFIELDNUM.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-CHANGE.

            MOVE "TRANSAMT" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-TRANS-AMT TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

           MOVE 0 TO LINES-DISPLAYED.
           IF WS-NO-OF-TRANS = 0
                GO TO GET-900.
           PERFORM READ-TRANSACTIONS.
       GET-900.
            IF WS-ANSWER = "Y"
               CLOSE CBTRANS-LY-FILE
               GO TO GET-999.
            IF F-INDEX < 15
             IF F-INDEX NOT = X"07"
               PERFORM ERROR-020
               MOVE 2910 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen." AT POS
               MOVE 3010 TO POS
               DISPLAY "Or Press 'F10' To Print All Transactions" AT POS
               MOVE 15 TO F-INDEX
               PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"07" AND NOT = X"1F"
                MOVE 1 TO F-INDEX
                GO TO GET-900.
            PERFORM ERROR1-020.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                OPEN I-O CBTRANS-LY-FILE
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE CBTRANS-LY-FILE
                PERFORM ERROR1-020
                PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           OPEN I-O CBTRANS-LY-FILE.
           IF WS-CBTRANS-LY-ST1 NOT = 0
              MOVE "CBTRANS-LY BUSY ON OPEN, RDTR-000, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-LY-ST1
              CLOSE CBTRANS-LY-FILE
              GO TO RDTR-000.
           MOVE 1 TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
       RDTR-005.
           MOVE CB-LY-NUMBER TO CBTRANS-LY-CBMASTER.
           MOVE 0            TO CBTRANS-LY-DATE.
           START CBTRANS-LY-FILE KEY NOT < CBTRANS-LY-ALT-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-LY-ST1 = 23 OR 35 OR 49
               MOVE "DESC" TO F-FIELDNAME
               MOVE 4 TO F-CBFIELDNAME
               MOVE 25 TO F-CBFIELDLENGTH
               MOVE 1 TO F-INDEX
               CLOSE CBTRANS-LY-FILE
               GO TO RDTR-999.
       RDTR-010.
           READ CBTRANS-LY-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CBTRANS-LY-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE CBTRANS-LY-FILE
               GO TO RDTR-900.
           IF WS-CBTRANS-LY-ST1 NOT = 0
              MOVE "CBTRANS-LY BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-LY-ST1
              GO TO RDTR-010.
           IF CBTRANS-LY-CBMASTER NOT = CB-LY-NUMBER
               MOVE 1 TO F-INDEX
               CLOSE CBTRANS-LY-FILE
               GO TO RDTR-900.
           IF WS-PER NOT = 0
            IF CBTRANS-LY-NO NOT = WS-PER
              MOVE 2910 TO POS
              DISPLAY "Reading Next Trans For Period Selected..." AT POS
              GO TO RDTR-010.
           PERFORM ERROR1-020.
           PERFORM ERROR-020.
       RDTR-020. 
           IF F-INDEX > 15
                MOVE 2910 TO POS
                DISPLAY "Press 'PgDn' For More, Or" AT POS
                ADD 27 TO POS
                DISPLAY "'ESC' To Clear The Screen !" AT POS
                MOVE 3010 TO POS
                DISPLAY "Or Press 'F10' To Print All Transactions"
                   AT POS
                ADD 40 TO POS
                DISPLAY "For This Account Number." AT POS
                MOVE 15 TO F-INDEX
                PERFORM USER-FILL-FIELD.
            PERFORM ERROR1-020.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-TRANSACTIONS
                MOVE " " TO F-EXIT-CH
                MOVE 1 TO F-INDEX.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                GO TO RDTR-900.
           IF F-EXIT-CH = X"1F"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO RDTR-900.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C"
                    AND NOT = X"07" AND NOT = " " AND NOT = X"1F"
                MOVE 16 TO F-INDEX
                GO TO RDTR-020.
           ADD 1 TO LINES-DISPLAYED.
           PERFORM SCROLLING.
           ADD 1 TO F-INDEX.
           GO TO RDTR-010.
       RDTR-900.
           PERFORM ERROR1-020.
           PERFORM ERROR-020.
       RDTR-999.
           EXIT.
      *
       READ-ALL-TRANSACTIONS SECTION.
       RALT-000.
           OPEN I-O CBTRANS-LY-FILE.
           IF WS-CBTRANS-LY-ST1 NOT = 0
              MOVE "CBTRANS-LY BUSY ON OPEN, RALT-000, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-LY-ST1
              CLOSE CBTRANS-LY-FILE
              GO TO RALT-000.
           MOVE 2910 TO POS.
           DISPLAY "Reading All transactions......" AT POS.
       RALT-005.
           MOVE 0 TO WS-NO-OF-TRANS WS-TRANS-AMT.
           MOVE CB-LY-NUMBER TO CBTRANS-LY-CBMASTER.
           MOVE 0            TO CBTRANS-LY-DATE.
           START CBTRANS-LY-FILE KEY NOT < CBTRANS-LY-ALT-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-LY-ST1 = 23 OR 35 OR 49
               GO TO RALT-900.
       RALT-010.
           READ CBTRANS-LY-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CBTRANS-LY-ST1 = 10
               GO TO RALT-900.
           IF WS-CBTRANS-LY-ST1 NOT = 0
              MOVE "CBTRANS-LY BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-LY-ST1
              GO TO RALT-010.
           IF CBTRANS-LY-CBMASTER NOT = CB-LY-NUMBER
               GO TO RALT-900.
           IF WS-PER NOT = 0
            IF CBTRANS-LY-NO NOT = WS-PER
               GO TO RALT-010.
           ADD CBTRANS-LY-AMOUNT TO WS-TRANS-AMT
           ADD 1                 TO WS-NO-OF-TRANS
           GO TO RALT-010.
       RALT-900.
           PERFORM ERROR-020.
           CLOSE CBTRANS-LY-FILE.
       RALT-999.
           EXIT.
      *
       READ-CBMASTER SECTION.
       RD-010.
           MOVE WS-CBNUMBER TO CB-LY-NUMBER.
           START CB-LY-MASTER KEY NOT < CB-LY-KEY.
       RD-015.
           READ CB-LY-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-LY-ST1 = 23 OR 35 OR 49
               MOVE "UNKNOWN" TO CB-LY-DESCRIPTION
               GO TO RD-999.
           IF WS-CB-LY-ST1 NOT = 0
              MOVE"CBMASTERLY BUSY ON READ, RD-015 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CB-LY-ST1
              GO TO RD-015.
       RD-999.
           EXIT.
      *
       START-CBMASTER SECTION.
       ST-ST-000.
              MOVE WS-CBNUMBER TO CB-LY-NUMBER.
              START CB-LY-MASTER KEY NOT < CB-LY-NUMBER.
       ST-ST-999.
             EXIT.
      *
       READ-CBMASTER-NEXT SECTION.
       R-ST-NX-000.
             MOVE 0 TO WS-CB-LY-ST1.
             MOVE " " TO WS-MESSAGE
             MOVE 3010 TO POS
             DISPLAY WS-MESSAGE AT POS.
       R-ST-NX-005. 
             READ CB-LY-MASTER NEXT
                 AT END NEXT SENTENCE.
             IF WS-CB-LY-ST1 NOT = 0
              MOVE"CBMASTERLY BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CB-LY-ST1
              PERFORM START-CBMASTER
              GO TO R-ST-NX-005.
       R-ST-NX-999.
             EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-001.
           MOVE 0  TO PAGE-CNT WS-TRANS-AMT.
           MOVE 66 TO LINE-CNT.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE CB-LY-NUMBER TO CBTRANS-LY-CBMASTER.
           MOVE 0            TO CBTRANS-LY-DATE.
           START CBTRANS-LY-FILE KEY NOT < CBTRANS-LY-ALT-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-LY-ST1 NOT = 0
               GO TO PRR-900.
       PRR-002.
           READ CBTRANS-LY-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CBTRANS-LY-ST1 = 10
               MOVE 0 TO WS-CBTRANS-LY-ST1
               GO TO PRR-900.
            IF WS-CBTRANS-LY-ST1 NOT = 0
              MOVE "CBTRANS-LY BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-LY-ST1
              GO TO PRR-002.
            IF CBTRANS-LY-CBMASTER < CB-LY-NUMBER
               GO TO PRR-002.
            IF CBTRANS-LY-CBMASTER > CB-LY-NUMBER
               GO TO PRR-900.
           IF WS-PER NOT = 0
            IF CBTRANS-LY-NO NOT = WS-PER
               GO TO PRR-002.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           MOVE CBTRANS-LY-PERIOD               TO D-PERIOD
           MOVE CBTRANS-LY-TRANS                TO D-TRANS
           MOVE CBTRANS-LY-TYPE                 TO D-TYPE 
           MOVE CBTRANS-LY-REFERENCE            TO D-JRNNO
           MOVE WS-TRANS-DESC (CBTRANS-LY-TYPE) TO D-TYPE-DESC
           MOVE CBTRANS-LY-TYPE-OF-POST         TO D-TYPE-OF-POST
           MOVE CBTRANS-LY-ALLOCATED            TO D-ALLOCATED
           MOVE CBTRANS-LY-ACCOUNT-NUMBER       TO D-ACCNO
           MOVE CBTRANS-LY-DATE                 TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE                    TO D-DATE.
           IF CBTRANS-LY-AMOUNT < 0
              MOVE 0                            TO D-DRAMT
              MOVE CBTRANS-LY-AMOUNT            TO D-CRAMT
           ELSE
              MOVE 0                            TO D-CRAMT
              MOVE CBTRANS-LY-AMOUNT            TO D-DRAMT.
           MOVE CBTRANS-LY-LINE-DESC            TO D-DESC
           WRITE PRINT-REC FROM DETAIL-LINE
           ADD CBTRANS-LY-AMOUNT TO WS-TRANS-AMT
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           GO TO PRR-002.
       PRR-060.
            ADD 1                       TO PAGE-CNT
            MOVE PAGE-CNT               TO H1-PAGE
            MOVE GLPA-CURRENT-GLPER     TO H1-PERIOD.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
            MOVE " "                    TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " "                    TO PRINT-REC
            MOVE CB-LY-NUMBER           TO H3-ACC
            WRITE PRINT-REC FROM HEAD2 AFTER 2
            MOVE " "                    TO PRINT-REC
            MOVE CB-LY-DESCRIPTION      TO H3-NAME
            WRITE PRINT-REC FROM HEAD2-1 AFTER 1
            MOVE " " TO PRINT-REC
            MOVE "CURRENT BALANCE    :" TO H3-DESC
            MOVE CB-LY-BALANCE          TO H3-BALANCE
            WRITE PRINT-REC FROM HEAD2-2 AFTER 1
            MOVE " "                    TO PRINT-REC
            MOVE "YEAR OPEN BALANCE  :" TO H3-DESC
            MOVE CB-LY-OPEN-YEAR-BAL    TO H3-BALANCE
            WRITE PRINT-REC FROM HEAD2-2 AFTER 1
            MOVE " "                    TO PRINT-REC
            MOVE "LAST YEAR BALANCE  :" TO H3-DESC
            MOVE CB-LY-LAST-YEAR-BAL    TO H3-BALANCE
            WRITE PRINT-REC FROM HEAD2-2 AFTER 1
            MOVE " "                    TO PRINT-REC.
            IF WS-PER = 0
                MOVE "ALL PERIODS"  TO H3-PERIOD-READ
            ELSE
                MOVE WS-PER         TO H3-PERIOD-READ.
            WRITE PRINT-REC FROM HEAD2-3 AFTER 1
            MOVE " "                TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " "                TO PRINT-REC
            WRITE PRINT-REC
            MOVE " "                TO PRINT-REC
            MOVE 10 TO LINE-CNT.
       PRR-900.
           MOVE WS-TRANS-AMT TO T-RANDAMT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " "              TO PRINT-REC
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       PRR-999.
           EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            MOVE "LINES"         TO F-FIELDNAME
            MOVE 5               TO F-CBFIELDNAME
            MOVE LINES-DISPLAYED TO F-EDNAMEFIELDCRED
            MOVE 5               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-CRED.

            MOVE "JRN" TO F-FIELDNAME
            MOVE 3 TO F-CBFIELDNAME
            MOVE CBTRANS-LY-REFERENCE TO F-NAMEFIELD
            MOVE 10 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPE" TO F-FIELDNAME
            MOVE 4 TO F-CBFIELDNAME
            MOVE CBTRANS-LY-TYPE TO F-NAMEFIELD
            MOVE 2 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

      * removed as the form does not fit the screen
      *      MOVE "TYPE-OF-POST" TO F-FIELDNAME
      *      MOVE 12 TO F-CBFIELDNAME
      *      MOVE CBTRANS-LY-TYPE-OF-POST TO F-NAMEFIELD
      *      MOVE 1 TO F-CBFIELDLENGTH
      *      PERFORM WRITE-FIELD-ALPHA.

            MOVE "ALLOCATED" TO F-FIELDNAME
            MOVE 9 TO F-CBFIELDNAME
            MOVE CBTRANS-LY-ALLOCATED TO F-NAMEFIELD
            MOVE 1 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLACCNO" TO F-FIELDNAME
            MOVE 7 TO F-CBFIELDNAME
            MOVE CBTRANS-LY-ACCOUNT-NUMBER TO F-NAMEFIELD
            MOVE 12 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE CBTRANS-LY-DATE TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE TO F-NAMEFIELD
            MOVE 10     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "RAND"            TO F-FIELDNAME
            MOVE 4                 TO F-CBFIELDNAME
            MOVE CBTRANS-LY-AMOUNT TO F-EDNAMEFIELDREC
            MOVE 12                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.

            MOVE "DESC" TO F-FIELDNAME
            MOVE 4 TO F-CBFIELDNAME
            MOVE CBTRANS-LY-LINE-DESC TO F-NAMEFIELD
            MOVE 25 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO SUB-1 F-INDEX.

            MOVE "LINES" TO F-FIELDNAME
            MOVE 5 TO F-CBFIELDNAME
            MOVE " " TO F-NAMEFIELD
            MOVE 5 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       CLTR-010.
            IF SUB-1 > 15
                GO TO CLTR-999.
            MOVE "JRN" TO F-FIELDNAME
            MOVE 3 TO F-CBFIELDNAME
            MOVE " "TO F-NAMEFIELD
            MOVE 10 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPE" TO F-FIELDNAME
            MOVE 4 TO F-CBFIELDNAME
            MOVE " " TO F-NAMEFIELD
            MOVE 2 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
      * removed as the form does not fit the screen
      *      MOVE "TYPE-OF-POST" TO F-FIELDNAME
      *      MOVE 12 TO F-CBFIELDNAME
      *      MOVE " " TO F-NAMEFIELD
      *      MOVE 1 TO F-CBFIELDLENGTH
      *      PERFORM WRITE-FIELD-ALPHA.

            MOVE "ALLOCATED" TO F-FIELDNAME
            MOVE 9 TO F-CBFIELDNAME
            MOVE " " TO F-NAMEFIELD
            MOVE 1 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLACCNO" TO F-FIELDNAME
            MOVE 7 TO F-CBFIELDNAME
            MOVE " " TO F-NAMEFIELD
            MOVE 12 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 10     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "RAND" TO F-FIELDNAME
            MOVE 4 TO F-CBFIELDNAME
            MOVE " " TO F-NAMEFIELD
            MOVE 12 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESC" TO F-FIELDNAME
            MOVE 4 TO F-CBFIELDNAME
            MOVE " " TO F-NAMEFIELD
            MOVE 25 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            ADD 1 TO SUB-1 F-INDEX
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
              MOVE "GLPARAMETER BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CB-LY-MASTER.
            IF WS-CB-LY-ST1 NOT = 0
               MOVE "CBMASTERLY BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CB-LY-ST1
              GO TO OPEN-000.
       OPEN-005.
            OPEN I-O GLPARAMETER-FILE.
            IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE "GLPARAMETER FILE BUSY ON OPEN,  'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-005.
           PERFORM READ-PARAMETER.
           MOVE GLPA-NAME TO CO-NAME.
           PERFORM ENTER-PERIOD-DATES.
           PERFORM OPEN-006.
           CLOSE GLPARAMETER-FILE.
       OPEN-006.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CbTrLyIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE CB-LY-MASTER.
       END-900.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldChange".
       Copy "WriteFieldCred".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldRec".
       Copy "WriteFieldPrice".
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "NumberCheck".
       Copy "DisplayFormCBTopInfo".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
      *
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "Error1Message".
       Copy "ErrorMessage".
      *
      * END-OF-JOB.
