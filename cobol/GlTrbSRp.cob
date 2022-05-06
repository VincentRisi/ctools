        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlTrbSRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlMaster".
        Copy "SelectGlParameter".
        Copy "SelectGlTrans".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdGlMast.
           COPY ChlfdGlTrans.
           COPY ChlfdGlParam.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(12) VALUE " ".
       77  WS-RANGE2            PIC X(12) VALUE " ".
       77  WS-ANSWER            PIC 99 VALUE 0.
       77  WS-1ST-HEAD-PRINT    PIC X VALUE " ".
       77  WS-1ST-SUBHEAD-PRINT PIC X VALUE " ".
       77  WS-LAST-ACC-READ     PIC X VALUE " ".
       77  WS-TOTAL-DEBIT       PIC S9(9)V99 VALUE 0.
       77  WS-TOTAL-CREDIT      PIC S9(9)V99 VALUE 0.
       77  WS-TRANS-TOTAL       PIC S9(9)V99 VALUE 0.
       77  WS-TRANS-AMT         PIC S9(9)V99 VALUE 0.
       77  WS-HEAD-ACCOUNT      PIC X(12) VALUE " ".
       77  WS-HEAD-TYPE         PIC X VALUE " ".
       77  WS-HEAD-OPEN-BAL     PIC S9(9)V99 VALUE 0.
       77  WS-HEAD-DR-TOTAL     PIC S9(9)V99 VALUE 0.
       77  WS-HEAD-CR-TOTAL     PIC S9(9)V99 VALUE 0.
       77  WS-HEAD-ACT-TOTAL    PIC S9(9)V99 VALUE 0.
       77  WS-HEAD-CLOSE-BAL    PIC S9(9)V99 VALUE 0.
       77  WS-HEAD-DESC         PIC X(40) VALUE " ".
       77  WS-HEAD-CNT          PIC 9(6) VALUE 0.
       77  WS-SUBHEAD-ACCOUNT   PIC X(12) VALUE " ".
       77  WS-SUBHEAD-TYPE      PIC X VALUE " ".
       77  WS-SUBHEAD-OPEN-BAL  PIC S9(9)V99 VALUE 0.
       77  WS-SUBHEAD-DR-TOTAL  PIC S9(9)V99 VALUE 0.
       77  WS-SUBHEAD-CR-TOTAL  PIC S9(9)V99 VALUE 0.
       77  WS-SUBHEAD-ACT-TOTAL PIC S9(9)V99 VALUE 0.
       77  WS-SUBHEAD-CLOSE-BAL PIC S9(9)V99 VALUE 0.
       77  WS-SUBHEAD-DESC      PIC X(40) VALUE " ".
       77  WS-SUB-CNT           PIC 9(6) VALUE 0.
       77  WS-DETAIL-CNT        PIC 9(6) VALUE 0.
       77  WS-NO-OF-TRANS       PIC 9(6) VALUE 0.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1      PIC 99.
       01  WS-GLTRANS-STATUS.
           03  WS-GLTRANS-ST1     PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-HEADER  PIC X(2).
               05  WS-SUB     PIC X(4).
           03  WS-REST        PIC X(6).
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(50) VALUE
           "GENERAL LEDGER YTD SUMMARY TRIAL BALANCE".
           03  FILLER         PIC X(7) VALUE "PERIOD:".
           03  H1-PERIOD      PIC X(3) VALUE " ".
           03  H1-PER-BEG     PIC X(10).
           03  FILLER         PIC X(4) VALUE " TO ".
           03  H1-PER-END     PIC X(10).
           03  FILLER         PIC X(8) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z(2)9.
           03  FILLER         PIC X(6) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(27) VALUE " ".
           03  FILLER         PIC X(40) VALUE ALL "*".
           03  FILLER         PIC X(65) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(17) VALUE "ACCOUNT     TRAN".
           03  FILLER         PIC X(5) VALUE "TYPE".
           03  FILLER         PIC X(49) VALUE "DESCRIPTION".
           03  FILLER         PIC X(15) VALUE "OPEN BAL".
           03  FILLER         PIC X(12) VALUE "DEBITS".
           03  FILLER         PIC X(12) VALUE "CREDITS".
           03  FILLER         PIC X(12) VALUE "ACTIVITY".
           03  FILLER         PIC X(10) VALUE "CLOSE BAL".
       01  DETAIL-LINE.
           03  D-ACCOUNT         PIC X(19).
           03  D-TYPE            PIC X(3).
           03  D-DESC            PIC X(45).
           03  D-OPEN            PIC Z(8)9.99-.
      *     03  FILLER            PIC X(1) VALUE " ".
           03  D-DEBIT           PIC Z(8)9.99-.
      *     03  FILLER            PIC X(1) VALUE " ".
           03  D-CREDIT          PIC Z(8)9.99-.
      *     03  FILLER            PIC X(1) VALUE " ".
           03  D-ACTIVITY        PIC Z(8)9.99-.
      *     03  FILLER            PIC X(1) VALUE " ".
           03  D-CLOSE           PIC Z(8)9.99-.
       01  TRANS-LINE.
           03  TRANS-REF         PIC X(11).
           03  TRANS-TRANS       PIC Z(5)9.
           03  TRANS-PERIOD      PIC X(3).
           03  FILLER            PIC X(2) VALUE " ".
           03  TRANS-DATE        PIC X(10).
           03  FILLER            PIC X(5) VALUE " ".
           03  TRANS-DESC        PIC X(43).
           03  TRANS-DEBIT       PIC Z(8)9.99- BLANK WHEN ZERO.
      *     03  FILLER            PIC X(1) VALUE " ".
           03  TRANS-CREDIT      PIC Z(8)9.99- BLANK WHEN ZERO.
           03  FILLER            PIC X(26) VALUE " ".
       01  UNDER-LINE.
           03  FILLER            PIC X(19) VALUE " ".
           03  UNDER-DESC        PIC X(48) VALUE " ".
           03  FILLER            PIC X(12) VALUE "============".
           03  FILLER            PIC X(1) VALUE " ".
           03  FILLER            PIC X(12) VALUE "============".
           03  FILLER            PIC X(1) VALUE " ".
           03  FILLER            PIC X(12) VALUE "============".
           03  FILLER            PIC X(1) VALUE " ".
           03  FILLER            PIC X(12) VALUE "============".
           03  FILLER            PIC X(1) VALUE " ".
           03  FILLER            PIC X(12) VALUE "============".
           03  FILLER            PIC X(1) VALUE " ".
       01  TOTAL-LINE.
           03  T-NAME         PIC X(22).
           03  T-QTY          PIC Z(5)9.
           03  FILLER         PIC X(104) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 321 TO POS
           DISPLAY "** GENERAL LEDGER SUMMARY YTD TRIAL BALANCE **"
           AT POS
           MOVE 421 TO POS
           DISPLAY "**********************************************"
           AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE " " TO WS-RANGE1 WS-RANGE2.
           MOVE 1010 TO POS.
           DISPLAY "         FROM GLMASTER NUMBER: [            ]"
                      AT POS.
           MOVE 1042 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 12        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

           MOVE WS-RANGE1 TO ALPHA-RATE.
           PERFORM NUMBER-CHECK.
           MOVE WS-GLNO-CHECK TO WS-RANGE1.
           MOVE 1042 TO POS.
           DISPLAY WS-RANGE1 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
           MOVE 1210 TO POS.
           DISPLAY "           TO GLMASTER NUMBER: [            ]"
                     AT POS.
           MOVE 1242 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 12        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

           MOVE WS-RANGE2 TO ALPHA-RATE.
           PERFORM NUMBER-CHECK.
           MOVE WS-GLNO-CHECK TO WS-RANGE2.
           MOVE 1242 TO POS.
           DISPLAY WS-RANGE2 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-000.
           IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-015.
           MOVE 1410 TO POS.
           DISPLAY "Print A Period, Leave BLANK for ALL:[  ]" AT POS.
           ADD 37 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 4
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-050
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
       GET-050.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE 2910 TO POS.
           DISPLAY "The Report is being compiled.........." AT POS.
       GET-999.
            EXIT.
      *
       READ-ALL-TRANSACTIONS SECTION.
       RALT-001.
           MOVE 2610 TO POS.
           DISPLAY "Processing Transactions For Account:" AT POS.
           ADD 37 TO POS.
           DISPLAY GL-NUMBER AT POS.
       RALT-005.
           MOVE 0 TO WS-TRANS-TOTAL WS-TRANS-AMT.
           MOVE GL-NUMBER TO GLTRANS-ACCOUNT-NUMBER.
           START GLTRANS-FILE KEY NOT < GLTRANS-ACCOUNT-NUMBER.
           IF WS-GLTRANS-ST1 = 23 OR 35 OR 49
               GO TO RALT-900.
       RALT-010.
           READ GLTRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-GLTRANS-ST1 = 10
               GO TO RALT-900.
           IF WS-GLTRANS-ST1 NOT = 0
               MOVE "GL-TRANS FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLTRANS-ST1
               GO TO RALT-010.
           IF GLTRANS-ACCOUNT-NUMBER NOT = GL-NUMBER
               GO TO RALT-900.
           IF GLTRANS-FUTURE = "F"
               GO TO RALT-010.
           IF WS-ANSWER NOT = 0
            IF GLTRANS-NO > WS-ANSWER
               GO TO RALT-010.
       RALT-020.
           MOVE GLTRANS-REFERENCE     TO TRANS-REF
           MOVE GLTRANS-TRANS         TO TRANS-TRANS
           MOVE GLTRANS-DATE          TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE          TO TRANS-DATE
           MOVE GLTRANS-PERIOD        TO TRANS-PERIOD
           MOVE GLTRANS-LINE-DESC     TO TRANS-DESC.
           IF GLTRANS-AMOUNT < 0
                MOVE 0                TO TRANS-DEBIT
                ADD GLTRANS-AMOUNT    TO WS-TOTAL-CREDIT
                                         WS-HEAD-CR-TOTAL
                                         WS-SUBHEAD-CR-TOTAL
                MOVE GLTRANS-AMOUNT   TO TRANS-CREDIT
           ELSE
                MOVE 0                TO TRANS-CREDIT
                ADD GLTRANS-AMOUNT    TO WS-TOTAL-DEBIT
                                         WS-HEAD-DR-TOTAL
                                         WS-SUBHEAD-DR-TOTAL
                MOVE GLTRANS-AMOUNT   TO TRANS-DEBIT.
      *     WRITE PRINT-REC FROM TRANS-LINE.
      *     MOVE " "                   TO PRINT-REC.
           ADD GLTRANS-AMOUNT         TO WS-TRANS-TOTAL WS-TRANS-AMT.
      *     ADD 1 TO LINE-CNT.
           ADD 1 TO WS-NO-OF-TRANS.
       RALT-200.
            IF LINE-CNT > 58
               PERFORM PRR-010.
           GO TO RALT-010.
       RALT-900.
           MOVE WS-TRANS-AMT     TO D-ACTIVITY
           ADD WS-TRANS-AMT      TO WS-HEAD-ACT-TOTAL
                                    WS-SUBHEAD-ACT-TOTAL
           ADD WS-TRANS-TOTAL    TO GL-OPEN-YEAR-BAL
           MOVE GL-OPEN-YEAR-BAL TO D-CLOSE.
           IF GL-OPEN-YEAR-BAL NOT = GL-BALANCE
               MOVE "*ACCOUNT IN IM-BALANCE*" TO D-DESC
               WRITE PRINT-REC FROM DETAIL-LINE
               MOVE " " TO PRINT-REC DETAIL-LINE
               WRITE PRINT-REC
               ADD 2 TO LINE-CNT
           ELSE
               WRITE PRINT-REC FROM DETAIL-LINE
               MOVE " " TO PRINT-REC DETAIL-LINE
               ADD 1 TO LINE-CNT.
           WRITE PRINT-REC.
           ADD 1 TO LINE-CNT.
           MOVE 2646 TO POS.
           DISPLAY "               " AT POS.
       RALT-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE 0 TO WS-NO-OF-TRANS WS-TOTAL-DEBIT WS-TOTAL-CREDIT.
            MOVE "Y" TO WS-1ST-HEAD-PRINT
                        WS-1ST-SUBHEAD-PRINT.
            MOVE WS-RANGE1 TO GL-NUMBER.
            START GL-MASTER KEY NOT < GL-KEY.
       PRR-002.
            READ GL-MASTER NEXT
               AT END NEXT SENTENCE.
            IF WS-GLMAST-ST1 = 10
               MOVE 0 TO WS-GLMAST-ST1
               PERFORM PTFSH-010
               PERFORM PTFH-010
               GO TO PRR-999.
            IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLMASTER BUSY ON READ-NEXT, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO PRR-002.
            IF GL-NUMBER < WS-RANGE1
               GO TO PRR-002.
            IF GL-NUMBER > WS-RANGE2
               PERFORM PTFSH-010
               PERFORM PTFH-010
               GO TO PRR-999.
            IF LINE-CNT < 58
               GO TO PRR-020.
       PRR-010.
            ADD 1                                TO PAGE-CNT
            MOVE PAGE-CNT                        TO H1-PAGE
            MOVE GLPA-CURRENT-GLPER              TO H1-PERIOD
            MOVE GL-BEGDATE (GLPA-CURRENT-GLPER) TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE                    TO H1-PER-BEG
            MOVE GL-ENDDATE (GLPA-CURRENT-GLPER) TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE                    TO H1-PER-END
            MOVE " " TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE 7 TO LINE-CNT.
       PRR-020.
           MOVE GL-NUMBER TO WS-GLNUMBER.
      *CHECK FOR HEADER ACCOUNT
           IF WS-SUB = "    "
            IF WS-REST = "      "
               PERFORM PRINT-TOTAL-FOR-SUBHEADER
               PERFORM PRINT-TOTAL-FOR-HEADER
               MOVE "H"              TO WS-LAST-ACC-READ
               MOVE GL-NUMBER        TO WS-HEAD-ACCOUNT
               MOVE GL-P-B           TO WS-HEAD-TYPE
               MOVE GL-DESCRIPTION   TO WS-HEAD-DESC
               MOVE GL-OPEN-YEAR-BAL TO WS-HEAD-OPEN-BAL
               MOVE GL-BALANCE       TO WS-HEAD-CLOSE-BAL
               MOVE " "              TO PRINT-REC
               WRITE PRINT-REC
               ADD 1 TO LINE-CNT
               GO TO PRR-050.
      *CHECK FOR SUB-HEADER ACCOUNT
           IF WS-SUB NOT = "    "
            IF WS-REST = "      "
             IF WS-LAST-ACC-READ = "D"
               PERFORM PRINT-TOTAL-FOR-SUBHEADER
               MOVE GL-NUMBER        TO WS-SUBHEAD-ACCOUNT
               MOVE GL-P-B           TO WS-SUBHEAD-TYPE
               MOVE GL-DESCRIPTION   TO WS-SUBHEAD-DESC
               MOVE GL-OPEN-YEAR-BAL TO WS-SUBHEAD-OPEN-BAL
               MOVE GL-BALANCE       TO WS-SUBHEAD-CLOSE-BAL
               GO TO PRR-002
             ELSE
               MOVE "N"              TO WS-1ST-SUBHEAD-PRINT
               MOVE GL-NUMBER        TO WS-SUBHEAD-ACCOUNT
               MOVE GL-P-B           TO WS-SUBHEAD-TYPE
               MOVE GL-DESCRIPTION   TO WS-SUBHEAD-DESC
               MOVE GL-OPEN-YEAR-BAL TO WS-SUBHEAD-OPEN-BAL
               MOVE GL-BALANCE       TO WS-SUBHEAD-CLOSE-BAL
               MOVE 0                TO WS-SUBHEAD-CR-TOTAL
                                        WS-SUBHEAD-ACT-TOTAL
                                        WS-SUBHEAD-DR-TOTAL
               GO TO PRR-002.
       PRR-050.
           IF WS-1ST-SUBHEAD-PRINT = "Y"
            IF WS-1ST-HEAD-PRINT = "Y"
               GO TO PRR-060.
           MOVE " " TO PRINT-REC DETAIL-LINE
           MOVE GL-NUMBER        TO D-ACCOUNT
           MOVE GL-P-B           TO D-TYPE
           MOVE GL-DESCRIPTION   TO D-DESC
           MOVE GL-OPEN-YEAR-BAL TO D-OPEN

           WRITE PRINT-REC FROM DETAIL-LINE
           ADD 1 TO LINE-CNT.
       PRR-060.
           MOVE " " TO PRINT-REC DETAIL-LINE.
           IF WS-SUB = "    "
            IF WS-REST = "      "
               MOVE "************" TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               ADD 2 TO LINE-CNT
               ADD 1 TO WS-HEAD-CNT
               GO TO PRR-002.
           IF WS-REST NOT = "      "
               PERFORM READ-ALL-TRANSACTIONS
               MOVE "D" TO WS-LAST-ACC-READ
               ADD 1 TO WS-DETAIL-CNT.
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       PRINT-TOTAL-FOR-HEADER SECTION.
       PTFH-005.
           IF WS-1ST-HEAD-PRINT = "Y"
               MOVE "N" TO WS-1ST-HEAD-PRINT
               GO TO PTFH-999.
       PTFH-010.
           MOVE " "                    TO PRINT-REC
           WRITE PRINT-REC FROM UNDER-LINE
           MOVE " "                    TO PRINT-REC
           MOVE WS-HEAD-ACCOUNT        TO D-ACCOUNT
           MOVE WS-HEAD-TYPE           TO D-TYPE
           MOVE WS-HEAD-DESC           TO D-DESC
           MOVE WS-HEAD-OPEN-BAL       TO D-OPEN
           MOVE WS-HEAD-DR-TOTAL       TO D-DEBIT
           MOVE WS-HEAD-CR-TOTAL       TO D-CREDIT
           MOVE WS-HEAD-ACT-TOTAL      TO D-ACTIVITY
           MOVE WS-HEAD-CLOSE-BAL      TO D-CLOSE
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " "                    TO PRINT-REC DETAIL-LINE.

           COMPUTE WS-HEAD-OPEN-BAL = 
               WS-HEAD-OPEN-BAL + WS-HEAD-ACT-TOTAL.
           IF WS-HEAD-OPEN-BAL NOT = WS-HEAD-CLOSE-BAL
               MOVE "*HEADER ACCOUNT IN IM-BALANCE*" TO UNDER-DESC.
           WRITE PRINT-REC FROM UNDER-LINE.
           MOVE " "                    TO PRINT-REC UNDER-DESC.
           ADD 3 TO LINE-CNT.
           MOVE 0 TO WS-HEAD-CR-TOTAL
                     WS-HEAD-ACT-TOTAL
                     WS-HEAD-DR-TOTAL.
       PTFH-999.
           EXIT.
      *
       PRINT-TOTAL-FOR-SUBHEADER SECTION.
       PTFSH-005.
           IF WS-1ST-SUBHEAD-PRINT = "Y"
            IF WS-1ST-HEAD-PRINT = "Y"
               GO TO PTFSH-999.
           IF WS-1ST-SUBHEAD-PRINT = "Y"
               MOVE "N" TO WS-1ST-SUBHEAD-PRINT
               GO TO PTFSH-999.
       PTFSH-010.
           MOVE " " TO PRINT-REC.
           IF WS-SUBHEAD-CLOSE-BAL =
                    WS-SUBHEAD-OPEN-BAL + WS-SUBHEAD-ACT-TOTAL
              MOVE WS-SUBHEAD-DESC              TO D-DESC
           ELSE
              MOVE "**SUB-HEAD IN IM-BALANCE**" TO D-DESC.
           MOVE WS-SUBHEAD-ACCOUNT              TO D-ACCOUNT
           MOVE WS-SUBHEAD-TYPE                 TO D-TYPE
           MOVE WS-SUBHEAD-OPEN-BAL             TO D-OPEN
           MOVE WS-SUBHEAD-DR-TOTAL             TO D-DEBIT
           MOVE WS-SUBHEAD-CR-TOTAL             TO D-CREDIT
           MOVE WS-SUBHEAD-ACT-TOTAL            TO D-ACTIVITY
           MOVE WS-SUBHEAD-CLOSE-BAL            TO D-CLOSE
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE
           WRITE PRINT-REC
           ADD 2 TO LINE-CNT
           ADD 1 TO WS-SUB-CNT
           MOVE "S" TO WS-LAST-ACC-READ.
           MOVE 0 TO WS-SUBHEAD-CR-TOTAL
                     WS-SUBHEAD-ACT-TOTAL
                     WS-SUBHEAD-DR-TOTAL.
       PTFSH-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
       RP-010.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "NO PARAMETER RECORD, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RP-010.
       RP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO OPEN-000.
       OPEN-005.
            OPEN I-O GLPARAMETER-FILE.
            IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-005.
            PERFORM READ-PARAMETER.
            MOVE GLPA-NAME TO CO-NAME.
            PERFORM ENTER-PERIOD-DATES.
            PERFORM OPEN-010.
            CLOSE GLPARAMETER-FILE.
        OPEN-008.
           OPEN I-O GLTRANS-FILE.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE "GLTRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO OPEN-008.
        OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF LINE-CNT > 55
                PERFORM PRR-010.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM UNDER-LINE.

           MOVE " "                    TO PRINT-REC
           MOVE " "                    TO D-ACCOUNT
           MOVE "Total General Ledger" TO D-DESC
           MOVE WS-TRANS-TOTAL         TO D-OPEN
           MOVE WS-TOTAL-DEBIT         TO D-DEBIT
           MOVE WS-TOTAL-CREDIT        TO D-CREDIT
           MOVE 0                      TO D-ACTIVITY
           MOVE 0                      TO D-CLOSE
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " "                    TO PRINT-REC DETAIL-LINE.

           COMPUTE WS-TOTAL-CREDIT = WS-TOTAL-CREDIT * -1.
           IF WS-TOTAL-DEBIT NOT = WS-TOTAL-CREDIT
               MOVE "*GENERAL LEDGER IN IM-BALANCE*" TO UNDER-DESC.
           WRITE PRINT-REC FROM UNDER-LINE.
           MOVE " "                    TO PRINT-REC UNDER-DESC.
           ADD 3 TO LINE-CNT.
       END-500.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC

           MOVE "No. of Header Acc's  :" TO T-NAME
           MOVE WS-HEAD-CNT              TO T-QTY
           WRITE PRINT-REC FROM TOTAL-LINE
           MOVE "No. of Sub-Head Acc's:" TO T-NAME
           MOVE WS-SUB-CNT               TO T-QTY
           WRITE PRINT-REC FROM TOTAL-LINE
           MOVE "No. of Detail Acc's  :" TO T-NAME
           MOVE WS-DETAIL-CNT            TO T-QTY
           WRITE PRINT-REC FROM TOTAL-LINE
           MOVE "No. of Tranactions   :" TO T-NAME
           MOVE WS-NO-OF-TRANS           TO T-QTY
           WRITE PRINT-REC FROM TOTAL-LINE.

           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE GL-MASTER.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "NumberCheck".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
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
