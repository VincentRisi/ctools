       IDENTIFICATION DIVISION.
       PROGRAM-ID. GlJrnBat.
       AUTHOR.  CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. B20.
       OBJECT-COMPUTER. B20.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
        Copy "SelectGlMaster".
        Copy "SelectGlMasterLy".
        Copy "SelectGlParameter".
        Copy "SelectGlJrn".
        Copy "SelectGlTrans".
        Copy "SelectGlTransLy".
        Copy "SelectSlDaily".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdGlMast.
           COPY ChlfdGlMastLy.
           COPY ChlfdGlParam.
           COPY ChlfdGlTrans.
           COPY ChlfdGlTransLy.
           COPY ChlfdGlJrn.
           COPY ChlfdDaily.
      *
       WORKING-STORAGE SECTION.
       77  WS-CURRENTPER        PIC 99 VALUE 0.
       77  WS-ACCEPT            PIC XX VALUE " ".
       77  WS-HELD              PIC 9(3) VALUE 0.
       77  WS-HELD-DIS          PIC Z(2)9 VALUE "0".
       77  WS-NORMAL            PIC 9(3) VALUE 0.
       77  WS-NORMAL-DIS        PIC Z(2)9 VALUE "0".
       77  WS-PREVIOUS          PIC 9(3) VALUE 0.
       77  WS-PREVIOUS-DIS      PIC Z(2)9 VALUE "0".
       77  WS-FUTURE            PIC 9(3) VALUE 0.
       77  WS-FUTURE-DIS        PIC Z(2)9 VALUE "0".
       77  WS-LASTYEAR          PIC 9(3) VALUE 0.
       77  WS-LASTYEAR-DIS      PIC Z(2)9 VALUE "0".
       77  WS-RECURRING         PIC 9(3) VALUE 0.
       77  WS-RECURRING-DIS     PIC Z(2)9 VALUE "0".
       77  WS-TOTAL             PIC 9(3) VALUE 0.
       77  WS-TOTAL-DIS         PIC Z(2)9 VALUE "0".
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1       PIC 99.
       01  WS-GL-LY-STATUS.
           03  WS-GL-LY-ST1        PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
       01  WS-GLJRN-STATUS.
           03  WS-GLJRN-ST1        PIC 99.
       01  WS-GLTRANS-STATUS.
           03  WS-GLTRANS-ST1      PIC 99.
       01  WS-GLTRANS-LY-STATUS.
           03  WS-GLTRANS-LY-ST1   PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       01  WS-JRNPERIOD.
           03  WS-1STPER           PIC X.
           03  WS-REST             PIC 99.
       01  WS-JRN-LY-PERIOD        PIC X(3).
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-GLHEADER     PIC X(2).
               05  WS-GLSUBHEADER  PIC X(4).
           03  WS-RESTOFACCOUNT    PIC X(6).
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       01  WS-GLNO-CHECK.
           03  WS-CH          PIC X OCCURS 15.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
           MOVE 320 TO POS.
           DISPLAY "*** BATCH POSTING OF GL JOURNALS ***" AT POS.
       CONTROL-010.
           PERFORM CLEAR-FIELDS.
           PERFORM DISPLAY-TOP-INFO.
           PERFORM GET-DATA.
           PERFORM END-OFF.
      *
       DISPLAY-TOP-INFO SECTION.
       DTI-005.
           MOVE 459 TO POS.
           DISPLAY "Period :" AT POS.
           ADD 8 TO POS.
           MOVE GLPA-CURRENT-GLPER TO WS-CURRENTPER.
           DISPLAY WS-CURRENTPER AT POS.

           MOVE WS-CURRENTPER      TO SUB-1.
           MOVE GL-BEGDATE (SUB-1) TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE 555 TO POS.
           DISPLAY DISPLAY-DATE AT POS.

           MOVE WS-CURRENTPER      TO SUB-1.
           MOVE GL-ENDDATE (SUB-1) TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE 566 TO POS.
           DISPLAY ":" AT POS.
           ADD 2 TO POS.
           DISPLAY DISPLAY-DATE AT POS.
       DTI-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-010.
            MOVE 1210 TO POS.
            DISPLAY
             "Press <RETURN> To Continue, OR <END> To Exit Program."
             AT POS.
            ADD 60 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 70        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

            IF W-ESCAPE-KEY = 0 OR 1
                GO TO GET-350.
            IF W-ESCAPE-KEY = 3
                PERFORM END-500.
            GO TO GET-010.
       GET-350.
            MOVE 1210 TO POS.
            DISPLAY "                                          " AT POS.
            ADD 40 TO POS.
            DISPLAY "                                          " AT POS.
           MOVE " " TO GLJRN-REFERENCE.
           START GLJRN-FILE KEY NOT < GLJRN-KEY
              INVALID KEY NEXT SENTENCE.
       GET-400.
            PERFORM READ-JOURNAL.
            IF WS-GLJRN-ST1 = 10
                GO TO GET-500.
            PERFORM DISPLAY-JOURNALS.
            PERFORM UPDATE-GLMASTER.
            IF WS-JRN-LY-PERIOD = "LYR"
                PERFORM UPDATE-GLMASTER-LY
                PERFORM WRITE-GLTRANS-LY
            ELSE
                PERFORM WRITE-GLTRANS.
            PERFORM REWRITE-JOURNAL.
            GO TO GET-400.
       GET-500.
            PERFORM UPDATE-PARAMETER.
            MOVE 3010 TO POS.
            DISPLAY
             "Batch Journal Posting COMPLETE, Press <RETURN> To Exit."
             AT POS.
            ADD 60 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 25        TO CDA-ROW.
           MOVE 70        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

            IF W-ESCAPE-KEY = 0 OR 1
                GO TO GET-999.
            GO TO GET-500.
       GET-999.
            EXIT.
      *
       READ-JOURNAL SECTION.
       RSTT-000.
           MOVE 1 TO SUB-1.
       RSTT-010.
           READ GLJRN-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-GLJRN-ST1 = 10
              GO TO RSTT-999.
           IF WS-GLJRN-ST1 NOT = 0
              MOVE "GLJRN FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLJRN-ST1
              GO TO RSTT-010.
           IF GLJRN-COMPLETE = "Y"
              GO TO RSTT-010.
           IF GLJRN-COMPLETE = "P"
              ADD 1 TO WS-HELD
              MOVE WS-HELD TO WS-HELD-DIS
              MOVE 1320 TO POS
              DISPLAY
            "Total No. Of Suspended Journals        :" AT POS
              ADD 41 TO POS
              DISPLAY WS-HELD-DIS AT POS
              GO TO RSTT-010.
           IF GLPA-RECJRN-POST = "Y"
            IF GLJRN-ACTION = "R"
              GO TO RSTT-010.
           IF GLJRN-PERIOD = "LYR"
              MOVE GLJRN-PERIOD TO WS-JRN-LY-PERIOD
           ELSE
              MOVE "   "        TO WS-JRN-LY-PERIOD
              MOVE GLJRN-PERIOD TO WS-JRNPERIOD.
       RSTT-999.
           EXIT.
      *
       DISPLAY-JOURNALS SECTION.
       DJ-005.
           MOVE 2220 TO POS.
           DISPLAY
            "Journal Currently Being Posted         :" AT POS.
           ADD 41 TO POS.
           DISPLAY GLJRN-REFERENCE AT POS.
       DJ-010.
           IF GLJRN-ACTION = " "
            IF GLJRN-PERIOD NOT = "LYR"
             IF WS-1STPER = " " 
                ADD 1 TO WS-NORMAL
                MOVE WS-NORMAL TO WS-NORMAL-DIS
                MOVE 1420 TO POS
                DISPLAY
            "Total No. Of Normal Journals Posted    :" AT POS
                ADD 41 TO POS
                DISPLAY WS-NORMAL-DIS AT POS.
           IF GLJRN-ACTION = " "
            IF GLJRN-PERIOD NOT = "LYR"
             IF WS-1STPER = "P" 
                ADD 1 TO WS-PREVIOUS
                MOVE WS-PREVIOUS TO WS-PREVIOUS-DIS
                MOVE 1520 TO POS
                DISPLAY
            "Total No. Of Previous Period JRN Posted:" AT POS
                ADD 41 TO POS
                DISPLAY WS-PREVIOUS-DIS AT POS.
           IF GLJRN-ACTION = " "
            IF GLJRN-PERIOD NOT = "LYR"
             IF WS-1STPER = "F" 
                ADD 1 TO WS-FUTURE
                MOVE WS-FUTURE TO WS-FUTURE-DIS
                MOVE 1620 TO POS
                DISPLAY
            "Total No. Of FUTURE Period JRN Posted  :" AT POS
                ADD 41 TO POS
                DISPLAY WS-FUTURE-DIS AT POS.
           IF GLJRN-ACTION = "R"
                ADD 1 TO WS-RECURRING
                MOVE WS-RECURRING TO WS-RECURRING-DIS
                MOVE 1720 TO POS
                DISPLAY
            "Total No. Of Recurring Journals Posted :" AT POS
                ADD 41 TO POS
                DISPLAY WS-RECURRING-DIS AT POS.
           IF GLJRN-ACTION = " "
            IF GLJRN-PERIOD = "LYR"
                ADD 1 TO WS-LASTYEAR
                MOVE WS-LASTYEAR TO WS-LASTYEAR-DIS
                MOVE 1820 TO POS
                DISPLAY
            "Total No. Of Last Year Journals Posted :" AT POS
                ADD 41 TO POS
                DISPLAY WS-LASTYEAR-DIS AT POS.

           ADD 1 TO WS-TOTAL
           MOVE WS-TOTAL TO WS-TOTAL-DIS
           MOVE 2020 TO POS
           DISPLAY
            "Total No. Of Journals Posted           :" AT POS
           ADD 41 TO POS
           DISPLAY WS-TOTAL-DIS AT POS.
       DJ-999.
           EXIT.
      *
       UPDATE-GLMASTER SECTION.
       UPGL-000.
            IF WS-1STPER = "F"
              GO TO UPGL-999.
           IF WS-JRN-LY-PERIOD = "LYR"
              MOVE 1 TO WS-REST SUB-3
              MOVE 1 TO SUB-1
              GO TO UPGL-005.
           IF WS-REST NOT = 0 AND NOT = " "
              MOVE WS-REST TO SUB-3
           ELSE
              MOVE WS-CURRENTPER TO WS-REST SUB-3.
           MOVE 2620 TO POS.
           DISPLAY "UPDATING GL-MASTER FILES..." AT POS.
           MOVE 1 TO SUB-1.
       UPGL-005.
           IF GLJRN-GLNUMBER (SUB-1) = "   "
                 GO TO UPGL-960.
           MOVE GLJRN-GLNUMBER (SUB-1) TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGL-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLMASTER FILE DOES NOT EXIST, CALL THE SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE GL-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO UPGL-950.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GL-MASTER RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGL-010.
 
           MOVE 2648 TO POS
           DISPLAY GL-NUMBER AT POS.
              
           IF WS-JRN-LY-PERIOD NOT = "LYR"
               ADD GLJRN-AMOUNT (SUB-1) TO GL-BALANCE
               ADD GLJRN-AMOUNT (SUB-1) TO GL-PER (SUB-3).
           IF WS-1STPER = "P"
              ADD GLJRN-AMOUNT (SUB-1) TO GL-OPEN-PER-BAL.
               
           IF WS-JRN-LY-PERIOD = "LYR"
            IF GL-P-B = "B"
               ADD GLJRN-AMOUNT (SUB-1) TO GL-BALANCE
                                           GL-OPEN-PER-BAL
                                           GL-OPEN-YEAR-BAL
               ADD GLJRN-AMOUNT (SUB-1) TO GL-LAST-PER (12)
            ELSE
               ADD GLJRN-AMOUNT (SUB-1) TO GL-LAST-PER (12).
       UPGL-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGL-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GL-MASTER RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGL-900.
       UPGL-950.
           PERFORM UPDATE-GLHEADER.
           PERFORM UPDATE-GLSUBHEADER.
           ADD 1 TO SUB-1.
           IF SUB-1 NOT > 50
               GO TO UPGL-005.
           MOVE 1 TO SUB-1.
       UPGL-960.
           MOVE 2620 TO POS.
           DISPLAY "                                     " AT POS.
       UPGL-999.
           EXIT.
     *
       UPDATE-GLHEADER SECTION.
       UPGLH-005.
           MOVE GLJRN-GLNUMBER (SUB-1) TO WS-GLNUMBER.
           MOVE WS-GLHEADER TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLHEADER FILE DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGLH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLHEADER FILE BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGLH-010.
           IF WS-JRN-LY-PERIOD NOT = "LYR"
               ADD GLJRN-AMOUNT (SUB-1) TO GL-BALANCE
               ADD GLJRN-AMOUNT (SUB-1) TO GL-PER (SUB-3).
           IF WS-1STPER = "P"
              ADD GLJRN-AMOUNT (SUB-1) TO GL-OPEN-PER-BAL.
            IF WS-JRN-LY-PERIOD = "LYR"
             IF GL-P-B = "B"
               ADD GLJRN-AMOUNT (SUB-1) TO GL-BALANCE
                                           GL-OPEN-PER-BAL
                                           GL-OPEN-YEAR-BAL
               ADD GLJRN-AMOUNT (SUB-1) TO GL-LAST-PER (12)
            ELSE
               ADD GLJRN-AMOUNT (SUB-1) TO GL-LAST-PER (12).
       UPGLH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLHEADER RECORD BUSY ON WRITE, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLH-900.
       UPGLH-999.
           EXIT.
     *
       UPDATE-GLSUBHEADER SECTION.
       UPGLSH-005.
           MOVE GLJRN-GLNUMBER (SUB-1) TO WS-GLNUMBER.
           MOVE WS-HEAD-SUB TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLSH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLSUBHEAD FILE DOES'NT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGLSH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLSUBHEADER FILE BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGLSH-010.
            IF WS-JRN-LY-PERIOD NOT = "LYR"
               ADD GLJRN-AMOUNT (SUB-1) TO GL-BALANCE
               ADD GLJRN-AMOUNT (SUB-1) TO GL-PER (SUB-3).
           IF WS-1STPER = "P"
              ADD GLJRN-AMOUNT (SUB-1) TO GL-OPEN-PER-BAL.
            IF WS-JRN-LY-PERIOD = "LYR"
             IF GL-P-B = "B"
               ADD GLJRN-AMOUNT (SUB-1) TO GL-BALANCE
                                           GL-OPEN-PER-BAL
                                           GL-OPEN-YEAR-BAL
               ADD GLJRN-AMOUNT (SUB-1) TO GL-LAST-PER (12)
            ELSE
               ADD GLJRN-AMOUNT (SUB-1) TO GL-LAST-PER (12).
       UPGLSH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLSH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLSUBHEAD FILE BUSY ON WRITE, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGLSH-900.
       UPGLSH-999.
           EXIT.
      *
       UPDATE-GLMASTER-LY SECTION.
       UPGL-LY000.
           MOVE 12 TO WS-REST SUB-3.
           MOVE 1  TO SUB-1.
       UPGL-LY005.
           IF GLJRN-GLNUMBER (SUB-1) = "   "
                 GO TO UPGL-LY999.
           MOVE GLJRN-GLNUMBER  (SUB-1) TO GL-LY-NUMBER.
           START GL-LY-MASTER KEY NOT < GL-LY-KEY.
       UPGL-LY010.
           READ GL-LY-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GL-LY-ST1 = 23 OR 35 OR 49
               MOVE "GLMASTER-LY DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GL-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GL-LY-ST1
               MOVE GL-LY-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGL-LY950.
           IF WS-GL-LY-ST1 NOT = 0
               MOVE "GLMASTER-LY BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GL-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GL-LY-ST1
               GO TO UPGL-LY010.
           ADD GLJRN-AMOUNT (SUB-1) TO GL-LY-BALANCE.
           ADD GLJRN-AMOUNT (SUB-1) TO GL-LY-PER (SUB-3).
       UPGL-LY900.
           REWRITE GL-LY-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GL-LY-ST1 = 23 OR 35 OR 49
               GO TO UPGL-LY999.
           IF WS-GL-LY-ST1 NOT = 0
               MOVE "GLMASTER-LY BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GL-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GL-LY-ST1
               GO TO UPGL-LY900.
       UPGL-LY950.
           PERFORM UPDATE-GLHEADER-LY.
           PERFORM UPDATE-GLSUBHEADER-LY.
           ADD 1 TO SUB-1.
           IF SUB-1 NOT > 50
               GO TO UPGL-LY005.
           MOVE 1 TO SUB-1.
       UPGL-LY999.
           EXIT.
     *
       UPDATE-GLHEADER-LY SECTION.
       UPGLH-LY005.
           MOVE GLJRN-GLNUMBER (SUB-1) TO WS-GLNUMBER.
           MOVE WS-GLHEADER TO GL-LY-NUMBER.
           START GL-LY-MASTER KEY NOT < GL-LY-KEY.
       UPGLH-LY010.
           READ GL-LY-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GL-LY-ST1 = 23 OR 35 OR 49
               MOVE "GLHEADER-LY DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GL-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GL-LY-ST1
               GO TO UPGLH-LY999.
           IF WS-GL-LY-ST1 NOT = 0
               MOVE "GLHEADER-LY BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GL-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GL-LY-ST1
               GO TO UPGLH-LY010.
           ADD GLJRN-AMOUNT (SUB-1) TO GL-LY-BALANCE.
           ADD GLJRN-AMOUNT (SUB-1) TO GL-LY-PER (SUB-3).
       UPGLH-LY900.
           REWRITE GL-LY-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GL-LY-ST1 = 23 OR 35 OR 49
               GO TO UPGLH-LY999.
           IF WS-GL-LY-ST1 NOT = 0
               MOVE "GLHEADER-LY BUSY ON WRITE, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GL-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GL-LY-ST1
               GO TO UPGLH-LY900.
       UPGLH-LY999.
           EXIT.
     *
       UPDATE-GLSUBHEADER-LY SECTION.
       UPGLSH-LY005.
           MOVE GLJRN-GLNUMBER (SUB-1) TO WS-GLNUMBER.
           MOVE WS-HEAD-SUB TO GL-LY-NUMBER.
           START GL-LY-MASTER KEY NOT < GL-LY-KEY.
       UPGLSH-LY010.
           READ GL-LY-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GL-LY-ST1 = 23 OR 35 OR 49
               MOVE "GLSUBHEAD-LY DOESN'T EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GL-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GL-LY-ST1
               GO TO UPGLSH-LY999.
           IF WS-GL-LY-ST1 NOT = 0
               MOVE "GLSUBHEADER-LY BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GL-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GL-LY-ST1
               GO TO UPGLSH-LY010.
           ADD GLJRN-AMOUNT (SUB-1) TO GL-LY-BALANCE.
           ADD GLJRN-AMOUNT (SUB-1) TO GL-LY-PER (SUB-3).
       UPGLSH-LY900.
           REWRITE GL-LY-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GL-LY-ST1 = 23 OR 35 OR 49
               GO TO UPGLSH-LY999.
           IF WS-GL-LY-ST1 NOT = 0
               MOVE "GLSUBHEAD FILE BUSY ON WRITE, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GL-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GL-LY-ST1
               GO TO UPGLSH-LY900.
       UPGLSH-LY999.
           EXIT.
      *
       REWRITE-JOURNAL SECTION.
       RWST-000.
            MOVE 1 TO SUB-1.
            MOVE "Y" TO GLJRN-COMPLETE.
       RWST-018.
           REWRITE GLJRN-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-GLJRN-ST1 NOT = 0
              MOVE "GLJRN-TRANS REWRITE ERROR - RWST-018" TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLJRN-ST1
              GO TO RWST-018.
       RWST-999.
            EXIT.
      *
       WRITE-GLTRANS SECTION.
       WRTR-000.
            MOVE 2610 TO POS
            MOVE SPACES TO WS-MESSAGE
            DISPLAY WS-MESSAGE AT POS.
            
            MOVE 2620 TO POS.
            DISPLAY "WRITING GL-TRANS FILES..." AT POS.
            MOVE 1 TO SUB-1.
       WRTR-010.
            IF GLJRN-GLNUMBER (SUB-1) = "   "
                 GO TO WRTR-900.
            PERFORM READ-PARAMETER-LOCK.
            MOVE GLPA-GLTRANSNO TO GLTRANS-TRANS.
            ADD 1 TO GLPA-GLTRANSNO.
            PERFORM REWRITE-PARAMETER.
            MOVE GLJRN-REFERENCE           TO GLTRANS-REFERENCE.
            MOVE 1                         TO GLTRANS-TYPE.
            IF WS-1STPER = "P"
                MOVE " "                   TO GLTRANS-FUTURE
                MOVE WS-REST               TO GLTRANS-NO
            ELSE
                MOVE WS-JRNPERIOD          TO GLTRANS-PERIOD.
            MOVE GLJRN-DATE                TO GLTRANS-DATE.
            MOVE GLJRN-GLNUMBER (SUB-1)    TO GLTRANS-ACCOUNT-NUMBER.
            MOVE GLJRN-AMOUNT (SUB-1)      TO GLTRANS-AMOUNT.
            MOVE GLJRN-LINE-DESC (SUB-1)   TO GLTRANS-LINE-DESC.
       WRTR-015.
            WRITE GLTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-GLTRANS-ST1 = 23 OR 35 OR 49
              MOVE "GLTRANS BUSY ON WRITE23, GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO WRTR-015.
            IF WS-GLTRANS-ST1 NOT = 0
              MOVE "GLTRANS FILE BUSY ON WRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO WRTR-015.
       WRTR-020.
            ADD 1 TO SUB-1.
            IF SUB-1 < 51
                GO TO WRTR-010.
       WRTR-900.
            MOVE 2620 TO POS.
            DISPLAY "                                          " AT POS.
       WRTR-999.
            EXIT.
      *
       WRITE-GLTRANS-LY SECTION.
       WRTR-LY-000.
           OPEN I-O GLTRANS-LY-FILE.
           IF WS-GLTRANS-LY-ST1 NOT = 0 
              MOVE "GLTRANS-LY BUSY ON OPEN, PRESS 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
              GO TO WRTR-LY-000.
           MOVE 1 TO SUB-1.
       WRTR-LY-010.
            IF GLJRN-GLNUMBER (SUB-1) = "   "
                 GO TO WRTR-LY-900.
            PERFORM READ-PARAMETER-LOCK.
            MOVE GLPA-GLTRANSNO          TO GLTRANS-LY-TRANS.
            ADD 1                        TO GLPA-GLTRANSNO.
            PERFORM REWRITE-PARAMETER.
            MOVE GLJRN-REFERENCE         TO GLTRANS-LY-REFERENCE
            MOVE 1                       TO GLTRANS-LY-TYPE
            MOVE WS-REST                 TO GLTRANS-LY-NO
            MOVE GLJRN-DATE              TO GLTRANS-LY-DATE
            MOVE GLJRN-GLNUMBER (SUB-1)  TO GLTRANS-LY-ACCNO
            MOVE GLJRN-AMOUNT (SUB-1)    TO GLTRANS-LY-AMOUNT
            MOVE GLJRN-LINE-DESC (SUB-1) TO GLTRANS-LY-LINE-DESC.
       WRTR-LY-015.
            WRITE GLTRANS-LY-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-GLTRANS-LY-ST1 = 23 OR 35 OR 49
              MOVE "GLTRANS-LY BUSY23, 'ESC' TO RETRY" TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
              GO TO WRTR-LY-015.
            IF WS-GLTRANS-LY-ST1 NOT = 0
              MOVE "GLTRANS-LY BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
              GO TO WRTR-LY-015.
       WRTR-LY-020.
            ADD 1 TO SUB-1.
            IF SUB-1 < 51
                GO TO WRTR-LY-010.
            MOVE 1 TO SUB-1.
       WRTR-LY-900.
            CLOSE GLTRANS-LY-FILE.
       WRTR-LY-999.
            EXIT.
      *
       UPDATE-PARAMETER SECTION.
       UPP-010.
           PERFORM READ-PARAMETER-LOCK.
           MOVE "Y" TO GLPA-RECJRN-POST.
           PERFORM REWRITE-PARAMETER.
       UPP-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "NO GLPARAMETER RECORD, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       READ-PARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE WITH LOCK          
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "NO GLPARAMETER RECORD, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSYON READ-LOCK, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RPL-000.
       RPL-999.
           EXIT.
      *
       REWRITE-PARAMETER SECTION.
       REWP-000.
           REWRITE GLPARAMETER-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
             MOVE "NO GLPARAMETER RECORD REWRITE, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON REWRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO REWP-000.
       REWP-999.
           EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
             MOVE 1 TO SUB-1.
       CF-010.
             MOVE " " TO GLJRN-GLNUMBER (SUB-1)
                         GLJRN-LINE-DESC (SUB-1).
             MOVE 0   TO GLJRN-AMOUNT (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 51
                 GO TO CF-010.
             MOVE 1 TO SUB-1.
       CF-020.
             MOVE " " TO GLJRN-REFERENCE
                         GLJRN-MAIN-DESC
                         GLJRN-PERIOD
                         GLJRN-ACTION
                         GLJRN-COMPLETE.
             MOVE 0 TO   GLJRN-DATE.
       CF-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-010.
           OPEN I-O GL-LY-MASTER.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER-LY FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GL-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GL-LY-ST1
              GO TO OPEN-010.
       OPEN-011.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0 
              MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO OPEN-011.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-012.

           PERFORM READ-PARAMETER.
           PERFORM ENTER-PERIOD-DATES.
       OPEN-015.
           OPEN I-O GLJRN-FILE.
           IF WS-GLJRN-ST1 NOT = 0 
              MOVE "GLJRN FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLJRN-ST1
              GO TO OPEN-015.
       OPEN-016.
           OPEN I-O GLTRANS-FILE.
           IF WS-GLTRANS-ST1 NOT = 0 
              MOVE "GL-TRANS BUSY ON OPEN, PRESS 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO OPEN-016.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE GL-MASTER
                 GLJRN-FILE
                 GLTRANS-FILE
                 GLPARAMETER-FILE.
       END-500.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "GetSystemY2KDate".
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
       Copy "CTOSCobolAccept".
      * END-OF-JOB
