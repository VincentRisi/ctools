        IDENTIFICATION DIVISION.
        PROGRAM-ID. CbPerEnd.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectCbMaster".
        Copy "SelectCbTrans".
        Copy "SelectCbMasterLy".
        Copy "SelectCbTransLy".
        Copy "SelectGlParameter".
        Copy "SelectGlMaster".
        Copy "SelectSlDaily".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCbMast.
           COPY ChlfdCbTrans.
           COPY ChlfdCbMastLy.
           COPY ChlfdCbTransLy.
           COPY ChlfdGlParam.
           COPY ChlfdGlMast.
           COPY ChlfdDaily.
      *
       WORKING-STORAGE SECTION.
       77  Ws-CbMasterLyInd     PIC X(60) VALUE " ".
       77  Ws-CbTransLyInd      PIC X(60) VALUE " ".
       77  WS-STOP-UPDATE       PIC X VALUE " ".
       77  WS-RECORD-TO-DELETE  PIC 9.
       77  WS-POSTED            PIC 9(5) VALUE 0.
       77  WS-POSTED-DIS        PIC Z(4)9.
       77  WS-CBTRANS-NO        PIC 9(2) VALUE 0.
       77  WS-ANSWER1           PIC X VALUE " ".
       77  WS-ANSWER2           PIC X VALUE " ".
       77  WS-ANSWER3           PIC X VALUE " ".
       77  WS-ANSWER4           PIC X VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-CALC-MM           PIC 9(2) VALUE 0.
       77  WS-WORK-MM           PIC 9(4) VALUE 0.
       77  WS-WORK-PO-MM        PIC 9(4) VALUE 0.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST     PIC X(20) VALUE " ".
           03  WS-DAILY-2ND     PIC X(20) VALUE " ".
           03  WS-DAILY-3RD     PIC X(20) VALUE " ".
           03  WS-DAILY-4TH     PIC X(20) VALUE " ".
       01  SPLIT-TIME.
           03  SPLIT-HR         PIC 99.
           03  SPLIT-FIL1       PIC X.
           03  SPLIT-MN         PIC 99.
           03  SPLIT-FIL2       PIC X.
           03  SPLIT-SC         PIC 99.
       01  WS-CBTRANS-STATUS.
           03  WS-CBTRANS-ST1     PIC 99.
       01  WS-CBTRANS-LY-STATUS.
           03  WS-CBTRANS-LY-ST1  PIC 99.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1      PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  WS-CB-STATUS.
           03  WS-CB-ST1          PIC 99.
       01  WS-CB-LY-STATUS.
           03  WS-CB-LY-ST1       PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1       PIC 99.
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
       CONTROL-001.
           PERFORM OPEN-001.
           PERFORM READ-PARAMETER
           PERFORM ENTER-PERIOD-DATES
           PERFORM CLEAR-SCREEN
           PERFORM DISPLAY-CB-TOP-INFO
           CLOSE GLPARAMETER-FILE.
           MOVE 0310 TO POS
           DISPLAY "* CASHBOOK MONTH / YEAR END PROCESSING *"
               AT POS
           MOVE 0410 TO POS
           DISPLAY "****************************************"
               AT POS.
       CONTROL-005.
           MOVE 0610 TO POS.
           DISPLAY "Is This A MONTH Or YEAR End ?????" AT POS.
           MOVE 0710 TO POS.
           DISPLAY "Enter (M = Month, Y = Year.)     :[ ]" AT POS.
           MOVE 0745 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 4         TO CDA-ROW.
           MOVE 44        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

           IF W-ESCAPE-KEY = 3
               PERFORM CONTROL-900.
           IF WS-ANSWER1 NOT = "M" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-005.
       CONTROL-010.
           IF WS-ANSWER1 = "M" OR = "Y"
               MOVE 810 TO POS
           DISPLAY "Have You Run Your Month / Year End Reports?:[ ]"
               AT POS
               MOVE 855 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER2.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF WS-ANSWER2 = "Y"
               GO TO CONTROL-012
           ELSE
               GO TO CONTROL-900.
       CONTROL-012.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-015.
           IF WS-ANSWER1 = "M"
               MOVE 910 TO POS
           DISPLAY "You Have Selected To Do MONTH END Processing."
               AT POS.
           IF WS-ANSWER1 = "Y"
               MOVE 910 TO POS
           DISPLAY "You Have Selected To Do YEAR END Processing."
               AT POS.
       CONTROL-040.
           PERFORM GET-SYSTEM-Y2K-DATE.
       CONTROL-055.
           MOVE 3010 TO POS.
           DISPLAY "                                             "
                 AT POS.
           MOVE 1210 TO POS.
           IF WS-ANSWER1 = "Y"
               DISPLAY "There Are 6 Data Files To Process." AT POS
           ELSE
               DISPLAY "There Are 4 Data Files To Process." AT POS.
           PERFORM CHECK-GLPERIODS.
           PERFORM CHECK-ALL-TRANS-POSTED.
           IF WS-ANSWER1 = "Y"
               PERFORM WRITE-CBMASTER-TO-LAST-YEAR
               PERFORM WRITE-TRANS-TO-LAST-YEAR.
           PERFORM UPDATE-CBMASTER.
           PERFORM GLPARAMETERS.
           PERFORM CHECK-FUTURE-TRANS. 
           ACCEPT WS-TIME FROM TIME.
           MOVE WS-HR TO SPLIT-HR
           MOVE ":"   TO SPLIT-FIL1
           MOVE WS-MIN TO SPLIT-MN
           MOVE ":"   TO SPLIT-FIL2
           MOVE WS-SEC TO SPLIT-SC
           MOVE "All Files Processed." TO WS-DAILY-1ST
           MOVE SPLIT-TIME             TO WS-DAILY-3RD
           PERFORM WRITE-DAILY.
       CONTROL-600.
           MOVE 2510 TO POS.
           DISPLAY "We Are Finished, Run The Maintainance Submit File,"
              AT POS.
           MOVE 2610 TO POS.
           DISPLAY "Press 'NEXT' Or 'GO' To End The Program. " AT POS.
           MOVE 2675 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 23        TO CDA-ROW.
           MOVE 75        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER3.

           IF W-ESCAPE-KEY = 1 OR 2
               GO TO CONTROL-900
           ELSE
               MOVE 3010 TO POS
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-600.
       CONTROL-900.
           EXIT PROGRAM.
      *
       CHECK-GLPERIODS SECTION.
       CH-GLPA-005.
            MOVE "Checking GlPeriods..............." TO WS-MESSAGE
            PERFORM ERROR-000.
            PERFORM OPEN-001.
            MOVE 1 TO GLPA-RECORD.
            READ GLPARAMETER-FILE WITH LOCK
                INVALID KEY
                MOVE "GLPARAMETER RECORD NOT FOUND!!!" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO CH-GLPA-999.
            PERFORM ERROR-020.
       CH-GLPA-010.
            IF GLPA-CURRENT-CBPER = 12
              IF WS-ANSWER1 = "M"
                MOVE 1510 TO POS
                DISPLAY " " AT 3079 WITH BELL
                DISPLAY "You should be running the YEAR END" AT POS
                MOVE 1610 TO POS
                DISPLAY "Press 'NEXT' Or 'GO' To End The Program."
                AT POS
                MOVE 3010 TO POS

                MOVE ' '       TO CDA-DATA
                MOVE 1         TO CDA-DATALEN
                MOVE 13        TO CDA-ROW
                MOVE 75        TO CDA-COL
                MOVE CDA-WHITE TO CDA-COLOR
                MOVE 'F'       TO CDA-ATTR
                PERFORM CTOS-ACCEPT
                MOVE CDA-DATA TO WS-ANSWER3
           IF W-ESCAPE-KEY = 1 OR 2
                CLOSE GLPARAMETER-FILE
                EXIT PROGRAM
           ELSE
               MOVE 3010 TO POS
               DISPLAY " " AT 3079 WITH BELL
               GO TO CH-GLPA-010.
           IF WS-ANSWER1 = "Y"
            IF GLPA-CURRENT-CBPER = 12
             IF GLPA-CURRENT-CRPER = 12
              OR GLPA-CURRENT-GLPER = 12
                MOVE 1510 TO POS
                DISPLAY " " AT 3079 WITH BELL
                DISPLAY
              "Creditors/General Ledger should have YEAR END run FIRST."
                 AT POS
                MOVE 1620 TO POS
                DISPLAY "Press 'NEXT' Or 'GO' To End The Program."
                AT POS
                MOVE 1670 TO POS

                MOVE ' '       TO CDA-DATA
                MOVE 1         TO CDA-DATALEN
                MOVE 13        TO CDA-ROW
                MOVE 75        TO CDA-COL
                MOVE CDA-WHITE TO CDA-COLOR
                MOVE 'F'       TO CDA-ATTR
                PERFORM CTOS-ACCEPT
                MOVE CDA-DATA TO WS-ANSWER3
            IF W-ESCAPE-KEY = 1 OR 2
                CLOSE GLPARAMETER-FILE
                EXIT PROGRAM
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CH-GLPA-010.
           IF WS-ANSWER1 = "N"
            IF GLPA-CURRENT-CBPER = GLPA-CURRENT-CRPER
             OR GLPA-CURRENT-CBPER = GLPA-CURRENT-GLPER
               MOVE 1510 TO POS
                DISPLAY " " AT 3079 WITH BELL
                DISPLAY
             "Creditors/General Ledger should have MONTH END run FIRST."
                AT POS
                MOVE 1620 TO POS
                DISPLAY "Press 'NEXT' Or 'GO' To End The Program."
                AT POS
                MOVE 1670 TO POS

                MOVE ' '       TO CDA-DATA
                MOVE 1         TO CDA-DATALEN
                MOVE 13        TO CDA-ROW
                MOVE 75        TO CDA-COL
                MOVE CDA-WHITE TO CDA-COLOR
                MOVE 'F'       TO CDA-ATTR
                PERFORM CTOS-ACCEPT
                MOVE CDA-DATA TO WS-ANSWER3
            IF W-ESCAPE-KEY = 1 OR 2
                CLOSE GLPARAMETER-FILE
                EXIT PROGRAM
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CH-GLPA-010.
        CH-GLPA-020.
            MOVE GLPA-CURRENT-CBPER TO WS-CBTRANS-NO.
            CLOSE GLPARAMETER-FILE.
        CH-GLPA-999.
            EXIT.
      *
       CHECK-ALL-TRANS-POSTED SECTION.
       CH-TR-PS-000.
            ACCEPT WS-TIME FROM TIME
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC
            MOVE "CashBook Period End:" TO WS-DAILY-1ST
            MOVE GLPA-CURRENT-CBPER     TO WS-DAILY-2ND
            MOVE "Start of process    " TO WS-DAILY-3RD
            MOVE SPLIT-TIME             TO WS-DAILY-4TH
            PERFORM WRITE-DAILY.
            MOVE " 1. CB-Trans Being  " TO WS-DAILY-1ST
            MOVE "checked for posting." TO WS-DAILY-2ND
            MOVE SPLIT-TIME             TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
       CH-TR-PS-005.
           PERFORM OPEN-005.
           MOVE GLPA-GLDRBANK            TO CBTRANS-CBMASTER.
           MOVE GLPA-PER (WS-CBTRANS-NO) TO CBTRANS-DATE.
           START CBTRANS-FILE KEY NOT < CBTRANS-ALT-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE 
            "WE HAVE A PROBLEM IN THE START OF CBTRANS, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO CH-TR-PS-900.
           MOVE 1410 TO POS.
           DISPLAY " 1. Checking all transactions are posted..." AT POS.
       CH-TR-PS-010.
           READ CBTRANS-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-CBTRANS-ST1 = 10
               GO TO CH-TR-PS-900.

           MOVE 2910 TO POS
           DISPLAY "CBTrans Being Processed:" AT POS
           ADD 26 TO POS
           DISPLAY CBTRANS-REFERENCE AT POS
           ADD 15 TO POS
           DISPLAY CBTRANS-TRANS AT POS
           ADD 10 TO POS
           DISPLAY CBTRANS-TYPE AT POS.

           IF CBTRANS-DATE < GL-BEGDATE (WS-CBTRANS-NO)
               GO TO CH-TR-PS-010.
           IF CBTRANS-DATE > GL-ENDDATE (WS-CBTRANS-NO)
               GO TO CH-TR-PS-010.
           IF CBTRANS-TYPE-OF-POST = "C" OR = "D" OR "G" OR = "S"
               GO TO CH-TR-PS-010.
           IF CBTRANS-ALLOCATED = "Y" OR = "H"
               GO TO CH-TR-PS-010.
               
           MOVE
            "SOME TRANSACTION HAVE NOT BEEN POSTED, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE CBTRANS-KEY TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              CLOSE CBTRANS-FILE
              EXIT PROGRAM.
       CH-TR-PS-900.
           CLOSE CBTRANS-FILE.
           MOVE 1410 TO POS
           DISPLAY " 1. Transactions all Posted.               " AT POS
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           ADD 20 TO POS
           DISPLAY WS-MESSAGE AT POS.
       CH-TR-PS-999.
            EXIT.
      *
       WRITE-CBMASTER-TO-LAST-YEAR SECTION.
       W-CBM-LY-000.
            ACCEPT WS-TIME FROM TIME.
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC
            MOVE " 2. CBMast-WriteLY  " TO WS-DAILY-1ST
            MOVE "Start of process    " TO WS-DAILY-2ND
            MOVE SPLIT-TIME             TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
       W-CBM-LY-005.
           MOVE 1 To WS-RECORD-TO-DELETE
      *     PERFORM DELETE-TRANS.
           PERFORM OPEN-010.
           PERFORM OPEN-011.
           MOVE " " TO CB-NUMBER.
           START CB-MASTER KEY NOT < CB-NUMBER
                INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 NOT = 0
              MOVE 
           "WE HAVE A PROBLEM IN THE START OF CBMASTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CB-ST1
              GO TO W-CBM-LY-900.
           MOVE 1510 TO POS
           DISPLAY " 2. CBMast Last/Year File Being Processed." AT POS.
       W-CBM-LY-010.
           READ CB-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-CB-ST1 = 10
               GO TO W-CBM-LY-900.
           MOVE 2910 TO POS
           DISPLAY "CBMast Being Processed:" AT POS
           ADD 26 TO POS
           DISPLAY CB-NUMBER AT POS.
       W-CBM-LY-200.
           MOVE CB-NUMBER           TO CB-LY-NUMBER
           MOVE CB-DESCRIPTION      TO CB-LY-DESCRIPTION
           MOVE CB-BALANCE          TO CB-LY-BALANCE
           MOVE CB-OPEN-YEAR-BAL    TO CB-LY-OPEN-YEAR-BAL
           MOVE CB-LY-OPEN-BAL      TO CB-LY-LAST-YEAR-BAL
           MOVE CB-CURRENT-PERIODS  TO CB-LY-CURRENT-PERIODS
           MOVE CB-LY-PERIODS       TO CB-LY-LAST-PERIODS.
       W-CBM-LY-800.
           WRITE CB-LY-RECORD.
           IF WS-CB-ST1 NOT = 0
                MOVE WS-CB-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                MOVE "CBMASTER NOT WRITTEN"  TO WS-DAILY-1ST
                MOVE CB-NUMBER               TO WS-DAILY-2ND
                MOVE " "                     TO WS-DAILY-3RD
                                                WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           GO TO W-CBM-LY-010.
       W-CBM-LY-900.
           CLOSE CB-MASTER.
           CLOSE CB-LY-MASTER.
           MOVE 1510 TO POS
           DISPLAY " 2. CBMast L/Y Process Complete.           " AT POS
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           ADD 20 TO POS
           DISPLAY WS-MESSAGE AT POS.
       W-CBM-LY-999.
            EXIT.
      *
       WRITE-TRANS-TO-LAST-YEAR SECTION.
       W-TO-YEAR-000.
            ACCEPT WS-TIME FROM TIME.
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC.
            MOVE " 3. CbTrans-WriteLY " TO WS-DAILY-1ST
            MOVE "Start of process    " TO WS-DAILY-2ND
            MOVE SPLIT-TIME             TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
       W-TO-YEAR-005.
           MOVE 2 To WS-RECORD-TO-DELETE
      *     PERFORM DELETE-TRANS.
           PERFORM OPEN-012.
           PERFORM OPEN-005.
           MOVE " " TO CBTRANS-FUTURE.
           MOVE 1   TO CBTRANS-NO.
           START CBTRANS-FILE KEY NOT < CBTRANS-PERIOD
                INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE 
            "WE HAVE A PROBLEM IN THE START OF CBTRANS, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO W-TO-YEAR-900.
           MOVE 1610 TO POS
           DISPLAY " 3. CBTrans Last/Year File Being Processed." AT POS.
       W-TO-YEAR-010.
           READ CBTRANS-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-CBTRANS-ST1 = 10
               GO TO W-TO-YEAR-900.

           MOVE 2910 TO POS
           DISPLAY "CBTrans Being Processed:" AT POS
           ADD 26 TO POS
           DISPLAY CBTRANS-REFERENCE AT POS
           ADD 15 TO POS
           DISPLAY CBTRANS-TRANS AT POS
           ADD 10 TO POS

           DISPLAY CBTRANS-TYPE AT POS.
           IF CBTRANS-FUTURE = "F"
               GO TO W-TO-YEAR-010.
       W-TO-YEAR-200.
           MOVE CBTRANS-REFERENCE      TO CBTRANS-LY-REFERENCE
           MOVE CBTRANS-TRANS          TO CBTRANS-LY-TRANS
           MOVE CBTRANS-TYPE           TO CBTRANS-LY-TYPE
           MOVE CBTRANS-NO             TO CBTRANS-LY-NO
           MOVE CBTRANS-CBMASTER       TO CBTRANS-LY-CBMASTER
           MOVE CBTRANS-DATE           TO CBTRANS-LY-DATE
           MOVE CBTRANS-TYPE-OF-POST   TO CBTRANS-LY-TYPE-OF-POST
           MOVE CBTRANS-ALLOCATED      TO CBTRANS-LY-ALLOCATED
           MOVE CBTRANS-ACCOUNT-NUMBER TO CBTRANS-LY-ACCOUNT-NUMBER
           MOVE CBTRANS-AMOUNT         TO CBTRANS-LY-AMOUNT
           MOVE CBTRANS-LINE-DESC      TO CBTRANS-LY-LINE-DESC.
       W-TO-YEAR-800.
           WRITE CBTRANS-LY-REC.
           IF WS-CBTRANS-LY-ST1 NOT = 0
                MOVE WS-CBTRANS-LY-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                MOVE "CBTRANS NOT WRITTEN "  TO WS-DAILY-1ST
                MOVE CBTRANS-REFERENCE       TO WS-DAILY-2ND
                MOVE CBTRANS-TRANS           TO WS-DAILY-3RD
                MOVE CBTRANS-TYPE            TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           IF CBTRANS-ALLOCATED = "Y"
            DELETE CBTRANS-FILE
               INVALID KEY
                MOVE "CBTRANS RECORD NOT  "  TO WS-DAILY-1ST
                MOVE "DELETED, NUMBER   : "  TO WS-DAILY-2ND
                MOVE CBTRANS-REFERENCE       TO WS-DAILY-3RD
                MOVE " "                     TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           GO TO W-TO-YEAR-010.
       W-TO-YEAR-900.
           CLOSE CBTRANS-FILE.
           CLOSE CBTRANS-LY-FILE.
           MOVE 1610 TO POS.
           DISPLAY " 3. CBTrans L/Y Process Complete.          " AT POS.
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           ADD 20 TO POS
           DISPLAY WS-MESSAGE AT POS.
       W-TO-YEAR-999.
            EXIT.
      *
       UPDATE-CBMASTER SECTION.
       GLM-000.
            ACCEPT WS-TIME FROM TIME
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC.
            IF WS-ANSWER1 = "Y"
               MOVE " 4. CB-Master File  " TO WS-DAILY-1ST
            ELSE
               MOVE " 2. CB-Master File  " TO WS-DAILY-1ST.
            MOVE "Start of process    "    TO WS-DAILY-2ND
            MOVE SPLIT-TIME                TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
       GLM-005.
            PERFORM OPEN-011.
            IF WS-ANSWER1 = "Y"
              MOVE 1710 TO POS
              DISPLAY " 4. CbMaster Files Being Processed...." AT POS
            ELSE
              MOVE 1510 TO POS
              DISPLAY " 2. CbMaster Files Being Processed...." AT POS.
              MOVE " " TO CB-NUMBER
              START CB-MASTER KEY NOT LESS CB-NUMBER
                 INVALID KEY NEXT SENTENCE.
       GLM-010.
            READ CB-MASTER NEXT WITH LOCK
                 AT END
                 GO TO GLM-900.
            MOVE 2910 TO POS.
            DISPLAY "Account No Being Processed :" AT POS.
            ADD 30 TO POS.
            DISPLAY CB-NUMBER AT POS.
            IF WS-ANSWER1 = "M"
               MOVE CB-BALANCE TO CB-OPEN-PER-BAL.
            IF WS-ANSWER1 = "Y"
               MOVE CB-OPEN-YEAR-BAL TO CB-LY-OPEN-BAL
               MOVE CB-BALANCE       TO CB-OPEN-PER-BAL
                                        CB-OPEN-YEAR-BAL.
            IF WS-ANSWER1 = "M"
                 GO TO GLM-020.
            MOVE 0 TO SUB-1.
       GLM-015.
            ADD 1 TO SUB-1.
            IF SUB-1 > 12
               MOVE 1 TO SUB-1
               GO TO GLM-020.
            MOVE CB-PER (SUB-1) TO CB-LAST-PER (SUB-1).
       GLM-016.
            MOVE 0 TO CB-PER (SUB-1).
            GO TO GLM-015.
       GLM-020.
            REWRITE CB-RECORD
                INVALID KEY
                MOVE "CBMASTER RECORD:"  TO WS-DAILY-1ST
                MOVE CB-NUMBER           TO WS-DAILY-2ND
                MOVE "NOT ROLLED!!"      TO WS-DAILY-3RD
                MOVE "THIS MONTH END"    TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
            GO TO GLM-010.
       GLM-900.
            IF WS-ANSWER1 = "Y"
              MOVE 1710 TO POS
              DISPLAY " 4. CbMaster Files Process Complete.       "
              AT POS
            ELSE
              MOVE 1510 TO POS
              DISPLAY " 2. CbMaster Files Process Complete.       "
              AT POS.
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           ADD 20 TO POS
           DISPLAY WS-MESSAGE AT POS.
           CLOSE CB-MASTER.
       GLM-999.
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
              MOVE "GLPARAMETER BUSY ON READ, 'ESC' TO RETRY"
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
       GLPARAMETERS SECTION.
       PAR-000.
            ACCEPT WS-TIME FROM TIME
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC.
            IF WS-ANSWER1 = "Y"
                MOVE " 5. GlParameter file" TO WS-DAILY-1ST
            ELSE
                MOVE " 3. GlParameter file" TO WS-DAILY-1ST.
            MOVE "Start of process        " TO WS-DAILY-2ND.
            MOVE SPLIT-TIME                 TO WS-DAILY-3RD.
            PERFORM WRITE-DAILY.
       PAR-005.
            IF WS-ANSWER1 = "Y"
                MOVE 1810 TO POS
                DISPLAY " 5. GlParameter File Being Processed." AT POS
            ELSE
                MOVE 1610 TO POS
                DISPLAY " 3. GlParameter File Being Processed." AT POS.
            PERFORM OPEN-001.
            MOVE 1 TO GLPA-RECORD.
            READ GLPARAMETER-FILE WITH LOCK
                INVALID KEY
                MOVE "GLPARAMETER RECORD NOT FOUND, 'ESC' TO EXIT." 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO PAR-999.
       PAR-010.
            ADD 1 TO GLPA-CURRENT-CBPER.
            MOVE "N" TO GLPA-CB-POST.
       PAR-020.
            IF GLPA-CURRENT-CBPER > 12
                MOVE 1 TO GLPA-CURRENT-CBPER.
            REWRITE GLPARAMETER-REC
                INVALID KEY
                MOVE "GLPARAMETER RECORD" TO WS-DAILY-1ST
                MOVE "NOT UPDATED!!!"     TO WS-DAILY-2ND
                MOVE " "                  TO WS-DAILY-3RD
                MOVE " "                  TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
            CLOSE GLPARAMETER-FILE.
        PAR-900.
           IF WS-ANSWER1 = "Y"
               MOVE 1810 TO POS
              DISPLAY " 5. GlParameter File Process Complete." AT POS
           ELSE
               MOVE 1610 TO POS
              DISPLAY " 3. GlParameter File Process Complete." AT POS.
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           ADD 20 TO POS
           DISPLAY WS-MESSAGE AT POS.
        PAR-999.
            EXIT.
      *
       CHECK-FUTURE-TRANS SECTION.
       CH-FU-000.
            MOVE 0 TO WS-POSTED.
            ACCEPT WS-TIME FROM TIME
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC.
            IF WS-ANSWER1 = "Y"
                MOVE " 6. CbTrans-Future  " TO WS-DAILY-1ST
            ELSE
                MOVE " 4. CbTrans-Future  " TO WS-DAILY-1ST.
            MOVE "Start of process        " TO WS-DAILY-2ND
            MOVE SPLIT-TIME                 TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
       CH-FU-004.
           PERFORM OPEN-011.
           PERFORM OPEN-005.
           IF WS-ANSWER1 = "Y"
             MOVE 1910 TO POS
             DISPLAY " 6. CbTrans Future File Being Processed.  "
                  AT POS
            ELSE
             MOVE 1710 TO POS
             DISPLAY " 4. CbTrans Future File Being Processed.  "
                  AT POS.
           MOVE GLPA-CURRENT-CBPER TO WS-CBTRANS-NO.
       CH-FU-006.
           MOVE "F"           TO CBTRANS-FUTURE.
           MOVE WS-CBTRANS-NO TO CBTRANS-NO.
           START CBTRANS-FILE KEY NOT < CBTRANS-PERIOD
               INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "NO FUTURE TRANSACTIONS TO POST, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO CH-FU-900.
       CH-FU-010.
           READ CBTRANS-FILE NEXT WITH LOCK
               AT END
               GO TO CH-FU-900.

           MOVE 2910 TO POS.
           DISPLAY "CbTrans Being Processed:" AT POS.
           ADD 26 TO POS.
           DISPLAY CBTRANS-REFERENCE AT POS.

           IF CBTRANS-NO NOT = WS-CBTRANS-NO
               GO TO CH-FU-010.
           IF CBTRANS-FUTURE NOT = "F"
               GO TO CH-FU-010.
           IF CBTRANS-FUTURE = "F"
            IF CBTRANS-NO NOT = GLPA-CURRENT-CBPER
              GO TO CH-FU-010.
           ADD 1 TO WS-POSTED
           MOVE WS-POSTED TO WS-POSTED-DIS
           MOVE 2510 TO POS
           DISPLAY "    No. Of Trans Posted:" AT POS
           ADD 26 TO POS
           DISPLAY WS-POSTED-DIS AT POS
           PERFORM UPDATE-CBMASTER-FILES.
       CH-FU-800.
           MOVE " " TO CBTRANS-FUTURE.
           REWRITE CBTRANS-REC
               INVALID KEY
                MOVE "GLTRANS RECORD NOT  "  TO WS-DAILY-1ST
                MOVE "REWRITTEN, NUMBER : "  TO WS-DAILY-2ND
                MOVE CBTRANS-REFERENCE       TO WS-DAILY-3RD
                MOVE " "                     TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           GO TO CH-FU-010.
       CH-FU-900.
           CLOSE CBTRANS-FILE.
           CLOSE CB-MASTER.
           IF WS-ANSWER1 = "Y"
            MOVE 1910 TO POS
               DISPLAY " 6. CbTrans Future Process Complete.          "
                   AT POS
           ELSE
            MOVE 1710 TO POS
               DISPLAY " 4. CbTrans Future Process Complete.          "
                   AT POS.
           MOVE " " TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS.
       CH-FU-999.
            EXIT.
      *
       UPDATE-CBMASTER-FILES SECTION.
       UPCB-000.
           MOVE 2610 TO POS.
           DISPLAY "UPDATING CB-MASTER FILE...." AT POS.
       UPCB-005.
           MOVE CBTRANS-CBMASTER TO CB-NUMBER.
           START CB-MASTER KEY NOT < CB-KEY.
       UPCB-010.
           READ CB-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
              MOVE "CBMASTER FILE DOES NOT EXIST23, 'ESC  TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CB-ST1
              GO TO UPCB-999.
           IF WS-CB-ST1 NOT = 0
              MOVE "CBMASTER RECORD BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CB-ST1
              GO TO UPCB-010.
           ADD CBTRANS-AMOUNT TO CB-BALANCE.
           ADD CBTRANS-AMOUNT TO CB-PER (WS-CBTRANS-NO).
       UPCB-900.
           REWRITE CB-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
               GO TO UPCB-999.
           IF WS-CB-ST1 NOT = 0
              MOVE "CBMASTER BUSY ON REWRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CB-ST1
              GO TO UPCB-900.
       UPCB-960.
           MOVE 2610 TO POS
           DISPLAY "                                          " AT POS.
       UPCB-999.
           EXIT.
      *
       CHECK-DATA-SIZE SECTION.         
       CDS-005.
           Move " " To Alpha-Rate.
           Move 0   To Sub-1.
       CDS-010.
           If Ws-Record-To-Delete = 1
               Move Ws-CbMasterLy To Alpha-Rate
           Else
               Move Ws-CbTransLy  To Alpha-Rate.
       CDS-015.
           Add 1 To Sub-1.
           If Al-Rate (Sub-1) Not = " "
            If Sub-1 Not > 60
            Go To CDS-015.
          Subtract 1 from Sub-1.
          Go to CDS-999.
       CDS-020.
           Add 1 To Sub-1
           Move "." to Al-Rate (Sub-1)
           Add 1 To Sub-1
           Move "I" to Al-Rate (Sub-1)
           Add 1 To Sub-1
           Move "n" to Al-Rate (Sub-1)
           Add 1 To Sub-1
           Move "d" to Al-Rate (Sub-1).
       CDS-030.
           Move 0   To Sub-1.
           Move " " to Data-Rate.
       CDS-035.
           Add 1 To Sub-1.
           If Al-Rate (Sub-1) Not = ">"
            Move Al-Rate (Sub-1) to Dat-Rate (Sub-1)
            If Sub-1 Not > 60
            Go To CDS-035.
            
           Move Sub-1 to Sub-2
           Move "I" to Dat-Rate (Sub-2)
           Add 1 to Sub-2
           Move "n" to Dat-Rate (Sub-2)
           Add 1 to Sub-2
           Move "d" to Dat-Rate (Sub-2)
           Subtract 1 from Sub-1.
       CDS-045.
           Add 1 To Sub-1 Sub-2.
           If Al-Rate (Sub-1) Not = " "
            Move Al-Rate (Sub-1) to Dat-Rate (Sub-2)
            If Sub-2 Not > 60
            Go To CDS-045.
            Subtract 1 from Sub-2.
            Move Sub-2 to Sub-1.
       CDS-900.
           If Ws-Record-To-Delete = 1
               Move Data-Rate to Ws-CbMasterLyInd
           Else
               Move Data-Rate to Ws-CbTransLyInd.
       CDS-999.
          EXIT.
      *
       DELETE-TRANS SECTION.
       DST-010.
           PERFORM CHECK-DATA-SIZE.
           
           If Ws-Record-To-Delete = 1
               MOVE Ws-CbMasterLy TO F-FILENAME
           Else
               MOVE Ws-CbTransLy  TO F-FILENAME.
           MOVE SUB-1     TO F-CBFILENAME.
           CALL "OPENFILE" USING  F-ERROR1
                                  F-FH
                                  F-FILENAME
                                  F-CBFILENAME
                                  F-FILENAME
                                  F-INTEGERZERO
                                  F-OPENMODE-MM.
           CALL "DELETEFILE" USING F-ERROR1
                                   F-FH.
           IF F-ERROR1 NOT = 0
              MOVE 2410 TO POS
              DISPLAY F-ERROR1 AT POS
              ADD 5 TO POS
              DISPLAY "ERROR IN DELETING FILE" AT POS
              ADD 23 TO POS
              DISPLAY F-FILENAME AT POS.
              
           PERFORM CDS-020 THRU CDS-900.
           
           If Ws-Record-To-Delete = 1
               MOVE Ws-CbMasterLyInd TO F-FILENAME
           Else
               MOVE Ws-CbTransLyInd  TO F-FILENAME.
           MOVE Sub-1        TO F-CBFILENAME.
           CALL "OPENFILE" USING  F-ERROR1
                                  F-FH
                                  F-FILENAME
                                  F-CBFILENAME
                                  F-FILENAME
                                  F-INTEGERZERO
                                  F-OPENMODE-MM.
           CALL "DELETEFILE" USING F-ERROR1
                                   F-FH.
           IF F-ERROR1 NOT = 0
              MOVE 2410 TO POS
              DISPLAY F-ERROR1 AT POS
              ADD 5 TO POS
              DISPLAY "ERROR IN DELETING FILE" AT POS
              ADD 23 TO POS
              DISPLAY F-FILENAME AT POS.
       DST-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-001.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE "GLPARAMETER FILE BUSY ON OPEN,  'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLPARAMETER-ST1
               GO TO OPEN-001.
       OPEN-005.
           OPEN I-O CBTRANS-FILE.
           IF WS-CBTRANS-ST1 NOT = 0
                MOVE "CB-TRANS BUSY ON OPEN I-O, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CBTRANS-ST1
                GO TO OPEN-005.
       OPEN-010.
           OPEN OUTPUT CB-LY-MASTER.
           IF WS-CB-LY-ST1 NOT = 0
                MOVE "CB-MASTERLY BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CB-LY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CB-LY-ST1
                GO TO OPEN-010.
       OPEN-011.
           OPEN I-O CB-MASTER.
           IF WS-CB-ST1 NOT = 0
                MOVE "CB-MASTER BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CB-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CB-ST1
                GO TO OPEN-011.
       OPEN-012.
           OPEN OUTPUT CBTRANS-LY-FILE.
           IF WS-CBTRANS-LY-ST1 NOT = 0
                MOVE "CB-TRANS-LY BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CBTRANS-LY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CBTRANS-LY-ST1
                GO TO OPEN-012.
       OPEN-999.
           EXIT.
      *
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "NumberCheck".
       Copy "GetSystemY2KDate".
       Copy "DisplayCBTopInfo".
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
       Copy "WriteDailyExcep1".
      * END-OF-JOB
