        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlPerEnd.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlMaster".
        Copy "SelectGlMasterLy".
        Copy "SelectGlTrans".
        Copy "SelectGlTransLy".
        Copy "SelectGlJrn".
        Copy "SelectGlParameter".
        Copy "SelectSlDaily".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdGlMast.
           COPY ChlfdGlMastLy.
           COPY ChlfdGlTrans.
           COPY ChlfdGlTransLy.
           COPY ChlfdGlJrn.
           COPY ChlfdGlParam.
           COPY ChlfdDaily.
      *
       WORKING-STORAGE SECTION.
       77  Ws-GlMasterLyInd     Pic X(60) value " ".
       77  Ws-GlTransLyInd      Pic X(60) value " ".
       77  WS-STOP-UPDATE       PIC X VALUE " ".
       77  WS-RECORD-TO-DELETE  PIC 9.
       77  WS-POSTED            PIC 9(5) VALUE 0.
       77  WS-POSTED-DIS        PIC Z(4)9.
       77  WS-GLTRANS-NO        PIC 9(2) VALUE 0.
       77  WS-BU-ACT            PIC X VALUE " ".
       77  WS-FACTOR            PIC 9(3)V99 VALUE 0.
       77  WS-FACTOR-ACCEPT     PIC X(6) VALUE " ".
       77  WS-FACTOR-DISPLAY    PIC Z(2)9.99.
       77  WS-BUDGET            PIC S9(8)V99 VALUE 0.
       77  WS-MONTH-YEAR           PIC X VALUE " ".
       77  WS-ANSWER2           PIC X VALUE " ".
       77  WS-ANSWER3           PIC X VALUE " ".
       77  WS-ANSWER4           PIC X VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-CALC-MM           PIC 9(2) VALUE 0.
       77  WS-WORK-MM           PIC 9(4) VALUE 0.
       77  WS-WORK-PO-MM        PIC 9(4) VALUE 0.
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
       01  SPLIT-TIME.
           03  SPLIT-HR            PIC 99.
           03  SPLIT-FIL1          PIC X.
           03  SPLIT-MN            PIC 99.
           03  SPLIT-FIL2          PIC X.
           03  SPLIT-SC            PIC 99.
       01  WS-GLTRANS-STATUS.
           03  WS-GLTRANS-ST1      PIC 99.
       01  WS-GLTRANS-LY-STATUS.
           03  WS-GLTRANS-LY-ST1   PIC 99.
       01  WS-GLJRN-STATUS.
           03  WS-GLJRN-ST1        PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1       PIC 99.
       01  WS-GL-LY-STATUS.
           03  WS-GL-LY-ST1        PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 0310 TO POS
           DISPLAY "* GENERAL LEDGER MONTH/YEAR END PROCESSING *"
               AT POS
           MOVE 0410 TO POS
           DISPLAY "********************************************"
               AT POS.
           PERFORM DISPLAY-TOP-INFO.
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
           MOVE CDA-DATA TO WS-MONTH-YEAR.

           IF W-ESCAPE-KEY = 3
               PERFORM CONTROL-900.
           IF WS-MONTH-YEAR NOT = "M" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-005.
       CONTROL-010.
           IF WS-MONTH-YEAR = "M" OR = "Y"
               MOVE 810 TO POS
           DISPLAY "Have You Run Your Month / Year End Reports?:[ ]"
               AT POS
               MOVE 855 TO POS

               MOVE ' '       TO CDA-DATA
               MOVE 1         TO CDA-DATALEN
               MOVE 5         TO CDA-ROW
               MOVE 54        TO CDA-COL
               MOVE CDA-WHITE TO CDA-COLOR
               MOVE 'F'       TO CDA-ATTR
               PERFORM CTOS-ACCEPT
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
           IF WS-MONTH-YEAR = "M"
               MOVE 910 TO POS
           DISPLAY "You Have Selected To Do MONTH END Processing."
               AT POS.
           IF WS-MONTH-YEAR = "Y"
               MOVE 910 TO POS
           DISPLAY "You Have Selected To Do YEAR END Processing."
               AT POS.
       CONTROL-040.
           PERFORM GET-SYSTEM-Y2K-DATE.
           IF WS-MONTH-YEAR = "M"
              GO TO CONTROL-055.
           MOVE 1010 TO POS.
           DISPLAY
            "Enter A Factor By Which Budgets Will be Reworked: [      ]"
                 AT POS.
           ADD 51 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 6         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 60        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FACTOR-ACCEPT.

           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-010.
           MOVE WS-FACTOR-ACCEPT TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WS-FACTOR WS-FACTOR-DISPLAY.
           DISPLAY WS-FACTOR-DISPLAY AT POS.
           IF WS-FACTOR = 0
                 GO TO CONTROL-055.
       CONTROL-045.
           MOVE 1110 TO POS.
           DISPLAY
               "Should This Be Worked On L/Year Budgets OR Actuals: [ ]"
               AT POS.
           ADD 53 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 8         TO CDA-ROW.
           MOVE 62        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-BU-ACT.

           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-040.
           IF WS-BU-ACT NOT = "B" AND NOT = "A"
              MOVE 3010 TO POS
              DISPLAY "YOU SHOULD ENTER 'B' OR 'A', RE-ENTER" AT POS
              DISPLAY " " AT 3079 WITH BELL
              GO TO CONTROL-045.
       CONTROL-055.
           MOVE 3010 TO POS.
           DISPLAY "                                             "
                 AT POS.
           MOVE 1210 TO POS.
           IF WS-MONTH-YEAR = "Y"
               DISPLAY "There Are 6 Data Files To Process." AT POS
           ELSE
               DISPLAY "There Are 4 Data Files To Process." AT POS.
           PERFORM CHECK-GLPERIODS.
           PERFORM DELETE-GLJRNS.
           
           IF WS-MONTH-YEAR = "Y"
              PERFORM WRITE-GLMASTER-TO-LAST-YEAR
              PERFORM WRITE-TRANS-TO-LAST-YEAR.
            PERFORM UPDATE-GLMASTER.
            PERFORM GLPARAMETERS.
            PERFORM CHECK-FUTURE-TRANS. 
            ACCEPT WS-TIME FROM TIME
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
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER3.

           IF W-ESCAPE-KEY = 1 OR 2
               GO TO CONTROL-900
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-600.
       CONTROL-900.
           EXIT PROGRAM.
      *
       DISPLAY-TOP-INFO SECTION.
       DTI-000.
            PERFORM OPEN-000.
            PERFORM READ-PARAMETER.
            PERFORM ENTER-PERIOD-DATES.
            CLOSE GLPARAMETER-FILE.
       DTI-005.
           MOVE 0360 TO POS.
           DISPLAY "Period :" AT POS
           ADD 9 TO POS
           DISPLAY GLPA-CURRENT-GLPER AT POS.

           MOVE GLPA-CURRENT-GLPER TO SUB-1
           MOVE 0456 TO POS
           MOVE GL-BEGDATE (SUB-1) TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           DISPLAY DISPLAY-DATE AT POS.
           ADD 11 TO POS
           DISPLAY ":" AT POS
           ADD 2 TO POS
           MOVE GL-ENDDATE (SUB-1) TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           DISPLAY DISPLAY-DATE AT POS.
       DTI-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO GLPARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING W-ERC
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON READ, 'ESC' TO RETRY."
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
       CHECK-GLPERIODS SECTION.
       CH-GLPA-005.
            PERFORM OPEN-000.
            MOVE 1 TO GLPA-RECORD.
            READ GLPARAMETER-FILE WITH LOCK
                INVALID KEY
                MOVE "GLPARAMETER RECORD NOT FOUND!!!" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO CH-GLPA-999.
       CH-GLPA-010.
            IF GLPA-CURRENT-GLPER = 12
              IF WS-MONTH-YEAR = "M"
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
                MOVE 50        TO CDA-COL
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
           IF WS-MONTH-YEAR = "Y"
            IF GLPA-CURRENT-GLPER = 12
             IF GLPA-CURRENT-CRPER = 12
                MOVE 1510 TO POS
                DISPLAY " " AT 3079 WITH BELL
                DISPLAY
               "Creditors Ledger should have YEAR END run FIRST." AT POS
                MOVE 1620 TO POS
                DISPLAY "Press 'NEXT' Or 'GO' To End The Program."
                AT POS
                MOVE 1670 TO POS

                MOVE ' '       TO CDA-DATA
                MOVE 1         TO CDA-DATALEN
                MOVE 13        TO CDA-ROW
                MOVE 70        TO CDA-COL
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
           IF WS-MONTH-YEAR = "M"
            IF GLPA-CURRENT-GLPER = GLPA-CURRENT-CRPER
                MOVE 1510 TO POS
                DISPLAY " " AT 3079 WITH BELL
                DISPLAY
               "Creditors Ledger should have MONTH END run FIRST."
                AT POS
                MOVE 1620 TO POS
                DISPLAY "Press 'NEXT' Or 'GO' To End The Program."
                AT POS
                MOVE 1670 TO POS

                MOVE ' '       TO CDA-DATA
                MOVE 1         TO CDA-DATALEN
                MOVE 13        TO CDA-ROW
                MOVE 50        TO CDA-COL
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
            CLOSE GLPARAMETER-FILE.
        CH-GLPA-999.
            EXIT.
      *
       DELETE-GLJRNS SECTION.
       DGLJ-000.
            ACCEPT WS-TIME FROM TIME
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC
            MOVE "G/Ledger Period End:" TO WS-DAILY-1ST
            MOVE GLPA-CURRENT-GLPER     TO WS-DAILY-2ND
            MOVE "Start of process    " TO WS-DAILY-3RD
            MOVE SPLIT-TIME             TO WS-DAILY-4TH
            PERFORM WRITE-DAILY.
            MOVE " 1. GlJrns-File     " TO WS-DAILY-1ST
            MOVE "Start of process    " TO WS-DAILY-2ND
            MOVE SPLIT-TIME             TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
           PERFORM OPEN-005.
           MOVE 1410 TO POS
           DISPLAY " 1. GlJrns File Being Processed.          " AT POS.
           
           MOVE " " TO GLJRN-REFERENCE.
           START GLJRN-FILE KEY NOT < GLJRN-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-GLJRN-ST1 NOT = 0
              MOVE "ERROR IN START ON GLJRN, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLJRN-ST1
              GO TO DGLJ-900.
       DGLJ-010.
           READ GLJRN-FILE NEXT WITH LOCK
               AT END
               GO TO DGLJ-900.
           MOVE 2910 TO POS.
           DISPLAY "      GlJrn Being Processed:" AT POS.
           ADD 30 TO POS.
           DISPLAY GLJRN-REFERENCE AT POS.
           IF GLJRN-ACTION = "R"
            IF GLJRN-COMPLETE = "Y"
               MOVE "P" TO GLJRN-COMPLETE
               GO TO DGLJ-850
            ELSE
               GO TO DGLJ-010.
           IF GLJRN-COMPLETE = "Y"
               GO TO DGLJ-800.
           GO TO DGLJ-010.
       DGLJ-800.
           DELETE GLJRN-FILE
               INVALID KEY
                MOVE "GLJRN TRANS RECORD N"  TO WS-DAILY-1ST
                MOVE "OT DELETED, NUMBER :"  TO WS-DAILY-2ND
                MOVE GLJRN-REFERENCE         TO WS-DAILY-3RD
                MOVE " "                     TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           GO TO DGLJ-010.
       DGLJ-850.
           REWRITE GLJRN-REC
               INVALID KEY
                MOVE "GLJRN TRANS RECORD N"  TO WS-DAILY-1ST
                MOVE "OT RE-WRITTEN NUMBR:"  TO WS-DAILY-2ND
                MOVE GLJRN-REFERENCE         TO WS-DAILY-3RD
                MOVE " "                     TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           GO TO DGLJ-010.
       DGLJ-900.
           CLOSE GLJRN-FILE
           MOVE 1410 TO POS
           DISPLAY " 1. GlJrn File Process Complete.           " AT POS
           MOVE 2910 TO POS
           DISPLAY "                                           " AT POS.
       DGLJ-999.
            EXIT.
      *
       WRITE-GLMASTER-TO-LAST-YEAR SECTION.
       W-GLM-LY-000.
            ACCEPT WS-TIME FROM TIME
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC
            MOVE " 2. GlMast-WriteLY  " TO WS-DAILY-1ST
            MOVE "Start of process    " TO WS-DAILY-2ND
            MOVE SPLIT-TIME             TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
       W-GLM-LY-005.
           MOVE 1 To WS-RECORD-TO-DELETE
      *     PERFORM DELETE-TRANS.
           PERFORM OPEN-011.
           PERFORM OPEN-015.
           MOVE " " TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 NOT = 0
                MOVE 
           "WE HAVE A PROBLEM IN THE START OF GLMASTER W-GLM-LY-005."
                TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE.
           MOVE 1510 TO POS
           DISPLAY " 2. GlMast Last/Year File Being Processed." AT POS.
       W-GLM-LY-010.
           READ GL-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 10
               GO TO W-GLM-LY-900.
           MOVE 2910 TO POS
           DISPLAY "GlMast Being Processed:" AT POS
           ADD 26 TO POS
           DISPLAY GL-NUMBER AT POS.
       W-GLM-LY-200.
           MOVE GL-NUMBER           TO GL-LY-NUMBER
           MOVE GL-DESCRIPTION      TO GL-LY-DESCRIPTION
           MOVE GL-P-B              TO GL-LY-P-B
           MOVE GL-DATE             TO GL-LY-DATE
           MOVE GL-BALANCE          TO GL-LY-BALANCE
           MOVE GL-OPEN-YEAR-BAL    TO GL-LY-OPEN-YEAR-BAL
           MOVE GL-LAST-YEAR-BAL    TO GL-LY-LAST-YEAR-BAL
           MOVE GL-CURRENT-PERIODS  TO GL-LY-CURRENT-PERIODS
           MOVE GL-CURRENT-BUDGETS  TO GL-LY-CURRENT-BUDGETS
           MOVE GL-LAST-PERIODS     TO GL-LY-LAST-PERIODS.
       W-GLM-LY-800.
           WRITE GL-LY-RECORD.
           IF WS-GL-LY-ST1 NOT = 0
                MOVE WS-GL-LY-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                MOVE "GLMASTER NOT WRITTEN"  TO WS-DAILY-1ST
                MOVE GL-NUMBER               TO WS-DAILY-2ND
                MOVE " "                     TO WS-DAILY-3RD
                                                WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           GO TO W-GLM-LY-010.
       W-GLM-LY-900.
           CLOSE GL-MASTER.
           CLOSE GL-LY-MASTER.
           MOVE 1510 TO POS
           DISPLAY " 2. GlMast L/Y Process Complete.           " AT POS
           MOVE 2910 TO POS
           DISPLAY "                                           " AT POS.
       W-GLM-LY-999.
            EXIT.
      *
       WRITE-TRANS-TO-LAST-YEAR SECTION.
       W-TO-YEAR-000.
            ACCEPT WS-TIME FROM TIME
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC
            MOVE " 3. GlTrans-WriteLY " TO WS-DAILY-1ST
            MOVE "Start of process    " TO WS-DAILY-2ND
            MOVE SPLIT-TIME             TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
       W-TO-YEAR-005.
           MOVE 2 To WS-RECORD-TO-DELETE
      *     PERFORM DELETE-TRANS.
           PERFORM OPEN-021.
           PERFORM OPEN-025.
           MOVE 1 TO GLTRANS-NO.
           START GLTRANS-FILE KEY NOT < GLTRANS-NO
                INVALID KEY NEXT SENTENCE.
           IF WS-GLTRANS-ST1 NOT = 0
                MOVE "WE HAVE A PROBLEM IN THE START OF GLTRANS"
                TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE.
           MOVE 1610 TO POS
           DISPLAY " 3. GlTrans Last/Year File Being Processed." AT POS.
       W-TO-YEAR-010.
           READ GLTRANS-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-GLTRANS-ST1 = 10
               GO TO W-TO-YEAR-900.
           MOVE 2910 TO POS
           DISPLAY "GlTrans Being Processed:" AT POS
           ADD 26 TO POS
           DISPLAY GLTRANS-REFERENCE AT POS
           ADD 15 TO POS
           DISPLAY GLTRANS-TRANS AT POS
           ADD 10 TO POS
           DISPLAY GLTRANS-TYPE AT POS.
           IF GLTRANS-FUTURE = "F"
               GO TO W-TO-YEAR-010.
       W-TO-YEAR-200.
           MOVE GLTRANS-REFERENCE      TO GLTRANS-LY-REFERENCE
           MOVE GLTRANS-TRANS          TO GLTRANS-LY-TRANS
           MOVE GLTRANS-TYPE           TO GLTRANS-LY-TYPE
           MOVE GLTRANS-NO             TO GLTRANS-LY-NO
           MOVE GLTRANS-DATE           TO GLTRANS-LY-DATE
           MOVE GLTRANS-ACCOUNT-NUMBER TO GLTRANS-LY-ACCNO
           MOVE GLTRANS-AMOUNT         TO GLTRANS-LY-AMOUNT
           MOVE GLTRANS-LINE-DESC      TO GLTRANS-LY-LINE-DESC.
       W-TO-YEAR-800.
           WRITE GLTRANS-LY-REC.
           IF WS-GLTRANS-LY-ST1 NOT = 0
                MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                MOVE "GLTRANS NOT WRITTEN "  TO WS-DAILY-1ST
                MOVE GLTRANS-REFERENCE       TO WS-DAILY-2ND
                MOVE GLTRANS-TRANS           TO WS-DAILY-3RD
                MOVE GLTRANS-TYPE            TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           DELETE GLTRANS-FILE
               INVALID KEY
                MOVE "GLTRANS RECORD NOT  "  TO WS-DAILY-1ST
                MOVE "DELETED, NUMBER   : "  TO WS-DAILY-2ND
                MOVE GLTRANS-REFERENCE       TO WS-DAILY-3RD
                MOVE " "                     TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           GO TO W-TO-YEAR-010.
       W-TO-YEAR-900.
           CLOSE GLTRANS-FILE
           CLOSE GLTRANS-LY-FILE
           MOVE 1610 TO POS
           DISPLAY " 3. GlTrans L/Y Process Complete.          " AT POS
           MOVE 2910 TO POS
           DISPLAY "                                                   "
               AT POS.
       W-TO-YEAR-999.
            EXIT.
      *
       UPDATE-GLMASTER SECTION.
       GLM-000.
            ACCEPT WS-TIME FROM TIME
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC.
           IF WS-MONTH-YEAR = "Y"
               MOVE " 4. Gl-Master File  " TO WS-DAILY-1ST
            ELSE
               MOVE " 2. Gl-Master File  " TO WS-DAILY-1ST.
            MOVE "Start of process    " TO WS-DAILY-2ND
            MOVE SPLIT-TIME             TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
       GLM-005.
           PERFORM OPEN-015.
           MOVE " " TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE
           "WE HAVE A PROBLEM IN THE START OF GLMASTER GLM-005."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1.
           IF WS-MONTH-YEAR = "Y"
               MOVE 1710 TO POS
               DISPLAY " 4. GlMaster Files Being Processed." AT POS
           ELSE
               MOVE 1510 TO POS
               DISPLAY " 2. GlMaster Files Being Processed." AT POS.
       GLM-010.
            READ GL-MASTER NEXT WITH LOCK
                 AT END
                 GO TO GLM-900.
            MOVE 2910 TO POS.
            DISPLAY "Account No Being Processed :" AT POS.
            ADD 30 TO POS.
            DISPLAY GL-NUMBER AT POS.
            IF WS-MONTH-YEAR = "M"
               MOVE GL-BALANCE TO GL-OPEN-PER-BAL.
            IF WS-MONTH-YEAR = "Y"
             IF GL-P-B = "P"
               MOVE 0          TO GL-BALANCE
                                  GL-LAST-YEAR-BAL
                                  GL-OPEN-PER-BAL
                                  GL-OPEN-YEAR-BAL
             ELSE
               MOVE GL-OPEN-YEAR-BAL TO GL-LAST-YEAR-BAL
               MOVE GL-BALANCE       TO GL-OPEN-PER-BAL
                                        GL-OPEN-YEAR-BAL.
            IF WS-MONTH-YEAR = "M"
                 GO TO GLM-020.
            MOVE 0 TO SUB-1.
       GLM-015.
            ADD 1 TO SUB-1.
            IF SUB-1 > 12
               MOVE 1 TO SUB-1
               GO TO GLM-020.
            MOVE GL-PER (SUB-1) TO GL-LAST-PER (SUB-1).
            IF WS-FACTOR = 0
               MOVE 0 TO GL-PER-BU (SUB-1)
               GO TO GLM-016.
            IF WS-BU-ACT = "B"
                COMPUTE WS-BUDGET = GL-PER-BU (SUB-1) * WS-FACTOR
            ELSE
                COMPUTE WS-BUDGET = GL-PER (SUB-1) * WS-FACTOR.
            MOVE WS-BUDGET TO GL-PER-BU (SUB-1).
       GLM-016.
            MOVE 0 TO GL-PER (SUB-1).
            GO TO GLM-015.
       GLM-020.
            REWRITE GL-RECORD
                INVALID KEY
                MOVE "GLMASTER RECORD:"  TO WS-DAILY-1ST
                MOVE GL-NUMBER           TO WS-DAILY-2ND
                MOVE "NOT ROLLED!!"      TO WS-DAILY-3RD
                MOVE "THIS MONTH END"    TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
            GO TO GLM-010.
       GLM-900.
           IF WS-MONTH-YEAR = "Y"
              MOVE 1710 TO POS
              DISPLAY " 4. GlMaster Files Process Complete.    " AT POS
           ELSE
              MOVE 1510 TO POS
              DISPLAY " 2. GlMaster Files Process Complete.    " AT POS.
           MOVE 2910 TO POS.
           DISPLAY "                                                   "
               AT POS.
           CLOSE GL-MASTER.
       GLM-999.
             EXIT.
      *
       GLPARAMETERS SECTION.
       PAR-000.
            ACCEPT WS-TIME FROM TIME
            MOVE WS-HR TO SPLIT-HR
            MOVE ":"   TO SPLIT-FIL1
            MOVE WS-MIN TO SPLIT-MN
            MOVE ":"   TO SPLIT-FIL2
            MOVE WS-SEC TO SPLIT-SC
            IF WS-MONTH-YEAR = "M"
                MOVE " 3. GlParameter file  " TO WS-DAILY-1ST
            ELSE
                MOVE " 5. GlParameter file  " TO WS-DAILY-1ST.
            MOVE "Start of process    "   TO WS-DAILY-2ND
            MOVE SPLIT-TIME               TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
       PAR-005.
            IF WS-MONTH-YEAR = "M"
                MOVE 1610 TO POS
                DISPLAY " 3. GlParameter File Being Processed." AT POS
            ELSE
                MOVE 1810 TO POS
                DISPLAY " 6. GlParameter File Being Processed." AT POS.
            PERFORM OPEN-000.
            MOVE 1 TO GLPA-RECORD.
            READ GLPARAMETER-FILE WITH LOCK
              INVALID KEY
              MOVE "GLPARAMETER RECORD NOT FOUND!!!" TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO PAR-999.
       PAR-010.
            ADD 1 TO GLPA-CURRENT-GLPER.
            MOVE "N" TO GLPA-RECJRN-POST.
       PAR-020.
            IF GLPA-CURRENT-GLPER > 12
                MOVE 1 TO GLPA-CURRENT-GLPER.
            REWRITE GLPARAMETER-REC
                INVALID KEY
                MOVE "GLPARAMETER RECORD" TO WS-DAILY-1ST
                MOVE "NOT UPDATED!!!"     TO WS-DAILY-2ND
                MOVE " "                  TO WS-DAILY-3RD
                MOVE " "                  TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
            CLOSE GLPARAMETER-FILE.
        PAR-900.
           IF WS-MONTH-YEAR = "M"
               MOVE 1610 TO POS
              DISPLAY " 3. GlParameter File Process Complete." AT POS
           ELSE
               MOVE 1810 TO POS
              DISPLAY " 5. GlParameter File Process Complete." AT POS.
           MOVE 2910 TO POS
           DISPLAY "                                                   "
               AT POS.
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
            IF WS-MONTH-YEAR = "M"
                MOVE " 4. GlTrans-Future  " TO WS-DAILY-1ST
            ELSE
                MOVE " 6. GlTrans-Future  " TO WS-DAILY-1ST.
            MOVE "Start of process    "     TO WS-DAILY-2ND
            MOVE SPLIT-TIME                 TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
       CH-FU-005.
           PERFORM OPEN-015.
           PERFORM OPEN-025.
           IF WS-MONTH-YEAR = "M"
             MOVE 1710 TO POS
             DISPLAY " 4. GlTrans Future File Being Processed.  "
                  AT POS
           ELSE
             MOVE 1910 TO POS
            DISPLAY " 6. GlTrans Future File Being Processed.  "
                  AT POS.
           MOVE GLPA-CURRENT-GLPER TO WS-GLTRANS-NO.
       CH-FU-006.
           MOVE WS-GLTRANS-NO TO GLTRANS-NO.
           START GLTRANS-FILE KEY NOT < GLTRANS-NO
               INVALID KEY NEXT SENTENCE.
           IF WS-GLTRANS-ST1 NOT = 0
               MOVE "NO FUTURE TRANSACTIONS TO POST, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO CH-FU-900.
       CH-FU-010.
           READ GLTRANS-FILE NEXT WITH LOCK
               AT END
               GO TO CH-FU-900.
               
           MOVE 2710 TO POS
           DISPLAY "GlTrans Being Processed:" AT POS
           ADD 26 TO POS
           DISPLAY GLTRANS-REFERENCE AT POS.
           
           IF GLTRANS-NO NOT = WS-GLTRANS-NO
               GO TO CH-FU-900.
           IF GLTRANS-FUTURE NOT = "F"
               GO TO CH-FU-010.
           IF GLTRANS-FUTURE = "F"
            IF GLTRANS-NO NOT = GLPA-CURRENT-GLPER
              GO TO CH-FU-010.
              
      *ADDED NEXT THREE LINES DUE TO BLANK ACCOUNT NUMBERS BEING POSTED
      *FROM THE CREDITOR SIDE. 30/3/2016 NEW LINUX ERRORS :(
           IF GLTRANS-ACCOUNT-NUMBER NOT > " "
              PERFORM CH-FU-850
              GO TO CH-FU-010.
           
           ADD 1 TO WS-POSTED
           MOVE WS-POSTED TO WS-POSTED-DIS
           MOVE 2510 TO POS
           DISPLAY "    No. Of Trans Posted:" AT POS
           ADD 26 TO POS
           DISPLAY WS-POSTED-DIS AT POS
           ADD 10 TO POS
           DISPLAY "Trans Batch #:" AT POS
           ADD 15 TO POS
           DISPLAY GLTRANS-KEY AT POS
           PERFORM UPDATE-GLMASTER-FILES.
       CH-FU-800.
           MOVE " " TO GLTRANS-FUTURE.
           REWRITE GLTRANS-REC
               INVALID KEY
                MOVE "GLTRANS RECORD NOT  "  TO WS-DAILY-1ST
                MOVE "REWRITTEN, NUMBER : "  TO WS-DAILY-2ND
                MOVE GLTRANS-REFERENCE       TO WS-DAILY-3RD
                MOVE " "                     TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           GO TO CH-FU-010.
       CH-FU-850.
           DELETE GLTRANS-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLTRANS-ST1 NOT = 0
               MOVE "GLTRANS BUSY ON DELETE, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO CH-FU-850.
       CH-FU-900.
           CLOSE GLTRANS-FILE.
           CLOSE GL-MASTER.
           IF WS-MONTH-YEAR = "M"
            MOVE 1710 TO POS
               DISPLAY " 4. GlTrans Future Process Complete.           "
                   AT POS
           ELSE
            MOVE 1910 TO POS
               DISPLAY " 6. GlTrans Future Process Complete.           "
                   AT POS.
           
           PERFORM ERROR1-020
           PERFORM ERROR-020
           MOVE 2501 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2701 TO POS
           DISPLAY WS-MESSAGE AT POS.
       CH-FU-999.
            EXIT.
      *
       UPDATE-GLMASTER-FILES SECTION.
       UPGL-000.
           MOVE 2610 TO POS
           DISPLAY "UPDATING GL-MASTER FILES..." AT POS.
       UPGL-005.
           MOVE GLTRANS-ACCOUNT-NUMBER TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY
              INVALID KEY NEXT SENTENCE.
       UPGL-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLMASTER FILE DOES NOT EXIST, 'ESC' TO SEE STATUS"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE GL-NUMBER TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE GLTRANS-KEY TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO UPGL-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLMASTER RECORD BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGL-010.
           ADD GLTRANS-AMOUNT TO GL-BALANCE
           ADD GLTRANS-AMOUNT TO GL-PER (WS-GLTRANS-NO).
       UPGL-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGL-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLMASTER RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGL-900.
       UPGL-950.
           PERFORM UPDATE-GLHEADER
           PERFORM UPDATE-GLSUBHEADER.
       UPGL-960.
           MOVE 2610 TO POS.
           DISPLAY "                                                  "
              AT POS.
       UPGL-999.
           EXIT.
     *
       UPDATE-GLHEADER SECTION.
       UPGLH-005.
           MOVE GLTRANS-ACCOUNT-NUMBER TO WS-GLNUMBER.
           MOVE WS-GLHEADER            TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY
               INVALID KEY NEXT SENTENCE.
       UPGLH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLHEADER FILE DOESN'T EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE GL-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
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
           ADD GLTRANS-AMOUNT TO GL-BALANCE.
           ADD GLTRANS-AMOUNT TO GL-PER (WS-GLTRANS-NO).
       UPGLH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLHEADER RECORD BUSY ON WRITE, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGLH-900.
       UPGLH-999.
           EXIT.
     *
       UPDATE-GLSUBHEADER SECTION.
       UPGLSH-005.
           MOVE GLTRANS-ACCOUNT-NUMBER TO WS-GLNUMBER.
           MOVE WS-HEAD-SUB            TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY
               INVALID KEY NEXT SENTENCE.
       UPGLSH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLSUBHEAD FILE DOESN'T EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE GL-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLSH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLSUBHEADER FILE BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGLSH-010.
           ADD GLTRANS-AMOUNT TO GL-BALANCE.
           ADD GLTRANS-AMOUNT TO GL-PER (WS-GLTRANS-NO).
       UPGLSH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLSH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLSUBHEADER FILE BUSY ON WRITE, 'ESC' TO RETRY"
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
       CHECK-DATA-SIZE SECTION.         
       CDS-005.
           Move " " To Alpha-Rate.
           Move 0   To Sub-1.
       CDS-010.
           If Ws-Record-To-Delete = 1
               Move WS-GLMasterLy To Alpha-Rate
           Else
               Move WS-GLTransLy  To Alpha-Rate.
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
           Move "d" to Dat-Rate (Sub-2).
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
               Move Data-Rate to WS-GLMasterLyInd
           Else
               Move Data-Rate to WS-GLTransLyInd.
       CDS-999.
          EXIT.
      *
       DELETE-TRANS SECTION.
       DST-010.
           PERFORM CHECK-DATA-SIZE.
           
           If Ws-Record-To-Delete = 1
             MOVE WS-GLMasterLy TO F-FILENAME
           Else
             MOVE WS-GLTransLy  TO F-FILENAME.
           MOVE SUB-1           TO F-CBFILENAME.
           
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
              DISPLAY F-FILENAME AT POS
              PERFORM ERROR-010.
              
           PERFORM CDS-020 THRU CDS-900.
           
           If Ws-Record-To-Delete = 1
               MOVE WS-GLMasterLyInd TO F-FILENAME
           Else
               MOVE WS-GLTransLyInd  TO F-FILENAME.
           MOVE Sub-1                TO F-CBFILENAME.
           
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
              DISPLAY F-FILENAME AT POS
              PERFORM ERROR-010.
       DST-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O GLPARAMETER-FILE.
            IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE "GLPARAMETER FILE BUSY ON OPEN,  'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-000.
       OPEN-005.
            OPEN I-O GLJRN-FILE.
            IF WS-GLJRN-ST1 NOT = 0 
              MOVE "GLJRN FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLJRN-ST1
              GO TO OPEN-005.
       OPEN-010.
            OPEN I-O GL-LY-MASTER.
            IF WS-GL-LY-ST1 NOT = 0
              MOVE "GL-MASTERLY BUSY ON OPEN I-O, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GL-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GL-LY-ST1
              GO TO OPEN-010.
       OPEN-0100.
            GO TO OPEN-015.
       OPEN-011.
            OPEN OUTPUT GL-LY-MASTER.
            IF WS-GL-LY-ST1 NOT = 0
              MOVE "GL-MASTERLY BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GL-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GL-LY-ST1
              GO TO OPEN-011.
       OPEN-015.
            OPEN I-O GL-MASTER.
            IF WS-GLMAST-ST1 NOT = 0
              MOVE "GL-MASTER BUSY ON OPEN I-O, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO OPEN-015.
       OPEN-020.
            OPEN I-O GLTRANS-LY-FILE.
            IF WS-GLTRANS-LY-ST1 NOT = 0
              MOVE "GL-TRANSLY BUSY ON OPEN I-O, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
              GO TO OPEN-020.
       OPEN-0200.
            GO TO OPEN-025.
       OPEN-021.
            OPEN OUTPUT GLTRANS-LY-FILE.
            IF WS-GLTRANS-LY-ST1 NOT = 0
              MOVE "GL-TRANSLY BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
              GO TO OPEN-020.
       OPEN-025.
            OPEN I-O GLTRANS-FILE.
            IF WS-GLTRANS-ST1 NOT = 0
              MOVE "GL-TRANSLY BUSY ON OPEN I-O, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO OPEN-025.
       OPEN-999.
            EXIT.
      *
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "NumberCheck".
       Copy "GetSystemY2KDate".
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
       Copy "WriteDailyExcep1".
       Copy "CTOSCobolAccept".
      * END-OF-JOB
