        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrPerEnd.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectCrMaster".
           Copy "SelectCrTrans".
           Copy "SelectCrJrn".
           Copy "SelectGlParameter".
           Copy "SelectSlDaily".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCrTrans.
           COPY ChlfdCrJrn.
           COPY ChlfdGlParam.
           COPY ChlfdDaily.
      *
       WORKING-STORAGE SECTION.
       77  WS-STOP-UPDATE       PIC X VALUE " ".
       77  WS-BUDGET            PIC S9(3)V99 VALUE 0.
       77  WS-ANSWER1           PIC X VALUE " ".
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
           03  WS-DAILY-1ST     PIC X(20) VALUE " ".
           03  WS-DAILY-2ND     PIC X(20) VALUE " ".
           03  WS-DAILY-3RD     PIC X(20) VALUE " ".
           03  WS-DAILY-4TH.
              05  WS-DAILY-4TH-1   PIC X(10) VALUE " ".
              05  WS-DAILY-4TH-2   PIC X(10) VALUE " ".
       01  SPLIT-DIS-DATE.
           03  SPLIT-DIS-DD         PIC 99.
           03  SPLIT-DIS-FILD1      PIC X.
           03  SPLIT-DIS-MM         PIC 99.
           03  SPLIT-DIS-FILD2      PIC X.
           03  SPLIT-DIS-YY         PIC 9999.
       01  SPLIT-TIME.
           03  SPLIT-HR         PIC 99.
           03  SPLIT-FIL1       PIC X.
           03  SPLIT-MN         PIC 99.
           03  SPLIT-FIL2       PIC X.
           03  SPLIT-SC         PIC 99.
       01  WS-CRTRANS-STATUS.
           03  WS-CRTRANS-ST1     PIC 99.
       01  WS-CRJRN-STATUS.
           03  WS-CRJRN-ST1       PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1       PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1    PIC 99.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 0315 TO POS.
           DISPLAY "* CREDITORS MONTH, & YEAR END PROCESSING *" AT POS.
           MOVE 0415 TO POS.
           DISPLAY "******************************************" AT POS.
           PERFORM OPEN-000 THRU OPEN-002.
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
               MOVE 3010 TO POS
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
               MOVE 855 TO POS

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
           PERFORM CHECK-CRPERIODS
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-YY  TO SPLIT-DIS-YY
           MOVE ":"    TO SPLIT-DIS-FILD1
           MOVE WS-MM  TO SPLIT-DIS-MM
           MOVE ":"    TO SPLIT-DIS-FILD2
           MOVE WS-DD  TO SPLIT-DIS-DD.
           ACCEPT WS-TIME FROM TIME
           MOVE WS-HR  TO SPLIT-HR
           MOVE ":"    TO SPLIT-FIL1
           MOVE WS-MIN TO SPLIT-MN
           MOVE ":"    TO SPLIT-FIL2
           MOVE WS-SEC TO SPLIT-SC
           MOVE "C/Ledger Period End:" TO WS-DAILY-1ST
           MOVE GLPA-CURRENT-CRPER     TO WS-DAILY-2ND
           MOVE "Start of process    " TO WS-DAILY-3RD
           MOVE SPLIT-DIS-DATE         TO WS-DAILY-4TH-1
           MOVE SPLIT-TIME             TO WS-DAILY-4TH-2
           PERFORM WRITE-DAILY.
       CONTROL-055.
           PERFORM ERROR-020
           MOVE 1210 TO POS
           DISPLAY "There Are 4 Data Files To Process." AT POS
           PERFORM UPDATE-CREDITORS
           PERFORM DELETE-CRJRNS
           PERFORM DELETE-CRTRANS
           PERFORM GLPARAMETERS
           ACCEPT WS-TIME FROM TIME
           MOVE WS-HR  TO SPLIT-HR
           MOVE ":"    TO SPLIT-FIL1
           MOVE WS-MIN TO SPLIT-MN
           MOVE ":"    TO SPLIT-FIL2
           MOVE WS-SEC TO SPLIT-SC
           MOVE "All Files Processed " TO WS-DAILY-1ST
           MOVE "In Creditors Ledger " TO WS-DAILY-2ND
           MOVE SPLIT-TIME             TO WS-DAILY-3RD
           PERFORM WRITE-DAILY.
       CONTROL-600.
           MOVE 2510 TO POS.
           DISPLAY "We Are Finished, Run The Maintainance Submit File,"
              AT POS.
           MOVE 2610 TO POS.
           DISPLAY "Press 'NEXT' Or 'GO' To End The Program. " AT POS.
           MOVE 2675 TO POS.
           ACCEPT WS-ANSWER3 AT POS.
           IF W-ESCAPE-KEY = 1 OR 2
               GO TO CONTROL-900
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-600.
       CONTROL-900.
           EXIT PROGRAM.
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
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER BUSY ON READ, RP-000, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       CHECK-CRPERIODS SECTION.
       CH-CRPER-005.
            PERFORM OPEN-000.
            MOVE 1 TO GLPA-RECORD.
            READ GLPARAMETER-FILE
                INVALID KEY
                MOVE "GLPARAMETER RECORD NOT FOUND, 'ESC' TO EXIT."
                 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO CH-CRPER-999.
       CH-CRPER-010.
            IF WS-ANSWER1 = "M"
             IF GLPA-CURRENT-CRPER = 12
                MOVE 1510 TO POS
                DISPLAY " " AT 3079 WITH BELL
                DISPLAY "You should be running the YEAR END" AT POS
                MOVE 1610 TO POS
                DISPLAY "Press 'NEXT' Or 'GO' To End The Program."
                AT POS

               MOVE ' '       TO CDA-DATA
               MOVE 1         TO CDA-DATALEN
               MOVE 13        TO CDA-ROW
               MOVE 60        TO CDA-COL
               MOVE CDA-WHITE TO CDA-COLOR
               MOVE 'F'       TO CDA-ATTR
               PERFORM CTOS-ACCEPT
               MOVE CDA-DATA TO WS-ANSWER3

              IF W-ESCAPE-KEY = 1 OR 2
                CLOSE GLPARAMETER-FILE
                EXIT PROGRAM
              ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO CH-CRPER-010.
           IF WS-ANSWER1 = "Y"
            IF GLPA-CURRENT-CRPER NOT = 12
                MOVE 1510 TO POS
                DISPLAY " " AT 3079 WITH BELL
                DISPLAY "You should be Running the MONTH END" AT POS
                MOVE 1610 TO POS
                DISPLAY "Press 'NEXT' Or 'GO' To End The Program."
                AT POS

               MOVE ' '       TO CDA-DATA
               MOVE 1         TO CDA-DATALEN
               MOVE 13        TO CDA-ROW
               MOVE 60        TO CDA-COL
               MOVE CDA-WHITE TO CDA-COLOR
               MOVE 'F'       TO CDA-ATTR
               PERFORM CTOS-ACCEPT
               MOVE CDA-DATA TO WS-ANSWER3

              IF W-ESCAPE-KEY = 1 OR 2
                CLOSE GLPARAMETER-FILE
                EXIT PROGRAM
              ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO CH-CRPER-010.
        CH-CRPER-020.
            CLOSE GLPARAMETER-FILE.
        CH-CRPER-999.
            EXIT.
      *
       UPDATE-CREDITORS SECTION.
       UCR-000.
            MOVE " 1. Creditors File  " TO WS-DAILY-1ST
            MOVE "Start of process    " TO WS-DAILY-2ND
            MOVE SPLIT-TIME             TO WS-DAILY-3RD
            PERFORM WRITE-DAILY.
            MOVE 1510 TO POS.
            DISPLAY " 1. Creditors Files Being Processed." AT POS.
            
            PERFORM OPEN-005.
            MOVE 0 TO CR-KEY.
            START CREDITOR-MASTER KEY NOT < CR-KEY.
       UCR-010.
            READ CREDITOR-MASTER NEXT WITH LOCK
                 AT END
                 CLOSE CREDITOR-MASTER
                 GO TO UCR-900.
            MOVE 2910 TO POS.
            DISPLAY "Account No Being Processed :" AT POS.
            ADD 30 TO POS.
            DISPLAY CR-ACCOUNT-NUMBER AT POS.

            MOVE CR-BALANCE   TO CR-BAL-LAST-STATE.
            ADD  CR-90DAY     TO CR-120DAY.
            MOVE CR-60DAY     TO CR-90DAY.
            MOVE CR-30DAY     TO CR-60DAY.
            MOVE CR-CURRENT   TO CR-30DAY.
            MOVE 0            TO CR-CURRENT.
            MOVE 0            TO CR-PURCHASE-PTD.
              
            IF WS-ANSWER1 = "Y"
                 MOVE CR-PURCHASE-YTD  TO CR-PURCHASE-LAST
                 MOVE CR-FOR-PURCH-YTD TO CR-FOR-PURCH-LAST
                 MOVE 0                TO CR-PURCHASE-YTD
                                          CR-FOR-PURCH-YTD.
       UCR-020.
            REWRITE CREDITOR-RECORD
                INVALID KEY
                MOVE "CREDITORS RECORD:"  TO WS-DAILY-1ST
                MOVE CR-ACCOUNT-NUMBER    TO WS-DAILY-2ND
                MOVE "NOT ROLLED!!"       TO WS-DAILY-3RD
                MOVE "THIS MONTH END"     TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
            GO TO UCR-010.
       UCR-900.
           MOVE 1510 TO POS.
           DISPLAY " 1. Creditors Files Process Complete.      " AT POS.
           MOVE 2910 TO POS.
           DISPLAY "                                           " AT POS.
       UCR-999.
             EXIT.
      *
       DELETE-CRJRNS SECTION.
       DCRJ-000.
           IF WS-ANSWER1 = "Y"
            MOVE 0 TO GLPA-CURRENT-CRPER.
       
            ACCEPT WS-TIME FROM TIME.
            MOVE WS-HR  TO SPLIT-HR.
            MOVE ":"    TO SPLIT-FIL1.
            MOVE WS-MIN TO SPLIT-MN.
            MOVE ":"    TO SPLIT-FIL2.
            MOVE WS-SEC TO SPLIT-SC.
            MOVE " 2. CrJrns-File     " TO WS-DAILY-1ST.
            MOVE "Start of process    " TO WS-DAILY-2ND.
            MOVE SPLIT-TIME             TO WS-DAILY-3RD.
            PERFORM WRITE-DAILY.
            PERFORM OPEN-008.
           MOVE 1610 TO POS.
           DISPLAY " 2. CrJrns File Being Processed.          " AT POS.
           MOVE " " TO CRJRN-REFERENCE
           MOVE 0   TO CRJRN-TRANS
                       CRJRN-TYPE.
           START CRJRN-FILE KEY NOT < CRJRN-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "NO CRJRN RECORDS TO CHECK, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO DCRJ-900.
       DCRJ-010.
           READ CRJRN-FILE NEXT WITH LOCK
               AT END
               GO TO DCRJ-900.
           MOVE 2910 TO POS.
           DISPLAY "      CrJrn Being Processed:" AT POS.
           ADD 30 TO POS.
           DISPLAY CRJRN-REFERENCE AT POS.
           ADD 12 TO POS.
           DISPLAY CRJRN-TRANS AT POS.
           
           IF CRJRN-FUTURE = "F"
            IF CRJRN-NO = GLPA-CURRENT-CRPER + 1
              GO TO DCRJ-850.
           
           IF CRJRN-COMPLETE = "Y"
            IF CRJRN-UNAPPLIED-AMT = 0
               GO TO DCRJ-800.
           GO TO DCRJ-010.
       DCRJ-800.
           DELETE CRJRN-FILE
               INVALID KEY
                MOVE "CRJRN TRANS RECORD N"  TO WS-DAILY-1ST
                MOVE "OT DELETED, NUMBER :"  TO WS-DAILY-2ND
                MOVE CRJRN-REFERENCE         TO WS-DAILY-3RD
                MOVE CRJRN-TRANS             TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           GO TO DCRJ-010.
       DCRJ-850.
          MOVE " " TO CRJRN-FUTURE.
          REWRITE CRJRN-REC
               INVALID KEY
                MOVE "CRJRN TRANS RECORD N"  TO WS-DAILY-1ST
                MOVE "OT REWRITN, NUMBER :"  TO WS-DAILY-2ND
                MOVE CRJRN-REFERENCE         TO WS-DAILY-3RD
                MOVE CRJRN-TRANS             TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
                
           MOVE 2410 TO POS
           DISPLAY "CRJRN IN FUTURE PERIOD CHANGED" AT POS
           MOVE 2441 TO POS
           DISPLAY CRJRN-KEY AT POS.
           
           GO TO DCRJ-010.
       DCRJ-900.
           IF WS-ANSWER1 = "Y"
               MOVE 12 TO GLPA-CURRENT-CRPER.
           CLOSE CRJRN-FILE.
           MOVE 1610 TO POS.
           DISPLAY " 2. CrJrn File Process Complete.           " AT POS.
           PERFORM ERROR-020
           PERFORM ERROR1-020
           MOVE 2410 TO POS.
           DISPLAY WS-MESSAGE AT POS.
       DCRJ-999.
            EXIT.
      *
       DELETE-CRTRANS SECTION.
       DCRT-000.
            ACCEPT WS-TIME FROM TIME.
            MOVE WS-HR  TO SPLIT-HR.
            MOVE ":"    TO SPLIT-FIL1.
            MOVE WS-MIN TO SPLIT-MN.
            MOVE ":"    TO SPLIT-FIL2.
            MOVE WS-SEC TO SPLIT-SC.
            MOVE " 3. CrTrans-File    " TO WS-DAILY-1ST.
            MOVE "Start of process    " TO WS-DAILY-2ND.
            MOVE SPLIT-TIME             TO WS-DAILY-3RD.
            PERFORM WRITE-DAILY.
            PERFORM OPEN-010.
           MOVE 1710 TO POS.
           DISPLAY " 3. CrTrans File Being Processed.  " AT POS.
           MOVE 0   TO CRTR-TYPE
                       CRTR-TRANS.
           START CRTR-FILE KEY NOT < CRTR-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE "NO CR-TRANS RECORDS TO CHECK, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO DCRT-900.
       DCRT-010.
           READ CRTR-FILE NEXT WITH LOCK
               AT END
               GO TO DCRT-900.
           MOVE 2910 TO POS.
           DISPLAY "CrTrans Being Processed:" AT POS.
           ADD 26 TO POS.
           DISPLAY CRTR-REFERENCE AT POS.
           ADD 12 TO POS.
           DISPLAY CRTR-TRANS AT POS.

           IF CRTR-UNAPPLIED-AMT = 0
               GO TO DCRT-800.
           GO TO DCRT-010.
       DCRT-800.
           DELETE CRTR-FILE
               INVALID KEY
                MOVE "CRTRANS RECORD NOT  "  TO WS-DAILY-1ST
                MOVE "DELETED,   NUMBER : "  TO WS-DAILY-2ND
                MOVE CRTR-REFERENCE          TO WS-DAILY-3RD
                MOVE CRTR-TRANS              TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           GO TO DCRT-010.
       DCRT-900.
           CLOSE CRTR-FILE.
           MOVE 1710 TO POS.
           DISPLAY " 3. CrTrans Process Complete.              " AT POS.
           MOVE 2910 TO POS.
           DISPLAY "                                                   "
              AT POS.
       DCRT-999.
            EXIT.
      *
       GLPARAMETERS SECTION.
       PAR-000.
            ACCEPT WS-TIME FROM TIME.
            MOVE WS-HR  TO SPLIT-HR.
            MOVE ":"    TO SPLIT-FIL1.
            MOVE WS-MIN TO SPLIT-MN.
            MOVE ":"    TO SPLIT-FIL2.
            MOVE WS-SEC TO SPLIT-SC.
            MOVE " 4. GlParameter file  " TO WS-DAILY-1ST.
            MOVE "Start of process    "   TO WS-DAILY-2ND.
            MOVE SPLIT-TIME               TO WS-DAILY-3RD.
            PERFORM WRITE-DAILY.
            MOVE 1810 TO POS.
            DISPLAY " 4. GlParameter File Being Processed." AT POS.
       PAR-005.
            PERFORM OPEN-000.
            MOVE 1 TO GLPA-RECORD.
            READ GLPARAMETER-FILE WITH LOCK
                INVALID KEY
                MOVE "GLPARAMETER RECORD" TO WS-DAILY-1ST
                MOVE "NOT UPDATED!!!"     TO WS-DAILY-2ND
                MOVE "BECAUSE NOT FOUND"  TO WS-DAILY-3RD
                MOVE " "                  TO WS-DAILY-4TH
                PERFORM WRITE-DAILY
                MOVE "GLPARAMETER RECORD NOT FOUND!!!" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO PAR-999.
       PAR-010.
           ADD 1    TO GLPA-CURRENT-CRPER.
           MOVE "N" TO GLPA-CR-REMIT.
           IF GLPA-CURRENT-CRPER > 12
                MOVE 1 TO GLPA-CURRENT-CRPER.
           REWRITE GLPARAMETER-REC
                INVALID KEY
                MOVE "GLPARAMETER RECORD" TO WS-DAILY-1ST
                MOVE "NOT UPDATED!!!"     TO WS-DAILY-2ND
                MOVE " "                  TO WS-DAILY-3RD
                MOVE " "                  TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           CLOSE GLPARAMETER-FILE.
        PAR-900.
           MOVE 1810 TO POS.
           DISPLAY " 4. GlParameter File Process Complete." AT POS.
           MOVE 2910 TO POS.
           DISPLAY "                                          " AT POS.
        PAR-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O GLPARAMETER-FILE.
            IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-002.
            PERFORM READ-PARAMETER.
            PERFORM ENTER-PERIOD-DATES.
            PERFORM DISPLAY-CR-TOP-INFO.
            CLOSE GLPARAMETER-FILE.
       OPEN-005.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
                MOVE "CREDITORS BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO OPEN-005.
       OPEN-008.
           OPEN I-O CRJRN-FILE.
            IF WS-CRJRN-ST1 NOT = 0
                MOVE "CRJRN-FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO OPEN-008.
       OPEN-010.
           OPEN I-O CRTR-FILE.
            IF WS-CRTRANS-ST1 NOT = 0
                MOVE "CRTRANS-FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO OPEN-010.
       OPEN-999.
            EXIT.
      *
       Copy "EnterPeriodDates".
       Copy "GetSystemY2KDate".
       Copy "DisplayCrTopInfo".
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
       Copy "WriteDailyExcep1".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
