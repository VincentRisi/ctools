        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrAgeTmp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrTrans".
         Copy "SelectGlParameter".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdDrTrans.
           COPY ChlfdGlParam.
      *
       WORKING-STORAGE SECTION.
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-LAST              PIC X VALUE "N".
       77  WS-AMT-OF-INVOICE    PIC S9(8)V99 VALUE 0.
       77  WS-AMT-OUTSTANDING   PIC S9(8)V99 VALUE 0.
       77  WS-BAL-LAST-MONTH    PIC S9(8)V99 VALUE 0.
       77  WS-TOT-BALANCE       PIC S9(8)V99 VALUE 0.
       77  WS-TOT-CURRENT       PIC S9(8)V99 VALUE 0.
       77  WS-TOT-30DAY         PIC S9(8)V99 VALUE 0.
       77  WS-TOT-60DAY         PIC S9(8)V99 VALUE 0.
       77  WS-TOT-90DAY         PIC S9(8)V99 VALUE 0.
       77  WS-TOT-120DAY        PIC S9(8)V99 VALUE 0.
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1      PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1     PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  WS-CRREF.
           03  WS-CRREF5        PIC X(5).
           03  WS-REFREST       PIC X(5).
       01  WS-TYPES.
           03  FILLER           PIC X(7) VALUE "INVOICE".
           03  FILLER           PIC X(7) VALUE "PAYMENT".
           03  FILLER           PIC X(7) VALUE "R/D CHQ".
           03  FILLER           PIC X(7) VALUE "JRN.DR.".
           03  FILLER           PIC X(7) VALUE "JRN.CR.".
           03  FILLER           PIC X(7) VALUE "C/NOTE ".
           03  FILLER           PIC X(7) VALUE "INTREST".
           03  FILLER           PIC X(7) VALUE "DISCNT.".
           03  FILLER           PIC X(7) VALUE "B/DEBT ".
           03  FILLER           PIC X(7) VALUE "CH REF.".
           03  FILLER           PIC X(7) VALUE "INT REV".
       01  WS-TYPES-RED REDEFINES WS-TYPES.
           03  WS-TYPE-DESC     PIC X(7) OCCURS 11.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 315 TO POS.
           DISPLAY "** Debtors Date Reset Of Period Balances **" AT POS.
           MOVE 415 TO POS.
           DISPLAY "*******************************************" AT POS.
           PERFORM ERROR-020.
       CONTROL-010.
           MOVE 1420 TO POS.
           DISPLAY "CALCULATE BAL-LAST-STATEMENT : [ ]" AT POS.
           MOVE 1452 TO POS.
           MOVE " "  TO CDA-DATA
           MOVE 11   TO CDA-ROW
           MOVE 51   TO CDA-COL
           MOVE 1    TO CDA-DATALEN
           MOVE "A"  TO CDA-ATTR
           MOVE 3    TO CDA-COLOR
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-LAST.

           IF W-ESCAPE-KEY = 3
               PERFORM END-900.
            IF WS-LAST NOT = "N" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-020
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-020.
           MOVE 1820 TO POS.
           DISPLAY "<RETURN> TO CONTINUE, <END> TO EXIT." AT POS.
           MOVE 1870 TO POS.
           MOVE " "  TO CDA-DATA
           MOVE 15   TO CDA-ROW
           MOVE 70   TO CDA-COL
           MOVE 1    TO CDA-DATALEN
           MOVE "A"  TO CDA-ATTR
           MOVE 3    TO CDA-COLOR
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY = 3
               PERFORM END-900.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-030.
           MOVE 1620 TO POS.
           DISPLAY "                                          " AT POS.
           MOVE 2810 TO POS.
           DISPLAY "PROCESS BEING RUN........" AT POS.
       CONTROL-040.
           PERFORM OPEN-FILES.
           PERFORM READ-DEBTOR-LOCK.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       READ-DEBTOR-LOCK SECTION.
       RCL-001.
           PERFORM SUB-020.
           MOVE 0 TO DR-KEY.
           START DEBTOR-MASTER KEY NOT < DR-KEY.
       RCL-010.
           READ DEBTOR-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
               GO TO RCL-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RCL-010.
           PERFORM PRINT-ROUTINE.

           GO TO RCL-010.
       RCL-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           MOVE DR-ACCOUNT-NUMBER TO DRTR-ACCOUNT-NUMBER.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACCOUNT-NUMBER
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
               PERFORM SUBTOTALS
               GO TO PR-999.
       PR-002.
           READ DEBTOR-TRANS-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 10
               PERFORM SUBTOTALS
               GO TO PR-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DRTRANS BUSY ON START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO PR-002.
           IF WS-MESSAGE NOT = " "
               MOVE " " TO WS-MESSAGE
               PERFORM ERROR-020.
           IF DRTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
               PERFORM SUBTOTALS
               GO TO PR-999.
               
      *NEW SECTION TO CALCULATE THE DR-BAL-LAST-STATE
      *INCASE THIS IS WRONG DUE TO 'FINGER' TROUBLE. 22/4/2002
           MOVE DRTR-DATE                TO WS-AGE-DATE.
           IF DRTR-TYPE = 2 OR = 5 OR = 6 OR = 8 OR = 9 OR = 11
            IF WS-AGE-MM = SPLIT-MM
             IF WS-AGE-YY = SPLIT-YY
               ADD DRTR-AMT-OF-INVOICE         TO WS-BAL-LAST-MONTH
               SUBTRACT DRTR-AMT-OUTSTANDING FROM WS-BAL-LAST-MONTH.
               
           IF DRTR-TYPE = 1 OR = 3 OR = 4 OR = 7 OR = 10
            IF WS-AGE-MM = SPLIT-MM
             IF WS-AGE-YY = SPLIT-YY
               SUBTRACT DRTR-AMT-OF-INVOICE  FROM WS-BAL-LAST-MONTH
               ADD      DRTR-AMT-OUTSTANDING TO   WS-BAL-LAST-MONTH.
      *         ADD      DRTR-AMT-OF-INVOICE  TO WS-BAL-LAST-MONTH
      *         SUBTRACT DRTR-AMT-OUTSTANDING FROM WS-BAL-LAST-MONTH.
               
               
           IF DRTR-AMT-OUTSTANDING = 0
               GO TO PR-002.
       PR-005.
           MOVE 2510 TO POS.
           DISPLAY "Account Number Being Processed:" AT POS.
           ADD 32 TO POS.
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
       PR-020.
           MOVE DRTR-DATE                TO WS-AGE-DATE
           MOVE DRTR-AMT-OF-INVOICE      TO WS-AMT-OF-INVOICE
           MOVE DRTR-AMT-OUTSTANDING     TO WS-AMT-OUTSTANDING.
           IF DRTR-TYPE = 2 OR = 5 OR = 6 OR = 8 OR = 9
              COMPUTE WS-AMT-OUTSTANDING = WS-AMT-OUTSTANDING * -1.
           
           
      *NEW SECTION TO CALCULATE THE DR-BAL-LAST-STATE
      *INCASE THIS IS WRONG DUE TO 'FINGER' TROUBLE. 22/4/2002
      *     IF WS-AGE-MM < SPLIT-MM
      *      IF WS-AGE-YY = SPLIT-YY
      *       ADD WS-AMT-OF-INVOICE TO WS-BAL-LAST-MONTH.
      *      IF WS-AGE-YY < SPLIT-YY
      *       ADD WS-AMT-OF-INVOICE TO WS-BAL-LAST-MONTH.

           PERFORM COMPUTE-DATE-PERIOD-RESET.

      *     MOVE WS-CALC-PERIOD TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

           IF WS-CALC-PERIOD = 0
               ADD WS-AMT-OUTSTANDING TO WS-TOT-CURRENT
                                         WS-TOT-BALANCE
               GO TO PR-030.
           IF WS-CALC-PERIOD = 1
               ADD WS-AMT-OUTSTANDING TO WS-TOT-30DAY
                                         WS-TOT-BALANCE
               GO TO PR-030.
          IF WS-CALC-PERIOD = 2
               ADD WS-AMT-OUTSTANDING TO WS-TOT-60DAY
                                         WS-TOT-BALANCE
               GO TO PR-030.
          IF WS-CALC-PERIOD = 3
               ADD WS-AMT-OUTSTANDING TO WS-TOT-90DAY
                                         WS-TOT-BALANCE
               GO TO PR-030.
          IF WS-CALC-PERIOD > 3
               ADD WS-AMT-OUTSTANDING TO WS-TOT-120DAY
                                         WS-TOT-BALANCE.
       PR-030.
           GO TO PR-002.
       PR-999.
           EXIT.
      *
       SUBTOTALS SECTION.
       SUB-000.
           IF WS-LAST = "Y"
             COMPUTE DR-BAL-LAST-STATE = (WS-BAL-LAST-MONTH 
                      + WS-TOT-BALANCE) - WS-TOT-CURRENT.
      *        MOVE WS-BAL-LAST-MONTH TO DR-BAL-LAST-STATE.
           MOVE WS-TOT-BALANCE       TO DR-BALANCE
           MOVE WS-TOT-CURRENT       TO DR-CURRENT
           MOVE WS-TOT-30DAY         TO DR-30DAY
           MOVE WS-TOT-60DAY         TO DR-60DAY
           MOVE WS-TOT-90DAY         TO DR-90DAY
           MOVE WS-TOT-120DAY        TO DR-120DAY.
      *
      *SECTION ADDED JUST FOR OPENING BALANCES IN SAFTEC TO BE WRITTEN
      *TO THE BAL-LAT-STATE. SEP 2001.
      *
      *     COMPUTE DR-BAL-LAST-STATE = DR-BALANCE - DR-CURRENT.
       SUB-010.
           REWRITE DEBTOR-RECORD.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTORS BUSY ON RE-WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO SUB-010.
       SUB-020.
           MOVE 0 TO WS-BAL-LAST-MONTH
                     WS-TOT-BALANCE
                     WS-TOT-CURRENT
                     WS-TOT-30DAY
                     WS-TOT-60DAY
                     WS-TOT-90DAY
                     WS-TOT-120DAY.
       SUB-999.
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
               MOVE "GLPARAMETER BUSY ON READ RP-010, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RP-010.
       RP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-012.

           PERFORM READ-GLPARAMETER.
           PERFORM ENTER-PERIOD-DATES.
      *     MOVE 1 TO SUB-1.
       OPEN-013.
      *     MOVE 2710 TO POS
      *     DISPLAY GL-BEGDATE (SUB-1) AT POS
      *     ADD 12 TO POS
      *     DISPLAY GL-ENDDATE (SUB-1) AT POS.
      *     ADD 12 TO POS
      *     ACCEPT WS-ACCEPT AT POS
      *     IF SUB-1 < 12
      *        ADD 1 TO SUB-1
      *        GO TO OPEN-013.
       OPEN-020.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DR-MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-020.
       OPEN-030.
           OPEN I-O DEBTOR-TRANS-FILE.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-DRTRANS-ST1
               MOVE "DRTRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-030.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE DEBTOR-TRANS-FILE
                  GLPARAMETER-FILE
                  DEBTOR-MASTER.
       END-900.
      *     STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ComputeDatePeriodReset".
       Copy "EnterPeriodDates".
       Copy "GetSystemY2KDate".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
