        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrAgeTmp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectCrMaster".
           Copy "SelectCrTrans".
           Copy "SelectGlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCrTrans.
           COPY ChlfdGlParam.
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-AMT-OF-INVOICE    PIC S9(8)V99 VALUE 0.
       77  WS-TOT-BALANCE       PIC S9(8)V99 VALUE 0.
       77  WS-TOT-CURRENT       PIC S9(8)V99 VALUE 0.
       77  WS-TOT-30DAY         PIC S9(8)V99 VALUE 0.
       77  WS-TOT-60DAY         PIC S9(8)V99 VALUE 0.
       77  WS-TOT-90DAY         PIC S9(8)V99 VALUE 0.
       77  WS-TOT-120DAY        PIC S9(8)V99 VALUE 0.
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1     PIC 99.
       01  WS-CRTRANS-STATUS.
           03  WS-CRTRANS-ST1      PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
       01  WS-CRREF.
           03  WS-CRREF5          PIC X(5).
           03  WS-REFREST         PIC X(5).
       01  WS-TYPES.
           03  FILLER           PIC X(7) VALUE "INVOICE".
           03  FILLER           PIC X(7) VALUE "PAYMENT".
           03  FILLER           PIC X(7) VALUE "PAYMENT".
           03  FILLER           PIC X(7) VALUE "JRN.DR.".
           03  FILLER           PIC X(7) VALUE "JRN.CR.".
           03  FILLER           PIC X(7) VALUE "C/NOTE ".
           03  FILLER           PIC X(7) VALUE "INTREST".
           03  FILLER           PIC X(7) VALUE "DISCNT.".
           03  FILLER           PIC X(7) VALUE "FOREX  ".
       01  WS-TYPES-RED REDEFINES WS-TYPES.
           03  WS-TYPE-DESC     PIC X(7) OCCURS 9.
       01  WS-TEMP-LINE.
           03  FILLER           PIC X(5) VALUE " ".
           03  T-PERIOD         PIC X(10) VALUE " ".
           03  T-TRANS-DATE     PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
           03  T-TRANS-DUEDATE  PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 315 TO POS
           DISPLAY "**Creditors Reset Of Period Balances**" AT POS
           MOVE 415 TO POS
           DISPLAY "**************************************" AT POS
           PERFORM ERROR-020.
       CONTROL-020.
           MOVE 1420 TO POS.
           DISPLAY "<RETURN> TO CONTINUE, <END> TO EXIT." AT POS.
           MOVE 1465 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 64        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
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
           MOVE 2610 TO POS.
           DISPLAY "PROCESS BEING RUN........" AT POS.
       CONTROL-040.
           PERFORM OPEN-FILES.
           PERFORM READ-CREDITOR-LOCK.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       READ-CREDITOR-LOCK SECTION.
       RCL-001.
           PERFORM SUB-020.
           MOVE 0 TO CR-KEY.
           START CREDITOR-MASTER KEY NOT < CR-KEY
              INVALID KEY NEXT SENTENCE.
       RCL-010.
           READ CREDITOR-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 10
               GO TO RCL-999.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR RECORD BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RCL-010.
               
           MOVE 2310 TO POS
           DISPLAY "Creditor Master Being Processed:" AT POS
           ADD 33 TO POS
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
           
           PERFORM PRINT-ROUTINE.
           GO TO RCL-010.
       RCL-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           MOVE CR-ACCOUNT-NUMBER TO CRTR-ACC-NUMBER
           MOVE 0                 TO CRTR-DATE.
           START CRTR-FILE KEY NOT < CRTR-ACC-DATE
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
               PERFORM SUBTOTALS
               GO TO PR-999.
       PR-002.
           READ CRTR-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 10
               PERFORM SUBTOTALS
               GO TO PR-999.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE "CR-TRANS FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PR-002.
           IF WS-MESSAGE NOT = " "
               MOVE " " TO WS-MESSAGE
               PERFORM ERROR-020.
           IF CRTR-ACC-NUMBER NOT = CR-ACCOUNT-NUMBER
               PERFORM SUBTOTALS
               GO TO PR-999.
           IF CRTR-UNAPPLIED-AMT = 0
               GO TO PR-002.
       PR-020.
           MOVE CRTR-DATE                TO WS-AGE-DATE
           MOVE CRTR-UNAPPLIED-AMT       TO WS-AMT-OF-INVOICE
           PERFORM COMPUTE-DATE-PERIOD.
       
      * USED TO CHECK DATE OF TRANS & PERIOD - PROGRAM DEBUGGING
      *     MOVE WS-CALC-PERIOD TO T-PERIOD
      *     MOVE CRTR-DATE      TO T-TRANS-DATE
      *     MOVE WS-TEMP-LINE   TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

           IF WS-CALC-PERIOD = 0
               ADD WS-AMT-OF-INVOICE TO WS-TOT-CURRENT
                                        WS-TOT-BALANCE
               GO TO PR-030.
           IF WS-CALC-PERIOD = 1
               ADD WS-AMT-OF-INVOICE TO WS-TOT-30DAY
                                        WS-TOT-BALANCE
               GO TO PR-030.
           IF WS-CALC-PERIOD = 2
               ADD WS-AMT-OF-INVOICE TO WS-TOT-60DAY
                                        WS-TOT-BALANCE
               GO TO PR-030.
           IF WS-CALC-PERIOD = 3
               ADD WS-AMT-OF-INVOICE TO WS-TOT-90DAY
                                        WS-TOT-BALANCE
               GO TO PR-030.
           IF WS-CALC-PERIOD > 3
               ADD WS-AMT-OF-INVOICE TO WS-TOT-120DAY
                                        WS-TOT-BALANCE.
       PR-030.
           GO TO PR-002.
       PR-999.
           EXIT.
      *
       SUBTOTALS SECTION.
       SUB-000.
           MOVE WS-TOT-BALANCE TO CR-BALANCE
           MOVE WS-TOT-CURRENT TO CR-CURRENT
           MOVE WS-TOT-30DAY   TO CR-30DAY
           MOVE WS-TOT-60DAY   TO CR-60DAY
           MOVE WS-TOT-90DAY   TO CR-90DAY
           MOVE WS-TOT-120DAY  TO CR-120DAY.
       SUB-010.
           REWRITE CREDITOR-RECORD.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE "CREDITORS BUSY ON RE-WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO SUB-010.
       SUB-020.
           MOVE 0 TO WS-TOT-BALANCE
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
               MOVE "GLPARAMETER BUSY ON READ, RP-010, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RP-010.
       RP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT SPLIT-DATE FROM DATE.
      *     MOVE SPLIT-DATE TO WS-DATE.
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
       OPEN-020.
           OPEN I-O CREDITOR-MASTER.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CR MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-020.
       OPEN-030.
           OPEN I-O CRTR-FILE.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE "CRTRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-030.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE CRTR-FILE
                  GLPARAMETER-FILE
                  CREDITOR-MASTER.
       END-900.
      *     STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ComputeCRDatePeriod".
       Copy "EnterPeriodDates".
       Copy "GetSystemY2KDate".
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
      * END-OF-JOB
