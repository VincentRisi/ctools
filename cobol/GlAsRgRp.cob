        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlAsRgRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlMaster".
        Copy "SelectGlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdGlMast.
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
       77  WS-BALANCE           PIC S9(8)V99 VALUE 0.
       77  WS-TOT-BALANCE       PIC S9(8)V99 VALUE 0.
       77  WS-TOT-DEPRECIATION  PIC S9(8)V99 VALUE 0.
       77  WS-MV-BALANCE        PIC S9(8)V99 VALUE 0.
       77  WS-MV-DEPRECIATION   PIC S9(8)V99 VALUE 0.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1      PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(30) VALUE " ".
           03  FILLER         PIC X(32) VALUE
           "GENERAL LEDGER ASSET REGISTER".
           03  FILLER         PIC X(10) VALUE " ".
           03  FILLER         PIC X(7) VALUE "PERIOD:".
           03  H1-PERIOD      PIC X(2).
           03  FILLER         PIC X(22) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(7) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(45) VALUE " ".
           03  FILLER         PIC X(29) VALUE ALL "*".
           03  FILLER         PIC X(58) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(15) VALUE "ACCOUNT".
           03  FILLER         PIC X(51) VALUE "DESCRIPTION".
           03  FILLER         PIC X(20) VALUE "PURCHASE PRICE".
           03  FILLER         PIC X(20) VALUE "DEPRECIATION".
           03  FILLER         PIC X(26) VALUE "BOOK VALUE".
       01  DETAIL-LINE.
           03  D-ACCOUNT      PIC X(15).
           03  D-DESC         PIC X(51).
           03  FILLER         PIC X(3) VALUE " ".
           03  D-BALANCE      PIC Z(7)9.99-.
           03  FILLER         PIC X(6) VALUE " ".
           03  D-DEPRECIATION PIC Z(7)9.99-.
           03  FILLER         PIC X(6) VALUE " ".
           03  D-BOOK         PIC Z(7)9.99-.
           03  FILLER         PIC X(15) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN
           MOVE 321 TO POS
           DISPLAY "** GLMASTER ASSET REGISTER REPORT **" AT POS
           MOVE 421 TO POS
           DISPLAY "************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-050.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE 2510 TO POS.
           DISPLAY "The Report is being compiled.........." AT POS.
       GET-999.
            EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
       RP-010.
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
              MOVE "GLPARAMETER BUSY ON READ, 'ESC' TO RETRY."
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
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE "75-010-00-00" TO GL-NUMBER.
            START GL-MASTER KEY NOT < GL-KEY
                INVALID KEY NEXT SENTENCE.
       PRR-002.
            READ GL-MASTER NEXT
               AT END NEXT SENTENCE.
            IF WS-GLMAST-ST1 = 10
               MOVE 0 TO WS-GLMAST-ST1
               GO TO PRR-999.
            IF WS-GLMAST-ST1 NOT = 0
              MOVE "GL-MASTER BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO PRR-002.
            IF GL-NUMBER > "75-011"
               GO TO PRR-999.
       PRR-010.
            IF LINE-CNT < 58
               GO TO PRR-050.
            ADD 1 TO PAGE-CNT.
            MOVE PAGE-CNT TO H1-PAGE.
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
            WRITE PRINT-REC FROM HEAD1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD2.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD3.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE 5 TO LINE-CNT.
       PRR-050.
           MOVE GL-NUMBER      TO D-ACCOUNT.
           MOVE GL-DESCRIPTION TO D-DESC.
           MOVE GL-BALANCE     TO D-BALANCE WS-BALANCE.
           ADD GL-BALANCE      TO WS-TOT-BALANCE.
           IF GL-NUMBER > "75-010-30"
              ADD GL-BALANCE   TO WS-MV-BALANCE.
       PRR-055.
            READ GL-MASTER NEXT
               AT END NEXT SENTENCE.
            IF WS-GLMAST-ST1 = 10
               MOVE 0 TO WS-GLMAST-ST1
               GO TO PRR-999.
            IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               GO TO PRR-055.
           MOVE GL-BALANCE    TO  D-DEPRECIATION.
           ADD GL-BALANCE     TO WS-TOT-DEPRECIATION.
           IF GL-NUMBER > "75-010-30"
              ADD GL-BALANCE  TO WS-MV-DEPRECIATION.
           COMPUTE WS-BALANCE = WS-BALANCE + GL-BALANCE.
           MOVE WS-BALANCE    TO D-BOOK.

           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC DETAIL-LINE.
           ADD 1 TO LINE-CNT.
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO OPEN-000.
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
           MOVE GLPA-NAME TO CO-NAME.
           MOVE GLPA-CURRENT-GLPER TO H1-PERIOD.
           CLOSE GLPARAMETER-FILE.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            IF LINE-CNT > 56
               MOVE 60 TO LINE-CNT
               PERFORM PRR-010.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE "***ASSET TOTALS***" TO D-DESC.
            MOVE WS-TOT-BALANCE       TO D-BALANCE.
            MOVE WS-TOT-DEPRECIATION  TO D-DEPRECIATION.
            COMPUTE WS-TOT-BALANCE =
                  WS-TOT-BALANCE + WS-TOT-DEPRECIATION.
            MOVE WS-TOT-BALANCE       TO D-BOOK.
            WRITE PRINT-REC FROM DETAIL-LINE.

            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE "***MOTOR VEHICLE TOTALS***" TO D-DESC.
            MOVE WS-MV-BALANCE                TO D-BALANCE.
            MOVE WS-MV-DEPRECIATION           TO D-DEPRECIATION.
            COMPUTE WS-MV-BALANCE =
                  WS-MV-BALANCE + WS-MV-DEPRECIATION.
            MOVE WS-MV-BALANCE                TO D-BOOK.
            WRITE PRINT-REC FROM DETAIL-LINE.

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
      * END-OF-JOB.
