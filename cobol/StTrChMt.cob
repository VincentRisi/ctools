        IDENTIFICATION DIVISION.
        PROGRAM-ID. StTrChMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStTrans".
          Copy "SelectStTransLy".
          Copy "SelectSlRegister".
          Copy "SelectSlDaily".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdStTrans.
           COPY ChlfdStTransLy.
           COPY ChlfdRegister.
           COPY ChlfdDaily.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(80).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-NUMBER-DIS        PIC Z(5)9.
       77  WS-ANSWER1           PIC X VALUE " ".
       77  WS-TYPE              PIC X VALUE " ".
       77  WS-TYPE-NUM          PIC 99 VALUE 0.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
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
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1   PIC 99.
       01  WS-STTRANSLY-STATUS.
           03  WS-STTRANSLY-ST1 PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1     PIC 99.
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
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
           MOVE 0310 TO POS
           DISPLAY "** PROGRAM TO CHECK REGISTER RECORDS + ST-TRANS **"
               AT POS
           MOVE 0410 TO POS
           DISPLAY "**************************************************"
                AT POS.
           MOVE " " TO WS-PRINTANSWER.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 910 TO POS
           DISPLAY "Enter the TYPE of REGISTER To CHECK" AT POS
           MOVE 1010 TO POS
           DISPLAY
           "I=INV, P=P/SLP, C=C/N, T=T/KIT, Q=QUOTE, R=REPAIR: [ ]"
                 AT POS
           ADD 52 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 61        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TYPE.

      *     ACCEPT WS-TYPE AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF WS-TYPE NOT = "I" AND NOT = "P" AND NOT = "C"
                  AND NOT = "T" AND NOT = "Q" AND NOT = "R"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-015.
           MOVE 1210 TO POS
           DISPLAY
           "Print a LIST only, Y=Yes N=No.(N=list & Delete)    [ ]"
                AT POS
           ADD 52 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 61        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

      *     ACCEPT WS-ANSWER1 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
           IF WS-ANSWER1 NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-020.
           IF WS-TYPE = "R"
            IF WS-ANSWER1 = "N"
              MOVE 1410 TO POS
            DISPLAY "REPAIRS WILL BE MOVED TO THE L/YR FILE." AT POS.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           PERFORM READ-TRANS.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       READ-TRANS SECTION.
       DQT-001.
           MOVE WS-CO-NAME TO CO-NAME
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           WRITE PRINT-REC FROM COMPANY-LINE
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           MOVE "** LIST OF UNMATCHED ST-TRANS & REGISTER RECORDS. **"
               TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC.
       DQT-005.
           IF WS-TYPE = "I"
               MOVE 1 TO STTR-TYPE WS-TYPE-NUM
               MOVE "** FOR INVOICE **" TO PRINT-REC.
           IF WS-TYPE = "R"
               MOVE 3 TO STTR-TYPE WS-TYPE-NUM
               MOVE "** FOR REPAIRS **" TO PRINT-REC.
           IF WS-TYPE = "P"
               MOVE 4 TO STTR-TYPE WS-TYPE-NUM
               MOVE "** FOR P/SLIPS **" TO PRINT-REC.
           IF WS-TYPE = "C"
               MOVE 6 TO STTR-TYPE WS-TYPE-NUM
               MOVE "** FOR C/NOTES **" TO PRINT-REC.
           IF WS-TYPE = "T"
               MOVE 7 TO STTR-TYPE WS-TYPE-NUM
               MOVE "** FOR BILLS OF MATERIAL **" TO PRINT-REC.
           IF WS-TYPE = "Q"
               MOVE 8 TO STTR-TYPE WS-TYPE-NUM
               MOVE "** FOR QUOTES **" TO PRINT-REC.
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           MOVE 1     TO STTR-TRANSACTION-NUMBER
                         STTR-REFERENCE1.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
                INVALID KEY NEXT SENTENCE.
       DQT-010.
           READ STOCK-TRANS-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
               MOVE "ST-TRANS FILE AT END, FINISHING." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO DQT-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO DQT-010.
           IF STTR-TYPE NOT = WS-TYPE-NUM
               GO TO DQT-999.
           IF INCR-INVOICE NOT = 0
            IF STTR-REFERENCE1 NOT = INCR-INVOICE
               PERFORM READ-REGISTER.
           IF INCR-INVOICE = 0
               PERFORM READ-REGISTER.
           IF WS-INCR-ST1 = 88
               GO TO DQT-800.
           GO TO DQT-010.
       DQT-800.
           MOVE STTR-TRANSACTION-NUMBER TO WS-NUMBER-DIS
           MOVE 2510 TO POS
           DISPLAY "Transaction # Being deleted:" AT POS
           ADD 28 TO POS
           DISPLAY WS-NUMBER-DIS AT POS.
           IF WS-ANSWER1 = "Y"
                 GO TO DQT-010.

           IF WS-ANSWER1 = "N"
            IF WS-TYPE = "R"
              PERFORM WR-ST-LY-010 THRU WR-ST-LY-030.
           IF WS-ANSWER1 = "N"
            IF WS-TYPE = "P"
             IF STTR-COMPLETE = "R"
              PERFORM WR-ST-LY-010 THRU WR-ST-LY-030.

           DELETE STOCK-TRANS-FILE
               INVALID KEY
                MOVE "STOCK TRANS RECORD N"  TO WS-DAILY-1ST
                MOVE "OT DELETED, NUMBER :"  TO WS-DAILY-2ND
                MOVE STTR-TRANSACTION-NUMBER TO WS-DAILY-3RD
                MOVE " "                     TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
           GO TO DQT-010.
       DQT-999.
            EXIT.
      *
       READ-REGISTER SECTION.
       DQR-005.
           MOVE STTR-TYPE       TO INCR-TRANS.
           MOVE STTR-REFERENCE1 TO INCR-INVOICE.
           START INCR-REGISTER KEY NOT < INCR-KEY
                INVALID KEY NEXT SENTENCE.
       DQR-010.
           READ INCR-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               MOVE 88 TO WS-INCR-ST1
               GO TO DQR-020.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON READ, 'CANCEL' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO DQR-010.
       DQR-020.
           MOVE 2910 TO POS
           DISPLAY "Register # in Process      :" AT POS
           ADD 28 TO POS
           DISPLAY INCR-INVOICE AT POS.
           IF WS-INCR-ST1 = 0
               GO TO DQR-999.
       DQR-030.
           MOVE INCR-INVOICE TO PRINT-REC
           WRITE PRINT-REC.
       DQR-999.
           EXIT.
      *
       WRITE-STTRANS-LY SECTION.
       WR-ST-LY-010.
           MOVE 1910 TO POS
           DISPLAY
           "Stock Trans to L/YR being processed....." AT POS.
       WR-ST-LY-015.
           MOVE STTR-TYPE               TO STTR-LY-TYPE
           MOVE STTR-REFERENCE1         TO STTR-LY-REFERENCE1
           MOVE STTR-TRANSACTION-NUMBER TO STTR-LY-TRANSACTION-NUMBER
           MOVE STTR-ST-COMPLETE        TO STTR-LY-ST-COMPLETE
           MOVE STTR-STOCK-NUMBER       TO STTR-LY-STOCK-NUMBER
           MOVE STTR-AC-COMPLETE        TO STTR-LY-AC-COMPLETE
           MOVE STTR-ACCOUNT-NUMBER     TO STTR-LY-ACCOUNT-NUMBER
           MOVE STTR-INV-NO             TO STTR-LY-INV-NO
           MOVE STTR-DATE               TO STTR-LY-DATE
           MOVE STTR-COMPLETE           TO STTR-LY-COMPLETE.

           MOVE STTR-STOCK-NUMBER TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               GO TO WR-ST-LY-020.

           MOVE STTR-ORDERQTY    TO STTR-LY-ORDERQTY
           MOVE STTR-SHIPQTY     TO STTR-LY-SHIPQTY
           MOVE STTR-SHIPPEDQTY  TO STTR-LY-SHIPPEDQTY
           MOVE STTR-SALES-VALUE TO STTR-LY-SALES-VALUE
           MOVE STTR-COST-VALUE  TO STTR-LY-COST-VALUE
           MOVE STTR-PRICE       TO STTR-LY-PRICE
           MOVE STTR-DESC1       TO STTR-LY-DESC1
           MOVE STTR-DESC2       TO STTR-LY-DESC2
           MOVE STTR-ITEMDISC    TO STTR-LY-ITEMDISC
           MOVE STTR-TAX         TO STTR-LY-TAX
           MOVE STTR-UNIT        TO STTR-LY-UNIT.
           
           GO TO WR-ST-LY-030.
        WR-ST-LY-020.
           MOVE SPACES       TO COMMENT-LY-FIELDS.
           MOVE COM-ORDERQTY TO COM-LY-ORDERQTY
           MOVE COM-SHIPQTY  TO COM-LY-SHIPQTY
           MOVE COM-DESC     TO COM-LY-DESC
           MOVE COM-UNIT     TO COM-LY-UNIT
           MOVE COM-PRICE    TO COM-LY-PRICE
           MOVE COM-COST     TO COM-LY-COST
           MOVE COM-DISC     TO COM-LY-DISC
           MOVE " "          TO COM-LY-FILLER.
        WR-ST-LY-030.
           WRITE STOCK-TRANSLY-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-STTRANSLY-ST1 NOT = 0
              MOVE "ST-TRANS MOVE TO L/Y" TO WS-DAILY-1ST
              MOVE STTR-LY-REFERENCE1     TO WS-DAILY-2ND
              MOVE "NOT WRITTEN IN PER- " TO WS-DAILY-3RD
              MOVE "END ROUTINE.        " TO WS-DAILY-4TH
              PERFORM WRITE-DAILY.

           MOVE 1910 TO POS
           MOVE SPACES TO WS-MESSAGE
           DISPLAY WS-MESSAGE AT POS.
        WSTRLY-EXIT.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-001.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS FILE BUSY ON OPEN, 'CANCEL' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-001.
       OPEN-002.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON OPEN, 'CANCEL' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
       OPEN-056.
           OPEN I-O STOCK-TRANSLY-FILE.
           IF WS-STTRANSLY-ST1 NOT = 0
              MOVE "ST-TRANS-LY FILE BUSY ON OPEN, 'CANCEL' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-056.
       OPEN-999.
            EXIT.
      *    
       END-OFF SECTION.
       END-000.
           CLOSE STOCK-TRANS-FILE
                 STOCK-TRANSLY-FILE
                 INCR-REGISTER.
                 
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           EXIT PROGRAM.
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
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
       Copy "WriteDailyExcep1".
      *
      * END-OF-JOB
