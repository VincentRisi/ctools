        IDENTIFICATION DIVISION.
        PROGRAM-ID. BmKtAnRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
       Copy "SelectStMaster".
       Copy "SelectBmMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdToolkit.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-RANGE3            PIC X(3) VALUE " ".
       77  WS-TOTAL             PIC 9(6).
       77  WS-QTY               PIC 9(3).
       77  WS-QTY-DISPLAY       PIC Z(2)9.
       77  WS-STORE             PIC X(15) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-ST-ST1        PIC 99.
       01  WS-TOOLKIT-STATUS.
           03  WS-TOOLKIT-ST1   PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(64) VALUE
           "T O O L K I T   A N A L Y S I S   R E P O R T".
           03  FILLER         PIC X(9) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(6) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(45) VALUE " ".
           03  FILLER         PIC X(45) VALUE ALL "*".
           03  FILLER         PIC X(42) VALUE " ".
       01  HEAD3-1.
           03  FILLER         PIC X(18) VALUE "TOOLKIT NUMBER  :".
           03  H3-KIT         PIC X(20) VALUE " ".
           03  FILLER         PIC X(94) VALUE " ".
       01  HEAD3-2.
           03  FILLER         PIC X(18) VALUE "KIT DESCRIPTION :".
           03  H3-DESC1       PIC X(20) VALUE " ".
           03  H3-DESC2       PIC X(25) VALUE " ".
           03  FILLER         PIC X(69) VALUE " ".
       01  HEAD3-3.
           03  FILLER         PIC X(18) VALUE "QTY TO CHECK ON :".
           03  H3-QTY         PIC Z(2)9.
           03  FILLER         PIC X(111) VALUE " ".
       01  HEAD4.
           03  FILLER         PIC X(58) VALUE
           "STOCK NUMBER    DESCRIPTION".   
           03  FILLER         PIC X(74) VALUE
           "PHYSICAL  QUANTITY       STOCK QUANTITY ON:      MIN".
       01  HEAD5.
           03  FILLER         PIC X(60) VALUE " ".
           03  FILLER         PIC X(72) VALUE
           "QTY.    TOT  KIT   HAND    RES  ORDER    B/O  LEVEL".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(16).
           03  D-DESC1        PIC X(20).
           03  D-DESC2        PIC X(22).
           03  D-COUNT        PIC X(7) VALUE ".......".
           03  D-TOT          PIC Z(5)9.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-KIT          PIC Z(2)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-HAND         PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-RES          PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-ORDER        PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-BORDER       PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-MIN          PIC Z(5)9.
           03  FILLER         PIC X(21) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY "** TOOLKIT STOCK ITEM ANALYSIS REPORT **" AT POS
           MOVE 415 TO POS
           DISPLAY "****************************************" AT POS.
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
            DISPLAY "          FROM TOOLKIT NUMBER: [               ]"
                      AT POS.
            MOVE 1042 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
            MOVE 1210 TO POS.
            DISPLAY "            TO TOOLKIT NUMBER: [               ]"
                      AT POS.
            MOVE 1242 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

           IF W-ESCAPE-KEY = 4
               GO TO GET-000.
            IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-020.
           MOVE 1410 TO POS.
           DISPLAY "Enter Qty Of Kits To Assemble: [   ]" AT POS.
           MOVE 1442 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 3         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE3.

           IF W-ESCAPE-KEY = 4
               GO TO GET-010.
           IF WS-RANGE3 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
           MOVE WS-RANGE3 TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           IF NUMERIC-RATE NOT > 0
              MOVE "YOU MUST ENTER A POSITVE QUANTITY, RE-ENTER."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO GET-020.
           MOVE 1442 TO POS
           MOVE NUMERIC-RATE TO WS-QTY
           MOVE WS-QTY TO WS-QTY-DISPLAY H3-QTY
           DISPLAY WS-QTY-DISPLAY AT POS.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-040.
           PERFORM ERROR1-020.
           PERFORM ERROR-020.
           MOVE 1442 TO POS
           MOVE NUMERIC-RATE TO WS-QTY
           MOVE WS-QTY TO WS-QTY-DISPLAY
           DISPLAY WS-QTY-DISPLAY AT POS.
           MOVE 2510 TO POS.
           DISPLAY "The Report Is Being Compiled, Please Be Patient."
           AT POS.
       GET-050.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
       GET-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            START STOCK-MASTER KEY NOT < ST-KEY.

            MOVE WS-RANGE1 TO TO-TOOLKIT-NUMBER.
            START TOOLKITS KEY NOT < TO-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 88 TO WS-TOOLKIT-ST1
               GO TO PRR-999.
       PRR-002.
            READ TOOLKITS NEXT
               AT END NEXT SENTENCE.
            IF WS-TOOLKIT-ST1 NOT = 0
               MOVE TO-TOOLKIT-NUMBER TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE
               "KIT BUSY AT ANOTHER WORK-STATION, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO PRR-002.
            IF TO-TOOLKIT-NUMBER < WS-RANGE1
               GO TO PRR-002.
            IF TO-TOOLKIT-NUMBER > WS-RANGE2
               GO TO PRR-999.
            IF WS-STORE = " "
               MOVE TO-TOOLKIT-NUMBER TO WS-STORE
                                         ST-STOCKNUMBER
               PERFORM RS-010
               MOVE TO-TOOLKIT-NUMBER TO H3-KIT
               MOVE ST-DESCRIPTION1   TO H3-DESC1
               MOVE ST-DESCRIPTION2   TO H3-DESC2
               GO TO PRR-002.
            IF TO-TOOLKIT-NUMBER NOT = WS-STORE
               MOVE TO-TOOLKIT-NUMBER TO H3-KIT
                                         WS-STORE
                                         TO-COMPONENT-NUMBER
               PERFORM READ-STOCK
               MOVE ST-DESCRIPTION1   TO H3-DESC1
               MOVE ST-DESCRIPTION2   TO H3-DESC2
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               WRITE PRINT-REC FROM HEAD3-1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD3-2
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC FROM HEAD3-3
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC
               ADD 5 TO LINE-CNT
               GO TO PRR-002.
            PERFORM READ-STOCK.
       PRR-010.
            IF LINE-CNT < 60
               GO TO PRR-020.
            ADD 1         TO PAGE-CNT.
            MOVE PAGE-CNT TO H1-PAGE.
            MOVE " "      TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD2.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD3-1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD3-2.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD3-3.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD4.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD5.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE 11 TO LINE-CNT.
       PRR-020.
           MOVE ST-STOCKNUMBER     TO D-STOCK
           MOVE ST-DESCRIPTION1    TO D-DESC1
           MOVE ST-DESCRIPTION2    TO D-DESC2
           MOVE ST-MINIMUMLEVEL    TO D-MIN
           COMPUTE WS-TOTAL = WS-QTY * TO-QUANTITY
           MOVE WS-TOTAL           TO D-TOT
           MOVE TO-QUANTITY        TO D-KIT
           MOVE ST-QTYONHAND       TO D-HAND
           MOVE ST-QTYONRESERVE    TO D-RES
           MOVE ST-QTYONORDER      TO D-ORDER
           MOVE ST-QTYONBORDER     TO D-BORDER.

           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       READ-STOCK SECTION.
       RS-005.
           MOVE TO-COMPONENT-NUMBER TO ST-STOCKNUMBER.
       RS-010.
           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-ST-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-ST-ST1
               MOVE "INVALID STOCK I" TO ST-DESCRIPTION1
               MOVE "TEM IN KIT FILE" TO ST-DESCRIPTION2
               GO TO RS-999.
          IF WS-ST-ST1 NOT = 0
               MOVE 0 TO WS-ST-ST1
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY." 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE ST-STOCKNUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR-020
               GO TO RS-010.
       RS-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O STOCK-MASTER.
           IF WS-ST-ST1 NOT = 0 
              MOVE 0 TO WS-ST-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-005.
           OPEN I-O TOOLKITS.
           IF WS-TOOLKIT-ST1 NOT = 0 
              MOVE 0 TO WS-TOOLKIT-ST1
              MOVE "KIT FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-005.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            IF LINE-CNT > 60
               PERFORM PRR-010.
            IF WS-TOOLKIT-ST1 = 88
               MOVE WS-RANGE1 TO PRINT-REC
               WRITE PRINT-REC
               MOVE "*** STARTING KIT NUMBER TOO HIGH, RE-TRY. ***"
               TO PRINT-REC
               WRITE PRINT-REC.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-010.
           CLOSE STOCK-MASTER
                  TOOLKITS
                  PRINT-FILE.
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
      *
      * END-OF-JOB.
