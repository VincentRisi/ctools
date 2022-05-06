        IDENTIFICATION DIVISION.
        PROGRAM-ID. StOrSuRp.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStOrders".
         Copy "SelectSlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdOutOrd.
           COPY ChlfdParam.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "StMastIq".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-CONFIRMED         PIC X VALUE " ".
       77  WS-FOR-LOC           PIC X VALUE " ".
       77  WS-DELIVERVIA        PIC X(20) VALUE " ".
       77  WS-DEL-SUB           PIC 9 VALUE 0.    
       77  WS-SUPPLIER          PIC X(7) VALUE " ".
       77  WS-ORDER-NUMBER      PIC X(20) VALUE " ".
       77  WS-QUANTITY          PIC 9(5) VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-WRITE             PIC X VALUE " ".
       77  WS-ACCEPT-DATE       PIC X(10) VALUE " ".
       77  WS-VALIDDATE         PIC 9(8) VALUE 0.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1      PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  STORE-DEL.
         02  WS-DEL-OCCUR OCCURS 10.
           03  WS-DEL-TYPE        PIC X.
           03  WS-DEL-CODE        PIC X.
           03  WS-DEL-TERM        PIC X(20).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(8) VALUE " ".
           03  FILLER         PIC X(60) VALUE
           "** DELINQUENT ORDERS ON SUPPLIERS REPORT BY ORDER **".
           03  FILLER         PIC X(10) VALUE "DUE-DATE:".
           03  H1-DUEDATE     PIC X(10) VALUE " ".
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(25) VALUE " ".
           03  FILLER         PIC X(52) VALUE ALL "*".
           03  FILLER         PIC X(93) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(31) VALUE
            "ORDER NUMBER         SUPPLIER".
           03  FILLER         PIC X(26) VALUE "DELIVER VIA".
           03  FILLER         PIC X(25) VALUE
            "ORDER DATE      DUE DATE".
           03  FILLER         PIC X(47) VALUE "  CONFIRMED  ITEMS".
       01  DETAIL-LINE.
           03  D-PONO         PIC X(21) VALUE " ".
           03  D-SUPPLIER     PIC X(10) VALUE " ".
           03  D-DELIVER      PIC X(26) VALUE " ".
           03  D-ORDDATE      PIC X(10) VALUE " ".
           03  FILLER         PIC X(5) VALUE " ".
           03  D-DUEDATE      PIC X(10) VALUE " ".
           03  FILLER         PIC X(4) VALUE " ".
           03  D-CONFIRMED    PIC X(6) VALUE " ".
           03  D-QTY          PIC Z(4)9.
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
           PERFORM CLEAR-SCREEN.
           MOVE 0310 TO POS
           DISPLAY "** SUPPLIERS DELINQUENT ORDERS REPORT BY ORDER **"
            AT POS
           MOVE 0410 TO POS
           DISPLAY "*************************************************"
            AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-DATA.
           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-003.
           PERFORM OPEN-FILES.
           PERFORM READ-DELIVERY-FILE.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE 1010 TO POS.
           DISPLAY "ENTER A DUE DATE TO PRINT FROM :[          ]"
               AT POS.  
           ADD 33 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT-DATE.

           IF W-ESCAPE-KEY = 4
                GO TO GET-999.
           MOVE WS-ACCEPT-DATE TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
               GO TO GET-000.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE
           MOVE DISPLAY-DATE TO H1-DUEDATE
           DISPLAY DISPLAY-DATE AT POS.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO WS-VALIDDATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO GET-000.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-010
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
       GET-010.
           MOVE 1315 TO POS
           DISPLAY "LEAVE BLANK FOR ALL P/ORDERS." AT POS
           MOVE 1210 TO POS
           DISPLAY "ENTER P/ORDER TO START FROM :[                    ]"
           AT POS
           ADD 30 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 9        TO CDA-ROW.
           MOVE 39        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ORDER-NUMBER.

           IF W-ESCAPE-KEY = 4
                GO TO GET-000.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-015
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
       GET-015.
           MOVE 1615 TO POS
           DISPLAY "LEAVE BLANK FOR ALL SUPPLIERS." AT POS
           MOVE 1510 TO POS
           DISPLAY "ENTER A SUPPLIER TO PRINT   :[       ]"
           AT POS
           ADD 30 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 39        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SUPPLIER.

           IF W-ESCAPE-KEY = 4
                GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-020
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-015.
       GET-020.
           MOVE 1810 TO POS
           DISPLAY "A=ALL ORDERS, C=CONFIRMED, U=UNCONFIRMED. [ ]"
           AT POS
           ADD 43 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 52        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CONFIRMED.

           IF WS-CONFIRMED NOT = "A" AND NOT = "C" AND NOT = "U"
                MOVE "ENTER ONLY A, C OR U, RE-ENTER" TO WS-MESSAGE
                PERFORM ERROR-000
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
           IF W-ESCAPE-KEY = 4
                GO TO GET-015.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-022
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
       GET-022.
           MOVE 2010 TO POS
           DISPLAY "F=FOREIGN, L=LOCAL, B=BOTH ORDERS : [ ]"
           AT POS
           ADD 37 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FOR-LOC.
           IF WS-FOR-LOC NOT = "B" AND NOT = "F" AND NOT = "L"
                MOVE "ENTER ONLY B, F OR L, RE-ENTER" TO WS-MESSAGE
                PERFORM ERROR-000
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-022.
           IF W-ESCAPE-KEY = 4
                GO TO GET-020.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-025
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-022.
       GET-025.
           PERFORM ERROR-020.
           MOVE 2510 TO POS
           DISPLAY "Orders Being Read, Report In Progress....." AT POS.
       GET-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-001.
           MOVE 0               TO PAGE-CNT WS-QUANTITY
           MOVE " "             TO OO-STOCK-NUMBER
           MOVE WS-ORDER-NUMBER TO OO-ORDER-NUMBER
           MOVE " "             TO WS-ORDER-NUMBER
           MOVE 66              TO LINE-CNT.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
              MOVE "BAD START ON S-ORDERS ERC 23/35/49, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO PRR-900.
           IF WS-OUTORD-ST1 NOT = 0
              MOVE "BAD START ON ST-ORDERS, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO PRR-001.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       PRR-002.
           READ OUTSTANDING-ORDERS NEXT
               AT END NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 10 OR = 91
            IF WS-QUANTITY NOT = 0
               MOVE WS-QUANTITY TO D-QTY
               PERFORM PRR-025
               GO TO PRR-900
            ELSE
               GO TO PRR-900.
               
           IF WS-OUTORD-ST1 NOT = 0
            MOVE "ST-ORDERS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO PRR-002.
               
           MOVE 2310 TO POS
           DISPLAY "Order Being Read:" AT POS
           ADD 18 TO POS
           DISPLAY OO-ORDER-NUMBER AT POS. 

           IF OO-DUEDATE > WS-VALIDDATE
               GO TO PRR-002.
               
           IF OO-QUANTITY NOT > 0
               GO TO PRR-002.
               
           IF WS-FOR-LOC = "L"
            IF OO-FOR-LOC = "F"
               GO TO PRR-002.
           IF WS-FOR-LOC = "F"
            IF OO-FOR-LOC = "L"
               GO TO PRR-002.

           IF WS-SUPPLIER NOT = " "
            IF WS-SUPPLIER NOT = OO-SUPPLIER-NUMBER
               GO TO PRR-002.

           IF WS-ORDER-NUMBER = " "
               MOVE OO-ORDER-NUMBER TO WS-ORDER-NUMBER
               ADD 1 TO WS-QUANTITY
               PERFORM PRR-020
               GO TO PRR-002.
           IF OO-ORDER-NUMBER = WS-ORDER-NUMBER
               ADD 1 TO WS-QUANTITY
               GO TO PRR-002.
           IF OO-ORDER-NUMBER NOT = WS-ORDER-NUMBER
            IF WS-QUANTITY > 0
               MOVE WS-QUANTITY TO D-QTY
               PERFORM PRR-025
               PERFORM PRR-027
               ADD 1 TO WS-QUANTITY
               MOVE OO-ORDER-NUMBER TO WS-ORDER-NUMBER
               PERFORM PRR-020
               GO TO PRR-002
            ELSE
               MOVE OO-ORDER-NUMBER TO WS-ORDER-NUMBER
               GO TO PRR-002.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           MOVE OO-ORDER-NUMBER          TO D-PONO
           MOVE OO-SUPPLIER-NUMBER       TO D-SUPPLIER
           MOVE OO-ORDERDATE             TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE             TO D-ORDDATE.
           IF OO-DUEDATE > 0
              MOVE OO-DUEDATE               TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE             TO D-DUEDATE
           ELSE
              MOVE " "                      TO D-DUEDATE.
           
           MOVE OO-DELIVERY-METHOD       TO WS-DEL-SUB
           MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELIVERVIA
           MOVE WS-DELIVERVIA            TO D-DELIVER.
           IF OO-UPDATED = "Y"
              MOVE "YES"                 TO D-CONFIRMED
           ELSE
              MOVE "NO "                 TO D-CONFIRMED.
       PRR-025.
           IF WS-CONFIRMED = "C"
            IF OO-UPDATED NOT = "Y"
               GO TO PRR-027.
           IF WS-CONFIRMED = "U"
            IF OO-UPDATED = "Y"
               GO TO PRR-027.
           
            IF LINE-CNT > 60
               PERFORM PRR-060.
           WRITE PRINT-REC FROM DETAIL-LINE.
       PRR-027.
           MOVE " " TO PRINT-REC DETAIL-LINE
           MOVE 0   TO WS-QUANTITY
           ADD 1    TO LINE-CNT.
       PRR-030.
           GO TO PRR-002.
       PRR-060.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE " " TO PRINT-REC
           MOVE 5 TO LINE-CNT.
       PRR-900.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           IF WS-SUPPLIER NOT = " "
              MOVE "REPORT ONLY FOR A PARTICULAR SUPPLIER PRINTED."
              TO PRINT-REC
              WRITE PRINT-REC.
           IF WS-FOR-LOC = "L"
              MOVE "REPORT ONLY FOR LOCAL SUPPLIER ORDERS PRINTED."
              TO PRINT-REC
              WRITE PRINT-REC.
           IF WS-FOR-LOC = "F"
              MOVE "REPORT ONLY FOR FOREIGN SUPPLIER ORDERS PRINTED."
              TO PRINT-REC
              WRITE PRINT-REC.
           IF WS-FOR-LOC = "B"
              MOVE "REPORT FOR ALL SUPPLIER ORDERS PRINTED."
              TO PRINT-REC
              WRITE PRINT-REC.
              
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       PRR-999.
           EXIT.
      *
       READ-DELIVERY-FILE SECTION.
       RDELIV-000.
            OPEN I-O PARAMETER-FILE.
            MOVE 1 TO SUB-1
                      PA-RECORD.
            MOVE 3 TO PA-TYPE.
       RDELIV-010.
            READ PARAMETER-FILE NEXT
                AT END NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 10
                GO TO RDELIV-900.
            IF PA-TYPE < 3
               GO TO RDELIV-010.
            IF PA-TYPE > 3
                GO TO RDELIV-900.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER DELV BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RDELIV-010.
            IF PARAMETER-REC = "           "
               GO TO RDELIV-010.           
            MOVE PARAMETER-REC TO WS-DEL-OCCUR (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 = 10
               PERFORM ERROR-020
               GO TO RDELIV-900.
            GO TO RDELIV-010.
       RDELIV-900. 
            CLOSE PARAMETER-FILE.
       RDELIV-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
           OPEN I-O OUTSTANDING-ORDERS.
           IF WS-OUTORD-ST1 NOT = 0
              MOVE "ST-ORDERS FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO OPEN-005.
       OPEN-106.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-010.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE OUTSTANDING-ORDERS.
       END-900.
            EXIT PROGRAM.
      *       STOP RUN.
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
      * END-OF-JOB.
