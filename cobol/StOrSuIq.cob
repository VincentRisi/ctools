        IDENTIFICATION DIVISION.
        PROGRAM-ID. StOrSuIq.
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
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-DELIVERVIA        PIC X(20) VALUE " ".
       77  WS-DEL-SUB           PIC 9 VALUE 0.    
       77  WS-SUPPLIER-NUMBER   PIC X(7) VALUE " ".
       77  WS-ORDER-NUMBER      PIC X(20) VALUE " ".
       77  WS-QUANTITY          PIC 9(5) VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-WRITE             PIC X VALUE " ".
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
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(75) VALUE
           "** ORDERS ON SUPPLIERS INQUIRY BY SUPPLIER **".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(1) VALUE " ".
           03  FILLER         PIC X(10) VALUE "SUPPLIER:".
           03  H3-SUPPLIER    PIC X(20).
           03  FILLER         PIC X(93) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(24) VALUE "ORDER NUMBER".
           03  FILLER         PIC X(25) VALUE "DELIVER VIA".
           03  FILLER         PIC X(24) VALUE " ORD DATE     DUE DATE".
           03  FILLER         PIC X(47) VALUE "CONFIRM  ITEMS".
       01  DETAIL-LINE.
           03  D-PONO         PIC X(24).
           03  D-DELIVER      PIC X(25).
           03  D-ORDDATE      PIC X(10).
           03  FILLER         PIC X(3) VALUE " ".
           03  D-DUEDATE      PIC X(10).
           03  FILLER         PIC X(4) VALUE " ".
           03  D-CONFIRMED    PIC X(6).
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
           DISPLAY "** ORDERS INQUIRY BY SUPPLIER **" AT POS
           MOVE 0410 TO POS
           DISPLAY "********************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
           MOVE 2510 TO POS
           DISPLAY "INQUIRY PROGRAM LOADING ...." AT POS.
           PERFORM OPEN-FILES.
           PERFORM READ-DELIVERY-FILE.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE "N"        TO WS-ANSWER.
            MOVE "SUPPLIER" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO OO-SUPPLIER-NUMBER WS-SUPPLIER-NUMBER.
            IF OO-SUPPLIER-NUMBER = "   "
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            MOVE " " TO F-EXIT-CH.

            PERFORM READ-TRANSACTIONS.
       GET-900.
            IF WS-ANSWER = "Y"
                GO TO GET-999.
            IF F-INDEX < 15
             IF F-INDEX NOT = X"07"
               MOVE 2910 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen." AT POS
               MOVE 3010 TO POS
               DISPLAY "Or Press 'F10' To Print All Supplier Orders."
                  AT POS
               MOVE 15 TO F-INDEX
               MOVE "CONFIRMED" TO F-FIELDNAME
               MOVE 9           TO F-CBFIELDNAME
               MOVE 1           TO F-CBFIELDLENGTH
               PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"07" AND NOT = X"1F"
                MOVE 1 TO F-INDEX
                GO TO GET-900.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                MOVE 1 TO F-INDEX
                CLOSE OUTSTANDING-ORDERS
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                PERFORM ERROR-020
                CLOSE OUTSTANDING-ORDERS
                GO TO GET-999.
       GET-999.
            EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           PERFORM OPEN-005.
           MOVE 1 TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
       RDTR-005.
           START OUTSTANDING-ORDERS KEY NOT < OO-SUPPLIER-NUMBER
              INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 NOT = 0
              CLOSE OUTSTANDING-ORDERS
              MOVE
             "NO ORDERS FOUND FOR SUPPLIER ENTERED, 'ESC' TO CLEAR."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE "Y" TO WS-ANSWER
              GO TO RDTR-999.
           IF WS-SUPPLIER-NUMBER NOT = OO-SUPPLIER-NUMBER
              CLOSE OUTSTANDING-ORDERS
              MOVE
             "NO ORDERS FOUND FOR SUPPLIER ENTERED, 'ESC' TO CLEAR."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE "Y" TO WS-ANSWER
              GO TO RDTR-999.
           MOVE " " TO WS-ORDER-NUMBER.
           MOVE 0 TO WS-QUANTITY.
       RDTR-010.
           READ OUTSTANDING-ORDERS NEXT
               AT END NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 10
            IF WS-QUANTITY NOT = 0
               PERFORM SCROLL-050
               MOVE 1 TO F-INDEX
               CLOSE OUTSTANDING-ORDERS
               GO TO RDTR-999
            ELSE
               MOVE 1 TO F-INDEX
               CLOSE OUTSTANDING-ORDERS
               GO TO RDTR-999.
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
               GO TO RDTR-010.
           IF OO-SUPPLIER-NUMBER NOT = WS-SUPPLIER-NUMBER
            IF WS-QUANTITY = 0
               CLOSE OUTSTANDING-ORDERS
               GO TO RDTR-999.

           IF OO-QUANTITY NOT > 0
               GO TO RDTR-010.

           IF WS-ORDER-NUMBER = " "
            IF WS-SUPPLIER-NUMBER = OO-SUPPLIER-NUMBER
               MOVE OO-ORDER-NUMBER TO WS-ORDER-NUMBER
               PERFORM SCROLL-000
               ADD 1 TO WS-QUANTITY
               GO TO RDTR-010.
           IF OO-ORDER-NUMBER = WS-ORDER-NUMBER
               ADD 1 TO WS-QUANTITY
               MOVE 3010 TO POS
               DISPLAY "Adding Total Number Of Items For Order...."
               AT POS
               GO TO RDTR-010.

           IF OO-ORDER-NUMBER NOT = WS-ORDER-NUMBER
            IF WS-SUPPLIER-NUMBER = OO-SUPPLIER-NUMBER
             IF F-INDEX NOT > 14
               PERFORM SCROLL-050
               ADD 1 TO F-INDEX
               MOVE OO-ORDER-NUMBER TO WS-ORDER-NUMBER
               PERFORM SCROLL-000
               MOVE 1 TO WS-QUANTITY
               GO TO RDTR-010
             ELSE
               PERFORM SCROLL-050
               ADD 1 TO F-INDEX
               GO TO RDTR-020.

           IF OO-SUPPLIER-NUMBER NOT = WS-SUPPLIER-NUMBER
            IF WS-QUANTITY NOT = 0
               PERFORM SCROLL-050
               MOVE 1 TO F-INDEX
               CLOSE OUTSTANDING-ORDERS
               GO TO RDTR-999.
           GO TO RDTR-010.
       RDTR-020.
           IF F-INDEX > 15
                MOVE 2910 TO POS
                DISPLAY "Press 'PgDn' For More, Or" AT POS
                ADD 31 TO POS
                DISPLAY "'ESC' To Clear The Screen !" AT POS
                MOVE 3010 TO POS
                DISPLAY "Or Press 'F10' To Print All Supplier Orders."
                   AT POS
                MOVE 15          TO F-INDEX
                MOVE "CONFIRMED" TO F-FIELDNAME
                MOVE 9           TO F-CBFIELDNAME
                MOVE 1           TO F-CBFIELDLENGTH
                PERFORM USER-FILL-FIELD.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-TRANSACTIONS
                MOVE " " TO F-EXIT-CH
                MOVE 1 TO F-INDEX.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                CLOSE OUTSTANDING-ORDERS
                GO TO RDTR-999.
           IF F-EXIT-CH = X"1F"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                CLOSE OUTSTANDING-ORDERS
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE OUTSTANDING-ORDERS
                MOVE "Y" TO WS-ANSWER
                PERFORM ERROR-020
                GO TO RDTR-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C"
                    AND NOT = X"07" AND NOT = " " AND NOT = X"1F"
                MOVE 16 TO F-INDEX
                GO TO RDTR-020.
           PERFORM SCROLL-000.
      *     ADD 1 TO F-INDEX.
           MOVE OO-ORDER-NUMBER TO WS-ORDER-NUMBER.
           MOVE 1 TO WS-QUANTITY.
           GO TO RDTR-010.
       RDTR-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-001.
           PERFORM OPEN-005.
           MOVE 0 TO PAGE-CNT WS-QUANTITY.
           MOVE " " TO WS-ORDER-NUMBER.
           MOVE 66 TO LINE-CNT.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE WS-SUPPLIER-NUMBER TO OO-SUPPLIER-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-SUPPLIER-NUMBER
              INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
              GO TO PRR-900.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "ST-ORDERS FILE BUSY ON START, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO PRR-001.
       PRR-002.
           READ OUTSTANDING-ORDERS NEXT
               AT END NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 10
               MOVE 0 TO WS-OUTORD-ST1
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
           IF OO-QUANTITY NOT > 0
               GO TO PRR-002.

           IF OO-SUPPLIER-NUMBER < WS-SUPPLIER-NUMBER
               GO TO PRR-002.
           IF OO-SUPPLIER-NUMBER > WS-SUPPLIER-NUMBER
            IF WS-QUANTITY NOT = 0
               MOVE WS-QUANTITY TO D-QTY
               PERFORM PRR-025
               GO TO PRR-900
            ELSE
               GO TO PRR-900.

           IF WS-ORDER-NUMBER = " "
            IF WS-SUPPLIER-NUMBER = OO-SUPPLIER-NUMBER
               MOVE OO-ORDER-NUMBER TO WS-ORDER-NUMBER
               ADD 1 TO WS-QUANTITY
               PERFORM PRR-020
               GO TO PRR-002.
           IF OO-ORDER-NUMBER = WS-ORDER-NUMBER
               ADD 1 TO WS-QUANTITY
               GO TO PRR-002.
           IF OO-ORDER-NUMBER NOT = WS-ORDER-NUMBER
               MOVE WS-QUANTITY TO D-QTY
               PERFORM PRR-025
               MOVE OO-ORDER-NUMBER TO WS-ORDER-NUMBER
               PERFORM PRR-020
               MOVE 1 TO WS-QUANTITY
               GO TO PRR-002.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           MOVE OO-ORDER-NUMBER          TO D-PONO.
           MOVE OO-ORDERDATE             TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE             TO D-ORDDATE.
           IF OO-DUEDATE > 0
              MOVE OO-DUEDATE             TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE           TO D-DUEDATE
           ELSE
              MOVE " "                    TO D-DUEDATE.
           MOVE OO-DELIVERY-METHOD       TO WS-DEL-SUB.
           MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELIVERVIA.
           MOVE WS-DELIVERVIA            TO D-DELIVER.
           MOVE OO-UPDATED               TO D-CONFIRMED.
       PRR-025.
            IF LINE-CNT > 60
               PERFORM PRR-060.
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC.
           MOVE 1 TO WS-QUANTITY.
           ADD 1 TO LINE-CNT.
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
           MOVE WS-SUPPLIER-NUMBER TO H3-SUPPLIER
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC
           MOVE 5 TO LINE-CNT.
       PRR-900.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE OUTSTANDING-ORDERS.
       PRR-999.
           EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            PERFORM ERROR-020.
            MOVE "ORDERNO"       TO F-FIELDNAME
            MOVE 7               TO F-CBFIELDNAME
            MOVE OO-ORDER-NUMBER TO F-NAMEFIELD
            MOVE 20              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDDATE"    TO F-FIELDNAME
            MOVE 7            TO F-CBFIELDNAME
            MOVE OO-ORDERDATE TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE TO F-NAMEFIELD
            MOVE 10           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DUEDATE"     TO F-FIELDNAME
            MOVE 7             TO F-CBFIELDNAME.
            IF OO-DUEDATE > 0
               MOVE OO-DUEDATE    TO SPLIT-DATE
               PERFORM CONVERT-DATE-FORMAT
               MOVE DISPLAY-DATE  TO F-NAMEFIELD
            ELSE
               MOVE " "           TO F-NAMEFIELD.
            MOVE 10            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL"                    TO F-FIELDNAME
            MOVE 3                        TO F-CBFIELDNAME
            MOVE OO-DELIVERY-METHOD       TO WS-DEL-SUB
            MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELIVERVIA
            MOVE WS-DELIVERVIA            TO F-NAMEFIELD
            MOVE 20                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CONFIRMED" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE OO-UPDATED  TO F-NAMEFIELD
            MOVE 1           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-050.
            MOVE "QTY"       TO F-FIELDNAME
            MOVE 3           TO F-CBFIELDNAME
            MOVE WS-QUANTITY TO F-EDNAMEFIELDQTY
            MOVE 5           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY.
       SCROLL-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO SUB-1 F-INDEX.
       CLTR-010.
            IF SUB-1 > 15
                GO TO CLTR-999.
            MOVE "ORDERNO" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 20        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QTY" TO F-FIELDNAME
            MOVE 3     TO F-CBFIELDNAME
            MOVE " "   TO F-NAMEFIELD
            MOVE 5     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDDATE" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 10        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DUEDATE" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 10        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL" TO F-FIELDNAME
            MOVE 3     TO F-CBFIELDNAME
            MOVE " "   TO F-NAMEFIELD
            MOVE 20    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CONFIRMED" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 1           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            ADD 1 TO SUB-1 F-INDEX.
            GO TO CLTR-010.
       CLTR-999.
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
               MOVE "PARAMETER FILE BUSY ON READ, 'ESC' TO RETRY."
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
       OPEN-001.
           GO TO OPEN-106.
       OPEN-005.
           OPEN I-O OUTSTANDING-ORDERS.
           IF WS-OUTORD-ST1 NOT = 0
              MOVE "ORDERS FILE BUSY ON OPEN, 'ESC' TO RETRY."
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
       OPEN-110.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StOrSuIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-900.
            EXIT PROGRAM.
      *       STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldPrice".
       Copy "WriteFieldQty".
       Copy "WriteFieldDate".
       Copy "DisplayForm".
       Copy "UserFillField".
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
      * END-OF-JOB.
