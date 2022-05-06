        IDENTIFICATION DIVISION.
        PROGRAM-ID. BmKtItIq.
        AUTHOR.    CHRISTENSEN.
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
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "StMastIq".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-PRINTANSWER       PIC X(20) VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-STOCKNUMBER       PIC X(15) VALUE " ".
       77  WS-COMPONENT-NUMBER  PIC X(15) VALUE " ".
       77  WS-NUMBER            PIC 9(3) VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       01  WS-STDESC.
           03  WS-DESC1         PIC X(20) VALUE " ".
           03  WS-DESC2         PIC X(20) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-TOOLKIT-STATUS.
           03  WS-TOOLKIT-ST1       PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(75) VALUE
           "** TOOLKIT ITEMS FOUND IN KITS INQUIRY **".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(1) VALUE " ".
           03  FILLER         PIC X(11) VALUE "STOCK No:".
           03  H3-STOCK       PIC X(16).
           03  FILLER         PIC X(13) VALUE "DESCRIPTION:".
           03  H3-DESC1       PIC X(20).
           03  H3-DESC2       PIC X(24).
           03  FILLER         PIC X(12) VALUE "QTY ON HAND:".
           03  H3-QTY         PIC Z(5)9.
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(12) VALUE "STOCK PRICE:".
           03  H3-PRICE       PIC Z(5)9.99.
           03  FILLER         PIC X(3) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(18) VALUE "KIT NAME".
           03  FILLER         PIC X(45) VALUE "DESCRIPTION".
           03  FILLER         PIC X(12) VALUE "QTY P/KIT".
           03  FILLER         PIC X(13) VALUE "QTY ONHAND".
           03  FILLER         PIC X(45) VALUE "KIT PRICE".
       01  DETAIL-LINE.
           03  D-KIT          PIC X(18).
           03  D-DESC1        PIC X(20).
           03  D-DESC2        PIC X(24).
           03  D-QTY-PER-KIT  PIC Z(5)9.
           03  FILLER         PIC X(6) VALUE " ".
           03  D-QTY-ONHAND   PIC Z(5)9.
           03  FILLER         PIC X(8) VALUE " ".
           03  D-PRICE        PIC Z(5)9.99.
           03  FILLER         PIC X(35) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER         PIC X(40).
           03  T-DESC         PIC X(22) VALUE "TOTAL NUMBER OF KITS:".
           03  T-QTY          PIC Z(5)9.
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
           DISPLAY "** TOOLKIT ITEM INQUIRY BY TOOLKIT NUMBER **" AT POS
           MOVE 0410 TO POS
           DISPLAY "********************************************"
           AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
           MOVE 2910 TO POS
           DISPLAY "Program now loading, Please be patient..." AT POS.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE "N" TO WS-ANSWER.
            MOVE "KIT" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-COMPONENT-NUMBER TO WS-STOCKNUMBER
                 PERFORM START-STOCK-NEXT
                 PERFORM READ-STOCK-NEXT
                 MOVE ST-STOCKNUMBER TO WS-COMPONENT-NUMBER
                                        TO-COMPONENT-NUMBER
                 PERFORM GET-010
                 GO TO GET-020.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO TO-COMPONENT-NUMBER
                                WS-COMPONENT-NUMBER
                                ST-STOCKNUMBER.
            IF TO-COMPONENT-NUMBER = 0 OR = "   "
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-005
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            PERFORM READ-STOCK.
            IF ST-DESCRIPTION1 = "UNKNOWN"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
            GO TO GET-020.
       GET-010.
            MOVE "KIT"               TO F-FIELDNAME
            MOVE 3                   TO F-CBFIELDNAME
            MOVE TO-COMPONENT-NUMBER TO F-NAMEFIELD
            MOVE 15                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-015.
            MOVE TO-TOOLKIT-NUMBER   TO ST-STOCKNUMBER
            PERFORM READ-STOCK.
       GET-020.
            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.
            
            MOVE "DESC"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            MOVE WS-STDESC   TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "KIT-QTY"    TO F-FIELDNAME
            MOVE 7            TO F-CBFIELDNAME
            MOVE ST-QTYONHAND TO F-EDNAMEFIELDINV
            MOVE 6            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-INV

            MOVE "KIT-PRICE" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE ST-PRICE    TO F-EDNAMEFIELDPRICE
            MOVE 9           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PRICE.

            MOVE " " TO F-EXIT-CH.
            MOVE 0 TO WS-NUMBER.
            PERFORM READ-TRANSACTIONS.
       GET-900.
            IF WS-ANSWER = "Y"
                CLOSE TOOLKITS
                GO TO GET-999.
            IF F-INDEX < 15
             IF F-INDEX NOT = "07"
               MOVE 2910 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen." AT POS
               MOVE 3010 TO POS
               DISPLAY "Or Press 'F10' To Print The List of Kits."
               AT POS
               MOVE 15 TO F-INDEX
               PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH NOT = X"07" AND NOT = X"1F"
                MOVE 1 TO F-INDEX
                GO TO GET-900.
            MOVE 2910 TO POS
            DISPLAY "                                      " AT POS
            MOVE 3010 TO POS
            DISPLAY "                                      " AT POS
            ADD 30 TO POS
            DISPLAY "                                      " AT POS.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
                MOVE "Printing In Progress, Please Be Patient."
                   TO WS-MESSAGE
                PERFORM ERROR-000
                OPEN I-O TOOLKITS
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE TOOLKITS
                MOVE " " TO WS-MESSAGE
                PERFORM ERROR-020
                GO TO GET-999.
       GET-999.
            EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           OPEN I-O TOOLKITS.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE "TOOLKITS BUSY ON OPEN, GOING TO RETRY IN 1 SECOND"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR-020
              CALL "C$SLEEP" USING 1
              CLOSE TOOLKITS
              GO TO RDTR-000.
           MOVE 1 TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
       RDTR-005.
           MOVE ST-STOCKNUMBER TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-COMPONENT-NUMBER
                INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              CLOSE TOOLKITS
              GO TO RDTR-999.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE "TOOLKITS BUSY ON OPEN, GOING TO RETRY IN 1 SECOND"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR-020
              CALL "C$SLEEP" USING 1
              CLOSE TOOLKITS
              GO TO RDTR-000.
       RDTR-010.
           READ TOOLKITS NEXT
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE TOOLKITS
               GO TO RDTR-999.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE "TOOLKITS BUSY READ-NEXT, GOING TO RETRY IN 1 SECOND"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR-020
              CALL "C$SLEEP" USING 1
               GO TO RDTR-010.
           IF TO-TOOLKIT-NUMBER = " "
               GO TO RDTR-010.
           IF TO-COMPONENT-NUMBER NOT = WS-COMPONENT-NUMBER
               MOVE 1 TO F-INDEX
               CLOSE TOOLKITS
               GO TO RDTR-999.
           MOVE 2910 TO POS
           DISPLAY "                                           " AT POS.
       RDTR-020. 
           IF F-INDEX > 15
                MOVE 2910 TO POS
                DISPLAY "Press 'Pgdn' For More, Or" AT POS
                ADD 27 TO POS
                DISPLAY "'ESC' To Clear The Screen !" AT POS
                MOVE 3010 TO POS
                DISPLAY "Or Press 'F10' To Print List Of Kits." AT POS
                MOVE 15 TO F-INDEX
                PERFORM USER-FILL-FIELD.
            MOVE 2910 TO POS
            DISPLAY "                                   " AT POS
            ADD 31 TO POS
            DISPLAY "                                   " AT POS
            MOVE 3010 TO POS.
            DISPLAY "                                          "
                 AT POS
            ADD 40 TO POS
            DISPLAY "                                   " AT POS.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-TRANSACTIONS
                MOVE " " TO F-EXIT-CH
                MOVE 1 TO F-INDEX.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                GO TO RDTR-999.
           IF F-EXIT-CH = X"1F"
                MOVE "Printing In Progress, Please Be Patient."
                   TO WS-MESSAGE
                PERFORM ERROR-000
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                MOVE 3010 TO POS
                DISPLAY "                                        "
                   AT POS
                GO TO RDTR-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C"
                    AND NOT = X"07" AND NOT = " " AND NOT = X"1F"
                MOVE 16 TO F-INDEX
                GO TO RDTR-020.
           MOVE TO-TOOLKIT-NUMBER TO ST-STOCKNUMBER.
           PERFORM READ-STOCK.
           ADD 1 TO WS-NUMBER.
           PERFORM SCROLLING.
           ADD 1 TO F-INDEX.
           GO TO RDTR-010.
       RDTR-999.
           EXIT.
      *
       READ-STOCK SECTION.
       RS-000.
            MOVE ST-STOCKNUMBER TO WS-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-KEY.
       RS-010.
            READ STOCK-MASTER
                INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER
                MOVE "UNKNOWN" TO ST-DESCRIPTION1
                GO TO RS-999.
            IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK BUSY READ, GOING TO RETRY IN 1 SECOND"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR-020
                CALL "C$SLEEP" USING 1
                GO TO RS-010.
       RS-999.
            EXIT.
      *
       START-STOCK SECTION.
       ST-ST-000.
              MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
              START STOCK-MASTER KEY NOT LESS ST-STOCKNUMBER.
       ST-ST-999.
             EXIT.
      *
       START-STOCK-NEXT SECTION.
       ST-ST-000.
              MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
              START STOCK-MASTER KEY > ST-STOCKNUMBER.
       ST-ST-999.
             EXIT.
      *
       READ-STOCK-NEXT SECTION.
       R-ST-NX-000.
             MOVE 0 TO WS-STOCK-ST1.
       R-ST-NX-005. 
             READ STOCK-MASTER NEXT
                 AT END NEXT SENTENCE.
             IF WS-STOCK-ST1 = 0
                 GO TO R-ST-NX-999
             ELSE
             MOVE
               "STOCK BUSY ON READ-NEXT - R-ST-NX-010, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-STOCK-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-STOCK-ST1
                 PERFORM START-STOCK
                 GO TO R-ST-NX-005.
       R-ST-NX-999.
             EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE 0  TO PAGE-CNT
                       WS-NUMBER.
            MOVE 66 TO LINE-CNT.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
       PRR-002.
            MOVE WS-COMPONENT-NUMBER TO TO-COMPONENT-NUMBER.
            START TOOLKITS KEY NOT < TO-COMPONENT-NUMBER
                INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              CLOSE TOOLKITS
              GO TO PRR-999.
           IF WS-TOOLKIT-ST1 NOT = 0
              CLOSE TOOLKITS
              MOVE "TOOLKITS BUSY ON STARTP, GOING TO RETRY IN 1 SECOND"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR-020
              CALL "C$SLEEP" USING 1
              GO TO PRR-002.
       PRR-005.
            READ TOOLKITS NEXT
               AT END NEXT SENTENCE.
            IF WS-TOOLKIT-ST1 = 10
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO PRR-900.
            IF WS-TOOLKIT-ST1 NOT = 0
             MOVE "TOOLKITS BUSY READ-NEXT, GOING TO RETRY IN 2 SECONDS"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR-020
               CALL "C$SLEEP" USING 2
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO PRR-005.
            IF TO-TOOLKIT-NUMBER = " "
               GO TO PRR-005.
            IF TO-COMPONENT-NUMBER < WS-COMPONENT-NUMBER
               GO TO PRR-005.
            IF TO-COMPONENT-NUMBER > WS-COMPONENT-NUMBER
               GO TO PRR-900.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           MOVE TO-TOOLKIT-NUMBER TO ST-STOCKNUMBER
                                      D-KIT
           PERFORM READ-STOCK
           MOVE ST-DESCRIPTION1   TO D-DESC1
           MOVE ST-DESCRIPTION2   TO D-DESC2
           MOVE TO-QUANTITY       TO D-QTY-PER-KIT
           MOVE ST-QTYONHAND      TO D-QTY-ONHAND
           MOVE ST-PRICE          TO D-PRICE
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           ADD 1 TO WS-NUMBER
           GO TO PRR-005.
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
            MOVE " "      TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1 AFTER 2
            MOVE " "      TO PRINT-REC
            MOVE WS-COMPONENT-NUMBER TO H3-STOCK
            MOVE WS-COMPONENT-NUMBER TO ST-STOCKNUMBER
            PERFORM READ-STOCK
            MOVE ST-DESCRIPTION1     TO H3-DESC1
            MOVE ST-DESCRIPTION2     TO H3-DESC2
            MOVE ST-QTYONHAND        TO H3-QTY
            MOVE ST-PRICE            TO H3-PRICE
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE " " TO PRINT-REC
            MOVE 7 TO LINE-CNT.
       PRR-900.
           MOVE WS-NUMBER TO T-QTY
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC.
       
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       PRR-999.
           EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            MOVE "STOCKNUMBER"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE ST-STOCKNUMBER TO F-NAMEFIELD
            MOVE 15             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.
            
            MOVE "ST-DESC"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            MOVE WS-STDESC   TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QTY-PER-KIT" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE TO-QUANTITY   TO F-EDNAMEFIELDKITQTY
            MOVE 3             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-KITQTY

            MOVE "QTY-ON-HAND" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE ST-QTYONHAND  TO F-EDNAMEFIELDINV
            MOVE 6             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-INV

            MOVE "PRICE"  TO F-FIELDNAME
            MOVE 5        TO F-CBFIELDNAME
            MOVE ST-PRICE TO F-EDNAMEFIELDPRICE
            MOVE 9        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PRICE

            MOVE "NUMBER"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            MOVE WS-NUMBER TO F-EDNAMEFIELDKITQTY
            MOVE 3         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-KITQTY.
       SCROLL-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO SUB-1 F-INDEX.
       CLTR-010.
            IF SUB-1 > 15
                GO TO CLTR-999.
            MOVE "STOCKNUMBER" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE " "           TO F-NAMEFIELD
            MOVE 15            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ST-DESC"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 40         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "QTY-PER-KIT" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE " "           TO F-NAMEFIELD
            MOVE 3             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "QTY-ON-HAND" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE " "           TO F-NAMEFIELD
            MOVE 6             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PRICE" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 9       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            ADD 1 TO SUB-1 F-INDEX
            GO TO CLTR-010.
       CLTR-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCKMASTER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-006.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "BmKtItIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-MASTER.
       END-900.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldInv".
       Copy "WriteFieldKitQty".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldPrice".
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
      * END-OF-JOB.
