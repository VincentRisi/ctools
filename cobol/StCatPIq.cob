        IDENTIFICATION DIVISION.
        PROGRAM-ID. StCatPIq.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStCatalogue".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStCatalogue.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "StMastIq".
       77  WS-TYPE              PIC X VALUE " ".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-END               PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-STOCKNUMBER       PIC X(15) VALUE " ".
       77  WS-ORDERQTY          PIC 9(5) VALUE 0.
       77  WS-SHIPQTY           PIC 9(5) VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       01  WS-STDESC.
           03  WS-DESC1         PIC X(20) VALUE " ".
           03  WS-DESC2         PIC X(20) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-STCAT-STATUS.
           03  WS-STCAT-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(8) VALUE " ".
           03  FILLER         PIC X(50) VALUE
           "** STOCKNUMBER INQUIRY BY CATALOGUE PAGE **".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(8) VALUE "PAGE #".
           03  FILLER         PIC X(16) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(45) VALUE "DESCRIPTION".
           03  FILLER         PIC X(25) VALUE "PRICE  ON HAND".
       01  DETAIL-LINE.
           03  D-PAGE         PIC X(8).
           03  D-STOCKNUMBER  PIC X(16).
           03  D-DESC1        PIC X(20).
           03  D-DESC2        PIC X(21).
           03  D-PRICE        PIC Z(5)9.99.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-QTYONHAND    PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
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
           DISPLAY "** STOCKNUMBER BY CATALOGUE PAGE INQUIRY **" AT POS
           MOVE 0410 TO POS
           DISPLAY "*******************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-020.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-020.
      *
       GET-DATA SECTION.
       GET-000.
            IF WS-TYPE NOT = " "
                MOVE "TYPE"  TO F-FIELDNAME
                MOVE 4       TO F-CBFIELDNAME
                MOVE WS-TYPE TO F-NAMEFIELD
                MOVE 1       TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                GO TO GET-005.
       GET-001.
            MOVE "TYPE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-TYPE.
            IF WS-TYPE NOT = "P" AND NOT = "N" AND NOT = " "
               MOVE
            "ENTRY MUST BE P=PAGE, N=STOCKNUMBER, 'ESC' TO RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-001.
            IF WS-TYPE = " "
                GO TO GET-001.
            IF WS-TYPE = " "
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-005
                PERFORM DISPLAY-FORM
                GO TO GET-001.
       GET-005.
            MOVE "N" TO WS-ANSWER.
            MOVE 0 TO WS-ORDERQTY
                      WS-SHIPQTY.
            MOVE "STOCK" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-001.
            IF F-EXIT-CH = X"07"
                GO TO GET-999.
            IF WS-TYPE = "P"
               MOVE F-NAMEFIELD TO STCAT-PAGE-NUM
            ELSE
               MOVE F-NAMEFIELD TO STCAT-STOCKNUMBER.
            IF F-EXIT-CH = X"0C"
                MOVE WS-STOCKNUMBER TO STCAT-STOCKNUMBER
                PERFORM START-CATALOGUE
                PERFORM READ-CATALOGUE-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-900
             ELSE
               GO TO GET-000.
            IF F-EXIT-CH = X"05"
                PERFORM READ-CATALOGUE-PREVIOUS
             IF WS-END NOT = "Y"
               GO TO GET-900
             ELSE
               GO TO GET-000.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-STOCKNUMBER.
            IF WS-TYPE = "P"
               MOVE F-NAMEFIELD TO STCAT-PAGE-NUM
            ELSE
               MOVE F-NAMEFIELD TO STCAT-STOCKNUMBER.
               
      *      IF WS-TYPE = "N"
      *       IF STCAT-STOCKNUMBER = 0 OR = "   "
      *          GO TO GET-005.
            IF WS-TYPE = "N"
             IF STCAT-STOCKNUMBER = 0 OR = " "
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-005
                PERFORM DISPLAY-FORM
                GO TO GET-005.
            IF WS-TYPE = "P"
             IF STCAT-PAGE-NUM = 0 OR = " "
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-005
                PERFORM DISPLAY-FORM
                GO TO GET-005.
            MOVE " " TO F-EXIT-CH.

            PERFORM READ-TRANSACTIONS.
            IF WS-ANSWER = "Y"
                GO TO GET-999.
       GET-900.
           IF WS-STCAT-ST1 = 88
                GO TO GET-999.
            IF WS-ANSWER = "Y"
                GO TO GET-999.
            IF F-INDEX < 15
               MOVE 2910 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen." AT POS
               MOVE 3010 TO POS
               DISPLAY "Or Press 'F10' To Print All Back Orders"
                  AT POS
               ADD 40 TO POS
               DISPLAY "For This Stock Number." AT POS
               MOVE 15 TO F-INDEX
               PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH NOT = X"07" AND NOT = X"1F" AND NOT = X"04"
                MOVE 1 TO F-INDEX
                GO TO GET-900.
            PERFORM ERROR-020
            PERFORM ERROR1-020.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
                MOVE "Printing In Progress, Please Be Patient."
                   TO WS-MESSAGE
                PERFORM ERROR-000
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE " " TO WS-MESSAGE
                PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-001.
           MOVE 1   TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
           PERFORM START-CATALOGUE.
           MOVE " " TO F-EXIT-CH.
       RDTR-010.
           IF WS-STCAT-ST1 = 88
               MOVE 1 TO F-INDEX
               GO TO RDTR-999.
           IF F-EXIT-CH = " "
            READ STCAT-MASTER NEXT
                 AT END NEXT SENTENCE.
           IF F-EXIT-CH = 1
            READ STCAT-MASTER PREVIOUS
                 AT END NEXT SENTENCE.
           IF F-EXIT-CH = " "
            IF WS-STCAT-ST1 = 10
               MOVE 1 TO F-INDEX
               GO TO RDTR-999.
           IF F-EXIT-CH = 1
            IF WS-STCAT-ST1 = 10
               MOVE 1 TO F-INDEX
               GO TO RDTR-001.
           IF WS-STCAT-ST1 NOT = 0
               MOVE "CATALOGUE FILE BUSY READ-NEXT, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCAT-ST1
               GO TO RDTR-010.
      *     IF WS-TYPE = "P"
      *      IF STCAT-PAGE-NUM NOT = WS-STOCKNUMBER
      *         MOVE "CAT NOT = WS-STOCKNUMBER" TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         MOVE STCAT-PAGE-NUM TO WS-MESSAGE
      *         PERFORM ERROR1-000
      *         MOVE WS-STOCKNUMBER TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         PERFORM ERROR1-020
      *         MOVE "Y" TO WS-ANSWER
      *         MOVE 1 TO F-INDEX
      *         GO TO RDTR-999.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR1-020.
       RDTR-020. 
           IF F-INDEX > 15
                MOVE 2905 TO POS
                DISPLAY "Press 'PgDn' For More, 'PgUp' for Previous,"
                  AT POS
                ADD 44 TO POS
                DISPLAY "OR 'ESC' To Clear The Screen !" AT POS
                MOVE 3010 TO POS
                DISPLAY "Or Press 'F10' To Print INFO" AT POS
                MOVE 15 TO F-INDEX
                PERFORM USER-FILL-FIELD.
            PERFORM ERROR-020
            PERFORM ERROR1-020.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-TRANSACTIONS
                MOVE " " TO F-EXIT-CH
                MOVE 1 TO F-INDEX.
            IF F-EXIT-CH = X"05"
                PERFORM CLEAR-TRANSACTIONS
                MOVE 1 TO F-EXIT-CH
                MOVE 1 TO F-INDEX.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                GO TO RDTR-999.
           IF F-EXIT-CH = X"1F"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                PERFORM ERROR-020
                PERFORM ERROR1-020
                GO TO RDTR-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                    AND NOT = X"07" AND NOT = " "   AND NOT = X"1F"
                    AND NOT = 1
                MOVE 16 TO F-INDEX
                GO TO RDTR-020.

      *     MOVE "GOING TO READ STOCK" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE

           PERFORM READ-STOCK
           PERFORM SCROLLING
           ADD 1 TO F-INDEX
           GO TO RDTR-010.
       RDTR-999.
           EXIT.
      *
       READ-STOCK SECTION.
       RS-000.
            MOVE STCAT-STOCKNUMBER TO ST-STOCKNUMBER
            START STOCK-MASTER KEY NOT < ST-KEY.
       RS-010.
            READ STOCK-MASTER
                INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE "** NOT FOUND ON STOC" TO ST-DESCRIPTION1
                MOVE "K-MASTER FILE **    " TO ST-DESCRIPTION2
                MOVE 0                      TO ST-PRICE
                MOVE 0                      TO ST-QTYONHAND
                GO TO RS-999.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
                GO TO RS-010.
       RS-999.
            EXIT.
      *
       START-CATALOGUE SECTION.
       ST-ST-000.

      *     MOVE "GOING TO START-CAT" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

              IF WS-TYPE = "N"
                 MOVE WS-STOCKNUMBER TO STCAT-STOCKNUMBER
                 START STCAT-MASTER KEY NOT < STCAT-STOCKNUMBER
                    INVALID KEY NEXT SENTENCE
              ELSE
                 MOVE WS-STOCKNUMBER TO STCAT-PAGE-NUM
                 START STCAT-MASTER KEY NOT < STCAT-PAGE-NUM
                    INVALID KEY NEXT SENTENCE.
             IF WS-STCAT-ST1 NOT = 0
                 MOVE 88 TO WS-STCAT-ST1
                 MOVE "NO RECORD WITH THAT NUMBER, 'ESC' TO EXIT."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE.

      *     MOVE "FINISHED START OF CAT" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      *         MOVE STCAT-PAGE-NUM TO WS-MESSAGE
      *         PERFORM ERROR1-000
      *         MOVE WS-STOCKNUMBER TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE.

       ST-ST-999.
             EXIT.
      *
       READ-CATALOGUE-NEXT SECTION.
       R-ST-NX-005. 
             IF WS-STCAT-ST1 = 88
                 GO TO R-ST-NX-999.
             READ STCAT-MASTER NEXT
                 AT END NEXT SENTENCE.
             IF WS-STCAT-ST1 = 10
               MOVE "END OF NEXT-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM START-CATALOGUE
               GO TO R-ST-NX-999.
             
             IF WS-STCAT-ST1 = 0
                 GO TO R-ST-NX-999
             ELSE
               MOVE "CATALOGUE FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-STCAT-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-STCAT-ST1
                 PERFORM START-CATALOGUE
                 GO TO R-ST-NX-005.
       R-ST-NX-999.
             EXIT.
      *
       READ-CATALOGUE-PREVIOUS SECTION.
       RPREV-005. 
             IF WS-STCAT-ST1 = 88
                 GO TO RPREV-999.
             READ STCAT-MASTER PREVIOUS
                 AT END NEXT SENTENCE.
             IF WS-STCAT-ST1 = 10
               MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM START-CATALOGUE
               GO TO RPREV-999.
             
             IF WS-STCAT-ST1 = 0
                 GO TO RPREV-999
             ELSE
               MOVE "CATALOGUE FILE BUSY ON READ-PREV, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-STCAT-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-STCAT-ST1
                 PERFORM START-CATALOGUE
                 GO TO RPREV-005.
       RPREV-999.
             EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-0000.
           IF WS-TYPE = "N"
              MOVE "YOU MAY ONLY PRINT BY PAGE NUMBER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO PRR-999.
           PERFORM ERROR-020
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 3010 TO POS
           DISPLAY "Printing In Progress, Please Be Patient." AT POS
           MOVE 0 TO PAGE-CNT
                     WS-ORDERQTY
                     WS-SHIPQTY.
           MOVE 66 TO LINE-CNT.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE WS-STOCKNUMBER TO STCAT-PAGE-NUM
           START STCAT-MASTER KEY NOT < STCAT-PAGE-NUM
                  INVALID KEY NEXT SENTENCE.
           IF WS-STCAT-ST1 NOT = 0
               MOVE 0 TO WS-STCAT-ST1
               GO TO PRR-900.
       PRR-002.
            READ STCAT-MASTER NEXT
               AT END NEXT SENTENCE.
            IF WS-STCAT-ST1 = 10
               MOVE 0 TO WS-STCAT-ST1
               GO TO PRR-900.
            IF WS-STCAT-ST1 NOT = 0
               MOVE "CATALOGUE FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCAT-ST1
               GO TO PRR-002.
           IF STCAT-PAGE-NUM < WS-STOCKNUMBER
               GO TO PRR-002.
           IF STCAT-PAGE-NUM > WS-STOCKNUMBER
               GO TO PRR-900.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           PERFORM READ-STOCK.
           MOVE STCAT-PAGE-NUM          TO D-PAGE.
           MOVE STCAT-STOCKNUMBER       TO D-STOCKNUMBER
           MOVE ST-DESCRIPTION1         TO D-DESC1
           MOVE ST-DESCRIPTION2         TO D-DESC2
           MOVE ST-QTYONHAND            TO D-QTYONHAND
           MOVE ST-PRICE                TO D-PRICE
           WRITE PRINT-REC FROM DETAIL-LINE

           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
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
            MOVE " " TO PRINT-REC
            MOVE 6 TO LINE-CNT.
       PRR-900.
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
            MOVE "PAGE-NUM"     TO F-FIELDNAME
            MOVE 8              TO F-CBFIELDNAME
            MOVE STCAT-PAGE-NUM TO F-NAMEFIELD
            MOVE 5              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "STOCKNUM"        TO F-FIELDNAME
            MOVE 8                 TO F-CBFIELDNAME
            MOVE STCAT-STOCKNUMBER TO F-NAMEFIELD
            MOVE 20                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.
            
            MOVE "DESC"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            MOVE WS-STDESC   TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ONHAND"     TO F-FIELDNAME
            MOVE 6            TO F-CBFIELDNAME
            MOVE ST-QTYONHAND TO F-EDNAMEFIELDINV
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-INV.

            MOVE "PRICE"    TO F-FIELDNAME
            MOVE 5          TO F-CBFIELDNAME
            MOVE ST-PRICE   TO F-EDNAMEFIELDPRICE
            MOVE 9          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PRICE.
       SCROLL-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO SUB-1 F-INDEX.
       CLTR-010.
            IF SUB-1 > 15
                GO TO CLTR-999.
            MOVE "PAGE-NUM" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 5          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "STOCKNUM" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 15         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESC"        TO F-FIELDNAME
            MOVE 4             TO F-CBFIELDNAME
            MOVE " "           TO F-NAMEFIELD
            MOVE 38            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ONHAND"   TO F-FIELDNAME
            MOVE 6          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 9          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PRICE" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 9       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            ADD 1 TO SUB-1 F-INDEX
            GO TO CLTR-010.
       CLTR-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-001.
            OPEN I-O STCAT-MASTER.
            IF WS-STCAT-ST1 NOT = 0
               MOVE "STCAT-MASTER FILE BUSY ON OPEN 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-001.
       OPEN-005.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK-MASTER FILE BUSY ON OPEN 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
           MOVE Ws-Co-Name to CO-NAME.
       OPEN-006.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StCatPIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STCAT-MASTER
                  STOCK-MASTER.
       END-900.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldPrice".
       Copy "WriteFieldInv".
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
      *
      * END-OF-JOB.
