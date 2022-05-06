        IDENTIFICATION DIVISION.
        PROGRAM-ID. StLablRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SPL-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdStock.
       FD  PRINT-FILE.
       01  PRINT-REC               PIC X(250).
      *
       WORKING-STORAGE SECTION.
       77  WS-FOUND             PIC X VALUE " ".
       77  W-CALC               PIC 9(8) VALUE 0.
       77  COL-CNT              PIC 9(3) VALUE 0.
       77  WS-STOCKNUM-BEGIN    PIC X(15) VALUE " ".
       77  WS-STOCKNUM-END      PIC X(15) VALUE " ".
       77  WS-END-OF-FILE       PIC X VALUE " ".
       77  WS-ONHAND-ENTER      PIC X VALUE " ".
       77  WS-QTY-EACH-ITEM     PIC 9(6) VALUE 0.
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-START-POS         PIC 9 VALUE 0.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-Spl-STATUS.
           03  WS-Spl-ST1       PIC 99.
       01  PLINE1.
         02  PLINE1-REC OCCURS 4.
           03  P1-COMP          PIC X(1) VALUE " ".
           03  P-CONAME         PIC X(49) VALUE " ".
       01  PLINE2.
         02  PLINE2-REC OCCURS 4.
           03  P2-BOLD          PIC X.
           03  P-STOCK          PIC X(15) VALUE " ".
           03  P2-UNBOLD        PIC X.
           03  FILLER           PIC X(4) VALUE " ".
           03  P2-COMP          PIC X(1) VALUE " ".
           03  P-DATE           PIC X(15) VALUE " ".
       01  PLINE3.
         02  PLINE3-REC OCCURS 4.
           03  P-DESC1          PIC X(20) VALUE " ".
           03  P-DESC2          PIC X(29) VALUE " ".
       01  PLINE4.
         02  PLINE4-REC OCCURS 4.
           03  P-BINDESC        PIC X(6) VALUE " ".
           03  P-BIN            PIC X(5) VALUE " ".
           03  P-UNITDESC       PIC X(5) VALUE " ".
           03  P-UNIT           PIC X(10) VALUE " ".
           03  P-CAT            PIC X(4) VALUE " ".
           03  P-INVNO          PIC X(19) VALUE " ".
       Copy "WsDateInfo".
       Copy "WStore".
      *
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 320 TO POS
           DISPLAY "** STOCK LABELS BY STOCK NUMBER REPORT **" AT POS
           MOVE 420 TO POS
           DISPLAY "*****************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
           MOVE 2510 TO POS
           DISPLAY "Program loading....." AT POS.
           PERFORM Open-Files.
        CONTROL-020.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           PERFORM PRINT-LABELS.
           GO TO CONTROL-020.
        CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-030.
            MOVE " " TO WS-STOCKNUM-BEGIN
                        WS-STOCKNUM-END.
            MOVE "RANGE1" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF F-EXIT-CH = X"0A"
               GO TO GET-035
            ELSE
               GO TO GET-030.
       GET-035.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO WS-STOCKNUM-BEGIN.
            IF WS-STOCKNUM-BEGIN NOT > " "
                GO TO GET-030.
       GET-040.
            MOVE "RANGE2" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-030.
            IF F-EXIT-CH = X"0A"
               GO TO GET-045
            ELSE
               GO TO GET-040.
       GET-045.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-STOCKNUM-END.
            IF WS-STOCKNUM-END NOT > " "
                GO TO GET-040.
       GET-046.
            MOVE "RANGE5" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-040.
            IF F-EXIT-CH = X"0A"
               GO TO GET-047
            ELSE
               GO TO GET-046.
       GET-047.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ONHAND-ENTER.
            IF WS-ONHAND-ENTER NOT = "N" AND NOT = "Y"
                MOVE "YOU MUST ENTER EITHER Y OR N, PLEASE RE-ENTER"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-046.
            IF WS-ONHAND-ENTER = "Y"
                GO TO GET-080.
       GET-050.
            MOVE "                " TO F-NAMEFIELD.
            MOVE "RANGE3" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-046.
            IF F-EXIT-CH = X"1B" OR = X"0A"
               GO TO GET-055
            ELSE
               GO TO GET-050.
      *****************************
      *QTY TO PRINT OF EACH LABEL *
      *****************************
       GET-055.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-QTY-EACH-ITEM.
            IF WS-QTY-EACH-ITEM < 0 OR > 999999
               MOVE "YOU MUST PRINT BETWEEN 1 AND 999999, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-050.
           PERFORM ERROR-020.
       GET-080.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "RANGE4" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
             IF WS-ONHAND-ENTER = "N"
               GO TO GET-050
             ELSE
               GO TO GET-046.
            IF F-EXIT-CH = X"0A" OR = X"1B"
               GO TO GET-085
            ELSE
               GO TO GET-080.
      *****************************
      *POSITION TO START PRINTING *
      *****************************
       GET-085.
            MOVE 1 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-START-POS.
            IF WS-START-POS NOT = 1 AND NOT = 2 AND NOT = 3 AND NOT = 4
               MOVE
          "START POSITION OF PRINTER MUST BE EITHER 1,2,3 OR 4, RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-START-POS TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-080.
           PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       END-OFF SECTION.
       END-100.
           CLOSE STOCK-MASTER.
       END-900.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
       PRINT-LABELS SECTION.
       PR-000.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           IF WS-Spl-ST1 NOT = 0
               MOVE "Print File Open error, 'ESC' To RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PR-000.
           MOVE " " TO WS-END-OF-FILE.
           MOVE 2510 TO POS
           DISPLAY "The Report is being compiled........." AT POS.
           MOVE WS-STOCKNUM-BEGIN TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
           PERFORM ERROR-020.
       PR-010.
           IF WS-END-OF-FILE = " "
               PERFORM READ-MASTER.
       PR-020.
           WRITE PRINT-REC FROM  PLINE1 AFTER 1
           MOVE " " TO PRINT-REC PLINE1
           WRITE PRINT-REC FROM  PLINE2 AFTER 1
           MOVE " " TO PRINT-REC PLINE2
           WRITE PRINT-REC FROM  PLINE3 AFTER 1
           MOVE " " TO PRINT-REC PLINE3
           WRITE PRINT-REC FROM  PLINE4 AFTER 1
           MOVE " " TO PRINT-REC PLINE4
           WRITE PRINT-REC AFTER 2.
       PR-030.
           IF WS-END-OF-FILE = " "
              GO TO PR-010.
       PR-900.
           MOVE 2510 TO POS
           DISPLAY "                                         " AT POS.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       PR-999.
           EXIT.
      *
       READ-MASTER SECTION.
       RM-000.
           IF WS-START-POS NOT = 1
              PERFORM FILL-IN-BLANKS.
           MOVE WS-START-POS TO SUB-1
           MOVE 0            TO SUB-8.
       RM-010.
           READ STOCK-MASTER NEXT
              AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
              PERFORM END-OFF.
           IF WS-STOCK-ST1 NOT = 0
             MOVE "STOCK BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
              GO TO RM-010.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           IF ST-STOCKNUMBER < WS-STOCKNUM-BEGIN
              GO TO RM-010.
           IF ST-STOCKNUMBER > WS-STOCKNUM-END
              MOVE "1" TO WS-END-OF-FILE
              GO TO RM-999.
              
           MOVE 2810 TO POS
           DISPLAY "Stocknumber Being Read:" AT POS
           ADD 25 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.
              
           IF WS-ONHAND-ENTER = "N"
              GO TO RM-020.
           IF ST-QTYONHAND = 0
              GO TO RM-010.
           IF WS-ONHAND-ENTER = "Y"
              MOVE ST-QTYONHAND   TO WS-QTY-EACH-ITEM.
       RM-020.
           MOVE WS-PRINT-COMP     TO P1-COMP (SUB-1)
                                     P2-COMP (SUB-1)
           MOVE WS-CO-NAME        TO P-CONAME (SUB-1)
           
           MOVE WS-PRINT-BOLD     TO P2-BOLD (SUB-1)
           MOVE ST-STOCKNUMBER    TO P-STOCK (SUB-1)
           MOVE WS-PRINT-UNBOLD   TO P2-UNBOLD (SUB-1)
           MOVE DISPLAY-DATE      TO P-DATE (SUB-1)
           
           MOVE ST-DESCRIPTION1   TO P-DESC1 (SUB-1)
           MOVE ST-DESCRIPTION2   TO P-DESC2 (SUB-1)
           
           MOVE "BIN #:"          TO P-BINDESC (SUB-1)
           MOVE ST-BINLOCATION    TO P-BIN (SUB-1)
           MOVE "PER :"           TO P-UNITDESC (SUB-1)
           MOVE ST-UNITOFMEASURE  TO P-UNIT (SUB-1).
           MOVE ST-CATEGORY       TO P-CAT (SUB-1)
           MOVE "*REPRINT2*"         TO P-INVNO (SUB-1).
           
           IF SUB-1 < 4
              ADD 1 TO SUB-1
           ELSE
              PERFORM PR-020
              MOVE 1 TO SUB-1.
           ADD 1 TO SUB-8.
           IF SUB-8 = WS-QTY-EACH-ITEM
              MOVE 0 TO SUB-8
              GO TO RM-010
           ELSE
              GO TO RM-020.
       RM-999.
           EXIT.
      *
       FILL-IN-BLANKS SECTION.
       FIB-005.
           MOVE 1 TO SUB-1.
       FIB-010.
           MOVE WS-PRINT-COMP     TO P1-COMP (SUB-1)
                                     P2-COMP (SUB-1)
           MOVE WS-PRINT-BOLD     TO P2-BOLD (SUB-1)
           MOVE WS-PRINT-UNBOLD   TO P2-UNBOLD (SUB-1)
           MOVE " "               TO P-CONAME (SUB-1)
                                     P-STOCK (SUB-1)
                                     P-DATE (SUB-1)
                                     P-DESC1 (SUB-1)
                                     P-DESC2 (SUB-1)
                                     P-BINDESC (SUB-1)
                                     P-BIN (SUB-1)
                                     P-UNITDESC (SUB-1)
                                     P-UNIT (SUB-1).
           
           ADD 1 TO SUB-1.
           IF SUB-1 < WS-START-POS
              GO TO FIB-010.
       FIB-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO OPEN-000.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT.
       OPEN-010.
           MOVE WS-FORMS-NAME   TO F-FILENAME
           MOVE WS-CBFORMS-NAME TO F-CBFILENAME.
           MOVE "StLablRp"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       Open-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "GetSystemY2KDate".
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
      *
      * END-OF-JOB
