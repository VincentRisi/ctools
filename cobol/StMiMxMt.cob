        IDENTIFICATION DIVISION.
        PROGRAM-ID. StMiMxMt.
        AUTHOR.  CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectStMaster".
           Copy "SelectSlDaily".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdDaily.
      *
       WORKING-STORAGE SECTION.
       77  WS-WEEKQUES          PIC 99 VALUE 0.
       77  WS-MONTHSOLD         PIC 99V99 VALUE 0.
       77  WS-MONTHSOLD-GENERAL PIC 99V99 VALUE 0.
       77  WS-CATEGORY          PIC X(3) VALUE " ".
       77  WS-ANALYSIS          PIC X VALUE " ".
       77  WS-READ-LYR          PIC X VALUE " ".
       77  WS-INCR-30           PIC X VALUE " ".
       77  WS-MIN-1             PIC X VALUE " ".
       77  WS-WORK-FIELD        PIC 9(5)V99 VALUE 0.
       77  WS-MONTH             PIC 9 VALUE 0.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1     PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST     PIC X(20) VALUE " ".
           03  WS-DAILY-2ND     PIC X(20) VALUE " ".
           03  WS-DAILY-3RD     PIC X(20) VALUE " ".
           03  WS-DAILY-4TH     PIC X(20) VALUE " ".
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
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " "        TO F-NAMEFIELD.
            MOVE "WEEKQUES" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 GO TO GET-999.
            MOVE 2            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-WEEKQUES.
       GET-010.
            MOVE "STOCKCATEGORY" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-000.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-CATEGORY.
            IF WS-CATEGORY NOT = " "
                GO TO GET-020.
            MOVE 
            "BE A SPORT & ENTER SOMETHING FOR CATEGORY, 'ESC' TO RETRY."
               TO WS-MESSAGE.
            PERFORM ERROR-MESSAGE.
            GO TO GET-010.
       GET-020.
            PERFORM READ-STOCK.
            IF WS-CATEGORY NOT = ST-CATEGORY
               MOVE 
             "WHY NOT ENTER A CATEGORY I CAN FIND, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-010.
       GET-025.
            MOVE "LYEAR" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-010.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-READ-LYR.
            IF WS-READ-LYR NOT = "Y" AND NOT = "N"
            MOVE "THIS FIELD MUST BE 'Y' OR 'N', 'ESC' TO RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-025.
       GET-030.
            PERFORM CLEAR-010.
            IF WS-MM = 1 OR = 2
                ADD 12 TO WS-MM.
            COMPUTE WS-MONTHSOLD = WS-MM - 3.
            IF WS-DD > 4
             IF WS-DD < 8
               ADD 0.25 TO WS-MONTHSOLD
               GO TO GET-800.
            IF WS-DD > 7
             IF WS-DD < 15
               ADD 0.5 TO WS-MONTHSOLD
               GO TO GET-800.
            IF WS-DD > 14
             IF WS-DD < 22
               ADD 0.75 TO WS-MONTHSOLD
               GO TO GET-800.
            IF WS-DD > 21
               ADD 1 TO WS-MONTHSOLD.
       GET-800.
      *****************************************************
      *WS-ANALYSIS: Y=MOVE 0 TO MINMAX IF ST-ANALYSIS=N   *
      *             S=MOVE 0 TO "S" ANALYSIS B=BOTH Y & S *
      *****************************************************
           MOVE 
           "B=BOTH 'S' & 'Y', 'S'='S' ANALYSIS ONLY, 'Y'=YES, N=NO."
           TO WS-MESSAGE
           PERFORM ERROR1-000.
           
           MOVE WS-MONTHSOLD TO WS-MONTHSOLD-GENERAL.
           MOVE "ANALYSIS" TO F-FIELDNAME.
           MOVE 8 TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
               GO TO GET-025.
           MOVE 1 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO WS-ANALYSIS.
           IF WS-ANALYSIS NOT = "Y" AND NOT = "N" AND NOT = "B"
                      AND NOT = "S"
              MOVE "ANALYSIS MUST BE 'B', 'S', 'Y' OR 'N', RE-ENTER."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO GET-800.
           PERFORM ERROR1-020.
       GET-805.
      **************************************************************
      *IF WS-INCR-30 = Y INCREASE MIN BY 30%.   THIS FOR 'T' ITEMS *
      **************************************************************
            MOVE "T-ITEM" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-800.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-INCR-30.
            IF WS-INCR-30 NOT = "Y" AND NOT = "N"
            MOVE "THIS FIELD MUST BE EITHER 'Y' OR 'N', RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-805.
       GET-810.
      **************************************************************
      *IF WS-MIN-1 = Y MOVE 0 TO MIN/MAX IF MIN=1                  *
      **************************************************************
            MOVE "ZERO" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-805.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-MIN-1.
            IF WS-MIN-1 NOT = "Y" AND NOT = "N"
            MOVE "THIS FIELD MUST BE EITHER 'Y' OR 'N', RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-810.
       GET-900.
            MOVE 2910 TO POS.
            DISPLAY "Update of Min/Max levels in progress......" AT POS.
            PERFORM UPDATE-STOCK.
       GET-900.
            MOVE " " TO WS-MESSAGE
            MOVE 2701 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE 2901 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE " " TO WS-READ-LYR
                        WS-CATEGORY
                        WS-ANALYSIS.
            MOVE 0   TO WS-WEEKQUES.
       GET-999.
            EXIT.
      *
       CHECK-DATE-CREATED SECTION.
       CDC-010.
      ****************************************************************
      * THIS SECTION IS USED TO CHECK IF THE ITEM WAS CREATED BEFORE *
      * THE BEGINNING OF THIS FINANCIAL YEAR.  IF CREATED DURING THE *
      * YEAR THE WS-MONTHSOLD SHOULD BE ADJUSTED ACCORDINGLY         *
      ****************************************************************
           IF WS-MM < 3
              SUBTRACT 1 FROM WS-YY.
           MOVE WS-DATE TO SPLIT-DATE.
           MOVE 01 TO SPLIT-DD
           MOVE 03 TO SPLIT-MM.
           IF ST-DATE-CREATED NOT > SPLIT-DATE
               MOVE WS-MONTHSOLD-GENERAL TO WS-MONTHSOLD
               GO TO CDC-999.
           MOVE ST-DATE-CREATED TO WS-BEG-DATE.
           COMPUTE WS-MONTHSOLD =
               WS-MONTHSOLD-GENERAL - WS-BEG-MM + 3.
       CDC-999.
           EXIT.
      *
       UPDATE-STOCK SECTION.
       UPST-000.
           PERFORM CHECK-DATE-CREATED.
       
           IF WS-WEEKQUES > 0
               MOVE WS-WEEKQUES TO ST-DEL-DELAY.
           IF ST-ANALYSIS = "D"
               MOVE 0 TO ST-MINIMUMLEVEL
                         ST-MAXIMUMLEVEL
               GO TO UPST-900.
           IF WS-ANALYSIS = "Y"
            IF ST-ANALYSIS = "N"
               MOVE 0 TO ST-MINIMUMLEVEL
                         ST-MAXIMUMLEVEL
               GO TO UPST-900.
           IF WS-ANALYSIS = "B"
            IF ST-ANALYSIS = "N" OR = "S"
               MOVE 0 TO ST-MINIMUMLEVEL
                         ST-MAXIMUMLEVEL
               GO TO UPST-900.
               
           IF WS-ANALYSIS = "S"
            IF ST-ANALYSIS = "S"
             IF ST-SALESUNITSYTD < 12
               MOVE 0 TO ST-MINIMUMLEVEL
                         ST-MAXIMUMLEVEL
             ELSE
               MOVE " " TO ST-ANALYSIS
               GO TO UPST-015.
       UPST-010.
           IF WS-READ-LYR = "Y"
               GO TO UPST-020.
               
           IF ST-SALESUNITSYTD = 0
            IF ST-QTYONBORDER = 0
             IF ST-SALESUNITSLAST = 0
                 MOVE 0 TO ST-MINIMUMLEVEL
                         ST-MAXIMUMLEVEL
              IF ST-ANALYSIS = " "
                 MOVE "S" TO ST-ANALYSIS
                 GO TO UPST-900
              ELSE
                 GO TO UPST-900.
       UPST-015.
           IF ST-SALESUNITSYTD = 0
            IF WS-WEEKQUES > 0
               COMPUTE ST-MINIMUMLEVEL ROUNDED =
                  ((ST-SALESUNITSLAST + ST-QTYONBORDER)
                    / 52 * WS-WEEKQUES)
           COMPUTE ST-MAXIMUMLEVEL ROUNDED = ST-MINIMUMLEVEL * 2
               GO TO UPST-500.
               
           IF ST-SALESUNITSYTD = 0
            IF WS-WEEKQUES NOT > 0
               COMPUTE ST-MINIMUMLEVEL ROUNDED =
                  ((ST-SALESUNITSLAST + ST-QTYONBORDER)
                    / 52 * ST-DEL-DELAY)
           COMPUTE ST-MAXIMUMLEVEL ROUNDED = ST-MINIMUMLEVEL * 2
               GO TO UPST-500.
               
           IF WS-WEEKQUES > 0
            COMPUTE ST-MINIMUMLEVEL ROUNDED = 
              (((ST-SALESUNITSYTD + ST-QTYONBORDER)
                   / WS-MONTHSOLD) * WS-WEEKQUES) / 4.33.
           IF WS-WEEKQUES NOT > 0
            COMPUTE ST-MINIMUMLEVEL ROUNDED = 
              (((ST-SALESUNITSYTD + ST-QTYONBORDER)
                   / WS-MONTHSOLD) * ST-DEL-DELAY) / 4.33.
                   
           COMPUTE ST-MAXIMUMLEVEL ROUNDED = ST-MINIMUMLEVEL * 2.
           
           GO TO UPST-500.
       UPST-020.
           IF WS-READ-LYR = "Y"
            IF WS-WEEKQUES > 0
               COMPUTE ST-MINIMUMLEVEL ROUNDED =
                  (((ST-SALESUNITSLAST + ST-QTYONBORDER)
                   * WS-WEEKQUES) / 52)
               COMPUTE ST-MAXIMUMLEVEL ROUNDED = ST-MINIMUMLEVEL * 2
            ELSE
               COMPUTE ST-MINIMUMLEVEL ROUNDED =
                  (((ST-SALESUNITSLAST + ST-QTYONBORDER)
                   * ST-DEL-DELAY) / 52)
               COMPUTE ST-MAXIMUMLEVEL ROUNDED = ST-MINIMUMLEVEL * 2.
       UPST-500.
            IF WS-INCR-30 = "N"
               GO TO UPST-600.
            IF ST-ANALYSIS = "T" OR = "B"
               COMPUTE ST-MINIMUMLEVEL = ST-MINIMUMLEVEL +
                        ((ST-MINIMUMLEVEL * 30) / 100)
            COMPUTE ST-MAXIMUMLEVEL = ST-MINIMUMLEVEL * 2.
       UPST-600.
            IF WS-MIN-1 = "Y"
             IF ST-MINIMUMLEVEL = 1
               MOVE 0 TO ST-MINIMUMLEVEL
                         ST-MAXIMUMLEVEL.
                         
      *****************************************************************
      * IF ST-MIN>1 THEN ITEM SHOULD NOT BE "S" ANALYSIS              *
      * IF ST-MIN=0 THEN IF ITEM WAS NOT "S" IT SHOULD BE MADE "S" NOW*
      *****************************************************************

            IF ST-MINIMUMLEVEL = 0
               MOVE "S" TO ST-ANALYSIS
               GO TO UPST-900.
            IF ST-MINIMUMLEVEL > 1
             IF ST-ANALYSIS = "S"
               MOVE " " TO ST-ANALYSIS.
       UPST-900.
            REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "LEVEL NOT CHANGED" TO WS-DAILY-1ST
               MOVE ST-STOCKNUMBER      TO WS-DAILY-2ND
               MOVE " "                 TO WS-DAILY-3RD WS-DAILY-4TH
               PERFORM WRITE-DAILY
               GO TO UPST-950.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO UPST-900.
       UPST-950.
            READ STOCK-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-STOCK-ST1 = 10
               GO TO UPST-999.
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
               GO TO UPST-950.
            IF ST-CATEGORY = WS-CATEGORY
               MOVE 2710 TO POS
               DISPLAY "Stock Number Being Read:" AT POS
               ADD 25 TO POS
               DISPLAY ST-STOCKNUMBER AT POS
               GO TO UPST-000.
       UPST-999.
           EXIT.
      *
       READ-STOCK SECTION.
       RS-000.
           CLOSE STOCK-MASTER.
       RS-010.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK FILE BUSY ON OPEM, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RS-010.
       RS-015.
           MOVE WS-CATEGORY TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "@@@" TO ST-CATEGORY
               GO TO RS-999.
           IF WS-STOCK-ST1 NOT = 0
             MOVE "STOCK BUSY ON START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
              GO TO RS-015.
       RS-020.
           READ STOCK-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               MOVE " " TO ST-STOCKNUMBER
                           ST-DESCRIPTION1
                           ST-DESCRIPTION2
               MOVE "@@@" TO ST-CATEGORY
               GO TO RS-999.
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
              GO TO RS-020.
       RS-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
       OPEN-010.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO OPEN-010.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StMiMxMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STOCK-MASTER.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldNumeric".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "GetSystemY2KDate".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "WriteDailyExcep1".
      * END-OF-JOB
