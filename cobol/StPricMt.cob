        IDENTIFICATION DIVISION.
        PROGRAM-ID. StPricMt.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStockLookup".
          Copy "SelectSlDaily".
          Copy "SelectStCatalogue".
           SELECT PRICE-MASTER ASSIGN TO "/ctools/spl/Prices"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PRICE-KEY
               FILE STATUS IS WS-PRICE-STATUS.
           SELECT PRICE-SP-MASTER ASSIGN TO "/ctools/spl/Prices"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PRICE-SP-KEY
               FILE STATUS IS WS-PRICE-STATUS.
           SELECT PRICE-IMP-MASTER ASSIGN TO "/ctools/spl/PriceSequ"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STOCK-STATUS.
           SELECT PRICE-IMP-SP-MASTER ASSIGN TO 
                                             "/ctools/spl/PriceSequSp"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STOCK-STATUS.
           SELECT STOCK-IMP-MASTER ASSIGN TO "/ctools/spl/StockSequ"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STOCK-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStockLookup.
           COPY ChlfdPriceUpdate.
           COPY ChlfdPriceUpdateSpecial.
           COPY ChlfdPriceImport.
           COPY ChlfdPriceImportSpecial.
           COPY ChlfdStockLookupImport.
           COPY ChlfdDaily.
           COPY ChlfdStCatalogue.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF             PIC X(3) VALUE "   ".
           77  WS-ACCEPT          PIC X(10).
           77  WS-TEMP            PIC X.
           77  WS-DEL-DELAY       PIC 99.
           77  WS-ONHAND          PIC 9(6).
           77  WS-AVECOST         PIC 9(6)V99.
           77  WS-DISCOST         PIC Z(14)9V99.
           77  WS-RANGE1          PIC X(15) VALUE " ".
           77  WS-RANGE2          PIC X(15) VALUE " ".
           77  WS-UP-DESC         PIC X VALUE "N".
           77  WS-UP-COST         PIC X VALUE "Y".
           77  WS-IMP-UPDATE      PIC X VALUE " ".
           77  WS-USE-LOOKUP-DATA PIC X VALUE " ".
           77  WS-FOR-LOC         PIC X VALUE " ".
           01  WS-DESC.
               03  WS-DESC1       PIC X(20) VALUE " ".
               03  WS-DESC2       PIC X(20) VALUE " ".
           01  WS-STOCK-PREFIX.
               03  WS-SP1         PIC X(4) VALUE " ".
               03  WS-SP2         PIC X(11) VALUE " ".
           01  WS-STOCK-CHECKING.
               03  WS-SC1         PIC X(3) VALUE " ".
               03  WS-SC2         PIC X(12) VALUE " ".
           01  WS-STOCK-STATUS.
               03  WS-STOCK-ST1   PIC 99.
           01  WS-LOOK-STATUS.
               03  WS-LOOK-ST1    PIC 99.
           01  WS-PRICE-STATUS.
               03  WS-PRICE-ST1   PIC 99.
           01  WS-DAILY-STATUS.
               03  WS-DAILY-ST1   PIC 99.
           01  WS-STCAT-STATUS.
               03  WS-STCAT-ST1   PIC 99.
               
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      * COPY CHLFDDateConversion.
      *
       PROCEDURE DIVISION USING Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
           PERFORM CLEAR-SCREEN.
           MOVE 0320 TO POS
           DISPLAY "*** STOCK PRICE IMPORT PROGRAM ****" AT POS
           MOVE 0420 TO POS
           DISPLAY "***********************************" AT POS.
       CONTROL-000.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           
      ***************************************************************
      * USED TO IMPORT FROM THE EXCEL FILE <SPL>PRICESEQU.  THIS IS *
      * FOR STOCK ITEMS WHERE THE PART NUMBERS ARE VERY SIMILAR TO  *
      * OUR OWN NUMBERS.  ONLY SAY KEN NEEDS TO BE ADDED IN FRONT OF*
      * THE SUPPLIERS PART NUMBER.                                  *
      ***************************************************************
           IF WS-IMP-UPDATE = "I"
              PERFORM IMPORT-DATA
              PERFORM END-OFF. 
           IF WS-IMP-UPDATE = "Z"
              PERFORM IMPORT-SPEC-DATA
              PERFORM END-OFF. 
           IF WS-IMP-UPDATE = "B"
              PERFORM IMPORT-DATA.
              
      ****************************************************************
      * USED TO WRITE A LOOKUP TABLE WITH OUR PART NUMBERS AND BLANK *
      * SUPPLIER NUMBERS SO THEY CAN BE ENTERED MANUALLY LATER IN AN *
      * EDIT PROGRAM.                                                *
      ****************************************************************
           IF WS-IMP-UPDATE = "W"
              PERFORM WRITE-LOOKUP
              GO TO CONTROL-900.
              
      ****************************************************************
      * USED TO WRITE A LOOKUP TABLE WITH OUR PART NUMBERS TAKEN FROM*
      * SUPPLIER NUMBERS IN AN EXCEL SPREADSHEET, PUT THRU PYTHON &  *
      * THEN USED IN THE UPDATE SECTION OF THIS PROGRAM.             *
      ****************************************************************
           IF WS-IMP-UPDATE = "S"
              PERFORM IMPORT-LOOKUP
              GO TO CONTROL-900.

           IF WS-IMP-UPDATE = "X"              
               PERFORM UPDATE-SPEC-PRICES
           ELSE 
               PERFORM UPDATE-PRICES.
       CONTROL-900.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE " " TO WS-RANGE1 WS-RANGE2.
           MOVE 0401 TO POS
           DISPLAY "USE LOOKUP:" AT POS
           MOVE 0501 TO POS
           DISPLAY "BKM, CK, FLK, GDR, WLR" AT POS
           
           MOVE 0701 TO POS
           DISPLAY "DON'T USE LOOKUP:" AT POS
           MOVE 0801 TO POS
           DISPLAY "BRN, DPR, KEN," AT POS.
           MOVE 0901 TO POS
           DISPLAY "MGL, MT, SCS" AT POS.
       GET-005.
           MOVE 821 TO POS.
           DISPLAY "FROM STOCK NUMBER: [               ]" AT POS
           MOVE 841 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 40        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

           IF W-ESCAPE-KEY = 3 OR 4
              EXIT PROGRAM.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-005.
       GET-010.
           MOVE 921 TO POS.
           DISPLAY "  TO STOCK NUMBER: [               ]" AT POS
           MOVE 941 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 6         TO CDA-ROW.
           MOVE 40        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

           IF W-ESCAPE-KEY = 4
              GO TO GET-005.
            IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-012
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-012.
           MOVE 1410 TO POS
           DISPLAY "I=IMPORT PRICE MUST BE /ctools/spl/PriceSequ" AT POS
           MOVE 1510 TO POS
           DISPLAY "S=LOOKUP TABLE MUST BE /ctools/spl/StockSequ" AT POS
           MOVE 1610 TO POS
           DISPLAY "Z=LOOKUP TABLE MUST BE /ctools/spl/PriceSequSp"
              AT POS
           MOVE 1110 TO POS.
           DISPLAY
           "I=IMPORT, U=UPDATE, B=I & U, W=BLANK Lookup, S=EXCEL"
            AT POS
           MOVE 1210 TO POS.
           DISPLAY
           "LOOKUP, Z=SPECIAL IMPORT, X=CREATE STOCK FILE: [ ]"
            AT POS
           ADD 47 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 57        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-IMP-UPDATE.

           IF W-ESCAPE-KEY = 4
              GO TO GET-010.
           IF WS-IMP-UPDATE NOT = "I" AND NOT = "U" AND NOT = "B"
                        AND NOT = "W" AND NOT = "S" AND NOT = "Z"
                        AND NOT = "X"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-012.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-012.
       GET-015.
           IF WS-IMP-UPDATE = "W" OR = "S" OR = "I" OR = "Z" OR = "X"
               GO TO GET-999.
           MOVE 1810 TO POS
           DISPLAY "UPDATE DESCRIPTIONS ON STOCK FILE : [ ]"
           AT POS
           ADD 37 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-UP-DESC
           
           IF W-ESCAPE-KEY = 4
              GO TO GET-012.
           IF WS-UP-DESC NOT = "N" AND NOT = "Y"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-020
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-015.
       GET-020.
           MOVE 2010 TO POS.
           DISPLAY "F=FOREIGN, L=LOCAL PRICES         : [ ]" AT POS.
           ADD 37 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FOR-LOC.

           IF W-ESCAPE-KEY = 4
              GO TO GET-015.
           IF WS-FOR-LOC NOT = "F" AND NOT = "L"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-025
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-025.
           MOVE "N" TO WS-UP-COST.
           MOVE " " TO ALPHA-RATE.
           MOVE 2210 TO POS.
           DISPLAY "UPDATE LAST COST ON STOCK FILE    : [ ]" AT POS.
           ADD 37 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 19        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-UP-COST.

           IF W-ESCAPE-KEY = 4
              GO TO GET-020.
           IF WS-UP-COST NOT = "N" AND NOT = "Y"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-025.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-025.
       GET-030.
           MOVE " " TO ALPHA-RATE.
           MOVE 2410 TO POS.
           DISPLAY "UPDATE USING LOOKUP TABLE. Y OR N : [ ]" AT POS.
           ADD 37 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 21        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-USE-LOOKUP-DATA.

           IF W-ESCAPE-KEY = 4
              GO TO GET-025.
           IF WS-USE-LOOKUP-DATA NOT = "N" AND NOT = "Y"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-030.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-999
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-999.
            EXIT.
      *
       IMPORT-DATA SECTION.
       ID-000.
           PERFORM CHECK-PREFIX-IN-STOCK.
                 
           MOVE 2510 TO POS
           DISPLAY "STOCK PRICES BEING IMPORTED...." AT POS.
           
           MOVE 0   TO SUB-20.
           MOVE " " TO PRICE-IMP-KEY.
       ID-005.
           READ PRICE-IMP-MASTER
              AT END
                 GO TO ID-900.
       ID-010.
           PERFORM MERGE-PARTS.
           MOVE WS-STOCK-PREFIX            TO PRICE-ST-NUM.
                 
           MOVE 2510 TO POS
           DISPLAY "STOCK ITEM BEING IMPORTED:      " AT POS
           ADD 27 TO POS
           DISPLAY PRICE-ST-NUM AT POS.
           ADD 18 TO POS
           DISPLAY PRICE-IMP-ST-NUM AT POS.
           
           MOVE PRICE-IMP-DESCRIPTION      TO PRICE-DESCRIPTION
           PERFORM ENTER-UNIT-OF-SALE.
           
           MOVE "EACH"                     TO PRICE-UNIT-OF-SALE.
           
           MOVE PRICE-IMP-OLD-DEALER-PRICE TO ALPHA-RATE
           PERFORM REMOVE-LEADING-ZEROS
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE               TO PRICE-OLD-DEALER-PRICE
           
           MOVE PRICE-IMP-NEW-DEALER-PRICE TO ALPHA-RATE
           PERFORM REMOVE-LEADING-ZEROS
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE               TO PRICE-NEW-DEALER-PRICE
           
           MOVE PRICE-IMP-DEALER-DISCOUNT  TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE               TO PRICE-DEALER-DISCOUNT

           MOVE PRICE-IMP-OLD-LIST-PRICE   TO ALPHA-RATE
           PERFORM REMOVE-LEADING-ZEROS
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE               TO PRICE-OLD-LIST-PRICE.

           MOVE PRICE-IMP-NEW-LIST-PRICE   TO ALPHA-RATE
           PERFORM REMOVE-LEADING-ZEROS
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE               TO PRICE-NEW-LIST-PRICE.
       ID-020.
           WRITE PRICE-RECORD.
           IF WS-PRICE-ST1 = 23 OR 35 OR 49
                 MOVE "INVALID WRITE OF PRICE IMPORT, ITEM EXISTS"
                  TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-PRICE-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 GO TO ID-005.
           IF WS-PRICE-ST1 = 22
                 GO TO ID-005.
           IF WS-PRICE-ST1 NOT = 0
                 MOVE "INVALID WRITE OF PRICE IMPORT, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-PRICE-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 GO TO ID-020.

            ADD 1 TO SUB-20.
           
            MOVE 2010 TO POS
            DISPLAY "NUMBER OF RECORDS:" AT POS
            ADD 20 TO POS
            DISPLAY SUB-20 AT POS.

           GO TO ID-005.
       ID-900.
            MOVE "IMPORT FINISHED.........." TO WS-MESSAGE
            PERFORM ERROR-MESSAGE.   
       ID-999.
           EXIT.
      *
       IMPORT-SPEC-DATA SECTION.
       ID-SPEC-000.
           PERFORM CHECK-PREFIX-IN-STOCK.
                 
           MOVE 2410 TO POS
           DISPLAY "STOCK SPECIAL PRICES BEING IMPORTED...." AT POS.
           
           MOVE " " TO PRICE-IMP-SP-KEY.
           MOVE 0   TO SUB-20.
       ID-SPEC-005.
           READ PRICE-IMP-SP-MASTER
              AT END
                 GO TO ID-SPEC-900.
       ID-SPEC-010.
           PERFORM MERGE-PARTS.
           MOVE WS-STOCK-PREFIX            TO PRICE-SP-ST-NUM.
           
           MOVE 2210 TO POS
           DISPLAY "STOCK ITEM BEING IMPORTED:          " AT POS
           ADD 27 TO POS
           DISPLAY PRICE-SP-ST-NUM AT POS.
           ADD 12 TO POS
           DISPLAY PRICE-IMP-SP-ST-NUM AT POS.
           
      *      PERFORM ERROR-010.
           
           MOVE PRICE-IMP-SP-DESCRIPTION      TO PRICE-SP-DESCRIPTION
           PERFORM ENTER-UNIT-OF-SALE.
           
           MOVE PRICE-IMP-SP-OLD-DEALER-PRICE TO PRICE-SP-TARIFF.
           
           MOVE PRICE-IMP-SP-NEW-DEALER-PRICE TO ALPHA-RATE
           PERFORM REMOVE-LEADING-ZEROS
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE               TO PRICE-SP-NEW-DEALER-PRICE.
           
           MOVE PRICE-IMP-SP-DEALER-DISCOUNT  TO PRICE-SP-PAGE-NUMBER.

           MOVE PRICE-IMP-SP-OLD-LIST-PRICE   TO ALPHA-RATE
           PERFORM REMOVE-LEADING-ZEROS
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE               TO PRICE-SP-OLD-LIST-PRICE.

           MOVE PRICE-IMP-SP-OLD-LIST-PRICE   TO ALPHA-RATE
           PERFORM REMOVE-LEADING-ZEROS
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE               TO PRICE-SP-OLD-LIST-PRICE.

           MOVE PRICE-IMP-SP-NEW-LIST-PRICE   TO ALPHA-RATE
           PERFORM REMOVE-LEADING-ZEROS
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE               TO PRICE-SP-NEW-LIST-PRICE.
       ID-SPEC-020.
           WRITE PRICE-SP-RECORD.
           IF WS-PRICE-ST1 = 23 OR 35 OR 49
                 MOVE "INVALID WRITE OF PRICE IMPORT, ITEM EXISTS"
                  TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-PRICE-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 GO TO ID-SPEC-005.
           IF WS-PRICE-ST1 = 22
                 MOVE "ST1=22 @ ID-SPEC-020 ON WRITE." TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE PRICE-SP-ST-NUM TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 GO TO ID-SPEC-005.
           IF WS-PRICE-ST1 NOT = 0
                 MOVE "INVALID WRITE OF PRICE IMPORT, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-PRICE-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 GO TO ID-SPEC-020.

      *      CALL "C$SLEEP" USING 1.

            ADD 1 TO SUB-20.
           
            MOVE 2010 TO POS
            DISPLAY "NUMBER OF RECORDS:" AT POS
            ADD 20 TO POS
            DISPLAY SUB-20 AT POS.

            GO TO ID-SPEC-005.
       ID-SPEC-900.
            MOVE "'Z' TYPE IMPORT FINISHED.........." TO WS-MESSAGE
            PERFORM ERROR-MESSAGE.   
       ID-SPEC-999.
           EXIT.
      *
       REMOVE-LEADING-ZEROS SECTION.
       RLZE-005.
           MOVE SPACES TO DATA-RATE.
           MOVE 1 TO SUB-1.
       RLZE-010.
           IF AL-RATE (SUB-1) NOT = "0"
              GO TO RLZE-015.

           MOVE " " TO AL-RATE (SUB-1).
           IF SUB-1 < 15
              ADD 1 TO SUB-1
              GO TO RLZE-010.
       RLZE-015.
           MOVE 14 TO SUB-1 SUB-2.
       RLZE-020.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2).
           SUBTRACT 1 FROM SUB-1 SUB-2.

           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2).
           SUBTRACT 1 FROM SUB-1 SUB-2.

           MOVE "." TO DAT-RATE (SUB-2).
           SUBTRACT 1 FROM SUB-2.
       RLZE-025.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2).
           
           IF SUB-2 > 1
               SUBTRACT 1 FROM SUB-1 SUB-2
               GO TO RLZE-025.

           MOVE DATA-RATE TO ALPHA-RATE.
       RLZE-999.
           EXIT.
      *
       ENTER-UNIT-OF-SALE SECTION.
       EUOS-005.
           MOVE SPACES                    TO ALPHA-RATE DATA-RATE.
           MOVE PRICE-IMP-SP-UNIT-OF-SALE TO ALPHA-RATE.
           MOVE 7 TO SUB-1
           MOVE 1 TO SUB-2.
       EUOS-010.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2)
           IF SUB-1 < 10
              ADD 1 TO SUB-1 SUB-2
              GO TO EUOS-010.
           MOVE DATA-RATE TO PRICE-UNIT-OF-SALE.
       EUOS-999.
           EXIT.
      *
       FIX-SIZE-OF-NUMBER SECTION.
       FSON-005.
           MOVE SPACES           TO ALPHA-RATE DATA-RATE.
           MOVE PRICE-IMP-SP-KEY TO ALPHA-RATE.
           MOVE 000000           TO DATA-RATE.

      *     MOVE PRICE-IMP-SP-KEY TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      *     MOVE DATA-RATE TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      *     MOVE ALPHA-RATE TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

           MOVE 6 TO SUB-1
                     SUB-2.
       FSON-010.
           IF AL-RATE (SUB-1) = " "
              SUBTRACT 1 FROM SUB-1
              GO TO FSON-010.
       FSON-020.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2).
           IF SUB-1 > 1
              SUBTRACT 1 FROM SUB-1 SUB-2
              GO TO FSON-020.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2).
       FSON-040.
           MOVE DATA-RATE TO PRICE-IMP-SP-KEY.

      *     IF PRICE-IMP-SP-KEY > "X15007"
      *     MOVE "FSON-040" TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE PRICE-IMP-SP-KEY TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     PERFORM ERROR1-020.
       FSON-999.
           EXIT.
      *
       MERGE-PARTS SECTION.
       MP-010.
           MOVE SPACES                 TO ALPHA-RATE DATA-RATE.

           IF WS-IMP-UPDATE = "Z" 
            IF WS-EOF = "DPR"
              PERFORM FIX-SIZE-OF-NUMBER
              MOVE PRICE-IMP-SP-KEY    TO WS-SP2
                                          WS-STOCK-CHECKING
              GO TO MP-020.

           IF WS-EOF = "DPR"
              MOVE PRICE-IMP-KEY          TO PRICE-IMP-SP-KEY
              PERFORM FIX-SIZE-OF-NUMBER
              MOVE PRICE-IMP-SP-KEY       TO PRICE-IMP-KEY
                                             WS-SP2
                                             WS-STOCK-CHECKING
           ELSE 
              MOVE PRICE-IMP-KEY       TO WS-SP2
                                          WS-STOCK-CHECKING.
           MOVE WS-STOCK-PREFIX     TO ALPHA-RATE.
       MP-020.
           MOVE 1 TO SUB-1 SUB-2.
    
         
      *     MOVE "MP-020" TO WS-MESSAGE
      *     PERFORM ERROR1-000.
      *     MOVE WS-EOF TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      *     MOVE WS-SC1 TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
      *     MOVE WS-STOCK-CHECKING TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      *     PERFORM ERROR1-020.

           IF WS-EOF = "BRN"
            IF WS-SC1 = "BRN"
              MOVE SPACES            TO ALPHA-RATE
              MOVE WS-STOCK-CHECKING TO ALPHA-RATE
              MOVE WS-SC1            TO DATA-RATE
              MOVE 4 TO SUB-1
              MOVE 5 TO SUB-2
              GO TO MP-050.

           IF WS-EOF = "DPR"
      *      IF WS-SC1 = "DPR"
            
      *      MOVE "YES" TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE
            
              MOVE SPACE             TO ALPHA-RATE
              MOVE WS-STOCK-CHECKING TO ALPHA-RATE
      *        MOVE PRICE-IMP-SP-KEY  TO ALPHA-RATE
              MOVE WS-EOF            TO DATA-RATE
              MOVE 1 TO SUB-1
              MOVE 5 TO SUB-2
         
      *     MOVE "MP-050" TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE ALPHA-RATE TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     MOVE DATA-RATE TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE

              GO TO MP-050.

           IF WS-EOF = "KEN"
            IF WS-SC1 = "KEN"
              MOVE SPACES            TO ALPHA-RATE
              MOVE WS-STOCK-CHECKING TO ALPHA-RATE
              MOVE WS-SC1            TO DATA-RATE
              MOVE 4 TO SUB-1
              MOVE 5 TO SUB-2
              GO TO MP-050.
           IF WS-EOF = "MGL"
            IF WS-SC1 = "MGL"
              MOVE SPACES            TO ALPHA-RATE
              MOVE WS-STOCK-CHECKING TO ALPHA-RATE
              MOVE WS-SC1            TO DATA-RATE
              MOVE 4 TO SUB-1
              MOVE 5 TO SUB-2
              GO TO MP-050.
           IF WS-EOF = "MT "
            IF WS-SC1 = "MT "
              MOVE SPACES            TO ALPHA-RATE
              MOVE WS-STOCK-CHECKING TO ALPHA-RATE
              MOVE WS-SC1            TO DATA-RATE
              MOVE 3 TO SUB-1
              MOVE 4 TO SUB-2
              GO TO MP-050.
           IF WS-EOF = "SCS"
            IF WS-SC1 = "SCS"
              MOVE SPACES            TO ALPHA-RATE
              MOVE WS-STOCK-CHECKING TO ALPHA-RATE
              MOVE WS-SC1            TO DATA-RATE
              MOVE 4 TO SUB-1
              MOVE 5 TO SUB-2
              GO TO MP-050.
           IF WS-EOF = "BKM" OR = "CK " OR = "FLK" OR = "GDR" OR = "WLR"
              MOVE SPACES            TO ALPHA-RATE
              MOVE WS-STOCK-CHECKING TO ALPHA-RATE
              MOVE SPACES            TO DATA-RATE
              MOVE 1      TO SUB-1
              MOVE 1      TO SUB-2
              GO TO MP-015.
              
            GO TO MP-900.
       MP-015.
            IF SUB-1 < 15
             IF AL-RATE (SUB-1) = " "
               ADD 1 TO SUB-1
               GO TO MP-015.
       MP-050.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2).
           IF SUB-1 < 15
              ADD 1 TO SUB-1 SUB-2
              GO TO MP-050.
           MOVE DATA-RATE TO ALPHA-RATE.
       MP-900.
           MOVE ALPHA-RATE TO WS-STOCK-PREFIX.
           MOVE SPACES     TO ALPHA-RATE.
           
      *     MOVE "MP-900" TO WS-MESSAGE
      *     PERFORM ERROR1-000.
      *     MOVE WS-STOCK-PREFIX TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      *     PERFORM ERROR1-020.
       MP-999.
           EXIT.
      *
       CHECK-PREFIX-IN-STOCK SECTION.
       CPIAS-010.
           MOVE WS-RANGE1 TO WS-EOF.
           IF WS-EOF = "KEN"
             GO TO CPIAS-020.
           IF WS-EOF = "BRN"
             GO TO CPIAS-030.
           IF WS-EOF = "MGL"
             GO TO CPIAS-050.
           IF WS-EOF = "MT "
             GO TO CPIAS-060.
           IF WS-EOF = "SCS"
             GO TO CPIAS-065.
           IF WS-EOF = "DPR"
             GO TO CPIAS-070.

           IF WS-EOF = "BKM" OR = "CK " OR = "FLK" OR = "GDR" OR = "WLR"
              MOVE " " TO WS-STOCK-PREFIX.
           GO TO CPIAS-999.
       CPIAS-020.
           MOVE "KEN " TO WS-STOCK-PREFIX
              GO TO CPIAS-999.
       CPIAS-030.
           MOVE "BRN " TO WS-STOCK-PREFIX
              GO TO CPIAS-999.
       CPIAS-050.
           MOVE "MGL " TO WS-STOCK-PREFIX
              GO TO CPIAS-999.
       CPIAS-060. 
           MOVE "MT " TO WS-STOCK-PREFIX
              GO TO CPIAS-999.
       CPIAS-065. 
           MOVE "SCS" TO WS-STOCK-PREFIX
              GO TO CPIAS-999.
       CPIAS-070.
           MOVE "DPR" TO WS-STOCK-PREFIX
              GO TO CPIAS-999.
       CPIAS-999.
           EXIT.
      *
       IMPORT-LOOKUP SECTION.
       ILUP-000.
           MOVE 2510 TO POS
           DISPLAY "STOCK LOOKUP BEING IMPORTED...." AT POS.
           MOVE WS-RANGE1 TO WS-EOF.
       ILUP-005.
           READ STOCK-IMP-MASTER
              AT END
                 GO TO ILUP-999.
       ILUP-010.
           IF STOCK-IMP-SUPPLIER = " "
               GO TO ILUP-005.
           MOVE STOCK-IMP-ST-NUM   TO STLOOK-STOCKNUMBER
           PERFORM REMOVE-LEADING-ZEROES.
                 
           MOVE 2510 TO POS
           DISPLAY "STOCK ITEM BEING IMPORTED:       " AT POS
           ADD 27 TO POS
           DISPLAY STOCK-IMP-ST-NUM AT POS.
       ILUP-020.
           WRITE STLOOK-RECORD.
           IF WS-LOOK-ST1 = 23 OR 35 OR 49
                 MOVE "INVALID WRITE, STOCK LOOKUP, ITEM EXISTS."
                  TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-LOOK-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 GO TO ILUP-025.
           IF WS-LOOK-ST1 NOT = 0
                 MOVE "INVALID WRITE OF STOCK LOOKUP, 'ESC' TO RETRY." 
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-LOOK-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 GO TO ILUP-020.
           GO TO ILUP-005.
       ILUP-025.
           REWRITE STLOOK-RECORD.
           IF WS-LOOK-ST1 = 23 OR 35 OR 49
                 MOVE "INVALID REWRITE OF STOCK LOOKUP, ITEM NOT THERE."
                  TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-LOOK-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 GO TO ILUP-020.
           IF WS-LOOK-ST1 NOT = 0
                 MOVE "INVALID WRITE OF STOCK LOOKUP, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-LOOK-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 GO TO ILUP-025.
           GO TO ILUP-005.
       ILUP-999.
           EXIT.
      *
       REMOVE-LEADING-ZEROES SECTION.
       RLZ-005.
           MOVE SPACES                TO ALPHA-RATE DATA-RATE
           MOVE STOCK-IMP-SUPPLIER    TO ALPHA-RATE
           MOVE SPACES                TO STLOOK-SUPPLIERNUMBER.
           IF WS-EOF = "WLR"
              MOVE 11 TO SUB-1
              MOVE 1 TO SUB-2
              GO TO RLZ-010.
           IF WS-EOF = "CK "
              MOVE 1 TO SUB-1
                        SUB-2
              GO TO RLZ-008.

              MOVE 1 TO SUB-1
                        SUB-2.
       RLZ-008.
           IF AL-RATE (SUB-1) = "0"
               ADD 1 TO SUB-1
            IF SUB-1 < 30 
               GO TO RLZ-008.
       RLZ-010.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2).
           IF SUB-1 < 30
               ADD 1 TO SUB-1 SUB-2
               GO TO RLZ-010.
       RLZ-900.
           MOVE DATA-RATE TO STLOOK-SUPPLIERNUMBER.
       RLZ-999.
           EXIT.
      *
       WRITE-LOOKUP SECTION.
       WRITE-002.
           MOVE WS-RANGE1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       WRITE-005.
           READ STOCK-MASTER NEXT
              AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
              GO TO WRITE-999.
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
               GO TO WRITE-005.
           IF ST-STOCKNUMBER < WS-RANGE1
              GO TO WRITE-005.
           IF ST-STOCKNUMBER > WS-RANGE2
              GO TO WRITE-999.
              
           MOVE 2620 TO POS
           DISPLAY "StockNumber Being Read:      " AT POS
           ADD 24 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.
       WRITE-010.
      *
      * THIS SECTION ADDED SO THAT WE CAN ADD NEW STOCK NUMBERS TO
      * THE LOOKUP TABLE SO THAT WE CAN UPDATE THE LOOKUP TABLE
      * WITHOUT OVER WRITING WHAT IS IN THE LOOKUP TABLE ALREADY.
      * WRITE-010 THRU WRITE-015.
      *
           MOVE ST-STOCKNUMBER   TO STLOOK-STOCKNUMBER
           START STLOOK-MASTER KEY NOT < STLOOK-KEY
              INVALID KEY NEXT SENTENCE.
       WRITE-015.
           READ STLOOK-MASTER
              INVALID KEY NEXT SENTENCE.
           IF WS-LOOK-ST1 = 23 OR 35 OR 49
              GO TO WRITE-020.
           IF WS-LOOK-ST1 = 0
              GO TO WRITE-050.
           IF WS-LOOK-ST1 NOT = 0
               MOVE "LOOKUP FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-LOOK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-LOOK-ST1
               GO TO WRITE-015.
       WRITE-020.
           MOVE ST-STOCKNUMBER   TO STLOOK-STOCKNUMBER
           MOVE " "              TO STLOOK-SUPPLIERNUMBER.
           WRITE STLOOK-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-LOOK-ST1 = 23 OR 35 OR 49
               GO TO WRITE-050.
           IF WS-LOOK-ST1 NOT = 0
               MOVE 
               "WRITING OF NEW LOOKUP LIST IN ERROR, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-LOOK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO WRITE-020.
       WRITE-050.
           GO TO WRITE-005.
       WRITE-999.
           EXIT.
      *
       READ-LOOKUP-FILE SECTION.
       RLF-005.
           MOVE ST-STOCKNUMBER TO STLOOK-STOCKNUMBER.
           
           START STLOOK-MASTER KEY NOT < STLOOK-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-LOOK-ST1 NOT = 0
                MOVE "UNKNOWN" TO PRICE-KEY
                GO TO RLF-999.
       RLF-010.
           READ STLOOK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-LOOK-ST1 = 23 OR 35 OR 49
                MOVE "UNKNOWN" TO PRICE-KEY
                GO TO RLF-999.
           IF WS-LOOK-ST1 NOT = 0
              MOVE "STLOOKUP FILE BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-LOOK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-LOOK-ST1
               GO TO RLF-010.
                 
           MOVE 2560 TO POS
           DISPLAY STLOOK-SUPPLIERNUMBER AT POS.

           IF STLOOK-SUPPLIERNUMBER = SPACES
               MOVE "UNKNOWN"             TO PRICE-KEY
           ELSE
               MOVE STLOOK-SUPPLIERNUMBER TO PRICE-KEY.
       RLF-999.
           EXIT.
      *
       READ-PRICE-MASTER SECTION.
       RPM-005.
           START PRICE-MASTER KEY NOT < PRICE-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-PRICE-ST1 NOT = 0
                MOVE "UNKNOWN" TO PRICE-KEY
                GO TO RPM-999.
       RPM-010.
           READ PRICE-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-PRICE-ST1 = 23 OR 35 OR 49
                MOVE "UNKNOWN" TO PRICE-KEY
                GO TO RPM-999.
           IF WS-PRICE-ST1 NOT = 0
              MOVE "ST-PRICE FILE BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-PRICE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-PRICE-ST1
               GO TO RPM-010.
                 
           MOVE 2660 TO POS
           DISPLAY PRICE-KEY AT POS.
       RPM-999.
           EXIT.
      *
       READ-PRICE-SP-MASTER SECTION.
       RPM-SP-005.
           START PRICE-SP-MASTER KEY NOT < PRICE-SP-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-PRICE-ST1 NOT = 0
                MOVE "UNKNOWN" TO PRICE-SP-KEY
                GO TO RPM-SP-999.
       RPM-SP-010.
           READ PRICE-SP-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-PRICE-ST1 = 23 OR 35 OR 49
                MOVE "UNKNOWN" TO PRICE-SP-KEY
                GO TO RPM-SP-999.
           IF WS-PRICE-ST1 NOT = 0
              MOVE "ST-PRICE FILE BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-PRICE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-PRICE-ST1
               GO TO RPM-SP-010.
                 
           MOVE 2660 TO POS
           DISPLAY PRICE-SP-KEY AT POS.
       RPM-SP-999.
           EXIT.
      *
        UPDATE-PRICES SECTION.
        UP-000.
           MOVE 2510 TO POS
           DISPLAY "STOCK PRICES BEING UPDATED ........" AT POS.
           MOVE 0 TO SUB-20.

           IF WS-USE-LOOKUP-DATA = "Y"
              MOVE WS-RANGE1 TO ST-KEY
              MOVE " "       TO PRICE-KEY
           ELSE
              MOVE WS-RANGE1 TO PRICE-KEY ST-KEY.
           
           START PRICE-MASTER KEY NOT < PRICE-KEY
                 INVALID KEY
                   MOVE "NO RECORDS ON PRICE LIST FILE " TO WS-MESSAGE
                   PERFORM ERROR-MESSAGE
                   GO TO END-900.
           START STOCK-MASTER KEY NOT < ST-KEY
                 INVALID KEY
                   MOVE "NO RECORDS ON STOCK FILE " TO WS-MESSAGE
                   PERFORM ERROR-MESSAGE
                   GO TO END-900.
        UP-005.
           READ STOCK-MASTER NEXT WITH LOCK
               AT END 
               DISPLAY " FILE AT END, FINISHING"
               GO TO UP-900.
           
           IF ST-STOCKNUMBER < WS-RANGE1
              GO TO UP-005.    
           IF ST-STOCKNUMBER > WS-RANGE2
              GO TO UP-900.
                 
           MOVE 2510 TO POS
           DISPLAY "STOCK ITEM BEING UPDATED:       " AT POS
           ADD 27 TO POS
           DISPLAY ST-KEY AT POS.
           
           IF WS-USE-LOOKUP-DATA = "Y"
              PERFORM READ-LOOKUP-FILE
           ELSE
              MOVE ST-KEY TO PRICE-KEY.
           
           IF WS-USE-LOOKUP-DATA = "Y"
            IF PRICE-KEY = "UNKNOWN"
             MOVE "D" TO ST-ANALYSIS
             GO TO UP-035.

           PERFORM READ-PRICE-MASTER.
      *****************************************************
      * IF THE PRICE RECORD IS NOT VALID THEN WE MOVE "D" *
      * TO ST-ANALYSIS ON STOCK-MASTER.                   *
      *****************************************************
           IF WS-PRICE-ST1 NOT = 0
             MOVE "D" TO ST-ANALYSIS
             GO TO UP-035.
           IF WS-USE-LOOKUP-DATA = "Y"
            IF PRICE-KEY = "UNKNOWN"
             MOVE "D" TO ST-ANALYSIS
            GO TO UP-035.

           IF ST-CATEGORY = "DPR"
              MOVE PRICE-OLD-LIST-PRICE   TO ST-FOREIGNCOST.
           
           IF WS-FOR-LOC = "L"
            IF PRICE-NEW-LIST-PRICE = ST-PRICE
              GO TO UP-025.
              
           IF WS-FOR-LOC = "F"
            IF PRICE-NEW-LIST-PRICE = ST-FOREIGNCOST
              IF PRICE-DEALER-DISCOUNT NOT = ST-SUPPLIERDISC
                 MOVE PRICE-DEALER-DISCOUNT TO ST-SUPPLIERDISC
                 MOVE PRICE-UNIT-OF-SALE    TO ST-UNITOFMEASURE
                 GO TO UP-022
              ELSE
                 GO TO UP-005.
        UP-022.
           IF WS-FOR-LOC = "L"
              MOVE ST-PRICE               TO ST-OLDPRICE
              MOVE PRICE-NEW-LIST-PRICE   TO ST-PRICE
              MOVE WS-DATE                TO ST-LASTPRICECHANGE
              MOVE PRICE-NEW-DEALER-PRICE TO ST-LASTCOST
           ELSE
              MOVE PRICE-NEW-LIST-PRICE   TO ST-FOREIGNCOST.
        UP-025.
           IF WS-UP-DESC = "Y"
              MOVE PRICE-DESCRIPTION     TO WS-DESC
              MOVE WS-DESC1              TO ST-DESCRIPTION1
              MOVE WS-DESC2              TO ST-DESCRIPTION2.

           MOVE PRICE-DEALER-DISCOUNT    TO ST-SUPPLIERDISC.
           MOVE PRICE-UNIT-OF-SALE       TO ST-UNITOFMEASURE.

           IF WS-FOR-LOC = "L"
            IF ST-AVERAGECOST = 0
                 MOVE PRICE-NEW-DEALER-PRICE TO ST-AVERAGECOST.
           IF WS-FOR-LOC = "L"
            IF ST-LASTCOST = 0
                 MOVE PRICE-NEW-DEALER-PRICE TO ST-LASTCOST.
               
           IF WS-FOR-LOC = "L"
            IF WS-UP-COST = "Y"
             IF ST-QTYONHAND = 0   
              IF ST-QTYONRESERVE = 0
               IF PRICE-NEW-DEALER-PRICE NOT = ST-AVERAGECOST
                 MOVE PRICE-NEW-DEALER-PRICE  TO ST-AVERAGECOST.
           IF WS-FOR-LOC = "L"
            IF WS-UP-COST = "Y"
              IF PRICE-NEW-DEALER-PRICE NOT = ST-LASTCOST
                 MOVE PRICE-NEW-DEALER-PRICE  TO ST-LASTCOST.
        UP-035.
           IF ST-UNITOFMEASURE = "0001" OR = "0000" OR = "    "
               MOVE "EACH" TO ST-UNITOFMEASURE.
           REWRITE STOCK-RECORD
                 INVALID KEY
                 MOVE "INVALID REWRITE OF STOCK-RECORD" TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 MOVE WS-STOCK-ST1 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 PERFORM ERROR-MESSAGE.

            ADD 1 TO SUB-20.
           
            MOVE 2610 TO POS
            DISPLAY "NUMBER OF RECORDS:" AT POS
            ADD 20 TO POS
            DISPLAY SUB-20 AT POS.

           GO TO UP-005.
        UP-900.
            MOVE "UPDATE FINISHED.........." TO WS-MESSAGE
            PERFORM ERROR-MESSAGE.   
        UP-999.
             EXIT.
      *
        UPDATE-SPEC-PRICES SECTION.
        UP-SPEC-000.
           MOVE 2510 TO POS
           DISPLAY "STOCK SPECIAL CREATION BEING UPDATED ......" AT POS.
           MOVE WS-RANGE1 TO PRICE-SP-KEY.
           
           START PRICE-SP-MASTER KEY NOT < PRICE-SP-KEY
                 INVALID KEY
                   MOVE "NO RECORDS ON PRICE LIST FILE " TO WS-MESSAGE
                   PERFORM ERROR-MESSAGE
                   GO TO END-900.
           MOVE 0 TO SUB-20.
        UP-SPEC-005.
           READ PRICE-SP-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-PRICE-ST1 = 10
                GO TO UP-SPEC-900.
           IF WS-PRICE-ST1 NOT = 0
              MOVE "ST-PRICE FILE BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-PRICE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-PRICE-ST1
               GO TO UP-SPEC-005.
                 
           MOVE 2510 TO POS
           DISPLAY "STOCK ITEM BEING UPDATED:                  " AT POS
           ADD 26 TO POS
           DISPLAY PRICE-SP-KEY AT POS.
           
           MOVE PRICE-SP-ST-NUM           TO ST-STOCKNUMBER
                                             ST-CATEGORY

           MOVE PRICE-SP-DESCRIPTION      TO WS-DESC
           MOVE WS-DESC1                  TO ST-DESCRIPTION1
           MOVE WS-DESC2                  TO ST-DESCRIPTION2
           
           MOVE "EACH"                    TO ST-UNITOFMEASURE

           MOVE PRICE-SP-TARIFF           TO ST-DUTYTARIFF

           MOVE WS-DATE                   TO ST-LASTPRICECHANGE
                                            
           MOVE PRICE-SP-NEW-DEALER-PRICE TO ST-LASTCOST
                                             ST-AVERAGECOST

           MOVE 0                         TO ST-SUPPLIERDISC
           MOVE "S"                       TO ST-ANALYSIS.

           MOVE PRICE-SP-OLD-LIST-PRICE   TO ST-FOREIGNCOST
           MOVE PRICE-SP-NEW-LIST-PRICE   TO ST-PRICE.

           MOVE "DRAPER" TO ST-SUPPLIER.
           MOVE "PND"    TO ST-CURRENCY.
           MOVE "IMPORT" TO ST-BINLOCATION.
           MOVE "N"      TO ST-PERMIT.

           MOVE WS-DATE  TO ST-DATE-CREATED
                            ST-LASTPRICECHANGE.
           MOVE 25       TO ST-MIN-PERC.

           MOVE 5    TO ST-DISCOUNT1
           MOVE 10   TO ST-DISCOUNT2
           MOVE 15   TO ST-DISCOUNT3 
           MOVE 20   TO ST-DISCOUNT4 
           MOVE 2.5  TO ST-DISCOUNT5 
           MOVE 7.5  TO ST-DISCOUNT6 
           MOVE 12.5 TO ST-DISCOUNT7  
           MOVE 25   TO ST-DISCOUNT8  
           MOVE 30   TO ST-DISCOUNT9.


           MOVE 0 TO ST-OLDPRICE
                     ST-SUPPLIERDISC
                     ST-CURRENCY-RATE
                     ST-MAXIMUMLEVEL
                     ST-MINIMUMLEVEL
                     ST-QTYONHAND 
                     ST-QTYONRESERVE
                     ST-QTYONORDER 
                     ST-QTYONBORDER
                     ST-LASTSALEDATE
                     ST-LASTRECEIPTDATE
                     ST-LASTORDERDATE
                     ST-QTYRECMTD
                     ST-QTYRECYTD  
                     ST-QTYRECLAST  
                     ST-QTYADJMTD 
                     ST-QTYADJYTD 
                     ST-QTYADJLAST
                     ST-SALESUNITMTD
                     ST-SALESUNITSYTD
                     ST-SALESUNITSLAST 
                     ST-SALESRANDSMTD 
                     ST-SALESRANDSYTD 
                     ST-SALESRANDSLAST 
                     ST-SALESCOSTMTD 
                     ST-SALESCOSTYTD
                     ST-SALESCOSTLAST
                     ST-DUTYPERCENT
                     ST-SURCHARGE
                     ST-QTY-ST-TAKE.

                       
           MOVE 1 TO ST-MINBUYQTY
                     ST-DEL-DELAY.
           
        UP-SPEC-035.
           WRITE STOCK-RECORD.
      *     IF WS-STOCK-ST1 NOT = 0
      *        MOVE "STOCK FILE BUSY ON WRITE, 'ESC' TO OMIT"
      *        TO WS-MESSAGE
      *         PERFORM ERROR1-000
      *         MOVE WS-STOCK-ST1 TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         PERFORM ERROR1-020
      *         MOVE ST-STOCKNUMBER TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         .MOVE 0 TO WS-STOCK-ST1
      *         GO TO UP-SPEC-005.
           
      *          INVALID KEY
      *           MOVE "INVALID WRITE OF STOCK-RECORD" TO WS-MESSAGE
      *           PERFORM ERROR-MESSAGE
      *           MOVE WS-STOCK-ST1 TO WS-MESSAGE
      *           PERFORM ERROR1-000
      *           PERFORM ERROR-MESSAGE.
                 
           PERFORM WRITE-CATALOGUE-RECORD.

           ADD 1 TO SUB-20.
           
           MOVE 2210 TO POS
           DISPLAY "NUMBER OF RECORDS:" AT POS
           ADD 20 TO POS
           DISPLAY SUB-20 AT POS.
 
           GO TO UP-SPEC-005.
        UP-SPEC-900.
            MOVE "UPDATE SPECIAL FINISHED.........." TO WS-MESSAGE
            PERFORM ERROR-MESSAGE.   
        UP-SPEC-999.
             EXIT.
      *
       WRITE-CATALOGUE-RECORD SECTION.
       RSR-005.
          MOVE ST-STOCKNUMBER       TO STCAT-STOCKNUMBER.
          MOVE PRICE-SP-PAGE-NUMBER TO STCAT-PAGE-NUM.
       RSR-020.
          WRITE STCAT-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STCAT-ST1 NOT = 0
      *        MOVE "ST-CATALOGUE BUSY ON WRITE, 'ESC' TO RETRY."
      *        TO WS-MESSAGE
      *         PERFORM ERROR1-000
      *         MOVE WS-STCAT-ST1 TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         PERFORM ERROR1-020
               MOVE 0 TO WS-STCAT-ST1
               GO TO RSR-030.
          GO TO RSR-999.
       RSR-030.
          REWRITE STCAT-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STCAT-ST1 NOT = 0
              MOVE "ST-CATALOGUE BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCAT-ST1
               GO TO RSR-020.
       RSR-999.
          EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
             MOVE  "STOCK BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STOCK-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             GO TO OPEN-000.
       OPEN-003.
           IF WS-IMP-UPDATE = "U"
               GO TO OPEN-005.
           IF WS-IMP-UPDATE = "X"
               GO TO OPEN-0051.
           IF WS-IMP-UPDATE = "Z"
               GO TO OPEN-0052.
            OPEN I-O PRICE-IMP-MASTER.
            IF WS-STOCK-ST1 NOT = 0
             MOVE 
             "ERC ON OPENING I-O IMPORT FILE - /ctools/spl/PriceSequ."
              TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STOCK-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             GO TO OPEN-003.
           GO TO OPEN-006.
       OPEN-005.
           OPEN I-O PRICE-MASTER.
           IF WS-PRICE-ST1 NOT = 0
             MOVE "ERC ON OPENING I-O PRICE LIST - /ctools/spl/Prices."
              TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-PRICE-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             GO TO OPEN-005.
           GO TO OPEN-010.
       OPEN-0051.
           OPEN I-O PRICE-SP-MASTER.
           IF WS-PRICE-ST1 NOT = 0
             MOVE "ERC ON OPEN I-O PRICE-SP LIST - /ctools/spl/Prices."
              TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-PRICE-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             GO TO OPEN-0051.
           GO TO OPEN-016.
       OPEN-0052.
            OPEN I-O PRICE-IMP-SP-MASTER.
            IF WS-STOCK-ST1 NOT = 0
             MOVE 
             "ERC ON OPENING I-O IMPORT FILE - /ctools/spl/PriceSequSp."
              TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STOCK-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             GO TO OPEN-0052.
       OPEN-006.
           IF WS-IMP-UPDATE = "Z"
               GO TO OPEN-007.
           OPEN OUTPUT PRICE-MASTER.
           IF WS-PRICE-ST1 NOT = 0
             MOVE 
             "ERC ON OPENING OUTPUT PRICE LIST - /ctools/spl/Prices."
              TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-PRICE-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             GO TO OPEN-006.
           GO TO OPEN-015.
       OPEN-007.
           OPEN OUTPUT PRICE-SP-MASTER.
           IF WS-PRICE-ST1 NOT = 0
             MOVE 
             "ERC ON OPEN OUTPUT PRICE-SP LIST - /ctools/spl/Prices."
              TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-PRICE-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             GO TO OPEN-007.
           GO TO OPEN-015.
       OPEN-010.
           OPEN I-O STLOOK-MASTER.
           IF WS-LOOK-ST1 NOT = 0
             MOVE 
           "ERROR ON OPENING EXCEL FILE - /ctools/data01/StockLookup."
              TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-LOOK-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE WS-STOCKLOOKUP TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             GO TO OPEN-010.
       OPEN-015.
           IF WS-IMP-UPDATE NOT = "S"
               GO TO OPEN-016.
           OPEN I-O STOCK-IMP-MASTER.
           IF WS-STOCK-ST1 NOT = 0
             MOVE "ERROR ON OPENING EXCEL FILE - /spl/StockSequ."
              TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STOCK-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             GO TO OPEN-015.
       OPEN-016.
            OPEN I-O STCAT-MASTER.
            IF WS-STCAT-ST1 NOT = 0
               MOVE 0 TO WS-STCAT-ST1
               MOVE "ST-CATALOGUE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-016.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT.
       OPEN-999.
           EXIT.
      *
        END-OFF SECTION.
        END-000.
           CLOSE STOCK-MASTER
                 PRICE-MASTER
                 PRICE-SP-MASTER
                 PRICE-IMP-MASTER
                 PRICE-IMP-SP-MASTER
                 STCAT-MASTER
                 STLOOK-MASTER.
        END-900.
           EXIT PROGRAM.
        END-999.
              EXIT.
      *
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".

       
      * END-OF-JOB.
