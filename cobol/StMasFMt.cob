        IDENTIFICATION DIVISION.
        PROGRAM-ID. StMasFMt.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStMasterAlpha".
          Copy "SelectStChanges".
          Copy "SelectSlParameter".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStockAlpha.
           COPY ChlfdStockChanges.
           COPY ChlfdParam.
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  WS-BEG-STOCK         PIC X(15) VALUE " ".
       77  WS-END-STOCK         PIC X(15) VALUE " ".
       77  WS-FIELD             PIC X(20) VALUE " ".
       77  WS-NEW-DATA          PIC X(20) VALUE " ".
       77  WS-CHANGE-FILE       PIC X VALUE " ".
       77  NEW-STOCKNO          PIC X VALUE " ".      
       77  WS-MATCH-FIELD       PIC X(20) VALUE " ".
       77  WS-MATCH             PIC 9(8)V999 VALUE 0.
       77  WS-FIELD-MUST-MATCH  PIC X VALUE " ".
       77  WS-WHAT-TYPE         PIC X VALUE " ".
       77  WS-NUMERIC-DISPLAY   PIC Z(7)9.999.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1            PIC 99.
       01  WS-STCHANGE-STATUS.
           03  WS-STCHANGE-ST1      PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1   PIC 99.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 325 TO POS
           DISPLAY "** STOCK FIELD CHANGE PROGRAM **" AT POS.
       CONTROL-005.
           MOVE 401 TO POS
           ADD 5 TO POS.
           DISPLAY "FIELDS TO CHANGE:" AT POS WITH REVERSE-VIDEO
           ADD 17 TO POS.
           MOVE 501 TO POS
           DISPLAY "ST-DESCRIPTION1/DESCRIPTION2" AT POS
           MOVE 601 TO POS
           DISPLAY "ST-CATEGORY" AT POS
           MOVE 701 TO POS
           DISPLAY "ST-SUPPLIER" AT POS
           MOVE 801 TO POS
           DISPLAY "ST-UNITOFMEASURE" AT POS
           MOVE 901 TO POS
           DISPLAY "ST-BINLOCATION" AT POS
           MOVE 1001 TO POS
           DISPLAY "ST-PERMIT" AT POS
           MOVE 1101 TO POS
           DISPLAY "ST-ANALYSIS" AT POS
           MOVE 1201 TO POS
           DISPLAY "ST-FOREIGNCOST" AT POS
           MOVE 1301 TO POS
           DISPLAY "ST-PRICE / ST-CURRENCY-RATE" AT POS
           
           MOVE 1401 TO POS
           DISPLAY "ST-CURRENCY" AT POS
           MOVE 1501 TO POS
           DISPLAY "ST-OLDPRICE" AT POS
           MOVE 1601 TO POS
           DISPLAY "ST-SUPPLIERDISC / ST-MIN-PERC" AT POS

           MOVE 1701 TO POS
           DISPLAY "ST-DISCOUNT1-9" AT POS
           MOVE 1801 TO POS
           DISPLAY "ST-AVERAGECOST" AT POS
           MOVE 1901 TO POS
           DISPLAY "ST-LASTCOST" AT POS
           MOVE 2001 TO POS
           DISPLAY "ST-DUTYPERCENT" AT POS
           MOVE 2101 TO POS
           DISPLAY "ST-SURCHARGE" AT POS
           MOVE 2201 TO POS
           DISPLAY "ST-DUTYTARIFF" AT POS
           MOVE 2301 TO POS
           DISPLAY "ST-MAXIMUMLEVEL" AT POS
           MOVE 2401 TO POS
           DISPLAY "ST-MINIMUMLEVEL" AT POS
           MOVE 2501 TO POS
           DISPLAY "ST-MINBUYQTY" AT POS
           MOVE 2601 TO POS
           DISPLAY "ST-DEL-DELAY" AT POS
           MOVE 2701 TO POS
           DISPLAY "ST-QTYONHAND / ST-QTYONRESERVE" AT POS
           MOVE 2801 TO POS
           DISPLAY "ST-FIELDFIX               " AT POS
           MOVE 535 TO POS
           DISPLAY "FROM STOCK NUMBER :[               ]" AT POS
           MOVE 735 TO POS
           DISPLAY "  TO STOCK NUMBER :[               ]" AT POS
           MOVE 1024 TO POS
           DISPLAY
            "WHAT FIELD SHOULD BE CHANGED :[                    ]"
                 AT POS
           MOVE 1124 TO POS
           DISPLAY "DO YOU WISH TO MATCH OLD DATA:[ ]" AT POS
           MOVE 1330 TO POS
           DISPLAY "NEW DATA FOR THE FIELD :[                    ]"
                 AT POS
           MOVE 1924 TO POS
           DISPLAY "BE FULLY AWARE OF FIELD LENGTHS BEFORE ENTERING."
                 AT POS.

           MOVE " " TO WS-BEG-STOCK WS-END-STOCK WS-FIELD WS-NEW-DATA.
       CONTROL-010.
           MOVE " " TO WS-MESSAGE
           MOVE 3001 TO POS
           DISPLAY WS-MESSAGE AT POS.
           PERFORM OPEN-FILES.
           
           MOVE "N" TO WS-CHANGE-FILE.
       CONTROL-015.
           PERFORM GET-DATA.
           IF WS-WHAT-TYPE = "F"
              PERFORM PRINT-ALPHA-ROUTINE
           ELSE
              PERFORM PRINT-ROUTINE.
           MOVE " " TO WS-MESSAGE
           MOVE 2701 TO POS
           DISPLAY WS-MESSAGE AT POS
           GO TO CONTROL-015.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE 535 TO POS
            DISPLAY "FROM STOCK NUMBER :[               ]" AT POS
            MOVE 555 TO POS
            DISPLAY WS-BEG-STOCK AT POS

           MOVE WS-BEG-STOCK TO CDA-DATA.
           MOVE 15           TO CDA-DATALEN.
           MOVE 2            TO CDA-ROW.
           MOVE 54           TO CDA-COL.
           MOVE CDA-WHITE    TO CDA-COLOR.
           MOVE 'F'          TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-BEG-STOCK.

            IF W-ESCAPE-KEY = 3
               PERFORM END-OFF.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-010.
            MOVE 735 TO POS
            DISPLAY "  TO STOCK NUMBER :[               ]" AT POS
            MOVE 755 TO POS
            DISPLAY WS-END-STOCK AT POS

           MOVE WS-END-STOCK TO CDA-DATA.
           MOVE 15           TO CDA-DATALEN.
           MOVE 4            TO CDA-ROW.
           MOVE 54           TO CDA-COL.
           MOVE CDA-WHITE    TO CDA-COLOR.
           MOVE 'F'          TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-END-STOCK.

            IF W-ESCAPE-KEY = 7
               MOVE WS-BEG-STOCK TO WS-END-STOCK
               DISPLAY WS-END-STOCK AT POS
               GO TO GET-015.
            IF W-ESCAPE-KEY = 4
               GO TO GET-000.
            IF WS-END-STOCK = " "
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-015.
            MOVE 1024 TO POS
            DISPLAY
            "WHAT FIELD SHOULD BE CHANGED :[                    ]"
                   AT POS
            MOVE 1055 TO POS
            DISPLAY WS-FIELD AT POS

           MOVE WS-FIELD  TO CDA-DATA.
           MOVE 20        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FIELD.

            IF W-ESCAPE-KEY = 4
               GO TO GET-010.
            IF WS-FIELD = " "
               GO TO GET-015.
            IF WS-FIELD = "ST-STOCKNUMBER"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-0155
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
        GET-0155.
            IF WS-FIELD = "ST-FIELDFIX"
                MOVE "F" TO WS-WHAT-TYPE
                GO TO GET-500.
            IF WS-FIELD = "ST-DESCRIPTION1"  OR = "ST-DESCRIPTION2"
                      OR = "ST-CATEGORY"      OR = "ST-SUPPLIER"
                      OR = "ST-UNITOFMEASURE" OR = "ST-BINLOCATION"
                      OR = "ST-PERMIT"        OR = "ST-ANALYSIS"
                      OR = "ST-CURRENCY"
               MOVE "A" TO WS-WHAT-TYPE
               GO TO GET-016.
            IF WS-FIELD = "ST-FOREIGNCOST"  OR = "ST-PRICE"
                      OR = "ST-SUPPLIERDISC"
                      OR = "ST-DISCOUNT1"    OR = "ST-DISCOUNT2"
                      OR = "ST-DISCOUNT3"    OR = "ST-DISCOUNT4"
                      OR = "ST-DISCOUNT5"    OR = "ST-DISCOUNT6"
                      OR = "ST-DISCOUNT7"    OR = "ST-DISCOUNT8"
                      OR = "ST-CURRENCY-RATE"
                      OR = "ST-DISCOUNT9"    OR = "ST-AVERAGECOST"
                      OR = "ST-LASTCOST"     OR = "ST-DUTYPERCENT"
                      OR = "ST-DUTYTARIFF"   OR = "ST-MAXIMUMLEVEL"
                      OR = "ST-MINIMUMLEVEL" OR = "ST-SURCHARGE"
                      OR = "ST-MINBUYQTY"    OR = "ST-OLDPRICE"
                      OR = "ST-DEL-DELAY"    OR = "ST-MIN-PERC"
                      OR = "ST-QTYONHAND"    OR = "ST-QTYONRESERVE"
               MOVE "N" TO WS-WHAT-TYPE
               GO TO GET-016.
            MOVE 2230 TO POS
            DISPLAY "The Field Entered CANNOT Be Changed." AT POS
            GO TO GET-015.
       GET-016.
            MOVE 1124 TO POS
            DISPLAY "DO YOU WISH TO MATCH OLD DATA:[ ]" AT POS
            MOVE 1155 TO POS
            DISPLAY WS-FIELD-MUST-MATCH AT POS

           MOVE WS-FIELD-MUST-MATCH TO CDA-DATA.
           MOVE 1                   TO CDA-DATALEN.
           MOVE 8                   TO CDA-ROW.
           MOVE 54                  TO CDA-COL.
           MOVE CDA-WHITE           TO CDA-COLOR.
           MOVE 'F'                 TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FIELD-MUST-MATCH.

            IF W-ESCAPE-KEY = 4
               GO TO GET-015.
            IF WS-FIELD-MUST-MATCH NOT = "Y" AND NOT = "N"
               GO TO GET-016.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-017
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-016.
       GET-017.
            IF WS-FIELD-MUST-MATCH = "N"
                  GO TO GET-020.
            MOVE 1224 TO POS
            DISPLAY
             "WHAT SHOULD THE OLD DATA BE  :[                    ]"
                  AT POS
            MOVE 1255 TO POS
            DISPLAY WS-MATCH-FIELD AT POS

           MOVE WS-MATCH-FIELD TO CDA-DATA.
           MOVE 20             TO CDA-DATALEN.
           MOVE 9              TO CDA-ROW.
           MOVE 54             TO CDA-COL.
           MOVE CDA-WHITE      TO CDA-COLOR.
           MOVE 'F'            TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-MATCH-FIELD.

            IF W-ESCAPE-KEY = 4
               GO TO GET-016.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-019
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-017.
       GET-019.
            IF WS-WHAT-TYPE = "A"
               GO TO GET-020.
            MOVE WS-MATCH-FIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE.
            IF SIGN-FOUND = 1
                MOVE "NO FIELDS CAN BE NEGATIVE, RE-ENTER" TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE " " TO WS-MATCH-FIELD
                GO TO GET-017.
            MOVE NUMERIC-RATE TO WS-NUMERIC-DISPLAY WS-MATCH.
            MOVE 1255 TO POS.
            DISPLAY WS-NUMERIC-DISPLAY AT POS.
       GET-020.
           MOVE 1330 TO POS
           DISPLAY "NEW DATA FOR THE FIELD :[                    ]"
                  AT POS
           MOVE 1924 TO POS
           DISPLAY "BE FULLY AWARE OF FIELD LENGTHS BEFORE ENTERING."
               AT POS
           MOVE 1355 TO POS
           DISPLAY WS-NEW-DATA AT POS

           MOVE WS-NEW-DATA TO CDA-DATA.
           MOVE 20          TO CDA-DATALEN.
           MOVE 10          TO CDA-ROW.
           MOVE 54          TO CDA-COL.
           MOVE CDA-WHITE   TO CDA-COLOR.
           MOVE 'F'         TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-NEW-DATA.

           IF W-ESCAPE-KEY = 4
              GO TO GET-016.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-300
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-300.
            IF WS-WHAT-TYPE = "A"
               GO TO GET-500.
            MOVE 2230 TO POS
            DISPLAY "                                    " AT POS
            MOVE WS-NEW-DATA TO ALPHA-RATE
            PERFORM DECIMALISE-RATE.
            IF SIGN-FOUND = 1
                MOVE "NO FIELDS CAN BE NEGATIVE, RE-ENTER" TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE " " TO WS-NEW-DATA
                GO TO GET-020.
            MOVE NUMERIC-RATE TO WS-NUMERIC-DISPLAY
           MOVE 1355 TO POS
           DISPLAY WS-NUMERIC-DISPLAY AT POS.
       GET-500.
           MOVE 1730 TO POS
           DISPLAY "UPDATE CHANGE FILE     :[ ]" AT POS
           MOVE 1755 TO POS
           DISPLAY WS-CHANGE-FILE AT POS

           MOVE WS-CHANGE-FILE TO CDA-DATA.
           MOVE 1              TO CDA-DATALEN.
           MOVE 14             TO CDA-ROW.
           MOVE 54             TO CDA-COL.
           MOVE CDA-WHITE      TO CDA-COLOR.
           MOVE 'F'            TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CHANGE-FILE.

           IF WS-CHANGE-FILE NOT = "N" AND NOT = "Y"
               MOVE "THIS FIELD MUST BE 'N' OR 'Y' RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-500.
            IF W-ESCAPE-KEY = 4
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-600
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-500.
       GET-600.
            MOVE " " TO WS-MESSAGE
            MOVE 3001 TO POS
            DISPLAY WS-MESSAGE AT POS
            PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE WS-BEG-STOCK TO ST-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
       PRR-002.
            READ STOCK-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-STOCK-ST1 = 10
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRR-999.
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
               GO TO PRR-002.
            IF ST-STOCKNUMBER < WS-BEG-STOCK
               GO TO PRR-002.
            IF ST-STOCKNUMBER > WS-END-STOCK
               MOVE ST-STOCKNUMBER TO WS-BEG-STOCK
               GO TO PRR-999.
               
            MOVE 2725 TO POS
            DISPLAY "StockNumber Being Changed:" AT POS
            ADD 27 TO POS
            DISPLAY ST-STOCKNUMBER AT POS.
            
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       PRR-010.
            IF WS-WHAT-TYPE = "N"
               GO TO PRR-025.
            IF WS-FIELD = "ST-DESCRIPTION1"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-DESCRIPTION1 = WS-MATCH-FIELD
               MOVE WS-NEW-DATA TO ST-DESCRIPTION1
               GO TO PRR-050.
            IF WS-FIELD = "ST-DESCRIPTION1"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE WS-NEW-DATA TO ST-DESCRIPTION1
               GO TO PRR-050.
            IF WS-FIELD = "ST-DESCRIPTION2"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-DESCRIPTION2 = WS-MATCH-FIELD
               MOVE WS-NEW-DATA TO ST-DESCRIPTION2
               GO TO PRR-050.
            IF WS-FIELD = "ST-DESCRIPTION2"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE WS-NEW-DATA TO ST-DESCRIPTION2
               GO TO PRR-050.
            IF WS-FIELD = "ST-CATEGORY"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-CATEGORY = WS-MATCH-FIELD
               MOVE WS-NEW-DATA TO ST-CATEGORY
               GO TO PRR-050.
            IF WS-FIELD = "ST-CATEGORY"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE WS-NEW-DATA TO ST-CATEGORY
               GO TO PRR-050.
            IF WS-FIELD = "ST-SUPPLIER"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-SUPPLIER = WS-MATCH-FIELD
               MOVE WS-NEW-DATA TO ST-SUPPLIER
               GO TO PRR-050.
            IF WS-FIELD = "ST-SUPPLIER"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE WS-NEW-DATA TO ST-SUPPLIER
               GO TO PRR-050.
            IF WS-FIELD = "ST-CURRENCY"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-CURRENCY = WS-MATCH-FIELD
               MOVE WS-NEW-DATA TO ST-CURRENCY
               GO TO PRR-050.
            IF WS-FIELD = "ST-CURRENCY"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE WS-NEW-DATA TO ST-CURRENCY
               GO TO PRR-050.
            IF WS-FIELD = "ST-UNITOFMEASURE"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-UNITOFMEASURE = WS-MATCH-FIELD
               MOVE WS-NEW-DATA TO ST-UNITOFMEASURE
               GO TO PRR-050.
            IF WS-FIELD = "ST-UNITOFMEASURE"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE WS-NEW-DATA TO ST-UNITOFMEASURE
               GO TO PRR-050.
            IF WS-FIELD = "ST-BINLOCATION"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-BINLOCATION = WS-MATCH-FIELD
               MOVE WS-NEW-DATA TO ST-BINLOCATION
               GO TO PRR-050.
            IF WS-FIELD = "ST-BINLOCATION"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE WS-NEW-DATA TO ST-BINLOCATION
               GO TO PRR-050.
            IF WS-FIELD = "ST-PERMIT"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-PERMIT = WS-MATCH-FIELD
               MOVE WS-NEW-DATA TO ST-PERMIT
               GO TO PRR-050.
            IF WS-FIELD = "ST-PERMIT"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE WS-NEW-DATA TO ST-PERMIT
               GO TO PRR-050.
            IF WS-FIELD = "ST-ANALYSIS"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-ANALYSIS = WS-MATCH-FIELD
               MOVE WS-NEW-DATA TO ST-ANALYSIS
               GO TO PRR-050.
            IF WS-FIELD = "ST-ANALYSIS"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE WS-NEW-DATA TO ST-ANALYSIS
               GO TO PRR-050.
            GO TO PRR-002.
       PRR-025.
            IF WS-FIELD = "ST-FOREIGNCOST"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-FOREIGNCOST = WS-MATCH
               MOVE NUMERIC-RATE TO ST-FOREIGNCOST
               GO TO PRR-050.
            IF WS-FIELD = "ST-FOREIGNCOST"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-FOREIGNCOST
               GO TO PRR-050.
            IF WS-FIELD = "ST-CURRENCY-RATE"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-FOREIGNCOST = WS-MATCH
               MOVE NUMERIC-RATE TO ST-CURRENCY-RATE
               GO TO PRR-050.
            IF WS-FIELD = "ST-CURRENCY-RATE"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-CURRENCY-RATE
               GO TO PRR-050.
            IF WS-FIELD = "ST-SUPPLIERDISC"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-SUPPLIERDISC
               GO TO PRR-050.
            IF WS-FIELD = "ST-SUPPLIERDISC"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-SUPPLIERDISC = WS-MATCH
               MOVE NUMERIC-RATE TO ST-SUPPLIERDISC
               GO TO PRR-050.
            IF WS-FIELD = "ST-PRICE"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-PRICE = WS-MATCH
               MOVE NUMERIC-RATE TO ST-PRICE
               GO TO PRR-050.
            IF WS-FIELD = "ST-PRICE"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-PRICE
               GO TO PRR-050.
            IF WS-FIELD = "ST-OLDPRICE"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-OLDPRICE = WS-MATCH
               MOVE NUMERIC-RATE TO ST-OLDPRICE
               GO TO PRR-050.
            IF WS-FIELD = "ST-OLDPRICE"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-OLDPRICE
               GO TO PRR-050.
            IF WS-FIELD = "ST-MIN-PERC"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-MIN-PERC = WS-MATCH
               MOVE NUMERIC-RATE TO ST-MIN-PERC
               GO TO PRR-050.
            IF WS-FIELD = "ST-MIN-PERC"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-MIN-PERC
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT1"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-DISCOUNT1 = WS-MATCH
               MOVE NUMERIC-RATE TO ST-DISCOUNT1
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT1"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-DISCOUNT1
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT2"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-DISCOUNT2 = WS-MATCH
               MOVE NUMERIC-RATE TO ST-DISCOUNT2
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT2"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-DISCOUNT2
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT3"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-DISCOUNT3 = WS-MATCH
               MOVE NUMERIC-RATE TO ST-DISCOUNT3
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT3"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-DISCOUNT3
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT4"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-DISCOUNT4 = WS-MATCH
               MOVE NUMERIC-RATE TO ST-DISCOUNT4
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT4"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-DISCOUNT4
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT5"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-DISCOUNT5 = WS-MATCH
               MOVE NUMERIC-RATE TO ST-DISCOUNT5
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT5"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-DISCOUNT5
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT6"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-DISCOUNT6 = WS-MATCH
               MOVE NUMERIC-RATE TO ST-DISCOUNT6
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT6"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-DISCOUNT6
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT7"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-DISCOUNT7 = WS-MATCH
               MOVE NUMERIC-RATE TO ST-DISCOUNT7
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT7"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-DISCOUNT7
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT8"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-DISCOUNT8 = WS-MATCH
               MOVE NUMERIC-RATE TO ST-DISCOUNT8
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT8"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-DISCOUNT8
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT9"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-DISCOUNT9 = WS-MATCH
               MOVE NUMERIC-RATE TO ST-DISCOUNT9
               GO TO PRR-050.
            IF WS-FIELD = "ST-DISCOUNT9"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-DISCOUNT9
               GO TO PRR-050.
            IF WS-FIELD = "ST-AVERAGECOST"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-AVERAGECOST = WS-MATCH
               MOVE NUMERIC-RATE TO ST-AVERAGECOST
               GO TO PRR-050.
            IF WS-FIELD = "ST-AVERAGECOST"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-AVERAGECOST
               GO TO PRR-050.
            IF WS-FIELD = "ST-LASTCOST"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-LASTCOST = WS-MATCH
               MOVE NUMERIC-RATE TO ST-LASTCOST
               GO TO PRR-050.
            IF WS-FIELD = "ST-LASTCOST"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-LASTCOST
               GO TO PRR-050.
            IF WS-FIELD = "ST-DUTYPERCENT"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-DUTYPERCENT = WS-MATCH
               MOVE NUMERIC-RATE TO ST-DUTYPERCENT
               GO TO PRR-050.
            IF WS-FIELD = "ST-DUTYPERCENT"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-DUTYPERCENT
               GO TO PRR-050.
            IF WS-FIELD = "ST-SURCHARGE"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-SURCHARGE = WS-MATCH
               MOVE NUMERIC-RATE TO ST-SURCHARGE
               GO TO PRR-050.
            IF WS-FIELD = "ST-SURCHARGE"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-SURCHARGE
               GO TO PRR-050.
            IF WS-FIELD = "ST-DUTYTARIFF"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-DUTYTARIFF = WS-MATCH
               MOVE NUMERIC-RATE TO ST-DUTYTARIFF
               GO TO PRR-050.
            IF WS-FIELD = "ST-DUTYTARIFF"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-DUTYTARIFF
               GO TO PRR-050.
            IF WS-FIELD = "ST-MAXIMUMLEVEL"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-MAXIMUMLEVEL = WS-MATCH
               MOVE NUMERIC-RATE TO ST-MAXIMUMLEVEL
               GO TO PRR-050.
            IF WS-FIELD = "ST-MAXIMUMLEVEL"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-MAXIMUMLEVEL
               GO TO PRR-050.
            IF WS-FIELD = "ST-MINIMUMLEVEL"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-MINIMUMLEVEL = WS-MATCH
               MOVE NUMERIC-RATE TO ST-MINIMUMLEVEL
               GO TO PRR-050.
            IF WS-FIELD = "ST-MINIMUMLEVEL"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-MINIMUMLEVEL
               GO TO PRR-050.
            IF WS-FIELD = "ST-MINBUYQTY"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-MINBUYQTY = WS-MATCH
               MOVE NUMERIC-RATE TO ST-MINBUYQTY
               GO TO PRR-050.
            IF WS-FIELD = "ST-MINBUYQTY"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-MINBUYQTY
               GO TO PRR-050.
            IF WS-FIELD = "ST-DEL-DELAY"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-DEL-DELAY = WS-MATCH
               MOVE NUMERIC-RATE TO ST-DEL-DELAY
               GO TO PRR-050.
            IF WS-FIELD = "ST-DEL-DELAY"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-DEL-DELAY
               GO TO PRR-050.
            IF WS-FIELD = "ST-QTYONHAND"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-QTYONHAND = WS-MATCH
               MOVE NUMERIC-RATE TO ST-QTYONHAND
               GO TO PRR-050.
            IF WS-FIELD = "ST-QTYONHAND"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-QTYONHAND
               GO TO PRR-050.
            IF WS-FIELD = "ST-QTYONRESERVE"
             IF WS-FIELD-MUST-MATCH = "Y"
              IF ST-QTYONRESERVE = WS-MATCH
               MOVE NUMERIC-RATE TO ST-QTYONRESERVE
               GO TO PRR-050.
            IF WS-FIELD = "ST-QTYONRESERVE"
             IF WS-FIELD-MUST-MATCH = "N"
               MOVE NUMERIC-RATE TO ST-QTYONRESERVE
               GO TO PRR-050.
            GO TO PRR-002.
       PRR-050.
            REWRITE STOCK-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK RECORD BUSY ON RE-WRITE" TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE ST-STOCKNUMBER TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK-ST1
                GO TO PRR-050.
           PERFORM ERROR-020.
       PRR-900.
           IF INVQUES-STOCK-CHANGE = "Y"
            IF WS-CHANGE-FILE = "Y"
               PERFORM WRITE-STOCK-CHANGES.
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       PRINT-ALPHA-ROUTINE SECTION.
       PRA-000.
            MOVE WS-BEG-STOCK TO ST-AL-STOCKNUMBER.
            START STOCK-ALPHA-MASTER KEY NOT < ST-AL-KEY
               INVALID KEY NEXT SENTENCE.
       PRA-002.
            READ STOCK-ALPHA-MASTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-STOCK-ST1 = 10
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRA-999.
            IF WS-STOCK-ST1 NOT = 0
             MOVE "ST-ALPHA BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRA-002.
            IF ST-AL-STOCKNUMBER < WS-BEG-STOCK
               GO TO PRA-002.
            IF ST-AL-STOCKNUMBER > WS-END-STOCK
               MOVE ST-STOCKNUMBER TO WS-BEG-STOCK
               GO TO PRA-999.
               
            MOVE 3125 TO POS
            DISPLAY "StockNumber Being Changed:" AT POS
            ADD 27 TO POS
            DISPLAY ST-AL-STOCKNUMBER AT POS.
            
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
            MOVE 1701 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE 1801 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE 1901 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE 2001 TO POS
            DISPLAY WS-MESSAGE AT POS.
               
       PRA-025.
            MOVE 1740 TO POS
            DISPLAY ST-AL-MINBUYQTY AT POS
            MOVE 1840 TO POS
            DISPLAY ST-AL-DEL-DELAY AT POS
            MOVE 1940 TO POS
            DISPLAY ST-AL-MAXIMUMLEVEL AT POS
            MOVE 2040 TO POS
            DISPLAY ST-AL-QTY-ST-TAKE AT POS
            MOVE 2140 TO POS
            DISPLAY ST-AL-QTYRECMTD AT POS
            MOVE 2240 TO POS
            DISPLAY ST-AL-QTYRECYTD AT POS.
            MOVE 2340 TO POS
            DISPLAY ST-AL-SALESRANDSMTD AT POS
            MOVE 2440 TO POS
            DISPLAY ST-AL-SALESRANDSYTD AT POS
            MOVE 2540 TO POS
            DISPLAY ST-AL-SALESRANDSLAST AT POS
            MOVE 2640 TO POS
            DISPLAY ST-AL-SALESCOSTMTD AT POS
            MOVE 2740 TO POS
            DISPLAY ST-AL-SALESCOSTYTD AT POS
            MOVE 2840 TO POS
            DISPLAY ST-AL-SALESCOSTLAST AT POS
       
            IF ST-AL-MINBUYQTY = " "
               MOVE "000000" TO ST-AL-MINBUYQTY.
            IF ST-AL-DEL-DELAY = " "
               MOVE "00" TO ST-AL-DEL-DELAY.
            IF ST-AL-MAXIMUMLEVEL = " "
               MOVE "000000" TO ST-AL-MAXIMUMLEVEL.
            IF ST-AL-MINIMUMLEVEL = " "
               MOVE "000000" TO ST-AL-MINIMUMLEVEL.
            IF ST-AL-QTY-ST-TAKE = " "
               MOVE "000000" TO ST-AL-QTY-ST-TAKE.
            IF ST-AL-DATE-CREATED = " "
               MOVE "000000" TO ST-AL-DATE-CREATED.
            IF ST-AL-LASTPRICECHANGE = " "
               MOVE "00000000" TO ST-AL-LASTPRICECHANGE.
            IF ST-AL-LASTSALEDATE = " "
               MOVE "00000000" TO ST-AL-LASTSALEDATE.
            IF ST-AL-LASTRECEIPTDATE = " "
               MOVE "00000000" TO ST-AL-LASTRECEIPTDATE.
            IF ST-AL-LASTORDERDATE = " "
               MOVE "00000000" TO ST-AL-LASTORDERDATE.
            IF ST-AL-QTYRECMTD = " "
               MOVE "000000" TO ST-AL-QTYRECMTD.
            IF ST-AL-QTYRECYTD = " "
               MOVE "000000" TO ST-AL-QTYRECYTD.
            IF ST-AL-QTYRECLAST = " "
               MOVE "000000" TO ST-AL-QTYRECLAST.
            IF ST-AL-QTYADJMTD = " "
               MOVE "000000" TO ST-AL-QTYADJMTD.
            IF ST-AL-QTYADJYTD = " "
               MOVE "000000" TO ST-AL-QTYADJYTD.
            IF ST-AL-QTYADJLAST = " "
               MOVE "000000" TO ST-AL-QTYADJLAST.
            IF ST-AL-SALESUNITMTD = " "
               MOVE "000000" TO ST-AL-SALESUNITMTD.
            IF ST-AL-SALESUNITSYTD = " "
               MOVE "000000" TO ST-AL-SALESUNITSYTD.
            IF ST-AL-SALESUNITSLAST = " "
               MOVE "000000" TO ST-AL-SALESUNITSLAST.
            IF ST-AL-SALESRANDSMTD = " "
               MOVE "000000000" TO ST-AL-SALESRANDSMTD.
            IF ST-AL-SALESRANDSYTD = " "
               MOVE "000000000" TO ST-AL-SALESRANDSYTD.
            IF ST-AL-SALESRANDSLAST = " "
               MOVE "000000000" TO ST-AL-SALESRANDSLAST.
            IF ST-AL-SALESCOSTMTD = " "
               MOVE "000000000" TO ST-AL-SALESCOSTMTD.
            IF ST-AL-SALESCOSTYTD = " "
               MOVE "000000000" TO ST-AL-SALESCOSTYTD.
            IF ST-AL-SALESCOSTLAST = " "
               MOVE "000000000" TO ST-AL-SALESCOSTLAST.
       PRA-050.
            REWRITE STOCK-ALPHA-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK ALPHA RECORD BUSY ON RE-WRITE" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE ST-AL-STOCKNUMBER TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO PRA-050.
           PERFORM ERROR-020.
       PRA-900.
           IF INVQUES-STOCK-CHANGE = "Y"
            IF WS-CHANGE-FILE = "Y"
               PERFORM WRITE-STOCK-CHANGES.
           GO TO PRA-002.
       PRA-999.
           EXIT.
      *
       READ-INVQUES-FILE SECTION.
       RINVQUES-000.
            MOVE 1 TO PA-RECORD.
            MOVE 6 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RINVQUES-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "N" TO INVQUES-STOCK-CHANGE
               GO TO RINVQUES-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY RINVQUES, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RINVQUES-010.
       RINVQUES-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-001.
            OPEN I-O STOCK-ALPHA-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK ALPHA FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-001.
       OPEN-005.
            OPEN I-O STOCKCHANGE-MASTER.
            IF WS-STCHANGE-ST1 NOT = 0
               MOVE 0 TO WS-STCHANGE-ST1
               MOVE "STCHANGE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-014.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
           PERFORM READ-INVQUES-FILE.
           CLOSE PARAMETER-FILE.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-MASTER
                  STOCK-ALPHA-MASTER
                  STOCKCHANGE-MASTER.
       END-900.
            EXIT PROGRAM.
      *     STOP RUN.
       END-999.
            EXIT.
      *      
       Copy "WriteStockChanges".
       Copy "GetSystemY2KDate".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
