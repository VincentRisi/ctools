        IDENTIFICATION DIVISION.
        PROGRAM-ID. StOrdrMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStOrders".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdOutOrd.
           COPY ChlfdStock.

       WORKING-STORAGE SECTION.
       77  NEW-ORDER          PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-ANSWER          PIC X VALUE " ".      
       77  WS-ORDERNUMBER     PIC X(20) VALUE " ".
       77  WS-STOCKNUMBER     PIC X(15) VALUE " ".
       77  WS-NUM-SAVE        PIC X(20) VALUE " ".
       77  WS-QTYSAVE         PIC S9(5) VALUE 0.
       01  WS-STDESC.
           03  WS-DESC1       PIC X(20) VALUE " ".
           03  WS-DESC2       PIC X(20) VALUE " ".
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1  PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1   PIC 99.
       Copy "WsDateInfo".
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
           PERFORM FILL-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO OUT-ORDER-REC.
            MOVE "N" TO NEW-ORDER
                        WS-END.
       GET-001.              
            MOVE "ORDER" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO OO-ORDER-NUMBER.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-NUM-SAVE    TO OO-ORDER-NUMBER
                 MOVE WS-STOCKNUMBER TO OO-STOCK-NUMBER
                 PERFORM START-ORDER
                 PERFORM READ-ORDER-NEXT
                 PERFORM READ-STOCK
              IF WS-END NOT = "Y"
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-ORDER-PREVIOUS
                 PERFORM READ-STOCK
              IF WS-END NOT = "Y"
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-000.
       GET-003.
            MOVE "STOCK" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO OO-STOCK-NUMBER.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-NUM-SAVE TO OO-ORDER-NUMBER
                 MOVE WS-STOCKNUMBER TO OO-STOCK-NUMBER
                 PERFORM START-ORDER
                 PERFORM READ-ORDER-NEXT
                 PERFORM READ-STOCK
              IF WS-END NOT = "Y"
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-ORDER-PREVIOUS
                 PERFORM READ-STOCK
              IF WS-END NOT = "Y"
                 GO TO GET-005
              ELSE
                 PERFORM CLEAR-FORM
                 GO TO GET-000.
            PERFORM READ-STOCK.
            IF ST-DESCRIPTION1 = " "
                 MOVE "INVALID STOCKNUMBER, PLEASE RE-ENTER"
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-003.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-003.
            PERFORM READ-ORDER.
            IF NEW-ORDER = "Y"
               PERFORM GET-010
               GO TO GET-999.
            GO TO GET-010.
       GET-005.
            MOVE "ORDER"         TO F-FIELDNAME.
            MOVE 5               TO F-CBFIELDNAME.
            MOVE OO-ORDER-NUMBER TO F-NAMEFIELD.
            MOVE 20              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCK" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE OO-STOCK-NUMBER TO F-NAMEFIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-010.
            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.
            
            MOVE "DESC"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            MOVE WS-STDESC   TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-015.
            MOVE "ORDERQTY"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE OO-ORIG-QTY TO F-EDNAMEFIELDNUMNEG.
            MOVE 5           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMNEG.

            MOVE "QUANTITY"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE OO-QUANTITY TO F-EDNAMEFIELDNUMNEG.
            MOVE 5           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMNEG.

            MOVE "ORDERDATE"  TO F-FIELDNAME.
            MOVE 9            TO F-CBFIELDNAME.
            MOVE OO-ORDERDATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DUEDATE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            IF OO-DUEDATE > 0
               MOVE OO-DUEDATE   TO SPLIT-DATE
               PERFORM CONVERT-DATE-FORMAT
               MOVE DISPLAY-DATE TO F-NAMEFIELD
            ELSE
               MOVE " "          TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELIVERY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE OO-DELIVERY-METHOD TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
 
            MOVE "SUPPLIER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE OO-SUPPLIER-NUMBER TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
 
            MOVE "CONFIRMED" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            IF OO-UPDATED = " "
                MOVE "N" TO OO-UPDATED.
            MOVE OO-UPDATED  TO F-NAMEFIELD.
            MOVE 1           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
 
            MOVE "COST"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            MOVE OO-COST     TO F-EDNAMEFIELDFOREIGN
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FOREIGN.
 
            MOVE "DISC"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            MOVE OO-DISC     TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "FOR-LOC"   TO F-FIELDNAME.
            MOVE 7           TO F-CBFIELDNAME.
            MOVE OO-FOR-LOC  TO F-NAMEFIELD.
            MOVE 1           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-000.
            MOVE OO-QUANTITY TO WS-QTYSAVE.
       FILL-001.
            IF WS-END = "Y"
                 GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ORDERQTY"   TO F-FIELDNAME.
            MOVE 8            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUMNEG
                                OO-ORIG-QTY.
            PERFORM WRITE-FIELD-NUMNEG.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-NEXT
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-PREVIOUS
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-ORDER
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.
       FILL-005.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QUANTITY"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUMNEG
                                 OO-QUANTITY.
            PERFORM WRITE-FIELD-NUMNEG.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-NEXT
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-PREVIOUS
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-ORDER
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-005.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-005.
       FILL-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ORDERDATE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-010.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE      TO OO-ORDERDATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-NEXT
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-PREVIOUS
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-ORDER
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-010.
       FILL-015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DUEDATE" TO F-FIELDNAME.
            MOVE 7  TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-NAMEFIELD = " "
               MOVE 0 TO OO-DUEDATE
               GO TO FILL-016.
               
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-015.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO OO-DUEDATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-015.
       FILL-016.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-NEXT
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-PREVIOUS
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-ORDER
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-015.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-015.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DELIVERY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO F-EDNAMEFIELDCODE
                                OO-DELIVERY-METHOD.
            PERFORM WRITE-FIELD-CODE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EDNAMEFIELDCODE NOT > "0"
               MOVE "THIS FIELD MUST BE BETWEEN 1 & 9, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-NEXT
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-PREVIOUS
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-ORDER
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.
       FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SUPPLIER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO OO-SUPPLIER-NUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF OO-SUPPLIER-NUMBER = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-025.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-NEXT
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-PREVIOUS
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-ORDER
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-025.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-025.
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CONFIRMED" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO OO-UPDATED.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF OO-UPDATED = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-030.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-NEXT
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-PREVIOUS
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-ORDER
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-030.
       FILL-035.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COST" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO OO-COST.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF OO-UPDATED = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-035.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-NEXT
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-PREVIOUS
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-ORDER
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-035.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-035.
       FILL-037.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DISC" TO F-FIELDNAME
            MOVE 4 TO F-CBFIELDNAME.
            IF NEW-ORDER = "Y"
               MOVE ST-SUPPLIERDISC TO F-EDNAMEFIELDAMOUNTDIS
               MOVE 5 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-AMOUNTDIS.
            
            PERFORM USER-FILL-FIELD.
            MOVE 5            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO OO-DISC F-EDNAMEFIELDAMOUNTDIS
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-035.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-NEXT
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-PREVIOUS
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-ORDER
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-037.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-037.
       FILL-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "FOR-LOC" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO OO-FOR-LOC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF OO-FOR-LOC = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-040.
            IF F-EXIT-CH = X"01"
               GO TO FILL-037.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-NEXT
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM READ-ORDER-PREVIOUS
               PERFORM READ-STOCK
               PERFORM GET-005 THRU GET-015
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-ORDER-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-ORDER
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-040.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-040.

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO OO-ORDER-NUMBER
                         OO-STOCK-NUMBER
                         OO-DELIVERY-METHOD
                         OO-SUPPLIER-NUMBER
                         OO-UPDATED
                         OO-FOR-LOC.
             MOVE 0 TO   OO-QUANTITY
                         OO-ORIG-QTY
                         OO-COST
                         OO-DISC
                         OO-ORDERDATE
                         OO-DUEDATE.
             MOVE WS-ORDERNUMBER TO OO-ORDER-NUMBER.
             MOVE WS-STOCKNUMBER TO OO-STOCK-NUMBER.
       CLSC-500.
             UNLOCK OUTSTANDING-ORDERS
             UNLOCK STOCK-MASTER.
       CLSC-999.
             EXIT.      
      *
       DELETE-ORDER SECTION.
       DO-000.
            IF NEW-ORDER = "Y"
                GO TO DO-999.
            MOVE 0 TO OO-QUANTITY.
            PERFORM REWRITE-STOCK.
       DO-010.
            DELETE OUTSTANDING-ORDERS
               INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 NOT = 0
                MOVE "ORDERS RECORD BUSY ON DELETE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-OUTORD-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-OUTORD-ST1
                GO TO DO-010.
       DO-999.
           EXIT.
      *
       REWRITE-ORDER-RECORD SECTION.
       ROR-010.
            IF NEW-ORDER = "Y"
               GO TO ROR-020.
            REWRITE OUT-ORDER-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 NOT = 0
                MOVE "ORDERS RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-OUTORD-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-OUTORD-ST1
                GO TO ROR-010.
            GO TO ROR-900.
       ROR-020.
            WRITE OUT-ORDER-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 NOT = 0
                MOVE "ORDERS RECORD BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-OUTORD-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-OUTORD-ST1
                GO TO ROR-020.
       ROR-900.
            PERFORM REWRITE-STOCK.
       ROR-999.
            EXIT.
      *
       REWRITE-STOCK SECTION.
       RWST-001.
           PERFORM CLEAR-010.
      *     PERFORM CLEAR-020.
       RWST-002.
           MOVE 2710 TO POS
           DISPLAY "AMEND THE QUANTITY ON THE STOCK-FILE [ ]" AT POS
           ADD 38 TO POS
           
           MOVE WS-ANSWER TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 24        TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF W-ESCAPE-KEY NOT = 0 AND NOT = 1 AND NOT = 2 AND NOT = 7
                DISPLAY " " AT 3079 WITH BELL
                GO TO RWST-002.
           IF WS-ANSWER NOT = "Y" AND NOT = "N"
                MOVE "ONLY Y OR N ARE VALID ANSWERS, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO RWST-002.
           IF WS-ANSWER = "N"
                GO TO RWST-999.
           COMPUTE WS-QTYSAVE = WS-QTYSAVE - OO-QUANTITY.
           SUBTRACT WS-QTYSAVE FROM ST-QTYONORDER.
       RWST-010.
           REWRITE STOCK-RECORD
                INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
                MOVE 0 TO WS-OUTORD-ST1
                MOVE "STOCK RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RWST-010.
       RWST-999.
            EXIT.
      *
       READ-ORDER SECTION.
       RO-000.
           MOVE OO-ORDER-NUMBER TO WS-ORDERNUMBER.
           MOVE OO-STOCK-NUMBER TO WS-STOCKNUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
             INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 NOT = 0
                MOVE 88 TO WS-OUTORD-ST1
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
       RO-010.
           READ OUTSTANDING-ORDERS WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-OUTORD-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
           IF WS-OUTORD-ST1 NOT = 0
                MOVE "ORDERS RECORD BUSY ON READ, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-OUTORD-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-OUTORD-ST1
                GO TO RO-010.
           MOVE "N" TO NEW-ORDER.
           MOVE OO-ORDER-NUMBER TO WS-NUM-SAVE.
       RO-999.
           EXIT.
      *
       READ-STOCK SECTION.
       RS-000.
           MOVE OO-STOCK-NUMBER TO WS-STOCKNUMBER
                                   ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.
       RS-010.
           READ STOCK-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-STOCK-ST1
                            ST-DESCRIPTION1
                GO TO RS-999.
           IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY."
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
       START-ORDER SECTION.
       ST-OO-000.
           MOVE WS-ORDERNUMBER TO OO-ORDER-NUMBER.
           MOVE WS-STOCKNUMBER TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE 88 TO WS-OUTORD-ST1
               MOVE "Y" TO NEW-ORDER.
       ST-OO-999.
             EXIT.
      *
       READ-ORDER-NEXT SECTION.
       RONX-001.
      *     MOVE 88 TO WS-OUTORD-ST1.
       RONX-005. 
           IF WS-OUTORD-ST1 = 88
              MOVE " " TO OO-ORDER-NUMBER
                          WS-NUM-SAVE
                          WS-ORDERNUMBER
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-999.
           READ OUTSTANDING-ORDERS NEXT WITH LOCK
            AT END
              MOVE " " TO OO-ORDER-NUMBER
                          WS-ORDERNUMBER
                          WS-NUM-SAVE
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-999.
           IF WS-OUTORD-ST1 NOT = 0
                MOVE "ORDERS RECORD BUSY ON READ-NEXT, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-OUTORD-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-OUTORD-ST1
                PERFORM START-ORDER
                GO TO RONX-005.
           MOVE OO-ORDER-NUMBER TO WS-ORDERNUMBER
                                   WS-NUM-SAVE.
           MOVE OO-STOCK-NUMBER TO WS-STOCKNUMBER.
           MOVE "N" TO NEW-ORDER.
       RONX-999.
           EXIT.
      *
       READ-ORDER-PREVIOUS SECTION.
       RPREV-001.
      *     MOVE 88 TO WS-OUTORD-ST1.
       RPREV-005. 
           IF WS-OUTORD-ST1 = 88
              MOVE " " TO OO-ORDER-NUMBER
                          WS-NUM-SAVE
                          WS-ORDERNUMBER
              MOVE "Y" TO WS-END
           MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RPREV-999.
           READ OUTSTANDING-ORDERS PREVIOUS WITH LOCK
            AT END
              MOVE " " TO OO-ORDER-NUMBER
                          WS-ORDERNUMBER
                          WS-NUM-SAVE
              MOVE "Y" TO WS-END
           MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RPREV-999.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "ORDERS RECORD BUSY ON READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               PERFORM START-ORDER
               GO TO RPREV-005.
           MOVE OO-ORDER-NUMBER TO WS-ORDERNUMBER
                                   WS-NUM-SAVE.
           MOVE OO-STOCK-NUMBER TO WS-STOCKNUMBER.
           MOVE "N" TO NEW-ORDER.
       RPREV-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O OUTSTANDING-ORDERS.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO WS-OUTORD-ST1
               MOVE "SORDERS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-005.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StOrdrMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE OUTSTANDING-ORDERS
                  STOCK-MASTER.
            EXIT PROGRAM.
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
       Copy "WriteFieldDuty".
       Copy "WriteFieldForeign".
       Copy "WriteFieldTariff".
       Copy "WriteFieldAmount".
       Copy "WriteFieldAmountDis".
       Copy "WriteFieldSale".
       Copy "WriteFieldCode".
       Copy "WriteFieldNumNeg".
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
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
