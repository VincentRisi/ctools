        IDENTIFICATION DIVISION.
        PROGRAM-ID. StImport.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStImports".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdImpReceipts.
      *
       WORKING-STORAGE SECTION.
       77  NEW-ORDER          PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-TRANS           PIC 9(6) VALUE 0.
       01  WS-IMPRECEIPT-STATUS.
           03  WS-IMPRECEIPT-ST1   PIC 99.
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
            MOVE " " TO IMPORT-RECEIPTS-REC.
            MOVE "N" TO NEW-ORDER
                        WS-END.
       GET-001.              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TRANSNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO IMRE-TRANSACTION-NUMBER
                                 F-EDNAMEFIELDNUM.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-TRANS TO IMRE-TRANSACTION-NUMBER
                 PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-004
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-TRANS-PREVIOUS
              IF WS-END NOT = "Y"
                 GO TO GET-004
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF IMRE-TRANSACTION-NUMBER NOT > 0
                 MOVE "PLEASE ENTER A VALID No"
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-001.
            PERFORM READ-TRANS.
            IF IMRE-TRANSACTION-NUMBER = 0
                PERFORM CLEAR-FORM
                GO TO GET-999.
            GO TO GET-005.
       GET-004.
            MOVE "TRANSNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE IMRE-TRANSACTION-NUMBER TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.
       GET-005.
            MOVE "SUPPLIER"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE IMRE-SUPPLIER TO F-NAMEFIELD
            MOVE 7             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICENUM"    TO F-FIELDNAME
            MOVE 10              TO F-CBFIELDNAME
            MOVE IMRE-INVOICENUM TO F-NAMEFIELD
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CURRENCY"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE IMRE-CURRENCY TO F-NAMEFIELD
            MOVE 4             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVAMTFOREIGN"    TO F-FIELDNAME
            MOVE 13                 TO F-CBFIELDNAME
            MOVE IMRE-INVAMTFOREIGN TO F-EDNAMEFIELDFORTOTAL
            MOVE 11                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FORTOTAL.

            MOVE "FREIGHTAMTFOREIGN"    TO F-FIELDNAME
            MOVE 17                     TO F-CBFIELDNAME
            MOVE IMRE-FREIGHTAMTFOREIGN TO F-EDNAMEFIELD99mil
            MOVE 11                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99mil.

            MOVE "RANDINVAMTGROSS"    TO F-FIELDNAME
            MOVE 15                   TO F-CBFIELDNAME
            MOVE IMRE-RANDINVAMTGROSS TO F-EDNAMEFIELD99MIL
            MOVE 11                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "TOTALDUTYRAND"    TO F-FIELDNAME
            MOVE 13                 TO F-CBFIELDNAME
            MOVE IMRE-TOTALDUTYRAND TO F-EDNAMEFIELD99mil
            MOVE 11                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99mil.

            MOVE "FREIGHTRAND"    TO F-FIELDNAME
            MOVE 11               TO F-CBFIELDNAME
            MOVE IMRE-FREIGHTRAND TO F-EDNAMEFIELD99mil
            MOVE 11               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99mil.

            MOVE "WARFAGE"    TO F-FIELDNAME
            MOVE 7            TO F-CBFIELDNAME
            MOVE IMRE-WARFAGE TO F-EDNAMEFIELD99mil
            MOVE 11           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99mil.

            MOVE "CLEARING"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE IMRE-CLEARING TO F-EDNAMEFIELD99mil
            MOVE 11            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99mil.

            MOVE "TOTALCHARGES"    TO F-FIELDNAME
            MOVE 12                TO F-CBFIELDNAME
            MOVE IMRE-TOTALCHARGES TO F-EDNAMEFIELD99mil
            MOVE 11                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99mil.

            MOVE "ONCOSTPERCENT"    TO F-FIELDNAME
            MOVE 13                 TO F-CBFIELDNAME
            MOVE IMRE-ONCOSTPERCENT TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "ORDERNUM"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE IMRE-ORDERNUM TO F-NAMEFIELD
            MOVE 20            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATERECEIVED"    TO F-FIELDNAME
            MOVE 12                TO F-CBFIELDNAME
            MOVE IMRE-DATERECEIVED TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE      TO F-NAMEFIELD
            MOVE 10                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "RATEPAID"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE IMRE-RATEPAID TO F-EDNAMEFIELDTRANS
            MOVE 10            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-TRANS.

            MOVE "EXCHANGERATE"    TO F-FIELDNAME
            MOVE 12                TO F-CBFIELDNAME
            MOVE IMRE-EXCHANGERATE TO F-EDNAMEFIELDTRANS
            MOVE 10                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-TRANS.

            MOVE "MARKUPPERCENT"    TO F-FIELDNAME
            MOVE 13                 TO F-CBFIELDNAME
            MOVE IMRE-MARKUPPERCENT TO F-EDNAMEFIELDTYPE
            MOVE 4                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-TYPE.

            MOVE "NETTRANDINVAMT"       TO F-FIELDNAME
            MOVE 14                     TO F-CBFIELDNAME
            MOVE IMRE-NETT-RAND-INV-AMT TO F-EDNAMEFIELD99Mil
            MOVE 11                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "SURCHARGERAND"     TO F-FIELDNAME
            MOVE 13                  TO F-CBFIELDNAME
            MOVE IMRE-SURCHARGE-RAND TO F-EDNAMEFIELD99mil
            MOVE 11                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99mil.

            MOVE "RANDAMTPAID"      TO F-FIELDNAME
            MOVE 11                 TO F-CBFIELDNAME
            MOVE IMRE-RAND-AMT-PAID TO F-EDNAMEFIELD99Mil
            MOVE 11                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "STOCKNUMBER"     TO F-FIELDNAME
            MOVE 11                TO F-CBFIELDNAME
            MOVE IMRE-STOCK-NUMBER TO F-NAMEFIELD
            MOVE 15                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QUANTITY"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE IMRE-QUANTITY TO F-EDNAMEFIELDQTY
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY.

            MOVE "COSTEACH"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE IMRE-COSTEACH TO F-EDNAMEFIELDFOREIGN99
            MOVE 12            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FOREIGN99.

            MOVE "DUTYPERCENT"    TO F-FIELDNAME
            MOVE 11               TO F-CBFIELDNAME
            MOVE IMRE-DUTYPERCENT TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "DISCOUNTPERCENT"    TO F-FIELDNAME
            MOVE 15                   TO F-CBFIELDNAME
            MOVE IMRE-DISCOUNTPERCENT TO F-EDNAMEFIELDPERC
            MOVE 6                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PERC.

            MOVE "LDCOST"    TO F-FIELDNAME
            MOVE 6           TO F-CBFIELDNAME
            MOVE IMRE-LDCOST TO F-EDNAMEFIELD99Mil
            MOVE 11          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "SPRICE"    TO F-FIELDNAME
            MOVE 6           TO F-CBFIELDNAME
            MOVE IMRE-SPRICE TO F-EDNAMEFIELD99Mil
            MOVE 11          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "UPDATEDYN"     TO F-FIELDNAME
            MOVE 9               TO F-CBFIELDNAME
            MOVE IMRE-UPDATED-YN TO F-NAMEFIELD
            MOVE 1               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                 GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SUPPLIER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO IMRE-SUPPLIER.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-140.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
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
            IF IMRE-SUPPLIER = "  "
               MOVE "SUPPLIER MUST BE > SPACES" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
       FILL-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVOICENUM" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO IMRE-INVOICENUM.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
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
            IF IMRE-INVOICENUM = "  "
               MOVE "INV No MUST BE > SPACES" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
       FILL-015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CURRENCY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO IMRE-CURRENCY.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
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
            IF IMRE-CURRENCY = "  "
               MOVE "CURRENCY MUST BE > SPACES" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-015.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVAMTFOREIGN" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99Mil
                              IMRE-INVAMTFOREIGN.
            PERFORM WRITE-FIELD-99Mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
            IF IMRE-INVAMTFOREIGN = 0
               MOVE "THIS FIELD MUST BE > 0, PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
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
            MOVE "FREIGHTAMTFOREIGN" TO F-FIELDNAME.
            MOVE 17 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99mil
                                 IMRE-FREIGHTAMTFOREIGN.
            PERFORM WRITE-FIELD-99mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
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
            MOVE "RANDINVAMTGROSS" TO F-FIELDNAME.
            MOVE 15 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99Mil
                                 IMRE-RANDINVAMTGROSS.
            PERFORM WRITE-FIELD-99mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
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
            MOVE "TOTALDUTYRAND" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99mil
                                 IMRE-TOTALDUTYRAND.
            PERFORM WRITE-FIELD-99mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
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
       FILL-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "FREIGHTRAND" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99mil
                                 IMRE-FREIGHTRAND.
            PERFORM WRITE-FIELD-99mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-035.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
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
       FILL-045.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "WARFAGE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99mil
                                 IMRE-WARFAGE.
            PERFORM WRITE-FIELD-99mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-040.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-045.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-045.
       FILL-050.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CLEARING" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99mil
                                 IMRE-CLEARING.
            PERFORM WRITE-FIELD-99mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-045.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-050.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-050.
       FILL-055.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TOTALCHARGES" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99mil
                                 IMRE-TOTALCHARGES.
            PERFORM WRITE-FIELD-99mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-050.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-055.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-055.
       FILL-060.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ONCOSTPERCENT" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 IMRE-ONCOSTPERCENT.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-055.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-060.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-060.
       FILL-070.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATERECEIVED" TO F-FIELDNAME.
            MOVE 12   TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10   TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-070.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO IMRE-DATERECEIVED.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-070.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-060.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-070.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-070.
       FILL-065.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ORDERNUM" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO IMRE-ORDERNUM.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-070.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-065.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-065.
       FILL-075.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "RATEPAID" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDTRANS
                                 IMRE-RATEPAID.
            PERFORM WRITE-FIELD-TRANS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-070.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-075.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-075.
       FILL-080.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "EXCHANGERATE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDTRANS
                                 IMRE-EXCHANGERATE.
            PERFORM WRITE-FIELD-TRANS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-075.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-080.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-080.
       FILL-085.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "MARKUPPERCENT" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDTYPE
                                 IMRE-MARKUPPERCENT.
            PERFORM WRITE-FIELD-TYPE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-080.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-085.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-085.
       FILL-090.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NETTRANDINVAMT" TO F-FIELDNAME.
            MOVE 14 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99Mil
                                 IMRE-NETT-RAND-INV-AMT.
            PERFORM WRITE-FIELD-99mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-085.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-090.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-090.
       FILL-095.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SURCHARGERAND" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99mil
                                 IMRE-SURCHARGE-RAND.
            PERFORM WRITE-FIELD-99mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-090.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-095.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-095.
       FILL-100.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "RANDAMTPAID" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99Mil
                                 IMRE-RAND-AMT-PAID.
            PERFORM WRITE-FIELD-99mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-095.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-100.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-100.
       FILL-105.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO IMRE-STOCK-NUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF IMRE-STOCK-NUMBER = "  "
               MOVE "THIS FIELD MUST BE > SPACES" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-105.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-100.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-105.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-105.
       FILL-110.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QUANTITY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDQTY
                                 IMRE-QUANTITY.
            PERFORM WRITE-FIELD-QTY.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF IMRE-QUANTITY = 0
               MOVE "THIS FIELD MUST BE > 0, PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-110.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-105.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-110.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-110.
       FILL-115.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COSTEACH" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDFOREIGN99
                                 IMRE-COSTEACH.
            PERFORM WRITE-FIELD-FOREIGN99.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF IMRE-COSTEACH = 0
               MOVE "THIS FIELD MUST BE > 0, PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-115.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-110.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-115.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-115.
       FILL-120.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DUTYPERCENT" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 IMRE-DUTYPERCENT.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-115.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-120.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-120.
       FILL-125.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DISCOUNTPERCENT" TO F-FIELDNAME.
            MOVE 15 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDPERC
                                 IMRE-DISCOUNTPERCENT.
            PERFORM WRITE-FIELD-PERC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-120.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-125.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-125.
       FILL-130.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LDCOST" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99Mil
                                 IMRE-LDCOST.
            PERFORM WRITE-FIELD-99mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF IMRE-LDCOST = 0
               MOVE "THIS FIELD MUST BE > 0, PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-130.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-125.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-130.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-130.
       FILL-135.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SPRICE" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99Mil
                                 IMRE-SPRICE.
            PERFORM WRITE-FIELD-99mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-130.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-135.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-135.
       FILL-140.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "UPDATEDYN" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO IMRE-UPDATED-YN.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-135.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               PERFORM GET-004 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-140.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-140.

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO IMRE-SUPPLIER
                         IMRE-INVOICENUM
                         IMRE-CURRENCY
                         IMRE-ORDERNUM
                         IMRE-STOCK-NUMBER
                         IMRE-UPDATED-YN.
             MOVE 0   TO IMRE-TRANSACTION-NUMBER
                         IMRE-INVAMTFOREIGN
                         IMRE-FREIGHTAMTFOREIGN
                         IMRE-RANDINVAMTGROSS
                         IMRE-TOTALDUTYRAND
                         IMRE-FREIGHTRAND
                         IMRE-WARFAGE
                         IMRE-CLEARING
                         IMRE-TOTALCHARGES
                         IMRE-ONCOSTPERCENT
                         IMRE-DATERECEIVED
                         IMRE-RATEPAID
                         IMRE-EXCHANGERATE
                         IMRE-MARKUPPERCENT
                         IMRE-NETT-RAND-INV-AMT
                         IMRE-SURCHARGE-RAND
                         IMRE-RAND-AMT-PAID
                         IMRE-QUANTITY
                         IMRE-COSTEACH
                         IMRE-DUTYPERCENT
                         IMRE-DISCOUNTPERCENT
                         IMRE-LDCOST
                         IMRE-SPRICE.
             MOVE WS-TRANS TO IMRE-TRANSACTION-NUMBER.
             UNLOCK IMPRECEIPTS-FILE.
       CLSC-999.
             EXIT.
      *
       DELETE-TRANS SECTION.
       DO-000.
            IF NEW-ORDER = "Y"
                GO TO DO-999.
       DO-010.
            DELETE IMPRECEIPTS-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-IMPRECEIPT-ST1 NOT = 0
                MOVE "IMPORTS RECORD BUSY ON DELETE, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-IMPRECEIPT-ST1
                GO TO DO-010.
       DO-999.
           EXIT.
      *
       REWRITE-TRANS-RECORD SECTION.
       ROR-010.
            IF NEW-ORDER = "Y"
               GO TO ROR-020.
            REWRITE IMPORT-RECEIPTS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-IMPRECEIPT-ST1 NOT = 0
                MOVE "IMPORTS RECORD BUSY ON REWRITE, BE PATIENT"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-IMPRECEIPT-ST1
                GO TO ROR-010.
            GO TO ROR-999.
       ROR-020.
            WRITE IMPORT-RECEIPTS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-IMPRECEIPT-ST1 NOT = 0
                MOVE "IMPORTS RECORD BUSY ON WRITE, BE PATIENT"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-IMPRECEIPT-ST1
                GO TO ROR-020.
       ROR-999.
            EXIT.
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE 0 TO WS-IMPRECEIPT-ST1.
           MOVE IMRE-TRANSACTION-NUMBER TO WS-TRANS.
           START IMPRECEIPTS-FILE KEY NOT < IMRE-KEY.
       RO-010.
           READ IMPRECEIPTS-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-IMPRECEIPT-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-IMPRECEIPT-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
           IF WS-IMPRECEIPT-ST1 NOT = 0
                MOVE "IMPORTS BUSY ON READ, 'ESC' To Retry"
                  TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-IMPRECEIPT-ST1
                GO TO RO-010.
           MOVE "N" TO NEW-ORDER.
           MOVE IMRE-TRANSACTION-NUMBER TO WS-TRANS.
       RO-999.
           EXIT.
      *
       START-TRANS SECTION.
       ST-OO-000.
           MOVE WS-TRANS TO IMRE-TRANSACTION-NUMBER.
           START IMPRECEIPTS-FILE KEY NOT < IMRE-KEY.
       ST-OO-999.
             EXIT.
      *
       READ-TRANS-NEXT SECTION.
       RONX-001.
           MOVE 0 TO WS-IMPRECEIPT-ST1.
       RONX-005. 
           READ IMPRECEIPTS-FILE NEXT WITH LOCK
            AT END
              MOVE 0 TO IMRE-TRANSACTION-NUMBER
                        WS-TRANS
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-999.
           IF WS-IMPRECEIPT-ST1 NOT = 0
                MOVE "IMPORTS BUSY ON READ-NEXT, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-IMPRECEIPT-ST1
                PERFORM START-TRANS
                GO TO RONX-005.
           MOVE IMRE-TRANSACTION-NUMBER TO WS-TRANS.
           MOVE "N"                     TO NEW-ORDER.
       RONX-999.
           EXIT.
      *
       READ-TRANS-PREVIOUS SECTION.
       RPREV-001.
           MOVE 0 TO WS-IMPRECEIPT-ST1.
       RPREV-005. 
           READ IMPRECEIPTS-FILE PREVIOUS WITH LOCK
            AT END
              MOVE 0 TO IMRE-TRANSACTION-NUMBER
                        WS-TRANS
              MOVE "Y" TO WS-END
              MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RPREV-999.
           IF WS-IMPRECEIPT-ST1 NOT = 0
                MOVE "IMPORTS BUSY ON READ-PREV, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-IMPRECEIPT-ST1
                PERFORM START-TRANS
                GO TO RPREV-005.
           MOVE IMRE-TRANSACTION-NUMBER TO WS-TRANS.
           MOVE "N"                     TO NEW-ORDER.
       RPREV-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O IMPRECEIPTS-FILE.
            IF WS-IMPRECEIPT-ST1 NOT = 0
               MOVE 0 TO WS-IMPRECEIPT-ST1
               MOVE "IMPORTS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StImport"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE IMPRECEIPTS-FILE.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldNumeric".
       Copy "WriteField99Mil".
       Copy "WriteFieldQty".
       Copy "WriteFieldForeign99".
       Copy "WriteFieldAmountDis".
       Copy "WriteFieldForTotal".
       Copy "WriteFieldPerc".
       Copy "WriteFieldType".
       Copy "WriteFieldTrans".
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
      * END-OF-JOB
