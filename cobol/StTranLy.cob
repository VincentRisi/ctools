        IDENTIFICATION DIVISION.
        PROGRAM-ID. StTranLy.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectStTransLy".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdStTransLy.
      *
       WORKING-STORAGE SECTION.
       77  WS-COM             PIC X VALUE " ".      
       77  NEW-ORDER          PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-READ            PIC X VALUE " ".      
       77  WS-TYPE            PIC 99 VALUE 0.
       77  WS-REF1            PIC 9(6) VALUE 0.
       77  WS-TRANSNO         PIC 9(6) VALUE 0.
       01  SPLIT-STOCK.
           03  SP-1STCHAR     PIC X VALUE " ".
           03  SP-REST        PIC X(14) VALUE " ".
       01  WS-STTRANSLY-STATUS.
           03  WS-STTRANSLY-ST1   PIC 99.
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
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN
           PERFORM DISPLAY-FORM.
       CONTROL-010.
           PERFORM GET-DATA
           PERFORM FILL-DATA.
           IF WS-READ NOT = "R"
             PERFORM DISPLAY-FORM
             MOVE "N" TO WS-READ.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            IF WS-READ = "R"
                 GO TO GET-0031.
            MOVE " " TO STOCK-TRANSLY-REC.
            MOVE "N" TO NEW-ORDER
                        WS-END
                        WS-READ
                        WS-COM.
       GET-001.              
            MOVE 0430 TO POS
            DISPLAY "** LAST YEAR INFO **" AT POS.

            MOVE "                    " TO F-NAMEFIELD.
            MOVE "REF1"       TO F-FIELDNAME
            MOVE 4            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 6            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 STTR-LY-REFERENCE1
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 PERFORM RELEASE-STOCK-RECORD
                 GO TO GET-000.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
              IF WS-END NOT = "Y"
                 MOVE "Y" TO WS-READ
                 GO TO GET-0031
              ELSE
                 MOVE 0 TO WS-TRANSNO
                           WS-REF1
                           WS-TYPE
                 PERFORM CLEAR-FORM
                 GO TO GET-999.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-TRANS-PREVIOUS
              IF WS-END NOT = "Y"
                 MOVE "Y" TO WS-READ
                 GO TO GET-0031
              ELSE
                 MOVE 0 TO WS-TRANSNO
                           WS-REF1
                           WS-TYPE
                 PERFORM CLEAR-FORM
                 GO TO GET-999.
            IF STTR-LY-REFERENCE1 NOT > 0
                 MOVE "PLEASE ENTER A TRANS No IDENTIFIER"
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-001.
        GET-002.
            MOVE "                    " TO F-NAMEFIELD
            MOVE "TYPE"       TO F-FIELDNAME
            MOVE 4            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 2            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDANAL
                                 STTR-LY-TYPE
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"0C"
                 PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
              IF WS-END NOT = "Y"
                 MOVE "Y" TO WS-READ
                 GO TO GET-0031
              ELSE
                 MOVE 0 TO WS-TRANSNO
                           WS-REF1
                           WS-TYPE
                 PERFORM CLEAR-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-TRANS-PREVIOUS
              IF WS-END NOT = "Y"
                 MOVE "Y" TO WS-READ
                 GO TO GET-0031
              ELSE
                 MOVE 0 TO WS-TRANSNO
                           WS-REF1
                           WS-TYPE
                 PERFORM CLEAR-FORM
                 GO TO GET-999.
            IF F-EXIT-CH = X"01"
                 GO TO GET-001.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 PERFORM RELEASE-STOCK-RECORD
                 GO TO GET-000.
            IF STTR-LY-TYPE NOT > 0
                 MOVE "PLEASE ENTER A TYPE IDENTIFIER"
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-002.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-002.
       GET-003.              
            MOVE "                    " TO F-NAMEFIELD
            MOVE "TRANSNO"    TO F-FIELDNAME
            MOVE 7            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 6            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 STTR-LY-TRANSACTION-NUMBER
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 PERFORM RELEASE-STOCK-RECORD
                 GO TO GET-000.
            IF F-EXIT-CH = X"01"
                 GO TO GET-002.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
              IF WS-END NOT = "Y"
                 MOVE "Y" TO WS-READ
                 GO TO GET-0031
              ELSE
                 MOVE 0 TO WS-TRANSNO
                           WS-REF1
                           WS-TYPE
                 PERFORM CLEAR-FORM
                 GO TO GET-999.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-TRANS-PREVIOUS
              IF WS-END NOT = "Y"
                 MOVE "Y" TO WS-READ
                 GO TO GET-0031
              ELSE
                 MOVE 0 TO WS-TRANSNO
                           WS-REF1
                           WS-TYPE
                 PERFORM CLEAR-FORM
                 GO TO GET-999.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-003.
            PERFORM READ-TRANS.
            IF NEW-ORDER = "Y"
               GO TO GET-999.
            MOVE "Y" TO WS-READ.
        GET-0031.
            MOVE "Y" TO WS-READ.
            MOVE STTR-LY-STOCK-NUMBER TO SPLIT-STOCK.
            IF SP-1STCHAR = "*"
               MOVE "Y" TO WS-COM
               PERFORM GET-004
               GO TO GET-050.
        GET-004.
            MOVE "REF1"          TO F-FIELDNAME
            MOVE 4               TO F-CBFIELDNAME
            MOVE STTR-LY-REFERENCE1 TO F-EDNAMEFIELDNUM
            MOVE 6               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC

            MOVE "TYPE"    TO F-FIELDNAME
            MOVE 4         TO F-CBFIELDNAME
            MOVE STTR-LY-TYPE TO F-EDNAMEFIELDANAL
            MOVE 2         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS

            MOVE "TRANSNO"               TO F-FIELDNAME
            MOVE 7                       TO F-CBFIELDNAME
            MOVE STTR-LY-TRANSACTION-NUMBER TO F-EDNAMEFIELDNUM
            MOVE 6                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC.
        GET-0041.
            MOVE "STOCKNO"         TO F-FIELDNAME
            MOVE 7                 TO F-CBFIELDNAME
            MOVE STTR-LY-STOCK-NUMBER TO F-NAMEFIELD
            MOVE 15                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ACCNO"             TO F-FIELDNAME
            MOVE 5                   TO F-CBFIELDNAME
            MOVE STTR-LY-ACCOUNT-NUMBER TO F-EDNAMEFIELDACC
            MOVE 7                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ACCOUNT

            MOVE "INVNO"     TO F-FIELDNAME
            MOVE 5           TO F-CBFIELDNAME
            MOVE STTR-LY-INV-NO TO F-EDNAMEFIELDNUM
            MOVE 6           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC

            MOVE "DATE"       TO F-FIELDNAME
            MOVE 4            TO F-CBFIELDNAME
            MOVE STTR-LY-DATE    TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE TO F-NAMEFIELD
            MOVE 10            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COMPLETE"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE STTR-LY-COMPLETE TO F-NAMEFIELD
            MOVE 1             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "N" TO WS-READ.
       GET-005.
            PERFORM CLEAR-COM-LY-FIELDS

            MOVE "REF2"        TO F-FIELDNAME
            MOVE 4             TO F-CBFIELDNAME
            MOVE STTR-LY-ORDERQTY TO F-EDNAMEFIELDQTY
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY

            MOVE "SHIP"       TO F-FIELDNAME
            MOVE 4            TO F-CBFIELDNAME
            MOVE STTR-LY-SHIPQTY TO F-EDNAMEFIELDQTY
            MOVE 5            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY

            MOVE "SHIPPED"       TO F-FIELDNAME
            MOVE 7               TO F-CBFIELDNAME
            MOVE STTR-LY-SHIPPEDQTY TO F-EDNAMEFIELDQTY
            MOVE 5               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY

            MOVE "SALESVAL"       TO F-FIELDNAME
            MOVE 8                TO F-CBFIELDNAME
            MOVE STTR-LY-SALES-VALUE TO F-EDNAMEFIELDPRICE
            MOVE 9                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PRICE

            MOVE "COSTVAL"       TO F-FIELDNAME
            MOVE 7               TO F-CBFIELDNAME
            MOVE STTR-LY-COST-VALUE TO F-EDNAMEFIELDPRICE
            MOVE 9               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PRICE

            MOVE "PRICE"    TO F-FIELDNAME
            MOVE 5          TO F-CBFIELDNAME
            MOVE STTR-LY-PRICE TO F-EDNAMEFIELDPRICE
            MOVE 9          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PRICE

            MOVE "DESC1"    TO F-FIELDNAME
            MOVE 5          TO F-CBFIELDNAME
            MOVE STTR-LY-DESC1 TO F-NAMEFIELD
            MOVE 20         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DESC2"    TO F-FIELDNAME
            MOVE 5          TO F-CBFIELDNAME
            MOVE STTR-LY-DESC2 TO F-NAMEFIELD
            MOVE 20         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ITEMDISC"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE STTR-LY-ITEMDISC TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS

            MOVE "TAX"    TO F-FIELDNAME
            MOVE 3        TO F-CBFIELDNAME
            MOVE STTR-LY-TAX TO F-NAMEFIELD
            MOVE 1        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "UNIT"    TO F-FIELDNAME
            MOVE 4         TO F-CBFIELDNAME
            MOVE STTR-LY-UNIT TO F-NAMEFIELD
            MOVE 4         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
        GET-006.
            GO TO GET-999.
        GET-050.
            PERFORM CLEAR-STOCK-FIELDS.
            IF NEW-ORDER = "Y"
               GO TO GET-999.

            MOVE "STOCKNO"         TO F-FIELDNAME
            MOVE 7                 TO F-CBFIELDNAME
            MOVE STTR-LY-STOCK-NUMBER TO F-NAMEFIELD
            MOVE 15                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ACCNO"             TO F-FIELDNAME
            MOVE 5                   TO F-CBFIELDNAME
            MOVE STTR-LY-ACCOUNT-NUMBER TO F-EDNAMEFIELDACC
            MOVE 7                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ACCOUNT

            MOVE "INVNO"     TO F-FIELDNAME
            MOVE 5           TO F-CBFIELDNAME
            MOVE STTR-LY-INV-NO TO F-EDNAMEFIELDNUM
            MOVE 6           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC

            MOVE "DATE"       TO F-FIELDNAME
            MOVE 4            TO F-CBFIELDNAME
            MOVE STTR-LY-DATE    TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE TO F-NAMEFIELD
            MOVE 10           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COMPLETE"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE STTR-LY-COMPLETE TO F-NAMEFIELD
            MOVE 1             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COMORDERQTY" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE COM-LY-ORDERQTY  TO F-NAMEFIELD
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COMSHIPQTY" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE COM-LY-SHIPQTY  TO F-NAMEFIELD
            MOVE 5            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COMDESC" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE COM-LY-DESC  TO F-NAMEFIELD
            MOVE 20        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COMUNIT" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE COM-LY-UNIT  TO F-NAMEFIELD
            MOVE 4         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COMPRICE" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE COM-LY-PRICE  TO F-NAMEFIELD
            MOVE 9          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COMCOST" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE COM-LY-COST  TO F-NAMEFIELD
            MOVE 9         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COMDISC" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE COM-LY-DISC  TO F-NAMEFIELD
            MOVE 5         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
               GO TO FILL-999.
            IF WS-COM = "Y"
               GO TO FILL-090.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "STOCKNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STTR-LY-STOCK-NUMBER.

            MOVE STTR-LY-STOCK-NUMBER TO SPLIT-STOCK.
            IF SP-1STCHAR = "*"
               MOVE "Y" TO WS-COM
             IF NEW-ORDER = "Y"
               PERFORM CLEAR-STOCK-FIELDS
               GO TO FILL-090
             ELSE
               PERFORM GET-050
               GO TO FILL-090.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
       FILL-005.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDACC
                                 STTR-LY-ACCOUNT-NUMBER.
            PERFORM WRITE-FIELD-ACCOUNT.
            IF STTR-LY-ACCOUNT-NUMBER NOT > 0
               MOVE "THIS FIELD MUST BE GREATER THAN 0, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-005.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
               GO TO FILL-005.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-005.
       FILL-011.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 STTR-LY-INV-NO.
            PERFORM WRITE-FIELD-NUMERIC.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-005.
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
               GO TO FILL-011.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-011.
       FILL-013.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-013.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO STTR-LY-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-013.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-011.
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
               GO TO FILL-013.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-013.
       FILL-014.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COMPLETE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STTR-LY-COMPLETE
                                STTR-LY-ST-COMPLETE
                                STTR-LY-AC-COMPLETE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-013.
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
               GO TO FILL-014.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-014.
       FILL-015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "REF2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDQTY
                                 STTR-LY-ORDERQTY.
            PERFORM WRITE-FIELD-QTY.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-014.
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
       FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SHIP" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDQTY
                                 STTR-LY-SHIPQTY.
            PERFORM WRITE-FIELD-QTY.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
       FILL-027.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SHIPPED" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDQTY
                                 STTR-LY-SHIPPEDQTY.
            PERFORM WRITE-FIELD-QTY.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
               GO TO FILL-027.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-027.
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESVAL" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDPRICE
                                 STTR-LY-SALES-VALUE.
            PERFORM WRITE-FIELD-PRICE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-027.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
            MOVE "COSTVAL" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDPRICE
                                 STTR-LY-COST-VALUE.
            PERFORM WRITE-FIELD-PRICE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
            MOVE "PRICE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDPRICE
                                 STTR-LY-PRICE.
            PERFORM WRITE-FIELD-PRICE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-035.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
            MOVE "DESC1" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STTR-LY-DESC1.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-040.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
            MOVE "DESC2" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STTR-LY-DESC2.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-045.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
            MOVE "ITEMDISC" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 STTR-LY-ITEMDISC.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-050.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
            MOVE "TAX" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STTR-LY-TAX.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-055.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
       FILL-065.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "UNIT" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STTR-LY-UNIT.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-060.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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

            GO TO FILL-001.
       FILL-090.
            IF WS-END = "Y"
               GO TO FILL-999.
            IF WS-COM = "Y"
               GO TO FILL-095.
       FILL-091.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "STOCKNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STTR-LY-STOCK-NUMBER.
            IF F-EXIT-CH = X"01"
               GO TO FILL-130.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDACC
                                 STTR-LY-ACCOUNT-NUMBER.
            PERFORM WRITE-FIELD-ACCOUNT.
            IF STTR-LY-ACCOUNT-NUMBER NOT > 0
               MOVE "THIS FIELD MUST BE GREATER THAN 0, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-095.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-091.
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
       FILL-096.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 STTR-LY-INV-NO.
            PERFORM WRITE-FIELD-NUMERIC.

            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
               GO TO FILL-096.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-096.
       FILL-097.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-097.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO STTR-LY-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-097.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-096.
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
               GO TO FILL-097.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-097.
       FILL-100.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COMPLETE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STTR-LY-COMPLETE
                                STTR-LY-ST-COMPLETE
                                STTR-LY-AC-COMPLETE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-097.
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
            MOVE "COMORDERQTY" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO COM-LY-ORDERQTY.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
            MOVE "COMSHIPQTY" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO COM-LY-SHIPQTY.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
            MOVE "COMDESC" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO COM-LY-DESC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-110.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
       FILL-117.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COMUNIT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO COM-LY-UNIT.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-115.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
               GO TO FILL-117.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-117.
       FILL-120.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COMPRICE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO COM-LY-PRICE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-117.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
            MOVE "COMCOST" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO COM-LY-COST.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-120.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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
            MOVE "COMDISC" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO COM-LY-DISC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               PERFORM RELEASE-STOCK-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-125.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
               MOVE "R" TO WS-READ
               GO TO FILL-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
               MOVE "R" TO WS-READ
               GO TO FILL-999.
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

            GO TO FILL-095.
       FILL-999.
            EXIT.
      *
       CLEAR-STOCK-FIELDS SECTION.
       CSF-010.
            MOVE "REF2" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 5      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "SHIP" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 5      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "SHIPPED" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 5         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "SALESVAL" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 9          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COSTVAL" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 9         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PRICE" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 9       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DESC1" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 20      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DESC2" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 20      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ITEMDISC" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 5          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "TAX" TO F-FIELDNAME
            MOVE 3     TO F-CBFIELDNAME
            MOVE " "   TO F-NAMEFIELD
            MOVE 1     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "UNIT" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 4      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       CSF-999.
            EXIT.
      *
       CLEAR-COM-LY-FIELDS SECTION.
       CCF-010.
            MOVE "COMDESC" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 20        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COMUNIT" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 4         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COMORDERQTY" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE " "           TO F-NAMEFIELD
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COMSHIPQTY" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE " "          TO F-NAMEFIELD
            MOVE 5            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COMPRICE" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 9          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COMCOST" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 9         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "COMDISC" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 5         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       CCF-999.
            EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
           MOVE " "        TO STOCK-TRANSLY-REC
           MOVE WS-REF1    TO STTR-LY-REFERENCE1
           MOVE WS-TYPE    TO STTR-LY-TYPE
           MOVE WS-TRANSNO TO STTR-LY-TRANSACTION-NUMBER.
       CLSC-999.
           EXIT.
      *
       RELEASE-STOCK-RECORD SECTION.
       REL-000.
           UNLOCK STOCK-TRANSLY-FILE.
       REL-999.
           EXIT.
      *
       REWRITE-TRANS-RECORD SECTION.
       ROR-005.
            IF NEW-ORDER = "Y"
               GO TO ROR-020.
       ROR-010.
            REWRITE STOCK-TRANSLY-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-STTRANSLY-ST1 = 23 OR 35 OR 49
                MOVE "STTRANS BUSY ON REWRITE, 'ESC' TO VIEW STATUS."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STTRANSLY-ST1
                GO TO ROR-020.
            IF WS-STTRANSLY-ST1 NOT = 0
                MOVE 
            "STTRANS RECORD BUSY ON REWRITE, ROR-010, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STTRANSLY-ST1
                GO TO ROR-010.
            GO TO ROR-999.
       ROR-020.
            WRITE STOCK-TRANSLY-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-STTRANSLY-ST1 NOT = 0
                MOVE "STTRANS BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STTRANSLY-ST1
                GO TO ROR-010.
       ROR-999.
            EXIT.
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE 0 TO WS-STTRANSLY-ST1
           MOVE STTR-LY-REFERENCE1         TO WS-REF1
           MOVE STTR-LY-TYPE               TO WS-TYPE
           MOVE STTR-LY-TRANSACTION-NUMBER TO WS-TRANSNO
           START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-KEY.
       RO-010.
           READ STOCK-TRANSLY-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-STTRANSLY-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-STTRANSLY-ST1
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
           IF WS-STTRANSLY-ST1 NOT = 0
                MOVE "STTRANS-LY BUSY ON READ-LOCK,  'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STTRANSLY-ST1
                GO TO RO-010.
           MOVE "N" TO NEW-ORDER
                       WS-COM.
           MOVE STTR-LY-REFERENCE1         TO WS-REF1
           MOVE STTR-LY-TRANSACTION-NUMBER TO WS-TRANSNO
           MOVE STTR-LY-TYPE               TO WS-TYPE.
       RO-999.
           EXIT.
      *
       START-TRANS SECTION.
       ST-OO-000.
           MOVE WS-REF1    TO STTR-LY-REFERENCE1
           MOVE WS-TYPE    TO STTR-LY-TYPE
           MOVE WS-TRANSNO TO STTR-LY-TRANSACTION-NUMBER
           START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STTRANSLY-ST1 NOT = 0
              MOVE 91 TO WS-STTRANSLY-ST1.
       ST-OO-999.
             EXIT.
      *
       READ-TRANS-NEXT SECTION.
       RONX-005. 
           READ STOCK-TRANSLY-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-STTRANSLY-ST1 = 10 OR = 91
              MOVE 0 TO STTR-LY-TYPE
                        STTR-LY-REFERENCE1
                        STTR-LY-TRANSACTION-NUMBER
                        WS-REF1
                        WS-TRANSNO
                        WS-TYPE
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-999.
           IF WS-STTRANSLY-ST1 NOT = 0
               MOVE "STTRANSLY FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANSLY-ST1
               PERFORM START-TRANS
               GO TO RONX-005.
           MOVE "N" TO NEW-ORDER
                       WS-COM.
           MOVE STTR-LY-REFERENCE1         TO WS-REF1
           MOVE STTR-LY-TRANSACTION-NUMBER TO WS-TRANSNO
           MOVE STTR-LY-TYPE               TO WS-TYPE.
       RONX-999.
           EXIT.
      *
       READ-TRANS-PREVIOUS SECTION.
       RPREV-005. 
           READ STOCK-TRANSLY-FILE PREVIOUS WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-STTRANSLY-ST1 = 10 OR = 91
              MOVE 0 TO STTR-LY-TYPE
                        STTR-LY-REFERENCE1
                        STTR-LY-TRANSACTION-NUMBER
                        WS-REF1
                        WS-TRANSNO
                        WS-TYPE
              MOVE "Y" TO WS-END
              MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RPREV-999.
           IF WS-STTRANSLY-ST1 NOT = 0
               MOVE "STTRANSLY FILE BUSY ON READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANSLY-ST1
               PERFORM START-TRANS
               GO TO RPREV-005.
           MOVE "N" TO NEW-ORDER
                       WS-COM.
           MOVE STTR-LY-REFERENCE1         TO WS-REF1
           MOVE STTR-LY-TRANSACTION-NUMBER TO WS-TRANSNO
           MOVE STTR-LY-TYPE               TO WS-TYPE.
       RPREV-999.
           EXIT.
      *
       DELETE-TRANS SECTION.
       DO-000.
            IF NEW-ORDER = "Y"
                GO TO DO-999.
       DO-010.
            DELETE STOCK-TRANSLY-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-STTRANSLY-ST1 NOT = 0
               MOVE "STTRANSLY FILE BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO DO-010.
       DO-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STOCK-TRANSLY-FILE.
            IF WS-STTRANSLY-ST1 NOT = 0
               MOVE "ST-TRANSLY FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StTrans"       TO F-FORMNAME
           MOVE 7               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-TRANSLY-FILE.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldPrice".
       Copy "WriteFieldQty".
       Copy "WriteFieldDate".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldAccount".
       Copy "WriteFieldAmountDis".
       Copy "DisplayForm".
       Copy "UserFillField".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
      *
      * END-OF-JOB
