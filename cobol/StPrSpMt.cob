        IDENTIFICATION DIVISION.
        PROGRAM-ID. StPrSpMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStSpecPr".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStPrice.
           COPY ChlfdStock.

       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "StDescIq".
       77  NEW-STPRICENO        PIC X VALUE " ".      
       77  WS-END               PIC X VALUE " ".      
       77  WS-STPRICENUMBER     PIC X(15) VALUE " ".
       01  WS-STPR-STATUS.
           03  WS-STPR-ST1      PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
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
            MOVE " " TO STPR-RECORD.
      *                  WS-STPRICENUMBER.
            MOVE "N" TO NEW-STPRICENO
                        WS-END.
            PERFORM ERROR-020.
       GET-001.              
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STPR-STOCKNUMBER.
            IF F-EXIT-CH = X"0C"
                MOVE WS-STPRICENUMBER TO STPR-STOCKNUMBER
                PERFORM START-STPRICE
                PERFORM READ-STPRICE-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-FORM
               GO TO GET-000.
            IF F-EXIT-CH = X"05"
                PERFORM READ-STPRICE-PREVIOUS
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-FORM
               GO TO GET-000.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF STPR-STOCKNUMBER = 0 OR = " "
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-005
                PERFORM DISPLAY-FORM
                GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-STPRICE.
            IF NEW-STPRICENO = "Y"
               PERFORM READ-STOCK
             IF WS-STOCK-ST1 NOT = 23 AND NOT = 35 
                         AND NOT = 49
               PERFORM GET-005
               GO TO GET-999.
            GO TO GET-005.
       GET-003.
            PERFORM READ-STOCK.
            
            MOVE "STOCKNUMBER"    TO F-FIELDNAME
            MOVE 11               TO F-CBFIELDNAME
            MOVE STPR-STOCKNUMBER TO F-NAMEFIELD
            MOVE 25               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-005.
            MOVE "DESCRIPTION1"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE ST-DESCRIPTION1 TO F-NAMEFIELD
            MOVE 20              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESCRIPTION2"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE ST-DESCRIPTION2 TO F-NAMEFIELD
            MOVE 20              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCK-PRICE" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE ST-PRICE      TO F-EDNAMEFIELDAMOUNT
            MOVE 9             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "STOCK-COST"   TO F-FIELDNAME
            MOVE 10             TO F-CBFIELDNAME
            MOVE ST-AVERAGECOST TO F-EDNAMEFIELDAMOUNT
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "QTYONHAND"  TO F-FIELDNAME.
            MOVE 9            TO F-CBFIELDNAME.
            MOVE ST-QTYONHAND TO F-EDNAMEFIELDINV.
            MOVE 6            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INV.
       GET-010.
            MOVE "PRICE"    TO F-FIELDNAME
            MOVE 5          TO F-CBFIELDNAME
            MOVE STPR-PRICE TO F-EDNAMEFIELDAMOUNT
            MOVE 9          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
 
            MOVE "DATE"       TO F-FIELDNAME
            MOVE 4            TO F-CBFIELDNAME
            MOVE STPR-DATE    TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE TO F-NAMEFIELD
            MOVE 10           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "THIS IS NOT A VALID STOCK-NUMBER" TO WS-MESSAGE
               PERFORM ERROR-000.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-030.
            IF WS-END = "Y"
                 GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PRICE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNT
                                 STPR-PRICE.
            PERFORM WRITE-FIELD-AMOUNT.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STPR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF STPR-PRICE = 0
               GO TO FILL-030.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STPR-RECORD
               PERFORM READ-STPRICE-NEXT
               PERFORM GET-003 THRU GET-010
               GO TO FILL-030.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STPR-RECORD
               PERFORM READ-STPRICE-PREVIOUS
               PERFORM GET-003 THRU GET-010
               GO TO FILL-030.
            IF F-EXIT-CH = X"01"
               GO TO FILL-135.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STPR-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STPR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-030.
        FILL-135.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STPR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-135.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO STPR-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-135.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STPR-RECORD
               PERFORM READ-STPRICE-NEXT
               PERFORM GET-003 THRU GET-010
               GO TO FILL-030.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STPR-RECORD
               PERFORM READ-STPRICE-PREVIOUS
               PERFORM GET-003 THRU GET-010
               GO TO FILL-030.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STPR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STPR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-135.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-135.
 
            GO TO FILL-030.
       FILL-999.
             EXIT.
      *
       DELETE-STPR-RECORD SECTION.
       DSR-000.
            IF NEW-STPRICENO = "Y"
               GO TO DSR-999.
       DSR-010.
            DELETE STPR-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-STPR-ST1 NOT = 0
              MOVE "STPRICE RECORD BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               GO TO DSR-010. 
       DSR-999.
            EXIT. 
      *
       RELEASE-STPR-RECORD SECTION.
       REL-000.
           UNLOCK STPR-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-STPR-RECORD SECTION.
       RSR-010.
          IF NEW-STPRICENO = "Y"
              GO TO RSR-020.
          REWRITE STPR-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STPR-ST1 NOT = 0
              MOVE "STPRICE RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               GO TO RSR-010.
          GO TO RSR-999.
       RSR-020.
          WRITE STPR-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STPR-ST1 NOT = 0
              MOVE "STPRICE RECORD BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
              GO TO RSR-020.
       RSR-999.
          EXIT.
      *
       READ-STPRICE SECTION.
       R-STPR-000.
             MOVE STPR-STOCKNUMBER TO WS-STPRICENUMBER.
             START STPR-MASTER KEY NOT < STPR-KEY
                  INVALID KEY NEXT SENTENCE.
       R-STPR-010.
             READ STPR-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-STPR-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-STPRICENO
                MOVE WS-STPRICENUMBER TO STPR-STOCKNUMBER
                PERFORM START-STPRICE
                GO TO R-STPR-999.
             IF WS-STPR-ST1 NOT = 0
               MOVE "STPRICE RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               GO TO R-STPR-010.
       R-STPR-999.
             EXIT.
      *
       START-STPRICE SECTION.
       STPR-STPR-000.
              MOVE WS-STPRICENUMBER TO STPR-STOCKNUMBER.
              START STPR-MASTER KEY NOT LESS STPR-STOCKNUMBER.
       STPR-STPR-999.
             EXIT.
      *
       READ-STPRICE-NEXT SECTION.
       RSN-005. 
           READ STPR-MASTER NEXT WITH LOCK
             AT END 
               MOVE " " TO STPR-STOCKNUMBER
                           WS-STPRICENUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-STPR-ST1 = 23 OR 35 OR 49
               MOVE "STPRICE FILE BUSY ON READ-NEXT-23, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               GO TO RSN-005.
           IF WS-STPR-ST1 NOT = 0
               MOVE "STPRICE BUSY ON READ-NEXT, 'ESC' TO RETRY." 
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               MOVE " " TO WS-STPRICENUMBER
               PERFORM START-STPRICE
               GO TO RSN-005.
           MOVE STPR-STOCKNUMBER TO WS-STPRICENUMBER.
           MOVE "N"              TO NEW-STPRICENO.
       RSN-999.
             EXIT.
      *
       READ-STPRICE-PREVIOUS SECTION.
       RSPREV-005. 
           READ STPR-MASTER PREVIOUS WITH LOCK
             AT END 
               MOVE " " TO STPR-STOCKNUMBER
                           WS-STPRICENUMBER
               MOVE "Y" TO WS-END
            MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSPREV-999.
           IF WS-STPR-ST1 = 23 OR 35 OR 49
               MOVE "STPRICE FILE BUSY ON READ-PREV-23, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               GO TO RSPREV-005.
           IF WS-STPR-ST1 NOT = 0
               MOVE "STPRICE BUSY ON READ-PREV, 'ESC' TO RETRY." 
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               MOVE " " TO WS-STPRICENUMBER
               PERFORM START-STPRICE
               GO TO RSPREV-005.
           MOVE STPR-STOCKNUMBER TO WS-STPRICENUMBER.
           MOVE "N"              TO NEW-STPRICENO.
       RSPREV-999.
             EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
            MOVE STPR-STOCKNUMBER TO ST-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-KEY.
       R-ST-010.
            READ STOCK-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE "***** INVALID STOCK-" TO ST-DESCRIPTION1
                MOVE "NUMBER ENTERED *****" TO ST-DESCRIPTION2
                GO TO R-ST-900.
             IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-010.
       R-ST-900.
             PERFORM ERROR-020.
       R-ST-999.
             EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO STPR-STOCKNUMBER.
             MOVE 0   TO STPR-PRICE
                         STPR-DATE.
       CLSC-500.
             UNLOCK STPR-MASTER.
       CLSC-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STPR-MASTER.
            IF WS-STPR-ST1 NOT = 0
               MOVE 0 TO WS-STPR-ST1
               MOVE "STOCK SPEC-PRICE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-005.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
              MOVE 0 TO WS-STOCK-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-005.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StPrSpMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STPR-MASTER
                  STOCK-MASTER.
            EXIT PROGRAM.
       END-999.
           EXIT.
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldInv".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldDate".
       Copy "WriteFieldAmount".
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
