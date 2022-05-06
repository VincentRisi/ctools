        IDENTIFICATION DIVISION.
        PROGRAM-ID. StPrNwMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStNewPrices".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStNewPrice.

       WORKING-STORAGE SECTION.
       77  NEW-STPRICENO      PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-STPRICENUMBER   PIC X(15) VALUE " ".
       01  WS-STNWPR-STATUS.
           03  WS-STNWPR-ST1   PIC 99.
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
            MOVE " " TO STNWPR-RECORD.
            MOVE "N" TO NEW-STPRICENO
                        WS-END.
       GET-001.              
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STNWPR-STOCKNUMBER.
            IF F-EXIT-CH = X"0C"
                MOVE WS-STPRICENUMBER TO STNWPR-STOCKNUMBER
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
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-STPRICE.
            IF NEW-STPRICENO = "Y"
               MOVE WS-DATE TO STNWPR-DATE
               PERFORM GET-006
               GO TO GET-999.
            GO TO GET-005.
       GET-003.
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE STNWPR-STOCKNUMBER TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-005.
            MOVE "PRICE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE STNWPR-PRICE TO F-EDNAMEFIELDAMOUNT.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.
       GET-006.
            MOVE "DATE"       TO F-FIELDNAME.
            MOVE 4            TO F-CBFIELDNAME.
            MOVE STNWPR-DATE  TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
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
                                 STNWPR-PRICE.
            PERFORM WRITE-FIELD-AMOUNT.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STNWPR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF STNWPR-PRICE = 0
               GO TO FILL-030.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STNWPR-RECORD
               PERFORM READ-STPRICE-NEXT
               PERFORM GET-003 THRU GET-006
               GO TO FILL-030.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STNWPR-RECORD
               PERFORM READ-STPRICE-PREVIOUS
               PERFORM GET-003 THRU GET-006
               GO TO FILL-030.
            IF F-EXIT-CH = X"01"
               GO TO FILL-135.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STNWPR-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STNWPR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'FINISH'"
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
            MOVE SPLIT-DATE TO STNWPR-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-135.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STNWPR-RECORD
               PERFORM READ-STPRICE-NEXT
               PERFORM GET-003 THRU GET-006
               GO TO FILL-030.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STNWPR-RECORD
               PERFORM READ-STPRICE-PREVIOUS
               PERFORM GET-003 THRU GET-006
               GO TO FILL-030.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-STNWPR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STNWPR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'FINISH'"
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
       DELETE-STNWPR-RECORD SECTION.
       DSR-000.
            IF NEW-STPRICENO = "Y"
               GO TO DSR-999.
       DSR-010.
           DELETE STNWPR-MASTER
              INVALID KEY NEXT SENTENCE.
           IF WS-STNWPR-ST1 NOT = 0
               MOVE "STPRICE FILE BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STNWPR-ST1
               GO TO DSR-010. 
       DSR-999.
            EXIT. 
      *
       RELEASE-STNWPR-RECORD SECTION.
       REL-000.
           UNLOCK STNWPR-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-STNWPR-RECORD SECTION.
       RSR-010.
          IF NEW-STPRICENO = "Y"
              GO TO RSR-020.
          REWRITE STNWPR-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STNWPR-ST1 NOT = 0
              MOVE "STPRICE RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STNWPR-ST1
               GO TO RSR-010.
          GO TO RSR-999.
       RSR-020.
          WRITE STNWPR-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STNWPR-ST1 NOT = 0
              MOVE "STPRICE RECORD BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STNWPR-ST1
               GO TO RSR-020.
       RSR-999.
          EXIT.
      *
       READ-STPRICE SECTION.
       R-STNWPR-000.
             MOVE STNWPR-STOCKNUMBER TO WS-STPRICENUMBER.
             START STNWPR-MASTER KEY NOT < STNWPR-KEY
                  INVALID KEY NEXT SENTENCE.
       R-STNWPR-010.
             READ STNWPR-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-STNWPR-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-STPRICENO
                MOVE WS-STPRICENUMBER TO STNWPR-STOCKNUMBER
                GO TO R-STNWPR-999.
             IF WS-STNWPR-ST1 NOT = 0
               MOVE "STPRICE RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STNWPR-ST1
               GO TO R-STNWPR-010.
       R-STNWPR-999.
             EXIT.
      *
       START-STPRICE SECTION.
       STNWPR-STNWPR-000.
              MOVE WS-STPRICENUMBER TO STNWPR-STOCKNUMBER.
              START STNWPR-MASTER KEY NOT < STNWPR-STOCKNUMBER.
             IF WS-STNWPR-ST1 = 10
               MOVE 88 TO WS-STNWPR-ST1
               MOVE 
            "THERE ARE NO PRICE RECORDS IN THIS FILE, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
       STNWPR-STNWPR-999.
             EXIT.
      *
       READ-STPRICE-NEXT SECTION.
       RSN-005. 
           IF WS-STNWPR-ST1 = 88
               MOVE "Y" TO WS-END
               GO TO RSN-999.
           READ STNWPR-MASTER NEXT WITH LOCK
             AT END 
               MOVE " " TO STNWPR-STOCKNUMBER
                           WS-STPRICENUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF NEXT-PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-STNWPR-ST1 = 23 OR 35 OR 49
               MOVE "STPRICE FILE BUSY ON READ-NEXT-23, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STNWPR-ST1
               GO TO RSN-999.
           IF WS-STNWPR-ST1 NOT = 0
               MOVE "STPRICE FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STNWPR-ST1
               PERFORM START-STPRICE
               GO TO RSN-005.
           MOVE STNWPR-STOCKNUMBER TO WS-STPRICENUMBER.
           MOVE "N"                TO NEW-STPRICENO.
       RSN-999.
             EXIT.
      *
       READ-STPRICE-PREVIOUS SECTION.
       RPREV-005. 
           IF WS-STNWPR-ST1 = 88
               MOVE "Y" TO WS-END
               GO TO RPREV-999.
           READ STNWPR-MASTER PREVIOUS WITH LOCK
             AT END 
               MOVE " " TO STNWPR-STOCKNUMBER
                           WS-STPRICENUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RPREV-999.
           IF WS-STNWPR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-STNWPR-ST1
               MOVE "STPRICE FILE BUSY ON READ-PREV-23, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RPREV-999.
           IF WS-STNWPR-ST1 NOT = 0
               MOVE "STPRICE FILE BUSY ON READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STNWPR-ST1
               PERFORM START-STPRICE
               GO TO RPREV-005.
           MOVE STNWPR-STOCKNUMBER TO WS-STPRICENUMBER.
           MOVE "N"                TO NEW-STPRICENO.
       RPREV-999.
             EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO STNWPR-STOCKNUMBER.
             MOVE 0   TO STNWPR-PRICE
                         STNWPR-DATE.
       CLSC-500.
             UNLOCK STNWPR-MASTER.
       CLSC-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STNWPR-MASTER.
            IF WS-STNWPR-ST1 = 35 OR 49
               GO TO OPEN-001.
            IF WS-STNWPR-ST1 NOT = 0
               MOVE "NEW PRICE FILE BUSY ON OPEN I-O, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STNWPR-ST1
               GO TO OPEN-000.
            GO TO OPEN-010.
       OPEN-001.
            OPEN OUTPUT STNWPR-MASTER.
            IF WS-STNWPR-ST1 NOT = 0
               MOVE "NEW PRICE FILE BUSY ON OPEN OUPUT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STNWPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STNWPR-ST1
               GO TO OPEN-001.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StPrNwMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE StNwPr-Master.
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
       Copy "WriteFieldAmount".
       Copy "DisplayForm".
       Copy "UserFillField".
      ******************
      *Mandatory Copies*
      ******************
       Copy "GetSystemY2KDate".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
      * END-OF-JOB
