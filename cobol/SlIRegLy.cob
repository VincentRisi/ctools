        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlIRegLy.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectSlRegLy".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdRegisterLy.
      *
       WORKING-STORAGE SECTION.
       77  NEW-ORDER          PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-INVOICE         PIC 9(6) VALUE 0.
       77  WS-TRANS           PIC 9 VALUE 0.
       01  WS-INCR-LY-STATUS.
           03  WS-INCR-LY-ST1   PIC 99.
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
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE 0420 TO POS
            DISPLAY "** LAST YEAR INFO **" AT POS.
            MOVE 2910 TO POS
            DISPLAY "TRANS TYPE: 1=INVOICE, 6=C/NOTE." AT POS.
            
            MOVE " " TO INCR-LY-REC.
            MOVE "N" TO NEW-ORDER
                        WS-END.
       GET-001.              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO INCR-LY-INVOICE.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-INVOICE TO INCR-LY-INVOICE
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
            IF INCR-LY-INVOICE NOT > 0
                 MOVE "PLEASE ENTER A VALID NUMBER, 'ESC' TO RE-ENTER."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-001.
        GET-002.              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TRANS" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO INCR-LY-TRANS
                                 F-EDNAMEFIELDIND1.
            PERFORM WRITE-FIELD-INDEX1.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-002.

            IF INCR-LY-TRANS NOT = 1 AND NOT = 6
                MOVE 
              "YOU CAN ONLY VIEW 1=INV & 6=C/NOTES, 'ESC' TO RE-ENTER."
                  TO WS-MESSAGE
                  PERFORM ERROR-MESSAGE
                  GO TO GET-002.

            IF F-EXIT-CH = X"0C"
                 MOVE WS-INVOICE TO INCR-LY-INVOICE
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
            IF INCR-LY-INVOICE NOT > 0
                 MOVE "PLEASE ENTER A VALID NUMBER, 'ESC' TO RE-ENTER."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-002.
            PERFORM READ-TRANS.
            IF INCR-LY-TRANS = 0
                PERFORM CLEAR-FORM
                GO TO GET-999.
            GO TO GET-005.
        GET-004.
            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-LY-INVOICE TO F-EDNAMEFIELDINV.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INV.

            MOVE "TRANS" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR-LY-TRANS TO F-EDNAMEFIELDIND1.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INDEX1.
        GET-005.
            MOVE "ACCOUNT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-LY-ACCOUNT TO F-EDNAMEFIELDACC.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ACCOUNT.

            MOVE "PORDER" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE INCR-LY-PORDER TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GSTNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR-LY-GSTNO TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"       TO F-FIELDNAME.
            MOVE 4            TO F-CBFIELDNAME.
            MOVE INCR-LY-DATE    TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALES" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR-LY-SALES TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "INVCREDAMT"        TO F-FIELDNAME
            MOVE 10                  TO F-CBFIELDNAME
            MOVE INCR-LY-INVCRED-AMT TO F-EDNAMEFIELD99MIL
            MOVE 11                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.

            MOVE "TAX"       TO F-FIELDNAME
            MOVE 3           TO F-CBFIELDNAME
            MOVE INCR-LY-TAX TO F-EDNAMEFIELD99MIL
            MOVE 11          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.

            MOVE "ADDONS"       TO F-FIELDNAME
            MOVE 6              TO F-CBFIELDNAME
            MOVE INCR-LY-ADDONS TO F-EDNAMEFIELD99MIL
            MOVE 11             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.

            MOVE "DISCOUNT"       TO F-FIELDNAME
            MOVE 8                TO F-CBFIELDNAME
            MOVE INCR-LY-DISCOUNT TO F-EDNAMEFIELD99MIL
            MOVE 11               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.

            MOVE "INVCREDCOST"        TO F-FIELDNAME
            MOVE 11                   TO F-CBFIELDNAME
            MOVE INCR-LY-INVCRED-COST TO F-EDNAMEFIELD99MIL
            MOVE 11                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99MIL.

            MOVE "SBTYPE" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE INCR-LY-SB-TYPE TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DRTRANSNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE INCR-LY-DRTRANS-NO TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "PBTYPE" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE INCR-LY-PULLBY TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-DATE" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            IF INCR-LY-PULL-DATE = 0
                MOVE " " TO F-NAMEFIELD
            ELSE
                MOVE INCR-LY-PULL-DATE TO SPLIT-DATE
                PERFORM CONVERT-DATE-FORMAT
                MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10  TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-TIME" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            IF INCR-LY-PULL-TIME = 0
                MOVE " " TO F-NAMEFIELD
            ELSE
                MOVE INCR-LY-PULL-TIME TO ALPHA-RATE
                PERFORM TIME-CHECKING
                MOVE WS-DATE-CHECK TO F-NAMEFIELD.
            MOVE 8 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PRINTED" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-LY-PRINTED TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELAREA" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-LY-AREA TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COPYNUM" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-LY-COPY-NUMBER TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "PART-ORDERS"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            MOVE INCR-LY-PART-ORDERS TO F-EDNAMEFIELDANAL.
            MOVE 1                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-LY-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-LY-ADD1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-LY-ADD2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-LY-ADD3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CODE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-LY-CODE TO F-EDNAMEFIELDPOST.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-POST.

            MOVE "DEL1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-LY-DEL1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-LY-DEL2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-LY-DEL3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TERMS" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR-LY-TERMS TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PHONE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR-LY-PHONE TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CONTACT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-LY-CONTACT TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BIN" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE INCR-LY-BIN TO F-NAMEFIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COMMENT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-LY-COMMENT TO F-NAMEFIELD.
            MOVE 30 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BOINVNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-LY-BO-INV-NO TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "BO-DATE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE INCR-LY-BO-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADDPOST" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-LY-ADDPOST TO F-EDNAMEFIELD99MIL.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99MIL.

            MOVE "ADDFREIGHT" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE INCR-LY-ADDFREIGHT TO F-EDNAMEFIELD99MIL.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99MIL.

            MOVE "ADDLABOUR"       TO F-FIELDNAME.
            MOVE 9                 TO F-CBFIELDNAME.
            MOVE INCR-LY-ADDLABOUR TO F-EDNAMEFIELD99MIL.
            MOVE 11                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99MIL.

            MOVE "ADDMISC" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-LY-ADDMISC TO F-EDNAMEFIELD99MIL.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99MIL.

            MOVE "LINENO" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE INCR-LY-LINENO TO F-EDNAMEFIELDLINE.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-LINE.

            GO TO GET-001.
       GET-999.
            EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO INCR-LY-GSTNO
                         INCR-LY-SB-TYPE
                         INCR-LY-PRINTED
                         INCR-LY-AREA
                         INCR-LY-NAME
                         INCR-LY-ADD1
                         INCR-LY-ADD2
                         INCR-LY-ADD3
                         INCR-LY-DEL1
                         INCR-LY-DEL2
                         INCR-LY-DEL3
                         INCR-LY-TERMS
                         INCR-LY-PHONE
                         INCR-LY-CONTACT
                         INCR-LY-PORDER
                         INCR-LY-AREA
                         INCR-LY-BIN
                         INCR-LY-COMMENT
                         INCR-LY-PART-ORDERS.
             MOVE 0   TO INCR-LY-INVOICE
                         INCR-LY-TRANS
                         INCR-LY-ACCOUNT
                         INCR-LY-COPY-NUMBER
                         INCR-LY-DATE
                         INCR-LY-INVCRED-AMT
                         INCR-LY-TAX
                         INCR-LY-ADDONS
                         INCR-LY-DISCOUNT
                         INCR-LY-INVCRED-COST
                         INCR-LY-DRTRANS-NO
      *                   INCR-LY-STTRANS-NO
                         INCR-LY-CODE
                         INCR-LY-BO-INV-NO
                         INCR-LY-BO-DATE
                         INCR-LY-ADDPOST
                         INCR-LY-ADDFREIGHT
                         INCR-LY-ADDLABOUR
                         INCR-LY-ADDMISC
                         INCR-LY-LINENO.
             MOVE WS-INVOICE TO INCR-LY-INVOICE.
             MOVE WS-TRANS   TO INCR-LY-TRANS.
       CLSC-999.
             EXIT.      
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE 0               TO WS-INCR-LY-ST1
           MOVE INCR-LY-INVOICE TO WS-INVOICE
           MOVE INCR-LY-TRANS   TO WS-TRANS.
           START INCR-LY-REGISTER KEY NOT < INCR-LY-KEY
                 INVALID KEY NEXT SENTENCE.
       RO-010.
           READ INCR-LY-REGISTER
                 INVALID KEY NEXT SENTENCE.
           IF WS-INCR-LY-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-INCR-LY-ST1
                PERFORM CLEAR-FORM
                PERFORM START-TRANS
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
           IF WS-INCR-LY-ST1 NOT = 0
               MOVE "REGISTER BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-LY-ST1
               GO TO RO-010.
           MOVE "N" TO NEW-ORDER.
           MOVE INCR-LY-INVOICE TO WS-INVOICE.
           MOVE INCR-LY-TRANS   TO WS-TRANS.
       RO-999.
           EXIT.
      *
       START-TRANS SECTION.
       ST-OO-000.
           MOVE WS-INVOICE TO INCR-LY-INVOICE
           MOVE WS-TRANS   TO INCR-LY-TRANS.
           START INCR-LY-REGISTER KEY NOT < INCR-LY-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-LY-ST1 NOT = 0
               MOVE "REGISTERLY BUSY ON START, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-LY-ST1.
       ST-OO-999.
             EXIT.
      *
       READ-TRANS-NEXT SECTION.
       RONX-005. 
           IF WS-INCR-LY-ST1 NOT = 0
              MOVE "Y" TO WS-END
              GO TO RONX-999.
           READ INCR-LY-REGISTER NEXT WITH LOCK
            AT END
              MOVE 0 TO INCR-LY-INVOICE
                        WS-INVOICE
                        INCR-LY-TRANS
                        WS-TRANS
              CLOSE INCR-LY-REGISTER
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              OPEN I-O INCR-LY-REGISTER
              GO TO RONX-999.
           IF WS-INCR-LY-ST1 NOT = 0
               MOVE "REGISTER-LY BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-LY-ST1
               PERFORM START-TRANS
               GO TO RONX-005.
           MOVE INCR-LY-INVOICE TO WS-INVOICE.
           MOVE INCR-LY-TRANS   TO WS-TRANS.
           MOVE "N" TO NEW-ORDER.
       RONX-999.
           EXIT.
      *
       READ-TRANS-PREVIOUS SECTION.
       RPREV-005. 
           IF WS-INCR-LY-ST1 NOT = 0
              MOVE "Y" TO WS-END
              GO TO RPREV-999.
           READ INCR-LY-REGISTER PREVIOUS WITH LOCK
            AT END
              MOVE 0 TO INCR-LY-INVOICE
                        WS-INVOICE
                        INCR-LY-TRANS
                        WS-TRANS
              CLOSE INCR-LY-REGISTER
              MOVE "Y" TO WS-END
              MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              OPEN I-O INCR-LY-REGISTER
              GO TO RPREV-999.
           IF WS-INCR-LY-ST1 NOT = 0
               MOVE "REGISTER-LY BUSY ON READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-LY-ST1
               PERFORM START-TRANS
               GO TO RPREV-005.
           MOVE INCR-LY-INVOICE TO WS-INVOICE.
           MOVE INCR-LY-TRANS   TO WS-TRANS.
           MOVE "N" TO NEW-ORDER.
       RPREV-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O INCR-LY-REGISTER.
           IF WS-INCR-LY-ST1 NOT = 0
               MOVE 0 TO WS-INCR-LY-ST1
               MOVE "REG-LY FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlIRegIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE INCR-LY-REGISTER.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAccount".
       Copy "ReadFieldAlpha".
       Copy "WriteField99Mil".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAccount".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldAmount".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldIndex1".
       Copy "WriteFieldInv".
       Copy "WriteFieldLine".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldPost".
       Copy "WriteFieldPrice".
       Copy "GetSystemY2KDate".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "TimeChecking".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "Error1Message".
       Copy "ErrorMessage".
      *
      * END-OF-JOB
