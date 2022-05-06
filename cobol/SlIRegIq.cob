        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlIRegIq.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectSlRegister".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdRegister.
      *
       WORKING-STORAGE SECTION.
       77  NEW-ORDER          PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-INVOICE         PIC 9(6) VALUE 0.
       77  WS-TRANS           PIC 9 VALUE 0.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1    PIC 99.
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
            MOVE " " TO INCR-REC.
            MOVE "N" TO NEW-ORDER
                        WS-END.
            MOVE 2910 TO POS
            DISPLAY
          "TRANS TYPE: 1=INV, 3=REPAIR, 4=PSLIP, 6=CRD, 7=B/M, 8=QUOTE."
             AT POS.
       GET-001.              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO INCR-INVOICE.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-INVOICE TO INCR-INVOICE
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
            IF INCR-INVOICE NOT > 0
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
            MOVE NUMERIC-RATE TO INCR-TRANS
                                 F-EDNAMEFIELDIND1.
            PERFORM WRITE-FIELD-INDEX1.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-INVOICE TO INCR-INVOICE
      *           PERFORM START-TRANS
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
                 GO TO GET-002.
            IF INCR-INVOICE NOT > 0
                 MOVE "PLEASE ENTER A VALID NUMBER, 'ESC' TO RE-ENTER."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-002.
            PERFORM READ-TRANS.
            IF INCR-TRANS = 0
                PERFORM CLEAR-FORM
                GO TO GET-999.
            GO TO GET-005.
        GET-004.
            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-INVOICE TO F-EDNAMEFIELDINV.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INV.

            MOVE "TRANS" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR-TRANS TO F-EDNAMEFIELDIND1.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INDEX1.
        GET-005.
            MOVE "ACCOUNT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-ACCOUNT TO F-EDNAMEFIELDACC.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ACCOUNT.

            MOVE "PORDER" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE INCR-PORDER TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GSTNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR-GSTNO TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"       TO F-FIELDNAME.
            MOVE 4            TO F-CBFIELDNAME.
            MOVE INCR-DATE    TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALES" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR-SALES TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "INVCREDAMT"     TO F-FIELDNAME
            MOVE 10               TO F-CBFIELDNAME
            MOVE INCR-INVCRED-AMT TO F-EDNAMEFIELD99Mil
            MOVE 11               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "TAX"    TO F-FIELDNAME
            MOVE 3        TO F-CBFIELDNAME
            MOVE INCR-TAX TO F-EDNAMEFIELD99Mil
            MOVE 11       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "ADDONS"    TO F-FIELDNAME
            MOVE 6           TO F-CBFIELDNAME
            MOVE INCR-ADDONS TO F-EDNAMEFIELD99Mil
            MOVE 11          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "DISCOUNT"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE INCR-DISCOUNT TO F-EDNAMEFIELD99Mil
            MOVE 11            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "INVCREDCOST"     TO F-FIELDNAME
            MOVE 11                TO F-CBFIELDNAME
            MOVE INCR-INVCRED-COST TO F-EDNAMEFIELD99Mil
            MOVE 11                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "SBTYPE" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE INCR-SB-TYPE TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DRTRANSNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE INCR-DRTRANS-NO TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "PBTYPE" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE INCR-PULLBY TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-DATE" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            IF INCR-PULL-DATE = 0
                MOVE " " TO F-NAMEFIELD
            ELSE
                MOVE INCR-PULL-DATE TO SPLIT-DATE
                PERFORM CONVERT-DATE-FORMAT
                MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10  TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-TIME" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            IF INCR-PULL-TIME = 0
                MOVE " " TO F-NAMEFIELD
            ELSE
                MOVE INCR-PULL-TIME TO ALPHA-RATE
                PERFORM TIME-CHECKING
                MOVE WS-DATE-CHECK TO F-NAMEFIELD.
            MOVE 8 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PRINTED" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-PRINTED TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELAREA" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-AREA TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COPYNUM" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-COPY-NUMBER TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "PART-ORDERS"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            MOVE INCR-PART-ORDERS TO F-EDNAMEFIELDANAL.
            MOVE 1                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-ADD1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-ADD2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-ADD3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CODE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-CODE TO F-EDNAMEFIELDPOST.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-POST.

            MOVE "DEL1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-DEL1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-DEL2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR-DEL3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TERMS" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR-TERMS TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PHONE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR-PHONE TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CONTACT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-CONTACT TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELIVERY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE INCR-DELIVERY TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BIN" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE INCR-BIN TO F-NAMEFIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COMMENT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-COMMENT TO F-NAMEFIELD.
            MOVE 30 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BOINVNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-BO-INV-NO TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "BO-DATE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE INCR-BO-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADDPOST" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-ADDPOST TO F-EDNAMEFIELD99Mil.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99Mil.

            MOVE "ADDFREIGHT" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE INCR-ADDFREIGHT TO F-EDNAMEFIELD99Mil.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99Mil.

            MOVE "ADDLABOUR" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE INCR-ADDLABOUR TO F-EDNAMEFIELD99Mil.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99Mil.

            MOVE "ADDMISC" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR-ADDMISC TO F-EDNAMEFIELD99Mil.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99Mil.

            MOVE "LINENO" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE INCR-LINENO TO F-EDNAMEFIELDLINE.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-LINE.

            GO TO GET-001.
       GET-999.
            EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO INCR-GSTNO
                         INCR-SB-TYPE
                         INCR-PRINTED
                         INCR-AREA
                         INCR-NAME
                         INCR-ADD1
                         INCR-ADD2
                         INCR-ADD3
                         INCR-DEL1
                         INCR-DEL2
                         INCR-DEL3
                         INCR-TERMS
                         INCR-PHONE
                         INCR-CONTACT
                         INCR-PORDER
                         INCR-AREA
                         INCR-BIN
                         INCR-COMMENT
                         INCR-PART-ORDERS.
             MOVE 0   TO INCR-INVOICE
                         INCR-TRANS
                         INCR-ACCOUNT
                         INCR-COPY-NUMBER
                         INCR-DATE
                         INCR-INVCRED-AMT
                         INCR-TAX
                         INCR-ADDONS
                         INCR-DISCOUNT
                         INCR-INVCRED-COST
                         INCR-DRTRANS-NO
      *                   INCR-STTRANS-NO
                         INCR-CODE
                         INCR-BO-INV-NO
                         INCR-BO-DATE
                         INCR-ADDPOST
                         INCR-ADDFREIGHT
                         INCR-ADDLABOUR
                         INCR-ADDMISC
                         INCR-LINENO.
             MOVE WS-INVOICE TO INCR-INVOICE.
             MOVE WS-TRANS   TO INCR-TRANS.
       CLSC-999.
             EXIT.      
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE " "          TO WS-INCR-ST1
           MOVE INCR-INVOICE TO WS-INVOICE
           MOVE INCR-TRANS   TO WS-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
                 INVALID KEY NEXT SENTENCE.
       RO-010.
           READ INCR-REGISTER
                 INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-INCR-ST1
                PERFORM CLEAR-FORM
                PERFORM START-TRANS
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RO-010.
           MOVE "N" TO NEW-ORDER.
           MOVE INCR-INVOICE TO WS-INVOICE.
           MOVE INCR-TRANS   TO WS-TRANS.
       RO-999.
           EXIT.
      *
       START-TRANS SECTION.
       ST-OO-000.
           MOVE WS-INVOICE TO INCR-INVOICE
           MOVE WS-TRANS   TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON START, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1.
       ST-OO-999.
             EXIT.
      *
       READ-TRANS-NEXT SECTION.
       RONX-005. 
           IF WS-INCR-ST1 NOT = 0
              MOVE "Y" TO WS-END
              GO TO RONX-999.
           READ INCR-REGISTER NEXT WITH LOCK
            AT END
              MOVE 0 TO INCR-INVOICE
                        WS-INVOICE
                        INCR-TRANS
                        WS-TRANS
              CLOSE INCR-REGISTER
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              OPEN I-O INCR-REGISTER
              GO TO RONX-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE 0 TO WS-INCR-ST1
               MOVE "REGISTER BUSY ON READ-NEXT-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               PERFORM START-TRANS
               GO TO RONX-005.
           MOVE INCR-INVOICE TO WS-INVOICE.
           MOVE INCR-TRANS   TO WS-TRANS.
           MOVE "N" TO NEW-ORDER.
       RONX-999.
           EXIT.
      *
       READ-TRANS-PREVIOUS SECTION.
       RPREV-005. 
           IF WS-INCR-ST1 NOT = 0
              MOVE "Y" TO WS-END
              GO TO RPREV-999.
           READ INCR-REGISTER PREVIOUS WITH LOCK
            AT END
              MOVE 0 TO INCR-INVOICE
                          WS-INVOICE
                        INCR-TRANS
                          WS-TRANS
              CLOSE INCR-REGISTER
              MOVE "Y" TO WS-END
              MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              OPEN I-O INCR-REGISTER
              GO TO RPREV-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE 0 TO WS-INCR-ST1
               MOVE "REGISTER BUSY ON READ-PREV-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RPREV-005.
           MOVE INCR-INVOICE TO WS-INVOICE.
           MOVE INCR-TRANS   TO WS-TRANS.
           MOVE "N" TO NEW-ORDER.
       RPREV-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
               MOVE 0 TO WS-INCR-ST1
               MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
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
            CLOSE INCR-REGISTER.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAccount".
       Copy "WriteField99Mil".
       Copy "ReadFieldAlpha".
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
       Copy "ErrorMessage".
       Copy "Error1Message".
      *
      * END-OF-JOB
