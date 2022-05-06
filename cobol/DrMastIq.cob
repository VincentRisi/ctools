        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrMastIq.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           
       WORKING-STORAGE SECTION.
       77  WS-NO-READS        PIC 99 VALUE 0.
       77  WS-INQUIRY-PROGRAM PIC X(8) VALUE "DrNameIq".
       77  INVALID-START      PIC X VALUE " ".      
       77  WS-ACCOUNTNUMBER   PIC 9(7) VALUE 0.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1   PIC 99.
       Copy "WsDateInfo".
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN.
       CONT-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONT-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE 0 TO DEBTOR-RECORD.
            MOVE " "    TO INVALID-START.
       GET-001.
            MOVE SPACES TO ALPHA-RATE
                           F-NAMEFIELD.
            MOVE "ACCOUNTNUMBER" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO DR-ACCOUNT-NUMBER
                                 WS-ACCOUNTNUMBER.
            IF F-EXIT-CH = X"0C"
                 PERFORM READ-DEBTOR-NEXT
                 GO TO GET-003.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-DEBTOR-PREVIOUS
                 GO TO GET-003.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-SCREEN-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF DR-ACCOUNT-NUMBER = 0 OR = " "
                 CLOSE DEBTOR-MASTER
                 CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                 CANCEL WS-INQUIRY-PROGRAM
                 PERFORM CLEAR-SCREEN
                 PERFORM OPEN-000
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B"
                MOVE "INVALID KEY." TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-000.

            PERFORM ERROR-020.
            PERFORM READ-DEBTOR.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO GET-001.
            GO TO GET-005.
        GET-003.
            MOVE DR-ACCOUNT-NUMBER TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.

            MOVE "NAME1" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE DR-NAME TO F-NAMEFIELD
            MOVE 40      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD1" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-ADDRESS1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD2" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-ADDRESS2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD3" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-ADDRESS3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTCODE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-POST-CODE TO F-EDNAMEFIELDPOST.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-POST.

            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DR-DEL-ADDRESS1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD2" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DR-DEL-ADDRESS2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DR-DEL-ADDRESS3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TELEPHONE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DR-TELEPHONE TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TELEX" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DR-TELEX TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GSTNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DR-GSTNO TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESANAL" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DR-SALES-ANALYSIS TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "CREDITLIMIT"   TO F-FIELDNAME.
            MOVE 11              TO F-CBFIELDNAME.
            MOVE DR-CREDIT-LIMIT TO F-EDNAMEFIELDCREDIT.
            MOVE 7               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-CREDIT.

            MOVE "ACCBALANCE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DR-BALANCE TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "BALANCELASTSTATE" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE DR-BAL-LAST-STATE TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "CURRENTBAL" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DR-CURRENT TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "30DAYBAL" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-30DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "60DAYBAL" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-60DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "90DAYBAL" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-90DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "120DAYBAL" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DR-120DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "DATECREATED"   TO F-FIELDNAME.
            MOVE 11              TO F-CBFIELDNAME.
            MOVE DR-DATE-CREATED TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATELASTSALE"    TO F-FIELDNAME.
            MOVE 12                TO F-CBFIELDNAME.
            MOVE DR-DATE-LAST-SALE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE      TO F-NAMEFIELD.
            MOVE 10                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATELASTPAY"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            MOVE DR-DATE-LAST-PAY TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE     TO F-NAMEFIELD.
            MOVE 10               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESPTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-SALES-PTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "SALESYTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-SALES-YTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "SALESLAST" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DR-SALES-LAST TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "COSTPTD" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DR-COST-PTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "COSTYTD" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DR-COST-YTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "COSTLAST" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-COST-LAST TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "DISCOUNTCODE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE DR-DISCOUNT-CODE TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
 
            MOVE "TERMSCODE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DR-TERMS-CODE TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELIVERYCODE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE DR-DELIVERY-CODE TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUPPLY-Y-N" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DR-SUPPLY-Y-N TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESMAN" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-SALESMAN TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "B-O" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE DR-PART-ORDERS TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CORPGROUP" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DR-CORPORATE-GROUP TO F-EDNAMEFIELDGROUP.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-GROUP.

            MOVE "ACC-CONTACT" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE DR-ACCOUNTS-CONTACT TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALES-CONTACT" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE DR-SALES-CONTACT TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "S-EMAIL"      TO F-FIELDNAME
            MOVE 7              TO F-CBFIELDNAME
            MOVE DR-SALES-EMAIL TO F-NAMEFIELD
            MOVE 40             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACC-PHONE"    TO F-FIELDNAME
            MOVE 9              TO F-CBFIELDNAME
            MOVE DR-ACC-PHONE   TO F-NAMEFIELD
            MOVE 20             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACC-FAX"        TO F-FIELDNAME
            MOVE 7                TO F-CBFIELDNAME
            MOVE DR-ACC-FAX       TO F-NAMEFIELD
            MOVE 20               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "A-EMAIL"        TO F-FIELDNAME
            MOVE 7                TO F-CBFIELDNAME
            MOVE DR-ACC-EMAIL     TO F-NAMEFIELD
            MOVE 40               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            GO TO GET-001.
       GET-999.
            EXIT.
      *
       CLEAR-SCREEN-FORM SECTION.
       CLSC-000.
             MOVE " " TO DR-NAME
                         DR-ADDRESS1
                         DR-ADDRESS2
                         DR-ADDRESS3
                         DR-DEL-ADDRESS1
                         DR-DEL-ADDRESS2
                         DR-DEL-ADDRESS3
                         DR-TELEPHONE
                         DR-TELEX
                         DR-GSTNO
                         DR-DISCOUNT-CODE
                         DR-TERMS-CODE
                         DR-DELIVERY-CODE
                         DR-SUPPLY-Y-N
                         DR-SALESMAN.
             MOVE 0 TO   DR-ACCOUNT-NUMBER
                         DR-POST-CODE
                         DR-SALES-ANALYSIS
                         DR-CREDIT-LIMIT
                         DR-BALANCE
                         DR-BAL-LAST-STATE
                         DR-CURRENT
                         DR-30DAY
                         DR-60DAY
                         DR-90DAY
                         DR-120DAY
                         DR-DATE-CREATED
                         DR-DATE-LAST-SALE
                         DR-DATE-LAST-PAY
                         DR-SALES-PTD
                         DR-SALES-YTD
                         DR-SALES-LAST
                         DR-COST-PTD
                         DR-COST-YTD
                         DR-COST-LAST.
       CLSC-999.
             EXIT.      
      *
       READ-DEBTOR SECTION.
       RD-000.
             MOVE 0 TO WS-DEBTOR-ST1.
             MOVE WS-ACCOUNTNUMBER TO DR-ACCOUNT-NUMBER.
             START DEBTOR-MASTER KEY NOT < DR-ACCOUNT-NUMBER.
       RD-010.
             READ DEBTOR-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-SCREEN-FORM
                MOVE WS-ACCOUNTNUMBER TO DR-ACCOUNT-NUMBER
                MOVE "ENTER AN EXISTING DEBTOR OR PRESS 'NEXT PAGE'."
                   TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO RD-999.
             IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO RD-010.
       RD-999.
             EXIT.
      *
       START-DEBTOR SECTION.
       ST-DR-000.
              MOVE WS-ACCOUNTNUMBER TO DR-ACCOUNT-NUMBER.
              START DEBTOR-MASTER KEY NOT < DR-ACCOUNT-NUMBER.
       ST-DR-999.
             EXIT.
      *
       READ-DEBTOR-NEXT SECTION.
       RDNX-000.
             MOVE 0 TO WS-DEBTOR-ST1.
             MOVE 0 TO WS-NO-READS.
       RDNX-005. 
             READ DEBTOR-MASTER NEXT
                 AT END NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 0
                 PERFORM ERROR-020
                 GO TO RDNX-999.
             ADD 1 TO WS-NO-READS.
             IF WS-NO-READS > 10
                 MOVE 0 TO WS-DEBTOR-ST1
                 MOVE 0 TO WS-ACCOUNTNUMBER WS-NO-READS
                 PERFORM START-DEBTOR
                 MOVE
               "NEXT RECORD LOCKED AT ANOTHER STATION, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO RDNX-005
             ELSE
                 MOVE 0 TO WS-DEBTOR-ST1
                 PERFORM START-DEBTOR
                 GO TO RDNX-005.
       RDNX-999.
             EXIT.
      *
       READ-DEBTOR-PREVIOUS SECTION.
       RDPR-001.
           MOVE 0 TO WS-DEBTOR-ST1.
       RDPR-005. 
           READ DEBTOR-MASTER PREVIOUS
            AT END
              MOVE 0 TO DR-ACCOUNT-NUMBER
                        WS-ACCOUNTNUMBER
              PERFORM CLEAR-SCREEN-FORM
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO RDPR-999.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "DEBTORS BUSY ON READ-PREV, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RDPR-005.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE 
                "DEBTOR BUSY ON READ-PREV, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-ACCOUNTNUMBER
               MOVE 0 TO WS-DEBTOR-ST1
               PERFORM START-DEBTOR
               GO TO RDPR-005.
       RDPR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-DEBTOR TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO OPEN-000.
            MOVE Ws-Forms-Name   TO F-FILENAME
            MOVE Ws-cbForms-name TO F-CBFILENAME.
            MOVE "DrMaster"      TO F-FORMNAME
            MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE DEBTOR-MASTER.
            EXIT PROGRAM.
       END-999.
            EXIT.
       Copy "ReadFieldAlpha".
       Copy "ReadFieldAccount".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldNumeric5".
       Copy "WriteFieldNumeric6".
       Copy "WriteFieldCredit".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldAccount".
       Copy "WriteFieldPost".
       Copy "WriteFieldCode".
       Copy "WriteFieldGroup".
       Copy "GetSystemY2KDate".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".

      *
      * END-OF-JOB
