        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrMastIq.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectCrMaster".
      *
        DATA DIVISION.
        FILE SECTION.
         COPY ChlfdCreditor.

       WORKING-STORAGE SECTION.
       77  WS-INQUIRY         PIC X(8) VALUE "CrNameIq".
       77  INVALID-START      PIC X VALUE " ".      
       77  NEW-CRNO           PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-ACCOUNTNUMBER   PIC X(7) VALUE " ".
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1   PIC 99.
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
            MOVE 0 TO CREDITOR-RECORD.
            MOVE "N" TO NEW-CRNO
                        WS-END.
       GET-001.
            MOVE "                      " TO F-NAMEFIELD.
            MOVE " " TO INVALID-START.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ACCOUNTNUMBER.
            MOVE WS-ACCOUNTNUMBER TO CR-ACCOUNT-NUMBER.
            IF F-EXIT-CH = X"0C"
                 PERFORM READ-CREDITOR-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-SCREEN-FORM
                PERFORM DISPLAY-FORM
              GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-CREDITOR-PREVIOUS
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-SCREEN-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-SCREEN-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-000.
            IF CR-ACCOUNT-NUMBER = 0
                CLOSE CREDITOR-MASTER
                CALL WS-INQUIRY USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-INQUIRY
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                GO TO GET-001.
            PERFORM READ-CREDITOR.
            IF WS-CREDITOR-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO GET-001.
            GO TO GET-005.
        GET-003.
            MOVE CR-ACCOUNT-NUMBER TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            MOVE "NAME1" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CR-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD1" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CR-ADDRESS1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD2" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CR-ADDRESS2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD3" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CR-ADDRESS3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTCODE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CR-POST-CODE TO F-EDNAMEFIELDPOST.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-POST.

            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CR-DEL-ADDRESS1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD2" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CR-DEL-ADDRESS2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CR-DEL-ADDRESS3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TELEPHONE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE CR-TELEPHONE TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "FAX" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE CR-FAX TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CURRENCY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CR-CURRENCY TO F-NAMEFIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TERMS" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CR-TERMS TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "FOREIGNLOCAL" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE CR-FOREIGN-LOCAL TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PAYMETHOD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE CR-PAY-METHOD TO F-NAMEFIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SETTDISC" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CR-SETT-DISC TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "TRADEDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE CR-TRADE-DISC TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "SUPPLIERNUMBER" TO F-FIELDNAME.
            MOVE 14 TO F-CBFIELDNAME.
            MOVE CR-SUPPLIER-NUMBER TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATECREATED" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE CR-DATE-CREATED TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVDATE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CR-DATE-LAST-INVOICE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATELASTPAY" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE CR-DATE-LAST-PAY TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PURCHASE-FOR-YTD" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE CR-FOR-PURCH-YTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "PURCHASE-FOR-LAST" TO F-FIELDNAME.
            MOVE 17 TO F-CBFIELDNAME.
            MOVE CR-FOR-PURCH-LAST TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "PURCHASEPTD" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE CR-PURCHASE-PTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "PURCHASEYTD" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE CR-PURCHASE-YTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "PURCHASELAST" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE CR-PURCHASE-LAST TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "BALANCE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CR-BALANCE TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "BALLASTSTATE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE CR-BAL-LAST-STATE TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "CURRENT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CR-CURRENT TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "30DAY" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CR-30DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "60DAY" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CR-60DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "90DAY" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CR-90DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "120DAY" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE CR-120DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "CONTACTNAME" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE CR-CONTACT-NAME TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "EMAIL"  TO F-FIELDNAME.
            MOVE 5        TO F-CBFIELDNAME.
            MOVE CR-EMAIL TO F-NAMEFIELD.
            MOVE 40       TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCCONTACT"        TO F-FIELDNAME.
            MOVE 10                  TO F-CBFIELDNAME.
            MOVE CR-ACC-CONTACT-NAME TO F-NAMEFIELD.
            MOVE 25                  TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCEMAIL"   TO F-FIELDNAME.
            MOVE 8            TO F-CBFIELDNAME.
            MOVE CR-ACC-EMAIL TO F-NAMEFIELD.
            MOVE 40           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BANK-NAME"       TO F-FIELDNAME
            MOVE 9                 TO F-CBFIELDNAME
            MOVE CR-CAMS-BANK-NAME TO F-NAMEFIELD
            MOVE 20                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BANK-NUM"       TO F-FIELDNAME
            MOVE 8                TO F-CBFIELDNAME
            MOVE CR-CAMS-BANK-NUM TO F-NAMEFIELD
            MOVE 11               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BRANCH-NUM"       TO F-FIELDNAME
            MOVE 10                 TO F-CBFIELDNAME
            MOVE CR-CAMS-BRANCH-NUM TO F-NAMEFIELD
            MOVE 6                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCTYPE"         TO F-FIELDNAME
            MOVE 7                 TO F-CBFIELDNAME
            MOVE CR-CAMS-ACC-TYPE  TO F-NAMEFIELD
            MOVE 1                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            GO TO GET-001.
       GET-999.
            EXIT.
      *
       CLEAR-SCREEN-FORM SECTION.
       CLSC-000.
             MOVE " " TO CR-NAME
                         CR-ADDRESS1
                         CR-ADDRESS2
                         CR-ADDRESS3
                         CR-DEL-ADDRESS1
                         CR-DEL-ADDRESS2
                         CR-DEL-ADDRESS3
                         CR-TELEPHONE
                         CR-FAX
                         CR-CURRENCY
                         CR-TERMS
                         CR-FOREIGN-LOCAL
                         CR-PAY-METHOD
                         CR-SUPPLIER-NUMBER
                         CR-CONTACT-NAME
                         CR-EMAIL
                         CR-ACC-CONTACT-NAME
                         CR-ACC-EMAIL
                         CR-CAMS-BANK-NAME
                         CR-CAMS-BANK-NUM
                         CR-CAMS-BRANCH-NUM
                         CR-CAMS-ACC-TYPE.
             MOVE 0 TO   CR-ACCOUNT-NUMBER
                         CR-POST-CODE
                         CR-SETT-DISC
                         CR-TRADE-DISC
                         CR-DATE-CREATED
                         CR-DATE-LAST-INVOICE
                         CR-DATE-LAST-PAY
                         CR-PURCHASE-PTD
                         CR-PURCHASE-YTD
                         CR-PURCHASE-LAST
                         CR-FOR-PURCH-YTD
                         CR-FOR-PURCH-LAST
                         CR-BALANCE
                         CR-BAL-LAST-STATE
                         CR-CURRENT
                         CR-30DAY
                         CR-60DAY
                         CR-90DAY
                         CR-120DAY.
       CLSC-999.
             EXIT.      
      *
       READ-CREDITOR SECTION.
       RD-000.
             MOVE WS-ACCOUNTNUMBER TO CR-ACCOUNT-NUMBER.
             START CREDITOR-MASTER KEY NOT < CR-ACCOUNT-NUMBER.
       RD-010.
             READ CREDITOR-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-SCREEN-FORM
                MOVE WS-ACCOUNTNUMBER TO CR-ACCOUNT-NUMBER
                MOVE "Enter An Existing CREDITOR OR Press 'NEXT PAGE'"
                   TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO RD-999.
             IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITOR BUSY ON READ, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-010.
             PERFORM ERROR-020.
       RD-999.
             EXIT.
      *
       START-CREDITOR SECTION.
       ST-CR-000.
              MOVE WS-ACCOUNTNUMBER TO CR-ACCOUNT-NUMBER.
              START CREDITOR-MASTER KEY NOT < CR-ACCOUNT-NUMBER.
       ST-CR-999.
             EXIT.
      *
       READ-CREDITOR-NEXT SECTION.
       RDNX-005. 
             READ CREDITOR-MASTER NEXT
               AT END
               MOVE 0 TO WS-ACCOUNTNUMBER CR-ACCOUNT-NUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDNX-999.
             IF WS-CREDITOR-ST1 NOT = 0
                 MOVE "CREDITOR BUSY ON READ-NEXT, 'ESC' TO RETRY"
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 MOVE 0 TO WS-CREDITOR-ST1
                 PERFORM START-CREDITOR
                 GO TO RDNX-005.
           MOVE CR-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER.
           MOVE "N" TO NEW-CRNO.
       RDNX-999.
             EXIT.
      *
       READ-CREDITOR-PREVIOUS SECTION.
       RDPREV-005. 
           READ CREDITOR-MASTER PREVIOUS WITH LOCK
            AT END
              MOVE 0 TO CR-ACCOUNT-NUMBER
                        WS-ACCOUNTNUMBER
              MOVE "Y" TO WS-END
            MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDPREV-999.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR BUSY ON READ-PREVIOUS-23, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDPREV-005.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE "CREDITOR BUSY ON READ-PREVIOUS, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CREDITOR-ST1
               PERFORM START-CREDITOR
               GO TO RDPREV-005.
           MOVE CR-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER.
       RDPREV-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrMastIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CREDITOR-MASTER.
      *      STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAccount".
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAccount".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAmountDis".
       Copy "WriteFieldCode".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldNumeric6".
       Copy "WriteFieldPost".
      *
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
      * END-OF-JOB
