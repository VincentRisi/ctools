        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrMasOld.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectCrMaster".
           Copy "SelectCrMasterOld".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCreditorOld.

       WORKING-STORAGE SECTION.
       77  WS-INQUIRY         PIC X(8) VALUE "CrNamOld".
       77  NEW-CREDITORNO     PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-ACCOUNTNUMBER   PIC 9(7) VALUE 0.
       77  WS-ACC-SAVE        PIC 9(7) VALUE 0.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1    PIC 99.
       01  WS-CREDITOROLD-STATUS.
           03  WS-CREDITOROLD-ST1 PIC 99.
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
            MOVE "N" TO NEW-CREDITORNO
                        WS-END.
       GET-001.              
            MOVE 0423 TO POS
            DISPLAY "*OLD / DELETED RECORDS ONLY*" AT POS.
            MOVE 2910 TO POS
            DISPLAY 
           "PRESS <F10> TO RE-INSTATE THE CREDITOR & DELETE THIS FILE."
             AT POS.
       
            MOVE 0 TO F-NAMEFIELDACC.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ACCOUNT.
            MOVE F-NAMEFIELDACC TO CROLD-ACCOUNT-NUMBER.
            IF F-EXIT-CH = X"0C"
                 PERFORM READ-CREDITOROLD-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM CLEAR-SCREEN
                 PERFORM DISPLAY-FORM
                 MOVE 0 TO CROLD-ACCOUNT-NUMBER
                           WS-ACCOUNTNUMBER
                 PERFORM START-CREDITOROLD
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-CREDITOROLD-PREVIOUS
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM CLEAR-SCREEN
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH = X"1F"
                 PERFORM WRITE-CREDITOR-RECORD
                 PERFORM DELETE-CREDITOROLD-RECORD
                 PERFORM CLEAR-FORM
                 PERFORM CLEAR-SCREEN
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF CROLD-ACCOUNT-NUMBER = 0 OR = " "
                CLOSE CREDITOROLD-MASTER
                CALL WS-INQUIRY USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-INQUIRY
                PERFORM OPEN-002
                PERFORM DISPLAY-FORM
                GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-000.
            PERFORM READ-CREDITOROLD.
            IF NEW-CREDITORNO = "Y"
                 GO TO GET-001.
            GO TO GET-005.
       GET-003.
            MOVE "ACCNO"              TO F-FIELDNAME
            MOVE 5                    TO F-CBFIELDNAME
            MOVE CROLD-ACCOUNT-NUMBER TO F-NAMEFIELD
            MOVE 7                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.

            MOVE "NAME1" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CROLD-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD1" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CROLD-ADDRESS1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD2" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CROLD-ADDRESS2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD3" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CROLD-ADDRESS3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTCODE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CROLD-POST-CODE TO F-EDNAMEFIELDPOST.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-POST.

            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CROLD-DEL-ADDRESS1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD2" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CROLD-DEL-ADDRESS2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CROLD-DEL-ADDRESS3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TELEPHONE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE CROLD-TELEPHONE TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "FAX" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE CROLD-FAX TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CURRENCY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CROLD-CURRENCY TO F-NAMEFIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "FOREIGNLOCAL" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE CROLD-FOREIGN-LOCAL TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TERMS" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CROLD-TERMS TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PAYMETHOD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE CROLD-PAY-METHOD TO F-NAMEFIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SETTDISC" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CROLD-SETT-DISC TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "TRADEDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE CROLD-TRADE-DISC TO F-EDNAMEFIELDAMOUNTDIS.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "SUPPLIERNUMBER" TO F-FIELDNAME.
            MOVE 14 TO F-CBFIELDNAME.
            MOVE CROLD-SUPPLIER-NUMBER TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATECREATED"   TO F-FIELDNAME.
            MOVE 11              TO F-CBFIELDNAME.
            MOVE CROLD-DATE-CREATED TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVDATE"            TO F-FIELDNAME.
            MOVE 7                    TO F-CBFIELDNAME.
            MOVE CROLD-DATE-LAST-INVOICE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE         TO F-NAMEFIELD.
            MOVE 10                   TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATELASTPAY"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            MOVE CROLD-DATE-LAST-PAY TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE     TO F-NAMEFIELD.
            MOVE 10               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PURCHASE-FOR-YTD" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE CROLD-FOR-PURCH-YTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "PURCHASE-FOR-LAST" TO F-FIELDNAME.
            MOVE 17 TO F-CBFIELDNAME.
            MOVE CROLD-FOR-PURCH-LAST TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "PURCHASEPTD" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE CROLD-PURCHASE-PTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "PURCHASEYTD" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE CROLD-PURCHASE-YTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "PURCHASELAST" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE CROLD-PURCHASE-LAST TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "BALANCE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CROLD-BALANCE TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "BALLASTSTATE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE CROLD-BAL-LAST-STATE TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "CURRENT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CROLD-CURRENT TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "30DAY" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CROLD-30DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "60DAY" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CROLD-60DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "90DAY" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CROLD-90DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "120DAY" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE CROLD-120DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "CONTACTNAME" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE CROLD-CONTACT-NAME TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "EMAIL"  TO F-FIELDNAME.
            MOVE 5        TO F-CBFIELDNAME.
            MOVE CROLD-EMAIL TO F-NAMEFIELD.
            MOVE 40       TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCCONTACT"        TO F-FIELDNAME.
            MOVE 10                  TO F-CBFIELDNAME.
            MOVE CROLD-ACC-CONTACT-NAME TO F-NAMEFIELD.
            MOVE 25                  TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCEMAIL"   TO F-FIELDNAME.
            MOVE 8            TO F-CBFIELDNAME.
            MOVE CROLD-ACC-EMAIL TO F-NAMEFIELD.
            MOVE 40           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BANK-NAME"       TO F-FIELDNAME
            MOVE 9                 TO F-CBFIELDNAME
            MOVE CROLD-CAMS-BANK-NAME TO F-NAMEFIELD
            MOVE 20                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BANK-NUM"       TO F-FIELDNAME
            MOVE 8                TO F-CBFIELDNAME
            MOVE CROLD-CAMS-BANK-NUM TO F-NAMEFIELD
            MOVE 11               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BRANCH-NUM"       TO F-FIELDNAME
            MOVE 10                 TO F-CBFIELDNAME
            MOVE CROLD-CAMS-BRANCH-NUM TO F-NAMEFIELD
            MOVE 6                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCTYPE"            TO F-FIELDNAME
            MOVE 7                    TO F-CBFIELDNAME
            MOVE CROLD-CAMS-ACC-TYPE  TO F-NAMEFIELD
            MOVE 1                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            GO TO GET-001.
       GET-999.
            EXIT.
      *
       DELETE-CREDITOROLD-RECORD SECTION.
       DCR-000.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                GO TO DCR-999.
       DCR-010.
           DELETE CREDITOROLD-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE "ERROR IN DELETING CRMASTEROLD, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CREDITOROLD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO DCR-010.
       DCR-999.
           EXIT.
      *
       RELEASE-CREDITOR-RECORD SECTION.
       REL-000.
           UNLOCK CREDITOR-MASTER.
       REL-999.
           EXIT.
      *
       WRITE-CREDITOR-RECORD SECTION.
       WOR-010.
            MOVE CREDITOROLD-RECORD TO CREDITOR-RECORD.
       WOR-020.
            WRITE CREDITOR-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE "CREDITOR REC ALREADY EXISTS, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO WOR-999.
            IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITOR REC BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO WOR-020.
       WOR-999.
            EXIT.
      *
       READ-CREDITOROLD SECTION.
       RD-000.
           MOVE CROLD-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER.
           START CREDITOROLD-MASTER KEY NOT < CROLD-ACCOUNT-NUMBER.
        RD-010.
           READ CREDITOROLD-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOROLD-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-CREDITOROLD-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-CREDITORNO
                MOVE WS-ACCOUNTNUMBER TO CROLD-ACCOUNT-NUMBER
                GO TO RD-999.
           IF WS-CREDITOROLD-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOROLD-ST1
                MOVE "CREDITOR BUSY ON READ-LOCK, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-010.
           MOVE "N" TO NEW-CREDITORNO.
           MOVE CROLD-ACCOUNT-NUMBER TO WS-ACC-SAVE.
       RD-999.
             EXIT.
      *
       START-CREDITOROLD SECTION.
       ST-CROLD-000.
              MOVE WS-ACCOUNTNUMBER TO CROLD-ACCOUNT-NUMBER.
              START CREDITOROLD-MASTER KEY NOT < CROLD-ACCOUNT-NUMBER.
       ST-CR-999.
             EXIT.
      *
       READ-CREDITOROLD-NEXT SECTION.
       RDNX-005. 
           READ CREDITOROLD-MASTER NEXT WITH LOCK
            AT END
              MOVE 0 TO CROLD-ACCOUNT-NUMBER
                           WS-ACCOUNTNUMBER
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDNX-999.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR BUSY ON READ-NEXT-23, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDNX-005.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE "CREDITOR BUSY ON READ-NEXT, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CREDITOR-ST1
               PERFORM START-CREDITOROLD
               GO TO RDNX-005.
           MOVE CROLD-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER
                                        WS-ACC-SAVE.
           MOVE "N" TO NEW-CREDITORNO.
       RDNX-999.
           EXIT.
      *
       READ-CREDITOROLD-PREVIOUS SECTION.
       RDPR-005. 
           READ CREDITOROLD-MASTER PREVIOUS WITH LOCK
            AT END
              MOVE 0 TO CROLD-ACCOUNT-NUMBER
                           WS-ACCOUNTNUMBER
              MOVE "Y" TO WS-END
            MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDPR-999.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR BUSY ON READ-PREVIOUS-23, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDPR-005.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE "CREDITOR BUSY ON READ-PREVIOUS, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CREDITOR-ST1
               PERFORM START-CREDITOROLD
               GO TO RDPR-005.
           MOVE CROLD-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER
                                        WS-ACC-SAVE.
           MOVE "N" TO NEW-CREDITORNO.
       RDPR-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
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
                         CR-TERMS
                         CR-CURRENCY
                         CR-FOREIGN-LOCAL
                         CR-PAY-METHOD
                         CR-SUPPLIER-NUMBER
                         CR-CONTACT-NAME
                         CR-EMAIL
                         CR-ACC-CONTACT-NAME
                         CR-ACC-EMAIL
                         CR-CAMS-BANK-NAME
                         CR-CAMS-BANK-NUM
                         CR-CAMS-BRANCH-NUM.
             MOVE 0 TO   CR-ACCOUNT-NUMBER
                         CR-POST-CODE
                         CR-SETT-DISC
                         CR-TRADE-DISC
                         CR-DATE-CREATED
                         CR-DATE-LAST-INVOICE
                         CR-DATE-LAST-PAY
                         CR-FOR-PURCH-YTD
                         CR-FOR-PURCH-LAST
                         CR-PURCHASE-PTD
                         CR-PURCHASE-YTD
                         CR-PURCHASE-LAST
                         CR-BALANCE
                         CR-BAL-LAST-STATE
                         CR-CURRENT
                         CR-30DAY
                         CR-60DAY
                         CR-90DAY
                         CR-120DAY.
             MOVE WS-ACCOUNTNUMBER TO CR-ACCOUNT-NUMBER.
       CLSC-999.
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
       OPEN-002.
            OPEN I-O CREDITOROLD-MASTER.
            IF WS-CREDITOROLD-ST1 NOT = 0
               MOVE "CREDITOROLD BUSY ON OPEN I-O, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-CREDITOROLD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CREDITOROLD-ST1
               GO TO OPEN-003.
       OPEN-0021.
             GO TO OPEN-005.
       OPEN-003.
            OPEN OUTPUT CREDITOROLD-MASTER.
            IF WS-CREDITOROLD-ST1 NOT = 0
               MOVE "CREDITOROLD BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-CREDITOROLD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CREDITOROLD-ST1
               GO TO OPEN-002.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *      ACCEPT WS-DATE FROM DATE.
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
           CLOSE CREDITOR-MASTER
                 CREDITOROLD-MASTER.
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
       Copy "GetSystemY2KDate".
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
       Copy "Error1Message".
      * END-OF-JOB
