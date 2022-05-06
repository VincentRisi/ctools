        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrMasOld.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrMasterOld".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtorOld.
           COPY ChlfdDebtor.
           
       WORKING-STORAGE SECTION.
       77  WS-NO-READS        PIC 99 VALUE 0.
       77  WS-INQUIRY-PROGRAM PIC X(8) VALUE "DrNamOld".
       77  NEW-DEBTORNO       PIC X VALUE " ".      
       77  INVALID-START      PIC X VALUE " ".      
       77  WS-ACCOUNTNUMBER   PIC X(7) VALUE " ".
       77  WS-ACC-SAVE        PIC 9(7) VALUE 0.
       77  WS-END             PIC X VALUE " ".      
       01  WS-DEBTOROLD-STATUS.
           03  WS-DEBTOROLD-ST1 PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
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
            MOVE 0 TO DEBTOROLD-RECORD.
            MOVE " " TO INVALID-START.
            MOVE "N" TO NEW-DEBTORNO
                        WS-END.
       GET-001.              
            MOVE 0430 TO POS
            DISPLAY "*** OLD / DELETED RECORDS ONLY ***" AT POS.
            MOVE 2910 TO POS
            DISPLAY 
           "PRESS <F10> TO RE-INSTATE THE DEBTOR & DELETE THIS FILE."
             AT POS.
       
            MOVE "ACCOUNTNUMBER" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ACCOUNTNUMBER.
            MOVE WS-ACCOUNTNUMBER TO DROLD-ACCOUNT-NUMBER.
            IF F-EXIT-CH = X"0C"
      *           MOVE WS-ACC-SAVE TO DROLD-ACCOUNT-NUMBER
      *           PERFORM START-DEBTOROLD
                 PERFORM READ-DEBTOROLD-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM CLEAR-SCREEN
                 PERFORM DISPLAY-FORM
                 MOVE 0 TO DROLD-ACCOUNT-NUMBER
                           WS-ACCOUNTNUMBER
                 PERFORM START-DEBTOROLD
                 GO TO GET-000.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-FORM
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH = X"1F"
                 PERFORM WRITE-DEBTOR-RECORD
                 PERFORM DELETE-DEBTOROLD-RECORD
                 PERFORM CLEAR-FORM
                 PERFORM CLEAR-SCREEN
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF DROLD-ACCOUNT-NUMBER = 0 OR = " "
                 CLOSE DEBTOROLD-MASTER
                 CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                 CANCEL WS-INQUIRY-PROGRAM
                 PERFORM CLEAR-SCREEN
                 PERFORM OPEN-000
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-000.
            PERFORM READ-DEBTOROLD.
            IF WS-DEBTOROLD-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-DEBTOROLD-ST1
               GO TO GET-001.
            GO TO GET-005.
        GET-003.
            MOVE DROLD-ACCOUNT-NUMBER TO F-NAMEFIELD.
            MOVE 7                    TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.

            MOVE "NAME1" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE DROLD-NAME TO F-NAMEFIELD
            MOVE 40      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD1" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DROLD-ADDRESS1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD2" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DROLD-ADDRESS2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD3" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DROLD-ADDRESS3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTCODE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DROLD-POST-CODE TO F-EDNAMEFIELDPOST.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-POST.

            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DROLD-DEL-ADDRESS1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD2" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DROLD-DEL-ADDRESS2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DROLD-DEL-ADDRESS3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TELEPHONE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DROLD-TELEPHONE TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TELEX" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DROLD-TELEX TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GSTNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DROLD-GSTNO TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESANAL" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DROLD-SALES-ANALYSIS TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "CREDITLIMIT"   TO F-FIELDNAME.
            MOVE 11              TO F-CBFIELDNAME.
            MOVE DROLD-CREDIT-LIMIT TO F-EDNAMEFIELDCREDIT.
            MOVE 7               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-CREDIT.

            MOVE "ACCBALANCE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DROLD-BALANCE TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "BALANCELASTSTATE" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE DROLD-BAL-LAST-STATE TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "CURRENTBAL" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DROLD-CURRENT TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "30DAYBAL" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DROLD-30DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "60DAYBAL" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DROLD-60DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "90DAYBAL" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DROLD-90DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "120DAYBAL" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DROLD-120DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "DATECREATED"   TO F-FIELDNAME.
            MOVE 11              TO F-CBFIELDNAME.
            MOVE DROLD-DATE-CREATED TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATELASTSALE"    TO F-FIELDNAME.
            MOVE 12                TO F-CBFIELDNAME.
            MOVE DROLD-DATE-LAST-SALE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE      TO F-NAMEFIELD.
            MOVE 10                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATELASTPAY"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            MOVE DROLD-DATE-LAST-PAY TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE     TO F-NAMEFIELD.
            MOVE 10               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESPTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DROLD-SALES-PTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "SALESYTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DROLD-SALES-YTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "SALESLAST" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DROLD-SALES-LAST TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "COSTPTD" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DROLD-COST-PTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "COSTYTD" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DROLD-COST-YTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "COSTLAST" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DROLD-COST-LAST TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "DISCOUNTCODE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE DROLD-DISCOUNT-CODE TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
 
            MOVE "TERMSCODE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DROLD-TERMS-CODE TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELIVERYCODE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE DROLD-DELIVERY-CODE TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUPPLY-Y-N" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DROLD-SUPPLY-Y-N TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESMAN" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DROLD-SALESMAN TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "B-O" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE DROLD-PART-ORDERS TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CORPGROUP" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DROLD-CORPORATE-GROUP TO F-EDNAMEFIELDGROUP.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-GROUP.

            MOVE "ACC-CONTACT" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE DROLD-ACCOUNTS-CONTACT TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALES-CONTACT" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE DROLD-SALES-CONTACT TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "S-EMAIL"      TO F-FIELDNAME
            MOVE 7              TO F-CBFIELDNAME
            MOVE DROLD-SALES-EMAIL TO F-NAMEFIELD
            MOVE 40             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACC-PHONE"    TO F-FIELDNAME
            MOVE 9              TO F-CBFIELDNAME
            MOVE DROLD-ACC-PHONE   TO F-NAMEFIELD
            MOVE 20             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACC-FAX"        TO F-FIELDNAME
            MOVE 7                TO F-CBFIELDNAME
            MOVE DROLD-ACC-FAX       TO F-NAMEFIELD
            MOVE 20               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "A-EMAIL"        TO F-FIELDNAME
            MOVE 7                TO F-CBFIELDNAME
            MOVE DROLD-ACC-EMAIL  TO F-NAMEFIELD
            MOVE 40               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            GO TO GET-001.
       GET-999.
            EXIT.
      *
       DELETE-DEBTOROLD-RECORD SECTION.
       DDR-000.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               GO TO DDR-999.
       DDR-010.
            DELETE DEBTOROLD-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOROLD-ST1 NOT = 0
               MOVE "ERROR IN DELETING DRMASTEROLD, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOROLD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOROLD-ST1
               GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       WRITE-DEBTOR-RECORD SECTION.
       RDR-010.
           MOVE DEBTOROLD-RECORD TO DEBTOR-RECORD.
           GO TO RDR-020.
       RDR-020.
           WRITE DEBTOR-RECORD
                INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE "DEBTOR RECORD ALREADY EXISTS, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-999.
           IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR RECORD BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO RDR-020.
       RDR-999.
           EXIT.
      *
       READ-DEBTOROLD SECTION.
       RD-000.
             MOVE 0 TO WS-DEBTOROLD-ST1.
             MOVE WS-ACCOUNTNUMBER TO DROLD-ACCOUNT-NUMBER.
             START DEBTOROLD-MASTER KEY NOT < DROLD-ACCOUNT-NUMBER.
       RD-010.
             READ DEBTOROLD-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-DEBTOROLD-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FORM
                MOVE WS-ACCOUNTNUMBER TO DROLD-ACCOUNT-NUMBER
                MOVE "ENTER AN EXISTING DEBTOR OR PRESS 'NEXT PAGE'."
                   TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO RD-999.
             IF WS-DEBTOROLD-ST1 = 10
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-999.
             IF WS-DEBTOROLD-ST1 NOT = 0
                MOVE 0 TO WS-DEBTOROLD-ST1
                MOVE "DEBTOROLD RECORD BUSY ON READ, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-010.
       RD-999.
             EXIT.
      *
       START-DEBTOROLD SECTION.
       ST-DROLD-000.
              MOVE WS-ACCOUNTNUMBER TO DROLD-ACCOUNT-NUMBER.
              START DEBTOROLD-MASTER KEY NOT < DROLD-ACCOUNT-NUMBER.
       ST-DROLD-999.
             EXIT.
      *
       READ-DEBTOROLD-NEXT SECTION.
       RDNX-000.
             MOVE 0 TO WS-DEBTOROLD-ST1.
             MOVE 0 TO WS-NO-READS.
       RDNX-005. 
             READ DEBTOROLD-MASTER NEXT WITH LOCK
                 AT END
              MOVE 0 TO DROLD-ACCOUNT-NUMBER
                           WS-ACCOUNTNUMBER
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDNX-999.
             IF WS-DEBTOROLD-ST1 = 0
                 PERFORM ERROR-020
                 GO TO RDNX-999.
             ADD 1 TO WS-NO-READS.
             IF WS-NO-READS > 10
               MOVE 
           "DEBTOROLD FILE BUSY ON READ-NEXT, IN 1 SEC GOING TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOROLD-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
                 MOVE 0 TO WS-DEBTOROLD-ST1
                 MOVE 0 TO WS-ACCOUNTNUMBER WS-NO-READS
                 PERFORM START-DEBTOROLD
                 GO TO RDNX-005
             ELSE
                 MOVE 0 TO WS-DEBTOROLD-ST1
                 PERFORM START-DEBTOROLD
                 GO TO RDNX-005.
           MOVE DROLD-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER
                                        WS-ACC-SAVE.
           MOVE "N" TO NEW-DEBTORNO.
       RDNX-999.
             EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO DROLD-NAME
                         DROLD-ADDRESS1
                         DROLD-ADDRESS2
                         DROLD-ADDRESS3
                         DROLD-DEL-ADDRESS1
                         DROLD-DEL-ADDRESS2
                         DROLD-DEL-ADDRESS3
                         DROLD-TELEPHONE
                         DROLD-TELEX
                         DROLD-GSTNO
                         DROLD-DISCOUNT-CODE
                         DROLD-TERMS-CODE
                         DROLD-DELIVERY-CODE
                         DROLD-SUPPLY-Y-N
                         DROLD-SALESMAN.
             MOVE 0 TO   DROLD-ACCOUNT-NUMBER
                         DROLD-POST-CODE
                         DROLD-SALES-ANALYSIS
                         DROLD-CREDIT-LIMIT
                         DROLD-BALANCE
                         DROLD-BAL-LAST-STATE
                         DROLD-CURRENT
                         DROLD-30DAY
                         DROLD-60DAY
                         DROLD-90DAY
                         DROLD-120DAY
                         DROLD-DATE-CREATED
                         DROLD-DATE-LAST-SALE
                         DROLD-DATE-LAST-PAY
                         DROLD-SALES-PTD
                         DROLD-SALES-YTD
                         DROLD-SALES-LAST
                         DROLD-COST-PTD
                         DROLD-COST-YTD
                         DROLD-COST-LAST.
       CLSC-999.
             EXIT.      
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOROLD-MASTER.
            IF WS-DEBTOROLD-ST1 NOT = 0
               MOVE "DEBTOROLD FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-010.
               
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
            CLOSE DEBTOROLD-MASTER
                  DEBTOR-MASTER.
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
