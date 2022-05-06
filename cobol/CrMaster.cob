        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrMaster.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
    
        SPECIAL-NAMES.
        CLASS WS-VALID-EMAIL IS
          '@' '_' '.' '-' '#'
          'a' THRU 'i'
          'j' THRU 'r'
          's' THRU 'z'
          '0' THRU '9'.

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
       77  WS-INQUIRY         PIC X(8) VALUE "CrNameIq".
       77  NEW-CREDITORNO     PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-ACCOUNTNUMBER   PIC 9(7) VALUE 0.
       77  WS-ACC-SAVE        PIC 9(7) VALUE 0.
       77  WS-ACC-ERROR       PIC X VALUE " ".      
       01  WS-SPACE-CNT            PIC 9(2) VALUE ZEROES.
       01  WS-EMAIL                PIC X(50).
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
           PERFORM FILL-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE 0 TO CREDITOR-RECORD.
            MOVE "N" TO NEW-CREDITORNO
                        WS-END.
       GET-001.              
            MOVE 0 TO F-NAMEFIELDACC.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ACCOUNT.
            MOVE F-NAMEFIELDACC TO CR-ACCOUNT-NUMBER.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-ACC-SAVE TO CR-ACCOUNT-NUMBER
                 PERFORM START-CREDITOR
                 PERFORM READ-CREDITOR-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-CREDITOR-PREVIOUS
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-000.
            IF CR-ACCOUNT-NUMBER = 0 OR = " "
                CLOSE CREDITOR-MASTER
                CALL WS-INQUIRY USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-INQUIRY
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                GO TO GET-001.
            PERFORM READ-CREDITOR.
            IF NEW-CREDITORNO = "Y"
                 PERFORM CLEAR-FORM
                 MOVE "DATECREATED" TO F-FIELDNAME
                 MOVE 11            TO F-CBFIELDNAME
                 MOVE WS-DATE       TO CR-DATE-CREATED SPLIT-DATE
                 PERFORM CONVERT-DATE-FORMAT
                 MOVE DISPLAY-DATE  TO F-NAMEFIELD
                 MOVE 10            TO F-CBFIELDLENGTH
                 PERFORM WRITE-FIELD-ALPHA
                 GO TO GET-999.
            GO TO GET-005.
       GET-003.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
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

            MOVE "FOREIGNLOCAL" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE CR-FOREIGN-LOCAL TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TERMS" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CR-TERMS TO F-NAMEFIELD.
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

            MOVE "DATECREATED"   TO F-FIELDNAME.
            MOVE 11              TO F-CBFIELDNAME.
            MOVE CR-DATE-CREATED TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVDATE"            TO F-FIELDNAME.
            MOVE 7                    TO F-CBFIELDNAME.
            MOVE CR-DATE-LAST-INVOICE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE         TO F-NAMEFIELD.
            MOVE 10                   TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATELASTPAY"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            MOVE CR-DATE-LAST-PAY TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE     TO F-NAMEFIELD.
            MOVE 10               TO F-CBFIELDLENGTH.
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
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
               GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NAME1" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-NAME.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-240.
            IF CR-NAME = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
       FILL-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "POSTADD1" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-ADDRESS1.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CR-ADDRESS1 = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, ENTER A CHARACTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
       FILL-015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "POSTADD2" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-ADDRESS2.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CR-ADDRESS2 = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, ENTER A CHARACTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-015.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "POSTADD3" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-ADDRESS3.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
            MOVE "POSTCODE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDPOST
                                 CR-POST-CODE.
            PERFORM WRITE-FIELD-POST.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-DEL-ADDRESS1.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
            MOVE "DELADD2" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-DEL-ADDRESS2.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-DEL-ADDRESS3.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-035.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
            MOVE "TELEPHONE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-TELEPHONE.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-040.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
            MOVE "FAX" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-FAX.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-045.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
            MOVE "CURRENCY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-CURRENCY.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-050.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
       FILL-056.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TERMS" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-TERMS.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CR-TERMS = " "
               MOVE "THIS FIELD MUST NOT BE SPACES, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-056.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-055.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-056.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-056.
       FILL-057.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "FOREIGNLOCAL" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-FOREIGN-LOCAL.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CR-FOREIGN-LOCAL NOT = "F" AND NOT = "L"
               MOVE "THIS FIELD MUST BE 'F' OR 'L', RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-057.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-056.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-057.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-057.
       FILL-058.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PAYMETHOD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-PAY-METHOD.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CR-PAY-METHOD NOT = "A" AND NOT = "M"
               MOVE "THIS FIELD MUST BE 'A' OR 'M', RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-058.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
              GO TO FILL-057.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-058.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-058.
       FILL-060.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SETTDISC" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 CR-SETT-DISC.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-058.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
            MOVE "TRADEDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDAMOUNTDIS
                                 CR-TRADE-DISC.
            PERFORM WRITE-FIELD-AMOUNTDIS.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-060.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
       FILL-070.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SUPPLIERNUMBER" TO F-FIELDNAME.
            MOVE 14 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-SUPPLIER-NUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CR-SUPPLIER-NUMBER NOT > " "
               MOVE "THIS FIELD MUST NOT BE BLANK, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-070.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-065.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
       FILL-102.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATECREATED" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-102.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CR-DATE-CREATED
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-102.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-070.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-102.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-102.
       FILL-105.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVDATE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-105.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CR-DATE-LAST-INVOICE
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-105.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-102.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
            MOVE "DATELASTPAY" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-110.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CR-DATE-LAST-PAY
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-110.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-105.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
       FILL-112.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PURCHASE-FOR-YTD" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 CR-FOR-PURCH-YTD.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-110.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-112.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-112.
       FILL-114.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PURCHASE-FOR-LAST" TO F-FIELDNAME.
            MOVE 17 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 CR-FOR-PURCH-LAST.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-112.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-114.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-114.
       FILL-115.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PURCHASEPTD" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 CR-PURCHASE-PTD.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-110.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
            MOVE "PURCHASEYTD" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 CR-PURCHASE-YTD.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-115.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
            MOVE "PURCHASELAST" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 CR-PURCHASE-LAST.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-120.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
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
       FILL-170.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BALANCE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 CR-BALANCE.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-125.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-170.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-170.
       FILL-175.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BALLASTSTATE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 CR-BAL-LAST-STATE.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-170.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-175.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-175.
       FILL-180.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CURRENT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 CR-CURRENT.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-175.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-180.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-180.
       FILL-185.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "30DAY" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 CR-30DAY.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-180.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-185.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-185.
       FILL-190.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "60DAY" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 CR-60DAY.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-185.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-190.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-190.
       FILL-195.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "90DAY" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 CR-90DAY.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-190.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-195.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-195.
       FILL-200.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "120DAY" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 CR-120DAY.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-195.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-200.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-200.
       FILL-205.
            PERFORM ERROR1-020.

            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CONTACTNAME" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-CONTACT-NAME.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CR-CONTACT-NAME NOT > " "
               MOVE "THIS FIELD MUST NOT BE BLANK, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-205.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-200.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-205.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-205.
       FILL-210.
            MOVE 0 TO WS-SPACE-CNT.
            MOVE "THIS FIELD MUST BE ENTERED IN lower case ONLY"
              TO WS-MESSAGE
              PERFORM ERROR1-000.
       
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "EMAIL" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 40      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-EMAIL.

            IF F-NAMEFIELD = " "
                GO TO FILL-212.

            PERFORM ERROR1-020.

            IF F-EXIT-CH = X"07"
               PERFORM ERROR1-020
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.

            MOVE FUNCTION LOWER-CASE(F-NAMEFIELD) TO CR-EMAIL 
                                                     WS-EMAIL.
            INSPECT WS-EMAIL TALLYING WS-SPACE-CNT FOR CHARACTERS
                BEFORE INITIAL SPACE.
 
            IF WS-EMAIL(1:(WS-SPACE-CNT)) IS NOT WS-VALID-EMAIL
                MOVE "EMAIL ADDRESS HAS AN INVALID CHARACTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-210.
 
            MOVE "EMAIL"    TO F-FIELDNAME.
            MOVE 5          TO F-CBFIELDNAME.
            MOVE WS-EMAIL   TO F-NAMEFIELD
            MOVE 40         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM CHECK-EMAIL-FOR-VALIDITY.
            IF WS-ACC-ERROR = "Y"
                GO TO FILL-210.
            IF WS-SPACE-CNT < 10
                MOVE 
            "EMAIL ADDRESS INVALID AS IT'S TOO SHORT, 'ESC' TO RETRY." 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-210.

            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-205.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-210.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-210.
       FILL-212.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACCCONTACT" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-ACC-CONTACT-NAME.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CR-ACC-CONTACT-NAME NOT > " "
               MOVE "THIS FIELD MUST NOT BE BLANK, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-212.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-210.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-212.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-212.
       FILL-213.
            MOVE 0 TO WS-SPACE-CNT.
            MOVE "THIS FIELD MUST BE ENTERED IN lower case ONLY"
              TO WS-MESSAGE
              PERFORM ERROR1-000.
       
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACCEMAIL" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 40         TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.

            IF F-NAMEFIELD = " "
                GO TO FILL-215.
                
            IF F-EXIT-CH = X"07"
               PERFORM ERROR1-020
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
 
            PERFORM ERROR1-020.

            MOVE FUNCTION LOWER-CASE(F-NAMEFIELD) TO CR-ACC-EMAIL 
                                                     WS-EMAIL.
            INSPECT WS-EMAIL TALLYING WS-SPACE-CNT FOR CHARACTERS
                BEFORE INITIAL SPACE.
            IF WS-EMAIL(1:(WS-SPACE-CNT)) IS NOT WS-VALID-EMAIL
                MOVE "EMAIL ADDRESS HAS AN INVALID CHARACTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-213.
 
            MOVE "ACCEMAIL" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE WS-EMAIL   TO F-NAMEFIELD
            MOVE 40         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
 
            PERFORM CHECK-EMAIL-FOR-VALIDITY.
            IF WS-ACC-ERROR = "Y"
                GO TO FILL-213.
            IF WS-SPACE-CNT < 10
                MOVE 
            "EMAIL ADDRESS INVALID AS IT'S TOO SHORT, 'ESC' TO RETRY." 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-213.

            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-212.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-213.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-213.
       FILL-215.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BANK-NAME"     TO F-FIELDNAME
            MOVE 9               TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 20              TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD     TO CR-CAMS-BANK-NAME.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
               
            IF CR-CAMS-BANK-NAME = " "
               MOVE "THIS ENTRY CANNOT BE BLANK, 'ESC' TO RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-215.
               
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-213.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-215.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-215.
       FILL-220.
            MOVE 
            "ACC NUMBER MUST BE 11 CHARS, PAD WITH ZERO'S ON THE LEFT."
              TO WS-MESSAGE
              PERFORM ERROR1-000.
       
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BANK-NUM"      TO F-FIELDNAME
            MOVE 8               TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 11              TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD     TO CR-CAMS-BANK-NUM.

            PERFORM ERROR1-020.

            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
               
            IF CR-CAMS-BANK-NUM = " "
               MOVE "THIS ENTRY CANNOT BE BLANK, 'ESC' TO RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-220.
               
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-215.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-220.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-220.
       FILL-225.
            MOVE 
            "BRANCH NUM MUST BE 6 CHARS, PAD WITH ZERO'S ON THE LEFT."
              TO WS-MESSAGE
              PERFORM ERROR1-000.
       
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BRANCH-NUM"    TO F-FIELDNAME
            MOVE 10              TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 6               TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD     TO CR-CAMS-BRANCH-NUM.

            PERFORM ERROR1-020.

            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
               
            IF CR-CAMS-BRANCH-NUM = " "
               MOVE "THIS ENTRY CANNOT BE BLANK, 'ESC' TO RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-225.
               
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-220.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-225.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-225.
       FILL-240.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACCTYPE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1         TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CR-CAMS-ACC-TYPE.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CR-CAMS-ACC-TYPE NOT = "1" AND NOT = "2"
               MOVE "THIS FIELD MUST BE 1 OR 2, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-240.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM READ-CREDITOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-225.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-240.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-240.

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       DELETE-CREDITOR-RECORD SECTION.
       DCR-000.
           IF NEW-CREDITORNO = "Y"
                GO TO DCR-999.
       DCR-010.
           IF CR-BALANCE = 0
            IF CR-BAL-LAST-STATE = 0
                GO TO DCR-005.
           IF CR-BALANCE NOT = 0
               MOVE "BALANCE NOT ZERO - CAN'T DELETE, 'ESC' TO CLEAR."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DCR-999.
           IF CR-BAL-LAST-STATE NOT = 0
               MOVE
          "BALANCE LAST-PERIOD NOT ZERO - CAN'T DELETE, 'ESC' TO CLEAR."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DCR-999.
       DCR-005.
            PERFORM WRITE-CREDITOR-OLD-RECORD.
       DCR-020.
           DELETE CREDITOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE "CREDITOR BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO DCR-020.
       DCR-999.
           EXIT.
      *
       RELEASE-CREDITOR-RECORD SECTION.
       REL-000.
           UNLOCK CREDITOR-MASTER.
       REL-999.
           EXIT.
      *
       WRITE-CREDITOR-OLD-RECORD SECTION.
       WOR-010.
            MOVE CREDITOR-RECORD TO CREDITOROLD-RECORD.
            GO TO WOR-020.
       WOR-015.
            REWRITE CREDITOROLD-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-CREDITOROLD-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOROLD-ST1
                MOVE 
             "OLD CREDITOR RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO WOR-015.
            GO TO WOR-999.
       WOR-020.
            WRITE CREDITOROLD-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-CREDITOROLD-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOROLD-ST1
                MOVE
             "OLD CREDITOR REC BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO WOR-015.
       WOR-999.
            EXIT.
      *
       REWRITE-CREDITOR-RECORD SECTION.
       RCR-010.
            IF NEW-CREDITORNO = "Y"
               GO TO RCR-020.
            REWRITE CREDITOR-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITOR BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RCR-010.
            GO TO RCR-999.
       RCR-020.
            WRITE CREDITOR-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITOR BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RCR-020.
       RCR-999.
            EXIT.
      *
       READ-CREDITOR SECTION.
       RD-000.
           MOVE CR-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER.
           START CREDITOR-MASTER KEY NOT < CR-ACCOUNT-NUMBER.
        RD-010.
           READ CREDITOR-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-CREDITOR-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-CREDITORNO
                MOVE WS-ACCOUNTNUMBER TO CR-ACCOUNT-NUMBER
                GO TO RD-999.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITOR BUSY ON READ-LOCK, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-010.
           MOVE "N" TO NEW-CREDITORNO.
           MOVE CR-ACCOUNT-NUMBER TO WS-ACC-SAVE.
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
           READ CREDITOR-MASTER NEXT WITH LOCK
            AT END
              MOVE 0 TO CR-ACCOUNT-NUMBER
                        WS-ACCOUNTNUMBER
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDNX-999.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR BUSY ON READ-NEXT-23, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDNX-005.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE "CREDITOR BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CREDITOR-ST1
               PERFORM START-CREDITOR
               GO TO RDNX-005.
           MOVE CR-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER
                                     WS-ACC-SAVE.
           MOVE "N" TO NEW-CREDITORNO.
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
           MOVE CR-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER
                                     WS-ACC-SAVE.
           MOVE "N" TO NEW-CREDITORNO.
       RDPREV-999.
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
       Copy "CheckEmailForValidity".
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
