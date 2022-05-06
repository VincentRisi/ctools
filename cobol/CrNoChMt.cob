        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrNoChMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectCrMaster".
           Copy "SelectCrTrans".
           Copy "SelectCrJrn".
           Copy "SelectCrRemittance".
           Copy "SelectSlDaily".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCrTrans.
           COPY ChlfdCrJrn.
           COPY ChlfdCrRemit.
           COPY ChlfdDaily.

       WORKING-STORAGE SECTION.
       77  WS-NEWCREDITORNO      PIC X VALUE " ".      
       77  WS-END                PIC X VALUE " ".      
       77  WS-INVALID            PIC X VALUE " ".      
       77  WS-CREDITORNUMBER     PIC 9(7) VALUE 0.
       77  WS-NEWCREDITORNUMBER  PIC 9(7) VALUE 0.
       77  WS-CREATE-ONE-ACC     PIC X VALUE " ".      
       77  WS-OLDCREDITORNUMBER  PIC 9(7) VALUE 0.
       77  WS-ACCOUNT-NUMBER     PIC 9(7) VALUE 0.
       77  WS-BALANCE            PIC S9(7)V99.
       77  WS-LAST-BALANCE       PIC S9(7)V99.
       77  WS-CURRENT            PIC S9(7)V99.
       77  WS-30DAY              PIC S9(7)V99.
       77  WS-60DAY              PIC S9(7)V99.
       77  WS-90DAY              PIC S9(7)V99.
       77  WS-120DAY             PIC S9(7)V99.
       77  WS-NO-REWRITES        PIC S9(7).
       77  WS-NO-DIS             PIC Z(7).
       01  DEBTOR-SALES-AMOUNTS.
           03  WS-PURCHASE-PTD          PIC S9(7)V99.
           03  WS-PURCHASE-YTD          PIC S9(7)V99.
           03  WS-PURCHASE-LAST         PIC S9(7)V99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1 PIC 99.
       01  WS-CRTRANS-STATUS.
           03  WS-CRTRANS-ST1  PIC 99.
       01  WS-CRJRN-STATUS.
           03  WS-CRJRN-ST1    PIC 99.
       01  WS-REMI-STATUS.
           03  WS-REMI-ST1     PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1    PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       Copy "WsDateInfo".
      *
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
           IF WS-INVALID NOT = "Y"
              PERFORM READ-CRJRN
              PERFORM READ-CREDITOR-TRANS
              PERFORM READ-REMITTANCES.
              
           PERFORM ERROR1-020.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO CREDITOR-RECORD.
            MOVE "N" TO WS-NEWCREDITORNO
                        WS-INVALID
                        WS-END.
       GET-001.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "OLDNUMBER" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 7           TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO CR-ACCOUNT-NUMBER.
            IF F-EXIT-CH = X"0C"
                MOVE WS-CREDITORNUMBER TO CR-ACCOUNT-NUMBER
                PERFORM START-CREDITOR
                PERFORM READ-CREDITOR-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-FORM
               GO TO GET-000.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CREDITOR-RECORD
               PERFORM CLEAR-FORM
               GO TO GET-000.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-CREDITOR.
            IF WS-NEWCREDITORNO = "Y"
               MOVE "YOU CAN ONLY CHANGE AN EXISTING CREDITOR NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-000.
            GO TO GET-005.
        GET-003.
            MOVE "OLDNUMBER" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
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
            MOVE CR-SETT-DISC TO F-EDNAMEFIELDCRED.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-CREDIT.

            MOVE "TRADEDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE CR-TRADE-DISC TO F-EDNAMEFIELDCRED.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-CREDIT.

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
       GET-500.
            MOVE CR-ACCOUNT-NUMBER TO WS-OLDCREDITORNUMBER.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NEWNUMBER"       TO F-FIELDNAME
            MOVE 9                 TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 7                 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD       TO ALPHA-RATE
            PERFORM DECIMALISE-RATE.
            IF F-EXIT-CH = X"01" OR = X"07"
               PERFORM CLEAR-FORM
               PERFORM DISPLAY-FORM
               GO TO GET-000.
            IF F-NAMEFIELD = "    "
               MOVE "YOU MUST ENTER A NEW CREDITOR NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-500.

            MOVE CR-BALANCE        TO WS-BALANCE
            MOVE CR-BAL-LAST-STATE TO WS-LAST-BALANCE
            MOVE CR-CURRENT        TO WS-CURRENT
            MOVE CR-30DAY          TO WS-30DAY
            MOVE CR-60DAY          TO WS-60DAY
            MOVE CR-90DAY          TO WS-90DAY
            MOVE CR-120DAY         TO WS-120DAY.
            
            MOVE CR-PURCHASE-PTD   TO WS-PURCHASE-PTD
            MOVE CR-PURCHASE-YTD   TO WS-PURCHASE-YTD
            MOVE CR-PURCHASE-LAST  TO WS-PURCHASE-LAST.
       GET-550.
            PERFORM CLEAR-010.
            MOVE 3010 TO POS
            DISPLAY "ENTER Y=MERGE TWO A/C'S, N=RENAME A/C ONLY" AT POS
            MOVE 2910 TO POS
            DISPLAY "DO YOU WISH TO CREATE ONE A/C FROM TWO : [ ]"
              AT POS
            ADD 42 TO POS
            ACCEPT WS-CREATE-ONE-ACC AT POS.
            IF WS-CREATE-ONE-ACC NOT = "N" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-550.
               
            PERFORM ERROR1-020
            PERFORM ERROR-020.
               
            MOVE NUMERIC-RATE TO CR-ACCOUNT-NUMBER 
                                 WS-NEWCREDITORNUMBER.
            IF WS-CREATE-ONE-ACC = "Y"
               PERFORM READ-CREDITOR.
               
            IF WS-CREATE-ONE-ACC = "N"
                MOVE "Y" TO WS-NEWCREDITORNO.
            PERFORM REWRITE-CREDITOR-RECORD.
            IF WS-INVALID = "Y"
                MOVE 
            "THIS TRY HAS BEEN ABORTED, PRESS <ESC> TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM CLEAR-FORM
                GO TO GET-999.
            MOVE WS-OLDCREDITORNUMBER TO CR-ACCOUNT-NUMBER.
            PERFORM READ-CREDITOR.
            PERFORM DELETE-CREDITOR-RECORD.
       GET-999.
            EXIT.
      *
       READ-CRJRN SECTION.
       RCRJRN-000.
            MOVE 2910 TO POS
            DISPLAY "CHANGING CRJRN TRANSACTIONS TO NEW ACCOUNT.       "
            AT POS.
            MOVE WS-OLDCREDITORNUMBER TO CRJRN-CRACC-NUMBER.
            START CRJRN-FILE KEY NOT < CRJRN-CRACC-NUMBER
                 INVALID KEY NEXT SENTENCE.
            IF WS-CRJRN-ST1 NOT = 0
               GO TO RCRJRN-999.
       RCRJRN-002.
            READ CRJRN-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-CRJRN-ST1 = 10
               GO TO RCRJRN-999.
            IF WS-CRJRN-ST1 NOT = 0
               MOVE 0 TO WS-CRJRN-ST1
               MOVE "CRJRN FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RCRJRN-002.
            IF CRJRN-CRACC-NUMBER NOT = WS-OLDCREDITORNUMBER
               GO TO RCRJRN-999.
       RCRJRN-005.
            MOVE WS-NEWCREDITORNUMBER TO CRJRN-CRACC-NUMBER.
            REWRITE CRJRN-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CRJRN-ST1 NOT = 0
               MOVE CRJRN-KEY              TO WS-DAILY-1ST
               MOVE CRJRN-CRACC-NUMBER     TO WS-DAILY-2ND
               MOVE "NO CHNG TO CRJRN    " TO WS-DAILY-3RD
               MOVE WS-NEWCREDITORNUMBER   TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RCRJRN-002.
       RCRJRN-999.
           EXIT.
      *
       READ-CREDITOR-TRANS SECTION.
       RDTR-000.
            MOVE 2910 TO POS.
            DISPLAY "CHANGING CR-TRANSACTIONS TO NEW ACCOUNT.          "
               AT POS.
               
            MOVE WS-OLDCREDITORNUMBER TO CRTR-ACC-NUMBER.
            START CRTR-FILE KEY NOT < CRTR-ACC-NUMBER
                 INVALID KEY NEXT SENTENCE.
            IF WS-CRTRANS-ST1 NOT = 0
               GO TO RDTR-999.
       RDTR-002.
            READ CRTR-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-CRTRANS-ST1 = 10
               GO TO RDTR-999.
            IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE "CR-TRANS FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDTR-002.
            IF CRTR-ACC-NUMBER NOT = WS-OLDCREDITORNUMBER
               GO TO RDTR-999.
       RDTR-005.
            MOVE WS-NEWCREDITORNUMBER TO CRTR-ACC-NUMBER.
            REWRITE CRTR-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CRTRANS-ST1 NOT = 0
               MOVE CRTR-KEY               TO WS-DAILY-1ST
               MOVE CRTR-ACC-NUMBER        TO WS-DAILY-2ND
               MOVE "NO CHNG TO CRTRANS  " TO WS-DAILY-3RD
               MOVE WS-NEWCREDITORNUMBER   TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RDTR-002.
       RDTR-999.
           EXIT.
      *
       READ-REMITTANCES SECTION.
       CRREMIT-000.
            MOVE 0 TO WS-NO-REWRITES.
            MOVE 2910 TO POS
            DISPLAY "CHANGING CRREMITTANCES TO NEW ACCOUNT.       "
            AT POS.
       CRREMIT-001.
            MOVE WS-OLDCREDITORNUMBER TO CRREM-ACC-NUMBER.
            START CRREMIT-FILE KEY NOT < CRREM-ACC-NUMBER
                 INVALID KEY NEXT SENTENCE.
            IF WS-REMI-ST1 NOT = 0
               MOVE "BAD START ON REMIT CHANGE, 'ESC' TO EXIT."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-REMI-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO CRREMIT-999.
       CRREMIT-002.
            READ CRREMIT-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-REMI-ST1 = 10
                MOVE "READ CRREMIT END OF FILE, 'ESC' TO EXIT."
                 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
               GO TO CRREMIT-999.
            IF WS-REMI-ST1 NOT = 0
               MOVE 0 TO WS-REMI-ST1
               MOVE "REMIT FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO CRREMIT-002.
               
            MOVE CRREM-ACC-NUMBER TO WS-ACCOUNT-NUMBER.
            IF WS-ACCOUNT-NUMBER NOT = WS-OLDCREDITORNUMBER
               GO TO CRREMIT-999.
       CRREMIT-005.
            ADD 1 TO WS-NO-REWRITES
            MOVE WS-NO-REWRITES TO WS-NO-DIS
            MOVE 3010 TO POS
            DISPLAY "REMITTANCE NUMBER WRITTEN:" AT POS
            ADD 27 TO POS
            DISPLAY WS-NO-DIS AT POS
            ADD 10 TO POS
            DISPLAY CRREM-KEY AT POS.
            
            PERFORM CRREMIT-700.
            
            MOVE WS-NEWCREDITORNUMBER TO CRREM-ACC-NUMBER.
            WRITE CRREM-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-REMI-ST1 NOT = 0
               MOVE CRREM-KEY              TO WS-DAILY-1ST
               MOVE CRREM-ACC-NUMBER       TO WS-DAILY-2ND
               MOVE "NO CHNG TO CRREMIT  " TO WS-DAILY-3RD
               MOVE WS-NEWCREDITORNUMBER   TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.

            GO TO CRREMIT-001.
       CRREMIT-700.
            DELETE CRREMIT-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-REMI-ST1 NOT = 0
               MOVE CRREM-KEY              TO WS-DAILY-1ST
               MOVE CRREM-ACC-NUMBER       TO WS-DAILY-2ND
               MOVE "NO DELETE TO CRREMIT" TO WS-DAILY-3RD
               MOVE WS-OLDCREDITORNUMBER   TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
       CRREMIT-999.
           EXIT.
      *
       DELETE-CREDITOR-RECORD SECTION.
       DSR-000.
            IF WS-NEWCREDITORNO = "Y"
               GO TO DSR-999.
       DSR-010.
            DELETE CREDITOR-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-CREDITOR-ST1 NOT = 0
            MOVE "CREDITOR BUSY ON DELETE, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CREDITOR-ST1
               GO TO DSR-010. 
       DSR-999.
            EXIT. 
      *
       RELEASE-CREDITOR-RECORD SECTION.
       REL-000.
            IF WS-CREDITOR-ST1 = 51
               UNLOCK CREDITOR-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-CREDITOR-RECORD SECTION.
       RSR-010.
          IF WS-NEWCREDITORNO = "Y"
           IF WS-CREATE-ONE-ACC = "N"
              GO TO RSR-020.
            ADD WS-BALANCE        TO CR-BALANCE
            ADD WS-LAST-BALANCE   TO CR-BAL-LAST-STATE
            ADD WS-CURRENT        TO CR-CURRENT
            ADD WS-30DAY          TO CR-30DAY
            ADD WS-60DAY          TO CR-60DAY
            ADD WS-90DAY          TO CR-90DAY
            ADD WS-120DAY         TO CR-120DAY.
            
            ADD WS-PURCHASE-PTD   TO CR-PURCHASE-PTD
            ADD WS-PURCHASE-YTD   TO CR-PURCHASE-YTD
            ADD WS-PURCHASE-LAST  TO CR-PURCHASE-LAST.
       RSR-015.
          REWRITE CREDITOR-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
              MOVE 
           "CR-RECORD DOESN'T EXIST ON REWRITE, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE "Y" TO WS-INVALID
              GO TO RSR-999.
          IF WS-CREDITOR-ST1 NOT = 0
              MOVE 0 TO WS-CREDITOR-ST1
              MOVE "CREDITOR RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSR-015.
          GO TO RSR-999.
       RSR-020.
          WRITE CREDITOR-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
              MOVE 
           "CR-RECORD ALREADY EXISTS ON WRITE, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE "Y" TO WS-INVALID
              GO TO RSR-999.
          IF WS-CREDITOR-ST1 NOT = 0
              MOVE 0 TO WS-CREDITOR-ST1
              MOVE "CR-RECORD BUSY ON WRITE, <ESC> TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSR-020.
       RSR-999.
          EXIT.
      *
       READ-CREDITOR SECTION.
       R-CR-000.
             MOVE CR-ACCOUNT-NUMBER TO WS-CREDITORNUMBER.
             START CREDITOR-MASTER KEY NOT < CR-KEY
                 INVALID KEY NEXT SENTENCE.
       R-CR-010.
             READ CREDITOR-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FORM
                MOVE "Y" TO WS-NEWCREDITORNO
                MOVE WS-CREDITORNUMBER TO CR-ACCOUNT-NUMBER
                GO TO R-CR-999.
             IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITOR RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO R-CR-010.
             MOVE "N" TO WS-NEWCREDITORNO.
       R-CR-999.
             EXIT.
      *
       START-CREDITOR SECTION.
       CR-CR-000.
              MOVE WS-CREDITORNUMBER TO CR-ACCOUNT-NUMBER.
              START CREDITOR-MASTER KEY NOT LESS CR-ACCOUNT-NUMBER.
       CR-CR-999.
             EXIT.
      *
       READ-CREDITOR-NEXT SECTION.
       RSN-005. 
           READ CREDITOR-MASTER NEXT WITH LOCK
             AT END 
               MOVE 0 TO CR-ACCOUNT-NUMBER
                         WS-CREDITORNUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR FILE BUSY, PRESS 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-005.
           IF WS-CREDITOR-ST1 NOT = 0
              MOVE "CREDITOR BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CREDITOR-ST1
              PERFORM START-CREDITOR
              GO TO RSN-005.
           MOVE CR-ACCOUNT-NUMBER TO WS-CREDITORNUMBER.
           MOVE "N" TO WS-NEWCREDITORNO.
       RSN-999.
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
                         CR-CURRENCY
                         CR-TERMS
                         CR-FOREIGN-LOCAL
                         CR-PAY-METHOD
                         CR-SUPPLIER-NUMBER
                         CR-CONTACT-NAME.
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
                         CR-BALANCE
                         CR-BAL-LAST-STATE
                         CR-CURRENT
                         CR-30DAY
                         CR-60DAY
                         CR-90DAY
                         CR-120DAY.
       CLSC-500.
           PERFORM RELEASE-CREDITOR-RECORD.
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
           OPEN I-O CRREMIT-FILE.
           IF WS-REMI-ST1 NOT = 0
               MOVE "CRREMIT-FILE BUSY ON OPEN, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRREMIT TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO OPEN-002.
       OPEN-005.
            OPEN I-O CRTR-FILE.
            IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE "CRTR-FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-006.
            OPEN I-O CRJRN-FILE.
            IF WS-CRJRN-ST1 NOT = 0
               MOVE 0 TO WS-CRJRN-ST1
               MOVE "CRJRN-FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-006.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrNoChMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CREDITOR-MASTER
                 CRREMIT-FILE
                 CRTR-FILE
                 CRJRN-FILE.
      *     STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAccount".
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAccount".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAmount1".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldCredit".
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
       Copy "Error1Message".
       Copy "WriteDailyExcep1".
      * END-OF-JOB
