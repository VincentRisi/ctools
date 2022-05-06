        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrNoChMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrTrans".
         Copy "SelectSlRegister".
         Copy "SelectStTrans".
         Copy "SelectStTransLy".
         Copy "SelectDrContact".
         Copy "SelectSlRegLy".
         Copy "SelectSlDaily".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdStTrans.
           COPY ChlfdDrTrans.
           COPY ChlfdRegister.
           COPY ChlfdDrCont.
           COPY ChlfdStTransLy.
           COPY ChlfdRegisterLy.
           COPY ChlfdDaily.

       WORKING-STORAGE SECTION.
       77  NEW-DEBTORNO        PIC X VALUE " ".      
       77  WS-END              PIC X VALUE " ".      
       77  WS-CREATE-ONE-ACC   PIC X VALUE " ".      
       77  Ws-InValid          PIC X VALUE " ".      
       77  WS-NUMBER           PIC 9(7) VALUE 0.
       77  WS-REWRITE-ATTEMPT  PIC 9 VALUE 0.
       77  WS-NUMBER-DIS       PIC Z(6)9.
       77  WS-DEBTORNUMBER     PIC 9(7) VALUE 0.
       77  WS-NEWDEBTORNUMBER  PIC 9(7) VALUE 0.
       77  WS-OLDDEBTORNUMBER  PIC 9(7) VALUE 0.
       77  WS-BALANCE          PIC S9(7)V99.
       77  WS-LAST-BALANCE     PIC S9(7)V99.
       77  WS-CURRENT          PIC S9(7)V99.
       77  WS-30DAY            PIC S9(7)V99.
       77  WS-60DAY            PIC S9(7)V99.
       77  WS-90DAY            PIC S9(7)V99.
       77  WS-120DAY           PIC S9(7)V99.
       77  WS-SALESMAN         PIC X VALUE " ".
       01  DEBTOR-SALES-AMOUNTS.
           03  WS-SALES-PTD          PIC S9(7)V99.
           03  WS-SALES-YTD          PIC S9(7)V99.
           03  WS-SALES-LAST         PIC S9(7)V99.
           03  WS-COST-PTD           PIC S9(7)V99.
           03  WS-COST-YTD           PIC S9(7)V99.
           03  WS-COST-LAST          PIC S9(7)V99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1     PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTR-ST1       PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTR-ST1       PIC 99.
       01  WS-STTRANSLY-STATUS.
           03  WS-STTRANSLY-ST1  PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1       PIC 99.
       01  WS-INCR-LY-STATUS.
           03  WS-INCR-LY-ST1    PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1      PIC 99.
       01  WS-CONTACT-STATUS.
           03  WS-DC-ST1         PIC 99.
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

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN.
       CONT-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           IF Ws-InValid = "N"
              PERFORM READ-BACK-ORDERS
              PERFORM READ-STTRANS-LY
              PERFORM READ-DEBTOR-TRANS
              PERFORM READ-REGISTER-RECORDS
              PERFORM READ-REGISTERLY-RECORDS.
           MOVE 0 TO WS-MESSAGE
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           GO TO CONT-010.
       CONT-999.
           Exit.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO DEBTOR-RECORD.
            MOVE "N" TO NEW-DEBTORNO
                        Ws-InValid
                        WS-END.
       GET-001.              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "OLDNUMBER" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO DR-ACCOUNT-NUMBER.
            IF F-EXIT-CH = X"0C"
                MOVE WS-DEBTORNUMBER TO DR-ACCOUNT-NUMBER
                PERFORM START-DEBTOR
                PERFORM READ-DEBTOR-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-FORM
               GO TO GET-000.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO GET-000.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-DEBTOR.
            IF NEW-DEBTORNO = "Y"
               MOVE "YOU CAN ONLY CHANGE AN EXISTING DEBTOR NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-000.
            GO TO GET-005.
        GET-003.
            MOVE "OLDNUMBER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-ACCOUNT-NUMBER TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            MOVE "NAME1" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DR-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
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
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TELEX" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DR-TELEX TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
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

            MOVE "CREDITLIMIT" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE DR-CREDIT-LIMIT TO F-EDNAMEFIELDCRED.
            MOVE 5 TO F-CBFIELDLENGTH.
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
       GET-500.
            MOVE DR-ACCOUNT-NUMBER TO WS-OLDDEBTORNUMBER.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NEWNUMBER" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            IF F-EXIT-CH = X"01" OR = X"07"
               PERFORM CLEAR-FORM
               PERFORM DISPLAY-FORM
               GO TO GET-000.
            IF F-NAMEFIELD = "    "
               MOVE "YOU MUST ENTER A NEW DEBTOR NUMBER." TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-500.
               
            MOVE DR-BALANCE        TO WS-BALANCE
            MOVE DR-BAL-LAST-STATE TO WS-LAST-BALANCE
            MOVE DR-CURRENT        TO WS-CURRENT
            MOVE DR-30DAY          TO WS-30DAY
            MOVE DR-60DAY          TO WS-60DAY
            MOVE DR-90DAY          TO WS-90DAY
            MOVE DR-120DAY         TO WS-120DAY.
            
            MOVE DR-SALESMAN   TO WS-SALESMAN.

            MOVE DR-SALES-PTD  TO WS-SALES-PTD
            MOVE DR-SALES-YTD  TO WS-SALES-YTD
            MOVE DR-SALES-LAST TO WS-SALES-LAST
            MOVE DR-COST-PTD   TO WS-COST-PTD
            MOVE DR-COST-YTD   TO WS-COST-YTD
            MOVE DR-COST-LAST  TO WS-COST-LAST.
       GET-550.
            PERFORM CLEAR-010.
            MOVE 3010 TO POS
            DISPLAY "ENTER Y=MERGE TWO A/C'S, N=RENAME A/C ONLY" AT POS
            MOVE 2910 TO POS
            DISPLAY "DO YOU WISH TO CREATE ONE A/C FROM TWO : [ ]"
              AT POS
            ADD 42 TO POS

           MOVE WS-CREATE-ONE-ACC TO CDA-DATA.
           MOVE 1                 TO CDA-DATALEN.
           MOVE 26                TO CDA-ROW.
           MOVE 51                TO CDA-COL.
           MOVE CDA-WHITE         TO CDA-COLOR.
           MOVE 'F'               TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CREATE-ONE-ACC.

            IF WS-CREATE-ONE-ACC NOT = "N" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-550.
               
            PERFORM ERROR1-020
            PERFORM ERROR-020.
               
            MOVE NUMERIC-RATE TO DR-ACCOUNT-NUMBER WS-NEWDEBTORNUMBER.
            IF WS-CREATE-ONE-ACC = "Y"
               PERFORM READ-DEBTOR.
               
            IF WS-CREATE-ONE-ACC = "N"
               MOVE "Y" TO NEW-DEBTORNO.
            PERFORM REWRITE-DEBTOR-RECORD.
            
            IF Ws-InValid = "Y"
                MOVE "THIS TRY HAS BEEN ABORTED, ENTER A NEW NUMBER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM CLEAR-FORM
                GO TO GET-999.
            IF DR-SALESMAN NOT = WS-SALESMAN
               PERFORM RENAME-DEBTOR-CALL-SCHEDULE.

            MOVE WS-OLDDEBTORNUMBER TO DR-ACCOUNT-NUMBER.
            PERFORM READ-DEBTOR.
            PERFORM DELETE-DEBTOR-RECORD.
       GET-999.
            EXIT.
      *
       RENAME-DEBTOR-CALL-SCHEDULE SECTION.
       RDCS-000.
           PERFORM ERROR-020.
           MOVE 3010 TO POS
           DISPLAY "RENAMING CALL SCHEDULE RECORD ...." AT POS.
           MOVE 0 TO WS-DC-ST1.
           MOVE DR-ACCOUNT-NUMBER TO DC-ACCOUNT-NUMBER.
           START DRCONT-MASTER KEY NOT < DC-KEY
                INVALID KEY NEXT SENTENCE.
       RDCS-010.
           READ DRCONT-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-DC-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-DC-ST1
                GO TO RDCS-999.
           IF WS-DC-ST1 NOT = 0
                MOVE "DR-CONTACT RECORD BUSY, PRESS 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DC-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR-020
                MOVE 0 TO WS-DC-ST1
                GO TO RDCS-010.
       RDCS-020.
            MOVE WS-SALESMAN TO DC-SALESMAN.
            REWRITE DRCONT-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-DC-ST1 NOT = 0
                MOVE "DR-CONTACT RECORD BUSY ON REWRITE, BE PATIENT"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DC-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR-020
                MOVE 0 TO WS-DC-ST1
                GO TO RDCS-020.
       RDCS-999.
           EXIT.
      *
       READ-BACK-ORDERS SECTION.
       RBO-000.
            MOVE 0 TO WS-NUMBER.
            MOVE 2910 TO POS.
            DISPLAY
            "Changing StTrans File to new ACCOUNT, Record No:          "
            AT POS.
            MOVE WS-OLDDEBTORNUMBER TO STTR-ACCOUNT-NUMBER.
            START STOCK-TRANS-FILE KEY NOT < STTR-ACCOUNT-NUMBER
                 INVALID KEY NEXT SENTENCE.
            IF WS-STTR-ST1 NOT = 0
               GO TO RBO-999.
       RBO-002.
            READ STOCK-TRANS-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-STTR-ST1 = 10
               GO TO RBO-999.
            IF WS-STTR-ST1 NOT = 0
               MOVE 0 TO WS-STTR-ST1
               MOVE "ST-TRANS FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STTR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR-020
               GO TO RBO-002.
            IF STTR-ACCOUNT-NUMBER NOT = WS-OLDDEBTORNUMBER
               GO TO RBO-999.
            ADD 1          TO WS-NUMBER
            MOVE WS-NUMBER TO WS-NUMBER-DIS
            MOVE 2959 TO POS
            DISPLAY WS-NUMBER-DIS AT POS.
       RBO-005.
            MOVE WS-NEWDEBTORNUMBER TO STTR-ACCOUNT-NUMBER.
            REWRITE STOCK-TRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-STTR-ST1 NOT = 0
               MOVE STTR-KEY               TO WS-DAILY-1ST
               MOVE STTR-ACCOUNT-NUMBER    TO WS-DAILY-2ND
               MOVE "NO CHNG TO ST-TRANS " TO WS-DAILY-3RD
               MOVE WS-NEWDEBTORNUMBER     TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RBO-002.
       RBO-999.
           EXIT.
      *
       READ-STTRANS-LY SECTION.
       RST-LY-000.
            MOVE 0 TO WS-NUMBER.
            MOVE 2910 TO POS.
            DISPLAY
            "Changing St-Trans-LY To New Account, Record No:           "
            AT POS.
            MOVE WS-OLDDEBTORNUMBER TO STTR-LY-ACCOUNT-NUMBER.
            START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-ACCOUNT-NUMBER
                 INVALID KEY NEXT SENTENCE.
            IF WS-STTRANSLY-ST1 NOT = 0
               GO TO RST-LY-999.
       RST-LY-002.
            READ STOCK-TRANSLY-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-STTRANSLY-ST1 = 10
               GO TO RST-LY-999.
            IF WS-STTRANSLY-ST1 NOT = 0
               MOVE "ST-TRANS-LY BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR-020
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO RST-LY-002.
            IF STTR-LY-ACCOUNT-NUMBER NOT = WS-OLDDEBTORNUMBER
               GO TO RST-LY-999.
            ADD 1          TO WS-NUMBER
            MOVE WS-NUMBER TO WS-NUMBER-DIS
            MOVE 2958 TO POS
            DISPLAY WS-NUMBER-DIS AT POS.
       RST-LY-005.
            MOVE WS-NEWDEBTORNUMBER TO STTR-LY-ACCOUNT-NUMBER.
            REWRITE STOCK-TRANSLY-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-STTRANSLY-ST1 NOT = 0
               MOVE STTR-LY-KEY             TO WS-DAILY-1ST
               MOVE STTR-LY-ACCOUNT-NUMBER  TO WS-DAILY-2ND
               MOVE "NO CHNG TO STTRANSLY" TO WS-DAILY-3RD
               MOVE WS-NEWDEBTORNUMBER     TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RST-LY-002.
       RST-LY-999.
           EXIT.
      *
       READ-DEBTOR-TRANS SECTION.
       RDTR-000.
            MOVE 0 TO WS-NUMBER.
            MOVE 2910 TO POS.
            DISPLAY 
            "Changing Debtor-Trans To New Account, Record No:          "
            AT POS.
            MOVE WS-OLDDEBTORNUMBER TO DRTR-ACCOUNT-NUMBER
            MOVE 0                  TO DRTR-DATE.
            START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACC-KEY
                 INVALID KEY NEXT SENTENCE.
            IF WS-DRTR-ST1 NOT = 0
               GO TO RDTR-999.
       RDTR-002.
            READ DEBTOR-TRANS-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-DRTR-ST1 = 10
               GO TO RDTR-999.
            IF WS-DRTR-ST1 NOT = 0
               MOVE 
               "DR-TRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DRTR-ST1
               GO TO RDTR-002.
            IF DRTR-ACCOUNT-NUMBER NOT = WS-OLDDEBTORNUMBER
               GO TO RDTR-999.
            ADD 1 TO WS-NUMBER
            MOVE WS-NUMBER TO WS-NUMBER-DIS
            MOVE 2959 TO POS
            DISPLAY WS-NUMBER-DIS AT POS.
       RDTR-005.
            MOVE WS-NEWDEBTORNUMBER TO DRTR-ACCOUNT-NUMBER.
            REWRITE DEBTOR-TRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-DRTR-ST1 NOT = 0
               MOVE DRTR-KEY               TO WS-DAILY-1ST
               MOVE DRTR-ACCOUNT-NUMBER    TO WS-DAILY-2ND
               MOVE "NO CHNG TO DRTRANS  " TO WS-DAILY-3RD
               MOVE WS-NEWDEBTORNUMBER     TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RDTR-002.
       RDTR-999.
           EXIT.
      *
       READ-REGISTER-RECORDS SECTION.
       RRR-000.
            MOVE 0 TO WS-NUMBER
                      WS-REWRITE-ATTEMPT.
            MOVE 2910 TO POS.
            DISPLAY 
            "Changing Register Records To New Account, Record No:      "
               AT POS.
       RRR-001.
            MOVE WS-OLDDEBTORNUMBER TO INCR-ACCOUNT.
            MOVE " "                TO INCR-PORDER.
            START INCR-REGISTER KEY NOT < INCR-ALT-KEY
                 INVALID KEY NEXT SENTENCE.
            IF WS-INCR-LY-ST1 NOT = 0
               GO TO RRR-999.
       RRR-002.
            READ INCR-REGISTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-INCR-LY-ST1 = 10
               GO TO RRR-999.
            IF WS-INCR-LY-ST1 NOT = 0
               MOVE "REGISTER FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR-020
               GO TO RRR-002.
            IF INCR-ACCOUNT NOT = WS-OLDDEBTORNUMBER
               ADD 1 TO WS-REWRITE-ATTEMPT
             IF WS-REWRITE-ATTEMPT < 5
               MOVE 2965 TO POS
               DISPLAY WS-REWRITE-ATTEMPT AT POS
               CLOSE INCR-REGISTER
               PERFORM OPEN-008
               GO TO RRR-001
             ELSE 
               GO TO RRR-999.
            ADD 1          TO WS-NUMBER
            MOVE WS-NUMBER TO WS-NUMBER-DIS
            MOVE 2963 TO POS
            DISPLAY WS-NUMBER-DIS AT POS.
       RRR-010.
            MOVE WS-NEWDEBTORNUMBER TO INCR-ACCOUNT.
            REWRITE INCR-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-INCR-LY-ST1 NOT = 0
               MOVE INCR-KEY               TO WS-DAILY-1ST
               MOVE INCR-ALT-KEY           TO WS-DAILY-2ND
               MOVE "NO CHNGE TO REGISTER" TO WS-DAILY-3RD
               MOVE WS-NEWDEBTORNUMBER     TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RRR-002.
       RRR-999.
           EXIT.
      *
       READ-REGISTERLY-RECORDS SECTION.
       RRLY-000.
            MOVE 0 TO WS-NUMBER.
            MOVE 2910 TO POS.
            DISPLAY 
            "Changing Reg-LY Records To New Account, Record No:        "
               AT POS.
            MOVE WS-OLDDEBTORNUMBER TO INCR-LY-ACCOUNT.
            MOVE " "                TO INCR-LY-PORDER.
            START INCR-LY-REGISTER KEY NOT < INCR-LY-ALT-KEY
                 INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 NOT = 0
               GO TO RRLY-999.
       RRLY-002.
            READ INCR-LY-REGISTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-INCR-LY-ST1 = 10
               GO TO RRLY-999.
            IF WS-INCR-ST1 NOT = 0
               MOVE "REG-LY FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR-020
               MOVE 0 TO WS-INCR-LY-ST1
               GO TO RRLY-002.
            IF INCR-LY-ACCOUNT NOT = WS-OLDDEBTORNUMBER
               GO TO RRLY-999.
            ADD 1          TO WS-NUMBER
            MOVE WS-NUMBER TO WS-NUMBER-DIS
            MOVE 2965 TO POS
            DISPLAY WS-NUMBER-DIS AT POS.
       RRLY-010.
            MOVE WS-NEWDEBTORNUMBER TO INCR-LY-ACCOUNT.
            REWRITE INCR-LY-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-INCR-LY-ST1 NOT = 0
               MOVE INCR-LY-KEY            TO WS-DAILY-1ST
               MOVE INCR-LY-ALT-KEY        TO WS-DAILY-2ND
               MOVE "NO CHNGE TO REG-LY  " TO WS-DAILY-3RD
               MOVE WS-NEWDEBTORNUMBER     TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RRLY-002.
       RRLY-999.
           EXIT.
      *
       DELETE-DEBTOR-RECORD SECTION.
       DSR-000.
            IF NEW-DEBTORNO = "Y"
               GO TO DSR-999.
       DSR-010.
            DELETE DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO DSR-010. 
       DSR-999.
            EXIT. 
      *
       RELEASE-DEBTOR-RECORD SECTION.
       REL-000.
           UNLOCK DEBTOR-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-DEBTOR-RECORD SECTION.
       RSR-010.
          IF NEW-DEBTORNO = "Y"
           IF WS-CREATE-ONE-ACC = "N"
              GO TO RSR-020.
            ADD WS-BALANCE        TO DR-BALANCE
            ADD WS-LAST-BALANCE   TO DR-BAL-LAST-STATE
            ADD WS-CURRENT        TO DR-CURRENT
            ADD WS-30DAY          TO DR-30DAY
            ADD WS-60DAY          TO DR-60DAY
            ADD WS-90DAY          TO DR-90DAY
            ADD WS-120DAY         TO DR-120DAY.
            
            ADD WS-SALES-PTD  TO DR-SALES-PTD
            ADD WS-SALES-YTD  TO DR-SALES-YTD
            ADD WS-SALES-LAST TO DR-SALES-LAST
            ADD WS-COST-PTD   TO DR-COST-PTD
            ADD WS-COST-YTD   TO DR-COST-YTD
            ADD WS-COST-LAST  TO DR-COST-LAST.
       RSR-015.
          REWRITE DEBTOR-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
              MOVE 
           "DR-RECORD DOESN'T EXIST ON REWRITE, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE "Y" TO WS-INVALID
              GO TO RSR-999.
          IF WS-DEBTOR-ST1 NOT = 0
              MOVE "DEBTOR RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RSR-015.
          GO TO RSR-999.
       RSR-020.
          WRITE DEBTOR-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
              MOVE 
           "DR-RECORD ALREADY EXISTS ON WRITE, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE "Y" TO WS-INVALID
              GO TO RSR-999.
          IF WS-DEBTOR-ST1 NOT = 0
              MOVE "DR-RECORD BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RSR-020.
       RSR-999.
          EXIT.
      *
       READ-DEBTOR SECTION.
       R-DR-000.
             MOVE DR-ACCOUNT-NUMBER TO WS-DEBTORNUMBER.
             START DEBTOR-MASTER KEY NOT < DR-KEY
                 INVALID KEY NEXT SENTENCE.
       R-DR-010.
             READ DEBTOR-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 23 OR 35  OR 49
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-DEBTORNO
                MOVE WS-DEBTORNUMBER TO DR-ACCOUNT-NUMBER
                GO TO R-DR-999.
             IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO R-DR-010.
             MOVE "N" TO NEW-DEBTORNO.
       R-DR-999.
             EXIT.
      *
       START-DEBTOR SECTION.
       DR-DR-000.
              MOVE WS-DEBTORNUMBER TO DR-ACCOUNT-NUMBER.
              START DEBTOR-MASTER KEY NOT LESS DR-ACCOUNT-NUMBER.
       DR-DR-999.
             EXIT.
      *
       READ-DEBTOR-NEXT SECTION.
       RSN-005. 
           READ DEBTOR-MASTER NEXT WITH LOCK
             AT END 
               MOVE 0 TO DR-ACCOUNT-NUMBER
                         WS-DEBTORNUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-DEBTOR-ST1 = 23 OR = 35 OR = 49
               MOVE "DEBTOR FILE BUSY-23, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RSN-005.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RSN-005.
           MOVE DR-ACCOUNT-NUMBER TO WS-DEBTORNUMBER.
           MOVE "N" TO NEW-DEBTORNO.
       RSN-999.
             EXIT.
      *
       CLEAR-FORM SECTION.
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
       CLSC-500.
           IF F-EXIT-CH = X"07"
                PERFORM RELEASE-DEBTOR-RECORD.
       CLSC-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
        OPEN-002.
            OPEN I-O DRCONT-MASTER.
            IF WS-DC-ST1 NOT = 0
               MOVE 0 TO WS-DC-ST1
               MOVE "DR-CONTACT FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
       OPEN-003.
           OPEN I-O STOCK-TRANSLY-FILE.
           IF WS-STTRANSLY-ST1 NOT = 0
              MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-STTRANSLY-ST1
              MOVE "STOCK-TRANS-LY FILE BUSY, 'ESC' TO  RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-003.
       OPEN-004.
           OPEN I-O INCR-LY-REGISTER.
           IF WS-INCR-LY-ST1 NOT = 0 
              MOVE "REGISTER-LY FILE BUSY, 'ESC' TO  RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-004.
       OPEN-005.
            OPEN I-O DEBTOR-TRANS-FILE.
            IF WS-DRTR-ST1 NOT = 0
               MOVE 0 TO WS-DRTR-ST1
               MOVE "DR-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-006.
            OPEN I-O STOCK-TRANS-FILE.
            IF WS-STTR-ST1 NOT = 0
               MOVE 0 TO WS-STTR-ST1
               MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-006.
       OPEN-008.
            OPEN I-O INCR-REGISTER.
            IF WS-INCR-LY-ST1 NOT = 0
               MOVE 0 TO WS-INCR-LY-ST1
               MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-008.
       OPEN-009.
            MOVE Ws-Forms-Name   TO F-FILENAME
            MOVE Ws-cbForms-name TO F-CBFILENAME.
            MOVE "DrNoChMt"      TO F-FORMNAME
            MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE DEBTOR-MASTER
                  INCR-REGISTER
                  INCR-LY-REGISTER
                  DEBTOR-TRANS-FILE
                  STOCK-TRANS-FILE
                  STOCK-TRANSLY-FILE.
      *      STOP RUN.
            EXIT PROGRAM.
       END-999.
            EXIT.
       Copy "ReadFieldAlpha".
       Copy "ReadFieldAccount".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldNumeric6".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldCredit".
       Copy "WriteFieldPost".
       Copy "WriteFieldCode".
       Copy "WriteFieldGroup".
       Copy "CTOSCobolAccept".
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
      *
      * END-OF-JOB
