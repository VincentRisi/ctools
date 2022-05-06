       IDENTIFICATION DIVISION.
       PROGRAM-ID. SlCrdRev.
       AUTHOR. CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. B20.
       OBJECT-COMPUTER. B20.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectStMaster".
         Copy "SelectSlMaster".
         Copy "SelectDrTrans".
         Copy "SelectStTrans".
         Copy "SelectSlDistributions".
         Copy "SelectSlRegister".
         Copy "SelectSlSoldBy".
         Copy "SelectSlSpecials".
         Copy "SelectSlDaily".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdStock.
           COPY ChlfdSales.
           COPY ChlfdDrTrans.
           COPY ChlfdStTrans.
           COPY ChlfdDisTot.
           COPY ChlfdRegister.
           COPY ChlfdSoldBy.
           COPY ChlfdSpecialSales.
           COPY ChlfdDaily.
      *
       WORKING-STORAGE SECTION.
       77  WS-STOCKNUMBER       PIC X(15) VALUE " ".
       77  WS-DISCOUNT-CODE     PIC X VALUE " ".
       77  WS-ACCOUNT-NUMBER    PIC 9(7) VALUE 0.
       77  WS-DISCOUNTREG       PIC 9(6)V99 VALUE 0.
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-PORDERNO          PIC X(20) VALUE " ".
       77  WS-SALESANALYSIS     PIC X(14) VALUE " ".
       77  WS-ANAL-CODE         PIC XX VALUE " ".
       77  WS-DELIVERVIA        PIC X(20) VALUE " ".
       77  WS-TERMOFSALE        PIC X(11) VALUE " ".
       77  WS-BINNO             PIC X(6) VALUE " ".
       77  Ws-Sold-By           PIC XX VALUE " ".
       77  WS-GSTNO             PIC X(13) VALUE " ".
       77  WS-STTRANSNO         PIC 9(6) VALUE 0.
       77  WS-DRTRANSNO         PIC 9(6) VALUE 0.
       77  WS-INVOICEDATE       PIC 9(8) VALUE 0.
       77  WS-COMMENTLINE       PIC X(30) VALUE 
           "** THIS COPY IS A REVERSAL ** ".
       77  WS-ADDONFREIGHT      PIC 9(7)V99 VALUE 0.
       77  WS-POSTADDON         PIC 9(7)V99 VALUE 0.
       77  WS-HANDADDON         PIC 9(7)V99 VALUE 0.
       77  WS-MISCADDON         PIC 9(7)V99 VALUE 0.
       77  WS-SUBTOTAL          PIC 9(7)V99 VALUE 0.
       77  WS-ADDONAMT          PIC 9(7)V99 VALUE 0.
       77  WS-TAXAMT            PIC 9(7)V99 VALUE 0.
       77  WS-INVOICETOTAL      PIC 9(7)V99 VALUE 0.
       77  WS-TAXABLETOTAL      PIC 9(7)V99 VALUE 0.
       77  WS-NONTAXABLETOTAL   PIC 9(7)V99 VALUE 0.
       77  WS-WORKTOTAL         PIC 9(7)V99 VALUE 0.
       77  WS-WORKTOTAL2        PIC 9(7)V99 VALUE 0.
       77  WS-COSTTOTAL         PIC 9(7)V99 VALUE 0.
       77  WS-PRICETOTAL        PIC 9(7)V99 VALUE 0.
       77  WS-EXPORTTOTAL       PIC 9(7)V99 VALUE 0.
       77  WS-DISCOUNT          PIC 9(7)V99 VALUE 0.
       77  WS-GST-AMT-TAXED        PIC 9(7)V99 VALUE 0.
       77  WS-GST-AMT-TAXABLE      PIC 9(7)V99 VALUE 0.
       77  WS-GST-AMT-NONTAXABLE   PIC 9(7)V99 VALUE 0.
       77  WS-GST-AMT-EXPORT       PIC 9(7)V99 VALUE 0.
       77  WS-GST-REC              PIC X(56) VALUE " ".
       77  WS-GST-PERCENT          PIC 99V99 VALUE 0.
       77  WS-BODY-LINE         PIC ZZ9.
       77  WS-IMM-PR            PIC X VALUE " ".
       77  WS-PAGE              PIC 99 VALUE 0.
       77  WS-DEBTOR-INQUIRY    PIC X(8) VALUE "DrNameIq".
       77  WS-STOCK-INQUIRY     PIC X(8) VALUE "StMastIq".
       77  ANSWER1              PIC X VALUE " ".
       77  ANSWER2              PIC X VALUE " ".
       77  INVOICE-NO-CNT       PIC 9(8) VALUE 0.
       77  WS-PERCENT           PIC 9(2)V99 VALUE 0.
       77  WS-INVOICEDISCOUNT   PIC 9(2)V99 VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-WRITE             PIC X VALUE " ".
       77  WS-YN                PIC X VALUE SPACES.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1       PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1        PIC 99.
       01  WS-SALES-STATUS.
           03  WS-SALES-ST1        PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1      PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1      PIC 99.
       01  WS-DISTRIBUTION-STATUS.
           03  WS-DISTRIBUTION-ST1 PIC 99.
       01  WS-SOLDBY-STATUS.
           03  WS-SOLDBY-ST1       PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1         PIC 99.
       01  WS-SPECIALS-STATUS.
           03  WS-SPECIALS-ST1     PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       01  SPLIT-DELIVERVIA.
           03  WSDE-CODE        PIC X VALUE " ".
           03  WSDE-REST        PIC X(9) VALUE " ".
       01  WS-NAMEANDADDRESS.
           03  WS-NAME          PIC X(40) VALUE " ".
           03  WS-ADD1          PIC X(25) VALUE " ".
           03  WS-ADD2          PIC X(25) VALUE " ".
           03  WS-ADD3          PIC X(25) VALUE " ".
           03  WS-POSTCODE      PIC 9(4).
           03  WS-DELADD1       PIC X(25) VALUE " ".
           03  WS-DELADD2       PIC X(25) VALUE " ".
           03  WS-DELADD3       PIC X(25) VALUE " ".
       01  WS-DIST-TOTALS.
           03  WS-DIST-INVOICE  PIC 9(7)V99 VALUE 0.
           03  WS-DIST-ADDON    PIC 9(7)V99 VALUE 0.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X VALUE " ".
           03  SP-REST          PIC X(14) VALUE " ".
       01  SPLIT-ANALYSIS.
           03  WSAN-CODE.
               05  WSAN-CODE-1  PIC X VALUE " ".
               05  WSAN-CODE-2  PIC X VALUE " ".
           03  WSAN-REST        PIC X(12) VALUE " ".
       01  ALPHABET-FIELD.
           03  ALPHA-FIELD      PIC X.
           88  ALPHA-VALUE      VALUES ARE "A" THRU "Z".
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 200.
               05  B-STOCKNUMBER.
                   07  B-1ST-CHAR      PIC X.
                   07  B-REST          PIC X(14).
               05  B-STTRANS           PIC 9(6).
               05  B-SHIPQTY           PIC S9(5).
               05  B-STOCKDESCRIPTION  PIC X(20).
               05  B-STOCKDESCRIPTION2 PIC X(20).
               05  B-STOCKPRICE        PIC 9(6)V99.
               05  B-STOCKCOST         PIC 9(6)V99.
               05  B-DISCOUNTPERITEM   PIC 9(2)V99.
               05  B-TAX               PIC X.
               05  B-NETT              PIC 9(5)V99.
       01  COMM-LINES.
           03  C-LINE OCCURS 200.
               05  C-NUM.
                   07  C-1STCHAR      PIC X.
                   07  C-NUMREST      PIC X(14).
               05  C-SHIP             PIC X(5).
               05  C-DESC             PIC X(20).
               05  C-PRICE            PIC X(9).
               05  C-COST             PIC X(9).
               05  C-DISC             PIC X(5).
               05  C-REST             PIC X(5).
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       01  WS-REPORT-DATE-STRIP.
           03  WS-STRIP1          PIC X(4).
           03  WS-STRIP2          PIC X(18).
           03  WS-STRIP3          PIC X(3).
       01  WS-CHECK-DATE.
           03  WS-CHECK-YY           PIC 9999.
           03  WS-CHECK-MM           PIC 99.
           03  WS-CHECK-DD           PIC 99.
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
       CONTROL-010.
           PERFORM CLEAR-FIELDS
           MOVE " " TO WS-MESSAGE
           MOVE 2901 TO POS
           DISPLAY WS-MESSAGE AT POS
           PERFORM DISPLAY-FORM
           PERFORM GET-DATA.

           MOVE 2910 TO POS
           DISPLAY "1.  DEBTOR FILE BEING UPDATED....           " AT POS
           PERFORM UPDATE-DEBTOR
           
           MOVE 2910 TO POS
           DISPLAY "2.  STOCK FILES BEING UPDATED..........     " AT POS
           PERFORM UPDATE-STOCK
           
           MOVE 2910 TO POS
           DISPLAY "3.  DELETE ST-TRANS FILES..............     " AT POS
           PERFORM DELETE-STOCK-TRANSACTIONS
           
           MOVE 2910 TO POS
           DISPLAY "4.  SALES ANALYSIS FILE BEING UPDATED....   " AT POS
           PERFORM UPDATE-SALES
           
           MOVE 2910 TO POS
           DISPLAY "5.  DISTRIBUTION FILE BEING UPDATED......   " AT POS
           PERFORM UPDATE-DISTRIBUTION
           
           MOVE 2910 TO POS
           DISPLAY "6.  DEBTOR-TRANS BEING DELETED..........    " AT POS
           PERFORM DELETE-DEBTOR-TRANSACTION
           
           MOVE 2910 TO POS
           DISPLAY "7.  C/NOTE REGISTER BEING UPDATED.......    " AT POS
           PERFORM DELETE-INVOICE-REGISTER

           MOVE 2910 TO POS
           DISPLAY "8.  SOLDBY FILE BEING DELETED..........     " AT POS
           PERFORM DELETE-SOLDBY-FILE
           
           MOVE 2910 TO POS
           DISPLAY " 9.  DAILY EXCEPTION LOG BEING WRITTEN..... " AT POS
           
           MOVE "C/NOTE  REVERSED No:" TO WS-DAILY-1ST
           MOVE WS-INVOICE             TO WS-DAILY-2ND
           MOVE "DATE OF REVERSAL IS:" TO WS-DAILY-3RD
           PERFORM OPEN-000
           MOVE DISPLAY-DATE           TO WS-DAILY-4TH
           PERFORM WRITE-DAILY
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           MOVE "ABOVE REVERSED BY  :" TO WS-DAILY-1ST
           MOVE WS-pbValue             TO WS-DAILY-2ND
           MOVE "DATE & TIME CHANGED:" TO WS-DAILY-3RD
           MOVE pbRet                  TO WS-REPORT-DATE-STRIP
           MOVE WS-STRIP2              TO WS-DAILY-4TH
           PERFORM WRITE-DAILY.
           
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           
           GO TO CONTROL-010.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
            PERFORM CLEAR-FIELDS
            PERFORM DISPLAY-FORM
            PERFORM OPEN-000.
       GET-010.
            MOVE "                   " TO F-NAMEFIELD
            MOVE "CREDITNUM"           TO F-FIELDNAME
            MOVE 9                     TO F-CBFIELDNAME
            MOVE 6                     TO F-CBFIELDLENGTH
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            PERFORM READ-FIELD-ALPHA
            IF F-NAMEFIELD = "STOCK"
                CLOSE STOCK-MASTER
                CALL WS-STOCK-INQUIRY USING WS-LINKAGE
                CANCEL WS-STOCK-INQUIRY
                PERFORM CLEAR-SCREEN
                PERFORM OPEN-012
                PERFORM DISPLAY-FORM
                GO TO GET-010.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-INVOICE.
            IF WS-INVOICE = 0
                CLOSE DEBTOR-MASTER
                CALL WS-DEBTOR-INQUIRY USING WS-LINKAGE
                CANCEL WS-DEBTOR-INQUIRY
                PERFORM CLEAR-SCREEN
                PERFORM OPEN-011
                PERFORM DISPLAY-FORM
                GO TO GET-010.
            PERFORM READ-INVOICE-REGISTER.
            IF INCR-INVOICE = 0
               MOVE "WHY NOT ENTER A CREDIT NOTE NUMBER I CAN FIND?"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-010.
            MOVE INCR-DATE TO WS-CHECK-DATE.
            IF WS-CHECK-MM NOT = SPLIT-MM
              OR WS-CHECK-YY NOT = SPLIT-YY
              MOVE "THIS C/NOTE IS NOT FROM THIS CURRENT PERIOD."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO GET-000.
            PERFORM READ-DEBTOR-TRANSACTION.
            IF DRTR-AMT-OUTSTANDING NOT = DRTR-AMT-OF-INVOICE
               MOVE "THIS CREDIT NOTE HAS BEEN APPLIED TO A DEBIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-010.

            PERFORM READ-STOCK-TRANSACTIONS.

            MOVE "ACCOUNTNO"       TO F-FIELDNAME
            MOVE 9                 TO F-CBFIELDNAME
            MOVE WS-ACCOUNT-NUMBER TO F-NAMEFIELD
            MOVE 7                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DEBTORNAME" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE WS-NAME      TO F-NAMEFIELD
            MOVE 40           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "POSTADD1" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE WS-ADD1    TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "POSTADD2" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE WS-ADD2    TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "POSTADD3" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE WS-ADD3    TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "POSTCODE"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-POSTCODE TO F-NAMEFIELD
            MOVE 4           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DELADD1"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-DELADD1 TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DELADD2"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-DELADD2 TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DELADD3"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-DELADD3 TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "POORDERNO" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE WS-PORDERNO TO F-NAMEFIELD
            MOVE 20          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE WSAN-CODE        TO SA-KEY
            PERFORM READ-SALES-ANALYSIS
            MOVE "SALESANALYSIS"  TO F-FIELDNAME
            MOVE 13               TO F-CBFIELDNAME
            MOVE WS-SALESANALYSIS TO F-NAMEFIELD
            MOVE 14               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DELIVERVIA"  TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE WS-DELIVERVIA TO F-NAMEFIELD
            MOVE 20            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "TERMOFSALE"  TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE WS-TERMOFSALE TO F-NAMEFIELD
            MOVE 11            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "GSTNO"  TO F-FIELDNAME
            MOVE 5        TO F-CBFIELDNAME
            MOVE WS-GSTNO TO F-NAMEFIELD
            MOVE 13       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "CREDITDATE" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE WS-DATE      TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE TO F-NAMEFIELD
            MOVE 10           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "SOLDBY"   TO F-FIELDNAME
            MOVE 6          TO F-CBFIELDNAME
            MOVE Ws-Sold-By TO F-NAMEFIELD
            MOVE 2          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE 1 TO SUB-1 F-INDEX
            PERFORM SCROLL-NEXT.
            PERFORM SCROLL-PREVIOUS.

            MOVE "COMMENTLINE"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE WS-COMMENTLINE TO F-NAMEFIELD
            MOVE 30             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ADDONFREIGHT"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE WS-ADDONFREIGHT TO F-EDNAMEFIELDAMOUNT
            MOVE 9               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT

            MOVE "POSTADDON"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE WS-POSTADDON TO F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT

            MOVE "HANDADDON"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE WS-HANDADDON TO F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT
                 
            MOVE "MISC.ADDON" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE WS-MISCADDON TO F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT

            MOVE "SUBTOTAL"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-SUBTOTAL TO F-EDNAMEFIELDAMOUNT
            MOVE 9           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT

            MOVE "ADDONAMT"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-ADDONAMT TO F-EDNAMEFIELDAMOUNT
            MOVE 9           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT

            MOVE "TAXAMT"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            MOVE WS-TAXAMT TO F-EDNAMEFIELDAMOUNT
            MOVE 9         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT

            MOVE "CREDITTOTAL"   TO F-FIELDNAME
            MOVE 11              TO F-CBFIELDNAME
            MOVE WS-INVOICETOTAL TO F-EDNAMEFIELDAMOUNT
            MOVE 9               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
       GET-800.
            MOVE 2910 TO POS.
            DISPLAY "Press 'ESC' & 'RETURN' To Reverse C/Note, OR "
                   AT POS.
            MOVE 3010 TO POS.
            DISPLAY "Press 'ESC' & 'ESC' To Stop The Reversal. "
                   AT POS.
            CALL "LOCKKBD" USING F-FIELDNAME.
            MOVE "CREDITTOTAL" TO F-FIELDNAME.
       GET-825.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-FIELDS
                PERFORM DISPLAY-FORM
                GO TO GET-010.
            IF F-EXIT-CH NOT = X"0A"
                GO TO GET-825.

            PERFORM ERROR1-020
            PERFORM ERROR-020.
       GET-900.
            ADD WS-INVOICETOTAL TO WS-DIST-INVOICE.
            ADD WS-ADDONAMT     TO WS-DIST-ADDON.
            ADD WS-TAXAMT       TO WS-GST-AMT-TAXED.
            COMPUTE WS-WORKTOTAL =
              WS-INVOICETOTAL - (WS-ADDONAMT + WS-TAXAMT).
       GET-910.
            IF WS-ADDONAMT NOT = 0
               ADD 1 TO SUB-10.
       GET-999.
            EXIT.
      *
       READ-STOCK-LOCK SECTION.
       RSL-000.
             MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
             READ STOCK-MASTER WITH LOCK
                INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 0
                 GO TO RSL-999.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                 MOVE "STOCK RECORD BUSY ON READ-23, 'ESC' TO EXIT."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-STOCK-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-STOCK-ST1
                 GO TO RSL-999.
             IF WS-STOCK-ST1 NOT = 0
                 MOVE "STOCK RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-STOCK-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-STOCK-ST1
                 GO TO RSL-000.
             MOVE " " TO ST-STOCKNUMBER
                         ST-DESCRIPTION1
                         ST-DESCRIPTION2
             MOVE 0 TO ST-PRICE
                       ST-AVERAGECOST
                       ST-DISCOUNT1.
             PERFORM ERROR-020.
       RSL-999.
             EXIT.
      *
       READ-STOCK-TRANSACTIONS SECTION.
       RSTT-000.
           MOVE 1            TO SUB-1.
           MOVE 0            TO WS-COSTTOTAL.
           MOVE WS-INVOICE   TO STTR-REFERENCE1.
           MOVE 6            TO STTR-TYPE.
           MOVE 0            TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 23 OR 35 OR 49
              MOVE 0 TO STTR-TYPE
              GO TO RSTT-999.
       RSTT-010.
           READ STOCK-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
              MOVE 0 TO STTR-TYPE
              MOVE SUB-1 TO SUB-25
              GO TO RSTT-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "STOCK TRANS-RECORD BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RSTT-010.
           IF STTR-REFERENCE1 NOT = WS-INVOICE
               MOVE SUB-1 TO SUB-25
               GO TO RSTT-999.
           IF STTR-TYPE NOT = 6
               MOVE SUB-1 TO SUB-25
               GO TO RSTT-999.
           MOVE STTR-TRANSACTION-NUMBER TO B-STTRANS (SUB-1). 
           MOVE STTR-STOCK-NUMBER       TO B-STOCKNUMBER (SUB-1)
                                       SPLIT-STOCK.
           IF SP-1STCHAR = "*"
                GO TO RSTT-020.
           MOVE STTR-DESC1          TO B-STOCKDESCRIPTION (SUB-1)
           MOVE STTR-DESC2          TO B-STOCKDESCRIPTION2 (SUB-1)
           MOVE STTR-SHIPQTY        TO B-SHIPQTY (SUB-1)
           MOVE STTR-PRICE          TO B-STOCKPRICE (SUB-1)
           MOVE STTR-COST-VALUE     TO B-STOCKCOST (SUB-1)
           MOVE STTR-ITEMDISC       TO B-DISCOUNTPERITEM (SUB-1)
           MOVE STTR-SALES-VALUE    TO B-NETT (SUB-1)
           COMPUTE WS-COSTTOTAL = WS-COSTTOTAL +
               (B-STOCKCOST (SUB-1) * B-SHIPQTY (SUB-1)).
           GO TO RSTT-050.
       RSTT-020.
           MOVE STTR-STOCK-NUMBER TO C-NUM (SUB-1)
           MOVE COM-SHIPQTY       TO C-SHIP (SUB-1)
           MOVE COM-DESC          TO C-DESC (SUB-1)
           MOVE COM-PRICE         TO C-PRICE (SUB-1)
           MOVE COM-COST          TO C-COST (SUB-1)
           MOVE COM-DISC          TO C-DISC (SUB-1)
           MOVE " "               TO C-REST (SUB-1).
       RSTT-050.
      *     MOVE B-STOCKNUMBER (SUB-1) TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       
           ADD 1 TO SUB-1.
           IF SUB-1 > 200
              MOVE SUB-1 TO SUB-25
              GO TO RSTT-999.  
           GO TO RSTT-010.
       RSTT-999.
           EXIT.
      *
       DELETE-STOCK-TRANSACTIONS SECTION.
       DEL-ST-000.
           MOVE 1            TO SUB-1.
           MOVE WS-INVOICE   TO STTR-REFERENCE1.
           MOVE 6            TO STTR-TYPE.
           MOVE 0            TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
       DEL-ST-010.
           READ STOCK-TRANS-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
              MOVE 0 TO STTR-TYPE
              GO TO DEL-ST-999.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS BUSY ON READ-NEXT-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO DEL-ST-010.
           IF STTR-REFERENCE1 NOT = WS-INVOICE
              UNLOCK STOCK-TRANS-FILE
              GO TO DEL-ST-999.
           IF STTR-TYPE NOT = 6
              GO TO DEL-ST-010.
       DEL-ST-030.
           DELETE STOCK-TRANS-FILE
                INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
                MOVE "DELETE INV ST-TRANS BUSY, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STTRANS-ST1
                GO TO DEL-ST-030.
       DEL-ST-050.
           ADD 1 TO SUB-1.
           IF SUB-1 > 200
              GO TO DEL-ST-999.
           GO TO DEL-ST-010.
       DEL-ST-999.
           EXIT.
      *
       READ-INVOICE-REGISTER SECTION.
       RIR-000.
           MOVE WS-INVOICE TO INCR-INVOICE.
           MOVE 6          TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY.
       RIR-005.
           READ INCR-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
              MOVE 0 TO INCR-INVOICE
              GO TO RIR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "INV/CR. REGISTER BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RIR-005.
           IF INCR-PRINTED = "R"
              MOVE 0 TO INCR-INVOICE
              GO TO RIR-999.
       RIR-010.
            MOVE INCR-ACCOUNT        TO WS-ACCOUNT-NUMBER
            MOVE INCR-GSTNO          TO WS-GSTNO
            MOVE INCR-DATE           TO WS-DATE
            MOVE INCR-SALES          TO WSAN-CODE
            MOVE INCR-INVCRED-AMT    TO WS-INVOICETOTAL
            MOVE INCR-TAX            TO WS-TAXAMT
            MOVE INCR-ADDONS         TO WS-ADDONAMT
            MOVE INCR-ADDPOST        TO WS-POSTADDON
            MOVE INCR-ADDFREIGHT     TO WS-ADDONFREIGHT
            MOVE INCR-ADDLABOUR      TO WS-HANDADDON
            MOVE INCR-ADDMISC        TO WS-MISCADDON
            MOVE INCR-SB-TYPE        TO Ws-Sold-By
            MOVE INCR-DRTRANS-NO     TO WS-DRTRANSNO
      *      MOVE INCR-STTRANS-NO     TO WS-STTRANSNO.
            COMPUTE WS-SUBTOTAL =
                WS-INVOICETOTAL - (WS-TAXAMT + WS-ADDONAMT).
            MOVE INCR-NAME         TO WS-NAME
            MOVE INCR-ADD1         TO WS-ADD1
            MOVE INCR-ADD2         TO WS-ADD2
            MOVE INCR-ADD3         TO WS-ADD3
            MOVE INCR-CODE         TO WS-POSTCODE
            MOVE INCR-DEL1         TO WS-DELADD1
            MOVE INCR-DEL2         TO WS-DELADD2
            MOVE INCR-DEL3         TO WS-DELADD3
            MOVE INCR-TERMS        TO WS-TERMOFSALE
            MOVE INCR-PORDER       TO WS-PORDERNO
            MOVE INCR-DELIVERY     TO WS-DELIVERVIA
            MOVE INCR-BIN          TO WS-BINNO
            MOVE INCR-LINENO       TO SUB-20.
       RIR-999.
           EXIT.
      *
       DELETE-INVOICE-REGISTER SECTION.
       UIR-000.
           MOVE WS-INVOICE TO INCR-INVOICE.
           MOVE 6          TO INCR-TRANS.
       UIR-005.
           READ INCR-REGISTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
              MOVE 0 TO INCR-INVOICE
              GO TO UIR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "INV/CR. REGISTER BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO UIR-005.
       UIR-010.
           DELETE INCR-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               MOVE "CREDIT REGISTER NOT DELETED 23, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO UIR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "CREDIT REGISTER NOT DELETED NOT 0, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO UIR-010.
       UIR-999.
           EXIT.
      *
       READ-SALES-ANALYSIS SECTION.
       RSALES-010.
            MOVE WSAN-CODE TO SA-KEY.
       RSALES-500.
            READ SALES-ANALYSIS
                INVALID KEY NEXT SENTENCE.
            IF WS-SALES-ST1 = 23 OR 35 OR 49
                MOVE "UNKNOWN" TO SA-NAME
                GO TO RSALES-900.
            IF WS-SALES-ST1 NOT = 0
               MOVE "SALES RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO RSALES-500.
       RSALES-900.
            PERFORM ERROR-020.
            MOVE SA-NAME TO WS-SALESANALYSIS.
       RSALES-999.
            EXIT.
      *
       DELETE-SOLDBY-FILE SECTION.
       DSF-005.
           MOVE WS-SOLD-BY TO SB-TYPE.
           MOVE WS-INVOICE TO SB-INVOICE-NUMBER.
           START SOLD-BY KEY NOT < SB-KEY
               INVALID KEY NEXT SENTENCE.
       DSF-010.
           READ SOLD-BY
              INVALID KEY NEXT SENTENCE.
           IF WS-SOLDBY-ST1 = 23 OR 35 OR 49
              MOVE 0 TO SB-INVOICE-NUMBER
              GO TO DSF-999.
           IF WS-SOLDBY-ST1 NOT = 0
               MOVE "SOLDBY BUSY ON READ-DEL, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SOLDBY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SOLDBY-ST1
               GO TO DSF-010.
           DELETE SOLD-BY
              INVALID KEY NEXT SENTENCE.
       DSF-999.
           EXIT.
      *
       UPDATE-DEBTOR SECTION.
       UPDR-000.
           MOVE WS-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
           READ DEBTOR-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "THAT A/C NO DOES NOT EXIST, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPDR-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO UPDR-000.
            ADD WS-INVOICETOTAL TO DR-BALANCE
                                   DR-CURRENT.
            ADD WS-COSTTOTAL TO DR-COST-PTD
                                DR-COST-YTD.
            MOVE WS-WORKTOTAL TO WS-PRICETOTAL.
            ADD WS-PRICETOTAL TO DR-SALES-PTD
                                 DR-SALES-YTD.
       UPDR-900.
            REWRITE DEBTOR-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                GO TO UPDR-999.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO UPDR-900.
       UPDR-999.
           EXIT.
      *
       READ-DEBTOR-TRANSACTION SECTION.
       RDT-000.
           MOVE 6            TO DRTR-TYPE.
           MOVE WS-DRTRANSNO TO DRTR-TRANSACTION-NUMBER.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY
               INVALID KEY NEXT SENTENCE.
       RDT-010.
           READ DEBTOR-TRANS-FILE
              INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
              MOVE "NO SUCH DR TRANS No, RDT-010, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDT-999.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DR TRANS FILE BUSY ON READ, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO RDT-010.
       RDT-999.
           EXIT.
      *
       DELETE-DEBTOR-TRANSACTION SECTION.
       UDT-000.
           MOVE 6            TO DRTR-TYPE.
           MOVE WS-DRTRANSNO TO DRTR-TRANSACTION-NUMBER.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY
               INVALID KEY NEXT SENTENCE.
       UDT-010.
           READ DEBTOR-TRANS-FILE WITH LOCK
              INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
              MOVE "NO SUCH DR TRANS No TO DELETE" TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO UDT-999.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DR-TRANS FILE BUSY ON READ, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO UDT-010.
           IF DRTR-REFERENCE2 = WS-INVOICE
              GO TO UDT-020
           ELSE
              MOVE "WE HAVE THE WRONG CR/NOTE No. 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO UDT-999.
       UDT-020.
           DELETE DEBTOR-TRANS-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49 OR = 91
               MOVE "DEBTOR TRANS NOT DELETED 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO UDT-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DR-TRANS BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO UDT-020.
       UDT-999.
           EXIT.
      *
       DELETE-SPECIALS-FILE SECTION.
       D-SPEC-005.
           MOVE WS-SOLD-BY            TO SP-INITIALS
           MOVE WS-INVOICE            TO SP-INVOICE-NUMBER
           MOVE B-STOCKNUMBER (SUB-1) TO SP-STOCK.
           START SPECIALS-FILE KEY NOT < SP-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-SPECIALS-ST1 NOT = 0
              GO TO D-SPEC-999.
       D-SPEC-010.
           READ SPECIALS-FILE WITH LOCK
              INVALID KEY NEXT SENTENCE.
           IF WS-SPECIALS-ST1 = 23 OR 35 OR 49
              GO TO D-SPEC-999.
           IF WS-SPECIALS-ST1 NOT = 0
              MOVE "ERROR IN SPECIALS D-SPEC, 'ESC' TO SEE CODES."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SPECIALS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE "INFORM YOUR SUPERVISOR THAT NO SPECIAL IS WRITTEN."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO D-SPEC-999.
       D-SPEC-020.
           DELETE SPECIALS-FILE
              INVALID KEY NEXT SENTENCE.
           IF WS-SPECIALS-ST1 NOT = 0
              MOVE "ERROR IN SPECIALS DELETE, 'ESC' TO SEE CODES"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SPECIALS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE "INFORM YOUR SUPERVISOR THAT NO SPECIAL IS DELETED."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
       D-SPEC-999.
           EXIT.
      *
       UPDATE-DISTRIBUTION SECTION.
       UPDIS-005.
           MOVE "1" TO DIST-KEY.
       UPDIS-010.
           READ DISTRIBUTIONS WITH LOCK
              INVALID KEY NEXT SENTENCE.
           IF WS-DISTRIBUTION-ST1 = 23 OR 35 OR 49
              MOVE "DISTRIBUTION NOT FOUND ON READ, 'ESC' TO RE-TRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO UPDIS-900.
           IF WS-DISTRIBUTION-ST1 NOT = 0 
              MOVE "DISTRIBUTION BUSY ON READ, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DISTRIBUTION-ST1
              GO TO UPDIS-010.
           SUBTRACT WS-SUBTOTAL FROM DIST-CNOTEWEEK
                                     DIST-CNOTEPTD
                                     DIST-CNOTEYTD.
           ADD WS-DIST-INVOICE TO DIST-ACCRECWEEK
                                  DIST-ACCRECPTD
                                  DIST-ACCRECYTD.
           ADD WS-DIST-ADDON TO DIST-ADDONWEEK
                                DIST-ADDONPTD
                                DIST-ADDONYTD.
            IF WS-GSTNO NOT = "EXPORT" AND NOT = "EXPORT       "
                ADD WS-WORKTOTAL TO WS-GST-AMT-TAXABLE
            ELSE
                ADD WS-WORKTOTAL TO WS-GST-AMT-EXPORT.
       UPDIS-400.
           ADD WS-GST-AMT-TAXED TO GST-AMT-TAXED-WEEK
                                   GST-AMT-TAXED-PTD
                                   GST-AMT-TAXED-YTD.
           ADD WS-GST-AMT-TAXABLE TO GST-AMT-TAXABLE-WEEK
                                     GST-AMT-TAXABLE-PTD
                                     GST-AMT-TAXABLE-YTD.
           ADD WS-GST-AMT-NONTAXABLE TO GST-AMT-NONTAXABLE-WEEK
                                        GST-AMT-NONTAXABLE-PTD
                                        GST-AMT-NONTAXABLE-YTD.
           ADD WS-GST-AMT-EXPORT TO GST-AMT-EXPORT-WEEK
                                    GST-AMT-EXPORT-PTD
                                    GST-AMT-EXPORT-YTD.
       UPDIS-500.
           REWRITE DIST-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-DISTRIBUTION-ST1 = 23 OR 35 OR 49
              MOVE "DISTRIBUTION RECORD NOT UPDATED, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO UPDIS-900.
           IF WS-DISTRIBUTION-ST1 NOT = 0 
              MOVE "DISTRIBUTION BUSY ON REWRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DISTRIBUTION-ST1
              PERFORM ERROR-MESSAGE
              GO TO UPDIS-010.
       UPDIS-900.
           MOVE 0 TO WS-DIST-INVOICE
                     WS-DIST-ADDON
                     WS-GST-AMT-TAXED
                     WS-GST-AMT-TAXABLE
                     WS-GST-AMT-NONTAXABLE
                     WS-GST-AMT-EXPORT.
       UPDIS-999.
           EXIT.
      *
       UPDATE-STOCK SECTION.
       UPST-000.
           MOVE 1 TO SUB-1.
       UPST-010.
           IF B-STOCKNUMBER (SUB-1) = " "
               GO TO UPST-999.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*" OR = "/"
                GO TO UPST-500.
           MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER
                                         ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
                INVALID KEY NEXT SENTENCE.
       UPST-011.
           READ STOCK-MASTER WITH LOCK
              INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO UPST-011.
      *
      * ACTUAL UPDATE OF STOCK FILE.
      *
           SUBTRACT B-SHIPQTY (SUB-1) FROM ST-QTYONHAND.
           ADD B-SHIPQTY (SUB-1) TO ST-SALESUNITMTD
                                    ST-SALESUNITSYTD.
           ADD B-NETT (SUB-1)    TO ST-SALESRANDSMTD
                                    ST-SALESRANDSYTD.
           COMPUTE ST-SALESCOSTMTD = (ST-SALESCOSTMTD + 
               (B-SHIPQTY (SUB-1) * B-STOCKCOST (SUB-1))).
           COMPUTE ST-SALESCOSTYTD = (ST-SALESCOSTYTD + 
               (B-SHIPQTY (SUB-1) * B-STOCKCOST (SUB-1))).
       UPST-012.
           REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               GO TO UPST-020.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON REWRITE, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO UPST-012.
       UPST-020.
           IF ST-ANALYSIS = "S"
            IF B-STOCKPRICE (SUB-1) < ST-PRICE
             IF ST-QTYONHAND = 0
              IF ST-QTYONRESERVE = 0
               PERFORM DELETE-SPECIALS-FILE.
       UPST-500.
           ADD 1 TO SUB-1.
           IF SUB-1 < 101
               GO TO UPST-010.
       UPST-999.
           EXIT.
      *
       UPDATE-SALES SECTION.
       UPSA-010.
            MOVE WSAN-CODE TO SA-KEY.
       UPSA-500.
            READ SALES-ANALYSIS WITH LOCK
                INVALID KEY NEXT SENTENCE.
            IF WS-SALES-ST1 = 23 OR 35 OR 49
                GO TO UPSA-999.
            IF WS-SALES-ST1 NOT = 0
               MOVE "SALES RECORD BUSY ON READ, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO UPSA-500.
           ADD WS-COSTTOTAL  TO SA-COST-WEEK
                                SA-COST-PTD
                                SA-COST-YTD.
           ADD WS-PRICETOTAL TO SA-SALES-WEEK
                                SA-SALES-PTD
                                SA-SALES-YTD.
       UPSA-600.
           REWRITE SALES-ANALYSIS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-SALES-ST1 = 23 OR 35 OR 49
              GO TO UPSA-999.
           IF WS-SALES-ST1 NOT = 0
               MOVE "SALES RECORD BUSY ON REWRITE, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO UPSA-600.
       UPSA-999.
           EXIT.
      *
       CLEAR-BODY SECTION.
       CLEAR-000.
            MOVE 0 TO F-INDEX.
       CLEAR-002.
            ADD 1 TO F-INDEX.
            IF F-INDEX > 7
             GO TO CLEAR-999.
            MOVE "STOCKNUMBER" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE " "           TO F-NAMEFIELD
            MOVE 15            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "SHIPQTY"     TO F-FIELDNAME
            MOVE 7             TO F-CBFIELDNAME
            MOVE 0             TO F-EDNAMEFIELDNUM
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC

            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME
            MOVE 16                 TO F-CBFIELDNAME
            MOVE " "                TO F-NAMEFIELD
            MOVE 20                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "STOCKPRICE"       TO F-FIELDNAME
            MOVE 10                 TO F-CBFIELDNAME
            MOVE " "                TO F-NAMEFIELD
            MOVE 8                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "STOCKCOST" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 8           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME
            MOVE 15                TO F-CBFIELDNAME
            MOVE " "               TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "TAX"             TO F-FIELDNAME
            MOVE 3                 TO F-CBFIELDNAME
            MOVE " "               TO F-NAMEFIELD
            MOVE 1                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            GO TO CLEAR-002.
       CLEAR-999.
            EXIT.
      *
       SCROLL-NEXT SECTION.
       NEXT-000.
            ADD 1 TO SUB-1.
            IF SUB-1 > SUB-25
               MOVE SUB-25 TO SUB-1.
            IF SUB-1 < 1
               MOVE 1 TO SUB-1 SUB-25.
            MOVE 1 TO F-INDEX. 
            PERFORM CLEAR-BODY.
            MOVE 1 TO F-INDEX. 
            IF SUB-1 > 194
               MOVE 194 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 200  
                GO TO NEXT-030.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO B-TAX (SUB-1)
                MOVE 0   TO B-DISCOUNTPERITEM (SUB-1).
            IF F-INDEX < 8
                GO TO NEXT-010.
       NEXT-030.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 > 193
              IF SUB-25 > 193
               COMPUTE F-INDEX = 7 - (200 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX. 
            IF F-INDEX > 7
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3010 TO POS.
            DISPLAY "    BODY LINE: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-999.
             EXIT.
      *
       SCROLL-NEXT-PAGE SECTION.
       NEXT-PAGE-000.
            ADD 7 TO SUB-1.
            IF SUB-1 > SUB-25
               MOVE SUB-25 TO SUB-1.
            MOVE 1 TO F-INDEX. 
            PERFORM CLEAR-BODY.
            MOVE 1 TO F-INDEX. 
            IF SUB-1 > 194
               MOVE 194 TO SUB-1.
       NEXT-PAGE-010.
            PERFORM SCROLLING.
       NEXT-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 200  
                GO TO NEXT-PAGE-030.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO B-TAX (SUB-1)
                MOVE 0 TO B-DISCOUNTPERITEM (SUB-1).
            IF F-INDEX < 8
                GO TO NEXT-PAGE-010.
       NEXT-PAGE-030.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 > 193
              IF SUB-25 > 193
               COMPUTE F-INDEX = 7 - (200 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX.
            IF SUB-1 > 200
               MOVE 194 TO SUB-1.
            IF F-INDEX > 7
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3010 TO POS.
            DISPLAY "    BODY LINE: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-PAGE-999.
             EXIT.
      *
       SCROLL-PREVIOUS SECTION.
       PREV-000.
            SUBTRACT 7 FROM SUB-1.
            MOVE 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-010.
      *      MOVE "SCROLLING IN PREVIOUS" TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
      *      MOVE SUB-1 TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
            PERFORM SCROLLING.
       PREV-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 200   
                GO TO PREV-030.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO B-TAX (SUB-1)
                MOVE   0 TO B-DISCOUNTPERITEM (SUB-1).
            IF F-INDEX < 8
                GO TO PREV-010.
       PREV-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 7 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

             MOVE 3010 TO POS.
             DISPLAY "    BODY LINE: " AT POS.
             ADD 16 TO POS.
             MOVE SUB-1 TO WS-BODY-LINE.
             DISPLAY WS-BODY-LINE AT POS.
       PREV-999.
             EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
            IF SP-1STCHAR = "*"
                 PERFORM SCROLL-COMMENT
                 GO TO SCROLL-999.
       SCROLL-010.
            MOVE "STOCKNUMBER"         TO F-FIELDNAME
            MOVE 11                    TO F-CBFIELDNAME
            MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
            MOVE 15                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "SHIPQTY"     TO F-FIELDNAME
            MOVE 7             TO F-CBFIELDNAME
            COMPUTE WS-WORK-FIELD = B-SHIPQTY (SUB-1) * 10
            MOVE WS-WORK-FIELD TO F-EDNAMEFIELDNUM
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC

            MOVE "STOCKDESCRIPTION"         TO F-FIELDNAME
            MOVE 16                         TO F-CBFIELDNAME
            MOVE B-STOCKDESCRIPTION (SUB-1) TO F-NAMEFIELD
            MOVE 20                         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "STOCKPRICE" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE 9            TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
               MOVE B-STOCKPRICE (SUB-1) TO F-EDNAMEFIELDAMOUNT
               PERFORM WRITE-FIELD-AMOUNT
            ELSE
               MOVE " " TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKCOST" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE 9           TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
               MOVE B-STOCKCOST (SUB-1) TO F-EDNAMEFIELDAMOUNT
               PERFORM WRITE-FIELD-AMOUNT
            ELSE
               MOVE " " TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA.

            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME
            MOVE 15                TO F-CBFIELDNAME
            MOVE 5                 TO F-CBFIELDLENGTH.
            IF B-STOCKNUMBER (SUB-1) NOT = " "
            MOVE B-DISCOUNTPERITEM (SUB-1) TO F-EDNAMEFIELDAMOUNTDIS
               PERFORM WRITE-FIELD-AMOUNTDIS
            ELSE
               MOVE " " TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA.
   
            MOVE "TAX"         TO F-FIELDNAME
            MOVE 3             TO F-CBFIELDNAME
            MOVE B-TAX (SUB-1) TO F-NAMEFIELD
            MOVE 1             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-999.
             EXIT.
      *
       SCROLL-COMMENT SECTION.
       SCC-010.
            MOVE "STOCKNUMBER"         TO F-FIELDNAME
            MOVE 11                    TO F-CBFIELDNAME
            MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
            MOVE 15                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "SHIPQTY"      TO F-FIELDNAME
            MOVE 7              TO F-CBFIELDNAME
            MOVE C-SHIP (SUB-1) TO F-NAMEFIELD
            MOVE 5              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME
            MOVE 16                 TO F-CBFIELDNAME
            MOVE C-DESC (SUB-1)     TO F-NAMEFIELD
            MOVE 20                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "STOCKPRICE"    TO F-FIELDNAME
            MOVE 10              TO F-CBFIELDNAME
            MOVE C-PRICE (SUB-1) TO F-NAMEFIELD
            MOVE 9               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "STOCKCOST"    TO F-FIELDNAME
            MOVE 9              TO F-CBFIELDNAME
            MOVE C-COST (SUB-1) TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME
            MOVE 15                TO F-CBFIELDNAME
            MOVE C-DISC (SUB-1)    TO F-NAMEFIELD
            MOVE 5                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       SCC-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
             MOVE 1 TO SUB-1.
       CF-010.
             MOVE " " TO C-LINE (SUB-1).
             MOVE " " TO B-STOCKNUMBER (SUB-1)
                         B-STOCKDESCRIPTION (SUB-1)
                         B-STOCKDESCRIPTION2 (SUB-1)
                         B-TAX (SUB-1).
             MOVE 0   TO B-SHIPQTY (SUB-1)
                         B-STOCKPRICE (SUB-1)
                         B-STOCKCOST (SUB-1)
                         B-DISCOUNTPERITEM (SUB-1)
                         B-NETT (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 101
                 GO TO CF-010.
       CF-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE.
      *     ACCEPT SPLIT-DATE FROM DATE.
           PERFORM CONVERT-DATE-FORMAT.
       OPEN-010.
           OPEN I-O SOLD-BY.
           IF WS-SOLDBY-ST1 NOT = 0
              MOVE "SOLDBY-FILE BUSY ON OPEN. 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SOLDBY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SOLDBY-ST1
              GO TO OPEN-010.
       OPEN-011.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0 
              MOVE "DEBTOR FILE BUSY ON OPEN. 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DEBTOR-ST1
              GO TO OPEN-011.
       OPEN-012.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0 
              MOVE "STOCK FILE BUSY ON OPEN. 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STOCK-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STOCK-ST1
              GO TO OPEN-012.
       OPEN-013.
            OPEN I-O SALES-ANALYSIS.
            IF WS-SALES-ST1 NOT = 0
              MOVE "SALES FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SALES-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SALES-ST1
              GO TO OPEN-013.
       OPEN-014.
           OPEN I-O DISTRIBUTIONS.
           IF WS-DISTRIBUTION-ST1 NOT = 0
              MOVE "DISTRIBUTION FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DISTRIBUTION-ST1
              GO TO OPEN-014.
       OPEN-015.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0 
              MOVE "STOCK TRANS. FILE BUSY ON OPEN. 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STTRANS-ST1
              GO TO OPEN-015.
       OPEN-016.
           OPEN I-O DEBTOR-TRANS-FILE.
           IF WS-DRTRANS-ST1 NOT = 0 
              MOVE "DEBTOR TRANS. FILE BUSY ON OPEN. 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DRTRANS-ST1
              GO TO OPEN-016.
       OPEN-017.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0 
              MOVE "INV/CR. REGISTER BUSY ON OPEN. 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-INCR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-INCR-ST1
              GO TO OPEN-017.
       OPEN-018.
           OPEN I-O SPECIALS-FILE.
           IF WS-SPECIALS-ST1 NOT = 0 
              MOVE "SPECIALS FILE BUSY ON OPEN I-O, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SPECIALS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SPECIALS-ST1
              GO TO OPEN-019.
           GO TO OPEN-020.
       OPEN-019.
           OPEN OUTPUT SPECIALS-FILE.
           IF WS-SPECIALS-ST1 NOT = 0 
              MOVE 0 TO WS-SPECIALS-ST1
              MOVE "SPECIALS FILE BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SPECIALS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SPECIALS-ST1
              GO TO OPEN-018.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlCrdRev"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE DEBTOR-MASTER
                 SOLD-BY
                 SALES-ANALYSIS
                 DISTRIBUTIONS
                 STOCK-MASTER
                 SPECIALS-FILE
                 STOCK-TRANS-FILE
                 DEBTOR-TRANS-FILE
                 INCR-REGISTER.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAmount".
       Copy "WriteFieldAmountDis".
       Copy "WriteFieldNumeric".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
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
