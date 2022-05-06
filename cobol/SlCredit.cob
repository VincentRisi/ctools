       IDENTIFICATION DIVISION.
       PROGRAM-ID. SlCredit.
       AUTHOR. CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
        SPECIAL-NAMES.
          CRT STATUS IS W-CRTSTATUS.
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
         Copy "SelectSlDaily".
         Copy "SelectSlParameter".
         Copy "SelectSlSpecials".
         Copy "SelectStTransLy".
         Copy "SelectSlRegLy".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdStock.
           COPY ChlfdSales.
           COPY ChlfdDaily.
           COPY ChlfdParam.
           COPY ChlfdDrTrans.
           COPY ChlfdStTrans.
           COPY ChlfdDisTot.
           COPY ChlfdRegister.
           COPY ChlfdSoldBy.
           COPY ChlfdSpecialSales.
           COPY ChlfdStTransLy.
           COPY ChlfdRegisterLy.
      *
       WORKING-STORAGE SECTION.
       77  PSW-SUB1             PIC S9(5) VALUE 0.
       77  PSW-SUB2             PIC S9(5) VALUE 0.
       77  WS-ACCNO-X           PIC X(7) VALUE " ".
       77  WS-DISCOUNT-CODE     PIC X VALUE " ".
       77  WS-ACCOUNT-NUMBER    PIC 9(7) VALUE 0.
       77  WS-LINECHANGED       PIC X VALUE " ".
       77  WS-QTY               PIC 9(5) VALUE 0.
       77  WS-DISCOUNTREG       PIC 9(6)V99 VALUE 0.
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-PORDERNO          PIC X(20) VALUE " ".
       77  WS-SALESANALYSIS     PIC X(10) VALUE " ".
       77  WS-SALESANALYSIS-SAVE PIC X(10) VALUE " ".
       77  WS-ANAL-CODE         PIC XX VALUE " ".
       77  WSAN-CODE-SAVE       PIC XX VALUE " ".
       77  WS-DELIVERVIA        PIC X(20) VALUE " ".
       77  WS-TERMOFSALE        PIC X(11) VALUE " ".
       77  WS-BINNO             PIC X(6) VALUE " ".
       77  Ws-Sold-By            PIC XX VALUE " ".
       77  WS-GSTNO             PIC X(13) VALUE " ".
       77  WS-INVOICEDATE       PIC 9(8) VALUE 0.
       77  WS-INVOICE-SAVE      PIC 9(6) VALUE 0.
       77  WS-DATE-SAVE         PIC 9(8) VALUE 0.
       77  WS-COMMENTLINE       PIC X(30) VALUE " ".
       77  WS-TAX-ONLY          PIC X(3) VALUE SPACES.
       77  WS-ADDONFREIGHT      PIC 9(6)V99 VALUE 0.
       77  WS-POSTADDON         PIC 9(6)V99 VALUE 0.
       77  WS-HANDADDON         PIC 9(6)V99 VALUE 0.
       77  WS-MISCADDON         PIC 9(6)V99 VALUE 0.
       77  WS-SUBTOTAL          PIC 9(6)V99 VALUE 0.
       77  WS-ADDONAMT          PIC 9(6)V99 VALUE 0.
       77  WS-TAXAMT            PIC 9(6)V99 VALUE 0.
       77  WS-INVOICETOTAL      PIC 9(6)V99 VALUE 0.
       77  WS-TAXABLETOTAL      PIC 9(6)V99 VALUE 0.
       77  WS-NONTAXABLETOTAL   PIC 9(6)V99 VALUE 0.
       77  WS-WORKTOTAL         PIC 9(6)V99 VALUE 0.
       77  WS-WORKTOTAL2        PIC 9(7)V99 VALUE 0.
       77  WS-COSTTOTAL         PIC 9(6)V99 VALUE 0.
       77  WS-PRICETOTAL        PIC 9(6)V99 VALUE 0.
       77  WS-EXPORTTOTAL       PIC 9(6)V99 VALUE 0.
       77  WS-DISCOUNT          PIC 9(6)V99 VALUE 0.
       77  WS-COST              PIC S9(6)V99 VALUE 0.
       77  WS-GST-AMT-TAXED        PIC 9(6)V99 VALUE 0.
       77  WS-GST-AMT-TAXABLE      PIC 9(6)V99 VALUE 0.
       77  WS-GST-AMT-NONTAXABLE   PIC 9(6)V99 VALUE 0.
       77  WS-GST-AMT-EXPORT       PIC 9(6)V99 VALUE 0.
       77  WS-GST-REC              PIC X(56) VALUE " ".
       77  WS-GST-PERCENT          PIC 99V99 VALUE 0.
       77  WS-BODY-LINE         PIC ZZ9.
       77  WS-PARAM             PIC X(34) VALUE " ".
       77  WS-IMM-PR            PIC X VALUE " ".
       77  WS-PAGE              PIC 9 VALUE 0.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "DrNameIq".
       77  WS-STOCK-INQUIRY     PIC X(8) VALUE "StMastIq".
       77  ANSWER1              PIC X VALUE " ".
       77  ANSWER2              PIC X VALUE " ".
       77  INVOICE-NO-CNT       PIC 9(8) VALUE 0.
       77  WS-PERCENT           PIC 9(2)V99 VALUE 0.
       77  WS-INVOICEDISCOUNT   PIC 9(2)V99 VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-WRITE             PIC X VALUE " ".
       77  WS-STTRANS-NO        PIC 9(6).
       77  WS-DRTRANS-NO        PIC 9(6).
       77  WS-TERM-SUB          PIC 9 VALUE 0.
       77  WS-DEL-SUB           PIC 9 VALUE 0.
       01  W-CRTSTATUS             PIC 9(4) value 0.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1       PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1        PIC 99.
       01  WS-SALES-STATUS.
           03  WS-SALES-ST1        PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1  PIC 99.
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
       01  WS-STTRANSLY-STATUS.
           03  WS-STTRANSLY-ST1    PIC 99.
       01  WS-INCR-LY-STATUS.
           03  WS-INCR-LY-ST1      PIC 99.
       01  WS-PASSWORD-KEY.
           03  WS-PA-KEY        PIC X OCCURS 11.
       01  W-READ-KEY           PIC X.
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
           03  WS-DIST-INVOICE  PIC 9(6)V99 VALUE 0.
           03  WS-DIST-ADDON    PIC 9(6)V99 VALUE 0.
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
       01  SPLIT-DELIVERVIA.
           03  WSDE-CODE        PIC X VALUE " ".
           03  WSDE-REST        PIC X(9) VALUE " ".
       01  SPLIT-TERMOFSALE.
           03  WSTE-CODE        PIC X VALUE " ".
           03  WSTE-REST        PIC X(10) VALUE " ".
       01  STORE-DEL.
         02  WS-DEL-OCCUR OCCURS 10.
           03  WS-DEL-TYPE       PIC X.
           03  WS-DEL-CODE       PIC X.
           03  WS-DEL-TERM       PIC X(20).
       01  STORE-TERM.
           02  WS-TERM-OCCUR OCCURS 10.
           03  WS-ST-TYPE       PIC X.
           03  WS-ST-CODE       PIC X.
           03  WS-ST-TERM       PIC X(11).
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 200.
               05  B-STOCKNUMBER.
                   07  B-1ST-CHAR     PIC X.
                   07  B-REST         PIC X(14).
               05  B-REMAINDER.
                   07  B-SHIPQTY           PIC 9(5).
                   07  B-STOCKDESCRIPTION  PIC X(20).
                   07  B-STOCKDESCRIPTION2 PIC X(20).
                   07  B-STOCKPRICE        PIC 9(8)V99.
                   07  B-STOCKCOST         PIC 9(8)V99.
                   07  B-DISCOUNTPERITEM   PIC 9(2)V99.
                   07  B-TAX               PIC X.
                   07  B-NETT              PIC 9(8)V99.
                   07  B-UNIT              PIC X(4).
               05  C-LINE REDEFINES B-REMAINDER.
                   07  C-SHIP              PIC X(5).
                   07  C-DESC              PIC X(20).
                   07  C-PRICE             PIC X(8).
                   07  C-COST              PIC X(8).
                   07  C-DISC              PIC X(5).
                   07  C-REST              PIC X(5).
                   07  C-UNIT              PIC X(4).
       01  WS-BODY-LINES.
           03  WS-STOCKNUMBER      PIC X(15).
           03  WS-SHIPQTY          PIC 9(5).
           03  WS-STOCKDESCRIPTION PIC X(20).
           03  WS-STOCKPRICE       PIC 9(8)V99.
           03  WS-STOCKCOST        PIC 9(8)V99.
           03  WS-DISCOUNTPERITEM  PIC 9(2)V99.
           03  WS-TAX              PIC X.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
      *
       Copy "WsDateInfo".
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
           03  F-NAMEFIELDRED.
               05  F-NAMEFIELDRED1 PIC X.
               05  F-NAMEFIELDRED7 PIC X(6).
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM CLEAR-SCREEN.
           PERFORM CLEAR-FIELDS.
           PERFORM ERROR1-020
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.

           PERFORM READ-PARAMETER-LOCK
           MOVE PA-CREDIT-NUMBER TO WS-INVOICE
           ADD 1                 TO PA-CREDIT-NUMBER
           PERFORM REWRITE-PARAMETER
           MOVE "CREDITNUM"      TO F-FIELDNAME
           MOVE 9                TO F-CBFIELDNAME
           MOVE WS-INVOICE       TO F-EDNAMEFIELDNUM
           MOVE 6                TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-NUMERIC.

           MOVE 2910 TO POS
           DISPLAY "1. DEBTOR-TRANS BEING WRITTEN..........    " AT POS
           PERFORM WRITE-DEBTOR-TRANSACTIONS
           
           MOVE 2910 TO POS
           DISPLAY "2. STOCK-TRANS BEING WRITTEN..........     " AT POS
           PERFORM WRITE-STOCK-TRANSACTIONS
           
           MOVE 2910 TO POS
           DISPLAY "3. C/NOTE REGISTER BEING WRITTEN.......    " AT POS
           PERFORM WRITE-INCR-REGISTER
           
           MOVE 2910 TO POS
           DISPLAY "4. DEBTOR FILE BEING UPDATED....           " AT POS
           PERFORM UPDATE-DEBTOR
           
           MOVE 2910 TO POS
           DISPLAY "5. STOCK FILES BEING UPDATED..........     " AT POS
           PERFORM UPDATE-STOCK
           
           MOVE 2910 TO POS
           DISPLAY "6. SALES ANALYSIS FILE BEING UPDATED....   " AT POS
           PERFORM UPDATE-SALES
           
           MOVE 2910 TO POS
           DISPLAY "7. DISTRIBUTION FILE BEING UPDATED......   " AT POS
           PERFORM UPDATE-DISTRIBUTION
           
           MOVE 2910 TO POS
           DISPLAY "8. SOLDBY FILE BEING WRITTEN..........    " AT POS
           PERFORM WRITE-SOLD-BY
           
           GO TO CONTROL-010.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-010.
            PERFORM CI-900.
            PERFORM CLEAR-FIELDS.
            MOVE "ACCOUNTNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            MOVE " " TO WS-NAMEANDADDRESS
                        WS-ACCNO-X.
            MOVE 0 TO WS-POSTCODE.
            MOVE 0 TO DR-ACCOUNT-NUMBER
                      SUB-20.
            MOVE 0 TO WS-ADDONFREIGHT WS-POSTADDON
                      WS-HANDADDON WS-MISCADDON.
            MOVE 0 TO WS-INVOICE-SAVE
                      WS-DATE-SAVE.
            PERFORM CLEAR-FIELDS.
            MOVE " " TO WS-BINNO.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO F-NAMEFIELDRED.
      ****************************************************
      * 'F5' KEY, TO FIND AN INVOICE, DISPLAY AND CREDIT *
      ****************************************************
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 NOT = "*" AND NOT = "L"
                GO TO GET-010.
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "*"
                PERFORM CLEAR-FIELDS
                MOVE "          "    TO ALPHA-RATE
                MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE    TO WS-INVOICE
                PERFORM READ-INVOICE-REGISTER.
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "*"
              IF INCR-INVOICE = 0
                GO TO GET-010.
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "*"
                PERFORM READ-DEBTORS
                PERFORM READ-STOCK-TRANSACTIONS
                PERFORM FIND-INFO
             IF DR-NAME = "UNKNOWN"
                MOVE
           "THE ACCOUNT NUMBER IS IN-VALID, ADVISE YOUR SUPERVISOR."
                TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                GO TO GET-010
            ELSE
                GO TO GET-110.
      **************************************************************
      * 'F5' KEY, TO FIND AN INVOICE FROM L/YR, DISPLAY AND CREDIT *
      **************************************************************
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 NOT = "*" AND NOT = "L"
                GO TO GET-010.
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "L"
                PERFORM CLEAR-FIELDS
                MOVE "          "    TO ALPHA-RATE
                MOVE F-NAMEFIELDRED7 TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE    TO WS-INVOICE
                PERFORM READ-INVOICE-REGISTERLY.
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "L"
              IF INCR-LY-INVOICE = 0
                GO TO GET-010.
           IF F-EXIT-CH = X"19"
            IF F-NAMEFIELDRED1 = "L"
                PERFORM READ-DEBTORS
                PERFORM READ-STOCK-TRANSACTIONSLY
                PERFORM FIND-INFO
             IF DR-NAME = "UNKNOWN"
                MOVE
           "THE ACCOUNT NUMBER IS IN-VALID, ADVISE YOUR SUPERVISOR."
                TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                GO TO GET-010
            ELSE
                GO TO GET-110.

            IF F-NAMEFIELD = "STOCK"
                CLOSE STOCK-MASTER
                CLOSE PARAMETER-FILE
                CALL WS-STOCK-INQUIRY USING WS-LINKAGE
                CANCEL WS-STOCK-INQUIRY
                PERFORM CLEAR-SCREEN
                OPEN I-O STOCK-MASTER
                OPEN I-O PARAMETER-FILE
                PERFORM DISPLAY-FORM
                GO TO GET-010.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO DR-ACCOUNT-NUMBER
                                 WS-ACCOUNT-NUMBER.
            IF DR-ACCOUNT-NUMBER = 0
                CLOSE DEBTOR-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM CLEAR-SCREEN
                OPEN I-O DEBTOR-MASTER
                PERFORM DISPLAY-FORM
                GO TO GET-010.

            PERFORM READ-DEBTORS.
       GET-012.
            MOVE "DEBTORNAME" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE WS-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            IF DR-NAME = "UNKNOWN"
                GO TO GET-010.
            MOVE DR-DISCOUNT-CODE  TO WS-DISCOUNT-CODE
            MOVE DR-DELIVERY-CODE  TO WSDE-CODE.
            MOVE DR-SALES-ANALYSIS TO WSAN-CODE.
            MOVE "POSTADD1" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-ADD1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD2" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-ADD2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD3" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-ADD3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTCODE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-POSTCODE TO F-NAMEFIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-DELADD1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD2" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-DELADD2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-DELADD3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE WSAN-CODE TO SA-KEY.
            PERFORM READ-SALES-ANALYSIS.
            MOVE "SALESANALYSIS" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE WS-SALESANALYSIS TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELIVERVIA" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DR-DELIVERY-CODE TO WS-DEL-SUB.
            MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELIVERVIA.
            MOVE WS-DELIVERVIA TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TERMOFSALE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DR-TERMS-CODE TO WS-TERM-SUB.
            MOVE WS-ST-TERM (WS-TERM-SUB) TO WS-TERMOFSALE.
            MOVE WS-TERMOFSALE TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GSTNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DR-GSTNO TO F-NAMEFIELD WS-GSTNO.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CREDITDATE" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            MOVE WS-DATE      TO SPLIT-DATE WS-INVOICEDATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            GO TO GET-110.
       GET-020.
            MOVE "DEBTORNAME" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE WS-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                PERFORM CANCEL-INVOICE
                MOVE 0 TO DR-ACCOUNT-NUMBER
                          WS-ACCOUNT-NUMBER
                PERFORM CLEAR-FIELDS
                MOVE " " TO WS-BINNO
                GO TO GET-010.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-NAME.
       GET-030.
            MOVE "POSTADD1" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-020.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ADD1.
       GET-040.
            MOVE "POSTADD2" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-030.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ADD2.
       GET-050.
            MOVE "POSTADD3" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-040.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ADD3.
       GET-060.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "POSTCODE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-050.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-POSTCODE.
       GET-070.
            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-060.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DELADD1.
       GET-080.
            MOVE "DELADD2" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-070.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DELADD2.
       GET-090.
            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-080.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DELADD3.
       GET-110.
            MOVE "COMMENTLINE" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE 30 TO F-CBFIELDLENGTH.
            MOVE " " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE " " TO WS-PORDERNO.
            MOVE "POORDERNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"01"
                GO TO GET-070.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-PORDERNO.
       GET-115.
            MOVE "SOLDBY" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"01"
               GO TO GET-110.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO Ws-Sold-By.
            IF Ws-Sold-By = " "
               GO TO GET-115.
            IF F-EXIT-CH = X"1D"
               GO TO GET-120.
            GO TO GET-150.
       GET-120.
            MOVE WSAN-CODE TO WSAN-CODE-SAVE.
            MOVE WS-SALESANALYSIS TO WS-SALESANALYSIS-SAVE.
       GET-125.
            MOVE "SALESANALYSIS" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE 0 TO F-CBFIRSTLINE.
            MOVE 14 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            PERFORM ERROR-020.
            MOVE " " TO F-NAMEFIELD.
            IF F-EXIT-CH = X"01"
                PERFORM READ-FIELD-ALPHA
                IF F-NAMEFIELD = WS-SALESANALYSIS
                GO TO GET-115
            ELSE
                GO TO GET-125.
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = WS-SALESANALYSIS
                IF WSAN-CODE = " 6" OR = "6 " OR = "06" OR = "52"
                AND WS-GSTNO = "EXPORT" OR = "EXPORT       "
                GO TO GET-130.
            IF F-NAMEFIELD = WS-SALESANALYSIS
                 MOVE WSAN-CODE-SAVE TO WSAN-CODE
                 GO TO GET-130.
            MOVE F-NAMEFIELD TO WS-SALESANALYSIS.
            MOVE WS-SALESANALYSIS TO SPLIT-ANALYSIS.
            MOVE WSAN-CODE-2 TO ALPHA-FIELD.
            IF WSAN-CODE-1 NOT = "1" AND NOT = "2" AND NOT = "3"
                       AND NOT = "4" AND NOT = "5" AND NOT = "6"
                       AND NOT = "7" AND NOT = "8" AND NOT = "9"
                       AND NOT = "0"
                MOVE "INVALID SALES ANALYSIS !" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE WSAN-CODE-SAVE TO WSAN-CODE
                MOVE WS-SALESANALYSIS-SAVE TO WS-SALESANALYSIS
                GO TO GET-125.
            IF ALPHA-VALUE
                MOVE WSAN-CODE-1 TO WSAN-CODE-2
                MOVE "0" TO WSAN-CODE-1.
            PERFORM READ-SALES-ANALYSIS.
            IF WS-SALESANALYSIS = "UNKNOWN"
                MOVE "INVALID SALES ANALYSIS !" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE WSAN-CODE-SAVE TO WSAN-CODE
                MOVE WS-SALESANALYSIS-SAVE TO WS-SALESANALYSIS
                GO TO GET-125.
            IF WSAN-CODE = "06" OR = "6 " OR = " 6" OR = "52"
                MOVE "GSTNO" TO F-FIELDNAME
                MOVE 5 TO F-CBFIELDNAME
                MOVE "EXPORT" TO F-NAMEFIELD WS-GSTNO
                MOVE 13 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.
            MOVE "SALESANALYSIS" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE WS-SALESANALYSIS TO F-NAMEFIELD.
            MOVE 14 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            IF WS-SALESANALYSIS = "UNKNOWN"
                MOVE 2801 TO POS
                DISPLAY " " AT POS
                MOVE WSAN-CODE-SAVE TO WSAN-CODE
                MOVE WS-SALESANALYSIS-SAVE TO WS-SALESANALYSIS
                GO TO GET-125.
       GET-130.
            MOVE "DELIVERVIA" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-110.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-FIELD WS-DELIVERVIA.
            IF ALPHA-VALUE
                GO TO GET-140.
            MOVE WS-DELIVERVIA TO SPLIT-DELIVERVIA.
            MOVE WSDE-CODE TO WS-DEL-SUB.
            IF WS-DEL-SUB = 0
               MOVE 1 TO WS-DEL-SUB.
            MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELIVERVIA.
            IF WS-DELIVERVIA = ALL "X"
                MOVE "INVALID DELIVERY CODE!!!" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-130.
            MOVE "DELIVERVIA" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE WS-DELIVERVIA TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-140.
            MOVE "TERMOFSALE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
      *DELETE KEY
            IF F-EXIT-CH = X"7F"
               MOVE " " TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
               GO TO GET-140.
            IF F-EXIT-CH = X"01"
                GO TO GET-130.
            MOVE 0 TO F-CBFIRSTLINE.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-FIELD.
            IF ALPHA-VALUE
                GO TO GET-150.
            MOVE F-NAMEFIELD TO WS-TERMOFSALE.
            MOVE WS-TERMOFSALE TO SPLIT-TERMOFSALE.
            MOVE WSTE-CODE TO WS-TERM-SUB.
            IF WS-TERM-SUB = 0
               MOVE 1 TO WS-TERM-SUB.
            MOVE WS-ST-TERM (WS-TERM-SUB) TO WS-TERMOFSALE.
            IF WS-TERMOFSALE = ALL "X"
                MOVE "INVALID TERM CODE!!!" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-140.
            MOVE "TERMOFSALE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE WS-TERMOFSALE TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-150.
            IF WS-SALESANALYSIS = "UNKNOWN"
                MOVE 2801 TO POS
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-120.
            MOVE "BINNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"01"
                GO TO GET-140.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-BINNO.
            IF WS-GSTNO = "EXPORT"
              AND WSAN-CODE NOT = "06" AND NOT = "6 " AND NOT = " 6"
                        AND NOT = "52"
              MOVE "SALESANALYSIS MUST BE 6 OR 52" TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO GET-120.
      *F8-KEY
            IF F-EXIT-CH NOT = X"1D"
                MOVE 0 TO WS-INVOICEDISCOUNT
                MOVE 1 TO SUB-1 F-INDEX
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
                GO TO GET-180.
       GET-160.
            MOVE "GSTNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-150.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-GSTNO.
       GET-170.
            IF WS-ABOVE-BODY NOT = "1"
                MOVE " " TO WS-ABOVE-BODY.
            IF WS-GSTNO = "EXPORT"
               AND WSAN-CODE NOT = "6 " AND NOT = " 6"
                   AND NOT = "06" AND NOT = "52"
                       MOVE "SALES ANALYSIS MUST BE 6 OR 52" TO
                       WS-MESSAGE
                       PERFORM ERROR-MESSAGE
                       GO TO GET-120.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "CREDITDATE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-160.
            IF F-EXIT-CH = X"0B"
                GO TO GET-180.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO GET-170.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO WS-INVOICEDATE
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO GET-170.
            PERFORM SET-GST.
            MOVE 1 TO SUB-1 F-INDEX.
            MOVE 0 TO SUB-25.
            PERFORM SCROLL-NEXT.
            PERFORM SCROLL-PREVIOUS.
       GET-180.
            MOVE "N" TO WS-LINECHANGED.
            PERFORM FILL-BODY.
            IF SUB-20 < SUB-1
                MOVE SUB-1 TO SUB-20.
            IF SUB-20 > 100
                MOVE 100 TO SUB-1 SUB-20.
            IF WS-ABOVE-BODY = "1"
                GO TO GET-150.
       GET-190.
            MOVE 1 TO F-INDEX.
            PERFORM CLEAR-BOTTOM-FIELDS.
            MOVE 0 TO WS-ADDONFREIGHT
                      WS-POSTADDON
                      WS-HANDADDON
                      WS-MISCADDON.
            MOVE 1 TO F-INDEX.
            MOVE "   " TO WS-TAX-ONLY.
            PERFORM GET-240
            PERFORM GET-250
            PERFORM GET-260
            PERFORM GET-270.
            MOVE "COMMENTLINE" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                MOVE 1 TO F-INDEX
                PERFORM CLEAR-BOTTOM-FIELDS
                MOVE 0 TO WS-ADDONFREIGHT
                          WS-POSTADDON
                          WS-HANDADDON
                          WS-MISCADDON
                          SUB-25
                MOVE 1 TO F-INDEX SUB-1
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
                GO TO GET-180.
            IF F-EXIT-CH = X"07"
                GO TO GET-170.
            MOVE 30 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-COMMENTLINE.
      *F8-KEY.
            IF F-EXIT-CH = X"1D"
                GO TO GET-200.
       GET-200.
            MOVE "                   " TO F-NAMEFIELD
            MOVE "ADDONFREIGHT"        TO F-FIELDNAME
            MOVE 12                    TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-190.
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-ADDONFREIGHT
                                 F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
       GET-210.
            MOVE "                   " TO F-NAMEFIELD
            MOVE "POSTADDON"           TO F-FIELDNAME
            MOVE 9                     TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-200.
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-POSTADDON
                                 F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
       GET-220.
            MOVE "                   " TO F-NAMEFIELD
            MOVE "HANDADDON"           TO F-FIELDNAME
            MOVE 9                     TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-210.
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-HANDADDON
                                 F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
       GET-230.                 
            MOVE "                   " TO F-NAMEFIELD
            MOVE "   "                 TO WS-TAX-ONLY
            MOVE "MISC.ADDON"          TO F-FIELDNAME
            MOVE 10                    TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-220.
            IF F-EXIT-CH = X"1D"
                MOVE "YES" TO WS-TAX-ONLY.
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-MISCADDON
                                 F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
       GET-240.
            PERFORM CALCULATE-TOTALS
            IF WS-TAX-ONLY = "YES"
               PERFORM TAX-ONLY.
            MOVE "SUBTOTAL"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-SUBTOTAL TO F-EDNAMEFIELDAMOUNT
            MOVE 9           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
       GET-250.
            MOVE "ADDONAMT"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-ADDONAMT TO F-EDNAMEFIELDAMOUNT
            MOVE 9           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
       GET-260.
            MOVE "TAXAMT"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            MOVE WS-TAXAMT TO F-EDNAMEFIELDAMOUNT
            MOVE 9         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
       GET-270.
            COMPUTE WS-INVOICETOTAL = WS-SUBTOTAL +
                                      WS-TAXAMT + 
                                      WS-ADDONAMT.
            MOVE "CREDITTOTAL"   TO F-FIELDNAME
            MOVE 11              TO F-CBFIELDNAME
            MOVE WS-INVOICETOTAL TO F-EDNAMEFIELDAMOUNT
            MOVE 9               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
       GET-900.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-190.
            ADD WS-INVOICETOTAL    TO WS-DIST-INVOICE
            ADD WS-ADDONAMT        TO WS-DIST-ADDON
            ADD WS-TAXAMT          TO WS-GST-AMT-TAXED
            ADD WS-TAXABLETOTAL    TO WS-GST-AMT-TAXABLE
            ADD WS-NONTAXABLETOTAL TO WS-GST-AMT-NONTAXABLE
            ADD WS-EXPORTTOTAL     TO WS-GST-AMT-EXPORT.
       GET-999.
            EXIT.
      *
       CALC-POS-OF-CURSOR SECTION.
       CPOC-005.
             IF SUB-1SAVE < 7
                 GO  TO CPOC-500.
       CPOC-010.
            COMPUTE SUB-1 = SUB-1SAVE - F-INDEXSAVE.
            IF SUB-1 < 0
               MOVE 0 TO SUB-1.
            PERFORM SCROLL-NEXT.
       CPOC-500.
            MOVE SUB-1SAVE   TO SUB-1
            MOVE F-INDEXSAVE TO F-INDEX.
       CPOC-999.
           EXIT.
      *
       FILL-BODY SECTION.
       FILL-000.
             MOVE " " TO WS-ABOVE-BODY.
             MOVE 1 TO SUB-1 SUB-2 SUB-3.
       FILL-005.
            MOVE 2710 TO POS
            DISPLAY
            "PRESS <ALT-Z> TO GO INTO ZOOMBOX MODE TO CALL UP STOCKINQ."
               AT POS.

             PERFORM ERROR-020.
             MOVE 3010 TO POS.
             DISPLAY "    BODY LINE: " AT POS.
             ADD 16 TO POS.
             MOVE SUB-1 TO WS-BODY-LINE.
             DISPLAY WS-BODY-LINE AT POS.
       FILL-010.
            PERFORM RUNNING-TOTAL.
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.

            IF F-EXIT-CH = X"0B" 
              IF B-STOCKNUMBER (SUB-1) = " "
                GO TO FILL-010.
            IF F-EXIT-CH = X"0A" 
             IF B-STOCKNUMBER (SUB-1) NOT = " "
              IF F-INDEX = 7
                PERFORM SCROLL-NEXT
                GO TO FILL-010
              ELSE
                ADD 1 TO SUB-1 F-INDEX
                GO TO FILL-010.

            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            PERFORM ERROR-020.
            PERFORM FILL-005.

            IF F-EXIT-CH = X"01" AND F-INDEX = 1
             IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                MOVE "1" TO WS-ABOVE-BODY
                GO TO FILL-999
            ELSE
                IF SP-1STCHAR NOT = "*" AND NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                MOVE "1" TO WS-ABOVE-BODY
                GO TO FILL-999.

            IF F-EXIT-CH = X"01" AND F-INDEX > 1
             IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-010
            ELSE
                IF SP-1STCHAR NOT = "*" AND NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-010.

            IF F-EXIT-CH = X"0B" AND F-INDEX < 7
                IF F-NAMEFIELD = B-STOCKNUMBER (SUB-1)
                ADD 1 TO F-INDEX SUB-1
                GO TO FILL-010
            ELSE
               IF SP-1STCHAR NOT = "*" AND NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
                MOVE 15 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                ADD 1 TO F-INDEX SUB-1
                GO TO FILL-010.

            IF F-EXIT-CH = X"0C"
                PERFORM SCROLL-NEXT-PAGE
                GO TO FILL-010.
            IF F-EXIT-CH = X"05"
                PERFORM SCROLL-PREVIOUS
                GO TO FILL-010.
            IF F-EXIT-CH = X"11"
                PERFORM SCROLL-NEXT
                GO TO FILL-010.
            IF F-EXIT-CH = X"13"
                GO TO FILL-010.
      * <TAB> CHARACTER
            IF F-EXIT-CH = X"09"
                MOVE " " TO WS-ABOVE-BODY
                PERFORM ERROR-020
                PERFORM CHECK-SUB1-TOTAL
                GO TO FILL-999.
      **********************************************************
      * 'ESC' = X"07"; 'CODE-CANCEL' = X"87" ; 'Alt-F10'=X"9F" *
      **********************************************************
      * <ESC>
            IF F-EXIT-CH = X"07" OR = X"87"
                AND B-STOCKNUMBER (SUB-1) = " "
                GO TO FILL-010.
      *<ALT-ESC>
            IF F-EXIT-CH = X"87" OR = X"9F"
                MOVE SUB-1 TO SUB-7
                PERFORM CANCEL-TRANSACTION
                MOVE 1 TO SUB-1
                          F-INDEX
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
                GO TO FILL-010.
      ***********************************************
      *ZOOMBOX MODE                                 *
      * <CODE-z> = X"FA"  <CODE-SHIFT-Z> = X"DA"    *
      ***********************************************
      *IN CTOS: <CODE-Z>; <ALT-Z> IN LINUX          *
           IF F-EXIT-CH = X"FA" OR = X"DA"
                MOVE SUB-1   TO SUB-1SAVE
                MOVE F-INDEX TO F-INDEXSAVE
                PERFORM CLEAR-SCREEN
                CALL WS-STOCK-INQUIRY USING WS-LINKAGE
                CANCEL WS-STOCK-INQUIRY
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                PERFORM FIND-010
                PERFORM CALC-POS-OF-CURSOR
                GO TO FILL-005.

            IF F-NAMEFIELD = " "
               GO TO FILL-010.
      *
      * NEW SECTION TO DISALLOW RE-ENTRY OF AN EXISTING LINE
      *
            IF F-NAMEFIELD NOT = B-STOCKNUMBER (SUB-1)
             IF B-STOCKNUMBER (SUB-1) NOT = " "
              IF SP-1STCHAR NOT = "*" AND NOT = "/"
               MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
               MOVE 15                    TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               GO TO FILL-010.
      *******************************************
      *SECTION TO CHANGE QTY'S OF EXISTING LINE *
      *F5      = "19"   CHANGES SHIP-QTY        *
      *CODE-F5 = "99"   CHANGES SHIP & ORDER-QTY*
      *******************************************
           IF F-EXIT-CH = X"19" OR = X"99"
            IF B-STOCKNUMBER (SUB-1) = " "
               GO TO FILL-010.
           IF F-EXIT-CH = X"19" OR = X"99"
            IF B-STOCKNUMBER (SUB-1) > " "
             IF SP-1STCHAR = "*"
                GO TO FILL-010.
           IF F-EXIT-CH = X"19" OR = X"99"
               GO TO FILL-035.
            
            MOVE F-NAMEFIELD TO B-STOCKNUMBER (SUB-1).
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
            IF SP-1STCHAR = "*"
                PERFORM FILL-COMMENT
             IF SUB-1 < SUB-25
                MOVE "Y" TO WS-LINECHANGED
                GO TO FILL-090
             ELSE
                GO TO FILL-090.

            IF SP-1STCHAR NOT = "/"
                AND B-STOCKDESCRIPTION (SUB-1) NOT = " "
                AND B-SHIPQTY (SUB-1) NOT = 0
                GO TO FILL-010.

            IF SP-1STCHAR NOT = "/"
                MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER
                PERFORM READ-STOCK-LOCK
            ELSE
                IF SP-1STCHAR NOT = "/"
                GO TO FILL-010.

            IF SP-1STCHAR = "/"
             IF SUB-1 < SUB-25
                MOVE "Y" TO WS-LINECHANGED
             ELSE
                MOVE " " TO ST-DESCRIPTION1
                            ST-DESCRIPTION2
                MOVE 0 TO ST-PRICE
                          ST-AVERAGECOST
                          ST-DISCOUNT1
                MOVE "Y"    TO B-TAX (SUB-1)
                MOVE "EACH" TO B-UNIT (SUB-1).
                
            IF SP-1STCHAR NOT = "/"
               AND ST-DESCRIPTION1 = " "
                   MOVE "INVALID STOCK ITEM!!!" TO WS-MESSAGE
                   PERFORM ERROR-MESSAGE
                   MOVE " " TO B-STOCKNUMBER (SUB-1)
                   GO TO FILL-005.

            IF ST-DESCRIPTION1 = " "
                GO TO FILL-0100.
            IF B-SHIPQTY (SUB-1) > 0
      *          ADD B-SHIPQTY (SUB-1) TO ST-QTYONHAND
                MOVE "SHIPQTY"        TO F-FIELDNAME
                MOVE 7                TO F-CBFIELDNAME
                MOVE 5                TO F-CBFIELDLENGTH
                MOVE " "              TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO FILL-035.

            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE ST-DESCRIPTION1 TO F-NAMEFIELD
                                    B-STOCKDESCRIPTION (SUB-1).
            MOVE ST-DESCRIPTION2 TO B-STOCKDESCRIPTION2 (SUB-1).
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       FILL-0100.
            IF ST-PRICE = 0
                 GO TO FILL-0110.
            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE ST-PRICE TO F-EDNAMEFIELDAMOUNT
                             B-STOCKPRICE (SUB-1).
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.
       FILL-0110.
            IF ST-AVERAGECOST = 0
                 GO TO FILL-0120.
            MOVE "STOCKCOST"    TO F-FIELDNAME.
            MOVE 9              TO F-CBFIELDNAME.
            MOVE ST-AVERAGECOST TO F-EDNAMEFIELDAMOUNT
                                   B-STOCKCOST (SUB-1).
            MOVE 9              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.
       FILL-0120.
            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME.
            MOVE 15 TO F-CBFIELDNAME.
            IF WS-INVOICEDISCOUNT > 0
                MOVE WS-INVOICEDISCOUNT TO F-EDNAMEFIELDAMOUNTDIS
                                           B-DISCOUNTPERITEM (SUB-1)
                MOVE 5 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNTDIS
                GO TO FILL-0130.
            IF WS-DISCOUNT-CODE = "0" OR = " "
                MOVE 0 TO F-EDNAMEFIELDAMOUNTDIS
                          B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "1"
                MOVE ST-DISCOUNT1 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "2"
                MOVE ST-DISCOUNT2 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "3"
                MOVE ST-DISCOUNT3 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "4"
                MOVE ST-DISCOUNT4 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "5"
                MOVE ST-DISCOUNT5 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "6"
                MOVE ST-DISCOUNT6 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "7"
                MOVE ST-DISCOUNT7 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "8"
                MOVE ST-DISCOUNT8 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            IF WS-DISCOUNT-CODE = "9"
                MOVE ST-DISCOUNT9 TO F-EDNAMEFIELDAMOUNTDIS
                                     B-DISCOUNTPERITEM (SUB-1).
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.
       FILL-0130.
            MOVE "TAX" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
      *      IF WS-GSTNO = "EXPORT" OR > " "
            IF WS-GSTNO = "EXPORT" OR = "EXPORT       "
                MOVE "N" TO F-NAMEFIELD
                            B-TAX (SUB-1)
            ELSE
                MOVE "Y" TO F-NAMEFIELD
                            B-TAX (SUB-1).
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       FILL-035.
            MOVE "                   " TO F-NAMEFIELD
            MOVE "SHIPQTY" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE 5         TO F-CBFIELDLENGTH
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK
              IF SP-1STCHAR NOT = "/"
                MOVE 0   TO B-SHIPQTY (SUB-1)
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO FILL-010
              ELSE
                GO TO FILL-010.

            IF F-EXIT-CH = X"1D" OR = X"0A"
               GO TO FILL-037.
               
            DISPLAY " " AT 3079 WITH BELL.
            GO TO FILL-035.
       FILL-037.
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD       TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE      TO B-SHIPQTY (SUB-1)
            MOVE B-SHIPQTY (SUB-1) TO F-EDNAMEFIELDQTY
            MOVE 5                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY.

            IF B-SHIPQTY (SUB-1) = 0
                DISPLAY " " AT 3079 WITH BELL
                GO TO FILL-035.
       FILL-038.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
      *      IF SP-1STCHAR NOT = "/"
      *          ADD B-SHIPQTY (SUB-1) TO ST-QTYONHAND.
       FILL-040.
            IF SP-1STCHAR NOT = "/"
              REWRITE STOCK-RECORD
                INVALID KEY
                MOVE "STOCK RECORD:"       TO WS-DAILY-1ST
                MOVE B-STOCKNUMBER (SUB-1) TO WS-DAILY-2ND
                MOVE "NOT RE-WRITTEN"      TO WS-DAILY-3RD
                MOVE "IN CREDIT NOTE"      TO WS-DAILY-4TH
                PERFORM WRITE-DAILY.
            IF SP-1STCHAR = "/"
                GO TO FILL-045.
      *F8-KEY
            IF F-EXIT-CH NOT = X"1D"
                GO TO FILL-090.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
            IF SP-1STCHAR NOT = "/"
              IF F-EXIT-CH = X"1D"
                GO TO FILL-050.
       FILL-045.
            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            PERFORM ERROR-020.
            MOVE 3010 TO POS.
            DISPLAY "    BODY LINE: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
            IF F-EXIT-CH = X"01"
                GO TO FILL-035.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
            IF SP-1STCHAR NOT = "/"
                GO TO FILL-050.
            MOVE 20 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-045.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO B-STOCKDESCRIPTION (SUB-1).
       FILL-050.
            MOVE SPACES TO F-NAMEFIELD.
            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO FILL-040.
            MOVE 9 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-050.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO B-STOCKPRICE (SUB-1).
            MOVE B-STOCKPRICE (SUB-1) TO F-EDNAMEFIELDAMOUNT.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNT.

            IF SP-1STCHAR = "/"
               GO TO FILL-060.
            IF F-EXIT-CH NOT = X"1D"
               GO TO FILL-070.
            
            IF SP-1STCHAR NOT = "/"
               MOVE X"16" TO F-EXIT-CH
               PERFORM CHECK-PASSWORD
               MOVE X"1D" TO F-EXIT-CH
             IF WS-PASSWORD-VALID = "N"
               GO TO FILL-070.
       FILL-060.
            MOVE SPACES TO F-NAMEFIELD.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
      *      IF SP-1STCHAR NOT = "/"
      *          GO TO FILL-070.
            MOVE "STOCKCOST" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO FILL-050.
            MOVE 9 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-060.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD         TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE        TO B-STOCKCOST (SUB-1).
            MOVE B-STOCKCOST (SUB-1) TO F-EDNAMEFIELDAMOUNT.
            PERFORM WRITE-FIELD-AMOUNT.
       FILL-070.
            MOVE SPACES TO F-NAMEFIELD.
            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME.
            MOVE 15 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO FILL-050.
            MOVE 5 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-070.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO B-DISCOUNTPERITEM (SUB-1).
            MOVE B-DISCOUNTPERITEM (SUB-1) TO F-EDNAMEFIELDAMOUNTDIS.
            PERFORM WRITE-FIELD-AMOUNTDIS.
       FILL-080.
            MOVE SPACES TO F-NAMEFIELD.
            MOVE "TAX" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO FILL-070.
            IF F-EXIT-CH = X"0B"
                GO TO FILL-090.
            MOVE 1 TO F-CBFIELDLENGTH.
      *DELETE KEY
            IF F-EXIT-CH = X"7F"
               MOVE " " TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
               GO TO FILL-080.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-080.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO B-TAX (SUB-1).

            MOVE "EACH" TO B-UNIT (SUB-1).
       FILL-090.
      *      MOVE B-STOCKNUMBER (SUB-1) TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.      
            ADD 1 TO SUB-1 F-INDEX.
            IF WS-LINECHANGED = "N"
               MOVE SUB-1 TO SUB-25.
            MOVE "N" TO WS-LINECHANGED.
            IF SUB-1 > 200
                MOVE 200 TO SUB-1 SUB-25
                MOVE "200 LINES ARE UP, PRESS 'ESC' TO TAB."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-999.
            IF F-INDEX < 8
                GO TO FILL-005.
            SUBTRACT 1 FROM SUB-1.
            PERFORM SCROLL-NEXT.
            MOVE 1 TO F-INDEX.
            GO TO FILL-010.
       FILL-999.
             EXIT.
      *
       TAX-ONLY SECTION.
       TO-000.
            MOVE "TAXAMT"     TO F-FIELDNAME
            MOVE 6            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-TAXAMT
                                 F-EDNAMEFIELDAMOUNT
            PERFORM WRITE-FIELD-AMOUNT.
       TO-999.
            EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.

           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE " " TO ST-STOCKNUMBER
                           ST-DESCRIPTION1
                           ST-DESCRIPTION2
               MOVE 0 TO ST-PRICE
                         ST-AVERAGECOST
                         ST-DISCOUNT1
               GO TO R-ST-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-000.
            MOVE ST-UNITOFMEASURE TO B-UNIT (SUB-1).
       R-ST-999.
           EXIT.
      *
       READ-STOCK-LOCK SECTION.
       R-STL-000.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.
       R-STL-010.
           READ STOCK-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE " " TO ST-STOCKNUMBER
                           ST-DESCRIPTION1
                           ST-DESCRIPTION2
               MOVE 0 TO ST-PRICE
                         ST-AVERAGECOST
                         ST-DISCOUNT1
               MOVE "STOCK ITEM DOES NOT EXIST, PRESS 'ESC' TO OMIT"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCKNUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO R-STL-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCKNUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-STL-010.
           MOVE ST-UNITOFMEASURE TO B-UNIT (SUB-1).
       R-STL-999.
           EXIT.
      *
       READ-PARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           READ PARAMETER-FILE LOCK          
               INVALID KEY
               NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO PARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON READ-LOCK, 'ESC' TO RETRY." 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RPL-000.
       RPL-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           READ PARAMETER-FILE
               INVALID KEY
               NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO PARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RP-000.
       RP-999.
           EXIT.
      *
       REWRITE-PARAMETER SECTION.
       REWP-000.
           REWRITE PARAMETER-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "PARAMETER RECORD NOT UPDATED!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON REWRITE , 'ESC' TO RETRY." 
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO REWP-000.
       REWP-999.
           EXIT.
      *
       READ-INVOICE-REGISTER SECTION.
       RIR-000.
           MOVE WS-INVOICE TO INCR-INVOICE.
           MOVE 1          TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY
               INVALID KEY NEXT SENTENCE.
       RIR-005.
           READ INCR-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               MOVE "NO SUCH INVOICE TO READ, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
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
       RIR-010.
              MOVE INCR-ACCOUNT     TO WS-ACCOUNT-NUMBER
              MOVE INCR-GSTNO       TO WS-GSTNO
              MOVE WS-DATE          TO WS-INVOICEDATE
              MOVE INCR-SALES       TO WSAN-CODE
              MOVE INCR-INVCRED-AMT TO WS-INVOICETOTAL
              MOVE INCR-TAX         TO WS-TAXAMT
              MOVE INCR-ADDONS      TO WS-ADDONAMT
              MOVE INCR-ADDPOST     TO WS-POSTADDON
              MOVE INCR-ADDFREIGHT  TO WS-ADDONFREIGHT
              MOVE INCR-ADDLABOUR   TO WS-HANDADDON
              MOVE INCR-ADDMISC     TO WS-MISCADDON
              MOVE INCR-SB-TYPE     TO Ws-Sold-By
              COMPUTE WS-SUBTOTAL =
                   WS-INVOICETOTAL - (WS-TAXAMT + WS-ADDONAMT).
              MOVE INCR-NAME        TO WS-NAME
              MOVE INCR-ADD1        TO WS-ADD1
              MOVE INCR-ADD2        TO WS-ADD2
              MOVE INCR-ADD3        TO WS-ADD3
              MOVE INCR-CODE        TO WS-POSTCODE
              MOVE INCR-DEL1        TO WS-DELADD1
              MOVE INCR-DEL2        TO WS-DELADD2
              MOVE INCR-DEL3        TO WS-DELADD3
              MOVE INCR-TERMS       TO WS-TERMOFSALE
              MOVE INCR-PORDER      TO WS-PORDERNO
              MOVE INCR-DELIVERY    TO WS-DELIVERVIA
              MOVE INCR-BIN         TO WS-BINNO
              MOVE INCR-COMMENT     TO WS-COMMENTLINE
              MOVE INCR-LINENO      TO SUB-20.
              
              MOVE INCR-INVOICE     TO WS-INVOICE-SAVE
              MOVE INCR-DATE        TO WS-DATE-SAVE.
       RIR-999.
           EXIT.
      *
       READ-INVOICE-REGISTERLY SECTION.
       RIRLY-000.
           MOVE WS-INVOICE TO INCR-LY-INVOICE.
           MOVE 1          TO INCR-LY-TRANS.
           START INCR-LY-REGISTER KEY NOT < INCR-LY-KEY
               INVALID KEY NEXT SENTENCE.
       RIRLY-005.
           READ INCR-LY-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-LY-ST1 = 23 OR 35 OR 49
               MOVE "NO SUCH INVOICE TO READ, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-LY-ST1
               MOVE 0 TO INCR-LY-INVOICE
               GO TO RIRLY-999.
           IF WS-INCR-LY-ST1 NOT = 0
               MOVE "INV/CR. REGLY BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-LY-ST1
               GO TO RIRLY-005.
       RIRLY-010.
              MOVE INCR-LY-ACCOUNT     TO WS-ACCOUNT-NUMBER
              MOVE INCR-LY-GSTNO       TO WS-GSTNO
              MOVE WS-DATE             TO WS-INVOICEDATE
              MOVE INCR-LY-SALES       TO WSAN-CODE
              MOVE INCR-LY-INVCRED-AMT TO WS-INVOICETOTAL
              MOVE INCR-LY-TAX         TO WS-TAXAMT
              MOVE INCR-LY-ADDONS      TO WS-ADDONAMT
              MOVE INCR-LY-ADDPOST     TO WS-POSTADDON
              MOVE INCR-LY-ADDFREIGHT  TO WS-ADDONFREIGHT
              MOVE INCR-LY-ADDLABOUR   TO WS-HANDADDON
              MOVE INCR-LY-ADDMISC     TO WS-MISCADDON
              MOVE INCR-LY-SB-TYPE     TO Ws-Sold-By
              COMPUTE WS-SUBTOTAL =
                   WS-INVOICETOTAL - (WS-TAXAMT + WS-ADDONAMT).
              MOVE INCR-LY-NAME        TO WS-NAME
              MOVE INCR-LY-ADD1        TO WS-ADD1
              MOVE INCR-LY-ADD2        TO WS-ADD2
              MOVE INCR-LY-ADD3        TO WS-ADD3
              MOVE INCR-LY-CODE        TO WS-POSTCODE
              MOVE INCR-LY-DEL1        TO WS-DELADD1
              MOVE INCR-LY-DEL2        TO WS-DELADD2
              MOVE INCR-LY-DEL3        TO WS-DELADD3
              MOVE INCR-LY-TERMS       TO WS-TERMOFSALE
              MOVE INCR-LY-PORDER      TO WS-PORDERNO
              MOVE INCR-LY-DELIVERY    TO WS-DELIVERVIA
              MOVE INCR-LY-BIN         TO WS-BINNO
              MOVE INCR-LY-COMMENT     TO WS-COMMENTLINE
              MOVE INCR-LY-LINENO      TO SUB-20.
              
              MOVE INCR-LY-INVOICE     TO WS-INVOICE-SAVE
              MOVE INCR-LY-DATE        TO WS-DATE-SAVE.
       RIRLY-999.
           EXIT.
      *
       READ-STOCK-TRANSACTIONS SECTION.
       RSTT-000.
           MOVE 1            TO SUB-1.
           MOVE WS-INVOICE   TO STTR-REFERENCE1
           MOVE 1            TO STTR-TYPE
           MOVE 0            TO STTR-TRANSACTION-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-KEY
              INVALID KEY NEXT SENTENCE.
       RSTT-010.
           READ STOCK-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-STTRANS-ST1 = 10
              MOVE 0 TO STTR-TYPE
              GO TO RSTT-900.
           IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS BUSY ON INVOICE READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE STTR-STOCK-NUMBER TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-STTRANS-ST1
              GO TO RSTT-010.
           IF STTR-REFERENCE1 NOT = WS-INVOICE
              GO TO RSTT-900.
           IF STTR-TYPE NOT = 1
              GO TO RSTT-010.
           IF STTR-SHIPQTY NOT > 0
               GO TO RSTT-010.

           MOVE STTR-STOCK-NUMBER    TO B-STOCKNUMBER (SUB-1)
                                       SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               GO TO RSTT-020.
               
           MOVE STTR-DESC1        TO B-STOCKDESCRIPTION (SUB-1)
           MOVE STTR-DESC2        TO B-STOCKDESCRIPTION2 (SUB-1)
           MOVE STTR-SHIPQTY      TO B-SHIPQTY (SUB-1)
           MOVE STTR-PRICE        TO B-STOCKPRICE (SUB-1)
           MOVE STTR-COST-VALUE   TO B-STOCKCOST (SUB-1)
           MOVE STTR-ITEMDISC     TO B-DISCOUNTPERITEM (SUB-1)
           MOVE STTR-SALES-VALUE  TO B-NETT (SUB-1)
           MOVE STTR-TAX          TO B-TAX (SUB-1)
           COMPUTE WS-COSTTOTAL = WS-COSTTOTAL +
               (B-STOCKCOST (SUB-1) * B-SHIPQTY (SUB-1)).
           GO TO RSTT-050.
       RSTT-020.
           MOVE COM-SHIPQTY       TO C-SHIP (SUB-1)
           MOVE COM-DESC          TO C-DESC (SUB-1)
           MOVE COM-UNIT          TO C-UNIT (SUB-1)
           MOVE COM-PRICE         TO C-PRICE (SUB-1)
           MOVE COM-COST          TO C-COST (SUB-1)
           MOVE COM-DISC          TO C-DISC (SUB-1).
       RSTT-050.
           ADD 1 TO SUB-1.
           IF SUB-1 > 200
              GO TO RSTT-900.
           GO TO RSTT-010.
       RSTT-900.
           MOVE SUB-1 TO SUB-25.
       RSTT-999.
           EXIT.
      *
       READ-STOCK-TRANSACTIONSLY SECTION.
       RSTTLY-000.
           MOVE 1            TO SUB-1.
           MOVE WS-INVOICE   TO STTR-LY-REFERENCE1
           MOVE 1            TO STTR-LY-TYPE
           MOVE 0            TO STTR-LY-TRANSACTION-NUMBER.
           START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-KEY
              INVALID KEY NEXT SENTENCE.
       RSTTLY-010.
           READ STOCK-TRANSLY-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-STTRANSLY-ST1 = 10
              MOVE 0 TO STTR-LY-TYPE
              GO TO RSTTLY-900.
           IF WS-STTRANSLY-ST1 NOT = 0
              MOVE 
              "STOCK TRANSLY RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANSLY-ST1
              GO TO RSTTLY-010.
           IF STTR-LY-REFERENCE1 NOT = WS-INVOICE
              GO TO RSTTLY-900.
           IF STTR-LY-TYPE NOT = 1
              GO TO RSTTLY-010.
           IF STTR-LY-SHIPQTY NOT > 0
               GO TO RSTTLY-010.

           MOVE STTR-LY-STOCK-NUMBER    TO B-STOCKNUMBER (SUB-1)
                                       SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               GO TO RSTTLY-020.
           IF STTR-LY-SHIPQTY = 0
               GO TO RSTTLY-010.
               
           MOVE STTR-LY-DESC1        TO B-STOCKDESCRIPTION (SUB-1)
           MOVE STTR-LY-DESC2        TO B-STOCKDESCRIPTION2 (SUB-1)
           MOVE STTR-LY-SHIPQTY      TO B-SHIPQTY (SUB-1)
           MOVE STTR-LY-PRICE        TO B-STOCKPRICE (SUB-1)
           MOVE STTR-LY-COST-VALUE   TO B-STOCKCOST (SUB-1)
           MOVE STTR-LY-ITEMDISC     TO B-DISCOUNTPERITEM (SUB-1)
           MOVE STTR-LY-SALES-VALUE  TO B-NETT (SUB-1)
           MOVE STTR-LY-TAX          TO B-TAX (SUB-1)
           COMPUTE WS-COSTTOTAL = WS-COSTTOTAL +
               (B-STOCKCOST (SUB-1) * B-SHIPQTY (SUB-1)).
           GO TO RSTTLY-050.
       RSTTLY-020.
           MOVE COM-LY-SHIPQTY       TO C-SHIP (SUB-1)
           MOVE COM-LY-DESC          TO C-DESC (SUB-1)
           MOVE COM-LY-UNIT          TO C-UNIT (SUB-1)
           MOVE COM-LY-PRICE         TO C-PRICE (SUB-1)
           MOVE COM-LY-COST          TO C-COST (SUB-1)
           MOVE COM-LY-DISC          TO C-DISC (SUB-1).
       RSTTLY-050.
           ADD 1 TO SUB-1.
           IF SUB-1 > 200
              GO TO RSTTLY-900.
           GO TO RSTTLY-010.
       RSTTLY-900.
           MOVE SUB-1 TO SUB-25.
       RSTTLY-999.
           EXIT.
      *
       FIND-INFO SECTION.
       FIND-010.
            MOVE "CREDITNUM"      TO F-FIELDNAME
            MOVE 9                TO F-CBFIELDNAME
            MOVE WS-INVOICE       TO F-EDNAMEFIELDNUM
            MOVE 6                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC.
 
            MOVE "ACCOUNTNO"       TO F-FIELDNAME
            MOVE 9                 TO F-CBFIELDNAME
            MOVE WS-ACCOUNT-NUMBER TO F-NAMEFIELD
            MOVE 7                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEBTORNAME" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE WS-NAME      TO F-NAMEFIELD
            MOVE 40           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD1" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE WS-ADD1    TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD2" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE WS-ADD2    TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD3" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE WS-ADD3    TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTCODE"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-POSTCODE TO F-NAMEFIELD
            MOVE 4           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD1"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-DELADD1 TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD2"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-DELADD2 TO F-NAMEFIELD
            MOVE 25         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD3"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            MOVE WS-DELADD3  TO F-NAMEFIELD
            MOVE 25          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POORDERNO"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE WS-PORDERNO TO F-NAMEFIELD
            MOVE 20           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE WSAN-CODE        TO SA-KEY
            PERFORM READ-SALES-ANALYSIS
            MOVE "SALESANALYSIS"  TO F-FIELDNAME
            MOVE 13               TO F-CBFIELDNAME
            MOVE WS-SALESANALYSIS TO F-NAMEFIELD
            MOVE 14               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELIVERVIA"  TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE WS-DELIVERVIA TO F-NAMEFIELD
            MOVE 20            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TERMOFSALE"  TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE WS-TERMOFSALE TO F-NAMEFIELD
            MOVE 11            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GSTNO"  TO F-FIELDNAME
            MOVE 5        TO F-CBFIELDNAME
            MOVE WS-GSTNO TO F-NAMEFIELD
            MOVE 13       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BINNO"  TO F-FIELDNAME
            MOVE 5        TO F-CBFIELDNAME
            MOVE WS-BINNO TO F-NAMEFIELD
            MOVE 6        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CREDITDATE"   TO F-FIELDNAME
            MOVE 10             TO F-CBFIELDNAME
            MOVE WS-DATE        TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE   TO F-NAMEFIELD
            MOVE 10             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SOLDBY"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            MOVE Ws-Sold-By TO F-NAMEFIELD
            MOVE 2         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            IF SUB-1 NOT > 0
                 MOVE 1 TO SUB-1
            PERFORM SCROLL-NEXT
            PERFORM SCROLL-PREVIOUS.

            MOVE "COMMENTLINE"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE WS-COMMENTLINE TO F-NAMEFIELD
            MOVE 30             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADDONFREIGHT"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE WS-ADDONFREIGHT TO F-EDNAMEFIELDAMOUNT
            MOVE 9               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "POSTADDON"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE WS-POSTADDON TO F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "HANDADDON"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE WS-HANDADDON TO F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "MISC.ADDON" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE WS-MISCADDON TO F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "SUBTOTAL"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-SUBTOTAL TO F-EDNAMEFIELDAMOUNT
            MOVE 9           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "ADDONAMT"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-ADDONAMT TO F-EDNAMEFIELDAMOUNT
            MOVE 9           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "TAXAMT"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            MOVE WS-TAXAMT TO F-EDNAMEFIELDAMOUNT
            MOVE 9         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "CREDITTOTAL"   TO F-FIELDNAME
            MOVE 11              TO F-CBFIELDNAME
            MOVE WS-INVOICETOTAL TO F-EDNAMEFIELDAMOUNT
            MOVE 9               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.
       FIND-999.  
            EXIT.
     *
       CHECK-SUB1-TOTAL SECTION.
       CHK-000.
            MOVE 1 TO SUB-1.
       CHK-010.
            IF SUB-1 < 200
               IF B-STOCKNUMBER (SUB-1) NOT = " "
               ADD 1 TO SUB-1
               MOVE SUB-1 TO SUB-20
               GO TO CHK-010.
       CHK-999.
            EXIT.
      *
       FILL-COMMENT SECTION.
       COMM-A.
            MOVE " " TO C-SHIP (SUB-1)
                        C-DESC (SUB-1)
                        C-PRICE (SUB-1)
                        C-COST (SUB-1)
                        C-DISC (SUB-1).
       COMM-010.
            MOVE "SHIPQTY" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO COMM-999.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO C-SHIP (SUB-1).
       COMM-020.
            MOVE "STOCKDESCRIPTION" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO COMM-010.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO C-DESC (SUB-1).
       COMM-030.
            MOVE "STOCKPRICE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO COMM-020.
            MOVE 8 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO C-PRICE (SUB-1).
       COMM-040.
            MOVE "STOCKCOST" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO COMM-030.
            MOVE 8 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO C-COST (SUB-1).
       COMM-050.
            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME.
            MOVE 15 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO COMM-040.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO C-DISC (SUB-1).
            MOVE " " TO C-REST (SUB-1).
       COMM-999.
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
                MOVE 0 TO B-DISCOUNTPERITEM (SUB-1).
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
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-010.
            PERFORM SCROLLING.
       PREV-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 200   
                GO TO PREV-030.
            IF B-STOCKNUMBER (SUB-1) = " "
                MOVE " " TO B-TAX (SUB-1)
                MOVE 0 TO B-DISCOUNTPERITEM (SUB-1).
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
       SCROLL-010.
           MOVE "STOCKNUMBER"         TO F-FIELDNAME
           MOVE 11                    TO F-CBFIELDNAME
           MOVE B-STOCKNUMBER (SUB-1) TO F-NAMEFIELD
           MOVE 15                    TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
              PERFORM SCROLL-COMMENT
              GO TO SCROLL-999.

           MOVE "SHIPQTY"         TO F-FIELDNAME
           MOVE 7                 TO F-CBFIELDNAME
           MOVE 5                 TO F-CBFIELDLENGTH
           IF B-STOCKNUMBER (SUB-1) NOT = " "
               MOVE B-SHIPQTY (SUB-1) TO F-EDNAMEFIELDQTY
               PERFORM WRITE-FIELD-QTY
           ELSE
              MOVE " " TO F-NAMEFIELD
              PERFORM WRITE-FIELD-ALPHA.

           MOVE "STOCKDESCRIPTION"         TO F-FIELDNAME
           MOVE 16                         TO F-CBFIELDNAME
           MOVE B-STOCKDESCRIPTION (SUB-1) TO F-NAMEFIELD
           MOVE 20                         TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

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
       SCCO-000.
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
            MOVE 8               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "STOCKCOST"    TO F-FIELDNAME
            MOVE 9              TO F-CBFIELDNAME
            MOVE C-COST (SUB-1) TO F-NAMEFIELD
            MOVE 8              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DISCOUNTPERITEM" TO F-FIELDNAME
            MOVE 15                TO F-CBFIELDNAME
            MOVE C-DISC (SUB-1)    TO F-NAMEFIELD
            MOVE 5                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       SCCO-999.
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
       CLEAR-BOTTOM-FIELDS SECTION.
       CBF-000.
            MOVE "ADDONFREIGHT" TO F-FIELDNAME
            MOVE 12             TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "POSTADDON"    TO F-FIELDNAME
            MOVE 9              TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "HANDADDON"    TO F-FIELDNAME
            MOVE 9              TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "MISC.ADDON"   TO F-FIELDNAME
            MOVE 10             TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "SUBTOTAL"     TO F-FIELDNAME
            MOVE 8              TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ADDONAMT"     TO F-FIELDNAME
            MOVE 8              TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "TAXAMT"       TO F-FIELDNAME
            MOVE 6              TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "CREDITTOTAL"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       CBF-999.
            EXIT.
      *
       CANCEL-INVOICE SECTION.
       CI-000.
             MOVE 1 TO SUB-1.
       CI-010.
             IF B-STOCKNUMBER (SUB-1) = " "
                IF B-SHIPQTY (SUB-1) = 0
                 GO TO CI-900.
             PERFORM CANCEL-TRANSACTION.
             MOVE 1 TO SUB-1.
             GO TO CI-010.
       CI-900.
             UNLOCK INCR-REGISTER.
             UNLOCK STOCK-TRANS-FILE.
             PERFORM CLEAR-FIELDS.
             PERFORM DISPLAY-FORM.
             MOVE 2801 TO POS.
             MOVE " " TO WS-MESSAGE.
             DISPLAY WS-MESSAGE AT POS.
       CI-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
           MOVE 1 TO SUB-1.
       CF-010.
           MOVE " " TO C-LINE (SUB-1)
                       B-STOCKNUMBER (SUB-1)
                       B-STOCKDESCRIPTION (SUB-1)
                       B-STOCKDESCRIPTION2 (SUB-1)
                       B-UNIT (SUB-1)
                       B-TAX (SUB-1).
           MOVE 0 TO B-SHIPQTY (SUB-1)
                     B-STOCKPRICE (SUB-1)
                     B-STOCKCOST (SUB-1)
                     B-DISCOUNTPERITEM (SUB-1)
                     B-NETT (SUB-1).
           ADD 1 TO SUB-1.
           IF SUB-1 < 201
               GO TO CF-010.
       CF-999.
           EXIT.
      *
       CANCEL-TRANSACTION SECTION.
       CAN-000.
            COMPUTE SUB-2 = SUB-1 + 1.
            MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
            IF SP-1STCHAR = "*" OR = "/"
                GO TO CAN-010.
                
            GO TO CAN-010.
      **************************************************************
      * CHANGED 23/01/2005                                         *
      * SECTION BELOW REMOVED AS NO QTY'S ARE WRITTEN TO STOCK IN  *
      * THE FILL SECTION ANYMORE                                   *
      **************************************************************
                
            MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER.
            PERFORM READ-STOCK-LOCK.
            IF ST-STOCKNUMBER = " "
               MOVE " " TO B-STOCKNUMBER (SUB-1)
               GO TO CAN-999.
            SUBTRACT B-SHIPQTY (SUB-1) FROM ST-QTYONHAND.
       CAN-001.
            REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "STOCK RECORD:"       TO WS-DAILY-1ST
               MOVE B-STOCKNUMBER (SUB-1) TO WS-DAILY-2ND
               MOVE "NOT UPDATED "        TO WS-DAILY-3RD
               MOVE "ON CANCEL OF TRANS." TO WS-DAILY-4TH
               PERFORM WRITE-DAILY
               GO TO CAN-010.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO CAN-001.
       CAN-010.
           IF SUB-2 > 200
               GO TO CAN-090.
           IF B-STOCKNUMBER (SUB-2) = " "
                 MOVE " " TO C-LINE (SUB-1)
                 MOVE " " TO B-STOCKNUMBER (SUB-1)
                             B-STOCKDESCRIPTION (SUB-1)
                             B-STOCKDESCRIPTION2 (SUB-1)
                             B-TAX (SUB-1)
                 MOVE 0   TO B-SHIPQTY (SUB-1)
                             B-STOCKPRICE (SUB-1)
                             B-STOCKCOST (SUB-1)
                             B-DISCOUNTPERITEM (SUB-1)
                             B-NETT (SUB-1)
                 GO TO CAN-090.
           MOVE C-LINE (SUB-2)    TO C-LINE (SUB-1).
           MOVE BODY-LINE (SUB-2) TO BODY-LINE (SUB-1).
           ADD 1 TO SUB-1 SUB-2.
           GO TO CAN-010.
       CAN-090.
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
       CAN-999.
           EXIT.
      *
       RUNNING-TOTAL SECTION.
       RUN-000.
             MOVE 1 TO SUB-3.
             MOVE 0 TO WS-WORKTOTAL
                       WS-WORKTOTAL2.
       RUN-010.
             IF B-STOCKNUMBER (SUB-3) = " "
                GO TO RUN-020.
             MOVE B-STOCKNUMBER (SUB-3) TO SPLIT-STOCK.
             IF SP-1STCHAR = "*"
                 GO TO RUN-015.
             COMPUTE WS-WORKTOTAL = (B-SHIPQTY (SUB-3) * 
                 B-STOCKPRICE (SUB-3)).
             COMPUTE WS-DISCOUNT ROUNDED = (B-SHIPQTY (SUB-3) * 
              B-STOCKPRICE (SUB-3)) * B-DISCOUNTPERITEM (SUB-3) / 100.
             SUBTRACT WS-DISCOUNT FROM WS-WORKTOTAL.
             IF B-TAX (SUB-3) = "Y"
               COMPUTE WS-WORKTOTAL ROUNDED = 
               (WS-WORKTOTAL + (WS-WORKTOTAL * WS-GST-PERCENT / 100)).
             ADD WS-WORKTOTAL TO WS-WORKTOTAL2.
             IF WS-WORKTOTAL2 > 999999.99
              MOVE "** YOUR CREDIT VALUE HAS EXCEEDED R999 999.99 **"
              TO WS-MESSAGE
              PERFORM ERROR-000
              MOVE " **   YOU MUST CANCEL YOUR LAST STOCK LINE ! **"
              TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              MOVE " " TO WS-MESSAGE
              PERFORM ERROR-020
              SUBTRACT 1 FROM SUB-1
              IF F-INDEX > 1
                SUBTRACT 1 FROM F-INDEX
                PERFORM SCROLLING
              ELSE
                PERFORM SCROLLING.
       RUN-015.
             ADD 1 TO SUB-3.
             IF SUB-3 > 200
                GO TO RUN-020.
             GO TO RUN-010.
       RUN-020.
             MOVE "SUBTOTAL"    TO F-FIELDNAME
             MOVE 8             TO F-CBFIELDNAME
             MOVE WS-WORKTOTAL2 TO F-EDNAMEFIELDAMOUNT
             MOVE 9             TO F-CBFIELDLENGTH
             PERFORM WRITE-FIELD-AMOUNT.
       RUN-999.
             EXIT.
      *
       CALCULATE-TOTALS SECTION.
       CT-000.
           MOVE 1 TO SUB-1.
           MOVE 0 TO WS-TAXABLETOTAL
                     WS-NONTAXABLETOTAL
                     WS-WORKTOTAL
                     WS-DISCOUNT
                     WS-COSTTOTAL 
                     WS-PRICETOTAL
                     WS-EXPORTTOTAL
                     WS-DISCOUNTREG.
       CT-010.
           IF B-STOCKNUMBER (SUB-1) = " "
               GO TO CT-020.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               GO TO CT-015.
           COMPUTE WS-WORKTOTAL = B-SHIPQTY (SUB-1) * 
                               B-STOCKPRICE (SUB-1).
           COMPUTE WS-DISCOUNT ROUNDED = WS-WORKTOTAL * 
                               B-DISCOUNTPERITEM (SUB-1) / 100.
           ADD WS-DISCOUNT TO WS-DISCOUNTREG.
           SUBTRACT WS-DISCOUNT FROM WS-WORKTOTAL.
           COMPUTE WS-COSTTOTAL = (WS-COSTTOTAL +
                 B-STOCKCOST (SUB-1) * B-SHIPQTY (SUB-1)).
           IF WS-GSTNO NOT = "EXPORT"
      *      IF B-TAX (SUB-1) = "Y"
               ADD WS-WORKTOTAL TO WS-TAXABLETOTAL
            ELSE
      *         ADD WS-WORKTOTAL TO WS-NONTAXABLETOTAL.
      *     IF WS-GSTNO = "EXPORT"
               ADD WS-WORKTOTAL TO WS-EXPORTTOTAL.
           ADD WS-WORKTOTAL TO WS-PRICETOTAL.
           MOVE WS-WORKTOTAL TO B-NETT (SUB-1).
       CT-015.
           ADD 1 TO SUB-1.
           IF SUB-1 > 200
               GO TO CT-020.
           IF SUB-1 < 201
               GO TO CT-010.
       CT-020.
           COMPUTE WS-SUBTOTAL = WS-TAXABLETOTAL +
                                 WS-NONTAXABLETOTAL +
                                 WS-EXPORTTOTAL.
      *     COMPUTE WS-TAXAMT ROUNDED = WS-TAXABLETOTAL *
      *                                   WS-GST-PERCENT / 100.
           COMPUTE WS-ADDONAMT = WS-ADDONFREIGHT + WS-POSTADDON +
                                   WS-HANDADDON + WS-MISCADDON.
           COMPUTE WS-TAXAMT ROUNDED =
               (WS-TAXABLETOTAL + WS-ADDONAMT) * WS-GST-PERCENT / 100.
       CT-999.
             EXIT.
      *
       SET-GST SECTION.
       SG-000.
            MOVE 1 TO SUB-1.
       SG-010.
            IF WS-GSTNO NOT = "EXPORT" AND NOT = "EXPORT       "
               MOVE "Y" TO B-TAX (SUB-1)
            ELSE
               MOVE "N" TO B-TAX (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 201
               GO TO SG-010.
       SG-999.
            EXIT.
      *
       WRITE-DEBTOR-TRANSACTIONS SECTION.
       WRTR-000.
            PERFORM READ-PARAMETER-LOCK.
            MOVE PA-DRTRANS-NUMBER TO DRTR-TRANSACTION-NUMBER
                                      WS-DRTRANS-NO.
            ADD 1 TO PA-DRTRANS-NUMBER.
            PERFORM REWRITE-PARAMETER.
            MOVE 6                 TO DRTR-TYPE
            MOVE WS-ACCOUNT-NUMBER TO DRTR-ACCOUNT-NUMBER
            MOVE WS-PORDERNO       TO DRTR-REFERENCE1
            MOVE WS-INVOICE        TO DRTR-REFERENCE2
            MOVE WS-INVOICEDATE    TO DRTR-DATE
                                      DRTR-DEL-DATE
            MOVE WS-INVOICETOTAL   TO DRTR-AMT-OF-INVOICE
                                      DRTR-AMT-OUTSTANDING.
       WRTR-005.
            WRITE DEBTOR-TRANS-REC
                INVALID KEY
                NEXT SENTENCE.
            IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
                MOVE "DR.TRANS BUSY ON WRITE-23, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DRTRANS-ST1
                GO TO WRTR-000.
            IF WS-DRTRANS-ST1 NOT = 0
                MOVE "DR.TRANS BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DRTRANS-ST1
                GO TO WRTR-005.
       WRTR-999.
            EXIT.
      *
       WRITE-INCR-REGISTER SECTION.
       WRIC-000.
            PERFORM ERROR-020.
       WRIC-050.
            MOVE WS-INVOICE        TO INCR-INVOICE
            MOVE 6                 TO INCR-TRANS
            MOVE WS-ACCOUNT-NUMBER TO INCR-ACCOUNT
            MOVE WS-GSTNO          TO INCR-GSTNO
            MOVE WS-INVOICEDATE    TO INCR-DATE
            MOVE WSAN-CODE         TO INCR-SALES
            MOVE WS-INVOICETOTAL   TO INCR-INVCRED-AMT
            MOVE WS-TAXAMT         TO INCR-TAX
            MOVE WS-ADDONAMT       TO INCR-ADDONS
            MOVE WS-DISCOUNTREG    TO INCR-DISCOUNT
            MOVE WS-COSTTOTAL      TO INCR-INVCRED-COST
            MOVE Ws-Sold-By        TO INCR-SB-TYPE
            MOVE WS-DRTRANS-NO     TO INCR-DRTRANS-NO
            MOVE "Y"               TO INCR-PART-ORDERS.
            MOVE "$$"              TO INCR-PULLBY.
            MOVE 1                 TO INCR-COPY-NUMBER.
            MOVE "N"               TO INCR-PRINTED
            MOVE "X"               TO INCR-AREA.
     
            ACCEPT WS-TIME FROM TIME.
            MOVE WS-DATE           TO INCR-PULL-DATE
            MOVE WS-TIME           TO INCR-PULL-TIME.
            
            MOVE WS-NAME           TO INCR-NAME
            MOVE WS-ADD1           TO INCR-ADD1
            MOVE WS-ADD2           TO INCR-ADD2
            MOVE WS-ADD3           TO INCR-ADD3
            MOVE WS-POSTCODE       TO INCR-CODE
            MOVE WS-DELADD1        TO INCR-DEL1
            MOVE WS-DELADD2        TO INCR-DEL2
            MOVE WS-DELADD3        TO INCR-DEL3
            MOVE "**CREDIT**"      TO INCR-TERMS
            MOVE WS-PORDERNO       TO INCR-PORDER
            MOVE WS-DELIVERVIA     TO INCR-DELIVERY
            MOVE WS-BINNO          TO INCR-BIN
            MOVE WS-COMMENTLINE    TO INCR-COMMENT
            MOVE WS-INVOICE-SAVE   TO INCR-BO-INV-NO
            MOVE WS-DATE-SAVE      TO INCR-BO-DATE
            MOVE WS-POSTADDON      TO INCR-ADDPOST
            MOVE WS-ADDONFREIGHT   TO INCR-ADDFREIGHT
            MOVE WS-HANDADDON      TO INCR-ADDLABOUR
            MOVE WS-MISCADDON      TO INCR-ADDMISC
            MOVE SUB-20            TO INCR-LINENO.
       WRIC-010.
            WRITE INCR-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 = 0
                PERFORM ERROR-020
                GO TO WRIC-999.
            IF WS-INCR-ST1 NOT = 0
                MOVE "INV/CR. TRANS BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-INCR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-INCR-ST1
                GO TO WRIC-010.
       WRIC-999.
              EXIT.
      *
       WRITE-STOCK-TRANSACTIONS SECTION.
       WST-000.
            MOVE 1          TO SUB-1
            MOVE 6          TO STTR-TYPE
            MOVE WS-INVOICE TO STTR-REFERENCE1.
            MOVE 0          TO WS-STTRANS-NO.
            START STOCK-TRANS-FILE KEY NOT < STTR-KEY
                INVALID KEY NEXT SENTENCE.
            IF B-STOCKNUMBER (SUB-1) = " "
               GO TO WST-999.
       WST-005.
            MOVE B-STOCKNUMBER (SUB-1)       TO SPLIT-STOCK
            ADD 1                            TO WS-STTRANS-NO
            MOVE WS-STTRANS-NO               TO STTR-TRANSACTION-NUMBER
            MOVE B-STOCKNUMBER (SUB-1 )      TO SPLIT-STOCK.
       WST-010.
            MOVE WS-INVOICE                  TO STTR-REFERENCE1
            MOVE B-STOCKNUMBER (SUB-1)       TO STTR-STOCK-NUMBER
            MOVE WS-ACCOUNT-NUMBER           TO STTR-ACCOUNT-NUMBER
            MOVE WS-INVOICEDATE              TO STTR-DATE
                                                STTR-AC-DATE
                                                STTR-ST-DATE
            MOVE "Y"                         TO STTR-COMPLETE
                                                STTR-ST-COMPLETE
                                                STTR-AC-COMPLETE.
            IF SP-1STCHAR = "*"
                GO TO WST-015.
            MOVE 0                           TO STTR-ORDERQTY
            MOVE B-SHIPQTY (SUB-1)           TO STTR-SHIPQTY
            MOVE B-NETT (SUB-1)              TO STTR-SALES-VALUE
            MOVE B-STOCKPRICE (SUB-1)        TO STTR-PRICE
            MOVE B-STOCKDESCRIPTION (SUB-1)  TO STTR-DESC1
            MOVE B-STOCKDESCRIPTION2 (SUB-1) TO STTR-DESC2
            MOVE B-DISCOUNTPERITEM (SUB-1)   TO STTR-ITEMDISC
            MOVE B-STOCKCOST (SUB-1)         TO STTR-COST-VALUE.
            IF B-UNIT (SUB-1) NOT > " " 
                MOVE "EACH"                  TO STTR-UNIT
            ELSE
                MOVE B-UNIT (SUB-1)          TO STTR-UNIT.
            IF B-TAX (SUB-1) NOT > " " 
                MOVE "Y"                     TO STTR-TAX
            ELSE
                MOVE B-TAX (SUB-1)           TO STTR-TAX.
            GO TO WST-018.
       WST-015.
            MOVE " "                         TO COM-ORDERQTY
            MOVE C-SHIP (SUB-1)              TO COM-SHIPQTY
            MOVE C-DESC (SUB-1)              TO COM-DESC
            MOVE C-PRICE (SUB-1)             TO COM-PRICE
            MOVE C-COST (SUB-1)              TO COM-COST
            MOVE C-DISC (SUB-1)              TO COM-DISC
            MOVE " "                         TO COM-FILLER.
       WST-018.
            WRITE STOCK-TRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-STTRANS-ST1 NOT = 0
              MOVE "ST-TRANS BUSY ON WRITE WST-018, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE B-STOCKNUMBER (SUB-1)  TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE SUB-1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO WST-018.
       WST-020.
            ADD 1 TO SUB-1.
            IF SUB-1 < 101
              IF B-STOCKNUMBER (SUB-1) = " "
               GO TO WST-999.
            IF SUB-1 < 101
               GO TO WST-005.
       WST-999.
            EXIT.
      *
       READ-DEBTORS-LOCK SECTION.
       RDL-000.
           MOVE WS-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
           READ DEBTOR-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "NO SUCH DEBTOR TO UPDATE" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RDL-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RDL-000.
       RDL-999.
           EXIT.
      *
       UPDATE-DEBTOR SECTION.
       UPDR-000.
            PERFORM READ-DEBTORS-LOCK.
            SUBTRACT WS-INVOICETOTAL FROM DR-BALANCE
                                          DR-CURRENT.
            SUBTRACT WS-COSTTOTAL FROM DR-COST-PTD
                                       DR-COST-YTD.
            SUBTRACT WS-PRICETOTAL FROM DR-SALES-PTD
                                        DR-SALES-YTD.
       UPDR-900.
            REWRITE DEBTOR-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE "ACCOUNT NUMBER:" TO WS-DAILY-1ST
                MOVE DR-ACCOUNT-NUMBER TO WS-DAILY-2ND
                MOVE "NOT UPDATED"     TO WS-DAILY-3RD
                MOVE WS-INVOICE        TO WS-DAILY-4TH
                PERFORM WRITE-DAILY
                GO TO UPDR-999.
            IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR BUSY ON REWRITE, 'ESC' TO RETRY"
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
       UPDATE-DISTRIBUTION SECTION.
       UPDIS-005.
           MOVE "1" TO DIST-KEY.
       UPDIS-010.
           READ DISTRIBUTIONS WITH LOCK
              INVALID KEY NEXT SENTENCE.
           IF WS-DISTRIBUTION-ST1 = 23 OR 35 OR 49
              MOVE "DISTRIBUTIONS NOT THERE, CALL YOUR SUPERVISOR." 
                   TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO UPDIS-900.
           IF WS-DISTRIBUTION-ST1 NOT = 0 
              MOVE "DISTRIBUTIONS BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DISTRIBUTION-ST1
              GO TO UPDIS-010.

           SUBTRACT WS-DIST-ADDON FROM DIST-ADDONWEEK
                                       DIST-ADDONPTD
                                       DIST-ADDONYTD.
           ADD WS-SUBTOTAL TO DIST-CNOTEWEEK
                              DIST-CNOTEPTD
                              DIST-CNOTEYTD.
           SUBTRACT WS-DIST-INVOICE FROM DIST-ACCRECWEEK
                                         DIST-ACCRECPTD
                                         DIST-ACCRECYTD.
           SUBTRACT WS-GST-AMT-TAXED FROM GST-AMT-TAXED-WEEK
                                          GST-AMT-TAXED-PTD
                                          GST-AMT-TAXED-YTD.
           SUBTRACT WS-GST-AMT-TAXABLE FROM GST-AMT-TAXABLE-WEEK
                                            GST-AMT-TAXABLE-PTD
                                            GST-AMT-TAXABLE-YTD.
           SUBTRACT WS-GST-AMT-NONTAXABLE FROM GST-AMT-NONTAXABLE-WEEK
                                               GST-AMT-NONTAXABLE-PTD
                                               GST-AMT-NONTAXABLE-YTD.
           SUBTRACT WS-GST-AMT-EXPORT FROM GST-AMT-EXPORT-WEEK
                                           GST-AMT-EXPORT-PTD
                                           GST-AMT-EXPORT-YTD.
       UPDIS-500.
           REWRITE DIST-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-DISTRIBUTION-ST1 = 23 OR 35 OR 49
              MOVE "DISTRIBUTIONS NOT UPDATED, CALL YOUR SUPERVISOR"
                   TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO UPDIS-900.
           IF WS-DISTRIBUTION-ST1 NOT = 0 
              MOVE "DISTRIBUTIONS BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DISTRIBUTION-ST1
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
            IF B-SHIPQTY (SUB-1) = 0
                GO TO UPST-999.
           MOVE B-STOCKNUMBER (SUB-1) TO SPLIT-STOCK.
           IF SP-1STCHAR = "*" OR = "/"
                GO TO UPST-520.
           MOVE B-STOCKNUMBER (SUB-1) TO WS-STOCKNUMBER.
           PERFORM READ-STOCK-LOCK.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "STOCK RECORD:"       TO WS-DAILY-1ST
               MOVE B-STOCKNUMBER (SUB-1) TO WS-DAILY-2ND
               MOVE "NOT FOUND    "       TO WS-DAILY-3RD
               MOVE WS-INVOICE            TO WS-DAILY-4TH
               PERFORM WRITE-DAILY
              GO TO UPST-550.
      *
      * ACTUAL UPDATE OF STOCK FILE.
      *
           PERFORM REAVERAGE-COST.

           IF ST-QTYONRESERVE < ST-QTYONBORDER
                COMPUTE WS-QTY = (ST-QTYONBORDER - ST-QTYONRESERVE)
            IF WS-QTY > B-SHIPQTY (SUB-1)
                ADD B-SHIPQTY (SUB-1) TO ST-QTYONRESERVE
                GO TO UPST-045
            ELSE
                ADD WS-QTY            TO ST-QTYONRESERVE
                SUBTRACT WS-QTY      FROM B-SHIPQTY (SUB-1)
                ADD B-SHIPQTY (SUB-1) TO ST-QTYONHAND
                ADD WS-QTY            TO  B-SHIPQTY (SUB-1)
                GO TO UPST-045.

            ADD B-SHIPQTY (SUB-1)       TO ST-QTYONHAND.
       UPST-045.
           SUBTRACT B-SHIPQTY (SUB-1) FROM ST-SALESUNITMTD
                                           ST-SALESUNITSYTD
           SUBTRACT B-NETT (SUB-1)    FROM ST-SALESRANDSMTD
                                           ST-SALESRANDSYTD
           COMPUTE WS-COST = B-SHIPQTY (SUB-1) * B-STOCKCOST (SUB-1)
           SUBTRACT WS-COST           FROM ST-SALESCOSTMTD
                                           ST-SALESCOSTYTD
           MOVE 0                       TO WS-COST.
       UPST-050.
           REWRITE STOCK-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "STOCK RECORD:"       TO WS-DAILY-1ST
               MOVE B-STOCKNUMBER (SUB-1) TO WS-DAILY-2ND
               MOVE "NOT UPDATED"         TO WS-DAILY-3RD
               MOVE WS-INVOICE            TO WS-DAILY-4TH
               PERFORM WRITE-DAILY
               GO TO UPST-520.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY ON RE-WRITE, 'ESC' TO RETRY." 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO UPST-050.
       UPST-520.
           IF SP-1STCHAR = "*" OR = "/"
               GO TO UPST-550.
           IF ST-ANALYSIS = "S"
            IF B-STOCKPRICE (SUB-1) < ST-PRICE
               PERFORM WRITE-SPECIALS-FILE.
       UPST-550.
           ADD 1 TO SUB-1.
           IF SUB-1 < 201
               GO TO UPST-010.
       UPST-999.
           EXIT.
      *
       REAVERAGE-COST SECTION.
       REAV-000.
           IF B-STOCKCOST (SUB-1) = ST-AVERAGECOST
               GO TO REAV-999.
               
           IF ST-QTYONHAND > 0
              COMPUTE ST-AVERAGECOST ROUNDED =
                ((((ST-QTYONHAND + ST-QTYONRESERVE) * ST-AVERAGECOST) +
                (B-SHIPQTY (SUB-1) * WS-COST)) /
                (ST-QTYONHAND + ST-QTYONRESERVE + B-SHIPQTY (SUB-1)))
           ELSE
                MOVE B-STOCKCOST (SUB-1)  TO ST-AVERAGECOST.
      *     MOVE B-STOCKCOST (SUB-1)      TO ST-LASTCOST.
       REAV-999.
            EXIT.
      *
       WRITE-SPECIALS-FILE SECTION.
       W-SPEC-005.
           MOVE WS-SOLD-BY            TO SP-INITIALS
           MOVE WS-INVOICE            TO SP-INVOICE-NUMBER
           MOVE B-STOCKNUMBER (SUB-1) TO SP-STOCK
           MOVE 6                     TO SP-TRANS
           MOVE WS-ACCOUNT-NUMBER     TO SP-ACCOUNT-NUMBER
           MOVE WS-NAME               TO SP-ACCOUNT-NAME
           MOVE WS-DATE               TO SP-DATE-OF-INVOICE
           MOVE B-NETT (SUB-1)        TO SP-SALE-AMOUNT
           COMPUTE SP-COST-AMOUNT =
                 B-STOCKCOST (SUB-1) * B-SHIPQTY (SUB-1).
                 
           GO TO W-SPEC-010.
       W-SPEC-006.
           READ SPECIALS-FILE
              INVALID KEY NEXT SENTENCE.
           ADD B-NETT (SUB-1) TO SP-SALE-AMOUNT
           COMPUTE SP-COST-AMOUNT = SP-COST-AMOUNT + 
                 (B-STOCKCOST (SUB-1) * B-SHIPQTY (SUB-1)).
           REWRITE SPECIALS-RECORD
              INVALID KEY NEXT SENTENCE.
           IF WS-SPECIALS-ST1 NOT = 0
              MOVE "ERROR IN SPECIALS RE-WRITE, 'ESC' TO SEE CODES"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SPECIALS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE "INFORM YOUR SUPERVISOR THAT NO SPECIAL IS WRITTEN."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
           GO TO W-SPEC-999.
       W-SPEC-010.
           WRITE SPECIALS-RECORD
              INVALID KEY NEXT SENTENCE.
           IF WS-SPECIALS-ST1 = 23 OR 35 OR 49
              GO TO W-SPEC-006.
           IF WS-SPECIALS-ST1 NOT = 0
              MOVE "ERROR IN SPECIALS WRITE, 'ESC' TO SEE CODES"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SPECIALS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE "INFORM YOUR SUPERVISOR THAT NO SPECIAL IS WRITTEN."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
       W-SPEC-999.
           EXIT.
      *
       UPDATE-SALES SECTION.
       UPSA-010.
            MOVE WSAN-CODE TO SA-KEY.
       UPSA-500.
            READ SALES-ANALYSIS WITH LOCK
                INVALID KEY NEXT SENTENCE.
            IF WS-SALES-ST1 = 23 OR 35 OR 49
                MOVE "NO SUCH SALES ANALYSIS RECORD TO UPDATE"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE 0 TO WS-SALES-ST1
                GO TO UPSA-999.
            IF WS-SALES-ST1 NOT = 0
               MOVE "SALES RECORD BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO UPSA-500.
           SUBTRACT WS-COSTTOTAL FROM SA-COST-WEEK
                                      SA-COST-PTD
                                      SA-COST-YTD.
           SUBTRACT WS-PRICETOTAL FROM SA-SALES-WEEK
                                       SA-SALES-PTD
                                       SA-SALES-YTD.
       UPSA-600.
           REWRITE SALES-ANALYSIS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-SALES-ST1 = 23 OR 35 OR 49
              MOVE "SALES RECORD:" TO WS-DAILY-1ST
              MOVE SA-KEY          TO WS-DAILY-2ND
              MOVE "NOT UPDATED"   TO WS-DAILY-3RD
              MOVE WS-INVOICE      TO WS-DAILY-4TH
              PERFORM WRITE-DAILY.
           IF WS-SALES-ST1 NOT = 0
               MOVE "SALES RECORD BUSY ON REWRITE, 'ESC' TO RETRY"
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
       WRITE-SOLD-BY SECTION.
       SOLD-010.
           MOVE WS-SOLD-BY        TO SB-TYPE
           MOVE WS-INVOICE        TO SB-INVOICE-NUMBER
           MOVE 6                 TO SB-TRANS
           MOVE WS-ACCOUNT-NUMBER TO SB-ACCOUNT-NUMBER
           MOVE WS-NAME           TO SB-ACCOUNT-NAME
           MOVE WS-DATE           TO SB-DATE-OF-INVOICE
           MOVE WS-PRICETOTAL     TO SB-SALE-AMOUNT
           MOVE WS-COSTTOTAL      TO SB-COST-AMOUNT.
       SOLD-020.
           WRITE SOLDBY-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-SOLDBY-ST1 NOT = 0
               MOVE "SOLDBY NOT WRITTEN, 'ESC' TO EXIT."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-SOLDBY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-SOLDBY-ST1
                PERFORM ERROR-020.
       SOLD-999.
           EXIT.
      *
       READ-DEBTORS SECTION.
       RD-000.
           MOVE WS-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
           READ DEBTOR-MASTER INVALID KEY
               NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE " " TO DR-NAME DR-ADDRESS1 DR-ADDRESS2
                        DR-ADDRESS3 DR-DEL-ADDRESS1 DR-DEL-ADDRESS2
                        DR-DEL-ADDRESS3
               MOVE "UNKNOWN" TO DR-NAME
               MOVE 0 TO DR-POST-CODE
               GO TO RD-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTORS BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RD-000.
           MOVE DR-NAME         TO WS-NAME
           MOVE DR-ADDRESS1     TO WS-ADD1
           MOVE DR-ADDRESS2     TO WS-ADD2
           MOVE DR-ADDRESS3     TO WS-ADD3
           MOVE DR-DEL-ADDRESS1 TO WS-DELADD1
           MOVE DR-DEL-ADDRESS2 TO WS-DELADD2
           MOVE DR-DEL-ADDRESS3 TO WS-DELADD3
           MOVE DR-POST-CODE    TO WS-POSTCODE.
       RD-999.
           EXIT.
      *
       READ-TERMS-FILE SECTION.
       RTERM-000.
            MOVE 2 TO PA-TYPE.
            MOVE 1 TO SUB-1
                      PA-RECORD.
       RTERM-010.
            READ PARAMETER-FILE NEXT
                AT END NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 10
                CLOSE PARAMETER-FILE
                GO TO RTERM-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER TERMS BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               CLOSE PARAMETER-FILE
               GO TO RTERM-000.
            IF PA-TYPE < 2
               GO TO RTERM-010.
            IF PA-TYPE > 2
               CLOSE PARAMETER-FILE
               GO TO RTERM-999.
            MOVE PARAMETER-REC TO WS-TERM-OCCUR (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 = 10
               PERFORM ERROR-020
               CLOSE PARAMETER-FILE
               GO TO RTERM-999.
            GO TO RTERM-010.
       RTERM-999.
            EXIT.
      *
       READ-DELIVERY-FILE SECTION.
       RDELIV-000.
            OPEN I-O PARAMETER-FILE.
            MOVE 1 TO SUB-1
                      PA-RECORD.
            MOVE 3 TO PA-TYPE.
       RDELIV-010.
            READ PARAMETER-FILE NEXT
                AT END NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 10
                GO TO RDELIV-999.
            IF PA-TYPE < 3
               GO TO RDELIV-010.
            IF PA-TYPE > 3
                GO TO RDELIV-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER DELV BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RDELIV-010.
            IF PARAMETER-REC = "           "
               GO TO RDELIV-010.           
            MOVE PARAMETER-REC TO WS-DEL-OCCUR (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 = 10
               PERFORM ERROR-020
               GO TO RDELIV-999.
            GO TO RDELIV-010.
       RDELIV-999.
            EXIT.
      *
       READ-SALES-ANALYSIS SECTION.
       RSALES-500.
            MOVE WSAN-CODE TO SA-KEY.
            READ SALES-ANALYSIS
                INVALID KEY
                NEXT SENTENCE.
            IF WS-SALES-ST1 = 23 OR 35 OR 49
                MOVE "UNKNOWN" TO SA-NAME
                GO TO RSALES-999.
            IF WS-SALES-ST1 NOT = 0
               MOVE "SALES BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO RSALES-999.
       RSALES-900.
            PERFORM ERROR-020.
            MOVE 0 TO WS-SALES-ST1.
            MOVE SA-NAME TO WS-SALESANALYSIS.
       RSALES-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           PERFORM OPEN-014.
           PERFORM READ-PARAMETER.
           PERFORM GET-SYSTEM-Y2K-DATE.
           IF WS-MM = PA-CURRENT-PER-MM
             IF WS-YY = PA-CURRENT-PER-YY
               GO TO OPEN-010.
           DISPLAY "THE MONTH OR YEAR ON THE PARAMETER FILE".
           DISPLAY " "
           DISPLAY "DOES NOT CORRESPOND WITH TODAYS DATE!!!!".
           DISPLAY " ".
           DISPLAY "CHECK THE SYSTEM DATE, ".
           DISPLAY " ".
           DISPLAY "      OR ELSE RUN THE MONTH-END ROUTINE.".
           STOP 1.
           CLOSE PARAMETER-FILE.
           EXIT PROGRAM.
       OPEN-010.
           MOVE ALL "X" TO STORE-TERM
                           STORE-DEL.
           PERFORM READ-TERMS-FILE.
           PERFORM READ-DELIVERY-FILE.
           CLOSE PARAMETER-FILE.
       OPEN-011.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0 
              MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
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
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO OPEN-012.
       OPEN-013.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0 
              MOVE "REGISTER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO OPEN-013.
       OPEN-014.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO OPEN-014.
       OPEN-015.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0 
              MOVE "ST-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
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
              MOVE "DR-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO OPEN-016.
       OPEN-017.
           OPEN I-O DISTRIBUTIONS.
           IF WS-DISTRIBUTION-ST1 NOT = 0
              MOVE "DISTRIBUTIONS BUSY ON OPEN, PRESS 'ESC' TO RETRY"
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DISTRIBUTION-ST1
               GO TO OPEN-017.
       OPEN-018.
            OPEN I-O SALES-ANALYSIS.
            IF WS-SALES-ST1 NOT = 0
               MOVE "SALES FILE BUSY ON OPEN, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SALES-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SALES-ST1
               GO TO OPEN-018.
       OPEN-019.
           OPEN I-O SOLD-BY.
           IF WS-SOLDBY-ST1 NOT = 0
              MOVE "SOLD BY FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SOLDBY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SOLDBY-ST1
              GO TO OPEN-019.
       OPEN-020.
           OPEN I-O SPECIALS-FILE.
           IF WS-SPECIALS-ST1 NOT = 0
              MOVE "SPECIALS FILE BUSY ON OPEN I-O, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SLSPECIALS TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-SPECIALS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SPECIALS-ST1
              GO TO OPEN-0201.
           GO TO OPEN-021.
       OPEN-0201.
           OPEN OUTPUT SPECIALS-FILE.
           IF WS-SPECIALS-ST1 NOT = 0 
              MOVE "SPECIALS FILE BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-SPECIALS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-SPECIALS-ST1
              GO TO OPEN-020.
       OPEN-021.
           OPEN I-O STOCK-TRANSLY-FILE.
           IF WS-STTRANSLY-ST1 NOT = 0 
              MOVE "ST-TRANS-LY FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO OPEN-021.
       OPEN-022.
           OPEN I-O INCR-LY-REGISTER.
           IF WS-INCR-LY-ST1 NOT = 0 
              MOVE "REGISTER-LY FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-LY-ST1
               GO TO OPEN-022.
       OPEN-025.
           MOVE "ACCOUNTNO"     TO F-FIELDNAME.
           PERFORM READ-PARAMETER.
           MOVE PA-GST-PERCENT  TO WS-GST-PERCENT.
       OPEN-220.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlCredit"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE DEBTOR-MASTER
                 STOCK-MASTER
                 INCR-REGISTER
                 SALES-ANALYSIS
                 SOLD-BY
                 DISTRIBUTIONS
                 PARAMETER-FILE
                 STOCK-TRANS-FILE
                 DEBTOR-TRANS-FILE
                 SPECIALS-FILE
                 INCR-LY-REGISTER
                 STOCK-TRANSLY-FILE.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldQty".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAmount".
       Copy "WriteFieldAmountDis".
       Copy "WriteFieldNumeric".
       Copy "GetSystemY2KDate".
       Copy "OrderPassword".
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
       Copy "CTOSCobolAccept".
       Copy "WriteDailyExcep1".
      * END-OF-JOB
