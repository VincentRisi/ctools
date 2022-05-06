        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrPaymnt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectGlMaster".
           Copy "SelectGlParameter".
           Copy "SelectGlTrans".
           Copy "SelectCrMaster".
           Copy "SelectCrJrn".
           Copy "SelectCrTrans".
           Copy "SelectCbMaster".
           Copy "SelectCbTrans".
           Copy "SelectSlDaily".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCrTrans.
           COPY ChlfdCrJrn.
           COPY ChlfdCbMast.
           COPY ChlfdCbTrans.
           COPY ChlfdGlParam.
           COPY ChlfdGlMast.
           COPY ChlfdGlTrans.
           COPY ChlfdDaily.

       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-HAVE-PRINTED      PIC X VALUE " ".
       77  WS-LINE-CNT          PIC 9(3) VALUE 66.
       77  WS-PAGE-CNT          PIC 9(3) VALUE 0.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "CrTranIq".
       77  WS-DATE-INQ-PROGRAM  PIC X(8) VALUE "CrPayDMt".
       77  WS-ACCOUNT-NUMBER    PIC 9(7) VALUE 0.
       77  WS-CHEQUENO          PIC X(10) VALUE " ".
       77  WS-BATCH-TOTAL       PIC 9(7)V99 VALUE 0.
       77  WS-PAYAMT            PIC 9(7)V99 VALUE 0.
       77  WS-SETT-DISC         PIC 9(7)V99 VALUE 0.
       77  WS-CRJRN-DISC        PIC 9(7)V99 VALUE 0.
       77  WS-CRJRN-CALC-DISC   PIC 9(7)V99 VALUE 0.
       77  WS-CRJRN-ALLOC-DISC  PIC 9(7)V99 VALUE 0.
       77  WS-FOREX-AMT         PIC S9(6)V99 VALUE 0.
       77  WS-TRANSTYPE         PIC X VALUE " ".
       77  WS-TYPEOFPROCESS     PIC X VALUE " ".
       77  WS-LINE-ERROR        PIC X VALUE " ".
      * 77  WS-FOREX-LOSS-ENTRY  PIC X VALUE "N".
       77  WS-CURRENTGLPER      PIC 99 VALUE 0.
       77  WS-CURRENTCRPER      PIC 99 VALUE 0.
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-DISC-ALLOCATED    PIC 9(7)V99.
       77  WS-WORKTOTAL         PIC S9(7)V99 VALUE 0.
       77  WS-WORKTOTAL-DR      PIC S9(7)V99 VALUE 0.
       77  WS-WORKTOTAL-CR      PIC S9(7)V99 VALUE 0.
       77  WS-BODY-LINE         PIC Z9.
       77  WS-PAGE              PIC 9 VALUE 0.
       77  ANSWER1              PIC XX VALUE " ".
       77  WS-WORK-FIELD        PIC S9(7)V99 VALUE 0.
       77  RUN-TOTAL            PIC S9(8)V99 VALUE 0.
       77  WS-JNL-CR-AMT        PIC S9(7)V99 VALUE 0.
       77  WS-UNAPPLIED-AMT     PIC S9(7)V99 VALUE 0.
       77  WS-RUN-PAYMENTS      PIC S9(7)V99 VALUE 0.
       77  WS-RUN-DISCOUNT      PIC S9(7)V99 VALUE 0.
       77  WS-RUN-DEBITS        PIC S9(7)V99 VALUE 0.
       77  WS-RUN-CREDITS       PIC S9(7)V99 VALUE 0.
       77  WS-RUN-FOREX         PIC S9(6)V99 VALUE 0.
       01  WS-BATCH.
           03  WS-BATCH-1STCHAR  PIC X(2) VALUE "PC".
           03  WS-BATCH-REST     PIC X(8).
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1     PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1       PIC 99.
       01  WS-CRJRN-STATUS.
           03  WS-CRJRN-ST1        PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
       01  WS-GLTRANS-STATUS.
           03  WS-GLTRANS-ST1      PIC 99.
       01  WS-CRTRANS-STATUS.
           03  WS-CRTRANS-ST1      PIC 99.
       01  WS-CBTRANS-STATUS.
           03  WS-CBTRANS-ST1      PIC 99.
       01  WS-CB-STATUS.
           03  WS-CB-ST1           PIC 99.
       01  WS-DIST-TOTALS.
           03  WS-DIST-PAYMENT    PIC S9(7)V99 VALUE 0.
           03  WS-DIST-JOURNALDR  PIC S9(7)V99 VALUE 0.
           03  WS-DIST-JOURNALCR  PIC S9(7)V99 VALUE 0.
           03  WS-DIST-DISCOUNT   PIC S9(7)V99 VALUE 0.
           03  WS-DIST-FOREX      PIC S9(7)V99 VALUE 0.
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 401.
               05  WS-APPLY           PIC X.
               05  WS-ALREADY-APPLIED PIC X.
               05  WS-REFNUM          PIC X(10).
               05  WS-TYPE            PIC X(7).
               05  WS-BODDATE.
                   07  WS-BOD-DD     PIC 99.
                   07  WS-BOD-MM     PIC 99.
                   07  WS-BOD-YY     PIC 9999.
               05  WS-BODDATE-RED REDEFINES WS-BODDATE.
                   07  WS-BODDATENUM PIC 9(8).
               05  WS-AMTOFTYPE      PIC 9(7)V99.
               05  WS-PAYMENT        PIC 9(7)V99.
               05  WS-DISCOUNT       PIC 9(7)V99.
               05  WS-FOREX          PIC S9(6)V99.
               05  WS-CRTYPE         PIC 99.
               05  WS-CR-TRANSNO     PIC 9(6).
       01  CREDIT-REC.
           03  CR-APPLY          PIC X.
           03  CR-REFNUM         PIC 9(6).
           03  CR-TYPE           PIC X(7).
           03  CR-BODDATE.
               05  CR-BOD-DD     PIC 99.
               05  CR-BOD-MM     PIC 99.
               05  CR-BOD-YY     PIC 9999.
           03  CR-BODDATE-RED REDEFINES CR-BODDATE.
               05  CR-BODDATENUM PIC 9(8).
           03  CR-OLD-PAYMENT    PIC X.
           03  CR-AMTOFTYPE      PIC 9(7)V99.
           03  CR-PAYMENT        PIC 9(7)V99.
           03  CR-DISCOUNT       PIC 9(7)V99.
           03  CR-CRTYPE         PIC 99.
           03  CR-CRTRANS        PIC 9(6).
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       01  LINE-DESCRIPTION.
           03  WS-CR1              PIC X(4).
           03  WS-CRACC            PIC X(8).
           03  WS-CR2              PIC X(3).
           03  WS-CRINV            PIC X(10).
       01  WS-BATCHPERIOD-IND.
           03  WS-1STPER           PIC X.
           03  WS-REST             PIC 99.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-GLHEADER     PIC X(2).
               05  WS-GLSUBHEADER  PIC X(4).
           03  WS-RESTOFACCOUNT    PIC X(6).
       01  WS-TYPES.
           03  FILLER          PIC X(7) VALUE "INVOICE".
           03  FILLER          PIC X(7) VALUE "PAYMENT".
           03  FILLER          PIC X(7) VALUE "PAYMENT".
           03  FILLER          PIC X(7) VALUE "JRN.DR.".
           03  FILLER          PIC X(7) VALUE "JRN.CR.".
           03  FILLER          PIC X(7) VALUE "C/NOTE ".
           03  FILLER          PIC X(7) VALUE "INTREST".
           03  FILLER          PIC X(7) VALUE "DISCNT.".
           03  FILLER          PIC X(7) VALUE "FOREX  ".
       01  WS-TYPES-RED REDEFINES WS-TYPES.
           03  WS-TYPE-DESC    PIC X(7) OCCURS 9.
       01  WS-DUE-DATE.
           03  WS-DUE-YY           PIC 9999.
           03  WS-DUE-MM           PIC 99.
           03  WS-DUE-DD           PIC 99.
       01  WS-CHECK-DATE.
           03  WS-CHECK-YY           PIC 9999.
           03  WS-CHECK-MM           PIC 99.
           03  WS-CHECK-DD           PIC 99.
       01  WS-PRINTER-INFO.
           03  WS-PRN-FIL     PIC X(8) VALUE " ".
           03  WS-PRN-NAME    PIC X(12) VALUE " ".
       01  HEAD1.
           03  FILLER          PIC X(7) VALUE "  DATE".
           03  H1-DATE         PIC X(10).
           03  FILLER          PIC X(18) VALUE " ".
           03  FILLER          PIC X(50) VALUE
           "CREDITORS PAYMENT ALLOCATIONS AUDIT TRAIL".
           03  FILLER          PIC X(14) VALUE "PAYMENT DATE:".
           03  H1-PAY-DATE     PIC X(10).
           03  FILLER          PIC X(14) VALUE " ".
           03  FILLER          PIC X(5) VALUE "PAGE:".
           03  H1-PAGE         PIC Z9.
           03  FILLER          PIC X(1) VALUE " ".
       01  HEAD2.
           03  FILLER          PIC X(35) VALUE " ".
           03  FILLER          PIC X(41) VALUE ALL "*".
           03  FILLER          PIC X(15) VALUE " ".
           03  FILLER          PIC X(8) VALUE "PERIOD:".
           03  H1-PERIOD       PIC X(2).
           03  FILLER          PIC X(31) VALUE " ".
       01  HEAD3.
           03  FILLER          PIC X(10) VALUE "ACCOUNT".
           03  FILLER          PIC X(9) VALUE "TRANS".
           03  FILLER          PIC X(10) VALUE "TRANS".
           03  FILLER          PIC X(9) VALUE "TRANS".
           03  FILLER          PIC X(25) VALUE "BATCH NAME".
           03  FILLER          PIC X(13) VALUE "TRANS #".
           03  FILLER          PIC X(13) VALUE "DATE".
           03  FILLER          PIC X(12) VALUE "BEGIN".
           03  FILLER          PIC X(27) VALUE "REMAIN".
       01  HEAD4.
           03  FILLER          PIC X(10) VALUE "NUMBER".
           03  FILLER          PIC X(20) VALUE "CODE     DESC".
           03  FILLER          PIC X(59) VALUE "NO.".
           03  FILLER          PIC X(12) VALUE "AMOUNT".
           03  FILLER          PIC X(10) VALUE "AMOUNT".
           03  FILLER          PIC X(11) VALUE "DISCOUNT".
           03  FILLER          PIC X(9) VALUE "APPLIED".
       01  DETAIL-LINE.
           03  D-ACCOUNTNO     PIC X(7).
           03  FILLER          PIC X(5) VALUE " ".
           03  D-TRANSCODE     PIC 9.
           03  FILLER          PIC X(5) VALUE " ".
           03  D-TRANSDESC     PIC X(10).
           03  D-TRANSNO       PIC Z(5)9.
           03  FILLER          PIC X(4) VALUE " ".
           03  D-REFERENCE1    PIC X(25).
           03  D-REFERENCE2    PIC X(11).
           03  D-DATE          PIC X(10).
           03  FILLER          PIC X(1) VALUE " ".
           03  D-BEGIN         PIC Z(6)9.99-.
           03  FILLER          PIC X(1) VALUE " ".
           03  D-OUTST         PIC Z(6)9.99-.
      *     03  FILLER          PIC X(1) VALUE " ".
           03  D-DISCOUNT      PIC Z(6)9.99-.
      *     03  FILLER          PIC X(1) VALUE " ".
           03  D-APPLIED       PIC Z(6)9.99-.
           03  FILLER          PIC X(2) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER          PIC X(20) VALUE " ".
           03  TOT-DESC        PIC X(20) VALUE " ".
           03  FILLER          PIC X(2) VALUE " ".
           03  TOT-AMOUNT      PIC Z(6)9.99-.
           03  FILLER          PIC X(81) VALUE " ".
       01  WS-TEMP-LINE.
           03  FILLER           PIC X(8) VALUE " ".
           03  T-TRANS-DATE     PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
           03  T-TRANS-DUEDATE  PIC X(10).
           03  FILLER           PIC X(2) VALUE " ".
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
           PERFORM CLEAR-SCREEN.
           MOVE 0310 TO POS
           DISPLAY "** CREDITORS PAYMENT ALLOCATION PROGRAM **" AT POS
           MOVE 0410 TO POS
           DISPLAY "******************************************" AT POS.
       CONTROL-003.
       Copy "PrinterAcceptCr".
           MOVE 2910 TO POS
           DISPLAY "Program now loading, Please be patient.." AT POS.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
           MOVE "N" TO WS-HAVE-PRINTED.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           
           PERFORM GET-001.
           
           PERFORM OPEN-0121.
      ******************************************************************
      * ABOVE "PERFORM OPEN-0121" ADDED TO THE PROGRAM                 *
      * 14/12/2003 TO TRY TO STOP  PROBLEMS IN A GL MONTHEND WHILE THIS*
      * PROGRAM IS LOADED.                                             *
      * THIS SHOULD KEEP THE GL & CR PERIODS UP TO DATE WITHIN THIS    *
      * PROGRAM.  DISCOUNTS WERE NOT BEING UPDATED IN THE GL WHEN CR & *
      * GL PERIODS WERE THE SAME.                                      *
      ******************************************************************
           PERFORM DISPLAY-TOP-INFO.
           MOVE WS-PAY-DATE TO WS-DATE
           MOVE 0 TO WS-DIST-PAYMENT
                     WS-DIST-JOURNALDR
                     WS-DIST-JOURNALCR
                     WS-DIST-DISCOUNT
                     WS-DIST-FOREX.
           IF WS-BATCH-TOTAL NOT = 0
            IF WS-BATCH-TOTAL = WS-RUN-PAYMENTS
              MOVE
           "THE BATCH = RUN-TOTAL, ALLOCATIONS SHOULD BE COMPLETE."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
           IF WS-BATCH-TOTAL < WS-RUN-PAYMENTS
              MOVE
           "THE BATCH < RUN-TOTAL, YOU HAVE OVER ALLOCATED PAYMENTS."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
           IF WS-BATCH-REST = "    "
                PERFORM ENTER-BATCH-DETAILS.
           PERFORM GET-DATA.
           
           IF ANSWER1 = "N"
                GO TO CONTROL-010.
           IF WS-TYPEOFPROCESS = "3"
                GO TO CONTROL-010.

           IF WS-TYPEOFPROCESS = "2"
             IF CR-OLD-PAYMENT = " "
              IF ANSWER1 = "Y"
                PERFORM WRITE-PAYMENT-TRANSACTION
                PERFORM APPLY-AMOUNTS
                ADD WS-PAYAMT    TO WS-RUN-PAYMENTS
                ADD WS-SETT-DISC TO WS-RUN-DISCOUNT
                ADD WS-FOREX-AMT TO WS-RUN-FOREX
                PERFORM WRITE-GLSETT-DISCOUNTS
                PERFORM WRITE-GLFOREX-TRANS
                PERFORM WRITE-CASHBOOK
                MOVE " " TO PRINT-REC
                WRITE PRINT-REC
                ADD 1 TO WS-LINE-CNT
               IF WS-WORKTOTAL NOT = 0
                  PERFORM READ-CREDITORS-LOCK
                  SUBTRACT WS-WORKTOTAL FROM CR-BALANCE
                                             CR-CURRENT
                  PERFORM REWRITE-CREDITOR
                  GO TO CONTROL-010
               ELSE
                  GO TO CONTROL-010.

           IF WS-TYPEOFPROCESS = "2"
            IF CR-OLD-PAYMENT = "1"
             IF CR-CRTYPE = 2
              IF ANSWER1 = "Y"
                PERFORM UPDATE-OLD-CREDIT
                PERFORM APPLY-AMOUNTS
                ADD WS-SETT-DISC TO WS-RUN-DISCOUNT
                ADD WS-FOREX-AMT TO WS-RUN-FOREX
                PERFORM WRITE-GLSETT-DISCOUNTS
                PERFORM WRITE-GLFOREX-TRANS
                MOVE " " TO PRINT-REC
                WRITE PRINT-REC
                ADD 1 TO WS-LINE-CNT
                GO TO CONTROL-010.

           IF WS-TYPEOFPROCESS = "2"
            IF CR-OLD-PAYMENT = "1"
             IF CR-CRTYPE = 5 OR = 6
              IF ANSWER1 = "Y"
                PERFORM UPDATE-OLD-CREDIT
                PERFORM APPLY-AMOUNTS
                PERFORM REDUCE-CRJRN-CNOTE
                MOVE " " TO PRINT-REC
                WRITE PRINT-REC
                ADD 1 TO WS-LINE-CNT
                GO TO CONTROL-010.

           IF WS-TRANSTYPE = "1"
               PERFORM JOURNAL-DEBIT
               PERFORM READ-CREDITORS-LOCK
               ADD WS-PAYAMT TO CR-BALANCE
               ADD WS-PAYAMT TO CR-CURRENT
               ADD WS-PAYAMT TO WS-RUN-DEBITS
               PERFORM REWRITE-CREDITOR
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               ADD 1 TO WS-LINE-CNT
               GO TO CONTROL-010.
                
           IF WS-TRANSTYPE = "3"
               PERFORM JOURNAL-CREDIT
               PERFORM READ-CREDITORS-LOCK
               SUBTRACT WS-PAYAMT FROM CR-BALANCE
                                       CR-CURRENT
               ADD WS-PAYAMT TO WS-RUN-CREDITS
               PERFORM REWRITE-CREDITOR
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               ADD 1 TO WS-LINE-CNT
               GO TO CONTROL-010.

           MOVE "NOTHING UPDATED CONTROL-010, 'ESC' TO CONTINUE."
              TO WS-MESSAGE
           PERFORM ERROR-000
           MOVE "PLEASE NOTIFY YOUR SUPERVISOR OF THIS ERROR NOW!!!"
              TO WS-MESSAGE
           PERFORM ERROR1-MESSAGE.
           GO TO CONTROL-010.
       CONTROL-999.
           EXIT.
      *
       DISPLAY-TOP-INFO SECTION.
       DTI-005.
           MOVE "PERIOD"      TO F-FIELDNAME
           MOVE 6             TO F-CBFIELDNAME
           MOVE WS-CURRENTCRPER TO F-NAMEFIELD
           MOVE 2             TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE WS-CURRENTCRPER      TO SUB-1.
           MOVE "BEGDATE"          TO F-FIELDNAME
           MOVE 7                  TO F-CBFIELDNAME
           MOVE GL-BEGDATE (SUB-1) TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO F-NAMEFIELD
           MOVE 10                 TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE WS-CURRENTCRPER      TO SUB-1.
           MOVE "ENDDATE"          TO F-FIELDNAME
           MOVE 7                  TO F-CBFIELDNAME
           MOVE GL-ENDDATE (SUB-1) TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO F-NAMEFIELD
           MOVE 10                 TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "BATCHNAME"   TO F-FIELDNAME
           MOVE 9             TO F-CBFIELDNAME
           MOVE WS-BATCH-REST TO F-NAMEFIELD
           MOVE 8             TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "BATCHDATE" TO F-FIELDNAME
           MOVE 9           TO F-CBFIELDNAME
           MOVE H1-PAY-DATE TO F-NAMEFIELD
           MOVE 10          TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "BATCHTOTAL"    TO F-FIELDNAME
           MOVE 10              TO F-CBFIELDNAME
           MOVE WS-BATCH-TOTAL  TO F-EDNAMEFIELDSALE
           MOVE 10              TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-SALE.

           MOVE "RUNTOTAL"       TO F-FIELDNAME
           MOVE 8                TO F-CBFIELDNAME
           MOVE WS-RUN-PAYMENTS  TO F-EDNAMEFIELDSALE
           MOVE 10               TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-SALE.
       DTI-999.
           EXIT.
      *
       ENTER-BATCH-DETAILS SECTION.
       EBD-005.
           MOVE "                   " TO F-NAMEFIELD.
           MOVE "BATCHNAME" TO F-FIELDNAME
           MOVE 9           TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD
           MOVE 8           TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA
           MOVE F-NAMEFIELD TO WS-BATCH-REST.
           IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
       EBD-010.
           MOVE "                   " TO F-NAMEFIELD.
           MOVE "BATCHDATE" TO F-FIELDNAME.
           MOVE 9 TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
                GO TO EBD-005.
           MOVE 10 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
               GO TO EBD-010.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD H1-PAY-DATE.
            PERFORM WRITE-FIELD-ALPHA.
            
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO WS-CHECK-DATE WS-DATE WS-PAY-DATE
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO EBD-010.

            MOVE WS-CURRENTCRPER TO SUB-1.
            MOVE GL-BEGDATE (SUB-1) TO WS-BEG-DATE.
            MOVE GL-ENDDATE (SUB-1) TO WS-END-DATE.
            IF WS-CHECK-DATE NOT < WS-BEG-DATE
             IF WS-CHECK-DATE NOT > WS-END-DATE
                GO TO EBD-020.
            IF WS-CHECK-DATE > WS-END-DATE
            MOVE "THE JOURNAL DATE MUST BE < OR = THE END PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO EBD-010.
            IF WS-CHECK-DATE < WS-BEG-DATE
            MOVE "THE JOURNAL DATE MUST > OR = THE BEG. PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO EBD-010.
       EBD-020.
           PERFORM ERROR-020.
           MOVE "                   " TO F-NAMEFIELD.
           MOVE "BATCHTOTAL" TO F-FIELDNAME
           MOVE 10           TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
                GO TO EBD-010.
           MOVE 10           TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD  TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO WS-BATCH-TOTAL F-EDNAMEFIELDSALE.
           PERFORM WRITE-FIELD-SALE.
       EBD-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE 0 TO WS-UNAPPLIED-AMT
                      WS-SETT-DISC
                      WS-PAYAMT.
            MOVE 0 TO WS-JNL-CR-AMT
                      WS-DIST-DISCOUNT
                      WS-FOREX-AMT.
            MOVE " " TO CR-OLD-PAYMENT
                        ANSWER1
                        WS-ABOVE-BODY.
            PERFORM CLEAR-FIELDS.
       GET-001.
            MOVE "Printer:"   TO WS-PRN-FIL
            MOVE Ws-Printer   To WS-PRN-NAME
            MOVE 0359 TO POS
            DISPLAY WS-PRINTER-INFO AT POS.
       GET-010.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
             IF WS-BATCH-TOTAL NOT = WS-RUN-PAYMENTS
               MOVE "BATCH TOTALS DO NOT BALANCE, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM EBD-020
               GO TO GET-010.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 GO TO GET-999.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF F-NAMEFIELD = "XXXXXXX"
                CLOSE CREDITOR-MASTER
                CLOSE CRTR-FILE
                CLOSE CRJRN-FILE
                CALL WS-DATE-INQ-PROGRAM USING WS-LINKAGE
                CANCEL WS-DATE-INQ-PROGRAM
                PERFORM OPEN-011
                PERFORM OPEN-014
                PERFORM OPEN-015
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                PERFORM DISPLAY-TOP-INFO
                GO TO GET-000.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-ACCOUNT-NUMBER
                                 CR-ACCOUNT-NUMBER.
       GET-012.
            IF CR-ACCOUNT-NUMBER = 0
                CLOSE CREDITOR-MASTER
                CLOSE CRTR-FILE
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-011
                PERFORM OPEN-014
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                PERFORM DISPLAY-TOP-INFO
                GO TO GET-000.

            PERFORM READ-CREDITORS.
            MOVE "NAME"  TO F-FIELDNAME
            MOVE 4       TO F-CBFIELDNAME
            MOVE CR-NAME TO F-NAMEFIELD
            MOVE 40      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            IF CR-NAME = "UNKNOWN"
                GO TO GET-010.
       GET-020.
            MOVE "                   " TO F-NAMEFIELD
            MOVE "TRANSTYPE"           TO F-FIELDNAME
            MOVE 9                     TO F-CBFIELDNAME.
            MOVE WS-TRANSTYPE TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-000.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-TRANSTYPE.
            IF WS-TRANSTYPE = "1" OR = "2" OR = "3"
               GO TO GET-030.
            MOVE "INVALID TRANSACTION TYPE!!!" TO WS-MESSAGE.
            PERFORM ERROR-MESSAGE.
            GO TO GET-020.
       GET-030.
            IF WS-TRANSTYPE = "1" OR = "3"
               MOVE "JRN DR/CR ADJ. CANNOT BE PROCESSED AT THIS TIME"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-020.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "CHEQUENO"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE WS-CHEQUENO TO F-NAMEFIELD.
            MOVE 10          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-020.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-CHEQUENO.
            IF WS-TRANSTYPE = "1" OR = "3"
               MOVE "ONLY USE DR/CR ADJ. IF THE NETT DIFF. WILL = ZERO"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-050.
       GET-040.
            MOVE 1 TO SUB-1.
            PERFORM READ-FOR-OLD-CREDIT.
            PERFORM READ-CREDITOR-TRANS.
            MOVE 1 TO SUB-1.
            IF WS-TRANSTYPE = "2"
             IF WS-REFNUM (SUB-1) = " "
               MOVE
               "NO TRANSACTIONS TO APPLY CASH TO, 'ESC' TO CONTINUE."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "2" TO WS-TYPEOFPROCESS
               PERFORM GET-050
               PERFORM GET-057
               PERFORM QUESTION-UNAPPLIED-AMOUNT
               GO TO GET-999.
            IF CR-OLD-PAYMENT = "1"
             IF WS-JNL-CR-AMT NOT = 0
                MOVE WS-JNL-CR-AMT TO WS-PAYAMT
                GO TO GET-045
              ELSE
                MOVE WS-UNAPPLIED-AMT TO WS-PAYAMT
                GO TO GET-045.
            IF WS-TRANSTYPE = "2"
             IF CR-OLD-PAYMENT = " "
                MOVE 2 TO CR-CRTYPE.
            IF WS-PAYAMT = 0
                GO TO GET-050.
       GET-045.
            MOVE "                          " TO F-NAMEFIELD.
            MOVE "PAYAMT"   TO F-FIELDNAME.
            MOVE 6          TO F-CBFIELDNAME.
            MOVE WS-PAYAMT  TO F-EDNAMEFIELDSALE.
            MOVE 10         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.
            GO TO GET-059.
       GET-050.
            MOVE "PAYAMT" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
       GET-055.
            IF F-EXIT-CH = X"01"
                GO TO GET-030.
       GET-057.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-PAYAMT.
            MOVE WS-PAYAMT    TO WS-WORKTOTAL
                                  F-EDNAMEFIELDSALE.
            PERFORM WRITE-FIELD-SALE.
       GET-059.
            IF CR-OLD-PAYMENT = " "
             IF CR-SETT-DISC > 0
              COMPUTE WS-SETT-DISC =
               (WS-PAYAMT / ((100 - CR-SETT-DISC) / 100)) *
               (CR-SETT-DISC / 100).
            IF CR-OLD-PAYMENT = "1"
                MOVE CR-DISCOUNT TO WS-SETT-DISC.
                
            MOVE "SETT-DISC"  TO F-FIELDNAME.
            MOVE 9            TO F-CBFIELDNAME.
            MOVE WS-SETT-DISC TO F-EDNAMEFIELDSALE.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-SALE.
       GET-060.
            IF CR-CRTYPE = 6
                GO TO GET-068.
            MOVE "                          " TO F-NAMEFIELD.
            MOVE "SETT-DISC" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
       GET-061.
            IF F-EXIT-CH = X"01"
             IF CR-OLD-PAYMENT = "1"
                GO TO GET-030
             ELSE
                GO TO GET-050.
       GET-065.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-SETT-DISC
                                  F-EDNAMEFIELDSALE.
            PERFORM WRITE-FIELD-SALE.
       GET-068.
            IF WS-TRANSTYPE = "1" OR = "3"
               MOVE "Y" TO ANSWER1
               MOVE " " TO WS-TYPEOFPROCESS
               GO TO GET-999.
       GET-069.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "TYPEOFPROCESS" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE WS-TYPEOFPROCESS TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-060.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-TYPEOFPROCESS.
       GET-070.
            IF WS-TYPEOFPROCESS = "2" OR = "3"
               GO TO GET-080.
            MOVE "ONLY USE PROCESS TYPE 2 OR 3 !!!" TO WS-MESSAGE.
            PERFORM ERROR-MESSAGE.
            GO TO GET-060.
       GET-080.
            MOVE 0 TO SUB-1.
            PERFORM SCROLL-NEXT.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-090.
            MOVE "N" TO WS-LINE-ERROR.
            PERFORM FILL-BODY.
            PERFORM QUESTION-UNAPPLIED-AMOUNT.
       GET-999.
            EXIT.
      *
       FILL-BODY SECTION.
       FILL-000.
           MOVE 1 TO SUB-1 SUB-2 SUB-3.
       FILL-010.
      *     MOVE "N" TO WS-FOREX-LOSS-ENTRY.

           MOVE 3015 TO POS
           DISPLAY "1ST BODY LINE: " AT POS
           ADD 16 TO POS
           MOVE SUB-1 TO WS-BODY-LINE
           DISPLAY WS-BODY-LINE AT POS.

           IF SUB-1 < 1
              MOVE 1 TO SUB-1 F-INDEX.
           MOVE "                   " TO F-NAMEFIELD.
           MOVE "APPLY" TO F-FIELDNAME.
           MOVE 5 TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01" AND F-INDEX = 1
              MOVE "1" TO WS-ABOVE-BODY
              GO TO FILL-999.
           IF F-EXIT-CH = X"01" AND F-INDEX > 1
              SUBTRACT 1 FROM F-INDEX 
                              SUB-1
              GO TO FILL-010.
           IF F-EXIT-CH = X"11"
            IF SUB-1 NOT > SUB-9
              PERFORM SCROLL-NEXT
              MOVE 1 TO F-INDEX
              GO TO FILL-010
            ELSE
              GO TO FILL-010.
           IF F-EXIT-CH = X"0C"
            IF SUB-1 NOT > SUB-9
              PERFORM SCROLL-NEXT-PAGE
              MOVE 1 TO F-INDEX
              GO TO FILL-010
            ELSE
              GO TO FILL-010.
           IF F-EXIT-CH = X"05"
              PERFORM SCROLL-PREVIOUS
              MOVE 1 TO F-INDEX
              GO TO FILL-010.
           IF F-EXIT-CH = X"09"
              MOVE " " TO WS-ABOVE-BODY
              GO TO FILL-999.
           IF F-EXIT-CH = X"0B" AND F-INDEX < 12
            IF SUB-1 NOT > SUB-9
              ADD 1 TO F-INDEX SUB-1
              GO TO FILL-010.
           MOVE 1 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO WS-APPLY (SUB-1).
           IF F-INDEX = 12 AND WS-APPLY (SUB-1) NOT = "A"
              PERFORM SCROLL-NEXT
              GO TO FILL-010.
           IF SUB-1 < 1
              MOVE 1 TO SUB-1 F-INDEX.
           IF WS-APPLY (SUB-1) = " " AND WS-REFNUM (SUB-1) > " "
              ADD 1 TO F-INDEX SUB-1
              GO TO FILL-010.
           IF WS-APPLY (SUB-1) = " " AND WS-REFNUM (SUB-1) = " "
              GO TO FILL-010.
              
           IF WS-APPLY (SUB-1) = "A"
            IF WS-AMTOFTYPE (SUB-1) NOT > 0
              MOVE "YOU CANNOT APPLY CASH TO A BLANK LINE."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO FILL-010.
           IF WS-APPLY (SUB-1) NOT = "A"
              MOVE "APPLY FIELD MUST BE = 'A' OR SPACE "
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO FILL-010.
           IF WS-ALREADY-APPLIED (SUB-1) = "A"
              MOVE "CASH ALREADY APPLIED TO THE DEBIT IN THIS SESSION."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE "Y" TO WS-LINE-ERROR
              GO TO FILL-010.
       FILL-020.
      **SUB-9 = TOTAL NO OF CR-TRANS****
           MOVE "                            " TO F-NAMEFIELD
           MOVE "PAYMENT" TO F-FIELDNAME
           MOVE 7         TO F-CBFIELDNAME
           MOVE 10        TO F-CBFIELDLENGTH.
           IF WS-PAYMENT (SUB-1) < WS-AMTOFTYPE (SUB-1)
               AND WS-PAYMENT (SUB-1) < WS-PAYAMT
               MOVE WS-PAYMENT (SUB-1) TO F-EDNAMEFIELDSALE
               PERFORM WRITE-FIELD-SALE.

           IF SUB-9 = 1
               AND WS-PAYAMT < WS-PAYMENT (SUB-1)
               OR WS-PAYAMT = WS-PAYMENT (SUB-1)
               MOVE WS-PAYAMT TO F-EDNAMEFIELDSALE
               PERFORM WRITE-FIELD-SALE
           ELSE
            IF SUB-9 = 1
               AND WS-PAYMENT (SUB-1) < WS-PAYAMT
               MOVE WS-PAYMENT (SUB-1) TO F-EDNAMEFIELDSALE
               PERFORM WRITE-FIELD-SALE.

           IF SUB-9 > 1
            IF WS-PAYMENT (SUB-1) < WS-PAYAMT
               MOVE WS-PAYMENT (SUB-1) TO F-EDNAMEFIELDSALE
               PERFORM WRITE-FIELD-SALE
           ELSE
             IF WS-PAYMENT (SUB-1) > WS-PAYAMT
               MOVE WS-PAYAMT TO F-EDNAMEFIELDSALE
               PERFORM WRITE-FIELD-SALE.

           IF WS-WORKTOTAL = 0
               GO TO FILL-025.

           IF SUB-9 > 1
            IF WS-PAYMENT (SUB-1) > WS-WORKTOTAL
               MOVE WS-WORKTOTAL TO WS-PAYMENT (SUB-1)
                                    F-EDNAMEFIELDSALE
               PERFORM WRITE-FIELD-SALE.
       FILL-025.
           IF CR-FOREIGN-LOCAL = "F"
            MOVE 
            "TO ENTER A FOREX LOSS RATHER ENTER AN INVOICE FOR FOREX."
               TO WS-MESSAGE
               PERFORM ERROR1-000.
       
           MOVE "DISCOUNT" TO F-FIELDNAME.
           MOVE 8          TO F-CBFIELDNAME.
           MOVE WS-DISCOUNT (SUB-1) TO F-EDNAMEFIELDSALE.
           MOVE 10         TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-SALE.

           MOVE "                             " TO F-NAMEFIELD
           MOVE "PAYMENT" TO F-FIELDNAME
           MOVE 7         TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD
           MOVE 10        TO F-CBFIELDLENGTH.

           IF F-EXIT-CH = X"01"
               MOVE "APPLY" TO F-FIELDNAME
               MOVE 5       TO F-CBFIELDNAME
               MOVE " "     TO F-NAMEFIELD WS-APPLY (SUB-1)
               MOVE 1       TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA

               MOVE "PAYMENT" TO F-FIELDNAME
               MOVE 7         TO F-CBFIELDNAME
               MOVE 0         TO WS-PAYMENT (SUB-1)
               MOVE " "       TO F-NAMEFIELD
               MOVE 10        TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA

               MOVE "DISCOUNT" TO F-FIELDNAME
               MOVE 8          TO F-CBFIELDNAME
               MOVE 0          TO WS-DISCOUNT (SUB-1)
               MOVE " "        TO F-NAMEFIELD
               MOVE 10         TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               GO TO FILL-010.

           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           IF NUMERIC-RATE > WS-PAYMENT (SUB-1)
            IF CR-FOREIGN-LOCAL = "L"
               MOVE "PAYMENT CANNOT BE > AMOUNT OUTSTANDING!!!"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "Y" TO WS-LINE-ERROR
               GO TO FILL-020.
           IF F-CBFIRSTLINE > 0
               MOVE NUMERIC-RATE TO WS-PAYMENT (SUB-1).
           PERFORM SCROLL-010.

      * NEW SECTION FOR FOREX LOSS ENTRY TO BE ALLOWED....
      * TAKEN OUT AS IT COMPLICATES THE APPLYING SECTION.
      * RATHER ENTER ANOTHER INVOICE FOR THE FOREX LOSS AMT.
      * Jun 2014.
      *     IF F-EXIT-CH = X"1D"
      *      IF CR-FOREIGN-LOCAL = "F"
      *         MOVE "Y" TO WS-FOREX-LOSS-ENTRY
      *         GO TO FILL-032.
           
           IF WS-PAYMENT (SUB-1) = WS-AMTOFTYPE (SUB-1)
            IF CR-CRTYPE NOT = 6
             IF CR-FOREIGN-LOCAL = "L"
               MOVE 0 TO WS-DISCOUNT (SUB-1)
                         WS-FOREX (SUB-1)
               GO TO FILL-850
             ELSE
               GO TO FILL-050.
               
           IF WS-PAYMENT (SUB-1) > WS-AMTOFTYPE (SUB-1)
            IF CR-FOREIGN-LOCAL = "L"
               MOVE "PAYMENT CANNOT BE > AMOUNT OF INVOICE!!!"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "Y" TO WS-LINE-ERROR
               GO TO FILL-020.
       FILL-030.
           IF WS-JNL-CR-AMT > 0
            IF CR-OLD-PAYMENT = "1"
             IF WS-DISCOUNT (SUB-1) = 0
              GO TO FILL-850.
           IF CR-CRTYPE NOT = 6 AND CR-OLD-PAYMENT = "1"
             IF WS-PAYMENT (SUB-1) = WS-AMTOFTYPE (SUB-1)
              GO TO FILL-850.
           IF CR-CRTYPE = 6
              GO TO FILL-850.
       FILL-032.
           MOVE "                   " TO F-NAMEFIELD
           MOVE "DISCOUNT" TO F-FIELDNAME
           MOVE 8          TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
               GO TO FILL-020.
           MOVE 10         TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           IF NUMERIC-RATE > WS-DISCOUNT (SUB-1)
             MOVE "YOU CAN'T ENTER MORE DISCOUNT THAN ORIGINALLY INPUT."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE "Y" TO WS-LINE-ERROR
             GO TO FILL-030.
           MOVE NUMERIC-RATE TO WS-DISCOUNT (SUB-1).
           PERFORM SCROLL-020.

           IF CR-CRTYPE = 6 AND CR-OLD-PAYMENT = "1"
              MOVE WS-PAYMENT (SUB-1) TO WS-WORK-FIELD
           ELSE
              COMPUTE WS-WORK-FIELD = WS-PAYMENT (SUB-1) +
                                      WS-DISCOUNT (SUB-1).

      *     IF WS-FOREX-LOSS-ENTRY = "Y"
      *        GO TO FILL-060.

           IF WS-WORK-FIELD = WS-AMTOFTYPE (SUB-1)
              GO TO FILL-850.
           IF WS-WORK-FIELD < WS-AMTOFTYPE (SUB-1)
            IF CR-FOREIGN-LOCAL = "L"
              GO TO FILL-850
            ELSE
              GO TO FILL-050.

           IF WS-WORK-FIELD > WS-AMTOFTYPE (SUB-1)
            IF CR-FOREIGN-LOCAL = "F"
              GO TO FILL-050. 

           MOVE "DISCOUNT AMOUNT IS TOO LARGE!!!!!!" TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
           MOVE "Y" TO WS-LINE-ERROR
           GO TO FILL-030.
       FILL-050.
            IF CR-FOREIGN-LOCAL = "L"
              GO TO FILL-850.
              
      *     IF SUB-9 = 1
      *      IF CR-FOREIGN-LOCAL = "F"
      *       IF WS-PAYAMT =
      *           WS-DISCOUNT (SUB-1) + WS-PAYMENT (SUB-1)
      *           GO TO FILL-850
      *       ELSE
      *           GO TO FILL-060.
             IF WS-AMTOFTYPE (SUB-1) =
                 WS-DISCOUNT (SUB-1) + WS-PAYMENT (SUB-1)
                 GO TO FILL-850.
       FILL-060.
           MOVE "FOREX" TO F-FIELDNAME.
           MOVE 5       TO F-CBFIELDNAME.
           
      *     IF WS-FOREX-LOSS-ENTRY = "N"
           COMPUTE WS-FOREX (SUB-1) = (WS-AMTOFTYPE (SUB-1) - 
                  (WS-PAYMENT (SUB-1) + WS-DISCOUNT (SUB-1))) * -1.

      *     IF WS-FOREX-LOSS-ENTRY = "Y"
      *     COMPUTE RUN-TOTAL = WS-PAYMENT (SUB-1)+ WS-DISCOUNT (SUB-1)
      *      COMPUTE WS-WORKTOTAL = WS-PAYAMT - RUN-TOTAL
      *       MOVE WS-WORKTOTAL TO WS-FOREX (SUB-1)
      *       MOVE 0 TO RUN-TOTAL.
                  
           MOVE WS-FOREX (SUB-1) TO F-EDNAMEFIELDFOREX.
           MOVE 10               TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-FOREX.

           MOVE "                   " TO F-NAMEFIELD
           MOVE "FOREX" TO F-FIELDNAME
           MOVE 5       TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
               GO TO FILL-030.
           MOVE 10 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           PERFORM WRITE-FIELD-ALPHA.
           MOVE NUMERIC-RATE     TO WS-FOREX (SUB-1).
           MOVE WS-FOREX (SUB-1) TO F-EDNAMEFIELDFOREX.
           MOVE 10               TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-FOREX.

           COMPUTE WS-WORK-FIELD = (WS-PAYMENT (SUB-1) +
                      WS-DISCOUNT (SUB-1)) - WS-FOREX (SUB-1).
      *     IF WS-FOREX-LOSS-ENTRY = "Y"
      *      IF WS-WORK-FIELD = WS-PAYAMT
      *        GO TO FILL-850.
           IF WS-WORK-FIELD = WS-AMTOFTYPE (SUB-1)
              GO TO FILL-850.
           IF WS-WORK-FIELD < WS-AMTOFTYPE (SUB-1)
              GO TO FILL-850.
           IF CR-CRTYPE = 6
              GO TO FILL-850.
           MOVE "FOREX AMOUNT IS NOT CORRECT!!" TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
           MOVE "Y" TO WS-LINE-ERROR
           GO TO FILL-050.
       FILL-850.
           MOVE "PAYMENT"          TO F-FIELDNAME.
           MOVE 7                  TO F-CBFIELDNAME.
           MOVE WS-PAYMENT (SUB-1) TO F-EDNAMEFIELDSALE.
           MOVE 10                 TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-SALE.

           MOVE "DISCOUNT"          TO F-FIELDNAME.
           MOVE 8                   TO F-CBFIELDNAME.
           MOVE WS-DISCOUNT (SUB-1) TO F-EDNAMEFIELDSALE.
           MOVE 10                  TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-SALE.

           MOVE "FOREX"          TO F-FIELDNAME.
           MOVE 5                TO F-CBFIELDNAME.
           MOVE WS-FOREX (SUB-1) TO F-EDNAMEFIELDFOREX.
           MOVE 10               TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-FOREX.
           
           PERFORM ERROR1-020.

      *     IF WS-FOREX-LOSS-ENTRY = "N"
           COMPUTE RUN-TOTAL = RUN-TOTAL + WS-PAYMENT (SUB-1)
            COMPUTE WS-WORKTOTAL = WS-PAYAMT 
                     - RUN-TOTAL + WS-FOREX (SUB-1).
      *     IF WS-FOREX-LOSS-ENTRY = "Y"
      *      COMPUTE WS-WORKTOTAL = WS-PAYAMT - 
      *           (WS-PAYMENT (SUB-1) + WS-FOREX (SUB-1)).
           MOVE 2840 TO POS.
           DISPLAY "REMAINING AMOUNT TO APPLY:" AT POS.
           MOVE WS-WORKTOTAL TO F-EDNAMEFIELDSALE.
           ADD 26 TO POS.
           DISPLAY F-EDNAMEFIELDSALE AT POS.

           COMPUTE WS-DIST-DISCOUNT =
                WS-DIST-DISCOUNT + WS-DISCOUNT (SUB-1).
           COMPUTE WS-FOREX-AMT =
                WS-FOREX-AMT + WS-FOREX (SUB-1).
           COMPUTE WS-WORKTOTAL = WS-SETT-DISC - WS-DIST-DISCOUNT.
           MOVE 2938 TO POS.
           DISPLAY "REMAINING DISCOUNT TO APPLY:" AT POS.
           MOVE WS-WORKTOTAL TO F-EDNAMEFIELDSALE.
           ADD 28 TO POS.
           DISPLAY F-EDNAMEFIELDSALE AT POS.

          COMPUTE WS-WORKTOTAL-DR =
                WS-AMTOFTYPE (SUB-1) + WS-FOREX (SUB-1).
          COMPUTE WS-WORKTOTAL-CR =
      *     COMPUTE WS-WORKTOTAL-DR =
                WS-PAYAMT + WS-FOREX (SUB-1).
          IF CR-CRTYPE NOT = 6
              COMPUTE WS-WORKTOTAL-CR =
                   WS-PAYMENT (SUB-1) + WS-DISCOUNT (SUB-1)
          ELSE
              MOVE WS-PAYMENT (SUB-1) TO WS-WORKTOTAL-CR.
              
      *    MOVE WS-WORKTOTAL-CR TO WS-MESSAGE
      *    PERFORM ERROR-MESSAGE
      *    MOVE WS-WORKTOTAL-DR TO WS-MESSAGE
      *    PERFORM ERROR-MESSAGE.
              
      *    IF WS-FOREX-LOSS-ENTRY = "N"
          IF WS-WORKTOTAL-CR > WS-WORKTOTAL-DR
               MOVE
               "FOREX + INVOICE < PAYMENT + DISCOUNT, MAKE A CHANGE."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               COMPUTE RUN-TOTAL = RUN-TOTAL - WS-PAYMENT (SUB-1)
               COMPUTE WS-WORKTOTAL = WS-PAYAMT + RUN-TOTAL
               COMPUTE WS-DIST-DISCOUNT =
                   WS-DIST-DISCOUNT - WS-DISCOUNT (SUB-1)
               COMPUTE WS-FOREX-AMT =
                   WS-FOREX-AMT - WS-FOREX (SUB-1)
               GO TO FILL-010.
           COMPUTE WS-WORKTOTAL = WS-PAYAMT - RUN-TOTAL.

      *     IF WS-FOREX-LOSS-ENTRY = "Y"
      *       COMPUTE RUN-TOTAL = RUN-TOTAL 
      *             - WS-PAYMENT (SUB-1) + WS-FOREX (SUB-1)
      *         COMPUTE WS-WORKTOTAL = WS-PAYAMT + RUN-TOTAL.

           IF WS-WORKTOTAL = 0
            IF WS-DIST-DISCOUNT = WS-SETT-DISC
               MOVE " " TO WS-ABOVE-BODY
               GO TO FILL-999.
           IF WS-WORKTOTAL > 0 
               GO TO FILL-900.
           IF WS-WORKTOTAL = 0
            IF WS-DIST-DISCOUNT NOT = WS-SETT-DISC
               MOVE "THE DISCOUNT ALLOCATED IS NOT =  PAYMENT-DISCOUNT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "Y" TO WS-LINE-ERROR
               GO TO FILL-010.
           MOVE "YOUR MONEY HAS RUN OUT!!!!" TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
           MOVE "Y" TO WS-LINE-ERROR
           GO TO FILL-010.
       FILL-900.
           MOVE "A" TO WS-ALREADY-APPLIED (SUB-1).
           ADD 1 TO SUB-1 F-INDEX.
           IF SUB-1 > 400
               MOVE "400 LINES ARE UP!!!!!!" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO WS-ABOVE-BODY
               GO TO FILL-999.
           IF F-INDEX < 13
           
               MOVE "N" TO WS-LINE-ERROR
           
               GO TO FILL-010.
           SUBTRACT 1 FROM SUB-1.
           IF SUB-1 < 1
              MOVE 1 TO SUB-1.
           PERFORM SCROLL-NEXT.
           MOVE 1 TO F-INDEX
           MOVE "N" TO WS-LINE-ERROR
           GO TO FILL-010.
       FILL-999.
           EXIT.
      *
       JOURNAL-DEBIT SECTION.
       JDR-010.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-CRTRANSNO    TO CRTR-TRANS.
           ADD 1                  TO GLPA-CRTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 4                 TO CRTR-TYPE.
           MOVE " "               TO CRTR-FUTURE.
           MOVE WS-CURRENTCRPER     TO CRTR-NO.
           MOVE CR-ACCOUNT-NUMBER TO CRTR-ACC-NUMBER.
           MOVE WS-BATCH          TO CRTR-REFERENCE.
           MOVE WS-CHEQUENO       TO CRTR-INV-NO
                                     CRTR-DNOTE-NO.
           MOVE WS-DATE           TO CRTR-DATE
                                     CRTR-DUE-DATE.
           MOVE WS-PAYAMT         TO CRTR-LOC-AMT
                                     CRTR-UNAPPLIED-AMT.
           MOVE 0                 TO CRTR-FOR-AMT
                                     CRTR-EXCHANGE
                                     CRTR-SETT-DISC.
           PERFORM PRINT-ROUTINE.
       JDR-015.
           WRITE CRTR-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
              GO TO JDR-010.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE 0 TO WS-CRTRANS-ST1
              MOVE "CR-TRANS BUSY JDR-015, 'ESC' TO RETRY" 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO JDR-015.
           MOVE "YOUR JRN.DR. REF:" TO WS-DAILY-1ST.
           MOVE CRTR-TRANS          TO WS-DAILY-2ND.
           MOVE " "                 TO WS-DAILY-3RD WS-DAILY-4TH.
           MOVE WS-DAILY-MESSAGE    TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
       JDR-999.
           EXIT.
      *
       JOURNAL-CREDIT SECTION.
       JCR-010.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-CRTRANSNO    TO CRTR-TRANS.
           ADD 1                  TO GLPA-CRTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 5                 TO CRTR-TYPE.
           MOVE " "               TO CRTR-FUTURE.
           MOVE WS-CURRENTCRPER     TO CRTR-NO.
           MOVE CR-ACCOUNT-NUMBER TO CRTR-ACC-NUMBER.
           MOVE WS-BATCH          TO CRTR-REFERENCE.
           MOVE WS-CHEQUENO       TO CRTR-INV-NO
                                     CRTR-DNOTE-NO.
           MOVE WS-DATE           TO CRTR-DATE
                                     CRTR-DUE-DATE.
           MOVE WS-PAYAMT         TO CRTR-LOC-AMT.
           COMPUTE CRTR-LOC-AMT = CRTR-LOC-AMT * -1.
           MOVE CRTR-LOC-AMT      TO CRTR-UNAPPLIED-AMT.
           MOVE 0                 TO CRTR-FOR-AMT
                                     CRTR-EXCHANGE
                                     CRTR-SETT-DISC.
           PERFORM PRINT-ROUTINE.
       JCR-012.
           WRITE CRTR-REC
                 INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-CRTRANS-ST1
                GO TO JCR-010.
           IF WS-CRTRANS-ST1 NOT = 0
                MOVE 0 TO WS-CRTRANS-ST1
                MOVE "CR-TRANS BUSY JCR-012, 'ESC' TO RETRY" 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO JCR-012.
           MOVE "YOUR JRN.CR. REF:" TO WS-DAILY-1ST.
           MOVE CRTR-TRANS          TO WS-DAILY-2ND.
           MOVE " "                 TO WS-DAILY-3RD WS-DAILY-4TH.
           MOVE WS-DAILY-MESSAGE TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
       JCR-999.
           EXIT.
      *
       UPDATE-OLD-CREDIT SECTION.
       CR-020.
           MOVE CR-CRTYPE  TO CRTR-TYPE.
           MOVE CR-CRTRANS TO CRTR-TRANS.
           START CRTR-FILE KEY NOT < CRTR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 NOT = 0
                MOVE 0 TO WS-CRTRANS-ST1
                MOVE "CR TRANS BUSY CR-020, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO CR-020.
       CR-030.
           READ CRTR-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE 0 TO WS-UNAPPLIED-AMT
                         WS-JNL-CR-AMT
               GO TO CR-999.
           IF WS-CRTRANS-ST1 NOT = 0
                MOVE 0 TO WS-CRTRANS-ST1
                MOVE "DR TRANS BUSY CR-030, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO CR-030.
           ADD RUN-TOTAL    TO CRTR-UNAPPLIED-AMT.
           ADD WS-SETT-DISC TO CRTR-SETT-DISC.
           PERFORM PRINT-ROUTINE.     
       CR-025.
           REWRITE CRTR-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-CRTRANS-ST1
               GO TO CR-999.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE "CR TRANS BUSY CR-025, 'ESC' TO RETRY" 
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO CR-025.
           MOVE 0 TO WS-UNAPPLIED-AMT
                     WS-JNL-CR-AMT.
       CR-035.
           PERFORM READ-CREDITORS-LOCK.
           MOVE CR-BODDATE TO WS-AGE-DATE.
           PERFORM COMPUTE-DATE-PERIOD.
       CR-040.
           IF CR-CRTYPE NOT = "6"
              MOVE WS-DATE TO CR-DATE-LAST-PAY.
           PERFORM UPDATE-OLD-BALANCE.
           PERFORM REWRITE-CREDITOR.
       CR-999.
           EXIT.
      *
       QUESTION-UNAPPLIED-AMOUNT SECTION.
       QUNAP-000.
           PERFORM CLEAR-010.
           MOVE 3053 TO POS.
           DISPLAY "Apply? (Y/N): [ ]" AT POS.
           ADD 15 TO POS.
           DISPLAY " " AT 3079 WITH BELL.
       QUNAP-010.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 67        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO ANSWER1.

      *     ACCEPT ANSWER1 AT POS.
           IF W-ESCAPE-KEY = 1 OR 2
               GO TO QUNAP-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO QUNAP-000.
       QUNAP-020.
           IF ANSWER1 NOT = "N" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO QUNAP-010.
               
           IF ANSWER1 = "Y"
            IF WS-LINE-ERROR = "Y"
              MOVE
          "FIX THE LINE ERROR BEFORE UPDATE, ENTER 'N' TO ABORT ALLOC."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO QUNAP-000.
           
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           MOVE 2810 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 3010 TO POS.
           DISPLAY "UPDATE IN PROGRESS...........            " AT POS.
       QUNAP-050.
           IF WS-DIST-DISCOUNT = WS-SETT-DISC
                GO TO QUNAP-999.
           IF ANSWER1 = "N"
                GO TO QUNAP-999.
           PERFORM ERROR-020.
           MOVE "THIS ALLOCATION HAS BEEN ABORTED AS THE ALLOCATED"
               TO WS-MESSAGE
               PERFORM ERROR1-000.
           MOVE "DISCOUNT DOES NOT EQUAL THE PAYMENT-DISCOUNT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
           MOVE "N" TO ANSWER1.
       QUNAP-999.
             EXIT.
      *
       APPLY-AMOUNTS SECTION.
       AP-000.
           IF WS-WORKTOTAL = WS-PAYAMT
              GO TO AP-999.
           MOVE 3010 TO POS.
           DISPLAY "APPLYING PAYMENTS TO CR-TRANSACTIONS.......        "
           AT POS.
           MOVE 0 TO SUB-1.   
       AP-010.     
           ADD 1 TO SUB-1.
           IF SUB-1 > 400
               GO TO AP-999.
           IF WS-REFNUM (SUB-1) = " "
               GO TO AP-999.
           IF WS-APPLY (SUB-1) = "A"
              GO TO AP-020.
           GO TO AP-010.
       AP-020.
           MOVE WS-CRTYPE (SUB-1)     TO CRTR-TYPE.
           MOVE WS-CR-TRANSNO (SUB-1) TO CRTR-TRANS.
           START CRTR-FILE KEY NOT < CRTR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE 0 TO WS-CRTRANS-ST1
              MOVE "CR-TRANS BUSY ON START AP-020, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO AP-020.
       AP-030.               
           READ CRTR-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
               GO TO AP-010.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE 0 TO WS-CRTRANS-ST1
              MOVE "CR TRANS BUSY ON READ AP-030, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO AP-030.
       AP-035.
           IF WS-TRANSTYPE = "1" OR = "3" 
                GO TO AP-037.
           IF WS-TRANSTYPE = "2"
            IF CR-CRTYPE = 2
               SUBTRACT WS-FOREX (SUB-1)    FROM WS-PAYMENT (SUB-1)
            IF CRTR-UNAPPLIED-AMT NOT <
                 WS-PAYMENT (SUB-1) + WS-DISCOUNT (SUB-1)
               SUBTRACT WS-PAYMENT (SUB-1)  FROM CRTR-UNAPPLIED-AMT
               SUBTRACT WS-DISCOUNT (SUB-1) FROM CRTR-UNAPPLIED-AMT
               ADD WS-FOREX (SUB-1)           TO WS-PAYMENT (SUB-1)
            ELSE
               MOVE 0                         TO CRTR-UNAPPLIED-AMT
               ADD WS-FOREX (SUB-1)           TO WS-PAYMENT (SUB-1).
           IF WS-TRANSTYPE = "2"
            IF CR-CRTYPE = 6
             IF CRTR-UNAPPLIED-AMT NOT <
                 WS-PAYMENT (SUB-1) + WS-DISCOUNT (SUB-1)
               SUBTRACT WS-PAYMENT (SUB-1)  FROM CRTR-UNAPPLIED-AMT
               SUBTRACT WS-DISCOUNT (SUB-1) FROM CRTR-SETT-DISC
             ELSE
               MOVE 0                         TO CRTR-UNAPPLIED-AMT
                                                 CRTR-SETT-DISC.
           PERFORM UPDATE-CREDITOR.
       AP-037.
           PERFORM PRINT-ROUTINE.
       AP-038.
           REWRITE CRTR-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
               MOVE "REWRITE CRTRANS, AP038 STATUS=2" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO AP-010.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE 0 TO WS-CRTRANS-ST1
              MOVE "CRTRANS BUSY AP-038, 'ESC' TO RETRY" 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO AP-038.
           IF WS-DISCOUNT (SUB-1) NOT = 0
            IF CR-CRTYPE = 2
              PERFORM WRITE-DISCOUNT-TRANSACTION.
           IF WS-FOREX (SUB-1) NOT = 0
            IF CR-CRTYPE = 2
              PERFORM WRITE-FOREX-TRANSACTION.
       AP-040.
           MOVE WS-REFNUM (SUB-1) TO CRTR-INV-NO.
           PERFORM READ-CRJRN-TRANSACTIONS.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
              MOVE
              "READ CRJRN-FILE (AP-040) ST1=2, ESC TO SEE TRANS #."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE CRJRN-INV-NO TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO AP-010.
           IF CR-CRTYPE = 2
               SUBTRACT WS-FOREX (SUB-1) FROM WS-PAYMENT (SUB-1)
            IF CRJRN-UNAPPLIED-AMT NOT <
                   WS-PAYMENT (SUB-1) + WS-DISCOUNT (SUB-1)
              SUBTRACT WS-PAYMENT (SUB-1)  FROM CRJRN-UNAPPLIED-AMT
              SUBTRACT WS-DISCOUNT (SUB-1) FROM CRJRN-UNAPPLIED-AMT
               ADD WS-FOREX (SUB-1)          TO WS-PAYMENT (SUB-1)
            ELSE
               ADD WS-FOREX (SUB-1)          TO WS-PAYMENT (SUB-1)
              MOVE 0                         TO CRJRN-UNAPPLIED-AMT.
              
           IF CR-CRTYPE = 5 OR = 6
            IF CRJRN-UNAPPLIED-AMT NOT <
                   WS-PAYMENT (SUB-1) + WS-DISCOUNT (SUB-1)
              SUBTRACT WS-PAYMENT (SUB-1)  FROM CRJRN-UNAPPLIED-AMT
              SUBTRACT WS-DISCOUNT (SUB-1) FROM CRJRN-SETT-DISC
            ELSE
              MOVE 0                         TO CRJRN-UNAPPLIED-AMT
                                                CRJRN-SETT-DISC.
           IF CRJRN-UNAPPLIED-AMT = 0
               MOVE "Y" TO CRJRN-COMPLETE.
           IF CR-CRTYPE = 6
              PERFORM RECALC-GLDISC.
           REWRITE CRJRN-REC.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CRJRN-FILE RECORD NOT RE-WRITTEN AS COMPLETE"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE CRJRN-INV-NO TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
           GO TO AP-010.
       AP-999.
           EXIT.
      *
       REDUCE-CRJRN-CNOTE SECTION.
       RJNCN-005.
           IF CR-CRTYPE = 5
               GO TO RJNCN-999.
           MOVE WS-CHEQUENO TO CRJRN-INV-NO.
           START CRJRN-FILE KEY NOT < CRJRN-INV-NO.
       RJNCN-010.
           READ CRJRN-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 10
              MOVE "NO CR-JRN RECORD, C/NOTE NOT REDUCED, 'ESC' TO EXIT"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE CRJRN-INV-NO TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 10 TO WS-CRJRN-ST1
              GO TO RJNCN-040.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CR-TRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CRJRN-ST1
              GO TO RJNCN-010.
           IF CRJRN-INV-NO > WS-CHEQUENO
              MOVE 23 TO WS-CRJRN-ST1
              GO TO RJNCN-040.
           IF CRJRN-INV-NO NOT = WS-CHEQUENO
              GO TO RJNCN-010.
           IF CRJRN-INV-NO = WS-CHEQUENO
            IF CRJRN-CRACC-NUMBER NOT = CR-ACCOUNT-NUMBER
              GO TO RJNCN-010.
           IF CRJRN-INV-NO = WS-CHEQUENO
            IF CRJRN-CRACC-NUMBER = CR-ACCOUNT-NUMBER
              GO TO RJNCN-040.
           GO TO RJNCN-010.
       RJNCN-040.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CRJRN-FILE RJNCN-040 ST1=23, 'ESC' TO EXIT."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE CRJRN-INV-NO TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RJNCN-999.
      *      IF CRJRN-UNAPPLIED-AMT NOT < RUN-TOTAL
      *       AND CRJRN-SETT-DISC NOT < WS-DIST-DISCOUNT
      *        SUBTRACT RUN-TOTAL FROM        CRJRN-UNAPPLIED-AMT
      *        SUBTRACT WS-DIST-DISCOUNT FROM CRJRN-SETT-DISC.
      *      ELSE
      *        MOVE 0                      TO CRJRN-UNAPPLIED-AMT
      *                                       CRJRN-SETT-DISC.
              ADD RUN-TOTAL TO        CRJRN-UNAPPLIED-AMT
              ADD WS-DIST-DISCOUNT TO CRJRN-SETT-DISC.

           IF CRJRN-UNAPPLIED-AMT = 0
               MOVE "Y" TO CRJRN-COMPLETE.
           REWRITE CRJRN-REC.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CRJRN-FILE C/NOTE NOT RE-WRITTEN AS COMPLETE."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE CRJRN-INV-NO TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
       RJNCN-999.
           EXIT.
      *
       RECALC-GLDISC SECTION.
       RGD-005.
           MOVE 0 TO SUB-2.
           MOVE 0 TO WS-CRJRN-CALC-DISC WS-CRJRN-ALLOC-DISC.
           MOVE WS-DISCOUNT (SUB-1) TO WS-CRJRN-DISC.
       RDG-010.
           ADD 1 TO SUB-2.
           IF SUB-2 > 10
              MOVE 1 TO SUB-2
              GO TO RDG-999.
           IF CRJRN-GLACC (SUB-2) = " "
              GO TO RDG-999.
           COMPUTE WS-CRJRN-CALC-DISC = 
                  WS-CRJRN-DISC - WS-CRJRN-ALLOC-DISC.
           IF WS-CRJRN-CALC-DISC NOT > CRJRN-GLDISC (SUB-2)
               SUBTRACT WS-CRJRN-CALC-DISC FROM CRJRN-GLDISC (SUB-2)
               ADD WS-CRJRN-CALC-DISC        TO WS-CRJRN-ALLOC-DISC
           ELSE
               ADD CRJRN-GLDISC (SUB-2)       TO WS-CRJRN-ALLOC-DISC
               MOVE 0                         TO CRJRN-GLDISC (SUB-2).
           IF WS-CRJRN-DISC = WS-CRJRN-ALLOC-DISC
               GO TO RDG-999.
           GO TO RDG-010.
       RDG-999.
           EXIT.
      *
       READ-CREDITOR-TRANS SECTION.
       RDT-000.
       RDT-005.
           MOVE WS-ACCOUNT-NUMBER TO CRTR-ACC-NUMBER.
           START CRTR-FILE KEY NOT < CRTR-ACC-NUMBER
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
               GO TO RDT-999.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE "CRTRANS BUSY RDT-005, PRESS 'ESC' TO RETRY" 
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDT-005.
       RDT-010.
           READ CRTR-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 10
               CLOSE CRTR-FILE
               PERFORM OPEN-014
               GO TO RDT-999.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE 
              "CR-TRANS BUSY ON READ-RDT-010, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CRTRANS-ST1
              GO TO RDT-010.
           IF CRTR-ACC-NUMBER NOT = WS-ACCOUNT-NUMBER
              GO TO RDT-999.
           IF CRTR-TYPE = 6 OR = 8
              GO TO RDT-010.
           IF CRTR-UNAPPLIED-AMT = 0
              GO TO RDT-010.
           IF CRTR-TYPE = 2 OR = 3 OR = 5
            IF CRTR-INV-NO NOT = WS-CHEQUENO
              GO TO RDT-010
            ELSE
              PERFORM FOUND-CREDIT
              GO TO RDT-010.
           MOVE CRTR-TYPE                TO WS-CRTYPE (SUB-1).
           MOVE CRTR-TRANS               TO WS-CR-TRANSNO (SUB-1).
           MOVE CRTR-INV-NO              TO WS-REFNUM (SUB-1).
           MOVE WS-TYPE-DESC (CRTR-TYPE) TO WS-TYPE (SUB-1).
           MOVE CRTR-DATE                TO WS-BODDATE (SUB-1).
           MOVE CRTR-UNAPPLIED-AMT       TO WS-AMTOFTYPE (SUB-1).
           IF CR-CRTYPE NOT = 6
              COMPUTE WS-PAYMENT (SUB-1) =
                   CRTR-UNAPPLIED-AMT - CRTR-SETT-DISC
           ELSE
              MOVE CRTR-UNAPPLIED-AMT    TO WS-PAYMENT (SUB-1).
              
           MOVE CRTR-SETT-DISC           TO WS-DISCOUNT (SUB-1).
      **SUB-9 = TOTAL NO OF CR-TRANS****
           ADD 1 TO SUB-9
                    SUB-1.
           IF SUB-1 < 401
               GO TO RDT-010.
       RDT-999.
           EXIT.
      *
       READ-FOR-OLD-CREDIT SECTION.
       RFOC-000.
           MOVE 0 TO WS-PAYAMT
                     WS-JNL-CR-AMT
                     WS-UNAPPLIED-AMT.
           MOVE " " TO CR-OLD-PAYMENT.
       RFOC-005.
           MOVE WS-ACCOUNT-NUMBER TO CRTR-ACC-NUMBER.
           START CRTR-FILE KEY NOT < CRTR-ACC-NUMBER
               INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
               GO TO RFOC-999.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE "CRTRANS BUSY RFOC-005, PRESS 'ESC' TO RETRY" 
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RFOC-005.
       RFOC-010.
           READ CRTR-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 10
               CLOSE CRTR-FILE
               PERFORM OPEN-014
               GO TO RFOC-999.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE "CR-TRANS BUSY ON READ-RFOC, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CRTRANS-ST1
              GO TO RFOC-010.
           IF CRTR-ACC-NUMBER NOT = WS-ACCOUNT-NUMBER
              GO TO RFOC-999.
           IF CRTR-UNAPPLIED-AMT = 0
              GO TO RFOC-010.
           IF CRTR-TYPE NOT = 6
              GO TO RFOC-010.
           IF CRTR-INV-NO NOT = WS-CHEQUENO
              GO TO RFOC-010
           ELSE
              PERFORM FOUND-CREDIT
              GO TO RFOC-010.
       RFOC-999.
           EXIT.
      *
       FOUND-CREDIT SECTION.
       FC-010.
           IF CRTR-TYPE = 2 OR = 3
               MOVE CRTR-UNAPPLIED-AMT   TO WS-UNAPPLIED-AMT.
           IF CRTR-TYPE = 5 OR = 6
               MOVE CRTR-UNAPPLIED-AMT   TO WS-JNL-CR-AMT.
           MOVE "1"                      TO CR-OLD-PAYMENT.
           MOVE "A"                      TO CR-APPLY.
       FC-020.
           MOVE CRTR-TYPE                TO CR-CRTYPE.
           MOVE CRTR-TRANS               TO CR-CRTRANS.
           MOVE CRTR-INV-NO              TO CR-REFNUM.
           MOVE WS-TYPE-DESC (CRTR-TYPE) TO CR-TYPE.
           MOVE CRTR-DATE                TO CR-BODDATE.
           MOVE CRTR-SETT-DISC           TO CR-DISCOUNT.       
           MOVE CRTR-UNAPPLIED-AMT       TO CR-PAYMENT
                                            CR-AMTOFTYPE.
       FC-999.
           EXIT.
      *
       READ-GLPARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       RP-010.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE "GLPARAMETER BUSY ON READ, RP-010, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RP-010.
       RP-999.
           EXIT.
      *
       READ-GLPARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 1 TO GLPA-RECORD.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       RPL-010.
           READ GLPARAMETER-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE 
               "GLPARAMETER BUSY ON READ-LOCK, RPL-010, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RPL-010.
       RPL-999.
           EXIT.
      *
       REWRITE-GLPARAMETER SECTION.
       REWP-000.
           REWRITE GLPARAMETER-REC
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE
                "GLPARAMETER BUSY ON REWRITE, REWP-000, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RP-000.
       REWP-999.
           EXIT.
      *
       WRITE-PAYMENT-TRANSACTION SECTION.
       WRPTR-000.
           MOVE 3010 TO POS.
           DISPLAY "WRITING PAYMENT TRANSACTION..........              "
           AT POS.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-CRTRANSNO    TO CRTR-TRANS
           ADD 1                  TO GLPA-CRTRANSNO
           PERFORM REWRITE-GLPARAMETER
           MOVE 2                 TO CRTR-TYPE
           MOVE " "               TO CRTR-FUTURE
           MOVE WS-CURRENTCRPER     TO CRTR-NO
           MOVE CR-ACCOUNT-NUMBER TO CRTR-ACC-NUMBER
           MOVE WS-BATCH          TO CRTR-REFERENCE
           MOVE WS-CHEQUENO       TO CRTR-INV-NO
                                     CRTR-DNOTE-NO
           MOVE WS-DATE           TO CRTR-DATE
                                     CRTR-DUE-DATE
           MOVE WS-PAYAMT         TO CRTR-LOC-AMT.
           COMPUTE CRTR-LOC-AMT = CRTR-LOC-AMT * -1.
           MOVE WS-WORKTOTAL      TO CRTR-UNAPPLIED-AMT.
           COMPUTE CRTR-UNAPPLIED-AMT = CRTR-UNAPPLIED-AMT * -1.
           MOVE 0                 TO CRTR-SETT-DISC
                                     CRTR-FOR-AMT
                                     CRTR-VAT-AMT
                                     CRTR-EXCHANGE.
           PERFORM PRINT-ROUTINE.
       WRPTR-010.
           WRITE CRTR-REC
                 INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
                 GO TO WRPTR-000.
            IF WS-CRTRANS-ST1 = 91
                MOVE 0 TO WS-CRTRANS-ST1
                MOVE 
               "TRANS FILE BUSY ON WRITE, WRPTR-010, 'ESC' TO RE-TRY" 
                   TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO WRPTR-010.
           PERFORM ERROR-020.
       WRPTR-999.
              EXIT.
      *
       WRITE-DISCOUNT-TRANSACTION SECTION.
       WRDTR-000.
             PERFORM READ-GLPARAMETER-LOCK.
             MOVE GLPA-CRTRANSNO      TO CRTR-TRANS.
             ADD 1                    TO GLPA-CRTRANSNO.
             PERFORM REWRITE-GLPARAMETER.
             MOVE 8                   TO CRTR-TYPE.
             MOVE " "                 TO CRTR-FUTURE.
             MOVE WS-CURRENTCRPER       TO CRTR-NO.
             MOVE CR-ACCOUNT-NUMBER   TO CRTR-ACC-NUMBER.
             MOVE WS-BATCH            TO CRTR-REFERENCE.
             MOVE WS-CHEQUENO         TO CRTR-INV-NO
                                         CRTR-DNOTE-NO.
             MOVE WS-DATE             TO CRTR-DATE
                                         CRTR-DUE-DATE.
             MOVE WS-DISCOUNT (SUB-1) TO CRTR-LOC-AMT.
             COMPUTE CRTR-LOC-AMT = CRTR-LOC-AMT * -1.
             MOVE 0                   TO CRTR-SETT-DISC
                                         CRTR-UNAPPLIED-AMT.
            PERFORM PRINT-ROUTINE.
       WRDTR-010.
             WRITE CRTR-REC
                 INVALID KEY NEXT SENTENCE.
            IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
                 GO TO WRDTR-000.
            IF WS-CRTRANS-ST1 NOT = 0
                MOVE 0 TO WS-CRTRANS-ST1
                MOVE "CRTRANS BUSY ON WRITE, WRDTR-010, 'ESC' TO RETRY" 
                   TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO WRDTR-010.
       WRDTR-999.
              EXIT.
      *
       WRITE-FOREX-TRANSACTION SECTION.
       WFOTR-000.
             PERFORM READ-GLPARAMETER-LOCK.
             MOVE GLPA-CRTRANSNO      TO CRTR-TRANS.
             ADD 1                    TO GLPA-CRTRANSNO.
             PERFORM REWRITE-GLPARAMETER.
             MOVE 9                   TO CRTR-TYPE.
             MOVE " "                 TO CRTR-FUTURE.
             MOVE WS-CURRENTCRPER       TO CRTR-NO.
             MOVE CR-ACCOUNT-NUMBER   TO CRTR-ACC-NUMBER.
             MOVE WS-BATCH            TO CRTR-REFERENCE.
             MOVE WS-CHEQUENO         TO CRTR-INV-NO
                                         CRTR-DNOTE-NO.
             MOVE WS-DATE             TO CRTR-DATE
                                         CRTR-DUE-DATE.
             MOVE WS-FOREX (SUB-1)    TO CRTR-LOC-AMT.
             MOVE 0                   TO CRTR-UNAPPLIED-AMT
                                         CRTR-SETT-DISC.
             PERFORM PRINT-ROUTINE.
       WFOTR-010.
             WRITE CRTR-REC
                 INVALID KEY NEXT SENTENCE.
            IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
                 GO TO WFOTR-000.
            IF WS-CRTRANS-ST1 NOT = 0
                MOVE 0 TO WS-CRTRANS-ST1
                MOVE "CRTRANS BUSY ON WRITE, WFOTR-010, 'ESC' TO RETRY" 
                   TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO WFOTR-010.
       WFOTR-999.
              EXIT.
      *
       UPDATE-CREDITOR SECTION.
       UPCR-000.
             PERFORM READ-CREDITORS-LOCK.

             IF WS-PAYAMT = WS-WORKTOTAL
                 MOVE CRTR-DATE TO WS-AGE-DATE
                 GO TO UPCR-005.
             MOVE WS-BODDATE (SUB-1) TO WS-AGE-DATE.
       UPCR-005.
             IF WS-AGE-MM = 0
                 MOVE WS-DATE TO WS-AGE-DATE.
             PERFORM COMPUTE-DATE-PERIOD.

             IF CR-OLD-PAYMENT = "1"
              IF WS-CALC-PERIOD = 0
               IF CR-CRTYPE NOT = 6
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-CURRENT
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM CR-CURRENT
                 ADD WS-FOREX (SUB-1)           TO CR-CURRENT
                 GO TO UPCR-010
               ELSE
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-CURRENT
                 GO TO UPCR-010.
             IF CR-OLD-PAYMENT = "1"
              IF WS-CALC-PERIOD = 1
               IF CR-CRTYPE NOT = 6
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-30DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM CR-30DAY
                 ADD WS-FOREX (SUB-1)           TO CR-30DAY
                 GO TO UPCR-010
               ELSE
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-30DAY
                 GO TO UPCR-010.
             IF CR-OLD-PAYMENT = "1"
              IF WS-CALC-PERIOD = 2
               IF CR-CRTYPE NOT = 6
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-60DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM CR-60DAY
                 ADD WS-FOREX (SUB-1)           TO CR-60DAY
                 GO TO UPCR-010
               ELSE
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-60DAY
                 GO TO UPCR-010.
             IF CR-OLD-PAYMENT = "1"
              IF WS-CALC-PERIOD = 3
               IF CR-CRTYPE NOT = 6
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-90DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM CR-90DAY
                 ADD WS-FOREX (SUB-1)           TO CR-90DAY
                 GO TO UPCR-010
               ELSE
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-90DAY
                 GO TO UPCR-010.
             IF CR-OLD-PAYMENT = "1"
              IF WS-CALC-PERIOD > 3
               IF CR-CRTYPE NOT = 6
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-120DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM CR-120DAY
                 ADD WS-FOREX (SUB-1)           TO CR-120DAY
                 GO TO UPCR-010
               ELSE
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-120DAY
                 GO TO UPCR-010.
             IF CR-OLD-PAYMENT = "1"
                 GO TO UPCR-999.

             IF WS-CALC-PERIOD = 0
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-CURRENT
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM CR-CURRENT
                 ADD WS-FOREX (SUB-1)           TO CR-CURRENT
                 GO TO UPCR-010.
             IF WS-CALC-PERIOD = 1
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-30DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM CR-30DAY
                 ADD WS-FOREX (SUB-1)           TO CR-30DAY
                 GO TO UPCR-010.
             IF WS-CALC-PERIOD = 2
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-60DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM CR-60DAY
                 ADD WS-FOREX (SUB-1)           TO CR-60DAY
                 GO TO UPCR-010.
             IF WS-CALC-PERIOD = 3
                 SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-90DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM CR-90DAY
                 ADD WS-FOREX (SUB-1)           TO CR-90DAY
                 GO TO UPCR-010.
             SUBTRACT WS-PAYMENT (SUB-1)  FROM CR-120DAY.
             SUBTRACT WS-DISCOUNT (SUB-1) FROM CR-120DAY.
             ADD WS-FOREX (SUB-1)           TO CR-120DAY.
       UPCR-010.
             IF CR-OLD-PAYMENT = " "
              IF CR-CRTYPE NOT = "6"
                 MOVE WS-DATE TO CR-DATE-LAST-PAY.
             IF CRTR-TYPE NOT = 3
                 COMPUTE CR-BALANCE = CR-30DAY + CR-60DAY
                    + CR-90DAY + CR-120DAY + CR-CURRENT.
       UPCR-900.
             PERFORM REWRITE-CREDITOR.
       UPCR-999.
            EXIT.
      *
       UPDATE-OLD-BALANCE SECTION.
       UOB-000.
            IF WS-CALC-PERIOD = 0
               ADD RUN-TOTAL    TO CR-CURRENT
               GO TO UOB-999.
            IF WS-CALC-PERIOD = 1
               ADD RUN-TOTAL    TO CR-30DAY
               GO TO UOB-999.
            IF WS-CALC-PERIOD = 2
               ADD RUN-TOTAL    TO CR-60DAY
               GO TO UOB-999.
            IF WS-CALC-PERIOD = 3
               ADD RUN-TOTAL    TO CR-90DAY
               GO TO UOB-999.
            IF WS-CALC-PERIOD > 3
               ADD RUN-TOTAL    TO CR-120DAY.
       UOB-999.
            EXIT.
      *
       READ-CREDITORS SECTION.
       RD-0000.
           START CREDITOR-MASTER KEY NOT < CR-KEY.
       RD-000.
           READ CREDITOR-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
               MOVE " " TO CR-NAME CR-ADDRESS1 CR-ADDRESS2
                        CR-ADDRESS3 CR-DEL-ADDRESS1 CR-DEL-ADDRESS2
                        CR-DEL-ADDRESS3
               MOVE "UNKNOWN" TO CR-NAME
               MOVE 0         TO CR-POST-CODE
               GO TO RD-999.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR FILE BUSY ON READ RD-000, 'ESC' TO RETRY" 
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RD-000.
       RD-999.
           EXIT.
      *
       READ-CREDITORS-LOCK SECTION.
       RDL-000.
           START CREDITOR-MASTER KEY NOT < CR-KEY.
       RDL-010.
           READ CREDITOR-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
               MOVE "NO SUCH CREDITOR FILE TO UPDATE, 'ESC' TO EXIT."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDL-999.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITORS BUSY RDL-010, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDL-010.
       RDL-999.
           EXIT.
      *
       REWRITE-CREDITOR SECTION.
       REWRDB-000.
             REWRITE CREDITOR-RECORD
                 INVALID KEY NEXT SENTENCE.
             IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                 MOVE "ACC.NUMBER :PAYMENT" TO WS-DAILY-1ST
                 MOVE CR-ACCOUNT-NUMBER     TO WS-DAILY-2ND
                 MOVE "NOT UPDATED"         TO WS-DAILY-3RD
                 MOVE " "                   TO WS-DAILY-4TH
                 PERFORM WRITE-DAILY.
             IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITORS BUSY ON RE-WRITE, 'ESC' TO RETRY" 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO REWRDB-000.
       REWRDB-999.
            EXIT.
      *
       READ-CRJRN-TRANSACTIONS SECTION.
       RCINV-005.
           MOVE CRTR-INV-NO TO CRJRN-INV-NO.
           START CRJRN-FILE KEY NOT < CRJRN-INV-NO.
       RCINV-010.
           READ CRJRN-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 10
              MOVE "NO CR-JRN RECORD, DISCOUNT NOT TAKEN FROM GL"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE CRJRN-INV-NO TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 23 TO WS-CRJRN-ST1
              GO TO RCINV-999.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CRJRN BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CRJRN-ST1
              GO TO RCINV-010.
           IF CRJRN-INV-NO > CRTR-INV-NO
              MOVE 23 TO WS-CRJRN-ST1
              GO TO RCINV-999.
           IF CRJRN-INV-NO NOT = CRTR-INV-NO
              GO TO RCINV-010.
           IF CRJRN-INV-NO = CRTR-INV-NO
            IF CRJRN-CRACC-NUMBER NOT = CR-ACCOUNT-NUMBER
              GO TO RCINV-010.
           IF CRJRN-INV-NO = CRTR-INV-NO
            IF CRJRN-CRACC-NUMBER = CR-ACCOUNT-NUMBER
              GO TO RCINV-999.
           GO TO RCINV-010.
       RCINV-999.
           EXIT.
      *
       WRITE-GLSETT-DISCOUNTS SECTION.
       WGLSD-000.
           IF WS-DIST-DISCOUNT = 0
               GO TO WGLSD-999.
           PERFORM ERROR-020.
           MOVE 3010 TO POS.
           DISPLAY "WRITING GL-SETTLEMENT-DISCOUNTS........"
           AT POS.
           MOVE 0 TO SUB-1.
       WGLSD-001.
           ADD 1 TO SUB-1.
           IF SUB-1 > 400
               MOVE 1 TO SUB-1
               GO TO WGLSD-900.
           IF WS-REFNUM (SUB-1) = " "
               GO TO WGLSD-900.
           IF WS-APPLY (SUB-1) = "A"
            IF WS-DISCOUNT (SUB-1) > 0
              GO TO WGLSD-002.
           GO TO WGLSD-001.
       WGLSD-002.
           MOVE WS-REFNUM (SUB-1) TO CRTR-INV-NO.
           PERFORM READ-CRJRN-TRANSACTIONS.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
               MOVE "CAN'T FIND CRJRN, GOING TO WRITE TO 50-200-05-00"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE GLPA-GLTRANSNO    TO GLTRANS-TRANS
               ADD 1                  TO GLPA-GLTRANSNO
               PERFORM REWRITE-GLPARAMETER
               MOVE WS-BATCH          TO GLTRANS-REFERENCE
               MOVE 8                 TO GLTRANS-TYPE
               MOVE WS-DATE           TO GLTRANS-DATE
               MOVE 1 TO SUB-2
               MOVE "50-200-05-00"    TO CRJRN-GLACC (SUB-2)
               MOVE "50-200-05-00"    TO GLTRANS-ACCOUNT-NUMBER
               COMPUTE GLTRANS-AMOUNT = WS-DISCOUNT (SUB-1) * -1
               MOVE "SUP"             TO WS-CR1
               MOVE CR-ACCOUNT-NUMBER TO WS-CRACC
               MOVE "DS"              TO WS-CR2
               MOVE CRTR-INV-NO       TO WS-CRINV
               MOVE LINE-DESCRIPTION  TO GLTRANS-LINE-DESC
               IF WS-CURRENTGLPER = WS-CURRENTCRPER
                  MOVE CRTR-PERIOD    TO GLTRANS-PERIOD
                  PERFORM WGLSD-015
                  PERFORM UPGL-005 THRU UPGL-010
                  SUBTRACT WS-DISCOUNT (SUB-1) FROM GL-BALANCE
                                                    GL-PER (SUB-3)
                  PERFORM UPGL-900
                  PERFORM UPGLH-005 THRU UPGLH-010
                  SUBTRACT WS-DISCOUNT (SUB-1) FROM GL-BALANCE
                                                    GL-PER (SUB-3)
                  PERFORM UPGLH-900
                  PERFORM UPGLSH-005 THRU UPGLSH-010
                  SUBTRACT WS-DISCOUNT (SUB-1) FROM GL-BALANCE
                                                    GL-PER (SUB-3)
                  PERFORM UPGLSH-900
                  GO TO WGLSD-001
               ELSE
                  MOVE "F"            TO GLTRANS-FUTURE
                  MOVE CRTR-NO        TO GLTRANS-NO
                  PERFORM WGLSD-015
                  GO TO WGLSD-001.
           MOVE 0 TO SUB-2.
           MOVE WS-DISCOUNT (SUB-1) TO WS-DISC-ALLOCATED.
       WGLSD-003.
           IF WS-DISC-ALLOCATED = 0
               GO TO WGLSD-001.
           ADD 1 TO SUB-2.
           IF SUB-2 > 10
              MOVE 1 TO SUB-2
              GO TO WGLSD-001.
           IF CRJRN-GLACC (SUB-2) > " "
            IF CRJRN-GLDISC (SUB-2) > 0
              GO TO WGLSD-010.
           GO TO WGLSD-003.
       WGLSD-010.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO       TO GLTRANS-TRANS.
           ADD 1                     TO GLPA-GLTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE WS-BATCH             TO GLTRANS-REFERENCE.
           MOVE 8                    TO GLTRANS-TYPE.
           IF WS-CURRENTGLPER = WS-CURRENTCRPER
               MOVE " "              TO GLTRANS-FUTURE
           ELSE
               MOVE "F"              TO GLTRANS-FUTURE.
           MOVE WS-CURRENTCRPER      TO GLTRANS-NO.
           MOVE WS-DATE              TO GLTRANS-DATE.
           MOVE CRJRN-GLACC (SUB-2)  TO GLTRANS-ACCOUNT-NUMBER.
           IF CRJRN-GLDISC (SUB-2) > 0
            IF CRJRN-GLDISC (SUB-2) NOT > WS-DISC-ALLOCATED
               COMPUTE GLTRANS-AMOUNT = CRJRN-GLDISC (SUB-2) * -1
               SUBTRACT CRJRN-GLDISC (SUB-2) FROM WS-DISC-ALLOCATED
               MOVE 0                TO CRJRN-GLDISC (SUB-2)
            ELSE
               COMPUTE GLTRANS-AMOUNT = WS-DISC-ALLOCATED * -1
               SUBTRACT WS-DISC-ALLOCATED    FROM CRJRN-GLDISC (SUB-2)
               MOVE 0                TO WS-DISC-ALLOCATED.
               
           MOVE "SUP"                TO WS-CR1
           MOVE CR-ACCOUNT-NUMBER    TO WS-CRACC
           MOVE "DS"                 TO WS-CR2
           MOVE CRJRN-INV-NO         TO WS-CRINV.
           MOVE LINE-DESCRIPTION     TO GLTRANS-LINE-DESC.
       WGLSD-015.
           WRITE GLTRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-GLTRANS-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS WRITE ERROR WGLSD-015, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WGLSD-015.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS FILE BUSY ON WRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WGLSD-015.
       WGLSD-020.
           IF WS-CURRENTCRPER NOT = WS-CURRENTGLPER
                   GO TO WGLSD-030.
           MOVE WS-CURRENTGLPER TO SUB-3.
           PERFORM UPGL-005 THRU UPGL-010.
           SUBTRACT CRJRN-GLDISC (SUB-2) FROM GL-BALANCE.
           SUBTRACT CRJRN-GLDISC (SUB-2) FROM GL-PER (SUB-3).
           PERFORM UPGL-900.
           PERFORM UPGLH-005 THRU UPGLH-010.
           SUBTRACT CRJRN-GLDISC (SUB-2) FROM GL-BALANCE.
           SUBTRACT CRJRN-GLDISC (SUB-2) FROM GL-PER (SUB-3).
           PERFORM UPGLH-900.
           PERFORM UPGLSH-005 THRU UPGLSH-010.
           SUBTRACT CRJRN-GLDISC (SUB-2) FROM GL-BALANCE.
           SUBTRACT CRJRN-GLDISC (SUB-2) FROM GL-PER (SUB-3).
           PERFORM UPGLSH-900.
       WGLSD-030.
           PERFORM REWRITE-CRJRN-FOR-DISC.
           GO TO WGLSD-003.
       WGLSD-900.
           PERFORM ERROR-020.
      *     UNLOCK CRJRN-FILE.
       WGLSD-999.
           EXIT.
      *
       REWRITE-CRJRN-FOR-DISC SECTION.
       RCFD-900.
           REWRITE CRJRN-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
               GO TO RCFD-999.
           IF WS-CRJRN-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE 
           "CRJRN RECORD BUSY ON RE-WRITE FOR DISC, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RCFD-900.
       RCFD-999.
           EXIT.
      *
       WRITE-GLFOREX-TRANS SECTION.
       WGL-FOREX-000.
           IF WS-FOREX-AMT = 0
               GO TO WGL-FOREX-999.
           MOVE 3010 TO POS.
           DISPLAY "WRITING GL-FOREX-TRANSACTIONS..............        "
           AT POS.
           MOVE 0 TO SUB-1.
       WGL-FOREX-001.
           ADD 1 TO SUB-1.
           IF SUB-1 > 400
               MOVE 1 TO SUB-1
               GO TO WGL-FOREX-900.
           IF WS-REFNUM (SUB-1) = " "
               GO TO WGL-FOREX-900.
           IF WS-FOREX (SUB-1) = 0
              GO TO WGL-FOREX-001.
           IF WS-APPLY (SUB-1) = " "
              GO TO WGL-FOREX-001.
       WGL-FOREX-010.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO       TO GLTRANS-TRANS.
           ADD 1                     TO GLPA-GLTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE WS-BATCH             TO GLTRANS-REFERENCE.
           MOVE 9                    TO GLTRANS-TYPE.
           IF WS-CURRENTGLPER = WS-CURRENTCRPER
               MOVE " "              TO GLTRANS-FUTURE
           ELSE
               MOVE "F"              TO GLTRANS-FUTURE.
           MOVE WS-CURRENTCRPER      TO GLTRANS-NO.
           MOVE WS-DATE              TO GLTRANS-DATE.
           IF CRJRN-GLACC (SUB-2) = "50-200-05-00" OR = "50-200-10-00"
                               OR = "75-020-15-05"
            MOVE "50-200-15-00"      TO GLTRANS-ACCOUNT-NUMBER
           ELSE
            MOVE CRJRN-GLACC (SUB-2) TO GLTRANS-ACCOUNT-NUMBER.
            
      * THE ABOVE 4 LINES ADDED TO SPLIT ALL FOREX ON PURCHASES
      * TO THE FOREX ACCOUNT SO IT CAN BE SHOWN SEPERATELY ON THE
      * INCOME STATEMENT.  24/2/2002
      
           MOVE WS-FOREX (SUB-1)     TO GLTRANS-AMOUNT.
           MOVE "SUP"                TO WS-CR1.
           MOVE CRTR-ACC-NUMBER      TO WS-CRACC.
           MOVE "FX"                 TO WS-CR2.
           MOVE WS-REFNUM (SUB-1)    TO WS-CRINV.
           MOVE LINE-DESCRIPTION     TO GLTRANS-LINE-DESC.
       WGL-FOREX-015.
           WRITE GLTRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-GLTRANS-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS ERROR WGL-FOREX-015, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WGL-FOREX-015.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS FILE BUSY ON WRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WGL-FOREX-015.
       WGL-FOREX-020.
           IF WS-CURRENTCRPER NOT = WS-CURRENTGLPER
                   GO TO WGL-FOREX-030.
           MOVE WS-CURRENTGLPER TO SUB-3.
           
           IF CRJRN-GLACC (SUB-2) = "50-200-05-00" OR = "50-200-10-00"
                               OR = "75-020-15-05"
              MOVE "50-200-15-00" TO CRJRN-GLACC (SUB-2).
           
           PERFORM UPGL-000 THRU UPGL-010.
           ADD WS-FOREX (SUB-1) TO GL-BALANCE.
           ADD WS-FOREX (SUB-1) TO GL-PER (SUB-3).
           PERFORM UPGL-900.
           PERFORM UPGLH-005 THRU UPGLH-010.
           ADD WS-FOREX (SUB-1) TO GL-BALANCE.
           ADD WS-FOREX (SUB-1) TO GL-PER (SUB-3).
           PERFORM UPGLH-900.
           PERFORM UPGLSH-005 THRU UPGLSH-010.
           ADD WS-FOREX (SUB-1) TO GL-BALANCE.
           ADD WS-FOREX (SUB-1) TO GL-PER (SUB-3).
           PERFORM UPGLSH-900.
       WGL-FOREX-030.
           GO TO WGL-FOREX-001.
       WGL-FOREX-900.
           UNLOCK CRJRN-FILE.
           PERFORM ERROR-020.
       WGL-FOREX-999.
           EXIT.
      *
       UPDATE-DISTRIBUTION SECTION.
       UPDIS-000.
            IF WS-RUN-PAYMENTS = 0
             IF WS-RUN-DEBITS = 0
              IF WS-RUN-CREDITS = 0
               IF WS-RUN-DISCOUNT = 0
                IF WS-RUN-FOREX = 0
                  GO TO UPDIS-999.
           MOVE 3010 TO POS.
           DISPLAY "UPDATING GL-CREDITORS CONTROL & BANK A/C......     "
           AT POS.
       UPDIS-950.
           PERFORM UPDATE-GL-CREDITOR-ACC.
           IF WS-CURRENTCRPER NOT = WS-CURRENTGLPER
               GO TO UPDIS-960.
           MOVE GL-NUMBER   TO WS-GLNUMBER.
           MOVE WS-GLHEADER TO GL-NUMBER.
           PERFORM UGLCA-010 THRU UGLCA-900.
           MOVE WS-HEAD-SUB TO GL-NUMBER.
           PERFORM UGLCA-010 THRU UGLCA-900.
       UPDIS-960.
           PERFORM UPDATE-GL-BANK-ACCOUNT.
           IF WS-CURRENTCRPER NOT = WS-CURRENTGLPER
               GO TO UPDIS-970.
           MOVE GL-NUMBER   TO WS-GLNUMBER.
           MOVE WS-GLHEADER TO GL-NUMBER.
           PERFORM UPGLBANK-010 THRU UPGLBANK-900.
           MOVE WS-HEAD-SUB TO GL-NUMBER.
           PERFORM UPGLBANK-010 THRU UPGLBANK-900.
       UPDIS-970.
           PERFORM ERROR-020.
       UPDIS-999.
           EXIT.
      *
       WRITE-GLTRANS SECTION.
       WRTR-010.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO      TO GLTRANS-TRANS.
           ADD 1                    TO GLPA-GLTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE WS-BATCH            TO GLTRANS-REFERENCE.
           MOVE 2                   TO GLTRANS-TYPE.
           IF WS-CURRENTGLPER = WS-CURRENTCRPER
               MOVE " "             TO GLTRANS-FUTURE
           ELSE
               MOVE "F"             TO GLTRANS-FUTURE.
           MOVE WS-CURRENTCRPER       TO GLTRANS-NO.
           MOVE CRTR-DATE           TO GLTRANS-DATE.
           MOVE CRJRN-GLACC (SUB-2) TO GLTRANS-ACCOUNT-NUMBER.
           COMPUTE GLTRANS-AMOUNT = CRJRN-GLAMT (SUB-2) * -1.
           MOVE "SUP"               TO WS-CR1.
           MOVE CRTR-ACC-NUMBER     TO WS-CRACC.
           MOVE "PY"                TO WS-CR2.
           MOVE CRTR-INV-NO         TO WS-CRINV.
           MOVE LINE-DESCRIPTION    TO GLTRANS-LINE-DESC.
       WRTR-015.
           WRITE GLTRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-GLTRANS-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS WRITE ERROR WRTR-015, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRTR-015.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS FILE BUSY ON WRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRTR-015.
       WRTR-900.
           PERFORM ERROR-020.
       WRTR-999.
           EXIT.
      *
       UPDATE-GLMASTER SECTION.
       UPGL-000.
           IF WS-CURRENTCRPER NOT = WS-CURRENTGLPER
               GO TO UPGL-999.
           MOVE WS-CURRENTGLPER TO SUB-3.
       UPGL-005.
              MOVE CRJRN-GLACC (SUB-2) TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGL-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
              MOVE "GLMASTER FILE DOES NOT EXIST, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
      *         GO TO UPGL-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLMASTER RECORD BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGL-010.
       UPGL-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLMASTER RECORD NOT WRITTEN, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLMASTER RECORD BUSY ON WRITE, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGL-900.
       UPGL-999.
           EXIT.
     *
       UPDATE-GLHEADER SECTION.
       UPGLH-005.
           MOVE CRJRN-GLACC (SUB-2) TO WS-GLNUMBER
           MOVE WS-GLHEADER         TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
              MOVE "GLHEADER FILE DOES NOT EXIST, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLHEADER FILE BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLH-010.
       UPGLH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLHEADER RECORD NOT WRITTEN, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLHEADER RECORD BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLH-900.
       UPGLH-999.
           EXIT.
     *
       UPDATE-GLSUBHEADER SECTION.
       UPGLSH-005.
           MOVE CRJRN-GLACC (SUB-2) TO WS-GLNUMBER
           MOVE WS-HEAD-SUB         TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLSH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
              MOVE "GLSUBHEAD FILE DOES'NT EXIST, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLSUBHEADER FILE BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLSH-010.
       UPGLSH-020.
           SUBTRACT CRJRN-GLDISC (SUB-2) FROM GL-BALANCE.
           SUBTRACT CRJRN-GLDISC (SUB-2) FROM GL-PER (SUB-3).
       UPGLSH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLSUBHEAD FILE NOT WRITTEN, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLSUBHEAD FILE BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLSH-900.
       UPGLSH-999.
           EXIT.
      *
       UPDATE-GL-CREDITOR-ACC SECTION.
       UGLCA-000.
           IF WS-CURRENTCRPER NOT = WS-CURRENTGLPER
               GO TO UGLCA-950.
           MOVE WS-CURRENTGLPER TO SUB-3.
           PERFORM READ-GLPARAMETER.
       UGLCA-005.
           MOVE GLPA-GLCRED-NO TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UGLCA-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLCRED-ACC DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UGLCA-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLCRED-ACC BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE GL-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO UGLCA-010.
           ADD WS-RUN-PAYMENTS      TO GL-BALANCE
                                       GL-PER (SUB-3).
           ADD WS-RUN-DISCOUNT      TO GL-BALANCE
                                       GL-PER (SUB-3).
           ADD WS-RUN-CREDITS       TO GL-BALANCE
                                       GL-PER (SUB-3).
           ADD WS-RUN-DEBITS        TO GL-BALANCE
                                       GL-PER (SUB-3).
           SUBTRACT WS-RUN-FOREX  FROM GL-BALANCE
                                       GL-PER (SUB-3).
       UGLCA-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UGLCA-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLCRED-ACC BUSY ON REWRITE, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UGLCA-900.
       UGLCA-950.
           IF WS-RUN-PAYMENTS = 0
              GO TO UGLCA-957.
       UGLCA-955.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO     TO GLTRANS-TRANS.
           ADD 1 TO GLPA-GLTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 2                  TO GLTRANS-TYPE.
           MOVE WS-BATCH           TO GLTRANS-REFERENCE.
           IF WS-CURRENTGLPER = WS-CURRENTCRPER
               MOVE " "            TO GLTRANS-FUTURE
           ELSE
               MOVE "F"            TO GLTRANS-FUTURE.
           MOVE WS-CURRENTCRPER    TO GLTRANS-NO.
           MOVE WS-DATE            TO GLTRANS-DATE.
           MOVE GLPA-GLCRED-NO     TO GLTRANS-ACCOUNT-NUMBER.
           MOVE WS-RUN-PAYMENTS    TO GLTRANS-AMOUNT.
           MOVE "PAY"              TO WS-CR1.
           MOVE WS-BATCH           TO WS-CRACC.
           MOVE "  "               TO WS-CR2. 
           MOVE "BATCH POST"       TO WS-CRINV.
           MOVE LINE-DESCRIPTION   TO GLTRANS-LINE-DESC.

           PERFORM WRTR-015.
       UGLCA-957.
           IF WS-RUN-DISCOUNT = 0
              GO TO UGLCA-960.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO     TO GLTRANS-TRANS.
           ADD 1 TO GLPA-GLTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 8                  TO GLTRANS-TYPE.
           MOVE WS-BATCH           TO GLTRANS-REFERENCE.
           IF WS-CURRENTGLPER = WS-CURRENTCRPER
               MOVE " "            TO GLTRANS-FUTURE
           ELSE
               MOVE "F"            TO GLTRANS-FUTURE.
           MOVE WS-CURRENTCRPER    TO GLTRANS-NO.
           MOVE WS-DATE            TO GLTRANS-DATE.
           MOVE GLPA-GLCRED-NO     TO GLTRANS-ACCOUNT-NUMBER.
           MOVE WS-RUN-DISCOUNT    TO GLTRANS-AMOUNT.
           MOVE "PAY"              TO WS-CR1.
           MOVE WS-BATCH           TO WS-CRACC.
           MOVE "DS"               TO WS-CR2. 
           MOVE "BATCH POST"       TO WS-CRINV.
           MOVE LINE-DESCRIPTION   TO GLTRANS-LINE-DESC.

           PERFORM WRTR-015.
       UGLCA-960.
           IF WS-RUN-DEBITS = 0
              GO TO UGLCA-965.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO     TO GLTRANS-TRANS.
           ADD 1 TO GLPA-GLTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 4                  TO GLTRANS-TYPE.
           MOVE WS-BATCH           TO GLTRANS-REFERENCE.
           IF WS-CURRENTGLPER = WS-CURRENTCRPER
               MOVE " "            TO GLTRANS-FUTURE
           ELSE
               MOVE "F"            TO GLTRANS-FUTURE.
           MOVE WS-CURRENTCRPER    TO GLTRANS-NO.
           MOVE WS-DATE            TO GLTRANS-DATE.
           MOVE GLPA-GLCRED-NO     TO GLTRANS-ACCOUNT-NUMBER.
           MOVE WS-RUN-DEBITS      TO GLTRANS-AMOUNT.
           MOVE "JRN"              TO WS-CR1.
           MOVE WS-BATCH           TO WS-CRACC.
           MOVE "DR"               TO WS-CR2. 
           MOVE "BATCH POST"       TO WS-CRINV.
           MOVE LINE-DESCRIPTION   TO GLTRANS-LINE-DESC.

           PERFORM WRTR-015.
       UGLCA-965.
           IF WS-RUN-CREDITS = 0
              GO TO UGLCA-970.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO     TO GLTRANS-TRANS.
           ADD 1 TO GLPA-GLTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 5                  TO GLTRANS-TYPE.
           MOVE WS-BATCH           TO GLTRANS-REFERENCE.
           IF WS-CURRENTGLPER = WS-CURRENTCRPER
               MOVE " "            TO GLTRANS-FUTURE
           ELSE
               MOVE "F"            TO GLTRANS-FUTURE.
           MOVE WS-CURRENTCRPER    TO GLTRANS-NO.
           MOVE WS-DATE            TO GLTRANS-DATE.
           MOVE GLPA-GLCRED-NO     TO GLTRANS-ACCOUNT-NUMBER.
           MOVE WS-RUN-CREDITS     TO GLTRANS-AMOUNT.
           MOVE "JRN"              TO WS-CR1.
           MOVE WS-BATCH           TO WS-CRACC.
           MOVE "CR"               TO WS-CR2. 
           MOVE "BATCH POST"       TO WS-CRINV.
           MOVE LINE-DESCRIPTION   TO GLTRANS-LINE-DESC.

           PERFORM WRTR-015.
       UGLCA-970.
           IF WS-RUN-FOREX = 0
              GO TO UGLCA-999.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO     TO GLTRANS-TRANS.
           ADD 1 TO GLPA-GLTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 9                  TO GLTRANS-TYPE.
           MOVE WS-BATCH           TO GLTRANS-REFERENCE.
           IF WS-CURRENTGLPER = WS-CURRENTCRPER
               MOVE " "            TO GLTRANS-FUTURE
           ELSE
               MOVE "F"            TO GLTRANS-FUTURE.
           MOVE WS-CURRENTCRPER    TO GLTRANS-NO.
           MOVE WS-DATE            TO GLTRANS-DATE.
           MOVE GLPA-GLCRED-NO     TO GLTRANS-ACCOUNT-NUMBER.
           COMPUTE GLTRANS-AMOUNT = WS-RUN-FOREX * -1.
           MOVE "PAY"              TO WS-CR1.
           MOVE WS-BATCH           TO WS-CRACC.
           MOVE "FX"               TO WS-CR2. 
           MOVE "BATCH POST"       TO WS-CRINV.
           MOVE LINE-DESCRIPTION   TO GLTRANS-LINE-DESC.

           PERFORM WRTR-015.
       UGLCA-999.
           EXIT.
      *
       UPDATE-GL-BANK-ACCOUNT SECTION.
       UPGLBANK-000.
           IF WS-RUN-PAYMENTS = 0
               GO TO UPGLBANK-999.
           IF WS-CURRENTCRPER NOT = WS-CURRENTGLPER
               GO TO UPGLBANK-955.
           MOVE WS-CURRENTGLPER TO SUB-3.
       UPGLBANK-005.
           PERFORM READ-GLPARAMETER.
           MOVE GLPA-GLBANK TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLBANK-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLBANK-ACC DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLBANK-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLBANK-ACC BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLBANK-010.
           SUBTRACT WS-RUN-PAYMENTS FROM GL-BALANCE
                                         GL-PER (SUB-3).
       UPGLBANK-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLBANK-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLBANK-ACC BUSY ON REWRITE, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLBANK-900.
       UPGLBANK-955.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO     TO GLTRANS-TRANS.
           ADD 1 TO GLPA-GLTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 2                  TO GLTRANS-TYPE.
           MOVE WS-BATCH           TO GLTRANS-REFERENCE.
           IF WS-CURRENTGLPER = WS-CURRENTCRPER
               MOVE " "            TO GLTRANS-FUTURE
           ELSE
               MOVE "F"            TO GLTRANS-FUTURE.
           MOVE WS-CURRENTCRPER    TO GLTRANS-NO.
           MOVE WS-DATE            TO GLTRANS-DATE.
           MOVE GLPA-GLBANK        TO GLTRANS-ACCOUNT-NUMBER.
           COMPUTE GLTRANS-AMOUNT = WS-RUN-PAYMENTS * -1.
           MOVE "PAY"              TO WS-CR1.
           MOVE WS-BATCH           TO WS-CRACC.
           MOVE "  "               TO WS-CR2. 
           MOVE "BATCH POST"       TO WS-CRINV.
           MOVE LINE-DESCRIPTION   TO GLTRANS-LINE-DESC.

           PERFORM WRTR-015.
       UPGLBANK-999.
           EXIT.
      *
       WRITE-CASHBOOK SECTION.
       WRCB-000.
           MOVE 2910 TO POS
           DISPLAY "WRITING CASH-BOOK ENTRY........                "
               AT POS.
           PERFORM READ-GLPARAMETER.
           MOVE GLPA-CURRENT-CRPER TO SUB-3.
           IF GLPA-CURRENT-CRPER NOT = GLPA-CURRENT-CBPER
               GO TO WRCB-030.
           MOVE GLPA-GLBANK TO CB-NUMBER.
           START CB-MASTER KEY NOT < CB-KEY.
       WRCB-010.
           READ CB-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
               MOVE "CR-BANK-ACC DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO WRCB-999.
           IF WS-CB-ST1 NOT = 0
               MOVE 0 TO WS-CB-ST1
               MOVE "CR-BANK-ACC BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO WRCB-010.
           SUBTRACT WS-PAYAMT FROM CB-BALANCE
                                   CB-PER (SUB-3).
       WRCB-020.
           REWRITE CB-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
               GO TO WRCB-999.
           IF WS-CB-ST1 NOT = 0
               MOVE 0 TO WS-CB-ST1
               MOVE "CR-BANK-ACC BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO WRCB-020.
       WRCB-030.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-CBTRANSNO     TO CBTRANS-TRANS.
           ADD 1                   TO GLPA-CBTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 1                  TO CBTRANS-TYPE.
           MOVE WS-BATCH           TO CBTRANS-REFERENCE.
           IF GLPA-CURRENT-CRPER NOT = GLPA-CURRENT-CBPER
                MOVE "F"           TO CBTRANS-FUTURE
           ELSE
                MOVE " "           TO CBTRANS-FUTURE.
           MOVE SUB-3              TO CBTRANS-NO.
           MOVE GLPA-GLBANK        TO CBTRANS-CBMASTER.
           MOVE WS-DATE            TO CBTRANS-DATE.
           MOVE "C"                TO CBTRANS-TYPE-OF-POST.
           MOVE "N"                TO CBTRANS-ALLOCATED.
           MOVE GLPA-GLCRED-NO     TO CBTRANS-ACCOUNT-NUMBER.
           MOVE WS-PAYAMT          TO CBTRANS-AMOUNT.
           COMPUTE CBTRANS-AMOUNT = CBTRANS-AMOUNT * -1.
           MOVE "A/C"              TO WS-CR1.
           MOVE CR-ACCOUNT-NUMBER  TO WS-CRACC.
           MOVE "PMT"              TO WS-CR2. 
           MOVE WS-CHEQUENO        TO WS-CRINV.
           MOVE LINE-DESCRIPTION   TO CBTRANS-LINE-DESC.

           PERFORM WRITE-CBTRANS.
           PERFORM ERROR1-020.
           PERFORM ERROR-020.
       WRCB-999.
           EXIT.
      *
       WRITE-CBTRANS SECTION.
       WCBTR-015.
           WRITE CBTRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-CBTRANS-ST1
              MOVE "CBTRANS WRITE ERROR WCBTR-015, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WCBTR-015.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE 0 TO WS-CBTRANS-ST1
              MOVE "CBTRANS FILE BUSY ON WRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WCBTR-015.
       WCBTR-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           IF WS-HAVE-PRINTED = "Y"
              GO TO PRR-008.
            MOVE "Y" TO WS-HAVE-PRINTED.
       PRR-005.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
            IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
       PRR-008.
           IF CRTR-ACC-NUMBER = 0
              GO TO PRR-999.
       PRR-010.
           IF WS-LINE-CNT < 60
              GO TO PRR-020.
           ADD 1              TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT   TO H1-PAGE
           MOVE WS-CURRENTCRPER TO H1-PERIOD
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF WS-PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD3 AFTER 2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD4 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE 7 TO WS-LINE-CNT.
       PRR-020.
           MOVE CRTR-TRANS               TO D-TRANSNO
           MOVE CRTR-TYPE                TO D-TRANSCODE
           MOVE WS-TYPE-DESC (CRTR-TYPE) TO D-TRANSDESC
           MOVE CRTR-ACC-NUMBER          TO D-ACCOUNTNO
           MOVE CRTR-REFERENCE           TO D-REFERENCE1
           MOVE CRTR-INV-NO              TO D-REFERENCE2
           MOVE CRTR-DATE                TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE             TO D-DATE
           MOVE CRTR-LOC-AMT             TO D-BEGIN
           MOVE CRTR-UNAPPLIED-AMT       TO D-OUTST.
           IF WS-TRANSTYPE = "1" OR = "3" OR = "8" OR = "9"
              MOVE 0 TO D-APPLIED
                        D-DISCOUNT.
           IF WS-TRANSTYPE = "2"
            IF CRTR-TYPE = 5 OR = 6
              MOVE RUN-TOTAL           TO D-APPLIED
              MOVE CRTR-SETT-DISC      TO D-DISCOUNT.
           IF WS-TRANSTYPE = "2"
            IF CRTR-TYPE = 2 OR = 3 OR = "5" OR = "6"
             IF CR-OLD-PAYMENT = "1"
              MOVE WS-PAYAMT           TO D-APPLIED
              MOVE CRTR-SETT-DISC      TO D-DISCOUNT
             ELSE
              COMPUTE CRTR-SETT-DISC = WS-SETT-DISC * -1
              MOVE CRTR-SETT-DISC      TO D-DISCOUNT.
           IF WS-TRANSTYPE = "2"
            IF CRTR-TYPE = 9
               MOVE 0                  TO D-DISCOUNT
               MOVE CRTR-LOC-AMT       TO D-APPLIED.
           IF WS-TRANSTYPE = "2"
            IF CRTR-TYPE = 8
               MOVE WS-DISCOUNT (SUB-1) TO D-DISCOUNT
              COMPUTE WS-WORK-FIELD = CRTR-LOC-AMT
                                    - CRTR-UNAPPLIED-AMT
              MOVE WS-WORK-FIELD        TO D-APPLIED.
           IF WS-TRANSTYPE = "2"
            IF CRTR-TYPE = 1 OR = 4 OR = 7
              COMPUTE WS-WORK-FIELD =
      *  WS-PAYMENT (SUB-1) + WS-DISCOUNT (SUB-1) + WS-FOREX (SUB-1)
            WS-PAYMENT (SUB-1) + WS-FOREX (SUB-1)
              MOVE WS-WORK-FIELD       TO D-APPLIED
              MOVE WS-DISCOUNT (SUB-1) TO D-DISCOUNT.

           IF CRTR-TYPE = 2 OR = 5
            IF CR-OLD-PAYMENT = " "
              COMPUTE WS-WORK-FIELD = CRTR-LOC-AMT
                                    - CRTR-UNAPPLIED-AMT
              MOVE WS-WORK-FIELD       TO D-APPLIED.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           ADD 1 TO WS-LINE-CNT.
       PRR-999.
           EXIT.
      *
       PRINT-TOTALS SECTION.
       PRT-000.
           IF WS-HAVE-PRINTED = "N"
              GO TO PRT-999.
           IF WS-LINE-CNT > 60
              PERFORM PRR-010.
           IF   WS-RUN-PAYMENTS = 0 AND WS-RUN-DISCOUNT = 0 
              AND WS-RUN-DEBITS = 0 AND  WS-RUN-CREDITS = 0
              AND  WS-RUN-FOREX = 0
              MOVE "**** OLD PAYMENTS  :" TO TOT-DESC
              WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
              GO TO PRT-900.
              
           MOVE "**** TOTAL PAYMENTS:" TO TOT-DESC
           COMPUTE WS-RUN-PAYMENTS = WS-RUN-PAYMENTS * -1
           MOVE WS-RUN-PAYMENTS        TO TOT-AMOUNT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
           MOVE " " TO PRINT-REC.

           MOVE "**** TOTAL DISCOUNT:" TO TOT-DESC
           COMPUTE WS-RUN-DISCOUNT = WS-RUN-DISCOUNT * -1
           MOVE WS-RUN-DISCOUNT        TO TOT-AMOUNT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC.

           MOVE "**** TOTAL DEBITS  :" TO TOT-DESC
           MOVE WS-RUN-DEBITS          TO TOT-AMOUNT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC.

           MOVE "**** TOTAL CREDITS :" TO TOT-DESC
           MOVE WS-RUN-CREDITS         TO TOT-AMOUNT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC.

           MOVE "**** TOTAL FOREX   :" TO TOT-DESC
           MOVE WS-RUN-FOREX           TO TOT-AMOUNT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
        PRT-900.
           MOVE WS-BATCH               TO TOT-DESC
           MOVE 0                      TO TOT-AMOUNT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
           
           MOVE " " TO PRINT-REC.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
        PRT-999.
           EXIT.
      *
       SCROLL-NEXT SECTION.
       NEXT-000.
            PERFORM CLEAR-BODY.
            ADD 1 TO SUB-1.
            IF SUB-1 > SUB-9
               MOVE SUB-9 TO SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 > 389
                 MOVE 389 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 13
                GO TO NEXT-010.
            MOVE 1 TO F-INDEX.
            SUBTRACT 12 FROM SUB-1.
       NEXT-025.
            COMPUTE F-INDEX = (400 - SUB-1) + 1.
            IF F-INDEX > 12
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3015 TO POS.
            DISPLAY "1ST BODY LINE: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.

            IF WS-TYPEOFPROCESS NOT = "3" OR SUB-1 > 399
                GO TO NEXT-999.
            MOVE "APPLY" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"0C"
                ADD 10 TO SUB-1
                GO TO NEXT-000.
            IF F-EXIT-CH = X"05"
                PERFORM SCROLL-PREVIOUS
                GO TO NEXT-000.
       NEXT-999.
             EXIT.
      *
       SCROLL-NEXT-PAGE SECTION.
       NEXT-PAGE-000.
            PERFORM CLEAR-BODY.
            ADD 12 TO SUB-1.
            IF SUB-1 > SUB-9
               MOVE SUB-9 TO SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 > 389
                 MOVE 389 TO SUB-1.
       NEXT-PAGE-010.
            PERFORM SCROLLING.
       NEXT-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 13
                GO TO NEXT-PAGE-010.
            MOVE 1 TO F-INDEX.
            SUBTRACT 12 FROM SUB-1.
       NEXT-PAGE-025.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.
            MOVE 3015 TO POS.
            DISPLAY "1ST BODY LINE: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-PAGE-999.
             EXIT.
      *
       SCROLL-PREVIOUS SECTION.
       PREV-000.
            PERFORM CLEAR-BODY.
            SUBTRACT 12 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-010.
            PERFORM SCROLLING.
       PREV-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 13
                GO TO PREV-010.
            MOVE 1 TO F-INDEX.
            SUBTRACT 12 FROM SUB-1.
       PREV-025.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.
            MOVE 3015 TO POS.
            DISPLAY "1ST BODY LINE: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       PREV-999.
            EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
           IF WS-BODDATENUM (SUB-1) = 0 OR = " "
               GO TO SCROLL-999.
           IF WS-REFNUM (SUB-1) = " "
               GO TO SCROLL-999.
           MOVE "APPLY"          TO F-FIELDNAME
           MOVE 5                TO F-CBFIELDNAME
           MOVE WS-APPLY (SUB-1) TO F-NAMEFIELD
           MOVE 1                TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "REFNUM"          TO F-FIELDNAME
           MOVE 6                 TO F-CBFIELDNAME
           MOVE WS-REFNUM (SUB-1) TO F-NAMEFIELD
           MOVE 10                TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "TYPE"          TO F-FIELDNAME
           MOVE 4               TO F-CBFIELDNAME
           MOVE WS-TYPE (SUB-1) TO F-NAMEFIELD
           MOVE 7               TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           IF WS-BODDATENUM (SUB-1) = 0
               GO TO SCROLL-010.
           MOVE "DATE"                TO F-FIELDNAME
           MOVE 4                     TO F-CBFIELDNAME
           MOVE WS-BODDATENUM (SUB-1) TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE          TO F-NAMEFIELD
           MOVE 10                    TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       SCROLL-010.
           MOVE "AMTOFTYPE"          TO F-FIELDNAME
           MOVE 9                    TO F-CBFIELDNAME
           MOVE WS-AMTOFTYPE (SUB-1) TO F-EDNAMEFIELDSALE
           MOVE 10                   TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-SALE.

           IF WS-APPLY (SUB-1) = " "
      *        GO TO SCROLL-999.
              GO TO SCROLL-020.
           MOVE "PAYMENT"          TO F-FIELDNAME
           MOVE 7                  TO F-CBFIELDNAME
           MOVE WS-PAYMENT (SUB-1) TO F-EDNAMEFIELDSALE
           MOVE 10                 TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-SALE.
       SCROLL-020.
           MOVE "DISCOUNT"          TO F-FIELDNAME
           MOVE 8                   TO F-CBFIELDNAME
           MOVE WS-DISCOUNT (SUB-1) TO F-EDNAMEFIELDSALE
           MOVE 10                  TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-SALE.
       SCROLL-030.
           MOVE "FOREX"          TO F-FIELDNAME
           MOVE 5                TO F-CBFIELDNAME
           MOVE WS-FOREX (SUB-1) TO F-EDNAMEFIELDFOREX
           MOVE 10               TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-FOREX.
       SCROLL-999.
             EXIT.
      *
       CLEAR-BODY SECTION.
       CLEAR-BODY-000.
            MOVE 1 TO F-INDEX.
       CLEAR-BODY-010.
            MOVE "APPLY" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 1       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "REFNUM" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 6        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "TYPE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 7      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DATE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 10     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "AMTOFTYPE" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PAYMENT" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 10        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "DISCOUNT" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 10         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "FOREX" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE " "     TO F-EDNAMEFIELDFOREX
            MOVE 10      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            ADD 1 TO F-INDEX.
            IF F-INDEX < 13
             GO TO CLEAR-BODY-010.
       CLEAR-BODY-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
             MOVE 2930 TO POS
             MOVE " " TO WS-MESSAGE
             DISPLAY WS-MESSAGE AT POS.
             MOVE 1 TO SUB-1.
             MOVE 0 TO WS-PAYAMT
                       RUN-TOTAL
                       WS-WORKTOTAL
                       WS-UNAPPLIED-AMT
                       WS-JNL-CR-AMT.
       CF-010.
             MOVE " " TO WS-APPLY (SUB-1)
                         WS-ALREADY-APPLIED (SUB-1)
                         WS-REFNUM (SUB-1)
                         WS-TYPE (SUB-1).
             MOVE 0   TO WS-BODDATE (SUB-1)
                         WS-AMTOFTYPE (SUB-1)
                         WS-PAYMENT (SUB-1)
                         WS-DISCOUNT (SUB-1)
                         WS-FOREX (SUB-1).
             ADD 1 TO SUB-1.

             IF SUB-1 < 401
                 GO TO CF-010.
       CF-020.
             MOVE 0 TO CR-AMTOFTYPE
                       CR-REFNUM
                       CR-BODDATE
                       CR-PAYMENT
                       CR-CRTYPE 
                       CR-CRTRANS.
             MOVE " " TO CR-APPLY
                         CR-TYPE
                         CR-OLD-PAYMENT.
       CF-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-011.
           OPEN I-O CREDITOR-MASTER.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITORS FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-011.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-012.
       OPEN-0121.
           PERFORM READ-GLPARAMETER.
           MOVE GLPA-NAME          TO CO-NAME
           MOVE GLPA-CURRENT-GLPER TO WS-CURRENTGLPER
           MOVE GLPA-CURRENT-CRPER TO WS-CURRENTCRPER
           PERFORM ENTER-PERIOD-DATES.
       OPEN-013.
           OPEN I-O GLTRANS-FILE.
           IF WS-GLTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GL-TRANS BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-013.
       OPEN-014.
           OPEN I-O CRTR-FILE.
           IF WS-CRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRTRANS-ST1
               MOVE "CR-TRANS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-014.
       OPEN-015.
           OPEN I-O CRJRN-FILE.
           IF WS-CRJRN-ST1 NOT = 0 
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CRJRN FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-015.
       OPEN-016.
           OPEN I-O CB-MASTER.
           IF WS-CB-ST1 NOT = 0
               MOVE 0 TO WS-CB-ST1
               MOVE "CBMASTER FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-016.
       OPEN-017.
           OPEN I-O CBTRANS-FILE.
           IF WS-CBTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-CBTRANS-ST1
              MOVE "CB-TRANS BUSY ON OPEN, PRESS 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-017.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-030.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrPaymnt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF WS-HAVE-PRINTED = "Y"
              PERFORM UPDATE-DISTRIBUTION
              PERFORM PRINT-TOTALS
              CLOSE PRINT-FILE
              PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE CREDITOR-MASTER
                 GL-MASTER
                 CRTR-FILE
                 GLTRANS-FILE
                 CRJRN-FILE
                 GLPARAMETER-FILE 
                 CB-MASTER
                 CBTRANS-FILE.
       END-900.
      *     STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldSale".
       Copy "WriteFieldForex".
       Copy "WriteFieldNumeric".
       Copy "EnterPeriodDates".
       Copy "ComputeCRDatePeriod".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
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
       Copy "CTOSCobolAccept".
      * END-OF-JOB
