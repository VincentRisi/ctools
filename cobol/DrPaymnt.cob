        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrPaymnt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrTrans".
         Copy "SelectSlParameter".
         Copy "SelectSlDistributions".
         Copy "SelectGlParameter".
         Copy "SelectCbMaster".
         Copy "SelectCbTrans".
         Copy "SelectGlTrans".
         Copy "SelectGlMaster".
         Copy "SelectSlDaily".
           SELECT PRINT-FILE ASSIGN TO Ws-Printer
               ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdDaily.
           COPY ChlfdParam.
           COPY ChlfdDrTrans.
           COPY ChlfdDisTot.
           COPY ChlfdGlParam.
           COPY ChlfdCbMast.
           COPY ChlfdCbTrans.
           COPY ChlfdGlMast.
           COPY ChlfdGlTrans.

       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-1STPRINT          PIC X VALUE " ".
       77  WS-LINE-CNT          PIC 9(3) VALUE 66.
       77  WS-PAGE-CNT          PIC 9(3) VALUE 0.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "DrAcStIq".
       77  WS-INVOICE-INQUIRY   PIC X(8) VALUE "SlIRegIq".
       77  WS-ACCOUNT-NUMBER    PIC 9(7) VALUE 0.
       77  WS-CHEQUENO          PIC X(6) VALUE " ".
       77  WS-PAYAMT            PIC 9(7)V99 VALUE 0.
       77  WS-TRANSTYPE         PIC X VALUE " ".
       77  WS-TYPEOFPROCESS     PIC X VALUE " ".
       77  WS-CURRENTPER        PIC 99 VALUE 0.
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-BATCH-TOTAL       PIC 9(7)V99 VALUE 0.
       77  WS-WORKTOTAL         PIC S9(7)V99 VALUE 0.
       77  WS-BODY-LINE         PIC ZZ9.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-PAGE              PIC 9 VALUE 0.
       77  WS-ANSWER1           PIC X VALUE " ".
       77  WS-WORK-FIELD        PIC S9(7)V99 VALUE 0.
       77  WS-WRITE             PIC X VALUE " ".
       77  WS-INVALID-NUMERIC   PIC 9 VALUE 0.
       77  WS-MONTH             PIC 999 VALUE 0.
       77  RUN-TOTAL            PIC 9(8)V99 VALUE 0.
       77  WS-JNL-CR-AMT        PIC 9(7)V99 VALUE 0.
       77  WS-UNAPPLIED-AMT     PIC 9(7)V99 VALUE 0.
       77  WS-RUN-PAYMENTS      PIC 9(7)V99 VALUE 0.
       77  WS-RUN-DISCOUNT      PIC 9(7)V99 VALUE 0.
       77  WS-RUN-DEBITS        PIC 9(7)V99 VALUE 0.
       77  WS-RUN-CREDITS       PIC 9(7)V99 VALUE 0.
       77  WS-RUN-RDCHEQUE      PIC 9(7)V99 VALUE 0.
       77  WS-RUN-BDEBT         PIC 9(7)V99 VALUE 0.
       77  WS-RUN-REFUND        PIC 9(7)V99 VALUE 0.
       77  WS-RUN-INTEREST      PIC 9(7)V99 VALUE 0.
       77  WS-PasswordSaved     Pic X(10).
       77  PSW-SUB1             PIC S9(5) VALUE 0.
       77  PSW-SUB2             PIC S9(5) VALUE 0.
       01  W-READ-KEY           PIC X(20).
       01  WS-PASSWORD-KEY.
           03  WS-PA-KEY         PIC X OCCURS 11.
       01  WS-BATCH.
           03  WS-BATCH-1STCHAR PIC X(2) VALUE "DC".
           03  WS-BATCH-REST    PIC X(8).
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1         PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1          PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1    PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1        PIC 99.
       01  WS-DISTRIBUTION-STATUS.
           03  WS-DISTRIBUTION-ST1   PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1    PIC 99.
       01  WS-CBTRANS-STATUS.
           03  WS-CBTRANS-ST1        PIC 99.
       01  WS-CB-STATUS.
           03  WS-CB-ST1             PIC 99.
       01  WS-GLTRANS-STATUS.
           03  WS-GLTRANS-ST1        PIC 99.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1         PIC 99.
       01  WS-DIST-TOTALS.
           03  WS-DIST-PAYMENT    PIC 9(7)V99 VALUE 0.
           03  WS-DIST-RDCHEQUE   PIC 9(7)V99 VALUE 0.
           03  WS-DIST-JOURNALDR  PIC 9(7)V99 VALUE 0.
           03  WS-DIST-JOURNALCR  PIC 9(7)V99 VALUE 0.
           03  WS-DIST-DISCOUNT   PIC 9(7)V99 VALUE 0.
           03  WS-DIST-BDEBT      PIC 9(7)V99 VALUE 0.
           03  WS-DIST-INTEREST   PIC 9(7)V99 VALUE 0.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-GLHEADER     PIC X(2).
               05  WS-GLSUBHEADER  PIC X(4).
           03  WS-RESTOFACCOUNT    PIC X(6).
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 751.
               05  WS-APPLY           PIC X.
               05  WS-ALREADY-APPLIED PIC X.
               05  WS-REFNUM          PIC 9(6).
               05  WS-TYPE            PIC X(7).
               05  WS-BODDATE.
                   07  WS-BOD-YY     PIC 9999.
                   07  WS-BOD-MM     PIC 99.
                   07  WS-BOD-DD     PIC 99.
               05  WS-BODDATE-RED REDEFINES WS-BODDATE.
                   07  WS-BODDATENUM PIC 9(8).
               05  WS-AMTOFTYPE      PIC 9(7)V99.
               05  WS-PAYMENT        PIC 9(7)V99.
               05  WS-DISCOUNT       PIC 9(7)V99.
               05  WS-DRTYPE         PIC 99.
               05  WS-DR-TRANSNO     PIC 9(6).
       01  WS-PRINTER-INFO.
           03  WS-PRN-FIL     PIC X(8) VALUE " ".
           03  WS-PRN-NAME    PIC X(25) VALUE " ".
       01  CREDIT-REC.
           03  CR-APPLY      PIC X.
           03  CR-REFNUM     PIC 9(6).
           03  CR-TYPE       PIC X(7).
           03  CR-BODDATE.
               05  CR-BOD-YY     PIC 9999.
               05  CR-BOD-MM     PIC 99.
               05  CR-BOD-DD     PIC 99.
           03  CR-BODDATE-RED REDEFINES CR-BODDATE.
               05  CR-BODDATENUM PIC 9(8).
           03  CR-AMTOFTYPE      PIC 9(7)V99.
           03  CR-PAYMENT        PIC 9(7)V99.
           03  CR-OLD-PAYMENT    PIC X.
           03  CR-DRTYPE         PIC 99.
           03  CR-DR-TRANSNO     PIC 9(6).
       01  LINE-DESCRIPTION.
           03  WS-CR1              PIC X(4).
           03  WS-CRACC.
               05  WS-CRACC1       PIC X(8).
               05  WS-CR2          PIC X(3).
           03  WS-CRINV            PIC X(10).
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       01  ALPHA-RATE8          PIC X(8).
       01  WS-TYPES.
           03  FILLER          PIC X(7) VALUE "INVOICE".
           03  FILLER          PIC X(7) VALUE "PAYMENT".
           03  FILLER          PIC X(7) VALUE "R/D CHQ".
           03  FILLER          PIC X(7) VALUE "JRN.DR.".
           03  FILLER          PIC X(7) VALUE "JRN.CR.".
           03  FILLER          PIC X(7) VALUE "C/NOTE ".
           03  FILLER          PIC X(7) VALUE "INTREST".
           03  FILLER          PIC X(7) VALUE "DISCNT.".
           03  FILLER          PIC X(7) VALUE "B-DEBT.".
           03  FILLER          PIC X(7) VALUE "CH REF.".
           03  FILLER          PIC X(7) VALUE "INT REV".
       01  WS-TYPES-RED REDEFINES WS-TYPES.
           03  WS-TYPE-DESC    PIC X(7) OCCURS 11.
      * 01  FIND-SIGN.
      *     03  F-SIGN-OCC      PIC X OCCURS 5.
      * 01  FIND-SIGN-POS       PIC X(5) REDEFINES FIND-SIGN.
      *     03  FILLER          PIC X(5).
      * 01  FIND-SIGN-MIN REDEFINES FIND-SIGN-POS.
      *     03  FILLER          PIC X.
      *     03  FIND-SIGN-MIN-4 PIC X(4).
       01  HEAD1.
           03  FILLER          PIC X(5) VALUE "DATE".
           03  H1-DATE         PIC X(10).
           03  FILLER          PIC X(20) VALUE " ".
           03  FILLER          PIC X(50) VALUE
           "** DEBTOR PAYMENT ALLOCATIONS AUDIT TRAIL **".
           03  FILLER          PIC X(14) VALUE "PAYMENT DATE:".
           03  H1-PAY-DATE     PIC X(10).
           03  FILLER          PIC X(13) VALUE " ".
           03  FILLER          PIC X(5) VALUE "PAGE:".
           03  H1-PAGE         PIC Z9.
           03  FILLER          PIC X(1) VALUE " ".
       01  HEAD2.
           03  FILLER          PIC X(35) VALUE " ".
           03  FILLER          PIC X(44) VALUE ALL "*".
           03  FILLER          PIC X(53) VALUE " ".
       01  HEAD3.
           03  FILLER          PIC X(10) VALUE "ACCOUNT".
           03  FILLER          PIC X(9) VALUE "TRANS.".
           03  FILLER          PIC X(11) VALUE "TRANS.".
           03  FILLER          PIC X(10) VALUE "TRANS.".
           03  FILLER          PIC X(26) VALUE "REFERENCE 1".
           03  FILLER          PIC X(14) VALUE "REF.2".
           03  FILLER          PIC X(13) VALUE "DATE".
           03  FILLER          PIC X(11) VALUE "BEGIN".
           03  FILLER          PIC X(14) VALUE "AMOUNT".
           03  FILLER          PIC X(14) VALUE " ".
       01  HEAD4.
           03  FILLER          PIC X(11) VALUE "NUMBER".
           03  FILLER          PIC X(20) VALUE "CODE     DESC.".
           03  FILLER          PIC X(61) VALUE "NO.".
           03  FILLER          PIC X(12) VALUE "AMOUNT".
           03  FILLER          PIC X(9) VALUE "REMAIN".
           03  FILLER          PIC X(11) VALUE "DISCOUNT".
           03  FILLER          PIC X(10) VALUE "APPLIED".
       01  DETAIL-LINE.
           03  D-ACCOUNTNO     PIC X(7).
           03  FILLER          PIC X(5) VALUE " ".
           03  D-TRANSCODE     PIC 99.
           03  FILLER          PIC X(4) VALUE " ".
           03  D-TRANSDESC     PIC X(11).
           03  D-TRANSNO       PIC Z(5)9.
           03  FILLER          PIC X(5) VALUE " ".
           03  D-REFERENCE1    PIC X(25).
           03  D-REFERENCE2    PIC Z(5)9.
           03  FILLER          PIC X(5) VALUE " ".
           03  D-DATE          PIC X(10).
           03  FILLER          PIC X(1) VALUE " ".
           03  D-BEGIN         PIC Z(6)9.99.
           03  FILLER          PIC X(2) VALUE " ".
           03  D-OUTST         PIC Z(6)9.99.
           03  FILLER          PIC X(1) VALUE " ".
           03  D-DISCOUNT      PIC Z(6)9.99.
           03  FILLER          PIC X(1) VALUE " ".
           03  D-APPLIED       PIC Z(6)9.99.
           03  FILLER          PIC X(3) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER          PIC X(23) VALUE " ".
           03  TOT-DESC        PIC X(20) VALUE " ".
           03  FILLER          PIC X(2) VALUE " ".
           03  TOT-AMOUNT      PIC Z(6)9.99.
           03  FILLER          PIC X(82) VALUE " ".
       01  TOTAL2-LINE.
           03  FILLER          PIC X(23) VALUE " ".
           03  TOT2-DESC       PIC X(20) VALUE " ".
           03  FILLER          PIC X(2) VALUE " ".
           03  TOT-NAME        PIC X(10).
           03  FILLER          PIC X(82) VALUE " ".
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
           PERFORM CLEAR-SCREEN
           PERFORM OPEN-FILES
      *     PERFORM CLEAR-SCREEN
           MOVE "N" TO WS-1STPRINT.
           MOVE 0310 TO POS
           DISPLAY "** DEBTOR PAYMENT ALLOCATIONS PROGRAM **" AT POS
           MOVE 0410 TO POS
           DISPLAY "****************************************" AT POS.
           Copy "PrinterAcceptDr".
       CONT-010.
           PERFORM DISPLAY-FORM.
           
           PERFORM GET-001.
           
           PERFORM DISPLAY-TOP-INFO.
           PERFORM ERROR1-020.
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
           PERFORM ERROR1-020.
           PERFORM ERROR-020.
           
      *
      * REF1 : ORDER NUMBER
      * REF2 : OUTST.INV.NO./CASH RECEIPT NO./JOURNAL NO.
      *DRTR-TYPE:1=INV(+),             2=CASH(-),   
      *          3=R/D CHEQUE(+),      5=CR-JRN(-),
      *          4=DR-JRN(+)           6=C/NOTE(-),
      *          7=INTEREST(+),        8=DISC(-),
      *         10=CHEQUE REFUND(+)    9=B-DEBT(-),
      *                               11=INTEREST REV(-)           
      *
      *  WS-TRANSTYPE = THE ENTERED PROCESS NUMBER BY USER (FIELD 2)
      *
           IF WS-ANSWER1 NOT = "Y"
                GO TO CONT-010.
           IF WS-TYPEOFPROCESS = "3"
                GO TO CONT-010.

           IF WS-TYPEOFPROCESS = "2"
             IF CR-OLD-PAYMENT = " "
                PERFORM WRITE-PAYMENT-TRANSACTION
                PERFORM APPLY-AMOUNTS
                ADD WS-PAYAMT  TO WS-RUN-PAYMENTS
                MOVE WS-PAYAMT TO WS-DIST-PAYMENT
                PERFORM UPDATE-DISTRIBUTION
                MOVE " " TO PRINT-REC
                WRITE PRINT-REC
                ADD 1 TO WS-LINE-CNT
               IF WS-WORKTOTAL NOT = 0
                  PERFORM READ-DEBTORS-LOCK
                  SUBTRACT WS-WORKTOTAL FROM DR-BALANCE
                                             DR-CURRENT
                  PERFORM REWRITE-DEBTOR
                  GO TO CONT-010
               ELSE
                  GO TO CONT-010.

           IF WS-TYPEOFPROCESS = "2"
             IF CR-OLD-PAYMENT = "1"
                PERFORM UPDATE-OLD-CREDIT
                PERFORM APPLY-AMOUNTS
                PERFORM UPDATE-DISTRIBUTION
                MOVE " " TO PRINT-REC
                WRITE PRINT-REC
                ADD 1 TO WS-LINE-CNT
                GO TO CONT-010.

           IF WS-TRANSTYPE = "1"
            IF WS-TYPEOFPROCESS = " "
               PERFORM JOURNAL-DEBIT
               PERFORM READ-DEBTORS-LOCK
               ADD WS-PAYAMT TO DR-BALANCE
               ADD WS-PAYAMT TO DR-CURRENT
               ADD WS-PAYAMT TO WS-RUN-DEBITS
               PERFORM REWRITE-DEBTOR
               ADD WS-PAYAMT TO WS-DIST-JOURNALDR
               PERFORM UPDATE-DISTRIBUTION
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               ADD 1 TO WS-LINE-CNT
               GO TO CONT-010.
                
          IF WS-TRANSTYPE = "3"
           IF WS-TYPEOFPROCESS = " "
               PERFORM JOURNAL-CREDIT
               PERFORM READ-DEBTORS-LOCK
               SUBTRACT WS-PAYAMT FROM DR-BALANCE
               SUBTRACT WS-PAYAMT FROM DR-CURRENT
               ADD WS-PAYAMT TO WS-RUN-CREDITS
               PERFORM REWRITE-DEBTOR
               ADD WS-PAYAMT TO WS-DIST-JOURNALCR
               PERFORM UPDATE-DISTRIBUTION
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               ADD 1 TO WS-LINE-CNT
               GO TO CONT-010.

           IF WS-TRANSTYPE = "4"
            IF WS-TYPEOFPROCESS = " "
               PERFORM RD-CHEQUE
               PERFORM WRITE-RD-CASHBOOK
               PERFORM MAKE-DETAILS
               MOVE GLPA-GLDRBANK TO GL-NUMBER WS-GLNUMBER
               PERFORM UPDATE-GLMASTER
               PERFORM UPDATE-GLHEADER
               PERFORM UPDATE-GLSUBHEADER
               PERFORM MAKE-DRACC-DETAILS
               MOVE GLPA-GLDEBT-NO TO GL-NUMBER WS-GLNUMBER
               PERFORM UPDATE-GLMASTER
               PERFORM UPDATE-GLHEADER
               PERFORM UPDATE-GLSUBHEADER
               PERFORM READ-DEBTORS-LOCK
               ADD WS-PAYAMT TO DR-BALANCE
               ADD WS-PAYAMT TO DR-CURRENT
               ADD WS-PAYAMT TO WS-RUN-RDCHEQUE
               PERFORM REWRITE-DEBTOR
               ADD WS-PAYAMT TO WS-DIST-RDCHEQUE
               PERFORM UPDATE-DISTRIBUTION
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               ADD 1 TO WS-LINE-CNT
               GO TO CONT-010.

          IF WS-TRANSTYPE = "5"
           IF WS-TYPEOFPROCESS = " "
               PERFORM BAD-DEBT
               PERFORM MAKE-DETAILS
               MOVE GLPA-GLBDEBT-ACC TO GL-NUMBER WS-GLNUMBER
               PERFORM UPDATE-GLMASTER
               PERFORM UPDATE-GLHEADER
               PERFORM UPDATE-GLSUBHEADER
               PERFORM MAKE-DRACC-DETAILS
               MOVE GLPA-GLDEBT-NO TO GL-NUMBER WS-GLNUMBER
               PERFORM UPDATE-GLMASTER
               PERFORM UPDATE-GLHEADER
               PERFORM UPDATE-GLSUBHEADER
               PERFORM READ-DEBTORS-LOCK
               SUBTRACT WS-PAYAMT FROM DR-BALANCE
               SUBTRACT WS-PAYAMT FROM DR-CURRENT
      *
      * THE NEXT TWO LINES WERE ADDED TO MAKE THE SALESMAN PAY FOR
      * THE B/DEBT BY REDUCING HIS COMM FOR THE MONTH AND YEAR. 4/4/2000
      *
               SUBTRACT WS-PAYAMT FROM DR-SALES-PTD
               SUBTRACT WS-PAYAMT FROM DR-SALES-YTD
               ADD WS-PAYAMT TO WS-RUN-BDEBT
               PERFORM REWRITE-DEBTOR
               ADD WS-PAYAMT TO WS-DIST-BDEBT
               PERFORM UPDATE-DISTRIBUTION
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               ADD 1 TO WS-LINE-CNT
               GO TO CONT-010.

           IF WS-TRANSTYPE = "6"
            IF WS-TYPEOFPROCESS = " "
               PERFORM CHEQUE-REFUND
               PERFORM WRITE-REF-CASHBOOK
               PERFORM READ-DEBTORS-LOCK
               ADD WS-PAYAMT TO DR-BALANCE
               ADD WS-PAYAMT TO DR-CURRENT
               ADD WS-PAYAMT TO WS-RUN-REFUND
               PERFORM REWRITE-DEBTOR
               ADD WS-PAYAMT TO WS-DIST-JOURNALDR
               PERFORM UPDATE-DISTRIBUTION
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               ADD 1 TO WS-LINE-CNT
               GO TO CONT-010.

          IF WS-TRANSTYPE = "7"
           IF WS-TYPEOFPROCESS = " "
               PERFORM REVERSE-INTEREST
               PERFORM READ-DEBTORS-LOCK
               SUBTRACT WS-PAYAMT FROM DR-BALANCE
               SUBTRACT WS-PAYAMT FROM DR-CURRENT
               ADD WS-PAYAMT TO WS-RUN-INTEREST
               PERFORM REWRITE-DEBTOR
               ADD WS-PAYAMT TO WS-DIST-INTEREST
               PERFORM UPDATE-DISTRIBUTION
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               ADD 1 TO WS-LINE-CNT
               GO TO CONT-010.
               
          MOVE "NOTHING UPDATED CONT-010, 'ESC' TO CONTINUE."
             TO WS-MESSAGE.
          PERFORM ERROR-MESSAGE.
          GO TO CONT-010.
       CONT-999.
          MOVE "PROGRAM GONE PAST CONT-999, 'ESC' TO EXIT."
             TO WS-MESSAGE.
          PERFORM ERROR-MESSAGE.
          PERFORM PRINT-TOTALS.
          PERFORM END-OFF.
          EXIT PROGRAM.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE 0   TO WS-UNAPPLIED-AMT
                        WS-JNL-CR-AMT
                        RUN-TOTAL
                        WS-WORKTOTAL.
            MOVE " " TO CR-OLD-PAYMENT
                        WS-ANSWER1
                        WS-ABOVE-BODY.
            PERFORM CLEAR-FIELDS.
            MOVE WS-LASTPASSWORD TO WS-PASSWORDSAVED.
        GET-001.
            MOVE "Printer:"   TO WS-PRN-FIL
            MOVE Ws-Printer   To WS-PRN-NAME
            MOVE 1125 TO POS
            DISPLAY WS-PRINTER-INFO AT POS.
        GET-010.

            MOVE "                   " TO F-NAMEFIELD.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
             IF WS-BATCH-TOTAL NOT = WS-RUN-PAYMENTS
                 MOVE "BATCH TOTALS DO NOT BALANCE, RE-ENTER."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 MOVE "8" TO WS-ABOVE-BODY
                 PERFORM EBD-020
                 GO TO GET-010.
            IF F-EXIT-CH = X"04"
                 PERFORM UPDATE-CASHBOOK
                 PERFORM PRINT-TOTALS
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 GO TO GET-999.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF F-NAMEFIELD = "INVOICE"
                CALL WS-INVOICE-INQUIRY USING WS-LINKAGE
                CANCEL WS-INVOICE-INQUIRY
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                PERFORM DISPLAY-TOP-INFO
                GO TO GET-000.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-ACCOUNT-NUMBER
                                 DR-ACCOUNT-NUMBER.
       GET-012.
            IF DR-ACCOUNT-NUMBER = 0
                CLOSE DEBTOR-MASTER
                CLOSE DEBTOR-TRANS-FILE
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-011
                PERFORM OPEN-014
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                PERFORM DISPLAY-TOP-INFO
                GO TO GET-000.

            PERFORM READ-DEBTORS.
            MOVE "NAME"  TO F-FIELDNAME
            MOVE 4       TO F-CBFIELDNAME
            MOVE DR-NAME TO F-NAMEFIELD
            MOVE 40      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            IF DR-NAME = "UNKNOWN"
                GO TO GET-010.
       GET-020.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "TRANSTYPE"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE WS-TRANSTYPE TO F-NAMEFIELD
            MOVE 1            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-000.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-TRANSTYPE.
            IF WS-TRANSTYPE = "1" OR = "2" OR = "3" OR = "4"
                         OR = "5" OR = "6" OR = "7"
               GO TO GET-025.
            MOVE "INVALID TRANSACTION TYPE!!!" TO WS-MESSAGE.
            PERFORM ERROR-MESSAGE.
            GO TO GET-020.
       GET-025.
           IF WS-TRANSTYPE = "4" OR "5" OR = "6" OR = "7"
              MOVE WS-LASTPASSWORD TO WS-PASSWORDSAVED
              PERFORM CHECK-PASSWORD.
              
           IF WS-TRANSTYPE = "4" OR = "5" OR = "6" OR = "7"
            IF WS-PASSWORD-VALID = "N"
              MOVE "YOU NEED A VALID PASSWORD TO FERFORM THIS FUNCTION"
                TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-PASSWORDSAVED TO WS-LASTPASSWORD
              GO TO GET-010.
            IF WS-LASTPASSWORD = " "  
               MOVE WS-PASSWORDSAVED TO WS-LASTPASSWORD.
       GET-030.
            IF WS-TRANSTYPE = "6"
               MOVE "YOU SHOULD ENTER OUR CHEQUE # USED FOR THE REFUND."
               TO WS-MESSAGE
               PERFORM ERROR-000.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "CHEQUENO"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-CHEQUENO TO F-NAMEFIELD
            MOVE 6           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-020.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-CHEQUENO.
            IF WS-MESSAGE NOT = " "
                 PERFORM ERROR-020.
            IF WS-TRANSTYPE = "1" OR = "3" OR = "4" OR = "5"
                         OR = "6" OR = "7"
               GO TO GET-050.
       GET-040.
            MOVE 0   TO WS-PAYAMT.
            MOVE " " TO CR-OLD-PAYMENT.
            MOVE 1   TO SUB-1.
            PERFORM READ-DEBTOR-TRANS.
            MOVE 1   TO SUB-1.
            IF WS-TRANSTYPE = "2"
             IF WS-REFNUM (SUB-1) = 0
               MOVE "NO TRANSACTIONS FOR THIS ACCOUNT!!!" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-050.
            IF CR-OLD-PAYMENT = "1"
              IF WS-JNL-CR-AMT > 0
                MOVE WS-JNL-CR-AMT TO WS-PAYAMT
                GO TO GET-045
              ELSE
                MOVE WS-UNAPPLIED-AMT TO WS-PAYAMT
                GO TO GET-045.
            IF WS-PAYAMT = 0
                GO TO GET-050.
       GET-045.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "PAYAMT"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            MOVE WS-PAYAMT TO F-EDNAMEFIELD9MIL
            MOVE 10        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.
            GO TO GET-060.
       GET-050.
            MOVE "PAYAMT" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
       GET-055.
            IF F-EXIT-CH = X"01"
               MOVE "YOU MUST 'ESC' TO CHANGE ANY PREVIOUS FIELD."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-050.
            IF F-EXIT-CH = X"07"
               MOVE "N" TO WS-ANSWER1
               GO TO GET-999.
       GET-057.
            MOVE 10           TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-PAYAMT
            MOVE WS-PAYAMT    TO WS-WORKTOTAL
                                 F-EDNAMEFIELD9MIL
            PERFORM WRITE-FIELD-9MIL.
            IF WS-PAYAMT = 0
               MOVE "PAYMENT AMOUNT MUST BE > ZERO, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-050.
          IF WS-TRANSTYPE = "1" OR = "3"
           IF WS-PAYAMT > 5.00
              MOVE WS-LASTPASSWORD TO WS-PASSWORDSAVED
              PERFORM CHECK-PASSWORD.
           IF WS-PASSWORD-VALID = "N"
              MOVE 
         "YOU NEED A VALID PASSWORD TO ADJ AN AMT HIGHER THAN R5.00"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
               MOVE WS-PASSWORDSAVED TO WS-LASTPASSWORD
               GO TO GET-050.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
            MOVE WS-PASSWORDSAVED TO WS-LASTPASSWORD.
       GET-058.
            IF WS-TRANSTYPE = "1" OR = "3" OR = "4" OR = "5"
                         OR = "6" OR = "7"
               MOVE "Y" TO WS-ANSWER1
               MOVE " " TO WS-TYPEOFPROCESS
               GO TO GET-999.
       GET-060.
            MOVE "                   " TO F-NAMEFIELD.
            MOVE "TYPEOFPROCESS"  TO F-FIELDNAME
            MOVE 13               TO F-CBFIELDNAME
            MOVE WS-TYPEOFPROCESS TO F-NAMEFIELD
            MOVE 1                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
              IF CR-OLD-PAYMENT = "1"
                GO TO GET-030
              ELSE
                GO TO GET-050.
            MOVE 1 TO F-CBFIELDLENGTH
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
            IF WS-TRANSTYPE = "2"
             IF WS-REFNUM (SUB-1) = 0
               GO TO GET-095.
       GET-090.
            PERFORM FILL-BODY.
       GET-095.
            PERFORM QUESTION-UNAPPLIED-AMOUNT.
       GET-999.
            EXIT.
      *
       FILL-BODY SECTION.
       FILL-000.
           MOVE 1 TO SUB-1 SUB-2 SUB-3.
       FILL-010.
           MOVE 3015 TO POS
           DISPLAY "Current Line#: " AT POS
           ADD 16 TO POS
           MOVE SUB-1 TO WS-BODY-LINE
           DISPLAY WS-BODY-LINE AT POS.

           IF SUB-1 < 1
              MOVE 1 TO SUB-1 F-INDEX.
           MOVE "                   " TO F-NAMEFIELD.
           MOVE "APPLY" TO F-FIELDNAME
           MOVE 5       TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01" AND F-INDEX = 1
            IF SUB-1 = 1
              MOVE "1" TO WS-ABOVE-BODY
              GO TO FILL-999
            ELSE
              PERFORM SCROLL-PREVIOUS
              MOVE 1 TO F-INDEX
              GO TO FILL-010.
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
              GO TO FILL-010
            ELSE
              GO TO FILL-010.
           MOVE 1 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO WS-APPLY (SUB-1).
           IF F-INDEX = 12
            IF WS-ALREADY-APPLIED (SUB-1) = "A"
             IF SUB-1 NOT > SUB-9
              PERFORM SCROLL-NEXT
              MOVE 1 TO F-INDEX
              GO TO FILL-010
             ELSE
              GO TO FILL-010.
           IF F-INDEX = 12
           IF WS-APPLY (SUB-1) = " "
              PERFORM SCROLL-NEXT
              MOVE 1 TO F-INDEX
              GO TO FILL-010.
           IF SUB-1 < 1
              MOVE 1 TO SUB-1 F-INDEX.
           IF WS-APPLY (SUB-1) = " " AND WS-REFNUM (SUB-1) > 0
              ADD 1 TO F-INDEX SUB-1
              GO TO FILL-010.
           IF WS-APPLY (SUB-1) = " " AND WS-REFNUM (SUB-1) = 0
              GO TO FILL-010.
           IF WS-APPLY (SUB-1) NOT = "A"
              MOVE "APPLY FIELD MUST BE = 'A' OR SPACE "
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO FILL-010.
           IF F-EXIT-CH = X"0A" AND F-INDEX < 12
            IF WS-ALREADY-APPLIED (SUB-1) = "A"
             IF SUB-1 NOT > SUB-9
              ADD 1 TO F-INDEX SUB-1
              GO TO FILL-010
             ELSE
              GO TO FILL-010.
           IF WS-ALREADY-APPLIED (SUB-1) = "A"
              MOVE "CASH ALREADY APPLIED TO THE DEBIT IN THIS SESSION."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO FILL-010.
       FILL-020.
           MOVE "                   " TO F-NAMEFIELD.
           MOVE "PAYMENT" TO F-FIELDNAME
           MOVE 7         TO F-CBFIELDNAME
           MOVE 10        TO F-CBFIELDLENGTH.
           IF WS-PAYMENT (SUB-1) < WS-AMTOFTYPE (SUB-1)
               AND WS-PAYMENT (SUB-1) < WS-PAYAMT
               MOVE WS-PAYMENT (SUB-1) TO F-EDNAMEFIELD9MIL
               PERFORM WRITE-FIELD-9MIL.

           IF SUB-9 = 1
               AND WS-PAYAMT < WS-AMTOFTYPE (SUB-1)
               OR WS-PAYAMT = WS-AMTOFTYPE (SUB-1)
               MOVE WS-PAYAMT TO WS-PAYMENT (SUB-1)
                                 F-EDNAMEFIELD9MIL
               PERFORM WRITE-FIELD-9MIL
           ELSE
              IF SUB-9 = 1
               AND WS-AMTOFTYPE (SUB-1) < WS-PAYAMT
               MOVE WS-AMTOFTYPE (SUB-1) TO WS-PAYMENT (SUB-1)
                                            F-EDNAMEFIELD9MIL
               PERFORM WRITE-FIELD-9MIL.

           IF SUB-9 > 1
             IF WS-AMTOFTYPE (SUB-1) < WS-PAYAMT
               MOVE WS-AMTOFTYPE (SUB-1) TO WS-PAYMENT (SUB-1)
                                            F-EDNAMEFIELD9MIL
               PERFORM WRITE-FIELD-9MIL
           ELSE
             IF WS-AMTOFTYPE (SUB-1) > WS-PAYAMT
               MOVE WS-PAYAMT TO WS-PAYMENT (SUB-1)
                                 F-EDNAMEFIELD9MIL
               PERFORM WRITE-FIELD-9MIL.

           IF WS-WORKTOTAL = 0
               GO TO FILL-025.

           IF SUB-9 > 1
               IF WS-PAYMENT (SUB-1) > WS-WORKTOTAL
               MOVE WS-WORKTOTAL TO WS-PAYMENT (SUB-1)
                                    F-EDNAMEFIELD9MIL
               PERFORM WRITE-FIELD-9MIL.
       FILL-025.
           MOVE 10 TO F-CBFIELDLENGTH.
           PERFORM USER-FILL-FIELD.
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
               MOVE "PAYMENT CANNOT BE > AMOUNT OUTSTANDING!!!"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
           IF F-CBFIRSTLINE > 0
               MOVE NUMERIC-RATE TO WS-PAYMENT (SUB-1).
           IF WS-PAYMENT (SUB-1) = WS-AMTOFTYPE (SUB-1)
               MOVE 0 TO WS-DISCOUNT (SUB-1)
               GO TO FILL-040.
           IF WS-PAYMENT (SUB-1) > WS-AMTOFTYPE (SUB-1)
               MOVE "PAYMENT CANNOT BE > AMOUNT OF INVOICE!!!"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
       FILL-030.
           IF WS-JNL-CR-AMT > 0
            IF CR-OLD-PAYMENT = "1"
              MOVE 0 TO WS-DISCOUNT (SUB-1)
              GO TO FILL-040.
           IF NUMERIC-RATE < WS-AMTOFTYPE (SUB-1)
              COMPUTE WS-DISCOUNT (SUB-1) =
                 (WS-AMTOFTYPE (SUB-1) - NUMERIC-RATE)
              MOVE "DISCOUNT"          TO F-FIELDNAME
              MOVE 8                   TO F-CBFIELDNAME
              MOVE WS-DISCOUNT (SUB-1) TO F-EDNAMEFIELD9MIL
              MOVE 10                  TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-9MIL.

           MOVE "                   " TO F-NAMEFIELD.
           MOVE "DISCOUNT" TO F-FIELDNAME
           MOVE 8          TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
               GO TO FILL-020.
           MOVE 10 TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA
           MOVE F-NAMEFIELD TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
      *     PERFORM WRITE-FIELD-ALPHA
           MOVE NUMERIC-RATE TO WS-DISCOUNT (SUB-1).
           COMPUTE WS-WORK-FIELD = WS-PAYMENT (SUB-1) +
                                   WS-DISCOUNT (SUB-1).
           IF WS-WORK-FIELD = WS-AMTOFTYPE (SUB-1)
              GO TO FILL-040.
           IF WS-WORK-FIELD < WS-AMTOFTYPE (SUB-1)
              GO TO FILL-040. 
           MOVE "DISCOUNT AMOUNT IS TOO LARGE!!!!!!" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE
           GO TO FILL-030.
       FILL-040.
           MOVE "PAYMENT"          TO F-FIELDNAME
           MOVE 7                  TO F-CBFIELDNAME
           MOVE WS-PAYMENT (SUB-1) TO F-EDNAMEFIELD9MIL
           MOVE 10                  TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-9MIL.

           MOVE "DISCOUNT"          TO F-FIELDNAME
           MOVE 8                   TO F-CBFIELDNAME
           MOVE WS-DISCOUNT (SUB-1) TO F-EDNAMEFIELD9MIL
           MOVE 10                   TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-9MIL.

           COMPUTE RUN-TOTAL = RUN-TOTAL + WS-PAYMENT (SUB-1).
           COMPUTE WS-WORKTOTAL = WS-PAYAMT - RUN-TOTAL.
           MOVE 2940 TO POS.
           DISPLAY "AMT. TO APPLY:" AT POS.
           MOVE WS-WORKTOTAL TO F-EDRUNNINGAMOUNT.
           ADD 15 TO POS.
           DISPLAY F-EDRUNNINGAMOUNT AT POS.
           IF WS-WORKTOTAL = 0
               MOVE " " TO WS-ABOVE-BODY
               GO TO FILL-999.
           IF WS-WORKTOTAL > 0 
               GO TO FILL-050.
           MOVE "YOUR MONEY HAS RUN OUT!!!!" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE
           GO TO FILL-010.
       FILL-050.
           MOVE "A" TO WS-ALREADY-APPLIED (SUB-1).
           ADD 1 TO SUB-1 F-INDEX.
           IF SUB-1 > 750
               MOVE "750 LINES ARE UP, 'ESC' TO <TAB>."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO WS-ABOVE-BODY
               GO TO FILL-999.
           IF F-INDEX < 13
               GO TO FILL-010.
           SUBTRACT 1 FROM SUB-1.
           IF SUB-1 < 1
              MOVE 1 TO SUB-1.
           PERFORM SCROLL-NEXT.
           MOVE 1 TO F-INDEX.
           GO TO FILL-010.
       FILL-999.
           EXIT.
      *
       QUESTION-UNAPPLIED-AMOUNT SECTION.
       QUNAP-000.
           PERFORM CLEAR-010.
       QUNAP-005.
           MOVE 2910 TO POS
           DISPLAY "This Amount is Unapplied:" AT POS
           ADD 27 TO POS
           MOVE WS-WORKTOTAL TO F-EDNAMEFIELD9MIL
           DISPLAY F-EDNAMEFIELD9MIL AT POS.
           MOVE 2955 TO POS
           DISPLAY "Apply? (Y/N): [ ]        " AT POS WITH BELL.
       QUNAP-010.
      *     MOVE 3069 TO POS
      *     DISPLAY "       " AT POS.
      *     ACCEPT WS-ANSWER1 AT POS
           
           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 69        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.
           
      *     MOVE WS-ANSWER1 TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     MOVE W-ESCAPE-KEY TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      *     MOVE CDA-KEY TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
           IF W-ESCAPE-KEY = 1 OR 2
               GO TO QUNAP-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO QUNAP-010.
       QUNAP-020.
           IF WS-ANSWER1 NOT = "N" AND NOT = "Y"
                MOVE "INVALID ANSWER, MUST BE N OR Y, RE-ENTER"
                TO WS-MESSAGE
                PERFORM ERROR-000
                DISPLAY " " AT 3079 WITH BELL
                GO TO QUNAP-005.
           PERFORM ERROR1-020.
           PERFORM ERROR-020.
       QUNAP-030.
           IF WS-ANSWER1 = "Y "
            IF CR-OLD-PAYMENT = "1"
             IF WS-WORKTOTAL = WS-PAYAMT
                MOVE "YOU MUST ENTER 'N' AS NOTHING HAS BEEN APPLIED."
                TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                MOVE " " TO WS-ANSWER1
                GO TO QUNAP-010.
       QUNAP-999.
           EXIT.
      *
       JOURNAL-DEBIT SECTION.
       JDR-010.
           IF WS-TRANSTYPE NOT = "1"
               MOVE "SOMETHINGS WRONG, 'ESC' TO EXIT JRN-DEBITS."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO JDR-999.
           PERFORM READ-PARAMETER-LOCK.
           MOVE PA-JOURNAL-NUMBER       TO DRTR-TRANSACTION-NUMBER.
           ADD 1                        TO PA-JOURNAL-NUMBER.
           PERFORM REWRITE-PARAMETER.
           MOVE 4                       TO DRTR-TYPE.
           MOVE DR-ACCOUNT-NUMBER       TO DRTR-ACCOUNT-NUMBER.
           MOVE WS-CHEQUENO             TO DRTR-REFERENCE1.
           MOVE DRTR-TRANSACTION-NUMBER TO DRTR-REFERENCE2.
           MOVE WS-DATE                 TO DRTR-DATE
                                           DRTR-DEL-DATE.
           MOVE WS-PAYAMT               TO DRTR-AMT-OF-INVOICE
                                           DRTR-AMT-OUTSTANDING.
           PERFORM PRINT-ROUTINE.
       JDR-015.
           WRITE DEBTOR-TRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
              GO TO JDR-010.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DR TRANS BUSY JDR-015, 'ESC' TO RETRY" 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DRTRANS-ST1
              GO TO JDR-015.
           MOVE "YOUR JRN.DR. REF:" TO WS-DAILY-1ST.
           MOVE DRTR-REFERENCE2     TO WS-DAILY-2ND.
           MOVE " "                 TO WS-DAILY-3RD WS-DAILY-4TH.
           MOVE WS-DAILY-MESSAGE    TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
           MOVE " " TO WS-MESSAGE.
           PERFORM ERROR-020.
       JDR-999.
           EXIT.
      *
       JOURNAL-CREDIT SECTION.
       JCR-010.
           IF WS-TRANSTYPE NOT = "3"
               MOVE "SOMETHINGS WRONG, 'ESC' TO EXIT JRN-CREDITS."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO JCR-999.
           PERFORM READ-PARAMETER-LOCK.
           MOVE PA-JOURNAL-NUMBER       TO DRTR-TRANSACTION-NUMBER.
           ADD 1                        TO PA-JOURNAL-NUMBER.
           PERFORM REWRITE-PARAMETER.
           MOVE 5                       TO DRTR-TYPE.
           MOVE WS-PAYAMT               TO DRTR-AMT-OF-INVOICE
                                           DRTR-AMT-OUTSTANDING.
           MOVE DR-ACCOUNT-NUMBER       TO DRTR-ACCOUNT-NUMBER.
           MOVE WS-CHEQUENO             TO DRTR-REFERENCE1.
           MOVE DRTR-TRANSACTION-NUMBER TO DRTR-REFERENCE2.
           MOVE WS-DATE                 TO DRTR-DATE
                                           DRTR-DEL-DATE.
           PERFORM PRINT-ROUTINE.
       JCR-012.
           WRITE DEBTOR-TRANS-REC
                 INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-DRTRANS-ST1
                GO TO JCR-010.
           IF WS-DRTRANS-ST1 NOT = 0
                MOVE "DR TRANS BUSY JCR-012, 'ESC' TO RETRY" 
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DRTRANS-ST1
                GO TO JCR-012.
           MOVE " " TO WS-MESSAGE.
           PERFORM ERROR-020.
           MOVE "YOUR JRN.CR. REF:" TO WS-DAILY-1ST.
           MOVE DRTR-REFERENCE2     TO WS-DAILY-2ND.
           MOVE " "                 TO WS-DAILY-3RD WS-DAILY-4TH.
           MOVE WS-DAILY-MESSAGE TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
       JCR-999.
           EXIT.
      *
       REVERSE-INTEREST SECTION.
       RINT-010.
           IF WS-TRANSTYPE NOT = "7"
               MOVE "SOMETHINGS WRONG, 'ESC' TO EXIT REV INTEREST."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RINT-999.
           PERFORM READ-PARAMETER-LOCK.
           MOVE PA-JOURNAL-NUMBER       TO DRTR-TRANSACTION-NUMBER.
           ADD 1                        TO PA-JOURNAL-NUMBER.
           PERFORM REWRITE-PARAMETER.
           MOVE 11                      TO DRTR-TYPE.
           MOVE WS-PAYAMT               TO DRTR-AMT-OF-INVOICE
                                           DRTR-AMT-OUTSTANDING.
           MOVE DR-ACCOUNT-NUMBER       TO DRTR-ACCOUNT-NUMBER.
           MOVE "REVERSAL OF INTEREST"  TO DRTR-REFERENCE1.
           MOVE DRTR-TRANSACTION-NUMBER TO DRTR-REFERENCE2.
           MOVE WS-DATE                 TO DRTR-DATE
                                           DRTR-DEL-DATE.
           PERFORM PRINT-ROUTINE.
       RINT-012.
           WRITE DEBTOR-TRANS-REC
                 INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 or 35 OR 49
                MOVE 0 TO WS-DRTRANS-ST1
                GO TO RINT-010.
           IF WS-DRTRANS-ST1 NOT = 0
                MOVE "DR TRANS BUSY RINT-012, 'ESC' TO RETRY" 
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DRTRANS-ST1
                GO TO RINT-012.
           MOVE " " TO WS-MESSAGE.
           PERFORM ERROR-020.
           MOVE "YOUR INT REV REF:" TO WS-DAILY-1ST
           MOVE DRTR-REFERENCE2     TO WS-DAILY-2ND
           MOVE " "                 TO WS-DAILY-3RD WS-DAILY-4TH
           MOVE WS-DAILY-MESSAGE    TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
       RINT-999.
           EXIT.
      *
       RD-CHEQUE SECTION.
       RDC-010.
           IF WS-TRANSTYPE NOT = "4"
               MOVE "SOMETHINGS WRONG, 'ESC' TO EXIT RD-CHEQUES."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDC-999.
           PERFORM READ-PARAMETER-LOCK.
           MOVE PA-JOURNAL-NUMBER       TO DRTR-TRANSACTION-NUMBER.
           ADD 1                        TO PA-JOURNAL-NUMBER.
           PERFORM REWRITE-PARAMETER.
           MOVE 3                       TO DRTR-TYPE.
           MOVE DR-ACCOUNT-NUMBER       TO DRTR-ACCOUNT-NUMBER.
           MOVE WS-CHEQUENO             TO DRTR-REFERENCE1.
           MOVE DRTR-TRANSACTION-NUMBER TO DRTR-REFERENCE2.
           MOVE WS-DATE                 TO DRTR-DATE
                                           DRTR-DEL-DATE.
           MOVE WS-PAYAMT               TO DRTR-AMT-OF-INVOICE
                                           DRTR-AMT-OUTSTANDING.
           PERFORM PRINT-ROUTINE.
       RDC-015.
           WRITE DEBTOR-TRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
              GO TO RDC-010.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DR TRANS BUSY RDC-015, 'ESC' TO RETRY" 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DRTRANS-ST1
              GO TO RDC-015.
           MOVE "YOUR RD-CHEQUE #:" TO WS-DAILY-1ST.
           MOVE DRTR-REFERENCE2     TO WS-DAILY-2ND.
           MOVE " "                 TO WS-DAILY-3RD WS-DAILY-4TH.
           MOVE WS-DAILY-MESSAGE    TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
       RDC-999.
           EXIT.
      *
       BAD-DEBT SECTION.
       BDEBT-010.
           IF WS-TRANSTYPE NOT = "5"
               MOVE "SOMETHINGS WRONG, 'ESC' TO EXIT BAD-DEBTS."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO BDEBT-999.
           PERFORM READ-PARAMETER-LOCK.
           MOVE PA-JOURNAL-NUMBER       TO DRTR-TRANSACTION-NUMBER.
           ADD 1                        TO PA-JOURNAL-NUMBER.
           PERFORM REWRITE-PARAMETER.
           MOVE 9                       TO DRTR-TYPE.
           MOVE WS-PAYAMT               TO DRTR-AMT-OF-INVOICE
                                           DRTR-AMT-OUTSTANDING.
           MOVE DR-ACCOUNT-NUMBER       TO DRTR-ACCOUNT-NUMBER.
           MOVE WS-CHEQUENO             TO DRTR-REFERENCE1.
           MOVE DRTR-TRANSACTION-NUMBER TO DRTR-REFERENCE2.
           MOVE WS-DATE                 TO DRTR-DATE
                                           DRTR-DEL-DATE.
           PERFORM PRINT-ROUTINE.
       BDEBT-012.
           WRITE DEBTOR-TRANS-REC
                 INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 or 35 OR 49
                MOVE 0 TO WS-DRTRANS-ST1
                GO TO BDEBT-010.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DR TRANS BUSY BDEBT-012, 'ESC' TO RETRY" 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DRTRANS-ST1
              GO TO BDEBT-012.
           MOVE " " TO WS-MESSAGE.
           PERFORM ERROR-020.
           MOVE "YOUR BAD-DEBT #:"  TO WS-DAILY-1ST.
           MOVE DRTR-REFERENCE2     TO WS-DAILY-2ND.
           MOVE " "                 TO WS-DAILY-3RD WS-DAILY-4TH.
           MOVE WS-DAILY-MESSAGE TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
       BDEBT-999.
           EXIT.
      *
       CHEQUE-REFUND SECTION.
       CHREF-010.
           IF WS-TRANSTYPE NOT = "6"
               MOVE "SOMETHINGS WRONG, 'ESC' TO EXIT REFUNDS."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO CHREF-999.
           PERFORM READ-PARAMETER-LOCK.
           MOVE PA-JOURNAL-NUMBER       TO DRTR-TRANSACTION-NUMBER.
           ADD 1                        TO PA-JOURNAL-NUMBER.
           PERFORM REWRITE-PARAMETER.
           MOVE 10                      TO DRTR-TYPE.
           MOVE DR-ACCOUNT-NUMBER       TO DRTR-ACCOUNT-NUMBER.
           MOVE WS-CHEQUENO             TO DRTR-REFERENCE1.
           MOVE DRTR-TRANSACTION-NUMBER TO DRTR-REFERENCE2.
           MOVE WS-DATE                 TO DRTR-DATE
                                           DRTR-DEL-DATE.
           MOVE WS-PAYAMT               TO DRTR-AMT-OF-INVOICE
                                           DRTR-AMT-OUTSTANDING.
           PERFORM PRINT-ROUTINE.
       CHREF-015.
           WRITE DEBTOR-TRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
              GO TO CHREF-010.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DR TRANS BUSY CHREF-015, 'ESC' TO RETRY" 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DRTRANS-ST1
              GO TO CHREF-015.
           MOVE "YOUR CHEQUE REF :" TO WS-DAILY-1ST.
           MOVE DRTR-REFERENCE2     TO WS-DAILY-2ND.
           MOVE " "                 TO WS-DAILY-3RD WS-DAILY-4TH.
           MOVE WS-DAILY-MESSAGE    TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
           MOVE " " TO WS-MESSAGE.
           PERFORM ERROR-020.
       CHREF-999.
           EXIT.
      *
       UPDATE-OLD-CREDIT SECTION.
       CR-020.
           MOVE 2910 TO POS
           DISPLAY "UPDATING OLD CREDIT TRANSACTION...." AT POS.
           MOVE CR-DRTYPE     TO DRTR-TYPE.
           MOVE CR-DR-TRANSNO TO DRTR-TRANSACTION-NUMBER.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 NOT = 0
                MOVE "DRTRANS BUSY ON START CR-020, 'ESC' TO RETRY" 
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DRTRANS-ST1
                GO TO CR-020.
           PERFORM ERROR-020.
       CR-030.
           READ DEBTOR-TRANS-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-DRTRANS-ST1
               MOVE 0 TO WS-UNAPPLIED-AMT
                         WS-JNL-CR-AMT
               GO TO CR-999.
           IF WS-DRTRANS-ST1 NOT = 0
                MOVE "DR TRANS BUSY READ CR-030, 'ESC' TO RETRY" 
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DRTRANS-ST1
                GO TO CR-030.
           PERFORM ERROR-020.
           SUBTRACT RUN-TOTAL FROM DRTR-AMT-OUTSTANDING.
           PERFORM PRINT-ROUTINE.
       CR-025.
           REWRITE DEBTOR-TRANS-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO CR-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DR TRANS BUSY REWRITE CR-025, 'ESC' TO RETRY" 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO CR-025.
           PERFORM ERROR-020.
           MOVE 0 TO WS-UNAPPLIED-AMT
                     WS-JNL-CR-AMT.
       CR-035.
           PERFORM READ-DEBTORS-LOCK.
           MOVE CR-BODDATE TO WS-AGE-DATE.
       CR-040.
           IF WS-AGE-MM = 0
               MOVE WS-MM TO WS-AGE-MM
               MOVE WS-YY TO WS-AGE-YY.
           MOVE WS-MM TO WS-MONTH.
      *     MOVE WS-DATE TO DR-DATE-LAST-PAY.
           IF WS-AGE-YY < WS-YY
               COMPUTE WS-MONTH = (((WS-YY - WS-AGE-YY) * 12)
                                    + WS-MONTH).
           SUBTRACT WS-AGE-MM FROM WS-MONTH.

           PERFORM UPDATE-OLD-BALANCE.
           PERFORM REWRITE-DEBTOR.
       CR-999.
           EXIT.
      *
       APPLY-AMOUNTS SECTION.
       AP-000.
           IF WS-WORKTOTAL = WS-PAYAMT
              GO TO AP-999.
           MOVE 2910 TO POS
           DISPLAY "APPLYING CASH TO DEBIT TRANSACTIONS......." AT POS.
           MOVE 0 TO SUB-1.   
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DR TRANS BUSY START AP-000, 'ESC' TO RETRY" 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DRTRANS-ST1
              GO TO AP-000.
       AP-010.     
           ADD 1 TO SUB-1.
           IF SUB-1 > 750
               GO TO AP-999.
           IF WS-APPLY (SUB-1) = "A"
              GO TO AP-020.
           GO TO AP-010.
       AP-020.
           MOVE WS-DRTYPE (SUB-1)     TO DRTR-TYPE.
           MOVE WS-DR-TRANSNO (SUB-1) TO DRTR-TRANSACTION-NUMBER.
       AP-030.               
           READ DEBTOR-TRANS-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
               GO TO AP-010.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DR TRANS BUSY READ AP-030, 'ESC' TO RETRY" 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DRTRANS-ST1
              GO TO AP-030.

           IF WS-TRANSTYPE = "2" 
              SUBTRACT WS-DISCOUNT (SUB-1) FROM DRTR-AMT-OUTSTANDING
              SUBTRACT WS-PAYMENT (SUB-1)  FROM DRTR-AMT-OUTSTANDING
              PERFORM UPDATE-DEBTOR.
           PERFORM PRINT-ROUTINE.
       AP-038.
           REWRITE DEBTOR-TRANS-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
               MOVE "AP-038 REWRITE STATUS CODE 23, 'ESC' TO EXIT."
                 TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DRTRANS-ST1
              GO TO AP-010.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DR TRANS BUSY REWRITE AP-038, 'ESC' TO RETRY" 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DRTRANS-ST1
              GO TO AP-038.
           IF WS-DISCOUNT (SUB-1) > 0
              PERFORM WRITE-DISCOUNT-TRANSACTION.
       AP-040.
           ADD WS-DISCOUNT (SUB-1) TO WS-DIST-DISCOUNT
                                      WS-RUN-DISCOUNT.
           GO TO AP-010.
       AP-999.
           EXIT.
      *
       READ-DEBTOR-TRANS SECTION.
       RDT-000.
           MOVE 2910 TO POS
           DISPLAY "READING DEBTORS TRANSACTIONS......." AT POS.
           MOVE 0 TO SUB-9.
       RDT-005.
           MOVE WS-ACCOUNT-NUMBER TO DRTR-ACCOUNT-NUMBER.
           MOVE 0                 TO DRTR-DATE.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACC-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
               GO TO RDT-900.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DRTRANS BUSY START RDT-005, PRESS 'ESC' TO RETRY" 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO RDT-005.
           PERFORM ERROR-020.
       RDT-010.
           READ DEBTOR-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 10
               CLOSE DEBTOR-TRANS-FILE
               PERFORM OPEN-014
               GO TO RDT-900.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DR TRANS BUSY READ RDT-010, PRESS 'ESC' TO RETRY" 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO RDT-010.
           MOVE " " TO WS-MESSAGE.
           PERFORM ERROR-020.
           IF DRTR-ACCOUNT-NUMBER NOT = WS-ACCOUNT-NUMBER
              GO TO RDT-900.
           IF DRTR-TYPE = 8
              GO TO RDT-010.
           IF DRTR-AMT-OUTSTANDING < 0.01
              GO TO RDT-010.
           IF DRTR-TYPE = 2 AND DRTR-AMT-OUTSTANDING < 0.01
              GO TO RDT-010.
           IF DRTR-TYPE = 2 OR = 5 OR = 6 OR = 8 OR = 9 OR = 11
            IF DRTR-REFERENCE2 NOT = WS-CHEQUENO
              GO TO RDT-010
            ELSE
              PERFORM FOUND-CREDIT
              GO TO RDT-010.
           ADD 1                         TO SUB-9
           MOVE DRTR-TYPE                TO WS-DRTYPE (SUB-1)
           MOVE DRTR-TRANSACTION-NUMBER  TO WS-DR-TRANSNO (SUB-1)
           MOVE DRTR-REFERENCE2          TO WS-REFNUM (SUB-1)
           MOVE WS-TYPE-DESC (DRTR-TYPE) TO WS-TYPE (SUB-1)
           MOVE DRTR-DATE                TO WS-BODDATE (SUB-1)
           MOVE DRTR-AMT-OUTSTANDING     TO WS-PAYMENT (SUB-1)
                                            WS-AMTOFTYPE (SUB-1).
           ADD 1 TO SUB-1.
           IF SUB-1 < 751
               GO TO RDT-010.
       RDT-900.
           PERFORM ERROR1-020.
       RDT-999.
           EXIT.
      *
       FOUND-CREDIT SECTION.
       FC-010.
           IF DRTR-TYPE = 2
               MOVE DRTR-AMT-OUTSTANDING TO WS-UNAPPLIED-AMT.
           IF DRTR-TYPE = 5 OR = 6 OR = 9 OR = 11
               MOVE DRTR-AMT-OUTSTANDING TO WS-JNL-CR-AMT.
           MOVE "1" TO CR-OLD-PAYMENT.
           MOVE "A" TO CR-APPLY.
       FC-020.
           MOVE DRTR-TYPE                TO CR-DRTYPE
           MOVE DRTR-TRANSACTION-NUMBER  TO CR-DR-TRANSNO
           MOVE DRTR-REFERENCE2          TO CR-REFNUM
           MOVE WS-TYPE-DESC (DRTR-TYPE) TO CR-TYPE
           MOVE DRTR-DATE                TO CR-BODDATE
           MOVE DRTR-AMT-OUTSTANDING     TO CR-PAYMENT
                                            CR-AMTOFTYPE.
       FC-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           START PARAMETER-FILE KEY NOT < PA-KEY.
       RP-010.
           READ PARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY ON READ RP-010, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RP-010.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       RP-999.
           EXIT.
      *
       READ-PARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           START PARAMETER-FILE KEY NOT < PA-KEY.
       RPL-010.
           READ PARAMETER-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY READ-LOCK RPL-010, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RPL-010.
       RPL-999.
           EXIT.
      *
       REWRITE-PARAMETER SECTION.
       REWP-000.
           REWRITE PARAMETER-REC
               INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY REWRITE REWP-000, 'ESC' TO RETRY"
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
       WRITE-PAYMENT-TRANSACTION SECTION.
       WRTR-000.
             MOVE 2910 TO POS.
             DISPLAY "WRITING PAYMENT TRANSACTION........" AT POS.
             PERFORM READ-PARAMETER-LOCK.
             MOVE PA-CASH-RECEIPT-NUMBER TO DRTR-TRANSACTION-NUMBER.
             ADD 1 TO PA-CASH-RECEIPT-NUMBER.
             PERFORM REWRITE-PARAMETER.
             MOVE 2                       TO DRTR-TYPE.
             MOVE DR-ACCOUNT-NUMBER       TO DRTR-ACCOUNT-NUMBER.
             MOVE WS-CHEQUENO             TO DRTR-REFERENCE1.
             MOVE DRTR-TRANSACTION-NUMBER TO DRTR-REFERENCE2.
             MOVE WS-DATE                 TO DRTR-DATE
                                             DRTR-DEL-DATE.
             MOVE WS-PAYAMT               TO DRTR-AMT-OF-INVOICE.
             MOVE WS-WORKTOTAL            TO DRTR-AMT-OUTSTANDING.
             PERFORM PRINT-ROUTINE.
       WRTR-010.
             WRITE DEBTOR-TRANS-REC
                 INVALID KEY NEXT SENTENCE.
            IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
                 GO TO WRTR-000.
      * ERC 22 = ATTEMPT TO DUPLICATE A KEY VALUE
            IF WS-DRTRANS-ST1 = 22
                MOVE 
             "DRTRANS ERC22=ATTEMPT TO DUPLICATE A KEY, 'ESC' TO RETRY." 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE.
            IF WS-DRTRANS-ST1 NOT = 0
                MOVE 
             "DRTRANS BUSY ON PMT WRITE WRTR-010, 'ESC' TO RETRY." 
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DRTRANS-ST1
                GO TO WRTR-010.
       WRTR-999.
              EXIT.
      *
       WRITE-DISCOUNT-TRANSACTION SECTION.
       WRDTR-000.
             MOVE 2910 TO POS.
             DISPLAY "WRITING DISCOUNT TRANSACTION......." AT POS.
             PERFORM READ-PARAMETER-LOCK.
             MOVE PA-CASH-RECEIPT-NUMBER  TO DRTR-TRANSACTION-NUMBER.
             ADD 1                        TO PA-CASH-RECEIPT-NUMBER.
             PERFORM REWRITE-PARAMETER.
             MOVE 8                       TO DRTR-TYPE.
             MOVE DR-ACCOUNT-NUMBER       TO DRTR-ACCOUNT-NUMBER.
             MOVE WS-CHEQUENO             TO DRTR-REFERENCE1.
             MOVE DRTR-TRANSACTION-NUMBER TO DRTR-REFERENCE2.
             MOVE WS-DATE                 TO DRTR-DATE
                                             DRTR-DEL-DATE.
             MOVE WS-DISCOUNT (SUB-1)     TO DRTR-AMT-OF-INVOICE.
             MOVE 0                       TO DRTR-AMT-OUTSTANDING.
             PERFORM PRINT-ROUTINE.
       WRDTR-010.
             WRITE DEBTOR-TRANS-REC
                 INVALID KEY NEXT SENTENCE.
            IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
                 GO TO WRDTR-000.
            IF WS-DRTRANS-ST1 NOT = 0
                MOVE "DR TRANS BUSY WRITE WRDTR-010, 'ESC' TO RETRY" 
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DRTRANS-ST1
                GO TO WRDTR-010.
           MOVE " " TO WS-MESSAGE.
           PERFORM ERROR-020.
       WRDTR-999.
              EXIT.
      *
       UPDATE-DEBTOR SECTION.
       UPDR-000.
             MOVE 2910 TO POS.
             DISPLAY "UPDATING DEBTOR FILE..............." AT POS.
             PERFORM READ-DEBTORS-LOCK.

             IF WS-PAYAMT = WS-WORKTOTAL
                 MOVE DRTR-DATE TO WS-AGE-DATE
                 GO TO UPDR-005.
             MOVE WS-BODDATE (SUB-1) TO WS-AGE-DATE.
       UPDR-005.
             IF WS-AGE-MM = 0
                 MOVE WS-MM TO WS-AGE-MM
                 MOVE WS-YY TO WS-AGE-YY.
             MOVE WS-MM TO WS-MONTH.
           IF WS-TYPEOFPROCESS = "2"
             IF CR-OLD-PAYMENT = " "
                 MOVE WS-DATE TO DR-DATE-LAST-PAY.
             IF WS-AGE-YY < WS-YY
                 COMPUTE WS-MONTH = (((WS-YY - WS-AGE-YY) * 12)
                                     + WS-MONTH).
             SUBTRACT WS-AGE-MM FROM WS-MONTH.

             IF CR-OLD-PAYMENT = "1"
             IF WS-MONTH = 0
                 SUBTRACT WS-PAYMENT (SUB-1) FROM DR-CURRENT
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM DR-CURRENT
                 GO TO UPDR-010.
             IF CR-OLD-PAYMENT = "1"
             IF WS-MONTH = 1
                 SUBTRACT WS-PAYMENT (SUB-1) FROM DR-30DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM DR-30DAY
                 GO TO UPDR-010.
             IF CR-OLD-PAYMENT = "1"
             IF WS-MONTH = 2
                 SUBTRACT WS-PAYMENT (SUB-1) FROM DR-60DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM DR-60DAY
                 GO TO UPDR-010.
             IF CR-OLD-PAYMENT = "1"
             IF WS-MONTH = 3
                 SUBTRACT WS-PAYMENT (SUB-1) FROM DR-90DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM DR-90DAY
                 GO TO UPDR-010.
             IF CR-OLD-PAYMENT = "1"
             IF WS-MONTH > 3
                 SUBTRACT WS-PAYMENT (SUB-1) FROM DR-120DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM DR-120DAY
                 GO TO UPDR-010.
             IF CR-OLD-PAYMENT = "1"
                 GO TO UPDR-999.

             IF WS-MONTH = 0
                 SUBTRACT WS-PAYMENT (SUB-1) FROM DR-CURRENT
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM DR-CURRENT
                 GO TO UPDR-010.
             IF WS-MONTH = 1
                 SUBTRACT WS-PAYMENT (SUB-1) FROM DR-30DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM DR-30DAY
                 GO TO UPDR-010.
             IF WS-MONTH = 2
                 SUBTRACT WS-PAYMENT (SUB-1) FROM DR-60DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM DR-60DAY
                 GO TO UPDR-010.
             IF WS-MONTH = 3
                 SUBTRACT WS-PAYMENT (SUB-1) FROM DR-90DAY
                 SUBTRACT WS-DISCOUNT (SUB-1) FROM DR-90DAY
                 GO TO UPDR-010.
             SUBTRACT WS-PAYMENT (SUB-1) FROM DR-120DAY.
             SUBTRACT WS-DISCOUNT (SUB-1) FROM DR-120DAY.
       UPDR-010.
      *       IF DRTR-TYPE NOT = 3 AND NOT = 6
                 COMPUTE DR-BALANCE = DR-30DAY + DR-60DAY
                    + DR-90DAY + DR-120DAY + DR-CURRENT.
       UPDR-900.
             PERFORM REWRITE-DEBTOR.
       UPDR-999.
            EXIT.
      *
       UPDATE-OLD-BALANCE SECTION.
       UOB-000.
            IF WS-MONTH = 0
               ADD RUN-TOTAL TO DR-CURRENT
               GO TO UOB-999.
            IF WS-MONTH = 1
               ADD RUN-TOTAL TO DR-30DAY
               GO TO UOB-999.
            IF WS-MONTH = 2
               ADD RUN-TOTAL TO DR-60DAY
               GO TO UOB-999.
            IF WS-MONTH = 3
               ADD RUN-TOTAL TO DR-90DAY
               GO TO UOB-999.
            IF WS-MONTH > 3
               ADD RUN-TOTAL TO DR-120DAY.
       UOB-999.
            EXIT.
      *
       READ-DEBTORS SECTION.
       RD-000.
           READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE " " TO DR-NAME DR-ADDRESS1 DR-ADDRESS2
                        DR-ADDRESS3 DR-DEL-ADDRESS1 DR-DEL-ADDRESS2
                        DR-DEL-ADDRESS3
               MOVE "UNKNOWN" TO DR-NAME
               MOVE 0 TO DR-POST-CODE
               GO TO RD-999.
            IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR FILE BUSY ON READ RD-000, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO RD-000.
       RD-999.
           EXIT.
      *
       READ-DEBTORS-LOCK SECTION.
       RDL-000.
           START DEBTOR-MASTER KEY NOT < DR-KEY.
       RDL-010.
           READ DEBTOR-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "NO SUCH DEBTOR FILE TO UPDATE, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDL-999.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTORS BUSY READ-LOCK RDL-010, 'ESC' TO RETRY" 
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                PERFORM ERROR-MESSAGE
                GO TO RDL-010.
       RDL-999.
           EXIT.
      *
       REWRITE-DEBTOR SECTION.
       REWRDB-000.
             REWRITE DEBTOR-RECORD
                 INVALID KEY NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                 MOVE "ACC.NUMBER :PAYMENT" TO WS-DAILY-1ST
                 MOVE DR-ACCOUNT-NUMBER     TO WS-DAILY-2ND
                 MOVE "NOT UPDATED"         TO WS-DAILY-3RD
                 MOVE " "                   TO WS-DAILY-4TH
                 PERFORM WRITE-DAILY.
             IF WS-DEBTOR-ST1 NOT = 0
                 MOVE "DEBTORS BUSY REWRITE REWRDB-000, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-DEBTOR-ST1
                 GO TO REWRDB-000.
       REWRDB-999.
            EXIT.
      *
       UPDATE-DISTRIBUTION SECTION.
       UPDIS-000.
            IF WS-ANSWER1 NOT = "Y"
                GO TO UPDIS-950.
             MOVE 2910 TO POS.
             DISPLAY "UPDATING DISTRIBUTIONS FILE........" AT POS.
            MOVE "1" TO DIST-KEY.
            START DISTRIBUTIONS KEY NOT < DIST-KEY.
       UPDIS-010.
            READ DISTRIBUTIONS WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-DISTRIBUTION-ST1 = 23 OR 35 OR 49
               MOVE "DISTRIBUTION NOT FOUND UPDIS-010, 'ESC' TO EXIT."
                  TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DISTRIBUTION-ST1
               GO TO UPDIS-950.
            IF WS-DISTRIBUTION-ST1 NOT = 0
                MOVE "DISTRIBUTION BUSY UPDIS-010, 'ESC' TO RETRY." 
                   TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DISTRIBUTION-ST1
                GO TO UPDIS-010.
            ADD WS-DIST-PAYMENT          TO DIST-PAYMENTWEEK
                                            DIST-PAYMENTPTD
                                            DIST-PAYMENTYTD.
            ADD WS-DIST-RDCHEQUE         TO DIST-RDCHEQUEWEEK
                                            DIST-RDCHEQUEPTD
                                            DIST-RDCHEQUEYTD.
            ADD WS-DIST-JOURNALDR        TO DIST-JOURNALDRWEEK
                                            DIST-JOURNALDRPTD
                                            DIST-JOURNALDRYTD.
            ADD WS-DIST-JOURNALCR        TO DIST-JOURNALCRWEEK
                                            DIST-JOURNALCRPTD
                                            DIST-JOURNALCRYTD.
            SUBTRACT WS-DIST-INTEREST  FROM DIST-INTERESTWEEK
                                            DIST-INTERESTPTD
                                            DIST-INTERESTYTD.
            ADD WS-DIST-DISCOUNT         TO DIST-DISCOUNTWEEK
                                            DIST-DISCOUNTPTD
                                            DIST-DISCOUNTYTD.
            ADD WS-DIST-BDEBT            TO DIST-BDEBTWEEK
                                            DIST-BDEBTPTD
                                            DIST-BDEBTYTD.
            SUBTRACT WS-DIST-PAYMENT   FROM DIST-ACCRECWEEK
                                            DIST-ACCRECPTD
                                            DIST-ACCRECYTD.
            ADD WS-DIST-RDCHEQUE         TO DIST-ACCRECWEEK
                                            DIST-ACCRECPTD
                                            DIST-ACCRECYTD.
            ADD WS-DIST-JOURNALDR        TO DIST-ACCRECWEEK
                                            DIST-ACCRECPTD
                                            DIST-ACCRECYTD.
            SUBTRACT WS-DIST-JOURNALCR FROM DIST-ACCRECWEEK
                                            DIST-ACCRECPTD
                                            DIST-ACCRECYTD.
            SUBTRACT WS-DIST-INTEREST  FROM DIST-ACCRECWEEK
                                            DIST-ACCRECPTD
                                            DIST-ACCRECYTD.
            SUBTRACT WS-DIST-DISCOUNT  FROM DIST-ACCRECWEEK
                                            DIST-ACCRECPTD
                                            DIST-ACCRECYTD.
            SUBTRACT WS-DIST-BDEBT     FROM DIST-ACCRECWEEK
                                            DIST-ACCRECPTD
                                            DIST-ACCRECYTD.
       UPDIS-020.
            REWRITE DIST-REC
               INVALID KEY NEXT SENTENCE.
            IF WS-DISTRIBUTION-ST1 = 23 OR 35 OR 49
               MOVE "DISTRIBUTION TOTALS NOT UPDATED, 'ESC' TO EXIT."
                  TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DISTRIBUTION-ST1
                GO TO UPDIS-950.
            IF WS-DISTRIBUTION-ST1 NOT = 0
                MOVE "DISTRIBUTION BUSY UPDIS-020, 'ESC' TO RETRY" 
                   TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DISTRIBUTION-ST1
                GO TO UPDIS-020.
       UPDIS-950.
            MOVE 0 TO WS-DIST-PAYMENT
                      WS-DIST-JOURNALDR
                      WS-DIST-JOURNALCR
                      WS-DIST-RDCHEQUE
                      WS-DIST-BDEBT
                      WS-DIST-DISCOUNT
                      WS-DIST-INTEREST.
       UPDIS-999.
           EXIT.
      *
       UPDATE-CASHBOOK SECTION.
       UCB-000.
            IF WS-RUN-PAYMENTS = 0
                GO TO UCB-999.
           MOVE 2910 TO POS.
           DISPLAY "WRITING CASH-BOOK ENTRIES......                "
           AT POS.
       UCB-001.
           PERFORM READ-GLPARAMETER.
           MOVE GLPA-CURRENT-SLPER TO SUB-3.
           IF GLPA-CURRENT-SLPER NOT = GLPA-CURRENT-CBPER
               GO TO UCB-030.
       UCB-005.
           MOVE GLPA-GLDRBANK TO CB-NUMBER.
           START CB-MASTER KEY NOT < CB-KEY.
       UCB-010.
           READ CB-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
               MOVE "DRBANK-ACC DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UCB-999.
           IF WS-CB-ST1 NOT = 0
               MOVE "DRBANK-ACC BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CB-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CB-ST1
               GO TO UCB-010.
           ADD WS-RUN-PAYMENTS TO CB-BALANCE
                                  CB-PER (SUB-3).
       UCB-020.
           REWRITE CB-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
               GO TO UCB-999.
           IF WS-CB-ST1 NOT = 0
               MOVE "DRBANK-ACC BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CB-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CB-ST1
               GO TO UCB-020.
       UCB-030.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-CBTRANSNO     TO CBTRANS-TRANS.
           ADD 1                   TO GLPA-CBTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 20                 TO CBTRANS-TYPE.
           MOVE WS-BATCH           TO CBTRANS-REFERENCE.
           IF GLPA-CURRENT-SLPER NOT = GLPA-CURRENT-CBPER
                MOVE "F"           TO CBTRANS-FUTURE
           ELSE
                MOVE " "           TO CBTRANS-FUTURE.
           MOVE SUB-3              TO CBTRANS-NO.
           MOVE GLPA-GLDRBANK      TO CBTRANS-CBMASTER.
           MOVE WS-DATE            TO CBTRANS-DATE.
           MOVE "D"                TO CBTRANS-TYPE-OF-POST.
           MOVE "N"                TO CBTRANS-ALLOCATED.
           MOVE GLPA-GLDEBT-NO     TO CBTRANS-ACCOUNT-NUMBER.
           MOVE WS-RUN-PAYMENTS    TO CBTRANS-AMOUNT.
           MOVE "DEP"              TO WS-CR1.
           MOVE WS-BATCH           TO WS-CRACC.
           MOVE "BATCH POST"       TO WS-CRINV.
           MOVE LINE-DESCRIPTION   TO CBTRANS-LINE-DESC.

           PERFORM WRITE-CBTRANS.
           MOVE 2710 TO POS.
           DISPLAY "                                                 "
              AT POS.
       UCB-999.
           EXIT.
      *
       WRITE-RD-CASHBOOK SECTION.
       WRCB-000.
           IF WS-TRANSTYPE NOT = "4"
               GO TO WRCB-999.
           MOVE 2910 TO POS
           DISPLAY "WRITING CASH-BOOK ENTRY........                "
               AT POS.
       WRCB-001.
           PERFORM READ-GLPARAMETER.
           MOVE GLPA-CURRENT-SLPER TO SUB-3.
           IF GLPA-CURRENT-SLPER NOT = GLPA-CURRENT-CBPER
               GO TO WRCB-030.
       WRCB-005.
           MOVE GLPA-GLDRBANK TO CB-NUMBER.
           START CB-MASTER KEY NOT < CB-KEY.
       WRCB-010.
           READ CB-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
               MOVE "DRBANK-ACC DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO WRCB-999.
           IF WS-CB-ST1 NOT = 0
                MOVE "DRBANK-ACC BUSY READ WRCB-010, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CB-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CB-ST1
                GO TO WRCB-010.
           SUBTRACT WS-PAYAMT FROM CB-BALANCE
                                   CB-PER (SUB-3).
       WRCB-020.
           REWRITE CB-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
               GO TO WRCB-999.
           IF WS-CB-ST1 NOT = 0
               MOVE "DRBANK-ACC BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CB-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CB-ST1
               GO TO WRCB-020.
       WRCB-030.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-CBTRANSNO     TO CBTRANS-TRANS.
           ADD 1                   TO GLPA-CBTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 4                  TO CBTRANS-TYPE.
           MOVE WS-BATCH           TO CBTRANS-REFERENCE.
           IF GLPA-CURRENT-SLPER NOT = GLPA-CURRENT-CBPER
                MOVE "F"           TO CBTRANS-FUTURE
           ELSE
                MOVE " "           TO CBTRANS-FUTURE.
           MOVE SUB-3              TO CBTRANS-NO.
           MOVE GLPA-GLDRBANK      TO CBTRANS-CBMASTER.
           MOVE WS-DATE            TO CBTRANS-DATE.
           MOVE "D"                TO CBTRANS-TYPE-OF-POST.
           MOVE "N"                TO CBTRANS-ALLOCATED.
           MOVE GLPA-GLDEBT-NO     TO CBTRANS-ACCOUNT-NUMBER.
           MOVE WS-PAYAMT          TO CBTRANS-AMOUNT.
           COMPUTE CBTRANS-AMOUNT = CBTRANS-AMOUNT * -1.
           MOVE "A/C"              TO WS-CR1.
           MOVE DR-ACCOUNT-NUMBER  TO WS-CRACC.
           MOVE "RD"               TO WS-CR2. 
           MOVE WS-CHEQUENO        TO WS-CRINV.
           MOVE LINE-DESCRIPTION   TO CBTRANS-LINE-DESC.

           PERFORM WRITE-CBTRANS.
           MOVE 2910 TO POS
           DISPLAY "                                                 "
              AT POS.
       WRCB-999.
           EXIT.
      *
       WRITE-REF-CASHBOOK SECTION.
       WREF-CB-000.
           IF WS-TRANSTYPE NOT = "6"
               GO TO WREF-CB-999.
           MOVE 2910 TO POS
           DISPLAY "WRITING CASH-BOOK REFUND ENTRY........            "
               AT POS.
       WREF-CB-001.
           PERFORM READ-GLPARAMETER.
           MOVE GLPA-CURRENT-SLPER TO SUB-3.
           IF GLPA-CURRENT-SLPER NOT = GLPA-CURRENT-CBPER
               GO TO WREF-CB-030.
       WREF-CB-005.
           MOVE GLPA-GLDRBANK TO CB-NUMBER.
           START CB-MASTER KEY NOT < CB-KEY.
       WREF-CB-010.
           READ CB-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
               MOVE "DRBANK-ACC DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO WREF-CB-999.
           IF WS-CB-ST1 NOT = 0
               MOVE "DRBANK-ACC BUSY ON READ, 'ESC' TO RETRY"
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CB-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CB-ST1
               GO TO WREF-CB-010.
           SUBTRACT WS-PAYAMT FROM CB-BALANCE
                                   CB-PER (SUB-3).
       WREF-CB-020.
           REWRITE CB-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
               GO TO WREF-CB-999.
           IF WS-CB-ST1 NOT = 0
               MOVE "DRBANK-ACC BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CB-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CB-ST1
               GO TO WREF-CB-020.
       WREF-CB-030.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-CBTRANSNO     TO CBTRANS-TRANS.
           ADD 1                   TO GLPA-CBTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 1                  TO CBTRANS-TYPE.
           MOVE WS-BATCH           TO CBTRANS-REFERENCE.
           IF GLPA-CURRENT-SLPER NOT = GLPA-CURRENT-CBPER
                MOVE "F"           TO CBTRANS-FUTURE
           ELSE
                MOVE " "           TO CBTRANS-FUTURE.
           MOVE SUB-3              TO CBTRANS-NO.
           MOVE GLPA-GLDRBANK      TO CBTRANS-CBMASTER.
           MOVE WS-DATE            TO CBTRANS-DATE.
           MOVE "S"                TO CBTRANS-TYPE-OF-POST.
           MOVE "N"                TO CBTRANS-ALLOCATED.
           MOVE GLPA-GLSALES-ADJ   TO CBTRANS-ACCOUNT-NUMBER.
           MOVE WS-PAYAMT          TO CBTRANS-AMOUNT.
           COMPUTE CBTRANS-AMOUNT = CBTRANS-AMOUNT * -1.
           MOVE "A/C"              TO WS-CR1.
           MOVE DR-ACCOUNT-NUMBER  TO WS-CRACC.
           MOVE "RF"               TO WS-CR2. 
           MOVE WS-CHEQUENO        TO WS-CRINV.
           MOVE LINE-DESCRIPTION   TO CBTRANS-LINE-DESC.

           PERFORM WRITE-CBTRANS.
           MOVE 2910 TO POS
           DISPLAY "                                                 "
              AT POS.
       WREF-CB-999.
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
              GO TO WCBTR-999.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS FILE BUSY ON WRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CB-ST1
              GO TO WCBTR-015.
       WCBTR-999.
           EXIT.
      *
       WRITE-GLTRANS SECTION.
       WRTR-015.
           WRITE GLTRANS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-GLTRANS-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS - NO FILE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRTR-015.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE "GLTRANS FILE BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO WRTR-015.
       WRTR-999.
           EXIT.
      *
       MAKE-DETAILS SECTION.
       MD-010.
           IF WS-TRANSTYPE = "5"
              GO TO MD-020.
           IF WS-TRANSTYPE NOT = "4"
              GO TO MD-999.
      *R/D CHEQUE SECTION
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO          TO GLTRANS-TRANS.
           ADD 1                        TO GLPA-GLTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE WS-BATCH                TO GLTRANS-REFERENCE.
           MOVE 4                       TO GLTRANS-TYPE.
           IF GLPA-CURRENT-GLPER = GLPA-CURRENT-SLPER
               MOVE " "                 TO GLTRANS-FUTURE
               MOVE GLPA-CURRENT-SLPER  TO GLTRANS-NO
           ELSE
               MOVE "F"                 TO GLTRANS-FUTURE
               MOVE GLPA-CURRENT-SLPER  TO GLTRANS-NO.
           MOVE WS-DATE                 TO GLTRANS-DATE.
           MOVE WS-PAYAMT               TO GLTRANS-AMOUNT.
           COMPUTE GLTRANS-AMOUNT = GLTRANS-AMOUNT * -1.
           MOVE GLPA-GLDRBANK           TO GLTRANS-ACCOUNT-NUMBER.
           MOVE "A/C"                   TO WS-CR1.
           MOVE DR-ACCOUNT-NUMBER       TO WS-CRACC.
           MOVE "RD"                    TO WS-CR2. 
           MOVE WS-CHEQUENO             TO WS-CRINV.
           MOVE LINE-DESCRIPTION        TO GLTRANS-LINE-DESC.
           GO TO MD-900.
       MD-020.
      *BAD DEBT SECTION
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO          TO GLTRANS-TRANS.
           ADD 1                        TO GLPA-GLTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE WS-BATCH                TO GLTRANS-REFERENCE.
           MOVE 5                       TO GLTRANS-TYPE.
           IF GLPA-CURRENT-GLPER = GLPA-CURRENT-SLPER
               MOVE " "                 TO GLTRANS-FUTURE
               MOVE GLPA-CURRENT-SLPER  TO GLTRANS-NO
           ELSE
               MOVE "F"                 TO GLTRANS-FUTURE
               MOVE GLPA-CURRENT-SLPER  TO GLTRANS-NO.
           MOVE WS-DATE                 TO GLTRANS-DATE.
           MOVE WS-PAYAMT               TO GLTRANS-AMOUNT.
           MOVE GLPA-GLBDEBT-ACC        TO GLTRANS-ACCOUNT-NUMBER.
           MOVE "A/C"                   TO WS-CR1.
           MOVE DR-ACCOUNT-NUMBER       TO WS-CRACC.
           MOVE "BD"                    TO WS-CR2. 
           MOVE WS-CHEQUENO             TO WS-CRINV.
           MOVE LINE-DESCRIPTION        TO GLTRANS-LINE-DESC.
       MD-900.
           PERFORM WRTR-015.
       MD-999.
           EXIT.
      *
       MAKE-DRACC-DETAILS SECTION.
       MDAC-010.
           IF WS-TRANSTYPE = "5"
              GO TO MDAC-020.
           IF WS-TRANSTYPE NOT = "4"
              GO TO MDAC-999.
      *R/D CHEQUE SECTION
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO          TO GLTRANS-TRANS.
           ADD 1                        TO GLPA-GLTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE WS-BATCH                TO GLTRANS-REFERENCE.
           MOVE 4                       TO GLTRANS-TYPE.
           IF GLPA-CURRENT-GLPER = GLPA-CURRENT-SLPER
               MOVE " "                 TO GLTRANS-FUTURE
               MOVE GLPA-CURRENT-SLPER  TO GLTRANS-NO
           ELSE
               MOVE "F"                 TO GLTRANS-FUTURE
               MOVE GLPA-CURRENT-SLPER  TO GLTRANS-NO.
           MOVE WS-DATE                 TO GLTRANS-DATE.
           MOVE WS-PAYAMT               TO GLTRANS-AMOUNT.
           MOVE GLPA-GLDEBT-NO          TO GLTRANS-ACCOUNT-NUMBER.
           MOVE "A/C"                   TO WS-CR1.
           MOVE DR-ACCOUNT-NUMBER       TO WS-CRACC.
           MOVE "RD"                    TO WS-CR2. 
           MOVE WS-CHEQUENO             TO WS-CRINV.
           MOVE LINE-DESCRIPTION        TO GLTRANS-LINE-DESC.
           GO TO MDAC-900.
       MDAC-020.
      *BAD DEBT SECTION
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO          TO GLTRANS-TRANS.
           ADD 1                        TO GLPA-GLTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE WS-BATCH                TO GLTRANS-REFERENCE.
           MOVE 5                       TO GLTRANS-TYPE.
           IF GLPA-CURRENT-GLPER = GLPA-CURRENT-SLPER
               MOVE " "                 TO GLTRANS-FUTURE
               MOVE GLPA-CURRENT-SLPER  TO GLTRANS-NO
           ELSE
               MOVE "F"                 TO GLTRANS-FUTURE
               MOVE GLPA-CURRENT-SLPER  TO GLTRANS-NO.
           MOVE WS-DATE                 TO GLTRANS-DATE.
           MOVE WS-PAYAMT               TO GLTRANS-AMOUNT.
           COMPUTE GLTRANS-AMOUNT = GLTRANS-AMOUNT * -1.
           MOVE GLPA-GLDEBT-NO          TO GLTRANS-ACCOUNT-NUMBER.
           MOVE "A/C"                   TO WS-CR1.
           MOVE DR-ACCOUNT-NUMBER       TO WS-CRACC.
           MOVE "BD"                    TO WS-CR2. 
           MOVE WS-CHEQUENO             TO WS-CRINV.
           MOVE LINE-DESCRIPTION        TO GLTRANS-LINE-DESC.
       MDAC-900.
           PERFORM WRTR-015.
       MDAC-999.
           EXIT.
      *
       UPDATE-GLMASTER SECTION.
       UPGL-000.
           IF GLPA-CURRENT-SLPER NOT = GLPA-CURRENT-GLPER
               GO TO UPGL-999.
           MOVE GLPA-CURRENT-SLPER TO SUB-3.
       UPGL-005.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGL-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLMASTER FILE DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE GL-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO UPGL-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLMASTER RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGL-010.
           ADD GLTRANS-AMOUNT TO GL-BALANCE
                                 GL-PER (SUB-3).
       UPGL-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGL-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLMASTER RECORD BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGL-900.
       UPGL-999.
           EXIT.
     *
       UPDATE-GLHEADER SECTION.
       UPGLH-005.
           IF GLPA-CURRENT-SLPER NOT = GLPA-CURRENT-GLPER
               GO TO UPGLH-999.
           MOVE GLPA-CURRENT-SLPER TO SUB-3.
           MOVE WS-GLHEADER        TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLHEADER FILE DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE GL-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO UPGLH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLHEADER FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGLH-010.
           ADD GLTRANS-AMOUNT TO GL-BALANCE
                                 GL-PER (SUB-3).
       UPGLH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLHEADER RECORD BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGLH-900.
       UPGLH-999.
           EXIT.
     *
       UPDATE-GLSUBHEADER SECTION.
       UPGLSH-005.
           IF GLPA-CURRENT-SLPER NOT = GLPA-CURRENT-GLPER
               GO TO UPGLSH-999.
           MOVE GLPA-CURRENT-SLPER TO SUB-3.
           MOVE WS-HEAD-SUB        TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLSH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLSUBHEAD FILE DOES'NT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE GL-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO UPGLSH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLSUBHEADER FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGLSH-010.
           ADD GLTRANS-AMOUNT TO GL-BALANCE
                                 GL-PER (SUB-3).
       UPGLSH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLSH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLSUBHEAD FILE BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGLSH-900.
       UPGLSH-999.
           EXIT.
      *
       READ-GLPARAMETER SECTION.
       RGLP-000.
           MOVE 1 TO GLPA-RECORD.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       RGLP-010.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE "GLPARAMETER BUSY RGLP-010, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLPARAMETER-ST1
               GO TO RGLP-010.
       RGLP-999.
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
               MOVE "GLPARAMETER BUSY RPL-010, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLPARAMETER-ST1
               GO TO RPL-010.
       RPL-999.
           EXIT.
      *
       REWRITE-GLPARAMETER SECTION.
       REWP-000.
           REWRITE GLPARAMETER-REC
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE "GLPARAMETER BUSY REWP-000, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLPARAMETER-ST1
               GO TO RP-000.
       REWP-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           IF WS-1STPRINT = "N"
              MOVE "Y" TO WS-1STPRINT
              PERFORM GET-USER-PRINT-NAME
              OPEN OUTPUT PRINT-FILE.
              
           IF DRTR-ACCOUNT-NUMBER = 0
              GO TO PRR-999.
             MOVE 2910 TO POS.
             DISPLAY "PRINTING CURRENT ALLOCATIONS......." AT POS.
       PRR-010.
           IF WS-LINE-CNT < 60
              GO TO PRR-020.
           ADD 1            TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT TO H1-PAGE.
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
           MOVE DRTR-TRANSACTION-NUMBER  TO D-TRANSNO
           MOVE DRTR-TYPE                TO D-TRANSCODE
           MOVE WS-TYPE-DESC (DRTR-TYPE) TO D-TRANSDESC
           MOVE DRTR-ACCOUNT-NUMBER      TO D-ACCOUNTNO
           MOVE DRTR-REFERENCE1          TO D-REFERENCE1
           MOVE DRTR-REFERENCE2          TO D-REFERENCE2
           MOVE DRTR-DATE                TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE             TO D-DATE
           MOVE DRTR-AMT-OF-INVOICE      TO D-BEGIN
           MOVE DRTR-AMT-OUTSTANDING     TO D-OUTST.
           IF WS-TRANSTYPE = "1" OR = "3" OR = "4" OR = "5"
                        OR = "6" OR = "7"
              MOVE 0 TO D-APPLIED
                        D-DISCOUNT
           ELSE
              MOVE WS-PAYMENT (SUB-1)  TO D-APPLIED
              MOVE WS-DISCOUNT (SUB-1) TO D-DISCOUNT.
           IF DRTR-TYPE = 2 OR = 5 OR = 6 OR = 9 OR = 11
              COMPUTE WS-WORK-FIELD = DRTR-AMT-OF-INVOICE
                                    - DRTR-AMT-OUTSTANDING
              MOVE WS-WORK-FIELD       TO D-APPLIED.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO WS-LINE-CNT.
       PRR-999.
           EXIT.
      *
       PRINT-TOTALS SECTION.
       PRT-000.
           IF WS-1STPRINT = "N"
               GO TO PRT-999.
           IF WS-LINE-CNT > 56
              MOVE 60 TO WS-LINE-CNT
              PERFORM PRR-010.
           IF WS-RUN-PAYMENTS = 0    AND WS-RUN-DISCOUNT = 0 
               AND WS-RUN-DEBITS = 0 AND WS-RUN-CREDITS = 0
               AND WS-RUN-BDEBT = 0  AND WS-RUN-RDCHEQUE = 0
               AND WS-RUN-REFUND = 0
               MOVE "*ALLOCATIONS OF OLD*" TO TOT2-DESC
               MOVE "*PAYMENTS & CREDITS*" TO TOT-NAME
               WRITE PRINT-REC FROM TOTAL2-LINE AFTER 2
               MOVE " " TO PRINT-REC
               GO TO PRT-900.
           MOVE "**** TOTAL PAYMENTS:" TO TOT-DESC
           MOVE WS-RUN-PAYMENTS        TO TOT-AMOUNT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
           MOVE " " TO PRINT-REC
           MOVE "**** TOTAL DISCOUNT:" TO TOT-DESC
           MOVE WS-RUN-DISCOUNT        TO TOT-AMOUNT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           MOVE "**** TOTAL DEBITS  :" TO TOT-DESC
           MOVE WS-RUN-DEBITS          TO TOT-AMOUNT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           MOVE "**** TOTAL CREDITS :" TO TOT-DESC
           MOVE WS-RUN-CREDITS         TO TOT-AMOUNT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           MOVE "**** TOTAL R-D CHEQ:" TO TOT-DESC
           MOVE WS-RUN-RDCHEQUE        TO TOT-AMOUNT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           MOVE "**** TOTAL B/DEBTS :" TO TOT-DESC
           MOVE WS-RUN-BDEBT           TO TOT-AMOUNT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           MOVE "**** TOTAL REFUNDS :" TO TOT-DESC
           MOVE WS-RUN-REFUND          TO TOT-AMOUNT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC.
           MOVE "**** TOTAL INT. REV:" TO TOT-DESC
           MOVE WS-RUN-INTEREST        TO TOT-AMOUNT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " " TO PRINT-REC.
       PRT-900.
           MOVE "**** BATCH NAME    :" TO TOT2-DESC
           MOVE WS-BATCH               TO TOT-NAME
           WRITE PRINT-REC FROM TOTAL2-LINE AFTER 1
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
            ADD 1  TO SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 > 489
                 MOVE 489 TO SUB-1.
            IF SUB-1 > SUB-9
               MOVE SUB-9 TO SUB-1.
            IF SUB-1 = 0
               MOVE 1 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 13
                GO TO NEXT-010.
            MOVE 1 TO F-INDEX.
            SUBTRACT 12 FROM SUB-1.
       NEXT-025.
            COMPUTE F-INDEX = (750 - SUB-1) + 1.
            IF F-INDEX > 12
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3015 TO POS.
            DISPLAY "Current Line #: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.

            IF WS-TYPEOFPROCESS NOT = "3" OR SUB-1 > 749
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
            MOVE 1 TO F-INDEX.
            IF SUB-1 > 489
                 MOVE 489 TO SUB-1.
            IF SUB-1 > SUB-9
               MOVE SUB-9 TO SUB-1.
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
            DISPLAY "Current Line #: " AT POS.
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
            DISPLAY "Current Line #: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       PREV-999.
            EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            MOVE "APPLY"          TO F-FIELDNAME
            MOVE 5                TO F-CBFIELDNAME
            MOVE WS-APPLY (SUB-1) TO F-NAMEFIELD
            MOVE 1                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REFNUM"          TO F-FIELDNAME
            MOVE 6                 TO F-CBFIELDNAME
            MOVE WS-REFNUM (SUB-1) TO F-EDNAMEFIELDNUM
            MOVE 6                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "TYPE"          TO F-FIELDNAME
            MOVE 4               TO F-CBFIELDNAME
            MOVE WS-TYPE (SUB-1) TO F-NAMEFIELD
            MOVE 7               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            IF WS-BODDATENUM (SUB-1) NOT > 0
                GO TO SCROLL-010.
            MOVE "DATE"                TO F-FIELDNAME
            MOVE 4                     TO F-CBFIELDNAME
            MOVE WS-BODDATENUM (SUB-1) TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE          TO F-NAMEFIELD
            MOVE 10                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-010.
            MOVE "AMTOFTYPE" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME.
            IF WS-AMTOFTYPE (SUB-1) = 0
               MOVE " " TO F-NAMEFIELD
               MOVE 10  TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE WS-AMTOFTYPE (SUB-1) TO F-EDNAMEFIELD9MIL
               MOVE 10                   TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-9MIL.

            MOVE "PAYMENT" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME.
            IF WS-APPLY (SUB-1) = " "
               MOVE " " TO F-NAMEFIELD
               MOVE 9   TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               GO TO SCROLL-020.
            MOVE WS-PAYMENT (SUB-1) TO F-EDNAMEFIELD9MIL
            MOVE 10                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.
       SCROLL-020.
            MOVE "DISCOUNT" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME.
            IF WS-APPLY (SUB-1) = " "
               MOVE " " TO F-NAMEFIELD
               MOVE 10  TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               GO TO SCROLL-999.
            MOVE WS-DISCOUNT (SUB-1) TO F-EDNAMEFIELD9MIL
            MOVE 10                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.
       SCROLL-999.
             EXIT.
      *
       CLEAR-BODY SECTION.
       CLEAR-BODY-000.
            MOVE 1   TO F-INDEX
            MOVE " " TO F-NAMEFIELD.
       CLEAR-BODY-010.
            MOVE "APPLY" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE 1       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REFNUM" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE 6        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE 7      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE 10     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMTOFTYPE" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PAYMENT" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE 10        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DISCOUNT" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE 10         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            ADD 1 TO F-INDEX.
            IF F-INDEX < 13
             GO TO CLEAR-BODY-010.
       CLEAR-BODY-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
             MOVE 2930 TO POS.
             MOVE " " TO WS-MESSAGE.
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
                         WS-TYPE (SUB-1).
             MOVE 0   TO WS-REFNUM (SUB-1)
                         WS-BODDATE (SUB-1)
                         WS-AMTOFTYPE (SUB-1)
                         WS-PAYMENT (SUB-1)
                         WS-DISCOUNT (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 751
                 GO TO CF-010.
       CF-020.
             MOVE 0 TO CR-AMTOFTYPE
                       CR-REFNUM
                       CR-BODDATE
                       CR-PAYMENT
                       CR-DRTYPE
                       CR-DR-TRANSNO.
             MOVE " " TO CR-APPLY
                         CR-TYPE
                         CR-OLD-PAYMENT.
       CF-999.
             EXIT.
     *
       DISPLAY-TOP-INFO SECTION.
       DTI-005.
           MOVE "BATCHNAME"   TO F-FIELDNAME
           MOVE 9             TO F-CBFIELDNAME
           MOVE WS-BATCH-REST TO F-NAMEFIELD
           MOVE 8             TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "BATCHDATE" TO F-FIELDNAME
           MOVE 9           TO F-CBFIELDNAME
           MOVE H1-DATE     TO F-NAMEFIELD
           MOVE 10          TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "BATCHTOTAL"   TO F-FIELDNAME
           MOVE 10             TO F-CBFIELDNAME
           MOVE WS-BATCH-TOTAL TO F-EDNAMEFIELD9MIL
           MOVE 10             TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-9MIL.

           MOVE "RUNTOTAL"      TO F-FIELDNAME
           MOVE 8               TO F-CBFIELDNAME
           MOVE WS-RUN-PAYMENTS TO F-EDNAMEFIELD9MIL
           MOVE 10              TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-9MIL.
       DTI-999.
           EXIT.
      *
       ENTER-BATCH-DETAILS SECTION.
       EBD-005.
           MOVE "BATCHNAME" TO F-FIELDNAME
           MOVE 9           TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD
           MOVE 8           TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA
           MOVE F-NAMEFIELD TO WS-BATCH-REST.
           IF WS-BATCH-REST = " "
            IF F-EXIT-CH NOT = X"04"
               MOVE "THE BATCH MUST HAVE A VALID NAME, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO EBD-005.
           
           IF F-EXIT-CH = X"01" OR = X"04" OR = X"07"
                 PERFORM UPDATE-CASHBOOK
                 PERFORM PRINT-TOTALS
                 PERFORM END-OFF.
       EBD-010.
           MOVE "BATCHDATE" TO F-FIELDNAME
           MOVE 9           TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD
           MOVE 10          TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA.
           IF F-EXIT-CH = X"01"
                GO TO EBD-005.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
               GO TO EBD-010.
           MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           MOVE DISPLAY-DATE    TO F-NAMEFIELD H1-PAY-DATE.
           PERFORM WRITE-FIELD-ALPHA.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE      TO WS-PAY-DATE WS-DATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
               GO TO EBD-010.
           
            MOVE WS-CURRENTPER      TO SUB-1.
            MOVE GL-BEGDATE (SUB-1) TO WS-BEG-DATE.
            MOVE GL-ENDDATE (SUB-1) TO WS-END-DATE.
            
            IF SPLIT-DATE NOT < WS-BEG-DATE
             IF SPLIT-DATE NOT > WS-END-DATE
                GO TO EBD-020.
            IF SPLIT-DATE > WS-END-DATE
            MOVE "THE JOURNAL DATE MUST BE < OR = THE END PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO EBD-010.
            IF SPLIT-DATE < WS-BEG-DATE
            MOVE "THE JOURNAL DATE MUST > OR = THE BEG. PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO EBD-010.
       EBD-020.
           MOVE "BATCHTOTAL" TO F-FIELDNAME
           MOVE 10           TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
            IF WS-ABOVE-BODY NOT = "8"
                GO TO EBD-010.
           MOVE 10           TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA
           MOVE F-NAMEFIELD  TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-BATCH-TOTAL F-EDNAMEFIELD9MIL
           PERFORM WRITE-FIELD-9MIL.
       EBD-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
           IF WS-1STPRINT = "Y"
              MOVE "TRYING TO RE-OPEN FILES, 'ESC' X2 TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR-MESSAGE
              EXIT PROGRAM.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER FILE BUSY OPEN-005, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-005.
           PERFORM READ-PARAMETER.
           MOVE Ws-Co-Name TO CO-NAME.

           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.

           IF WS-MM = PA-CURRENT-PER-MM
            IF WS-YY = PA-CURRENT-PER-YY
               GO TO OPEN-010.
           MOVE 0410 TO POS
           DISPLAY "*** DEBTOR PAYMENT ALLOCATION PROGRAM ***" AT POS
           MOVE 1010 TO POS.
           DISPLAY "THE CURRENT MONTH OR YEAR ON THE PARAMETER FILE"
               AT POS.
           MOVE 1110 TO POS.
           DISPLAY "    DOES NOT CORRESPOND WITH TODAYS DATE!!!!" AT POS
           MOVE 1210 TO POS.
           DISPLAY "         GO AND CHECK THE SYSTEM DATE, " AT POS
           MOVE 1310 TO POS.
           DISPLAY "   AS IT APPEARS YOU'RE IN THE WRONG MONTH." AT POS
           MOVE 1610 TO POS.
           DISPLAY "   PRESS 'GO' OR 'NEXT' TO END THE PROGRAM." AT POS.
           
           MOVE 3010 TO POS.
           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 60        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.

           IF W-ESCAPE-KEY = 1 OR = 2
               CLOSE PARAMETER-FILE
               EXIT PROGRAM
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO OPEN-005.
       OPEN-010.
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
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER FILE BUSY OPEN-012, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO OPEN-012.
       OPEN-013.
           OPEN I-O DISTRIBUTIONS.
           IF WS-DISTRIBUTION-ST1 NOT = 0
               MOVE "DISTRIBUTION BUSY OPEN-013, 'ESC' TO RETRY" 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DISTRIBUTION-ST1
               GO TO OPEN-013.
       OPEN-014.
           OPEN I-O DEBTOR-TRANS-FILE.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DR TRANS BUSY OPEN-014, 'ESC' TO RETRY" 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO OPEN-014.
       OPEN-015.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-GLPARAMETER-ST1
               GO TO OPEN-015.

           PERFORM READ-GLPARAMETER.
           MOVE GLPA-CURRENT-SLPER TO WS-CURRENTPER.
           PERFORM ENTER-PERIOD-DATES.
       OPEN-016.
           OPEN I-O CBTRANS-FILE.
           IF WS-CBTRANS-ST1 NOT = 0 
              MOVE "CB-TRANS BUSY ON OPEN, PRESS 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO OPEN-016.
       OPEN-017.
           OPEN I-O CB-MASTER.
           IF WS-CB-ST1 NOT = 0
               MOVE "CBMASTER FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CB-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CB-ST1
               GO TO OPEN-017.
       OPEN-018.
           OPEN I-O GLTRANS-FILE.
           IF WS-GLTRANS-ST1 NOT = 0 
              MOVE "GL-TRANS BUSY ON OPEN, PRESS 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO OPEN-018.
       OPEN-019.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0 
              MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO OPEN-019.
       OPEN-020.
            MOVE Ws-Forms-Name   TO F-FILENAME
            MOVE Ws-cbForms-name TO F-CBFILENAME.
            MOVE "DrPaymnt" TO F-FORMNAME
            MOVE 8          TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           IF WS-1STPRINT = "Y"
              CLOSE PRINT-FILE
              PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE DEBTOR-MASTER
                 DEBTOR-TRANS-FILE
                 DISTRIBUTIONS
                 PARAMETER-FILE
                 GLPARAMETER-FILE
                 CBTRANS-FILE
                 GLTRANS-FILE
                 CB-MASTER
                 GL-MASTER.
       END-900.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldAccount".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldNumeric5".
       Copy "WriteField9Mil".
       Copy "EnterPeriodDates".
       Copy "ReadKBD".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "DebtorSpecPassword".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
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
      *
      * END-OF-JOB
