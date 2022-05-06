       IDENTIFICATION DIVISION.
       PROGRAM-ID. CrInCrMt.
       AUTHOR.  CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
        SPECIAL-NAMES.
           CRT STATUS IS W-CRTSTATUS.
       SOURCE-COMPUTER. B20.
       OBJECT-COMPUTER. B20.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           Copy "SelectGlMaster".
           Copy "SelectGlParameter".
           Copy "SelectSlParameter".
           Copy "SelectGlTrans".
           Copy "SelectCrMaster".
           Copy "SelectCrJrn".
           Copy "SelectCrTrans".
           Copy "SelectCrCurrency".
           Copy "SelectSlDaily".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdGlMast.
           COPY ChlfdGlParam.
           COPY ChlfdParam.
           COPY ChlfdGlTrans.
           COPY ChlfdCrJrn.
           COPY ChlfdCrTrans.
           COPY ChlfdCreditor.
           COPY ChlfdCrCurr.
           COPY ChlfdDaily.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(135).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  LINE-PRINTED         PIC 9(3) VALUE 0.
       77  PSW-SUB1             PIC S9(5) VALUE 0.
       77  PSW-SUB2             PIC S9(5) VALUE 0.
       77  WS-NEWBATCH          PIC X VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-TYPE-OF-READ      PIC X VALUE "I".
       77  WS-NEWINVOICE        PIC X VALUE " ".
       77  WS-LINE-CHANGED      PIC X VALUE " ".
       77  WS-FUTURE-BATCH      PIC X VALUE " ".
       77  WS-TOTAL-GLAMTS      PIC S9(7)V99.
       77  WS-SETT-DISC-CHECK   PIC S9(7)V99.
       77  WS-LOCAL-PURCHASE    PIC S9(7)V99.
       77  WS-FOREIGN-PURCHASE  PIC S9(7)V99.
       77  WS-CURRENTPER        PIC 99 VALUE 0.
       77  WS-CREDITOR          PIC 9(7).
       77  WS-CURRENTGLPER      PIC 99 VALUE 0.
       77  WS-TYPE-OF-END       PIC X VALUE " ".
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-PAGE              PIC 99 VALUE 0.
       77  WS-INQUIRY           PIC X(8) VALUE "CrNameIq".
       01  W-READ-KEY           PIC X.
       01  W-CRTSTATUS             PIC 9(4) VALUE 0.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1       PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1  PIC 99.
       01  WS-GLTRANS-STATUS.
           03  WS-GLTRANS-ST1      PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1     PIC 99.
       01  WS-CRJRN-STATUS.
           03  WS-CRJRN-ST1        PIC 99.
       01  WS-CRTRANS-STATUS.
           03  WS-CRTRANS-ST1      PIC 99.
       01  WS-CURRENCY-STATUS.
           03  WS-CU-ST1           PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       01  WS-PASSWORD-KEY.
           03  WS-PA-KEY           PIC X OCCURS 11.
       01  WS-PRINTER-INFO.
           03  WS-PRN-FIL          PIC X(8) VALUE " ".
           03  WS-PRN-NAME         PIC X(12) VALUE " ".
       01  JOURNAL-DATA.
           03  WS-JRN.
               05  WS-JRN-1STCHAR   PIC X(2) VALUE "PI".
               05  WS-JRN-REST      PIC X(8).
           03  WS-INV-TOTAL         PIC 9(3).
           03  WS-CONTROL-TOTAL     PIC S9(7)V99.
           03  WS-BATCH-TOTAL       PIC S9(7)V99.
           03  WS-VAT-TOTAL         PIC S9(7)V99.
           03  WS-VAT-AMT           PIC S9(7)V99.
           03  WS-VAT-PERC          PIC 99V99.
           03  WS-INV-AMT           PIC S9(7)V99.
           03  WS-UNAPPLIED         PIC S9(7)V99.
           03  WS-FORAMT            PIC S9(7)V99.
           03  WS-SETT-DISC         PIC S9(7)V99.
           03  WS-INV-NO            PIC X(10).
           03  WS-DNOTE-NO          PIC X(10).
       01  LINE-DESCRIPTION.
           03  WS-CR1              PIC X(4).
           03  WS-CRACC            PIC X(8).
           03  WS-CR2              PIC X(3).
           03  WS-CRINV            PIC X(10).
       01  WS-JRNPERIOD-IND.
           03  WS-1STPER           PIC X.
           03  WS-REST             PIC 99.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-GLHEADER     PIC X(2).
               05  WS-GLSUBHEADER  PIC X(4).
           03  WS-RESTOFACCOUNT    PIC X(6).
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       01  BATCH-DATE.
           03  BATCH-YY       PIC 9999.
           03  BATCH-MM       PIC 99.
           03  BATCH-DD       PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(8) VALUE " ".
           03  H1-FUTURE      PIC X(22).
           03  FILLER         PIC X(34) VALUE
           "PURCHASE LEDGER LISTING OF BATCH:".
           03  H1-BATCH       PIC X(36) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(10) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(45) VALUE " ".
           03  FILLER         PIC X(33) VALUE ALL "*".
           03  FILLER         PIC X(54) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(10) VALUE "ACCOUNT".
           03  FILLER         PIC X(23) VALUE "INVOICE    D/NOTE".
           03  FILLER         PIC X(26) VALUE "INV DATE   DUE DATE  ".
           03  FILLER         PIC X(11) VALUE "PERIOD".
           03  FILLER         PIC X(14) VALUE "LOCAL AMT".
           03  FILLER         PIC X(13) VALUE "SETT. DISC".
           03  FILLER         PIC X(15) VALUE "FOREIGN AMT".
           03  FILLER         PIC X(12) VALUE "CURRENCY".
           03  FILLER         PIC X(10) VALUE "VAT AMT".
       01  DETAIL-LINE1.
           03  D1-ACCOUNT      PIC X(10).
           03  D1-INVOICE      PIC X(11).
           03  D1-DNOTE        PIC X(11).
           03  D1-DATE         PIC X(10).
           03  FILLER          PIC X(1) VALUE " ".
           03  D1-DUEDATE      PIC X(10).
           03  FILLER          PIC X(7) VALUE " ".
           03  D1-PERIOD       PIC X(3) VALUE " ".
           03  FILLER          PIC X(6) VALUE " ".
           03  D1-LOC-AMT      PIC Z(6)9.99-.
           03  FILLER          PIC X(2) VALUE " ".
           03  D1-DISC         PIC Z(6)9.99-.
           03  D1-DISC-ERR     PIC X(2) VALUE " ".
           03  D1-FOR-AMT      PIC Z(6)9.99- BLANK WHEN ZERO.
           03  FILLER          PIC X(2) VALUE " ".
           03  D1-CURR         PIC Z99.9(5) BLANK WHEN ZERO.
           03  FILLER          PIC X(2) VALUE " ".
           03  D1-VAT-AMT      PIC Z(6)9.99- BLANK WHEN ZERO.
           03  FILLER          PIC X(2) VALUE " ".
       01  DETAIL-LINE2.
           03  FILLER          PIC X(10) VALUE " ".
           03  D2-ACCOUNT      PIC X(14).
           03  D2-DESC         PIC X(45).
           03  D2-AMT          PIC Z(6)9.99-.
           03  FILLER          PIC X(2) VALUE " ".
           03  D2-DISC         PIC Z(6)9.99- BLANK WHEN ZERO.
           03  FILLER          PIC X(39) VALUE " ".
       01  DETAIL-ACCLINE.
           03  D-ACCNAME      PIC X(132) VALUE " ".
       01  TOTAL-LINE1.
           03  T1-NAME        PIC X(41).
           03  T1-QTY         PIC Z(3)9.
           03  FILLER         PIC X(87) VALUE " ".
       01  TOTAL-LINE2.
           03  T2-NAME        PIC X(35).
           03  T2-QTY         PIC Z(6)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  T2-ERROR       PIC X(2) VALUE " ".
           03  FILLER         PIC X(82) VALUE " ".
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
           DISPLAY "** CREDITORS INVOICE/C-NOTE ENTRY PROGRAM **"
            AT POS
           MOVE 0410 TO POS
           DISPLAY "********************************************"
            AT POS.
       CONTROL-003.
           Copy "PrinterAcceptCr".
       CONTROL-010.
           MOVE 0 TO BATCH-DATE.
           PERFORM OPEN-FILES.
           PERFORM READ-SALES-LEDGER-PARAMETER.
           PERFORM CLEAR-SCREEN.
       CONTROL-020.
      *     IF WS-FUTURE-BATCH = "F"
           PERFORM OPEN-013.
           PERFORM CLEAR-FIELDS.
           PERFORM DISPLAY-FORM.
           
           PERFORM GET-001.
           
           PERFORM DISPLAY-TOP-INFO.
           PERFORM GET-DATA.
           GO TO CONTROL-020.
       CONTROL-030.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
       CONTROL-999.
           EXIT.
      *
       ENTER-BATCH-DETAILS SECTION.
       EBD-005.
           MOVE "                                    " TO F-NAMEFIELD
           MOVE "PERIOD"      TO F-FIELDNAME
           MOVE 6             TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           MOVE 2             TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA
           MOVE F-NAMEFIELD   TO ALPHA-RATE
           PERFORM DECIMALISE-RATE.
           IF NUMERIC-RATE > 12
               MOVE "PERIOD CANNOT BE > 12, RE-ENTER" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO EBD-005.
           IF NUMERIC-RATE > WS-CURRENTPER
               MOVE "F" TO WS-FUTURE-BATCH.
      ******************************************************************
      * THE ASSUMPTION HERE IS THAT IF WE ARE IN FEB AND WANT TO       *
      * PROCESS A MARCH BATCH OF INVOICES THE PERNO=1 THEREFOR IT      *
      * MUST BE IN THE "FUTURE".  IT IS UNLIKELY THAT ONE WOULD PROCESS*
      * AN INVOICE BATCH FOR MARCH IF PERNO < 12.                      *
      ******************************************************************
           IF NUMERIC-RATE < WS-CURRENTPER
            IF WS-CURRENTPER = 12
               MOVE "F" TO WS-FUTURE-BATCH
            ELSE
               MOVE "P" TO WS-FUTURE-BATCH.
           MOVE NUMERIC-RATE  TO WS-CURRENTPER
           MOVE WS-CURRENTPER TO F-NAMEFIELD
           MOVE 2             TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       EBD-999.
           EXIT.
      *
       ACCEPT-BATCH-DATE SECTION.
       PT-002.
           PERFORM OPEN-016
           MOVE "                                    " TO F-NAMEFIELD
           MOVE "BATCHDATE" TO F-FIELDNAME
           MOVE 9           TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           MOVE 10           TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA
           MOVE F-NAMEFIELD TO ALPHA-RATE
           PERFORM DATE-CHECKING
            IF SIGN-FOUND = 9
              GO TO PT-002.
           MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE
           MOVE DISPLAY-DATE    TO F-NAMEFIELD
           PERFORM WRITE-FIELD-ALPHA
           PERFORM CONVERT-SPLIT-FORMAT
           MOVE SPLIT-DATE TO BATCH-DATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO PT-002.

            MOVE WS-CURRENTPER TO SUB-1
            MOVE GL-BEGDATE (SUB-1) TO WS-BEG-DATE
            MOVE GL-ENDDATE (SUB-1) TO WS-END-DATE.
            IF BATCH-DATE NOT < WS-BEG-DATE
             IF BATCH-DATE NOT > WS-END-DATE
                GO TO PT-999.
            IF BATCH-DATE > WS-END-DATE
            MOVE "THE BATCH DATE MUST BE < OR = THE END PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO PT-002.
            IF BATCH-DATE < WS-BEG-DATE
            MOVE "THE BATCH DATE MUST > OR = THE BEG. PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO PT-002.
       PT-999.
           EXIT.
      *
       DISPLAY-TOP-INFO SECTION.
       DTI-005.
           MOVE "FUTURE" TO F-FIELDNAME
           MOVE 6        TO F-CBFIELDNAME.
           IF WS-FUTURE-BATCH = "Y" OR = "F"
               MOVE "F"  TO F-NAMEFIELD
               GO TO DTI-006.
           IF WS-FUTURE-BATCH = "P"
               MOVE "P"  TO F-NAMEFIELD
           ELSE
               MOVE " "  TO F-NAMEFIELD.
       DTI-006.
           MOVE 1        TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA

           MOVE "BATCHDATE"  TO F-FIELDNAME
           MOVE 9            TO F-CBFIELDNAME
           MOVE 10           TO F-CBFIELDLENGTH
           MOVE BATCH-DATE   TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO F-NAMEFIELD
           PERFORM WRITE-FIELD-ALPHA.

           MOVE "PERIOD"      TO F-FIELDNAME
           MOVE 6             TO F-CBFIELDNAME
           MOVE WS-CURRENTPER TO F-NAMEFIELD
           MOVE 2             TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA

           MOVE WS-CURRENTPER      TO SUB-1
           MOVE "BEGDATE"          TO F-FIELDNAME
           MOVE 7                  TO F-CBFIELDNAME
           MOVE GL-BEGDATE (SUB-1) TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO F-NAMEFIELD
           MOVE 10                 TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA

           MOVE WS-CURRENTPER      TO SUB-1
           MOVE "ENDDATE"          TO F-FIELDNAME
           MOVE 7                  TO F-CBFIELDNAME
           MOVE GL-ENDDATE (SUB-1) TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO F-NAMEFIELD
           MOVE 10                 TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       DTI-999.
           EXIT.
      *
       CHECK-GL-SHORT-NO SECTION.
       C-GL-SH-N-010.
           MOVE 1 TO SUB-2.
       C-GL-SH-N-020.
           IF GLPA-IND (SUB-2) = " "
                GO TO C-GL-SH-N-999.
           IF GLPA-IND (SUB-2) = F-NAMEFIELD
                MOVE GLPA-NO (SUB-2) TO F-NAMEFIELD
                GO TO C-GL-SH-N-999.
           IF SUB-2 NOT > 9
                ADD 1 TO SUB-2
                GO TO C-GL-SH-N-020.
       C-GL-SH-N-999.
           EXIT.
      *
       PRINT-BATCH SECTION.
       PRR-000.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE 0 TO WS-INV-TOTAL   WS-CONTROL-TOTAL
                     WS-BATCH-TOTAL WS-SETT-DISC   WS-UNAPPLIED
                     WS-VAT-TOTAL   WS-LOCAL-PURCHASE
                     WS-FOREIGN-PURCHASE LINE-PRINTED
                     WS-TOTAL-GLAMTS.

           MOVE WS-JRN TO CRJRN-REFERENCE.
           MOVE 0 TO CRJRN-CRACC-NUMBER.
           START CRJRN-FILE KEY NOT < CRJRN-CRACC-NUMBER
              INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 0 TO WS-CRJRN-ST1
              GO TO PRR-900.
       PRR-002.
           READ CRJRN-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 10
              GO TO PRR-900.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CRJRN RECORD LOCKED ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO PRR-002.
           IF CRJRN-REFERENCE NOT = WS-JRN
              GO TO PRR-002.

            IF CRJRN-INV-DATE < GL-BEGDATE (WS-CURRENTPER)
               GO TO PRR-002.
            IF CRJRN-INV-DATE > GL-ENDDATE (WS-CURRENTPER)
               GO TO PRR-002.
              
           ADD 1 TO LINE-PRINTED
           MOVE 2610 TO POS
           DISPLAY "LINE:" AT POS
           ADD 5 TO POS
           DISPLAY LINE-PRINTED AT POS
           
           MOVE 2620 TO POS
           DISPLAY "ACCOUNT & INVOICE #:" AT POS
           MOVE CRJRN-CRACC-NUMBER TO WS-CRACC
           MOVE CRJRN-INV-NO       TO WS-CRINV
           ADD 20 TO POS
           DISPLAY LINE-DESCRIPTION AT POS.
              
           IF LINE-CNT < 60
               GO TO PRR-020.
       PRR-010.
           ADD 1         TO PAGE-CNT
           MOVE WS-JRN   TO H1-BATCH
           MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF F-EXIT-CH = X"9F"
               MOVE "** REPRINT **"  TO H1-FUTURE.
           IF WS-FUTURE-BATCH = "Y" OR = "F"
               MOVE "FUTURE BATCH "  TO H1-FUTURE.
           IF WS-FUTURE-BATCH = "P"
               MOVE "PREVIOUS BATCH" TO H1-FUTURE.
           IF WS-FUTURE-BATCH = "B"
               MOVE "MIXD PERIODS "  TO H1-FUTURE.
           IF WS-FUTURE-BATCH = "N"
               MOVE "CURRENT BATCH"  TO H1-FUTURE.
           MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD3
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE 6 TO LINE-CNT.
       PRR-020.
           MOVE CRJRN-CRACC-NUMBER TO D1-ACCOUNT WS-CREDITOR
           MOVE CRJRN-INV-NO       TO D1-INVOICE
           MOVE CRJRN-DNOTE-NO     TO D1-DNOTE
           MOVE CRJRN-INV-DATE     TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D1-DATE
           MOVE CRJRN-DUE-DATE     TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO D1-DUEDATE
           MOVE CRJRN-PERIOD       TO D1-PERIOD
           MOVE CRJRN-LOC-AMT      TO D1-LOC-AMT
           MOVE CRJRN-VAT-AMT      TO D1-VAT-AMT
           MOVE CRJRN-SETT-DISC    TO D1-DISC
           MOVE CRJRN-FOR-AMT      TO D1-FOR-AMT
           MOVE CRJRN-EXCHANGE     TO D1-CURR.
           
           MOVE "  " TO D1-DISC-ERR.
      ****************************************************************
      * TAKEN OUT 14/1/2008 AS WE WANT TO CHECK ALL TRANSACTIONS TO  *
      * SEE IF THEY ADD UP CORRECTLY.                                *
      *     IF CRJRN-SETT-DISC > 0
              PERFORM CHECK-DISC-TOTALS.

           WRITE PRINT-REC FROM DETAIL-LINE1
           MOVE " " TO PRINT-REC DETAIL-LINE1
           ADD 1               TO WS-INV-TOTAL
           ADD CRJRN-LOC-AMT   TO WS-BATCH-TOTAL
           ADD CRJRN-VAT-AMT   TO WS-VAT-TOTAL
           ADD CRJRN-FOR-AMT   TO WS-CONTROL-TOTAL
           ADD CRJRN-SETT-DISC TO WS-SETT-DISC.

           PERFORM READ-CREDITOR
           MOVE CR-NAME TO D-ACCNAME
           WRITE PRINT-REC FROM DETAIL-ACCLINE
           MOVE " " TO PRINT-REC DETAIL-ACCLINE
           ADD 2 TO LINE-CNT.
           IF LINE-CNT > 60
              PERFORM PRR-010.

           MOVE 1 TO SUB-1.
       PRR-060.
           IF CRJRN-GLACC (SUB-1) = "    "
                WRITE PRINT-REC
                WRITE PRINT-REC
                ADD 2 TO LINE-CNT
                GO TO PRR-002.
           MOVE CRJRN-GLACC (SUB-1)  TO D2-ACCOUNT
           MOVE CRJRN-GLAMT (SUB-1)  TO D2-AMT
           MOVE CRJRN-GLDISC (SUB-1) TO D2-DISC
           MOVE CRJRN-GLDESC (SUB-1) TO D2-DESC.
           
           IF CRJRN-GLACC (SUB-1) = "50-200-05-00"
             ADD CRJRN-GLAMT (SUB-1) TO WS-LOCAL-PURCHASE.
           IF CRJRN-GLACC (SUB-1) = "50-200-10-00"
             ADD CRJRN-GLAMT (SUB-1) TO WS-FOREIGN-PURCHASE.

           WRITE PRINT-REC FROM DETAIL-LINE2.
           MOVE " " TO PRINT-REC DETAIL-LINE2.
           ADD 1 TO LINE-CNT.

           IF LINE-CNT > 60
              PERFORM PRR-010.
           IF SUB-1 NOT > 9
              ADD 1 TO SUB-1
              GO TO PRR-060.
           GO TO PRR-002.
       PRR-900.
            IF LINE-CNT > 56
               PERFORM PRR-010.
           MOVE "No. of Invoices In Batch        :" TO T1-NAME
           MOVE WS-INV-TOTAL                        TO T1-QTY
           WRITE PRINT-REC FROM TOTAL-LINE1
           MOVE " " TO PRINT-REC.

           MOVE "Total Amount Of Local Currency  :" TO T2-NAME
           MOVE WS-BATCH-TOTAL                      TO T2-QTY
           WRITE PRINT-REC FROM TOTAL-LINE2
           MOVE " " TO PRINT-REC.

           MOVE "Total Amount Of Input VAT       :" TO T2-NAME
           MOVE WS-VAT-TOTAL                        TO T2-QTY
           WRITE PRINT-REC FROM TOTAL-LINE2
           MOVE " " TO PRINT-REC.

           MOVE "VAT Percentage used             :" TO T2-NAME
           MOVE WS-VAT-PERC                         TO T2-QTY
           WRITE PRINT-REC FROM TOTAL-LINE2
           MOVE " " TO PRINT-REC.

           MOVE "Total Amount Of Foreign Currency:" TO T2-NAME
           MOVE WS-CONTROL-TOTAL                    TO T2-QTY
           WRITE PRINT-REC FROM TOTAL-LINE2
           MOVE " " TO PRINT-REC.

           MOVE "Total Amount Of Settlement Disc.:" TO T2-NAME
           MOVE WS-SETT-DISC                        TO T2-QTY
           WRITE PRINT-REC FROM TOTAL-LINE2
           MOVE " " TO PRINT-REC.

           MOVE "Total Posted to CONTROL A/C     :" TO T2-NAME.
           MOVE WS-BATCH-TOTAL                      TO T2-QTY
           IF WS-TOTAL-GLAMTS NOT = WS-BATCH-TOTAL
               MOVE "**"                            TO T2-ERROR.
           WRITE PRINT-REC FROM TOTAL-LINE2
           MOVE " " TO PRINT-REC TOTAL-LINE2.

           MOVE "Total Local Purchases in Batch  :" TO T2-NAME
           MOVE WS-LOCAL-PURCHASE                   TO T2-QTY
           WRITE PRINT-REC FROM TOTAL-LINE2
           MOVE " " TO PRINT-REC.

           MOVE "Total Foreign Purchases in Batch:" TO T2-NAME
           MOVE WS-FOREIGN-PURCHASE                 TO T2-QTY
           WRITE PRINT-REC FROM TOTAL-LINE2
           MOVE " " TO PRINT-REC.

           IF WS-FUTURE-BATCH = "B"
           MOVE 
           "THIS BATCH HAS MIXED PERIODS IN IT & CANNOT BE POSTED."
           TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC.
           
           IF WS-FUTURE-BATCH = "Y" OR = "F"
           MOVE 
           "THIS BATCH IS FOR A FUTURE PERIOD & CANNOT BE POSTED."
           TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC.
           
           IF WS-FUTURE-BATCH = "P"
           MOVE 
           "THIS BATCH IS FOR A PREVIOUS PERIOD & CANNOT BE POSTED."
           TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC.


           MOVE 
          " *=DISC In Error, **=GL-AMT'S In Error, $*=DISC & " &
          "GL-AMT's In Error." TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC.
           
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           MOVE 0  TO PAGE-CNT
           MOVE 66 TO LINE-CNT.
           PERFORM ERROR1-020.
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
       PRR-999.
           EXIT.
      *
       CHECK-DISC-TOTALS SECTION.
       CDT-005.
           MOVE 0 TO WS-SETT-DISC-CHECK.
           MOVE 1 TO SUB-1.
       CDT-010.
           IF CRJRN-GLACC (SUB-1) = "    "
              GO TO CDT-900.
           ADD CRJRN-GLDISC (SUB-1) TO WS-SETT-DISC-CHECK.
           IF SUB-1 < 10
              ADD 1 TO SUB-1
              GO TO CDT-010.
       CDT-900.
           IF WS-SETT-DISC-CHECK NOT = CRJRN-SETT-DISC
               MOVE " *" TO D1-DISC-ERR.
      *         GO TO CDT-999.
       CDT-1005.
           MOVE 0 TO WS-SETT-DISC-CHECK.
           MOVE 1 TO SUB-1.
       CDT-1010.
           IF CRJRN-GLACC (SUB-1) = "    "
              GO TO CDT-1900.
              
           ADD CRJRN-GLAMT (SUB-1)  TO WS-SETT-DISC-CHECK
                                       WS-TOTAL-GLAMTS
      *     ADD CRJRN-GLDISC (SUB-1) TO WS-SETT-DISC-CHECK.
           IF SUB-1 < 10
              ADD 1 TO SUB-1
              GO TO CDT-1010.
       CDT-1900.
           IF WS-SETT-DISC-CHECK NOT = CRJRN-LOC-AMT
            IF D1-DISC-ERR = " *"
               MOVE "$*" TO D1-DISC-ERR
            ELSE
               MOVE "**" TO D1-DISC-ERR.
       CDT-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-001.
            MOVE "Printer:"   TO WS-PRN-FIL
            MOVE Ws-Printer   To WS-PRN-NAME
            MOVE 0359 TO POS
            DISPLAY WS-PRINTER-INFO AT POS.
       GET-010.
      * RESET FLAGS.
            MOVE 0       TO SUB-20 SUB-25.
            MOVE "Y"     TO WS-NEWBATCH.
            MOVE " " TO WS-MESSAGE
                        WS-TYPE-OF-END
                        WS-ACCEPT.
            MOVE "N" TO WS-FUTURE-BATCH.
            
            PERFORM ENTER-BATCH-DETAILS.
            PERFORM DISPLAY-TOP-INFO.
            PERFORM ACCEPT-BATCH-DATE.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "BATCH" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                PERFORM DISPLAY-FORM
                PERFORM DISPLAY-TOP-INFO
                GO TO GET-010.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = "    "
                GO TO GET-010.
            MOVE F-NAMEFIELD TO WS-JRN-REST.
            PERFORM READ-NEXT-CRJRN.
            IF WS-CRJRN-ST1 = 88
                MOVE 0 TO WS-CRJRN-ST1
                GO TO GET-010.
            IF WS-NEWBATCH = "Y"
                PERFORM CLEAR-FIELDS
                GO TO GET-015.
      ************************************************************
      * 'ESC' = X"07"; 'CODE-CANCEL' = X"87" ; 'ALT-F10' = X"9F" *
      ************************************************************
           IF WS-NEWBATCH = "P"
            IF F-EXIT-CH NOT = X"9F" AND NOT = X"87"
               MOVE
              "THIS BATCH HAS ALREADY BEEN POSTED, USE A NEW NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-999.
            MOVE 2915 TO POS.
            DISPLAY
             "PRESS <F5> TO POST THIS INVOICE BATCH, <F10> TO PRINT,"
             AT POS.
            MOVE 2815 TO POS.
            DISPLAY
             "OR PRESS <ALT-F10> TO CANCEL THIS INVOICE BATCH."
             AT POS.
       GET-015.
            MOVE "BATCH"     TO F-FIELDNAME.
            MOVE 5           TO F-CBFIELDNAME.
            MOVE WS-JRN      TO CRJRN-REFERENCE.
            MOVE WS-JRN-REST TO F-NAMEFIELD.
            MOVE 8           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-016.
            MOVE "CONTROLAMT"     TO F-FIELDNAME
            MOVE 10               TO F-CBFIELDNAME
            MOVE WS-CONTROL-TOTAL TO F-EDNAMEFIELDNUM6
            MOVE 11               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC6.
       GET-020.
            MOVE "INVOICECOUNT" TO F-FIELDNAME
            MOVE 12             TO F-CBFIELDNAME
            MOVE WS-INV-TOTAL   TO F-EDNAMEFIELDLINE
            MOVE 3              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-LINE.

            MOVE "BATCHAMT"     TO F-FIELDNAME
            MOVE 8              TO F-CBFIELDNAME
            MOVE WS-BATCH-TOTAL TO F-EDNAMEFIELDNUM6
            MOVE 11             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC6.
       GET-050.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "CONTROLAMT" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-CONTROL-TOTAL F-EDNAMEFIELDNUM6.
            MOVE WS-CONTROL-TOTAL TO F-EDNAMEFIELDNUM6.
            PERFORM WRITE-FIELD-NUMERIC6.

            IF F-EXIT-CH = X"01"
             IF WS-CONTROL-TOTAL = WS-BATCH-TOTAL
                GO TO GET-999
             ELSE
                MOVE "THE BATCH TOTAL & CONTROL TOTALS ARE NOT EQUAL."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-050.
                
      *      MOVE " " TO WS-MESSAGE
      *      MOVE 2915 TO POS
      *      DISPLAY WS-MESSAGE AT POS
      *      MOVE 2815 TO POS
      *      DISPLAY WS-MESSAGE AT POS.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            MOVE SPACES TO WS-DNOTE-NO
                           WS-INV-NO.
            
            IF F-EXIT-CH = X"19"
             IF WS-FUTURE-BATCH = "B"
                MOVE 
           "THIS BATCH HAS A MIX OF PERIODS, POSTING IS NOT ALLOWED."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-999.
           IF WS-FUTURE-BATCH = "B"
            IF F-EXIT-CH NOT = X"1F"
                MOVE 
           "THIS BATCH HAS A MIX OF PERIODS, PRINTING ONLY IS ALLOWED."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-999.
                
            IF F-EXIT-CH = X"19"
             IF WS-FUTURE-BATCH = "Y" OR = "F"
                MOVE 
           "THIS BATCH IS FOR A FUTURE PERIOD & POSTING IS NOT ALLOWED."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-999.
            IF F-EXIT-CH = X"19"
             IF WS-FUTURE-BATCH = "P"
                MOVE 
           "THIS BATCH IS FOR A PREVIOUS PERIOD, POSTING NOT ALLOWED."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-999.
                
            IF F-EXIT-CH = X"19"
              PERFORM CHECK-PASSWORD.
            IF WS-PASSWORD-VALID = "N"
              MOVE 
            "THE PASSWORD YOU ENTERED IS INCORRECT, PLEASE TRY AGAIN."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO GET-999.
                
            IF F-EXIT-CH = X"19"
             IF WS-FUTURE-BATCH = "N"
              IF GLPA-CURRENT-CRPER = WS-CURRENTPER
                MOVE 2915 TO POS
                DISPLAY
                "POSTING OF INVOICE BATCH IN PROGRESS..........." AT POS
                PERFORM POST-TRANSACTIONS
                MOVE " " TO WS-MESSAGE
                MOVE 2915 TO POS
                DISPLAY WS-MESSAGE AT POS
                GO TO GET-999.
            IF F-EXIT-CH = X"19"
             IF WS-NEWBATCH NOT = "P"
              IF GLPA-CURRENT-CRPER NOT = WS-CURRENTPER
                MOVE "POSTING OF FUTURE BATCHES IS NOT ALLOWED."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-999.
            IF F-EXIT-CH = X"1F" OR = X"9F"
                MOVE 2915 TO POS
                DISPLAY
                "PRINTING OF INVOICE BATCH IN PROGRESS.........." AT POS
                PERFORM PRINT-BATCH
                MOVE " " TO WS-MESSAGE
                MOVE 2915 TO POS
                DISPLAY WS-MESSAGE AT POS
                GO TO GET-999.
            IF F-EXIT-CH = X"87" OR = X"9F"
             IF WS-NEWBATCH NOT = "P"
                MOVE 2915 TO POS
                DISPLAY
                "DELETING OF INVOICE BATCH IN PROGRESS.........." AT POS
                PERFORM DELETE-JRN
                MOVE " " TO WS-MESSAGE
                MOVE 2915 TO POS
                DISPLAY WS-MESSAGE AT POS
                GO TO GET-999.
       GET-060.
            IF WS-INV-NO NOT = " "
                PERFORM DIS-KEY-FUNC-005.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "DNOTE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-050.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-FIELDS
                PERFORM CLEAR-BODY
                PERFORM CLEAR-HEAD-DETAILS
                PERFORM DISPLAY-FORM
                PERFORM DISPLAY-TOP-INFO
                PERFORM GET-015 THRU GET-020
                GO TO GET-060.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = " "
               MOVE "          " TO WS-DNOTE-NO
               GO TO GET-070.
            MOVE F-NAMEFIELD TO WS-DNOTE-NO.
            MOVE "D" TO WS-ACCEPT.
            PERFORM GET-080.
            IF F-EXIT-CH = X"01"
                GO TO GET-060.
            PERFORM READ-CRJRN-DNOTES.
            IF WS-CRJRN-ST1 = 88
               GO TO GET-060.
            IF WS-NEWINVOICE = "N"
               PERFORM GET-065 THRU GET-067
               PERFORM SCROLL-NEXT
               PERFORM ENTER-INV-ONLY
               GO TO GET-090
            ELSE
               GO TO GET-070.
       GET-061.
            MOVE "DNOTE"     TO F-FIELDNAME
            MOVE 5           TO F-CBFIELDNAME
            MOVE WS-DNOTE-NO TO F-NAMEFIELD
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-065.
            MOVE "DNOTE"        TO F-FIELDNAME.
            MOVE 5              TO F-CBFIELDNAME.
            MOVE CRJRN-DNOTE-NO TO F-NAMEFIELD.
            MOVE 10             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE CRJRN-INV-NO TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUPPLIER"         TO F-FIELDNAME.
            MOVE 8                  TO F-CBFIELDNAME.
            MOVE CRJRN-CRACC-NUMBER TO F-NAMEFIELD WS-CREDITOR. 
            MOVE 7                  TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM READ-CREDITOR.
            MOVE "SUPPLIERNAME" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE CR-NAME TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-066.
            MOVE "LOCALAMT" TO F-FIELDNAME
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CRJRN-LOC-AMT TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "FOREIGNAMT" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE CRJRN-FOR-AMT TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "VATAMT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            IF WS-NEWINVOICE NOT = "N"
               COMPUTE CRJRN-VAT-AMT ROUNDED =
                 (CRJRN-LOC-AMT / (100 + WS-VAT-PERC)) * WS-VAT-PERC.
            MOVE CRJRN-VAT-AMT TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "EXCHANGE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            IF WS-NEWINVOICE = "N"
                MOVE CRJRN-EXCHANGE TO CU-VALUE.
            MOVE CU-VALUE TO F-EDNAMEFIELDVALUE.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-VALUE.
        GET-067.
            MOVE "SETTLEDISC" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE CRJRN-SETT-DISC TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "INVDATE"      TO F-FIELDNAME.
            MOVE 7              TO F-CBFIELDNAME.
            MOVE CRJRN-INV-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE   TO F-NAMEFIELD.
            MOVE 10             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DUEDATE"      TO F-FIELDNAME.
            MOVE 7              TO F-CBFIELDNAME.
            MOVE CRJRN-DUE-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE   TO F-NAMEFIELD.
            MOVE 10             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM COMPUTE-UNAPPLIED-AMT.
       GET-070.
            PERFORM DIS-KEY-FUNC-007.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-060.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-FIELDS
                PERFORM CLEAR-BODY
                PERFORM DISPLAY-FORM
                PERFORM CLEAR-HEAD-DETAILS
                PERFORM DISPLAY-TOP-INFO
                PERFORM GET-015 THRU GET-020
                GO TO GET-060.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = "       "
                CLOSE CREDITOR-MASTER
                PERFORM CLEAR-SCREEN
                CALL WS-INQUIRY USING WS-LINKAGE
                PERFORM CLEAR-SCREEN
                CANCEL WS-INQUIRY
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                PERFORM DISPLAY-TOP-INFO
                PERFORM GET-015 THRU GET-020
                PERFORM GET-061
                GO TO GET-070.
            MOVE F-NAMEFIELD TO WS-INV-NO.
            IF WS-DNOTE-NO = "    "
                 MOVE WS-INV-NO TO WS-DNOTE-NO.
       GET-080.
            PERFORM DIS-KEY-FUNC-050.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "SUPPLIER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
             IF WS-ACCEPT NOT = "D"
                GO TO GET-070.
            MOVE 7            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-CREDITOR.
            PERFORM READ-CREDITOR.

            MOVE "SUPPLIERNAME" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE CR-NAME TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            IF CR-NAME = "** UNKNOWN **"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-080.
       GET-085.
            PERFORM READ-CRJRN-INVOICES.
            IF WS-CRJRN-ST1 = 88
               GO TO GET-070.
            IF WS-NEWINVOICE = "N"
                PERFORM GET-065 THRU GET-067
                PERFORM SCROLL-NEXT.
       GET-090.
            IF WS-INV-NO = " "
                PERFORM ENTER-INV-ONLY.
            IF CR-FOREIGN-LOCAL = "F"
                 GO TO GET-091.
            PERFORM DIS-KEY-FUNC-010.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "LOCALAMT" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-080.
            IF F-EXIT-CH = X"87" OR = X"9F"
             IF WS-NEWINVOICE = "Y"
                PERFORM CLEAR-HEAD-DETAILS
                PERFORM CLEAR-FIELDS
                PERFORM CLEAR-BODY
                GO TO GET-060
             ELSE
                SUBTRACT 1 FROM WS-INV-TOTAL
                SUBTRACT CRJRN-LOC-AMT FROM WS-BATCH-TOTAL
                PERFORM DELETE-INVOICE
                PERFORM GET-020
                PERFORM CLEAR-HEAD-DETAILS
                PERFORM CLEAR-FIELDS
                PERFORM CLEAR-BODY
                GO TO GET-060.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            IF WS-NEWINVOICE = "N"
             IF NUMERIC-RATE NOT = CRJRN-LOC-AMT
                 SUBTRACT CRJRN-LOC-AMT FROM WS-BATCH-TOTAL
                 SUBTRACT 1 FROM WS-INV-TOTAL
                 PERFORM GET-020.
            MOVE NUMERIC-RATE TO CRJRN-LOC-AMT CRJRN-UNAPPLIED-AMT
                                 F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
            MOVE 0 TO CRJRN-FOR-AMT CRJRN-EXCHANGE.
            PERFORM GET-066.

            GO TO GET-095.
       GET-091.
            PERFORM DIS-KEY-FUNC-010.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "FOREIGNAMT" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-080.
            IF F-EXIT-CH = X"87" OR = X"9F"
             IF WS-NEWINVOICE = "Y"
                PERFORM CLEAR-HEAD-DETAILS
                PERFORM CLEAR-FIELDS
                PERFORM CLEAR-BODY
                GO TO GET-060
             ELSE
                SUBTRACT 1 FROM WS-INV-TOTAL
                SUBTRACT CRJRN-LOC-AMT FROM WS-BATCH-TOTAL
                PERFORM DELETE-INVOICE
                PERFORM GET-020
                PERFORM CLEAR-HEAD-DETAILS
                PERFORM CLEAR-FIELDS
                PERFORM CLEAR-BODY
                GO TO GET-060.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            IF WS-NEWINVOICE = "N"
             IF NUMERIC-RATE NOT = CRJRN-FOR-AMT
                 SUBTRACT CRJRN-LOC-AMT FROM WS-BATCH-TOTAL
                 SUBTRACT 1 FROM WS-INV-TOTAL.
            MOVE NUMERIC-RATE TO CRJRN-FOR-AMT F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF WS-NEWINVOICE = "N"
             IF F-EXIT-CH NOT = X"1D"
                 ADD CRJRN-LOC-AMT TO WS-BATCH-TOTAL
                 PERFORM GET-066
                 GO TO GET-093.
            PERFORM READ-CURRENCY.
            COMPUTE CRJRN-LOC-AMT ROUNDED =
                      CRJRN-FOR-AMT / CU-VALUE.
            MOVE CRJRN-LOC-AMT TO CRJRN-UNAPPLIED-AMT.
            IF WS-NEWINVOICE = "N"
                 ADD CRJRN-LOC-AMT TO WS-BATCH-TOTAL.
            PERFORM GET-066.
       GET-093.
            PERFORM DIS-KEY-FUNC-050.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "EXCHANGE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-093.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CU-VALUE CRJRN-EXCHANGE
                                 F-EDNAMEFIELDVALUE.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-VALUE.
            COMPUTE CRJRN-LOC-AMT ROUNDED =
                      CRJRN-FOR-AMT / CU-VALUE.
            MOVE CRJRN-LOC-AMT TO CRJRN-UNAPPLIED-AMT.
            PERFORM GET-066.
       GET-095.
           PERFORM DIS-KEY-FUNC-050.
            
           MOVE 2910 TO POS
           DISPLAY 
           "IF THE LOC-AMT HAS CHANGED, <F8> TO CHANGE THE SETT-DISC."
              AT POS.
           MOVE "                                    " TO F-NAMEFIELD.
           MOVE "VATAMT" TO F-FIELDNAME.
           MOVE 6 TO F-CBFIELDNAME.
           MOVE 11 TO F-CBFIELDLENGTH.
      *      IF WS-NEWINVOICE NOT = "N"
              COMPUTE CRJRN-VAT-AMT ROUNDED =
                (CRJRN-LOC-AMT / (100 + WS-VAT-PERC)) * WS-VAT-PERC.
              MOVE CRJRN-VAT-AMT TO F-EDNAMEFIELDNUM6
              MOVE 11 TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-NUMERIC6.
           PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-090.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRJRN-VAT-AMT
                                 F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
       GET-096.
            PERFORM ERROR1-020.
            IF WS-NEWINVOICE = "N"
             IF F-EXIT-CH NOT = X"1D"
                GO TO GET-100.
            COMPUTE NUMERIC-RATE = (CRJRN-LOC-AMT * CR-SETT-DISC) / 100.
            MOVE "SETTLEDISC" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6.
            MOVE 11           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
       GET-100.
           PERFORM DIS-KEY-FUNC-050.
           MOVE 2910 TO POS
           DISPLAY "TO CHANGE THE INV-DATE & DUE-DATE PRESS <F8>."
               AT POS.

           MOVE "                                    " TO F-NAMEFIELD.
           MOVE "SETTLEDISC" TO F-FIELDNAME.
           MOVE 10 TO F-CBFIELDNAME.
           MOVE 11 TO F-CBFIELDLENGTH.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
               GO TO GET-095.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE.
           MOVE NUMERIC-RATE TO CRJRN-SETT-DISC F-EDNAMEFIELDNUM6.
           MOVE 11 TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-NUMERIC6.
       GET-150.
            PERFORM ERROR1-020.
            IF WS-NEWINVOICE = "N"
             IF F-EXIT-CH NOT = X"1D"
                GO TO GET-190
             ELSE
                GO TO GET-152.
            PERFORM OPEN-016.
            MOVE "INVDATE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            IF WS-DATE > BATCH-DATE
              MOVE BATCH-DATE TO SPLIT-DATE
            ELSE
              MOVE WS-DATE    TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            GO TO GET-153.
       GET-152.
            PERFORM DIS-KEY-FUNC-050.
            MOVE "INVDATE"      TO F-FIELDNAME.
            MOVE 7              TO F-CBFIELDNAME.
            MOVE CRJRN-INV-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE   TO F-NAMEFIELD.
            MOVE 10             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-153.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "INVDATE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-100.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO GET-153.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CRJRN-INV-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO GET-153.

            MOVE WS-CURRENTPER TO SUB-1.
            MOVE GL-BEGDATE (SUB-1) TO WS-BEG-DATE.
            MOVE GL-ENDDATE (SUB-1) TO WS-END-DATE.
            IF CRJRN-INV-DATE NOT < WS-BEG-DATE
             IF CRJRN-INV-DATE NOT > WS-END-DATE
                GO TO GET-158.
            IF CRJRN-INV-DATE > WS-END-DATE
            MOVE "THE JOURNAL DATE MUST BE < OR = THE END PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-153.
            IF CRJRN-INV-DATE < WS-BEG-DATE
            MOVE "THE JOURNAL DATE MUST > OR = THE BEG. PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-153.
       GET-158.
            PERFORM ERROR-020.
            MOVE "DUEDATE"      TO F-FIELDNAME.
            MOVE 7              TO F-CBFIELDNAME.
            PERFORM CHECK-DUE-DATE.
            MOVE CRJRN-DUE-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE   TO F-NAMEFIELD.
            MOVE 10             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH NOT = X"1D"
                GO TO GET-190.
       GET-160.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "DUEDATE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-153.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO GET-160.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CRJRN-DUE-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO GET-160.
       GET-190.
            PERFORM DIS-KEY-FUNC-050.
            MOVE 1 TO SUB-1 F-INDEX.
            PERFORM SCROLL-NEXT.
            PERFORM FILL-BODY.
            IF WS-ABOVE-BODY = "1"
                GO TO GET-160.
       GET-195.
           MOVE " " TO WS-MESSAGE.
           PERFORM ERROR-020.
           ADD 1 TO WS-INV-TOTAL.
           ADD CRJRN-LOC-AMT TO WS-BATCH-TOTAL.
           PERFORM REWRITE-CRJRN.
           PERFORM GET-020.
           PERFORM CLEAR-HEAD-DETAILS.
           PERFORM CLEAR-FIELDS.
           PERFORM CLEAR-BODY.
           GO TO GET-060.
       GET-999.
           EXIT.
      *
       ENTER-INV-ONLY SECTION.
       EOI-010.
            MOVE "INVOICE"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            MOVE WS-DNOTE-NO TO F-NAMEFIELD
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       EOI-070.
            MOVE SPACES    TO F-NAMEFIELD.
            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10        TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-INV-NO CRJRN-INV-NO.
            IF WS-INV-NO = "    "
              MOVE "THIS ENTRY CANNOT BE BLANK, PLEASE RE-ENTER."
              TO WS-MESSAGE
              GO TO EOI-070.
       EOI-999.
           EXIT.
      *
       DISPLAY-KEY-FUNCTIONS SECTION.
       DIS-KEY-FUNC-005.
           MOVE 2910 TO POS.
           DISPLAY
           "PRESS 'ESC' TO CLEAR THE ENTRY.                         "
           AT POS.
       DIS-KEY-FUNC-007.
           MOVE 2910 TO POS.
           DISPLAY
           "PRESS 'ESC' TO CLEAR ENTRY, LEAVE BLANK FOR NAME ENQUIRY"
           AT POS.
       DIS-KEY-FUNC-010.
           MOVE 2910 TO POS.
           DISPLAY
           "PRESS 'ALT-F10' TO DELETE THIS ENTRY.                  "
           AT POS.
       DIS-KEY-FUNC-050.
           MOVE 2910 TO POS.
           DISPLAY
           "                                                           "
           AT POS.
       DIS-KEY-FUNC-999.
           EXIT.
      *
       COMPUTE-UNAPPLIED-AMT SECTION.
       CUA-005.
            MOVE SUB-1 TO SUB-3.
            MOVE CRJRN-LOC-AMT TO WS-UNAPPLIED.
            MOVE 0 TO WS-INV-AMT WS-SETT-DISC WS-VAT-AMT.
            MOVE 1 TO SUB-1.
       CUA-010.
            IF CRJRN-GLACC (SUB-1) = " "
                GO TO CUA-020.
            SUBTRACT CRJRN-GLAMT (SUB-1) FROM WS-UNAPPLIED.
            ADD CRJRN-GLAMT (SUB-1)        TO WS-INV-AMT.
            ADD CRJRN-GLDISC (SUB-1)       TO WS-SETT-DISC.
            IF CRJRN-GLACC (SUB-1) = GLPA-GLVAT-ACC
                ADD CRJRN-GLAMT (SUB-1)    TO WS-VAT-AMT.
            ADD 1 TO SUB-1.
            IF SUB-1 NOT > 10
               GO TO CUA-010.
       CUA-020.
            MOVE SUB-3 TO SUB-1.
            MOVE "UNAPPLIED"  TO F-FIELDNAME.
            MOVE 9            TO F-CBFIELDNAME.
            MOVE WS-UNAPPLIED TO F-EDNAMEFIELDNUM6.
            MOVE 11           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
       CUA-999.
           EXIT.
      *
       FILL-BODY SECTION.
       FILL-000.
           MOVE " " TO WS-ABOVE-BODY.
           MOVE 1 TO SUB-1 SUB-2 SUB-3.
           IF WS-NEWINVOICE = "Y"
               MOVE "N" TO WS-LINE-CHANGED
           ELSE
               MOVE "Y" TO WS-LINE-CHANGED.
       FILL-005.
           PERFORM ERROR-020.
       FILL-010.
           PERFORM COMPUTE-UNAPPLIED-AMT.
           MOVE SUB-1 TO F-INDEX.
           MOVE "                              " TO F-NAMEFIELD.
           MOVE "GLNUMBER" TO F-FIELDNAME.
           MOVE 8 TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.

           IF F-EXIT-CH = X"0B" 
            IF CRJRN-GLACC (SUB-1) = " "
               GO TO FILL-010.

           IF F-EXIT-CH = X"01" AND F-INDEX = 1
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM READ-FIELD-ALPHA
            IF F-NAMEFIELD = CRJRN-GLACC (SUB-1)
               MOVE "1" TO WS-ABOVE-BODY
               GO TO FILL-999
            ELSE
               MOVE CRJRN-GLACC (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               MOVE "1" TO WS-ABOVE-BODY
               GO TO FILL-999.

           MOVE 12 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           IF F-EXIT-CH = X"01"
            IF CRJRN-GLACC (SUB-1) = "  "
               SUBTRACT 1 FROM F-INDEX SUB-1
               GO TO FILL-010.
           
           IF F-EXIT-CH = X"01" AND F-INDEX > 1
            IF F-NAMEFIELD = CRJRN-GLACC (SUB-1)
               SUBTRACT 1 FROM F-INDEX SUB-1
               MOVE "Y" TO WS-LINE-CHANGED
               GO TO FILL-010
            ELSE
               MOVE CRJRN-GLACC (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               SUBTRACT 1 FROM F-INDEX SUB-1
               MOVE "Y" TO WS-LINE-CHANGED
               GO TO FILL-010.

           IF F-EXIT-CH = X"0B" AND F-INDEX < 10
            IF F-NAMEFIELD = CRJRN-GLACC (SUB-1)
               ADD 1 TO F-INDEX SUB-1
               GO TO FILL-010
            ELSE
               MOVE CRJRN-GLACC (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               ADD 1 TO F-INDEX SUB-1
               GO TO FILL-010.
           IF F-EXIT-CH = X"0B" AND F-INDEX = 10
            IF F-NAMEFIELD = CRJRN-GLACC (SUB-1)
               GO TO FILL-010
            ELSE
               MOVE CRJRN-GLACC (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               GO TO FILL-010.
      * TAB CHARACTER
           IF F-EXIT-CH = X"09"
            IF F-NAMEFIELD = CRJRN-GLACC (SUB-1)
               MOVE " " TO WS-ABOVE-BODY
               PERFORM ERROR-020
               GO TO FILL-900
            ELSE
               MOVE CRJRN-GLACC (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               MOVE " " TO WS-ABOVE-BODY
               PERFORM ERROR-020
               GO TO FILL-900.
           IF F-EXIT-CH = X"07"
               AND CRJRN-GLACC (SUB-1) = " "
               GO TO FILL-010.
           IF F-EXIT-CH = X"87" OR = X"9F"
               MOVE SUB-1 TO SUB-7
               PERFORM CANCEL-TRANSACTION
               MOVE 1 TO SUB-1
                         F-INDEX
               PERFORM SCROLL-NEXT
               GO TO FILL-010.
           IF F-NAMEFIELD = " "
                GO TO FILL-010.
           IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B"
                GO TO FILL-010.

           IF F-NAMEFIELD = "A" OR = "B" OR = "C" OR = "D" OR = "E"
                       OR = "F" OR = "G" OR = "H" OR = "I" OR = "J"
                PERFORM CHECK-GL-SHORT-NO.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           IF ALPHA-RATE > SPACES
                PERFORM NUMBER-CHECK.
           MOVE WS-GLNO-CHECK TO CRJRN-GLACC (SUB-1) WS-GLNUMBER.
           IF WS-GLSUBHEADER = "    "
                MOVE "YOU CAN ONLY POST TO A DETAIL ACCOUNT."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE " " TO CRJRN-GLACC (SUB-1)
                GO TO FILL-010.
           IF WS-RESTOFACCOUNT = "      "
                MOVE "YOU CAN ONLY POST TO A DETAIL ACCOUNT."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE " " TO CRJRN-GLACC (SUB-1)
                GO TO FILL-010.
           IF CRJRN-GLAMT (SUB-1) = 0
               MOVE "N" TO WS-LINE-CHANGED
           ELSE
               MOVE "Y" TO WS-LINE-CHANGED.
           PERFORM READ-GLMASTER.
           IF GL-DESCRIPTION = "INVALID"
                MOVE "INVALID GLMASTER NUMBER !!" TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE " " TO CRJRN-GLACC (SUB-1)
                GO TO FILL-010.

            MOVE "GLNUMBER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-GLNO-CHECK TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM ERROR-020.
       FILL-015.
            MOVE "GLNAME" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE GL-DESCRIPTION TO CRJRN-GLDESC (SUB-1) F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       FILL-020.
            IF WS-LINE-CHANGED = "Y"
             IF GL-NUMBER = GLPA-GLVAT-ACC
                SUBTRACT CRJRN-GLAMT (SUB-1) FROM WS-INV-AMT
                SUBTRACT CRJRN-GLAMT (SUB-1) FROM WS-VAT-AMT
             ELSE
                SUBTRACT CRJRN-GLAMT (SUB-1) FROM WS-INV-AMT.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "GLAMOUNT" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.
            MOVE "                       " TO F-NAMEFIELD.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRJRN-GLAMT (SUB-1).
            IF CRJRN-GLAMT (SUB-1) = 0
                MOVE 2801 TO POS
                DISPLAY " " AT 3079 WITH BELL
                GO TO FILL-020.
           IF GL-NUMBER = GLPA-GLVAT-ACC
                ADD CRJRN-GLAMT (SUB-1) TO WS-VAT-AMT.
            MOVE CRJRN-GLAMT (SUB-1) TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
       FILL-030.
            IF CR-SETT-DISC = 0
                GO TO FILL-050.
            IF WS-LINE-CHANGED = "Y"
                SUBTRACT CRJRN-GLDISC (SUB-1) FROM WS-SETT-DISC.
            IF GL-NUMBER = GLPA-GLVAT-ACC
               COMPUTE NUMERIC-RATE = CRJRN-SETT-DISC - WS-SETT-DISC
            ELSE
               COMPUTE NUMERIC-RATE =
                  (CRJRN-GLAMT (SUB-1) * CR-SETT-DISC) / 100.

            MOVE "GLSETDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
       FILL-050.
            MOVE "                                " TO F-NAMEFIELD.
            MOVE "GLSETDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
                SUBTRACT CRJRN-GLAMT (SUB-1) FROM WS-INV-AMT
                GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-050.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRJRN-GLDISC (SUB-1).
            ADD CRJRN-GLDISC (SUB-1) TO WS-SETT-DISC.
 
            MOVE CRJRN-GLDISC (SUB-1) TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
       FILL-090.
           IF F-INDEX < 10
               ADD 1 TO SUB-1 F-INDEX.
           MOVE "N" TO WS-LINE-CHANGED.
           GO TO FILL-010.
       FILL-900.
            PERFORM COMPUTE-UNAPPLIED-AMT.
            IF WS-INV-AMT NOT = CRJRN-LOC-AMT
                MOVE
            "THE ALLOCATED AMT'S DO NOT = THE ORIGINAL INVOICE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-010.
            IF WS-SETT-DISC NOT = CRJRN-SETT-DISC
                MOVE
            "THIS ALLOCATED DISC'S DO NOT = THE ORIGINAL DISCOUNT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-010.
            IF WS-VAT-AMT NOT = CRJRN-VAT-AMT
                MOVE
           "THE ALLOCATED VAT AMT DOES NOT = THE HEADER VAT AMT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-010.
       FILL-999.
           EXIT.
      *
       CLEAR-HEAD-DETAILS SECTION.
       CHD-005.
            MOVE "DNOTE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUPPLIER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD. 
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUPPLIERNAME" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "LOCALAMT" TO F-FIELDNAME
            MOVE 8 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "VATAMT" TO F-FIELDNAME
            MOVE 6 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "FOREIGNAMT" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SETTLEDISC" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVDATE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 10        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DUEDATE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 10        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "EXCHANGE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "UNAPPLIED" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       CHD-010.
            MOVE " " TO WS-INV-NO
                        WS-DNOTE-NO.
            MOVE 0 TO WS-INV-AMT
                      WS-UNAPPLIED
                      WS-FORAMT
                      WS-VAT-AMT
                      WS-SETT-DISC.
       CHD-999.
           EXIT.
      *
       CHECK-DUE-DATE SECTION.
       CDD-010.
           MOVE CRJRN-INV-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE CONVERT-DATE TO WS-CH-DATE.
           IF CR-TERMS = " " OR = "1" OR = "3" OR = "8"
              MOVE 25 TO WS-CH-DD
              ADD 1 TO WS-CH-MM
              GO TO CDD-020.
           IF CR-TERMS = "4"
              MOVE 25 TO WS-CH-DD
              ADD 2 TO WS-CH-MM
              GO TO CDD-020.
           IF CR-TERMS = "5"
              MOVE 25 TO WS-CH-DD
              ADD 3 TO WS-CH-MM
              GO TO CDD-020.
           IF CR-TERMS = "6"
              MOVE 25 TO WS-CH-DD
              ADD 4 TO WS-CH-MM
              GO TO CDD-020.
      *7=180DAYS   USED TO BE LETTER OF CREDIT    23/10/2000
           IF CR-TERMS = "7"
              MOVE 25 TO WS-CH-DD
              ADD 6 TO WS-CH-MM.
       CDD-020.
           IF WS-CH-MM > 12
              SUBTRACT 12 FROM WS-CH-MM
              ADD 1 TO WS-CH-YY.
      *LINES BELOW REMOVED FOR LINES ABOVE   23/10/2000
      *     IF WS-CH-MM = 13
      *        MOVE 1 TO WS-CH-MM 
      *        ADD 1 TO WS-CH-YY.
      *     IF WS-CH-MM = 14
      *        MOVE 2 TO WS-CH-MM 
      *        ADD 1 TO WS-CH-YY.
      *     IF WS-CH-MM = 15
      *        MOVE 3 TO WS-CH-MM 
      *        ADD 1 TO WS-CH-YY.
      *     IF WS-CH-MM = 16
      *        MOVE 4 TO WS-CH-MM 
      *        ADD 1 TO WS-CH-YY.

           MOVE WS-CH-DATE TO CONVERT-DATE.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO CRJRN-DUE-DATE.
       CDD-999.
           EXIT.
      *
       READ-CREDITOR SECTION.
       RCR-000.
           MOVE WS-CREDITOR TO CR-ACCOUNT-NUMBER.
           START CREDITOR-MASTER KEY NOT < CR-KEY.
       RCR-010.
           READ CREDITOR-MASTER
                INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE "** UNKNOWN **" TO CR-NAME
                MOVE 0 TO WS-CREDITOR-ST1
                GO TO RCR-999.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITOR BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RCR-010.
       RCR-999.
             EXIT.
      *
       READ-CURRENCY SECTION.
       RCU-000.
           OPEN I-O CURRENCY-MASTER.
           IF WS-CU-ST1 NOT = 0 
              MOVE 0 TO WS-CU-ST1
              MOVE "CURRENCY FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RCU-000.
           MOVE CR-CURRENCY TO CU-KEY.
           START CURRENCY-MASTER KEY NOT < CU-KEY.
       RCU-010.
           READ CURRENCY-MASTER
                 INVALID KEY NEXT SENTENCE.
           IF WS-CU-ST1 = 23 OR 35 OR 49
                MOVE 1 TO CU-VALUE
                MOVE 0 TO WS-CU-ST1
                GO TO RCU-900.
           IF WS-CU-ST1 NOT = 0
                MOVE 0 TO WS-CU-ST1
                MOVE "CURRENCY BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RCU-010.
       RCU-900.
           CLOSE CURRENCY-MASTER.
       RCU-999.
           EXIT.
      *
       READ-GLMASTER SECTION.
       RD-000.
           MOVE CRJRN-GLACC (SUB-1) TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       RD-010.
           READ GL-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE " " TO GL-NUMBER
               MOVE "INVALID" TO GL-DESCRIPTION
               GO TO RD-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLMASTER  BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RD-010.
       RD-999.
           EXIT.
      *
       READ-CRJRN-DNOTES SECTION.
       RSTT-000.
           IF WS-TYPE-OF-READ NOT = "D"
              CLOSE CRJRN-FILE
              PERFORM OPEN-015
              MOVE "D" TO WS-TYPE-OF-READ.
           MOVE "Y" TO WS-NEWINVOICE.
       RSTT-005.
           MOVE WS-DNOTE-NO TO CRJRN-DNOTE-NO.
           START CRJRN-FILE KEY NOT < CRJRN-DNOTE-NO
              INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-CRJRN-ST1
              GO TO RSTT-299.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CR-TRANS BUSY ON START, IN 1 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CRJRN-ST1
              CLOSE CRJRN-FILE
              PERFORM OPEN-015
              GO TO RSTT-005.
       RSTT-010.
           READ CRJRN-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 10
              GO TO RSTT-200.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CR-TRANS BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSTT-010.
           IF CRJRN-DNOTE-NO = WS-DNOTE-NO
            IF CRJRN-CRACC-NUMBER NOT = WS-CREDITOR
              GO TO RSTT-010.
           IF CRJRN-DNOTE-NO = WS-DNOTE-NO
            IF CRJRN-CRACC-NUMBER = WS-CREDITOR
             IF CRJRN-REFERENCE NOT = WS-JRN
              MOVE 88 TO WS-CRJRN-ST1
              MOVE "THIS INVOICE APPEARS IN BATCH            ALREADY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE 2945 TO POS
              DISPLAY CRJRN-REFERENCE AT POS
              MOVE "  PRESS <ESC> TO RE-ENTER AN INVOICE NUMBER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              PERFORM CLEAR-FIELDS
              MOVE " " TO WS-DNOTE-NO
              GO TO RSTT-999.
           IF CRJRN-DNOTE-NO NOT = WS-DNOTE-NO
              MOVE "Y" TO WS-NEWINVOICE
              GO TO RSTT-200.
           IF CRJRN-COMPLETE = "Y" OR = "P"
              MOVE 88 TO WS-CRJRN-ST1
              MOVE "THIS D/NOTE HAS BEEN PREVIOUSLY ENTERED & POSTED."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM CLEAR-FIELDS
              MOVE " " TO WS-DNOTE-NO
              GO TO RSTT-999.
           IF CRJRN-DNOTE-NO = WS-DNOTE-NO
            IF CRJRN-CRACC-NUMBER = WS-CREDITOR
              GO TO RSTT-050.
           GO TO RSTT-010.
       RSTT-050.
           IF WS-NEWINVOICE = "Y"
              MOVE CRJRN-INV-NO TO WS-INV-NO
              MOVE "N" TO WS-NEWINVOICE.
           GO TO RSTT-999.
       RSTT-200.
           UNLOCK CRJRN-FILE.
           IF WS-FUTURE-BATCH NOT = "Y" AND NOT = "B" AND NOT = "F"
              MOVE " " TO CRJRN-FUTURE
           ELSE
              MOVE "F" TO CRJRN-FUTURE.
       RSTT-299.
           PERFORM CF-000 THRU CF-010.
           MOVE "Y" TO WS-NEWINVOICE.
       RSTT-999.
           EXIT.
      *
       READ-CRJRN-INVOICES SECTION.
       RCRI-000.
           IF WS-TYPE-OF-READ NOT = "I"
              CLOSE CRJRN-FILE
              PERFORM OPEN-015
              MOVE "I" TO WS-TYPE-OF-READ.
           MOVE "Y" TO WS-NEWINVOICE.
       RCRI-005.
           MOVE WS-INV-NO TO CRJRN-INV-NO.
           START CRJRN-FILE KEY NOT < CRJRN-INV-NO
              INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-CRJRN-ST1
              GO TO RCRI-299.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 0 TO WS-CRJRN-ST1
              CLOSE CRJRN-FILE
              MOVE 
             "CR-JRN BUSY ON START READ-NEXT, IN 1 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR-020
              PERFORM ERROR1-020
              PERFORM OPEN-015
              GO TO RCRI-005.
       RCRI-010.
           READ CRJRN-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 10
              GO TO RCRI-200.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CR-JRN BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RCRI-010.
              
           IF CRJRN-INV-NO = WS-INV-NO
            IF CRJRN-CRACC-NUMBER NOT = WS-CREDITOR
              GO TO RCRI-010.
           IF CRJRN-INV-NO = WS-INV-NO
            IF CRJRN-CRACC-NUMBER = WS-CREDITOR
             IF CRJRN-REFERENCE NOT = WS-JRN
              MOVE 88 TO WS-CRJRN-ST1
              MOVE "THIS INVOICE APPEARS IN BATCH            ALREADY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE 2945 TO POS
              DISPLAY CRJRN-REFERENCE AT POS
              MOVE "  PRESS <CANCEL> TO RE-ENTER AN INVOICE NUMBER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              PERFORM CLEAR-FIELDS
              MOVE " " TO WS-DNOTE-NO
              GO TO RCRI-999.
              
           IF CRJRN-INV-NO NOT = WS-INV-NO
              MOVE "Y" TO WS-NEWINVOICE
              GO TO RCRI-200.
           IF CRJRN-COMPLETE = "Y" OR = "P"
              MOVE 88 TO WS-CRJRN-ST1
              MOVE "THIS INVOICE HAS BEEN PREVIOUSLY ENTERED & POSTED."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM CLEAR-FIELDS
              MOVE " " TO WS-DNOTE-NO
              GO TO RCRI-999.
       RCRI-050.
           IF WS-NEWINVOICE = "Y"
              MOVE CRJRN-DNOTE-NO TO WS-DNOTE-NO
              MOVE "N" TO WS-NEWINVOICE.
           GO TO RCRI-999.
       RCRI-200.
           UNLOCK CRJRN-FILE.
           IF WS-FUTURE-BATCH NOT = "Y" AND NOT = "B" AND NOT = "F"
              MOVE " " TO CRJRN-FUTURE
           ELSE
              MOVE "F" TO CRJRN-FUTURE.
       RCRI-299.
           PERFORM CF-000 THRU CF-010.
       RCRI-999.
           EXIT.
      *
       DELETE-INVOICE SECTION.
       DI-500.
           DELETE CRJRN-FILE
              INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CR-TRANS BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO DI-500.
       DI-999.
           EXIT.
      *
       READ-NEXT-CRJRN SECTION.
       RNCR-000.
           MOVE 2910 TO POS.
           DISPLAY "READING TRANSACTIONS FOR THIS BATCH......" AT POS.
           MOVE 0 TO WS-INV-TOTAL WS-CONTROL-TOTAL WS-BATCH-TOTAL
                     LINE-PRINTED.
           IF WS-FUTURE-BATCH NOT = "F" AND NOT = "P"
              MOVE " "        TO WS-FUTURE-BATCH.
           
           MOVE WS-JRN        TO CRJRN-REFERENCE.
           MOVE 0             TO CRJRN-TRANS.
           START CRJRN-FILE KEY NOT < CRJRN-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 NOT = 0
              GO TO RNCR-900.
       RNCR-010.
           READ CRJRN-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 10
              GO TO RNCR-900.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CR-JRN BUSY ON READ-NEXT RNCR-010, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CRJRN-ST1
              GO TO RNCR-010.
           IF CRJRN-REFERENCE NOT = WS-JRN
              GO TO RNCR-900.
           IF CRJRN-COMPLETE = "Y" OR = "P"
            IF WS-NEWBATCH = "Y"
              MOVE "P" TO WS-NEWBATCH
              GO TO RNCR-010
            ELSE
              GO TO RNCR-010.


           IF CRJRN-INV-DATE < GL-BEGDATE (WS-CURRENTPER)
               GO TO RNCR-010.
           IF CRJRN-INV-DATE > GL-ENDDATE (WS-CURRENTPER)
               GO TO RNCR-010.

           ADD 1 TO LINE-PRINTED
           MOVE 2610 TO POS
           DISPLAY "LINE:" AT POS
           ADD 5 TO POS
           DISPLAY LINE-PRINTED AT POS
           
           MOVE 2620 TO POS
           DISPLAY "ACCOUNT & INVOICE #:" AT POS
           MOVE CRJRN-CRACC-NUMBER TO WS-CRACC
           MOVE CRJRN-INV-NO       TO WS-CRINV
           ADD 20 TO POS
           DISPLAY LINE-DESCRIPTION AT POS.
               
       RNCR-050.
      *******************************************************************
      * IF WS-FUTURE-BATCH = "F" THIS WOULD BE BECAUSE THE OPERATOR HAS *
      * ENTERED THE BATCH AS A FUTURE BATCH. F06 E.G. WOULD BE DISPLAYED*
      * AT THE TOP OF THE SCREEN.                                       *
      * IF WS-FUTURE-BATCH = "P" THIS WOULD BE BECAUSE THE OPERATOR HAS *
      * ENTERED THE BATCH AS A PREVIOUS BATCH. P04 E.G. WOULD BE        *
      * DISPLAYED AT THE TOP OF THE SCREEN. MAYBE A BATCH FROM LAST     *
      * MONTH WAS NOT POSTED THEN                                       *
      *******************************************************************
      * NEW SECTION TO SEE IF INVOICES IN A BATCH HAVE MIXED PERIOD     *
      * INDICATORS.  IF WS-FUTURE-BATCH = "B" THEN A MIX HAS BEEN FOUND *
      * THEN WE WILL ALLOW ONLY PRINTING OF THE BATCH SO THAT THE MIX   *
      * CAN BE RECTIFIED.  07/11/2002.  WS-REST IS THE ACTUAL           *
      * CREDITOR PERIOD NUMBER FROM THE PARAMETER FILE.                 *
      *******************************************************************
      **********************************      
      *FUTURE - SETUP OF PERIOD NUMBER *
      **********************************      
           IF CRJRN-FUTURE = "F"
            IF CRJRN-NO > WS-CURRENTPER
              MOVE CRJRN-NO TO WS-CURRENTPER.
           IF CRJRN-FUTURE = "F"
            IF WS-REST = 12
            IF CRJRN-NO < WS-REST
              MOVE CRJRN-NO TO WS-CURRENTPER
            ELSE
              MOVE "B" TO WS-FUTURE-BATCH.
      ************************************      
      *PREVIOUS - SETUP OF PERIOD NUMBER *
      ************************************      
           IF CRJRN-FUTURE = "P" OR = " "
            IF CRJRN-NO < WS-CURRENTPER
                MOVE CRJRN-NO TO WS-CURRENTPER
             IF WS-FUTURE-BATCH = " "
                MOVE "P" TO WS-FUTURE-BATCH.
           IF CRJRN-FUTURE = "P" OR = " "
            IF WS-FUTURE-BATCH = "P"
             IF CRJRN-NO NOT < WS-REST
                MOVE "B" TO WS-FUTURE-BATCH.
      * FUTURE BATCHES
           IF CRJRN-FUTURE = "F"
            IF WS-FUTURE-BATCH = " " OR = "Y" OR = "F"
              MOVE "Y"      TO WS-FUTURE-BATCH
              GO TO RNCR-060
            ELSE
              MOVE "B"      TO WS-FUTURE-BATCH
              GO TO RNCR-060.
      * CURRENT BATCHES
           IF CRJRN-FUTURE = " "
            IF WS-FUTURE-BATCH = "Y" OR = "B" OR = "F"
               MOVE "B" TO WS-FUTURE-BATCH
              GO TO RNCR-060.
      * PREVIOUS PERIOD BATCHES        
           IF CRJRN-FUTURE = " "
            IF WS-FUTURE-BATCH = "P"
             IF CRJRN-NO < WS-REST
               MOVE "P" TO WS-FUTURE-BATCH
               GO TO RNCR-060
            ELSE
               MOVE "B" TO WS-FUTURE-BATCH
               GO TO RNCR-060.
                
           IF CRJRN-FUTURE = " "
            IF WS-FUTURE-BATCH = " "
               MOVE "N" TO WS-FUTURE-BATCH.
       RNCR-060.
           MOVE "N" TO WS-NEWBATCH.
           ADD 1    TO WS-INV-TOTAL.
           ADD CRJRN-LOC-AMT TO WS-CONTROL-TOTAL
                                WS-BATCH-TOTAL.
           ADD CRJRN-VAT-AMT TO WS-VAT-TOTAL.
           GO TO RNCR-010.
       RNCR-900.
           IF WS-FUTURE-BATCH = "Y" OR "P"
              PERFORM DISPLAY-TOP-INFO.
           IF WS-FUTURE-BATCH = "B"
              MOVE 
           "THERE ARE MIXED PERIODS IN THIS BATCH, YOU CAN PRINT ONLY."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE.
           IF WS-FUTURE-BATCH = "F" OR = "Y"
              MOVE 
           "THIS BATCH IS FOR A FUTURE PERIOD, YOU CAN PRINT ONLY."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE.
           IF WS-FUTURE-BATCH = "P"
              MOVE 
           "THIS BATCH IS FOR A PREVIOUS PERIOD, YOU CAN PRINT ONLY."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE.
           CLOSE CRJRN-FILE.
           PERFORM OPEN-015.
           PERFORM ERROR1-020
           MOVE 2610 TO POS.
           DISPLAY WS-MESSAGE AT POS.
       RNCR-999.
           EXIT.
      *
       REWRITE-CRJRN SECTION.
       RWCR-001.
           IF WS-NEWINVOICE = "N"
              GO TO RWCR-018.
       RWCR-005.
           PERFORM READ-PARAMETER-LOCK.
           MOVE GLPA-CRTRANSNO TO CRJRN-TRANS.
           ADD 1 TO GLPA-CRTRANSNO.
           PERFORM REWRITE-PARAMETER.

           START CRJRN-FILE KEY NOT < CRJRN-KEY.
           MOVE WS-JRN        TO CRJRN-REFERENCE.
           IF WS-FUTURE-BATCH = "Y"
              MOVE "F"        TO CRJRN-FUTURE.
           MOVE WS-CURRENTPER TO CRJRN-NO.
           MOVE "N"           TO CRJRN-COMPLETE.
           MOVE WS-INV-NO     TO CRJRN-INV-NO.
           MOVE WS-DNOTE-NO   TO CRJRN-DNOTE-NO.
           MOVE WS-CREDITOR   TO CRJRN-CRACC-NUMBER.
           IF CRJRN-LOC-AMT < 0
               MOVE 6         TO CRJRN-TYPE
           ELSE
               MOVE 1         TO CRJRN-TYPE.
           GO TO RWCR-019.
       RWCR-018.
           REWRITE CRJRN-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CRJRN BUSY ON REWRITE, RWCR-018, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-CRJRN-ST1
              GO TO RWCR-999.
           GO TO RWCR-999.
       RWCR-019.
           WRITE CRJRN-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-CRJRN-ST1
              GO TO RWCR-999.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CRJRN WRITE ERR, RWCR-019. TRANS NOT WRITTEN"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
           GO TO RWCR-999.
       RWCR-999.
           EXIT.
      *
       WRITE-CRTRANS SECTION.
       WRPTR-000.
           PERFORM READ-GLPARAMETER-LOCK.
           MOVE GLPA-CRTRANSNO    TO CRTR-TRANS.
           ADD 1                  TO GLPA-CRTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           IF CRJRN-LOC-AMT > 0
               MOVE 1             TO CRTR-TYPE
           ELSE
               MOVE 6             TO CRTR-TYPE.
           MOVE " "               TO CRTR-FUTURE.
           MOVE WS-CURRENTPER     TO CRTR-NO.
           MOVE CR-ACCOUNT-NUMBER TO CRTR-ACC-NUMBER.
           MOVE CRJRN-REFERENCE   TO CRTR-REFERENCE.
           MOVE CRJRN-INV-NO      TO CRTR-INV-NO.
           MOVE CRJRN-DNOTE-NO    TO CRTR-DNOTE-NO.
           MOVE CRJRN-INV-DATE    TO CRTR-DATE.
           MOVE CRJRN-DUE-DATE    TO CRTR-DUE-DATE.
           MOVE CRJRN-LOC-AMT     TO CRTR-LOC-AMT
                                     CRTR-UNAPPLIED-AMT.
           MOVE CRJRN-VAT-AMT     TO CRTR-VAT-AMT.
           MOVE CRJRN-FOR-AMT     TO CRTR-FOR-AMT.
           MOVE CRJRN-EXCHANGE    TO CRTR-EXCHANGE.
           MOVE CRJRN-SETT-DISC   TO CRTR-SETT-DISC.
       WRPTR-010.
           WRITE CRTR-REC
                 INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
                 GO TO WRPTR-000.
            IF WS-CRTRANS-ST1 = 91
                MOVE 0 TO WS-CRTRANS-ST1
                MOVE "CR-TRANS BUSY ON WRITE, ERC91, 'ESC' TO RE-TRY." 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO WRPTR-010.
            IF WS-CRTRANS-ST1 = 51
                MOVE 0 TO WS-CRTRANS-ST1
                MOVE "CR-TRANS BUSY ON WRITE, ERC51, 'ESC' TO RE-TRY." 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO WRPTR-010.
       WRPTR-999.
              EXIT.
      *
       POST-TRANSACTIONS SECTION.
       PT-001.
           PERFORM WRTR-000.
       PT-002.
           MOVE 2910 TO POS.
       PT-005.
           MOVE 0 TO WS-SETT-DISC
                     CRJRN-TRANS.
           MOVE WS-JRN TO CRJRN-REFERENCE.
           START CRJRN-FILE KEY NOT < CRJRN-KEY
              INVALID KEY NEXT SENTENCE.
       PT-010.
           READ CRJRN-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 10
              GO TO PT-900.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CR-JRN BUSY ON READ PT-010, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO PT-010.
           IF CRJRN-REFERENCE NOT = WS-JRN
              GO TO PT-900.

            IF CRJRN-INV-DATE NOT < WS-BEG-DATE
             IF CRJRN-INV-DATE NOT > WS-END-DATE
                GO TO PT-015.
            IF CRJRN-INV-DATE > WS-END-DATE
            MOVE "AN INV DATE IS > THE END PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE CRJRN-INV-NO TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO PT-010.
            IF CRJRN-INV-DATE < WS-BEG-DATE
            MOVE "AN INV DATE IS < THE BEGINNING PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE CRJRN-INV-NO TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO PT-010.
       PT-015.
           MOVE 2610 TO POS.
           DISPLAY "JRN BEING PROCESSED:" AT POS.
           ADD 21 TO POS.
           DISPLAY CRJRN-INV-NO AT POS.
           ADD CRJRN-SETT-DISC TO WS-SETT-DISC.
           MOVE "N" TO WS-NEWINVOICE.
           MOVE "P" TO CRJRN-COMPLETE.
           MOVE 1 TO SUB-1.
       PT-020.
           IF CRJRN-GLACC (SUB-1) = " "
                 GO TO PT-030.
           PERFORM WRTR-010 THRU WRTR-015.
           PERFORM UPDATE-GLMASTER.
           PERFORM UPDATE-GLHEADER.
           PERFORM UPDATE-GLSUBHEADER.
           ADD 1 TO SUB-1.
           IF SUB-1 < 11
               GO TO PT-020.
           MOVE 1 TO SUB-1.
       PT-030.
           PERFORM UPDATE-CREDITOR.
           PERFORM WRITE-CRTRANS.
           PERFORM REWRITE-CRJRN.
           GO TO PT-010.
       PT-900.
           PERFORM UPDATE-GL-CREDITOR-ACC.
           IF WS-CURRENTPER NOT = WS-CURRENTGLPER
               PERFORM WRTR-900
               GO TO PT-999.
           MOVE GL-NUMBER   TO WS-GLNUMBER.
           MOVE WS-GLHEADER TO GL-NUMBER.
           PERFORM UGLCA-010 THRU UGLCA-900.
           MOVE WS-HEAD-SUB TO GL-NUMBER.
           PERFORM UGLCA-010 THRU UGLCA-900.
           PERFORM WRTR-900.
       PT-999.
           EXIT.
      *
       UPDATE-CREDITOR SECTION.
       UDC-000.
           MOVE CRJRN-CRACC-NUMBER TO CR-ACCOUNT-NUMBER.
           START CREDITOR-MASTER KEY NOT < CR-KEY.
       UDC-010.
           READ CREDITOR-MASTER WITH LOCK
                INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE CR-ACCOUNT-NUMBER TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE "CREDITORS NOT ON FILE CALL YOUR SUPERVISOR."
                 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                MOVE 0 TO WS-CREDITOR-ST1
                GO TO UDC-999.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITORS BUSY ON READ-LOCK, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO UDC-010.
       UDC-020.
           MOVE CRJRN-INV-DATE TO CR-DATE-LAST-INVOICE.
           ADD  CRJRN-LOC-AMT  TO CR-PURCHASE-PTD
                                  CR-PURCHASE-YTD
                                  CR-BALANCE
                                  CR-CURRENT.
           IF CR-FOREIGN-LOCAL = "F"
            ADD CRJRN-FOR-AMT  TO CR-FOR-PURCH-YTD.
       UDC-030.
           REWRITE CREDITOR-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
               GO TO UDC-999.
           IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITORS BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UDC-030.
       UDC-999.
           EXIT.
      *
       DELETE-JRN SECTION.
       DJ-005.
           MOVE WS-JRN TO CRJRN-REFERENCE.
           MOVE 0 TO CRJRN-TRANS.
           START CRJRN-FILE KEY NOT < CRJRN-KEY
              INVALID KEY NEXT SENTENCE.
       DJ-010.
           READ CRJRN-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 10
              GO TO DJ-999.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CR-JRN BUSY ON READ-DELETE-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO DJ-010.
           IF CRJRN-REFERENCE NOT = WS-JRN
              GO TO DJ-999.
       DJ-500.
           DELETE CRJRN-FILE
              INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CRJRN BUSY ON DELETE, DJ-500, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO DJ-500.
           GO TO DJ-010.
       DJ-999.
           EXIT.
      *
       WRITE-GLTRANS SECTION.
       WRTR-000.
           OPEN I-O GLTRANS-FILE.
           IF WS-GLTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GL-TRANS BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRTR-000.
       WRTR-010.
           PERFORM READ-PARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO          TO GLTRANS-TRANS.
           ADD 1 TO GLPA-GLTRANSNO.
           PERFORM REWRITE-PARAMETER.
           MOVE WS-JRN                  TO GLTRANS-REFERENCE.
           IF WS-CURRENTGLPER = WS-CURRENTPER
               MOVE CRJRN-PERIOD        TO GLTRANS-PERIOD
           ELSE
               MOVE "F"                 TO GLTRANS-FUTURE
               MOVE CRJRN-NO            TO GLTRANS-NO.
           MOVE CRJRN-INV-DATE          TO GLTRANS-DATE.
           MOVE CRJRN-GLACC (SUB-1)     TO GLTRANS-ACCOUNT-NUMBER.
           MOVE CRJRN-GLAMT (SUB-1)     TO GLTRANS-AMOUNT.

           MOVE "SUP"              TO WS-CR1.
           MOVE CRJRN-CRACC-NUMBER TO WS-CRACC.
           IF CRJRN-LOC-AMT > 0
              MOVE 1               TO GLTRANS-TYPE
              MOVE "IN"            TO WS-CR2
           ELSE
              MOVE 6               TO GLTRANS-TYPE
              MOVE "CR"            TO WS-CR2.
           MOVE CRJRN-INV-NO       TO WS-CRINV.
           MOVE LINE-DESCRIPTION   TO GLTRANS-LINE-DESC.
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
              MOVE 0 TO WS-GLTRANS-ST1
              MOVE "GLTRANS FILE BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRTR-015.
       WRTR-900.
           CLOSE GLTRANS-FILE.
       WRTR-999.
           EXIT.
      *
       UPDATE-GLMASTER SECTION.
       UPGL-000.
           IF WS-CURRENTPER NOT = WS-CURRENTGLPER
               GO TO UPGL-999.
           MOVE WS-CURRENTGLPER TO SUB-3.
       UPGL-005.
           IF CRJRN-GLACC (SUB-1) NOT > " "
               GO TO UPGL-999.
           MOVE CRJRN-GLACC (SUB-1) TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGL-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLMASTER FILE DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGL-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLMASTER RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGL-010.
           ADD CRJRN-GLAMT (SUB-1) TO GL-BALANCE.
           ADD CRJRN-GLAMT (SUB-1) TO GL-PER (SUB-3).
       UPGL-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGL-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLMASTER RECORD BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGL-900.
       UPGL-999.
           EXIT.
      *
       UPDATE-GLHEADER SECTION.
       UPGLH-005.
           IF WS-CURRENTPER NOT = WS-CURRENTGLPER
               GO TO UPGLH-999.
           MOVE WS-CURRENTGLPER TO SUB-3.
           IF CRJRN-GLACC (SUB-1) NOT > " "
               GO TO UPGLH-999.
           MOVE CRJRN-GLACC (SUB-1) TO WS-GLNUMBER.
           MOVE WS-GLHEADER TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLHEADER FILE DOES NOT EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLHEADER FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLH-010.
           ADD CRJRN-GLAMT (SUB-1) TO GL-BALANCE.
           ADD CRJRN-GLAMT (SUB-1) TO GL-PER (SUB-3).
       UPGLH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLH-999.
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
           IF WS-CURRENTPER NOT = WS-CURRENTGLPER
               GO TO UPGLSH-999.
           MOVE WS-CURRENTGLPER TO SUB-3.
           IF CRJRN-GLACC (SUB-1) NOT > " "
               GO TO UPGLSH-999.
           MOVE CRJRN-GLACC (SUB-1) TO WS-GLNUMBER.
           MOVE WS-HEAD-SUB TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLSH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLSUBHEAD FILE DOESN'T EXIST, CALL YOUR SUPERVISOR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLSH-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLSUBHEADER FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGLSH-010.
           ADD CRJRN-GLAMT (SUB-1) TO GL-BALANCE.
           ADD CRJRN-GLAMT (SUB-1) TO GL-PER (SUB-3).
       UPGLSH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLSH-999.
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
           IF WS-CURRENTPER NOT = WS-CURRENTGLPER
               GO TO UGLCA-955.
           MOVE WS-CURRENTGLPER TO SUB-3.
       UGLCA-005.
           PERFORM READ-PARAMETER.
           MOVE GLPA-GLCRED-NO TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UGLCA-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLCRED-ACC DOES NOT EXIST, CALL YOUR SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UGLCA-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLCRED-ACC BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UGLCA-010.
           SUBTRACT WS-BATCH-TOTAL FROM GL-BALANCE.
           SUBTRACT WS-BATCH-TOTAL FROM GL-PER (SUB-3).
       UGLCA-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UGLCA-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLCRED-ACC BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UGLCA-900.
       UGLCA-950.
      *     PERFORM WRTR-000.
       UGLCA-955.
           PERFORM READ-PARAMETER-LOCK.
           MOVE GLPA-GLTRANSNO     TO GLTRANS-TRANS.
           ADD 1                   TO GLPA-GLTRANSNO.
           PERFORM REWRITE-PARAMETER.
           MOVE WS-JRN             TO GLTRANS-REFERENCE.
           MOVE 1                  TO GLTRANS-TYPE.
           IF WS-CURRENTGLPER = WS-CURRENTPER
               MOVE " "            TO GLTRANS-FUTURE
           ELSE
               MOVE "F"            TO GLTRANS-FUTURE.
           MOVE WS-CURRENTPER      TO GLTRANS-NO.
           MOVE BATCH-DATE         TO GLTRANS-DATE.
           MOVE GLPA-GLCRED-NO     TO GLTRANS-ACCOUNT-NUMBER.
           MOVE WS-BATCH-TOTAL     TO GLTRANS-AMOUNT.
           COMPUTE GLTRANS-AMOUNT = GLTRANS-AMOUNT * -1.
           MOVE "JRN"              TO WS-CR1.
           MOVE WS-JRN             TO WS-CRACC.
           MOVE "  "               TO WS-CR2. 
           MOVE "BATCH POSTING"    TO WS-CRINV.
           MOVE LINE-DESCRIPTION   TO GLTRANS-LINE-DESC.

           PERFORM WRTR-015 THRU WRTR-900.
       UGLCA-999.
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
               MOVE "GLPARAMETER BUSY ON READ-LOCK, 'ESC' TO RETRY."
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
               MOVE "GLPARAMETER BUSY REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RP-000.
       REWP-999.
           EXIT.
      *
       READ-SALES-LEDGER-PARAMETER SECTION.
       RSLP-000.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "SLPARAMETER BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSLP-000.
       RSLP-005.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           START PARAMETER-FILE KEY NOT < PA-KEY
              INVALID KEY NEXT SENTENCE.
       RSLP-010.
           READ PARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "SLPARAMETER BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSLP-010.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
            MOVE PA-GST-PERCENT TO WS-VAT-PERC.
            CLOSE PARAMETER-FILE.
       RSLP-020.
            MOVE 1015 TO POS
            DISPLAY "THE CURRENT VAT PERCENTAGE IS " AT POS
            ADD 30 TO POS
            MOVE WS-VAT-PERC TO F-EDNAMEFIELDAMOUNTDIS
            DISPLAY F-EDNAMEFIELDAMOUNTDIS AT POS
            MOVE 1210 TO POS
            DISPLAY "ENTER YES TO RE-ENTER, NO IS DEFAULT." AT POS
            ADD 40 TO POS

            MOVE 'N'       TO CDA-DATA.
            MOVE 1         TO CDA-DATALEN.
            MOVE 9         TO CDA-ROW.
            MOVE 50        TO CDA-COL.
            MOVE CDA-WHITE TO CDA-COLOR.
            MOVE 'F'       TO CDA-ATTR.
            PERFORM CTOS-ACCEPT.
            MOVE CDA-DATA TO WS-ACCEPT.

            IF WS-ACCEPT = "N"
                GO TO RSLP-999.
       RSLP-030.
            MOVE 1045 TO POS

            MOVE ' '       TO CDA-DATA.
            MOVE 10        TO CDA-DATALEN.
            MOVE 7         TO CDA-ROW.
            MOVE 44        TO CDA-COL.
            MOVE CDA-WHITE TO CDA-COLOR.
            MOVE 'F'       TO CDA-ATTR.
            PERFORM CTOS-ACCEPT.
            MOVE CDA-DATA TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-VAT-PERC
            GO TO RSLP-020.
       RSLP-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO GLPARAMETER RECORD ON READ, 'ESC' TO RETRY."
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       READ-PARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO GLPARAMETER RECORD ON READ-LOCK"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RPL-000.
       RPL-999.
           EXIT.
      *
       REWRITE-PARAMETER SECTION.
       REWP-000.
           REWRITE GLPARAMETER-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "GLPARAMETER RECORD NOT UPDATED ON REWRIE"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO REWP-000.
       REWP-999.
           EXIT.
      *
       SCROLL-NEXT SECTION.
       NEXT-000.
            MOVE 1 TO SUB-1 F-INDEX. 
            PERFORM CLEAR-BODY.
            MOVE 1 TO F-INDEX. 
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 10
                GO TO NEXT-030.
            IF F-INDEX < 11
                GO TO NEXT-010.
       NEXT-030.
            MOVE 1 TO SUB-1 F-INDEX. 
       NEXT-999.
             EXIT.
      *
       SCROLLING SECTION.
       SCROLL-010.
            MOVE "GLNUMBER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CRJRN-GLACC (SUB-1) TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-015.
            MOVE "GLAMOUNT" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            IF CRJRN-GLACC (SUB-1) = " "
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-020.
            MOVE CRJRN-GLAMT (SUB-1) TO F-EDNAMEFIELDNUM6.
            PERFORM WRITE-FIELD-NUMERIC6.
       SCROLL-016.
            MOVE "GLSETDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            IF CRJRN-GLACC (SUB-1) = " "
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-020.
            MOVE CRJRN-GLDISC (SUB-1) TO F-EDNAMEFIELDNUM6.
            PERFORM WRITE-FIELD-NUMERIC6.
       SCROLL-020.
            MOVE "GLNAME" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE CRJRN-GLDESC (SUB-1) TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-999.
             EXIT.
      *
       CANCEL-JOURNAL SECTION.
       CI-000.
             MOVE 1 TO SUB-1.
       CI-010.
             IF CRJRN-GLACC (SUB-1) = " "
                IF CRJRN-GLAMT (SUB-1) = 0
                 GO TO CI-900.
             PERFORM CANCEL-TRANSACTION.
             MOVE 1 TO SUB-1.
             GO TO CI-010.
       CI-900.
             PERFORM CLEAR-FIELDS.
             PERFORM DISPLAY-FORM.
             MOVE " " TO WS-MESSAGE.
             PERFORM ERROR-020.
       CI-999.
             EXIT.
      *
       CANCEL-TRANSACTION SECTION.
       CAN-005.
           COMPUTE SUB-2 = SUB-1 + 1.
       CAN-010.
           IF SUB-2 > 10
               GO TO CAN-090.
           IF CRJRN-GLACC (SUB-2) = " "
                 MOVE " " TO CRJRN-GLACC (SUB-1)
                             CRJRN-GLDESC (SUB-1)
                 MOVE 0   TO CRJRN-GLAMT (SUB-1)
                             CRJRN-GLDISC (SUB-1)
                 GO TO CAN-090.
             MOVE CRJRN-GL-DATA (SUB-2) TO CRJRN-GL-DATA (SUB-1).
             ADD 1 TO SUB-1 SUB-2.
             GO TO CAN-010.
       CAN-090.
             MOVE " " TO CRJRN-GLACC (SUB-1)
                         CRJRN-GLDESC (SUB-1).
             MOVE 0   TO CRJRN-GLAMT (SUB-1)
                         CRJRN-GLDISC (SUB-1).
       CAN-999.
             EXIT.
      *
       CLEAR-BODY SECTION.
       CLEAR-000.
            MOVE 0 TO F-INDEX.
       CLEAR-002.
            ADD 1 TO F-INDEX.
            IF F-INDEX > 10
               GO TO CLEAR-BODY-999.

            MOVE "GLNUMBER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLAMOUNT" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            MOVE " " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLSETDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            MOVE " " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLNAME" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            GO TO CLEAR-002.
       CLEAR-BODY-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
             MOVE 1 TO SUB-1.
       CF-010.
             MOVE " " TO CRJRN-GLACC (SUB-1)
                         CRJRN-GLDESC (SUB-1).
             MOVE 0   TO CRJRN-GLAMT (SUB-1)
                         CRJRN-GLDISC (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 11
                 GO TO CF-010.
       CF-020.
             MOVE " " TO CRJRN-REFERENCE
                         CRJRN-PERIOD
                         CRJRN-INV-NO
                         CRJRN-DNOTE-NO
                         CRJRN-COMPLETE.
             MOVE 0 TO   CRJRN-TRANS
                         CRJRN-NO
                         CRJRN-INV-DATE
                         CRJRN-DUE-DATE
                         CRJRN-CRACC-NUMBER
                         CRJRN-LOC-AMT
                         CRJRN-FOR-AMT
                         CRJRN-SETT-DISC.
       CF-999.
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
       OPEN-011.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0 
              MOVE 0 TO WS-GLMAST-ST1
              MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-011.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-GLPARAMETER-ST1
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-012.
       OPEN-013.
           MOVE " "                TO WS-FUTURE-BATCH
           PERFORM READ-PARAMETER
           MOVE GLPA-NAME          TO CO-NAME
           MOVE GLPA-CURRENT-GLPER TO WS-CURRENTGLPER
           MOVE GLPA-CURRENT-CRPER TO WS-CURRENTPER
           MOVE WS-CURRENTPER TO WS-REST.
           PERFORM ENTER-PERIOD-DATES.
           PERFORM OPEN-016.
       OPEN-014.
           OPEN I-O CRTR-FILE.
           IF WS-CRTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-CRTRANS-ST1
              MOVE "CRTRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
       OPEN-015.
           OPEN I-O CRJRN-FILE.
           IF WS-CRJRN-ST1 NOT = 0 
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CRJRN FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-015.
       OPEN-0151.
            GO TO OPEN-020.
       OPEN-016.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
           
           IF BATCH-DATE NOT > 0
            IF WS-DATE > GL-ENDDATE (WS-CURRENTPER)
               MOVE GL-ENDDATE (WS-CURRENTPER) TO BATCH-DATE
            ELSE
               MOVE WS-DATE TO BATCH-DATE.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrInCrMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE GL-MASTER
                 CREDITOR-MASTER
                 CRJRN-FILE
                 CRTR-FILE
                 GLPARAMETER-FILE.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       Copy "CreditorPassword".
       Copy "ReadKBD".
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldLine".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldNumeric6".
       Copy "WriteFieldValue".
       Copy "ComputeDatePeriod".
       Copy "EnterPeriodDates".
       Copy "NumberCheck".
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
      *
      * END-OF-JOB
