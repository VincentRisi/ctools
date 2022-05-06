        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrInCrRp.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectCrMaster".
          Copy "SelectCrJrn".
          Copy "SelectGlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCrJrn.
           COPY ChlfdGlParam.
           COPY ChlfdCreditor.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(135).
      *
       WORKING-STORAGE SECTION.
       77  WS-PERIOD-ACCEPT     PIC XX VALUE " ".
       77  WS-JRN-ACCEPT        PIC X(10) VALUE " ".
       77  WS-DET-SUMM          PIC X VALUE " ".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-CONFIRMED         PIC X VALUE " ".
       77  WS-FUTURE-ERR        PIC X VALUE " ".
       77  WS-FUTURE-BATCH      PIC X VALUE " ".
       77  WS-CURRENTPER        PIC 99 VALUE 0.
       77  WS-DELIVERVIA        PIC X(20) VALUE " ".
       77  WS-SUPPLIER          PIC X(7) VALUE " ".
       77  WS-REFERENCE         PIC X(20) VALUE " ".
       77  WS-QUANTITY          PIC 9(5) VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-LINE              PIC 9(4) VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-UNAPPLIED-TOTAL   PIC S9(7)V99.
       77  WS-WORK-TOTAL        PIC S9(7)V99.
       77  WS-GOODS-AMT         PIC S9(7)V99.
       77  WS-VAT-TOTAL         PIC S9(7)V99.
       77  WS-WRITE             PIC X VALUE " ".
       77  WS-ACCEPT-DATE       PIC X(10) VALUE " ".
       77  WS-VALIDDATE         PIC 9(8) VALUE 0.
       01  WS-PERIOD.
           03  WS-FUTURE        PIC X VALUE " ".
           03  WS-NO            PIC 99 VALUE 0.
       01  WS-CRJRN-STATUS.
           03  WS-CRJRN-ST1       PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1    PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(5) VALUE " ".
           03  FILLER         PIC X(49) VALUE
           "** CRJRN LISTING BY JOURNAL NAME **".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
       01  HEAD2.
           03  FILLER         PIC X(22) VALUE " ".
           03  FILLER         PIC X(35) VALUE ALL "*".
           03  FILLER         PIC X(93) VALUE " ".
       01  HEAD3-SUM.
           03  FILLER         PIC X(46) VALUE
            "CRJRN NAME        DATE    POST  PD     ENTRIES".
           03  H3-DET         PIC X(34) VALUE " ".
       01  HEAD3-DET.
           03  FILLER         PIC X(5) VALUE "LINE".
           03  FILLER         PIC X(61) VALUE
            "CRJRN NAME   DATE    POST PD ACCOUNT NUMBER & NAME".
           03  FILLER         PIC X(25) VALUE "INVOICE       AMOUNT".
       01  DETAIL-LINE.
           03  D-JRN          PIC X(15) VALUE " ".
           03  D-DATE         PIC X(10) VALUE " ".
           03  FILLER         PIC X(3) VALUE " ".
           03  D-POST         PIC X(2) VALUE " ".
           03  D-FILLER       PIC X(1) VALUE " ".
           03  D-PERIOD       PIC X(3) VALUE " ".
           03  D-FUTURE       PIC X(4) VALUE " ".
           03  D-QTY          PIC Z(4)9.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-GOODS-TOTAL  PIC Z(6)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-VAT-TOTAL    PIC Z(6)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-JRN-TOTAL    PIC Z(6)9.99.
       01  INVOICE-LINE.
           03  I-LINE         PIC Z(3)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  I-JRN          PIC X(11) VALUE " ".
           03  I-DATE         PIC X(10) VALUE " ".
           03  FILLER         PIC X(1) VALUE " ".
           03  I-POST         PIC X(2) VALUE " ".
           03  I-FILLER       PIC X(1) VALUE " ".
           03  I-PERIOD       PIC X(4) VALUE " ".
           03  I-ACC          PIC X(8) VALUE " ".
           03  I-NAME         PIC X(24) VALUE " ".
           03  FILLER         PIC X(1) VALUE " ".
           03  I-INV-NO       PIC X(10) VALUE " ".
           03  I-INV-AMT      PIC Z(6)9.99-.
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
           DISPLAY "** CRJRN LISTING BY JRN NUMBER **" AT POS
           MOVE 0410 TO POS
           DISPLAY "*********************************" AT POS.
       CONTROL-003.
           Copy "PrinterAcceptCr".
       CONTROL-010.
           PERFORM OPEN-012 THRU OPEN-013.
           PERFORM GET-DATE.
           PERFORM OPEN-FILES.
           PERFORM ERROR-020.
           MOVE 2510 TO POS
           DISPLAY "CRJRN's Being Read, Report In Progress...." AT POS.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       GET-DATE SECTION.
       GET-005.
           MOVE 1010 TO POS
           DISPLAY "ENTER A PERIOD TO PRINT, LEAVE BLANK FOR ALL : [  ]"
           AT POS
           ADD 48 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 57        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PERIOD-ACCEPT.

           IF WS-PERIOD-ACCEPT = " "
               GO TO GET-010.
           MOVE WS-PERIOD-ACCEPT TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-NO
           MOVE 1058 TO POS
           DISPLAY WS-NO AT POS.
           
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-005.
       GET-010.
           MOVE 1210 TO POS
           DISPLAY 
           "ENTER A JRN TO PRINT, LEAVE BLANK FOR ALL    : [          ]"
           AT POS
           ADD 48 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 9        TO CDA-ROW.
           MOVE 57        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-JRN-ACCEPT.

           IF W-ESCAPE-KEY = 4
               GO TO GET-005.
           IF WS-JRN-ACCEPT = " "
               GO TO GET-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-015.
           MOVE "S" TO WS-DET-SUMM.
           MOVE 1410 TO POS
           DISPLAY "D=DETAIL, S=SUMMARY OF JRN'S                 : [ ]"
           AT POS
           ADD 48 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 57        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DET-SUMM.

           IF WS-DET-SUMM NOT = "D" AND NOT = "S"
               GO TO GET-015.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-999
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
       GET-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-001.
           MOVE 0               TO PAGE-CNT WS-QUANTITY
           MOVE " "             TO WS-REFERENCE
           MOVE 66              TO LINE-CNT.
           
           IF WS-JRN-ACCEPT > " "
              MOVE WS-JRN-ACCEPT TO CRJRN-REFERENCE
              MOVE 0             TO CRJRN-TRANS
                                    CRJRN-TYPE
           ELSE
              MOVE " "           TO CRJRN-REFERENCE
              MOVE 0             TO CRJRN-TRANS
                                    CRJRN-TYPE.
           
           START CRJRN-FILE KEY NOT < CRJRN-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
              MOVE "BAD START ON CRJRN, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO PRR-900.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "BAD START ON CRJRN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO PRR-001.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
       PRR-002.
           READ CRJRN-FILE NEXT
               AT END NEXT SENTENCE.
               
           IF WS-CRJRN-ST1 = 10 OR = 91
            IF WS-DET-SUMM = "S"
      *      IF WS-QUANTITY NOT = 0
               MOVE WS-QUANTITY TO D-QTY
               PERFORM PRR-025
               PERFORM PRR-900
               PERFORM PRR-950
               PERFORM PRR-955
               GO TO PRR-970
            ELSE
               PERFORM PRR-900
               PERFORM PRR-950
               PERFORM PRR-955
               GO TO PRR-970.
               
           IF WS-CRJRN-ST1 NOT = 0
             MOVE "CRJRN FILE BUSY ON READ, 'ESC' TO SEE STATUS"
              TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-CRJRN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PRR-002.
      ****************************************************************
      *CHECKING FOR THE PERIOD                                       *
      * IF WS-NO = 0 THIS MEANS NO PERIOD WAS SELECTED FOR THE PRINT *
      ****************************************************************
           IF WS-NO NOT = 0
            IF CRJRN-NO NOT = WS-NO
              GO TO PRR-002.
     
      ***************************************************************
      * NEXT 5 LINES REFER TO JRN'S FROM OTHER PERIODS OTHER THAN   *
      * THE CURRENT PERIOD.  SUMMARY ONLY.                          *
      ***************************************************************
           IF WS-REFERENCE NOT = " "
            IF CRJRN-REFERENCE NOT = WS-REFERENCE
             IF WS-DET-SUMM = "S"
                PERFORM PRR-006
                GO TO PRR-002
            ELSE
                PERFORM PRR-007
                GO TO PRR-002.
              
      ***************************************************************
      * FROM HERE TO PRR-005 IS ONLY FOR CURRENT PERIOD CRJRN'S     *
      * TO WORK OUT IF THE DATE PARAMETERS ARE CORRECT.             *
      ***************************************************************
           IF CRJRN-NO = WS-CURRENTPER
            IF CRJRN-INV-DATE NOT < WS-BEG-DATE
             IF CRJRN-INV-DATE NOT > WS-END-DATE
                GO TO PRR-005.
           IF CRJRN-NO = WS-CURRENTPER
            IF CRJRN-INV-DATE > WS-END-DATE
            MOVE "*" TO D-FILLER I-FILLER
                GO TO PRR-005.
           IF CRJRN-NO = WS-CURRENTPER
            IF CRJRN-INV-DATE < WS-BEG-DATE
            MOVE "*" TO D-FILLER I-FILLER
                GO TO PRR-005.
       PRR-005.
           MOVE 2310 TO POS
           DISPLAY "CrJrn Being Read:" AT POS
           ADD 18 TO POS
           DISPLAY CRJRN-REFERENCE AT POS. 
           
      *************************************************************
      *1ST READ OF FILE IF NO CRJRN NUMBER ENTERED                *
      *************************************************************
           IF WS-REFERENCE = " "
               MOVE CRJRN-REFERENCE TO WS-REFERENCE
               ADD 1                TO WS-QUANTITY
            IF WS-DET-SUMM = "S"
               ADD CRJRN-LOC-AMT       TO WS-WORK-TOTAL
               ADD CRJRN-VAT-AMT       TO WS-VAT-TOTAL
               ADD CRJRN-UNAPPLIED-AMT TO WS-UNAPPLIED-TOTAL
               PERFORM PRR-020
               GO TO PRR-002
            ELSE
               PERFORM PRR-960
               GO TO PRR-002.
      ***********************************************************
      * VALID IF A JRN NUMBER WAS ENTERED.                      *
      * SUMMARY ONLY.                                           *
      ***********************************************************
           IF WS-JRN-ACCEPT NOT = " "
            IF CRJRN-REFERENCE NOT = WS-JRN-ACCEPT
             IF WS-DET-SUMM = "S"
              IF WS-QUANTITY > 0
               MOVE WS-QUANTITY TO D-QTY
               PERFORM PRR-025
               PERFORM PRR-027
               ADD 1 TO WS-QUANTITY
               MOVE CRJRN-REFERENCE TO WS-REFERENCE
               PERFORM PRR-020
               GO TO PRR-970
              ELSE
               MOVE CRJRN-REFERENCE TO WS-REFERENCE
               GO TO PRR-970.
               
      ***********************************************************
      * VALID IF A JRN NUMBER WAS ENTERED.                      *
      * DETAIL ONLY.                                            *
      ***********************************************************
           IF WS-JRN-ACCEPT NOT = " "
            IF CRJRN-REFERENCE NOT = WS-JRN-ACCEPT
             IF WS-DET-SUMM = "D"
               MOVE CRJRN-REFERENCE TO WS-REFERENCE
               GO TO PRR-970.
               
           IF CRJRN-REFERENCE = WS-REFERENCE
            IF WS-DET-SUMM = "S"
               ADD CRJRN-LOC-AMT       TO WS-WORK-TOTAL
               ADD CRJRN-VAT-AMT       TO WS-VAT-TOTAL
               ADD CRJRN-UNAPPLIED-AMT TO WS-UNAPPLIED-TOTAL
               ADD 1                   TO WS-QUANTITY
               PERFORM PRR-950
               PERFORM PRR-955
               GO TO PRR-002
            ELSE
               PERFORM PRR-960
               GO TO PRR-002.
               
            GO TO PRR-010.
               
       PRR-006.
      *************************************************************
      * VALID IF NO CRJRN NUMBER ENTERED FOR THE PRINT.           *
      * SUMMARY PRINT                                             *
      *************************************************************
           IF CRJRN-REFERENCE NOT = WS-REFERENCE
            IF WS-DET-SUMM = "S"
             IF WS-QUANTITY > 0
               MOVE WS-QUANTITY     TO D-QTY
               PERFORM PRR-955
               PERFORM PRR-025
               PERFORM PRR-027
               ADD CRJRN-LOC-AMT       TO WS-WORK-TOTAL
               ADD CRJRN-VAT-AMT       TO WS-VAT-TOTAL
               ADD CRJRN-UNAPPLIED-AMT TO WS-UNAPPLIED-TOTAL
               MOVE CRJRN-REFERENCE    TO WS-REFERENCE
               PERFORM PRR-020
               ADD 1 TO WS-QUANTITY
             ELSE
               MOVE CRJRN-REFERENCE TO WS-REFERENCE.
       PRR-007.
      *****************************************************************
      * NO ENTRIES FILLED IN AT ALL.  CRJRN READ IS DIFFERENT TO      *
      * THE LAST ONE.  DETAIL PRINT.                                  *
      *****************************************************************
           IF CRJRN-REFERENCE NOT = WS-REFERENCE
            IF WS-DET-SUMM = "D"
             IF WS-JRN-ACCEPT = " "
               MOVE 0 TO WS-LINE
               MOVE CRJRN-REFERENCE TO WS-REFERENCE
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC 
               PERFORM PRR-960.
      *       ELSE
      *         PERFORM PRR-960.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           MOVE CRJRN-REFERENCE          TO D-JRN
           MOVE CRJRN-INV-DATE           TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE             TO D-DATE
           MOVE CRJRN-COMPLETE           TO D-POST
           MOVE CRJRN-PERIOD             TO D-PERIOD.
           IF CRJRN-FUTURE = " "
              MOVE "N"  TO WS-FUTURE-ERR
           ELSE
              MOVE "Y"  TO WS-FUTURE-ERR.
       PRR-025.
           IF LINE-CNT > 60
               PERFORM PRR-060.
           IF WS-DET-SUMM = "S"
              WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           ELSE
              WRITE PRINT-REC FROM DETAIL-LINE AFTER 2.
       PRR-027.
           IF WS-DET-SUMM = "S"
               MOVE 0 TO WS-WORK-TOTAL
                         WS-GOODS-AMT
                         WS-VAT-TOTAL.
           MOVE " " TO PRINT-REC DETAIL-LINE D-FILLER
           MOVE 0   TO WS-QUANTITY
           ADD 1    TO LINE-CNT.
       PRR-030.
           GO TO PRR-002.
       PRR-060.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           IF WS-DET-SUMM = "D"
              WRITE PRINT-REC FROM HEAD3-DET AFTER 1
           ELSE
              MOVE "   NETT AMT    VAT AMT  JRN TOTAL" TO H3-DET
              WRITE PRINT-REC FROM HEAD3-SUM AFTER 1.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE " " TO PRINT-REC
           MOVE 5 TO LINE-CNT.
       PRR-900.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
       PRR-950.
           IF WS-FUTURE-ERR = "Y"
            IF CRJRN-FUTURE NOT = "F"
               MOVE "*" TO D-FUTURE.
           IF WS-FUTURE-ERR = "N"
            IF CRJRN-FUTURE = "F"
               MOVE "*" TO D-FUTURE.
       PRR-955.
           COMPUTE WS-GOODS-AMT = WS-WORK-TOTAL - WS-VAT-TOTAL.

           MOVE WS-GOODS-AMT  TO D-GOODS-TOTAL
           MOVE WS-VAT-TOTAL  TO D-VAT-TOTAL
           MOVE WS-WORK-TOTAL TO D-JRN-TOTAL.
       PRR-960.
           IF LINE-CNT > 60
               PERFORM PRR-060.
           ADD 1                   TO WS-LINE.
           MOVE CRJRN-REFERENCE    TO I-JRN
           
           MOVE CRJRN-INV-DATE     TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE       TO I-DATE.
           
           PERFORM READ-CREDITOR.
           
           MOVE WS-LINE            TO I-LINE
           MOVE CR-NAME            TO I-NAME
           MOVE CRJRN-PERIOD       TO I-PERIOD
           MOVE CRJRN-COMPLETE     TO I-POST
           MOVE CRJRN-CRACC-NUMBER TO I-ACC
           MOVE CRJRN-INV-NO       TO I-INV-NO
           MOVE CRJRN-LOC-AMT      TO I-INV-AMT.
           
           ADD CRJRN-LOC-AMT       TO WS-WORK-TOTAL
           ADD CRJRN-VAT-AMT       TO WS-VAT-TOTAL
           ADD CRJRN-UNAPPLIED-AMT TO WS-UNAPPLIED-TOTAL.
           
           WRITE PRINT-REC FROM INVOICE-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       INVOICE-LINE
                       I-FILLER.
       PRR-970.
           IF WS-DET-SUMM = "S"
      *      IF WS-JRN-ACCEPT NOT = " "
               GO TO PRR-999.
           MOVE SPACES             TO INVOICE-LINE
           
           MOVE "GOODS AMT "       TO I-INV-NO
           COMPUTE WS-GOODS-AMT = WS-WORK-TOTAL - WS-VAT-TOTAL.
           MOVE WS-GOODS-AMT       TO I-INV-AMT
           WRITE PRINT-REC FROM INVOICE-LINE AFTER 2.
           MOVE " " TO PRINT-REC
                       INVOICE-LINE.

           MOVE "  VAT AMT "       TO I-INV-NO
           MOVE WS-VAT-TOTAL       TO I-INV-AMT
           WRITE PRINT-REC FROM INVOICE-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       INVOICE-LINE.
           
           MOVE "    TOTAL "       TO I-INV-NO
           MOVE WS-WORK-TOTAL      TO I-INV-AMT.
           WRITE PRINT-REC FROM INVOICE-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       INVOICE-LINE.
       PRR-999.
           EXIT.
      *
       READ-CREDITOR SECTION.
       RCR-000.
           MOVE CRJRN-CRACC-NUMBER TO CR-ACCOUNT-NUMBER
           START CREDITOR-MASTER KEY NOT < CR-KEY
               INVALID KEY NEXT SENTENCE.
       RCR-010.
           READ CREDITOR-MASTER
                INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE "** UNKNOWN **" TO CR-NAME
                MOVE " "             TO WS-CREDITOR-ST1
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
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO GLPARAMETER RECORD, 'ESC' TO EXIT."
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
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-005.
           OPEN I-O CRJRN-FILE.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CRJRN FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-005.
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
           MOVE GLPA-CURRENT-CRPER TO WS-CURRENTPER
           PERFORM ENTER-PERIOD-DATES.
           MOVE WS-CURRENTPER TO SUB-1
           MOVE GL-BEGDATE (SUB-1) TO WS-BEG-DATE
           MOVE GL-ENDDATE (SUB-1) TO WS-END-DATE.
           CLOSE GLPARAMETER-FILE.
       OPEN-106.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-005.
           IF LINE-CNT > 60
               PERFORM PRR-060.
               
           IF WS-JRN-ACCEPT > " "
            IF WS-DET-SUMM = "S"
              MOVE "** ONLY A SPECIFIC CRJRN PRINTED IN SUMMARY. **"
                 TO PRINT-REC
            ELSE
              MOVE "** ONLY A SPECIFIC CRJRN PRINTED IN DETAIL. **"
                 TO PRINT-REC.
           IF WS-JRN-ACCEPT = " "
            IF WS-DET-SUMM = "S"
              MOVE "** ALL CRJRN'S PRINTED IN SUMMARY. **"
                 TO PRINT-REC
            ELSE
              MOVE "** ALL CRJRN'S PRINTED IN DETAIL. **"
                 TO PRINT-REC.

           WRITE PRINT-REC AFTER 1.
           MOVE " " TO PRINT-REC.
                 
           IF WS-NO NOT = 0
              MOVE "** ONLY CRJRN'S FOR A SPECIFIC PERIOD PRINTED. **"
              TO PRINT-REC
           ELSE
              MOVE "** CRJRN'S FOR ALL PERIODS PRINTED. **"
              TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
           MOVE " " TO PRINT-REC.
              
           MOVE "** POST: N=NOT POSTED, P=POSTED, Y=COMPLETE. ***"
              TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
           MOVE " " TO PRINT-REC.
           
       END-008.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-010.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE CRJRN-FILE
                 CREDITOR-MASTER.
       END-900.
            EXIT PROGRAM.
      *       STOP RUN.
       END-999.
            EXIT.
      *
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "EnterPeriodDates".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      * END-OF-JOB.
