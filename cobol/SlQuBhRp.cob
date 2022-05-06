        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlQuBhRp.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        REPOSITORY. 
           FUNCTION ALL INTRINSIC.
        SPECIAL-NAMES.
          CRT STATUS IS W-CRTSTATUS.
        SPECIAL-NAMES.
        CLASS WS-VALID-EMAIL IS
          '@' '_' '.' '-' '#'
          'a' THRU 'i'
          'j' THRU 'r'
          's' THRU 'z'
          '0' THRU '9'.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.

        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT QUOTE-IMP-MASTER ASSIGN TO "/ctools/spl/QuoteSequ"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-IMPORT-STATUS.
         Copy "SelectSlQuoteBatch".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdSlQuoteBatch.
           COPY ChlfdQuoteImport.
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(255).

      *
       WORKING-STORAGE SECTION.
       77  WS-INITIAL           PIC X(2) VALUE " ".
       77  WS-DATE-ACCEPT       PIC X(10) VALUE " ".
       77  WS-REP-ONLY          PIC X VALUE " ".
       77  WS-TODAY-ONLY        PIC X VALUE " ".
       77  WS-QUANTITY          PIC S9(5) VALUE 0.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-TRANSNO           PIC 9(3) VALUE 0.
       77  WS-TOTI-INVOICE      PIC S9(8)V99 VALUE 0.
       77  WS-QUOTE-NUMBER      PIC 9(6) VALUE 0.
       77  WS-INVDISPLAY        PIC Z(5)9.
       77  WS-ACCEPT            PIC XX VALUE " ".
       77  WS-PAGE              PIC 9(3) VALUE 0.
       77  WS-LINE              PIC 9(3) VALUE 66.
       77  WS-PRINTER-PDF       PIC X(100) VALUE " ".
       77  WS-SUBJECT-FIXED     PIC X(100) VALUE " ".      
       77  WSF-MAIL-NUMBER      PIC X(50) VALUE " ".
       01  W-CRTSTATUS          PIC 9(4) value 0.
       01  WS-QUBATCH-STATUS.
           03  WS-QUBATCH-ST1         PIC 99.
       01  WS-IMPORT-STATUS.
           03  WS-IMPORT-ST1         PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 315 TO POS
           DISPLAY "** Quotation Batch Email Re-Send Report **" AT POS
           MOVE 415 TO POS
           DISPLAY "******************************************" AT POS.
       CONTROL-003.
      *     Copy "PrinterAccept".
       CONTROL-015.
           MOVE 1010 TO POS
           DISPLAY "IMPORT FILE MUST BE /ctools/spl/QuoteSequ" AT POS

           MOVE 1310 TO POS.
           DISPLAY "PRESS ENTER TO START SENDING ....."
           AT POS.
           ADD 47 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-INITIAL.

      *     ACCEPT WS-INITIAL AT POS.
           IF W-ESCAPE-KEY = 4
               EXIT PROGRAM.
           IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
               GO TO CONTROL-018
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-018.
           PERFORM OPEN-FILES.
           PERFORM IMPORT-DATA.
           PERFORM END-OFF.
       CONTROL-999.
          EXIT.
      *
       IMPORT-DATA SECTION.
       ID-000.
           MOVE 0   TO SUB-20.
           MOVE " " TO QUOTE-IMP-KEY.
       ID-005.
           READ QUOTE-IMP-MASTER
             AT END
                 GO TO ID-900.
       ID-010.
           MOVE 2610 TO POS
           DISPLAY "QUOTE RECORD BEING IMPORTED:      " AT POS
           ADD 30 TO POS
           DISPLAY QUOTE-IMP-QB-NUM AT POS.

           MOVE QUOTE-IMP-QB-NUM TO ALPHA-RATE.
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE               TO WS-QUOTE-NUMBER.

            ADD 1 TO SUB-20.
           
            MOVE 2010 TO POS
            DISPLAY "NUMBER OF RECORDS:" AT POS
            ADD 20 TO POS
            DISPLAY SUB-20 AT POS.
       ID-020.
            PERFORM READ-QUOTE-BATCH.

            IF WS-QUBATCH-ST1 = 0
               PERFORM SETUP-QUOTE-FOR-PDF-MGEMAIL.

            CALL "C$SLEEP" USING 2.
            GO TO ID-005.
       ID-900.
            MOVE "IMPORT FINISHED.........." TO WS-MESSAGE
            PERFORM ERROR-MESSAGE.   
       ID-999.
           EXIT.
      *
       READ-QUOTE-BATCH SECTION.
       RQB-010.
            MOVE WS-QUOTE-NUMBER TO QB-QUOTENUMBER.
            START QUOTEBATCH KEY NOT < QB-KEY
                   INVALID KEY NEXT SENTENCE.
           IF WS-QUBATCH-ST1 NOT = 0
              GO TO RQB-999.
       RQB-020.
            READ QUOTEBATCH
                INVALID KEY NEXT SENTENCE.
            IF WS-QUBATCH-ST1 NOT = 0
      *          MOVE "QUOTE BATCH FILE BUSY ON READ, 'ESC' TO RETRY"
      *          TO WS-MESSAGE
      *          PERFORM ERROR1-000
      *          MOVE WS-QUBATCH-ST1 TO WS-MESSAGE
      *          PERFORM ERROR-MESSAGE
      *          PERFORM ERROR1-020
      *          MOVE 0 TO WS-QUBATCH-ST1
                GO TO RQB-999.
            MOVE QB-PDF-PRINT-FILE    TO WS-PRINTER-PDF
            MOVE QB-EMAIL-SENDER      TO WS-USEREMAILADD
            MOVE QB-EMAIL-RECIPIENT   TO WSF-MAIL-NUMBER
            MOVE QB-SUBJECT-FIXED     TO WS-SUBJECT-FIXED.
            
      *      MOVE WS-PRINTER-PDF TO WS-MESSAGE
      *      PERFORM ERROR1-000
      *      MOVE WS-USEREMAILADD TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE
            
      *      MOVE WS-SUBJECT-FIXED TO WS-MESSAGE
      *      PERFORM ERROR1-000
      *      MOVE WSF-MAIL-NUMBER TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       RQB-030.
           PERFORM WORK-OUT-PDF-FILE-NAME.
       RQB-999.
           EXIT.
      *
       WORK-OUT-PDF-FILE-NAME SECTION.
       WOPFN-001.
           MOVE SPACES             TO ALPHA-RATE DATA-RATE.
           MOVE WS-PRINTER-PDF     TO ALPHA-RATE.
           MOVE "/ctools/faxsent/" TO DATA-RATE.
           MOVE 13 TO SUB-45
           MOVE 17 TO SUB-46.
       WOPFN-010.
           MOVE AL-RATE (SUB-45) TO DAT-RATE (SUB-46)
           ADD 1 TO SUB-45 SUB-46.
           IF AL-RATE (SUB-45) NOT = " "
               GO TO WOPFN-010.
           MOVE SPACES    TO WS-PRINTER-PDF
           MOVE DATA-RATE TO WS-PRINTER-PDF.

      *      MOVE WS-PRINTER-PDF TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       WOPFN-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-003.
            OPEN I-O QUOTE-IMP-MASTER.
            IF WS-IMPORT-ST1 NOT = 0
             MOVE 
             "ERC ON OPENING I-O IMPORT FILE - /ctools/spl/QuoteSequ."
              TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-IMPORT-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             GO TO OPEN-003.
       OPEN-020.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-021.
          OPEN I-O QUOTEBATCH.
          IF WS-QUBATCH-ST1 NOT = 0
              MOVE "QUBATCH FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-QUBATCH-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE WS-SLQUOTEBATCH TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-QUBATCH-ST1
              GO TO OPEN-021.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
      *     CLOSE PRINT-FILE.
      *     PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE QUOTEBATCH
                 QUOTE-IMP-MASTER.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *      
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
       Copy "SetupQuoteBatchForPDFMgEmail".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
