        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlSManRp.
        AUTHOR.     STEVE CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

           SELECT READ-FILE ASSIGN TO "/ctools/spl/sl6" 
               ORGANIZATION IS LINE SEQUENTIAL 
               FILE STATUS IS WS-READ-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
      *
       FD  READ-FILE.
       01  READ-REC.
           03  FILLER           PIC X(80).

      *
       WORKING-STORAGE SECTION.
       77  WS-MESSAGE-PART      PIC X(79) VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  W-STATUS             PIC 99 VALUE 0.
       77  WS-MESS              PIC X VALUE " ".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  POS                  PIC 9(4) VALUE 0.
       77  WS-RANGE             PIC X VALUE " ".
       77  WS-THIS-YEAR         PIC X VALUE " ".
       77  WS-TOTALS-ONLY       PIC X VALUE " ".
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-MARGIN            PIC S9(7)V99 VALUE 0.
       77  WS-PERC              PIC S999V99.
       77  TOT-SALESAMT-PTD     PIC S9(7)V99 VALUE 0.
       77  TOT-SALESAMT-YTD     PIC S9(7)V99 VALUE 0.
       77  TOT-SALESAMT-LAST    PIC S9(7)V99 VALUE 0.
       77  TOT-COST-PTD         PIC S9(7)V99 VALUE 0.
       77  TOT-COST-YTD         PIC S9(7)V99 VALUE 0.
       77  TOT-COST-LAST        PIC S9(7)V99 VALUE 0.
       01  WS-READ-STATUS.
           03  WS-READ-ST1      PIC 99.
       01  WS-SBREP-STATUS.
           03  WS-SBREP-ST1     PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(6) VALUE " ".
           03  FILLER         PIC X(35) VALUE
           "SALESMAN CORPORATE ACCOUNT ANALYSIS".
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
       01  HEAD2.
           03  FILLER         PIC X(23) VALUE " ".
           03  FILLER         PIC X(35) VALUE ALL "*".
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(16) VALUE "SALESMAN CODE:".
           03  H1-SALESMAN    PIC X.
       01  HEAD2-1.
           03  FILLER         PIC X(62) VALUE " ".
           03  H2-SOLD-BY     PIC X(16).
       01  HEAD3.
           03  FILLER         PIC X(43) VALUE " ".
           03  FILLER         PIC X(33) VALUE
           "PERIOD / YEAR / LAST YEAR TO DATE".
       01  HEAD4.
           03  FILLER         PIC X(23) VALUE "ACCOUNT NO & NAME".
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(10) VALUE "BALANCE".
           03  FILLER         PIC X(4) VALUE " ".
           03  FILLER         PIC X(10) VALUE "SALES".
           03  FILLER         PIC X(10) VALUE "   COST".
           03  FILLER         PIC X(14) VALUE "   MARGIN".
           03  FILLER         PIC X(3) VALUE "%".
       01  DETAIL-LINE.
           03  D-CATEGORY.
               05  D-ACCOUNT  PIC X(9) VALUE " ".
               05  FILLER     PIC X(14) VALUE " ".
               05  D-BALANCE  PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-SALESAMT     PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-COST         PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-MARGIN       PIC Z(6)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PERC         PIC Z(2)9.99-.
       01  TOTAL-LINE.
           03 TOT-DESC        PIC X(28).
           03 TOT-DATE        PIC X(10).
      *
       Procedure Division.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
            CALL "SYSTEM" USING "less /ctools/spl/sl6"
            RETURNING W-STATUS.

            go to END-900.

            PERFORM CLEAR-SCREEN-PART.
            MOVE 315 TO POS.
            DISPLAY "** SALESMAN CORPORATE A/C ANALYSIS **" AT POS
            MOVE 415 TO POS
            DISPLAY "*************************************" AT POS.
            accept ws-accept at pos.
            PERFORM OPEN-FILES.
       CONTROL-045.
           PERFORM READ-ROUTINE.
       CONTROL-050.
           PERFORM END-OFF.
      *
      *
       READ-ROUTINE SECTION.
       PRR-005.
           READ READ-FILE
               AT END
               GO TO PRR-900.
           DISPLAY "READ-STATUS" AT POS.
           DISPLAY WS-READ-ST1 AT POS
           ACCEPT WS-ACCEPT.
       PRR-010.
           MOVE 0301 TO POS
           DISPLAY READ-REC AT POS
           ADD 100 TO POS
           GO TO PRR-005.
       prr-900.
           display "end of file reached" at pos
           accept ws-accept.
       PRR-999.
           EXIT.
      *
       CLEAR-SCREEN-PART SECTION.
       CSP-005.
           MOVE 0301 TO POS
           DISPLAY WS-MESSAGE-PART AT POS
           MOVE 0 TO LINE-CNT.
       CSP-010.
           ADD 1 TO LINE-CNT
           ADD 80 TO POS
           DISPLAY WS-MESSAGE-PART AT POS.
           IF LINE-CNT < 29
              GO TO CSP-010.
           MOVE 0280 TO POS
           DISPLAY WS-MESS AT POS.
       CSP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN INPUT READ-FILE.
           DISPLAY WS-READ-ST1 AT POS
           ACCEPT WS-ACCEPT.
       OPEN-999.
            EXIT.
      *
        END-OFF SECTION.
        END-000.
           CLOSE READ-FILE.

          ACCEPT WS-ACCEPT AT POS.
       END-900.
           STOP RUN.
       END-999.
           EXIT.
      *      
      *
      * END-OF-JOB.
