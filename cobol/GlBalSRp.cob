        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlBalSRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlMaster".
        Copy "SelectGlParameter".
        Copy "SelectGlTrans".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdGlMast.
           COPY ChlfdGlTrans.
           COPY ChlfdGlParam.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT                  PIC 9(3) VALUE 66.
       77  WS-PRINTANSWER            PIC X(10) VALUE " ".
       77  WS-PRINT                  PIC X VALUE " ".
       77  PAGE-CNT                  PIC 9(3) VALUE 0.
       77  WS-NEXT-NUMBER1           PIC X(12) VALUE " ".
       77  WS-NEXT-NUMBER2           PIC X(12) VALUE " ".
       77  WS-MARGIN-PERC            PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN                 PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-MV-YTD          PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-MV-LAST         PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN1                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN2                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN3                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN1-CAPIT          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN2-CAPIT          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN3-CAPIT          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN1-ASSETS         PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN2-ASSETS         PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN3-ASSETS         PIC S9(8)V99 VALUE 0.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1      PIC 99.
       01  WS-GLTRANS-STATUS.
           03  WS-GLTRANS-ST1     PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-HEADER  PIC X(2).
               05  WS-SUB     PIC X(4).
           03  WS-REST        PIC X(6).
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(33) VALUE " ".
           03  FILLER         PIC X(40) VALUE
           "GENERAL LEDGER BALANCE SHEET".
           03  FILLER         PIC X(7) VALUE "PERIOD:".
           03  H1-PERIOD      PIC X(3).
           03  H1-PER-BEG     PIC X(10).
           03  FILLER         PIC X(4) VALUE " TO ".
           03  H1-PER-END     PIC X(10).
           03  FILLER         PIC X(2) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
       01  HEAD3.
           03  FILLER         PIC X(44) VALUE " ".
           03  FILLER         PIC X(92) VALUE
           "This Year      Previous      Movement        Move".
       01  HEAD3-1.
           03  FILLER         PIC X(44) VALUE " ".
           03  H3-1BEG        PIC X(4).
           03  FILLER         PIC X VALUE "/".
           03  H3-1END        PIC X(9).
           03  H3-2BEG        PIC X(4).
           03  FILLER         PIC X VALUE "/".
           03  H3-2END        PIC X(14).
           03  FILLER         PIC X(20) VALUE "Rand          %".
       01  DETAIL-LINE.
           03  D-NAME         PIC X(42).
           03  D-MV-YTD       PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-MV-LAST      PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-MV-RAND      PIC Z(7)9.99-.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-PERC         PIC Z(3)9.99-.
           03  FILLER         PIC X(42) VALUE " ".
       01  UNDER-LINE.
           03  FILLER            PIC X(42) VALUE " ".
           03  FILLER            PIC X(14) VALUE "-----------".
           03  FILLER            PIC X(14) VALUE "-----------".
           03  FILLER            PIC X(16) VALUE "-----------".
           03  FILLER            PIC X(50) VALUE "-------".
       01  DOUBLE-UNDER-LINE.
           03  FILLER            PIC X(42) VALUE " ".
           03  FILLER            PIC X(14) VALUE "===========".
           03  FILLER            PIC X(14) VALUE "===========".
           03  FILLER            PIC X(16) VALUE "===========".
           03  FILLER            PIC X(50) VALUE "=======".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 321 TO POS.
           DISPLAY "** GENERAL LEDGER BALANCE SHEET **" AT POS.
           MOVE 421 TO POS.           
           DISPLAY "**********************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM OPEN-FILES.
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
            IF SUB-1 = 1
                MOVE " " TO PRINT-REC DETAIL-LINE
                MOVE "CAPITAL EMPLOYED" TO D-NAME
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                MOVE "70-015-10-00" TO WS-NEXT-NUMBER1
                MOVE "70-015-99-00" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 2
                MOVE "75-030-20-05" TO WS-NEXT-NUMBER1
                MOVE "75-030-20-99" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "N" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 3
                MOVE "Directors Loan A/cs   " TO D-NAME
                MOVE WS-MARGIN-MV-YTD         TO D-MV-YTD
                MOVE WS-MARGIN-MV-LAST        TO D-MV-LAST
                COMPUTE WS-MARGIN = WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST
                MOVE WS-MARGIN                TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = 
                   (WS-MARGIN / WS-MARGIN-MV-LAST) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                MOVE "DISTRIBUTABLE RESERVES" TO D-NAME
                MOVE WS-COLUMN1               TO D-MV-YTD
                MOVE WS-COLUMN2               TO D-MV-LAST
                MOVE WS-COLUMN3               TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = ((WS-COLUMN1 - WS-COLUMN2)
                    / WS-COLUMN2) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                MOVE "70-010      " TO WS-NEXT-NUMBER1
                MOVE "70-010      " TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 4
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                MOVE "TOTAL CAPITAL EMPLOYED" TO D-NAME
                MOVE WS-COLUMN1               TO D-MV-YTD
                MOVE WS-COLUMN2               TO D-MV-LAST
                MOVE WS-COLUMN3               TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = ((WS-COLUMN1 - WS-COLUMN2)
                     / WS-COLUMN2) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC FROM DOUBLE-UNDER-LINE
                MOVE " " TO PRINT-REC
                WRITE PRINT-REC
                MOVE "EMPLOYEMENT OF CAPITAL" TO D-NAME
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                MOVE 0 TO WS-COLUMN1 WS-COLUMN1-CAPIT
                          WS-COLUMN2 WS-COLUMN2-CAPIT
                          WS-COLUMN3 WS-COLUMN3-CAPIT
                MOVE "75-010      " TO WS-NEXT-NUMBER1
                MOVE "75-010      " TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 5
                MOVE "75-020-05-00" TO WS-NEXT-NUMBER1
                MOVE "75-020-05-00" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 6
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                MOVE "TOTAL FIXED ASSETS    " TO D-NAME
                MOVE WS-COLUMN1               TO D-MV-YTD
                MOVE WS-COLUMN2               TO D-MV-LAST
                MOVE WS-COLUMN3               TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = ((WS-COLUMN1 - WS-COLUMN2)
                     / WS-COLUMN2) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                MOVE WS-COLUMN1 TO WS-COLUMN1-CAPIT
                MOVE WS-COLUMN2 TO WS-COLUMN2-CAPIT
                MOVE WS-COLUMN3 TO WS-COLUMN3-CAPIT
                MOVE 0          TO WS-COLUMN1
                                   WS-COLUMN2
                                   WS-COLUMN3
                                   WS-MARGIN-MV-YTD
                                   WS-MARGIN-MV-LAST
                                   WS-MARGIN
                                   WS-MARGIN-PERC
                MOVE "75-020-10-05" TO WS-NEXT-NUMBER1
                MOVE "75-020-10-15" TO WS-NEXT-NUMBER2
                MOVE "N" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 7
                MOVE "Sundry Debtors        " TO D-NAME
                MOVE WS-MARGIN-MV-YTD         TO D-MV-YTD
                MOVE WS-MARGIN-MV-LAST        TO D-MV-LAST
                COMPUTE WS-MARGIN = WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST
                MOVE WS-MARGIN                TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = 
                   (WS-MARGIN / WS-MARGIN-MV-LAST) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                MOVE "75-020-05-05" TO WS-NEXT-NUMBER1
                MOVE "75-020-05-05" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 8
                MOVE "75-030-15-00" TO WS-NEXT-NUMBER1
                MOVE "75-030-15-00" TO WS-NEXT-NUMBER2
                MOVE "Y" TO WS-PRINT
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                GO TO GET-999.
                
            IF SUB-1 = 9
                MOVE "75-020-15-00" TO WS-NEXT-NUMBER1
                MOVE "75-020-20-99" TO WS-NEXT-NUMBER2
                MOVE "Y" TO WS-PRINT
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                GO TO GET-999.
            IF SUB-1 = 10
                MOVE "75-020-25-00" TO WS-NEXT-NUMBER1
                MOVE "75-020-25-99" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "N" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 11
                MOVE "Deposits              " TO D-NAME
                MOVE WS-MARGIN-MV-YTD         TO D-MV-YTD
                MOVE WS-MARGIN-MV-LAST        TO D-MV-LAST
                COMPUTE WS-MARGIN = WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST
                MOVE WS-MARGIN                TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = 
                   (WS-MARGIN / WS-MARGIN-MV-LAST) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                MOVE "75-020-30-10" TO WS-NEXT-NUMBER1
                MOVE "75-020-50-00" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
      *     IF SUB-1 = 11
      *         MOVE "Unsecured Loans       " TO D-NAME
      *         MOVE WS-MARGIN-MV-YTD         TO D-MV-YTD
      *         MOVE WS-MARGIN-MV-LAST        TO D-MV-LAST
      *         COMPUTE WS-MARGIN = WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST
      *         MOVE WS-MARGIN                TO D-MV-RAND
      *         COMPUTE WS-MARGIN-PERC = 
      *            (WS-MARGIN / WS-MARGIN-MV-LAST) * 100
      *         MOVE WS-MARGIN-PERC           TO D-PERC
      *         WRITE PRINT-REC FROM DETAIL-LINE
      *         MOVE " " TO PRINT-REC DETAIL-LINE
      *         MOVE "75-020-35-00" TO WS-NEXT-NUMBER1
      *         MOVE "75-020-50-00" TO WS-NEXT-NUMBER2
      *         MOVE "Y" TO WS-PRINT
      *         GO TO GET-999.
            IF SUB-1 = 12
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                MOVE "CURRENT ASSETS        " TO D-NAME
                MOVE WS-COLUMN1               TO D-MV-YTD
                MOVE WS-COLUMN2               TO D-MV-LAST
                MOVE WS-COLUMN3               TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = ((WS-COLUMN1 - WS-COLUMN2)
                     / WS-COLUMN2) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                ADD  WS-COLUMN1 TO WS-COLUMN1-CAPIT WS-COLUMN1-ASSETS
                ADD  WS-COLUMN2 TO WS-COLUMN2-CAPIT WS-COLUMN2-ASSETS
                ADD  WS-COLUMN3 TO WS-COLUMN3-CAPIT WS-COLUMN3-ASSETS
                MOVE 0          TO WS-COLUMN1
                                   WS-COLUMN2
                                   WS-COLUMN3
                                   WS-MARGIN-MV-YTD
                                   WS-MARGIN-MV-LAST
                                   WS-MARGIN
                                   WS-MARGIN-PERC
                MOVE "75-030-05-00" TO WS-NEXT-NUMBER1
                MOVE "75-030-05-15" TO WS-NEXT-NUMBER2
                MOVE "N" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 13
                MOVE "Sundry Creditors      " TO D-NAME
                MOVE WS-MARGIN-MV-YTD         TO D-MV-YTD
                MOVE WS-MARGIN-MV-LAST        TO D-MV-LAST
                COMPUTE WS-MARGIN = WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST
                MOVE WS-MARGIN                TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = 
                   (WS-MARGIN / WS-MARGIN-MV-LAST) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                MOVE "75-030-05-20" TO WS-NEXT-NUMBER1
                MOVE "75-030-15-99" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 14
                MOVE "75-030-30-05" TO WS-NEXT-NUMBER1
                MOVE "75-030-30-99" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "N" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 15
                MOVE "Bank Asset Finance    " TO D-NAME
                MOVE WS-MARGIN-MV-YTD         TO D-MV-YTD
                MOVE WS-MARGIN-MV-LAST        TO D-MV-LAST
                COMPUTE WS-MARGIN = WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST
                MOVE WS-MARGIN                TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = 
                   (WS-MARGIN / WS-MARGIN-MV-LAST) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                MOVE "CURRENT LIABILITIES   " TO D-NAME
                MOVE WS-COLUMN1               TO D-MV-YTD
                MOVE WS-COLUMN2               TO D-MV-LAST
                MOVE WS-COLUMN3               TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = (WS-COLUMN3 / WS-COLUMN2) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                MOVE " " TO PRINT-REC
                ADD  WS-COLUMN1 TO WS-COLUMN1-CAPIT WS-COLUMN1-ASSETS
                ADD  WS-COLUMN2 TO WS-COLUMN2-CAPIT WS-COLUMN2-ASSETS
                ADD  WS-COLUMN3 TO WS-COLUMN3-CAPIT WS-COLUMN3-ASSETS
                MOVE 0          TO WS-COLUMN1
                                   WS-COLUMN2
                                   WS-COLUMN3
                                   WS-MARGIN-MV-YTD
                                   WS-MARGIN-MV-LAST
                                   WS-MARGIN
                                   WS-MARGIN-PERC
                MOVE "NETT CURRENT ASSETS/LIABILITIES(-)" TO D-NAME
                MOVE WS-COLUMN1-ASSETS         TO D-MV-YTD
                MOVE WS-COLUMN2-ASSETS         TO D-MV-LAST
                MOVE WS-COLUMN3-ASSETS         TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC =
                  ((WS-COLUMN1-ASSETS - WS-COLUMN2-ASSETS)
                     / WS-COLUMN2-ASSETS) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                MOVE "ASSOCIATED COMPANIES  " TO D-NAME
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                MOVE 0 TO WS-COLUMN1
                          WS-COLUMN2
                          WS-COLUMN3
                          WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "75-040-05-00" TO WS-NEXT-NUMBER1
                MOVE "75-040-99-99" TO WS-NEXT-NUMBER2
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 16
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                MOVE "LOANS FROM(-)/TO ASSOCIATE CO'S" TO D-NAME
                MOVE WS-COLUMN1                        TO D-MV-YTD
                MOVE WS-COLUMN2                        TO D-MV-LAST
                MOVE WS-COLUMN3                        TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = ((WS-COLUMN1 - WS-COLUMN2)
                     / WS-COLUMN2) * 100
                MOVE WS-MARGIN-PERC                    TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                ADD  WS-COLUMN1 TO WS-COLUMN1-CAPIT
                ADD  WS-COLUMN2 TO WS-COLUMN2-CAPIT
                ADD  WS-COLUMN3 TO WS-COLUMN3-CAPIT
                MOVE "TOTAL EMPLOYEMENT OF CAPITAL" TO D-NAME
                MOVE WS-COLUMN1-CAPIT               TO D-MV-YTD
                MOVE WS-COLUMN2-CAPIT               TO D-MV-LAST
                MOVE WS-COLUMN3-CAPIT               TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC =
                  ((WS-COLUMN1-CAPIT - WS-COLUMN2-CAPIT)
                     / WS-COLUMN2-CAPIT) * 100
                MOVE WS-MARGIN-PERC    TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC FROM DOUBLE-UNDER-LINE
                MOVE " " TO PRINT-REC.
            PERFORM END-OFF.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE 1 TO SUB-1.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
            MOVE 2910 TO POS.
            DISPLAY "The Report is being compiled.........." AT POS.
       PRR-001.
            IF SUB-1 = 1
              PERFORM PRR-010.
            PERFORM GET-DATA.
            MOVE WS-NEXT-NUMBER1 TO GL-NUMBER.
            START GL-MASTER KEY NOT < GL-KEY
              INVALID KEY NEXT SENTENCE.
       PRR-002.
            READ GL-MASTER NEXT 
               AT END NEXT SENTENCE.
            IF WS-GLMAST-ST1 = 10
               MOVE 0 TO WS-GLMAST-ST1
               ADD 1 TO SUB-1
               GO TO PRR-001.
            IF WS-GLMAST-ST1 NOT = 0
              MOVE "GL-MASTER BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO PRR-002.
            IF GL-NUMBER < WS-NEXT-NUMBER1
               GO TO PRR-002.
            IF GL-NUMBER > WS-NEXT-NUMBER2
               GO TO PRR-060.
       PRR-010.
            IF LINE-CNT < 58
               GO TO PRR-020.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE.
            MOVE " " TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
            WRITE PRINT-REC FROM HEAD1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD3-1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE 6 TO LINE-CNT.
       PRR-020.
           IF SUB-1 = 8
            IF GL-NUMBER = "75-030-15-00"
             IF GL-BALANCE < 0
               GO TO PRR-060.
           IF SUB-1 = 13
            IF GL-NUMBER = "75-030-15-00"
             IF GL-BALANCE > 0
               GO TO PRR-002.
      *         GO TO PRR-060.
       
           PERFORM COMPUTE-FIGURES
           IF WS-PRINT = "N"
                PERFORM PRR-050
                GO TO PRR-002.
           MOVE GL-DESCRIPTION          TO D-NAME
           MOVE GL-BALANCE              TO D-MV-YTD
           MOVE GL-OPEN-YEAR-BAL        TO D-MV-LAST
           MOVE WS-MARGIN               TO D-MV-RAND
           MOVE WS-MARGIN-PERC          TO D-PERC.

           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE.
       PRR-050.
           MOVE 2610 TO POS
           DISPLAY "ACCOUNT PROCESSED:" AT POS
           ADD 20 TO POS
           DISPLAY GL-NUMBER AT POS.
       PRR-055.
           ADD 1 TO LINE-CNT
           GO TO PRR-002.
       PRR-060.
           ADD 1 TO LINE-CNT SUB-1
           GO TO PRR-001.
       PRR-999.
           EXIT.
      *
       COMPUTE-FIGURES SECTION.
       CF-010.
           IF WS-PRINT = "Y"
               MOVE 0 TO WS-MARGIN-MV-YTD
                         WS-MARGIN-MV-LAST
                         WS-MARGIN
                         WS-MARGIN-PERC.

       CF-015.
           ADD GL-BALANCE       TO WS-MARGIN-MV-YTD.
           ADD GL-OPEN-YEAR-BAL TO WS-MARGIN-MV-LAST.
           COMPUTE WS-MARGIN = WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST.
       CF-500.
           COMPUTE WS-MARGIN-PERC =
             ((GL-BALANCE - GL-OPEN-YEAR-BAL) / GL-OPEN-YEAR-BAL) * 100.
           COMPUTE WS-MARGIN = GL-BALANCE - GL-OPEN-YEAR-BAL.
           ADD GL-BALANCE       TO WS-COLUMN1
           ADD GL-OPEN-YEAR-BAL TO WS-COLUMN2
           ADD WS-MARGIN        TO WS-COLUMN3.
       CF-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO GLPARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING W-ERC
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON READ, RP-000, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0
             MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
             TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-GLMAST-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             MOVE 0 TO WS-GLMAST-ST1
             GO TO OPEN-000.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER FILE BUSY, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-012.

            PERFORM READ-PARAMETER.
            MOVE GLPA-NAME TO CO-NAME.
            PERFORM ENTER-PERIOD-DATES.
       OPEN-015.
           MOVE 1 TO SUB-1
           MOVE GL-BEGDATE (SUB-1) TO SPLIT-DATE
           MOVE SPLIT-YY TO H3-1BEG
           SUBTRACT 1 FROM SPLIT-YY
           MOVE SPLIT-YY TO H3-2BEG.

           MOVE 13 TO SUB-1
           MOVE GLPA-PER (SUB-1) TO SPLIT-DATE
           MOVE SPLIT-YY TO H3-1END
           SUBTRACT 1 FROM SPLIT-YY
           MOVE SPLIT-YY TO H3-2END
           PERFORM OPEN-260.
           CLOSE GLPARAMETER-FILE.
       OPEN-250.
           OPEN I-O GLTRANS-FILE.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE "GLTRANS-ST1 ERROR IN OPENING, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO OPEN-250.
       OPEN-260.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
          EXIT.
      *
       END-OFF SECTION.
       END-010.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-500.
           CLOSE GL-MASTER
                 GLTRANS-FILE.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       Copy "EnterGLPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
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
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
