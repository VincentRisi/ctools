        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlInStRp.
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
       77  PAGE-CNT                  PIC 9(3) VALUE 0.
       77  WS-NEXT-NUMBER            PIC X(12) VALUE " ".
       77  WS-MTD-COST               PIC S9(8)V99 VALUE 0.
       77  WS-YTD-COST               PIC S9(8)V99 VALUE 0.
       77  WS-MTD-SALES              PIC S9(8)V99 VALUE 0.
       77  WS-YTD-SALES              PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-PERC            PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-MV-YTD          PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-BU-YTD          PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-MV-LAST-YTD     PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-MV-LAST-TOTAL   PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-BU-TOTAL        PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN1                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN2                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN3                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN4                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN5                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN6                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN7                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN1-SALES          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN2-SALES          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN3-SALES          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN4-SALES          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN5-SALES          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN6-SALES          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN7-SALES          PIC S9(8)V99 VALUE 0.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1         PIC 99.
       01  WS-GLTRANS-STATUS.
           03  WS-GLTRANS-ST1        PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1    PIC 99.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-HEADER   PIC X(2).
               05  WS-SUB      PIC X(4).
           03  WS-REST         PIC X(6).
       01  WS-DATE-SPLIT.
           03  WS-DATE-CC      PIC 99.
           03  WS-DATE-YY      PIC 99.
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(18) VALUE " ".
           03  FILLER         PIC X(55) VALUE
           "GENERAL LEDGER SUMMARY INCOME STATEMENT".
           03  FILLER         PIC X(7) VALUE "PERIOD:".
           03  H1-PERIOD      PIC X(3) VALUE " ".
           03  H1-PER-BEG     PIC X(10).
           03  FILLER         PIC X(4) VALUE " TO ".
           03  H1-PER-END     PIC X(10).
           03  FILLER         PIC X(2) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(1) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(41) VALUE " ".
           03  FILLER         PIC X(47) VALUE
           "Movement      Budget    Movement      Budget".
           03  FILLER         PIC X(44) VALUE
           "Last Year   Last Year      Budget      Move".
       01  HEAD3-1.
           03  FILLER         PIC X(46) VALUE " ".
           03  FILLER         PIC X(22) VALUE "Ptd         Ptd   Ytd".
           03  H3-1BEG        PIC X(2).
           03  FILLER         PIC X VALUE "/".
           03  H3-1END        PIC X(5).
           03  FILLER         PIC X(4) VALUE "Ytd".
           03  H3-2BEG        PIC X(2).
           03  FILLER         PIC X VALUE "/".
           03  H3-2END        PIC X(5).
           03  FILLER         PIC X(4) VALUE "Ytd ".
           03  H3-3BEG        PIC X(2).
           03  FILLER         PIC X VALUE "/".
           03  H3-3END        PIC X(9).
           03  FILLER         PIC X(12) VALUE "Total".
           03  H3-4BEG        PIC X(2).
           03  FILLER         PIC X VALUE "/".
           03  H3-4END        PIC X(9).
           03  FILLER         PIC X VALUE "%".
       01  DETAIL-LINE.
           03  D-NAME.
              05  D-NAME1         PIC X(21).
              05  D-NAMEPIC1      PIC X.
              05  D-NAMEPERC1     PIC Z(1)99.99.
              05  D-NAMEPIC2      PIC X(2).
              05  D-NAMEPIC3      PIC X.
              05  D-NAMEPERC2     PIC Z(1)99.99.
              05  D-NAMEPIC4      PIC X.
           03  D-MV-PTD       PIC Z(7)9.99-.
           03  D-BU-PTD       PIC Z(7)9.99-.
           03  D-MV-YTD       PIC Z(7)9.99-.
           03  D-BU-YTD       PIC Z(7)9.99-.
           03  D-MV-LAST-YTD  PIC Z(7)9.99-.
           03  D-MV-LAST      PIC Z(7)9.99-.
           03  D-BU           PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-PERC         PIC Z(3)9.99-.
       01  UNDER-LINE.
           03  FILLER            PIC X(38) VALUE " ".
           03  FILLER            PIC X(12) VALUE "-----------".
           03  FILLER            PIC X(12) VALUE "-----------".
           03  FILLER            PIC X(12) VALUE "-----------".
           03  FILLER            PIC X(12) VALUE "-----------".
           03  FILLER            PIC X(12) VALUE "-----------".
           03  FILLER            PIC X(12) VALUE "-----------".
           03  FILLER            PIC X(14) VALUE "-----------".
           03  FILLER            PIC X(8) VALUE  "-------".
       01  DOUBLE-UNDER-LINE.
           03  FILLER            PIC X(38) VALUE " ".
           03  FILLER            PIC X(12) VALUE "===========".
           03  FILLER            PIC X(12) VALUE "===========".
           03  FILLER            PIC X(12) VALUE "===========".
           03  FILLER            PIC X(12) VALUE "===========".
           03  FILLER            PIC X(12) VALUE "===========".
           03  FILLER            PIC X(12) VALUE "===========".
           03  FILLER            PIC X(14) VALUE "===========".
           03  FILLER            PIC X(8) VALUE  "=======".
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
           DISPLAY "** SUMMARY INCOME STATEMENT **" AT POS.
           MOVE 421 TO POS.           
           DISPLAY "******************************" AT POS.
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
                MOVE "50-300-05-00" TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 2
                MOVE "50-200-05-00" TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 3
                MOVE "50-200-08-00" TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 4
                MOVE "50-200-10-00" TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 5
                MOVE "50-200-15-00" TO WS-NEXT-NUMBER
                GO TO GET-999.
      *      IF SUB-1 = 5
      *          MOVE "50-200-20-00" TO WS-NEXT-NUMBER
      *          GO TO GET-999.
            IF SUB-1 = 6
                MOVE "TOTAL COST OF SALES" TO D-NAME
                MOVE WS-COLUMN1            TO D-MV-PTD
                                              WS-MTD-COST
                MOVE WS-COLUMN2            TO D-BU-PTD
                MOVE WS-COLUMN3            TO D-MV-YTD
                                              WS-YTD-COST
                MOVE WS-COLUMN4            TO D-BU-YTD
                MOVE WS-COLUMN5            TO D-MV-LAST-YTD
                MOVE WS-COLUMN6            TO D-MV-LAST
                MOVE WS-COLUMN7            TO D-BU
                COMPUTE WS-MARGIN-PERC = ((WS-COLUMN3 - WS-COLUMN5)
                     / WS-COLUMN5) * 100
                MOVE WS-MARGIN-PERC        TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                MOVE "60-200      " TO WS-NEXT-NUMBER
                MOVE GL-PER (SUB-4)   TO WS-MTD-SALES
                MOVE WS-MARGIN-MV-YTD TO WS-YTD-SALES
                GO TO GET-999.
            IF SUB-1 = 7
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                MOVE "GROSS PROFIT(-)/LOSS" TO D-NAME1
                MOVE "M"                    TO D-NAMEPIC1
                MOVE "Y"                    TO D-NAMEPIC3
                MOVE "%"                    TO D-NAMEPIC2
                                               D-NAMEPIC4
                COMPUTE WS-MARGIN-PERC ROUNDED = 
                   ((WS-COLUMN1 / WS-MTD-COST) * 100)
                MOVE WS-MARGIN-PERC         TO D-NAMEPERC1
                COMPUTE WS-MARGIN-PERC ROUNDED =
                   ((WS-COLUMN3 / WS-YTD-COST) * 100)
                MOVE WS-MARGIN-PERC         TO D-NAMEPERC2

                MOVE WS-COLUMN1             TO D-MV-PTD
                MOVE WS-COLUMN2             TO D-BU-PTD
                MOVE WS-COLUMN3             TO D-MV-YTD
                MOVE WS-COLUMN4             TO D-BU-YTD
                MOVE WS-COLUMN5             TO D-MV-LAST-YTD
                MOVE WS-COLUMN6             TO D-MV-LAST
                MOVE WS-COLUMN7             TO D-BU
                COMPUTE WS-MARGIN-PERC = ((WS-COLUMN3 - WS-COLUMN5)
                     / WS-COLUMN5) * 100
                MOVE WS-MARGIN-PERC TO D-PERC

                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                MOVE WS-COLUMN1 TO WS-COLUMN1-SALES
                MOVE WS-COLUMN2 TO WS-COLUMN2-SALES
                MOVE WS-COLUMN3 TO WS-COLUMN3-SALES
                MOVE WS-COLUMN4 TO WS-COLUMN4-SALES
                MOVE WS-COLUMN5 TO WS-COLUMN5-SALES
                MOVE WS-COLUMN6 TO WS-COLUMN6-SALES
                MOVE WS-COLUMN7 TO WS-COLUMN7-SALES
                MOVE 0          TO WS-COLUMN1
                                   WS-COLUMN2
                                   WS-COLUMN3
                                   WS-COLUMN4
                                   WS-COLUMN5
                                   WS-COLUMN6
                                   WS-COLUMN7
                MOVE "50-010      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 8
                MOVE "50-020      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 9
                MOVE "50-025      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 10
                MOVE "50-030      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 11
                MOVE "50-035      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 12
                MOVE "50-040      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 13
                MOVE "50-045      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 14
                MOVE "50-050      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 15
                MOVE "50-090      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 16
                MOVE "50-052      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 17
                MOVE "50-055      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 18
                MOVE "50-060      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 19
                MOVE "50-070      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 20
                MOVE "50-078      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 21
                MOVE "50-075      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 22
                MOVE "50-080      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 23
                MOVE "50-082      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 24
                MOVE "50-084      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 25
                MOVE "50-085      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 26
                MOVE "50-092      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 27
                MOVE "50-095      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 28
                MOVE "50-100      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 29
                MOVE "50-106      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 30
                MOVE "50-110      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 31
                MOVE "50-400      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 32
                MOVE "50-076      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 33
                MOVE "50-115      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 34
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                MOVE "TOTAL EXPENSES    " TO D-NAME
                MOVE WS-COLUMN1           TO D-MV-PTD
                MOVE WS-COLUMN2           TO D-BU-PTD
                MOVE WS-COLUMN3           TO D-MV-YTD
                MOVE WS-COLUMN4           TO D-BU-YTD
                MOVE WS-COLUMN5           TO D-MV-LAST-YTD
                MOVE WS-COLUMN6           TO D-MV-LAST
                MOVE WS-COLUMN7           TO D-BU
                COMPUTE WS-MARGIN-PERC = ((WS-COLUMN3 - WS-COLUMN5)
                     / WS-COLUMN5) * 100
                MOVE WS-MARGIN-PERC TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                MOVE "NETT OPERATING PROFIT(-)/LOSS" TO D-NAME
                ADD WS-COLUMN1 TO WS-COLUMN1-SALES
                ADD WS-COLUMN2 TO WS-COLUMN2-SALES
                ADD WS-COLUMN3 TO WS-COLUMN3-SALES
                ADD WS-COLUMN4 TO WS-COLUMN4-SALES
                ADD WS-COLUMN5 TO WS-COLUMN5-SALES
                ADD WS-COLUMN6 TO WS-COLUMN6-SALES
                ADD WS-COLUMN7 TO WS-COLUMN7-SALES
                MOVE WS-COLUMN1-SALES TO D-MV-PTD      WS-COLUMN1
                MOVE WS-COLUMN2-SALES TO D-BU-PTD      WS-COLUMN2
                MOVE WS-COLUMN3-SALES TO D-MV-YTD      WS-COLUMN3
                MOVE WS-COLUMN4-SALES TO D-BU-YTD      WS-COLUMN4
                MOVE WS-COLUMN5-SALES TO D-MV-LAST-YTD WS-COLUMN5
                MOVE WS-COLUMN6-SALES TO D-MV-LAST     WS-COLUMN6
                MOVE WS-COLUMN7-SALES TO D-BU          WS-COLUMN7
                COMPUTE WS-MARGIN-PERC =
                  ((WS-COLUMN3-SALES - WS-COLUMN5-SALES)
                     / WS-COLUMN5-SALES) * 100
                MOVE WS-MARGIN-PERC                  TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                MOVE "60-015      " TO WS-NEXT-NUMBER
      *          MOVE "50-200-15-00" TO WS-NEXT-NUMBER
                GO TO GET-999.
      *     IF SUB-1 = 34
      *         MOVE "60-015      " TO WS-NEXT-NUMBER
      *         GO TO GET-999.
            IF SUB-1 = 35
                MOVE "60-300      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 36
                MOVE "60-400      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 37
                MOVE "55-010      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 38
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC UNDER-LINE
                MOVE "PROFIT(-)/LOSS NOT POSTED" TO D-NAME
                MOVE WS-COLUMN1                  TO D-MV-PTD
                MOVE WS-COLUMN2                  TO D-BU-PTD
                MOVE WS-COLUMN3                  TO D-MV-YTD
                MOVE WS-COLUMN4                  TO D-BU-YTD
                MOVE WS-COLUMN5                  TO D-MV-LAST-YTD
                MOVE WS-COLUMN6                  TO D-MV-LAST
                MOVE WS-COLUMN7                  TO D-BU
                COMPUTE WS-MARGIN-PERC = ((WS-COLUMN3 - WS-COLUMN5)
                     / WS-COLUMN5) * 100
                MOVE WS-MARGIN-PERC TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC FROM DOUBLE-UNDER-LINE
                MOVE " " TO PRINT-REC DOUBLE-UNDER-LINE.
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
            PERFORM GET-DATA.
            MOVE WS-NEXT-NUMBER TO GL-NUMBER.
            START GL-MASTER KEY NOT < GL-KEY.
       PRR-002.
            READ GL-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLMASTER BUSY23 ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE SUB-1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR-020
               ADD 1 TO SUB-1
               GO TO PRR-001.
            IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLMASTER FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO PRR-002.
       PRR-010.
            IF LINE-CNT < 58
               GO TO PRR-020.
            ADD 1         TO PAGE-CNT.
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
            WRITE PRINT-REC FROM HEAD1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD3.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD3-1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE 6 TO LINE-CNT.
       PRR-020.
           MOVE GL-DESCRIPTION          TO D-NAME.
           MOVE GL-PER (SUB-4)          TO D-MV-PTD.
           MOVE GL-PER-BU (SUB-4)       TO D-BU-PTD.
           PERFORM COMPUTE-FIGURES.
           MOVE WS-MARGIN-MV-YTD        TO D-MV-YTD.
           MOVE WS-MARGIN-BU-YTD        TO D-BU-YTD.
           MOVE WS-MARGIN-MV-LAST-YTD   TO D-MV-LAST-YTD.
           MOVE WS-MARGIN-MV-LAST-TOTAL TO D-MV-LAST.
           MOVE WS-MARGIN-BU-TOTAL      TO D-BU.
           MOVE WS-MARGIN-PERC          TO D-PERC.

           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC DETAIL-LINE.
           MOVE 2610 TO POS.
           DISPLAY "ACCOUNT PROCESSED:" AT POS.
           ADD 20 TO POS.
           DISPLAY GL-NUMBER AT POS.
       PRR-050.
           ADD 1 TO LINE-CNT SUB-1.
           GO TO PRR-001.
       PRR-999.
           EXIT.
      *
       COMPUTE-FIGURES SECTION.
       CF-010.
           MOVE 0 TO SUB-2
                     WS-MARGIN-MV-YTD
                     WS-MARGIN-BU-YTD
                     WS-MARGIN-MV-LAST-YTD
                     WS-MARGIN-MV-LAST-TOTAL
                     WS-MARGIN-BU-TOTAL
                     WS-MARGIN-PERC.
       CF-020.
           ADD 1 TO SUB-2.
           IF SUB-2 > 12
              GO TO CF-900.
           IF SUB-2 NOT > SUB-4
               ADD GL-PER (SUB-2)         TO WS-MARGIN-MV-YTD
               ADD GL-PER-BU (SUB-2)      TO WS-MARGIN-BU-YTD
               ADD GL-LAST-PER (SUB-2)    TO WS-MARGIN-MV-LAST-YTD.
           ADD GL-LAST-PER (SUB-2)        TO WS-MARGIN-MV-LAST-TOTAL.
           ADD GL-PER-BU (SUB-2)          TO WS-MARGIN-BU-TOTAL.
           GO TO CF-020.
       CF-900.
           COMPUTE WS-MARGIN-PERC =
             ((WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST-YTD) /
               WS-MARGIN-MV-LAST-YTD) * 100.
           ADD GL-PER (SUB-4)              TO WS-COLUMN1.
           ADD GL-PER-BU (SUB-4)           TO WS-COLUMN2.
           ADD WS-MARGIN-MV-YTD            TO WS-COLUMN3.
           ADD WS-MARGIN-BU-YTD            TO WS-COLUMN4.
           ADD WS-MARGIN-MV-LAST-YTD       TO WS-COLUMN5.
           ADD WS-MARGIN-MV-LAST-TOTAL     TO WS-COLUMN6.
           ADD WS-MARGIN-BU-TOTAL          TO WS-COLUMN7.
       CF-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
              MOVE "NO GLPARAMETER RECORD READ, CALL THE SUPERVISOR."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON READ, RP-000" TO WS-MESSAGE
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
       OPEN-005.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-005.
           PERFORM READ-PARAMETER
           MOVE GLPA-NAME TO CO-NAME
           PERFORM ENTER-PERIOD-DATES.
       OPEN-006.
           MOVE 1                  TO SUB-1
           MOVE GL-BEGDATE (SUB-1) TO SPLIT-DATE
           MOVE SPLIT-YY           TO WS-DATE-SPLIT.
           MOVE WS-DATE-YY         TO H3-1BEG H3-2BEG H3-3END H3-4BEG.
           IF WS-DATE-YY > 0
              SUBTRACT 1         FROM WS-DATE-YY
           ELSE
              MOVE 99              TO WS-DATE-YY.
           MOVE WS-DATE-YY         TO H3-3BEG.

           MOVE 13               TO SUB-1
           MOVE GLPA-PER (SUB-1) TO SPLIT-DATE
           MOVE SPLIT-YY         TO WS-DATE-SPLIT.
           MOVE WS-DATE-YY       TO H3-1END H3-2END H3-4END
           PERFORM OPEN-010.
            
           CLOSE GLPARAMETER-FILE.
       OPEN-008.
           OPEN I-O GLTRANS-FILE.
           IF WS-GLTRANS-ST1 NOT = 0
               MOVE "GLTRANS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLTRANS-ST1
               GO TO OPEN-008.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-500.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

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
      * END-OF-JOB.
