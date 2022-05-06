        IDENTIFICATION DIVISION.
        PROGRAM-ID. StCounRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStMaster".
           Select RANDOM-File Assign To WS-RANDOM-FILE
               Organization Is Indexed
               Access Mode Is Dynamic
               File Status Is Ws-RANDOM-Status
               Record Key Is RANDOM-Key.
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.

       FD  RANDOM-FILE.
       01  RANDOM-REC.
           03  RANDOM-KEY.
              05  RANDOM-NUMBER     PIC 9(5).
           03  RANDOM-STOCK         PIC X(15).
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-RANDOM-FILE       PIC X(30) VALUE
              "/ctools/spl/RandomCount".
       77  WS-RANDOM-FILE-ind   PIC X(30) VALUE
              "/ctools/spl/RandomCount.Ind".
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-CAT               PIC X(3) VALUE " ".
       77  WS-TOTAL-ITEMS       PIC 9(5) VALUE 0.
       77  WS-RANDOM-PRINTED    PIC 9(5) VALUE 0.
       77  WS-NUMBER            PIC 9(5) VALUE 0.
       77  WS-RANGE-BY-DESC     PIC X VALUE " ".
       77  WS-RANGE1            PIC X(15) VALUE " ".
       77  WS-RANGE2            PIC X(15) VALUE " ".
       77  WS-PRINT-ON-HAND     PIC X VALUE " ".
       77  WS-ONLY-ON-HAND      PIC X VALUE " ".
       77  WS-CAT-NEW-PAGE      PIC X VALUE " ".
       77  WS-RANDOM            PIC X VALUE " ".
       77  WS-RANDOM-NUM        PIC X(5) VALUE " ".
       77  WS-RANGE8            PIC X VALUE " ".
       77  WS-SKIP              PIC X VALUE " ".
       77  WS-1ST               PIC X VALUE " ".
       77  WS-VALID             PIC X VALUE " ".
       77  WS-SHORTDESC         PIC X(10) VALUE " ".
       77  WS-NUM-DIS           PIC Z(4)9.
       01  WS-DESCRIPTION.
           03 WS-DESC1          PIC X(20).
           03 WS-DESC2          PIC X(20).
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1        PIC 99.
       01  WS-RANDOM-STATUS.
           03  WS-RANDOM-ST1    PIC 99.
       01  WS-SPLIT-DESC.
           03  WS-SP-1          PIC X(5) VALUE " ".
           03  WS-SP-REST       PIC X(15) VALUE " ".
       01  WS-SPLIT-INPUT-DESC.
           03  WS-SP-I-1        PIC X(5) VALUE " ".
           03  WS-SP-I-REST     PIC X(15) VALUE " ".
       01  WS-DATETIME.
           03  WS-TIMESEC    PIC 9(6).
           03  WS-DATESEC    PIC 9(8).
       01  W-RANDOM-STUFF.
           03  W-SEED        PIC 9(9) COMP-5.
           03  W-LIMIT       PIC 9(5) COMP-5.
           03  W-RANDOM      PIC 9(5) COMP-5.
           03  W-JUNK        PIC 9(5) COMP-5.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(13) VALUE " ".
           03  FILLER         PIC X(21) VALUE "S T O C K   C O U N T".
           03  FILLER         PIC X(67) VALUE "   S H E E T".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(30) VALUE " ".
           03  FILLER         PIC X(33) VALUE ALL "*".
           03  FILLER         PIC X(69) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(18) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(42) VALUE "DESCRIPTION".
           03  FILLER         PIC X(6) VALUE "UNIT".
           03  FILLER         PIC X(7) VALUE "BIN".
           03  FILLER         PIC X(13) VALUE "PHYSICAL".
           03  FILLER         PIC X(46) VALUE
           "COMPUTER QTY  STOCK NUMBER        ADJ QTY".
       01  HEAD4.
           03  FILLER         PIC X(73) VALUE " ".
           03  FILLER         PIC X(13) VALUE " COUNT".
           03  FILLER         PIC X(46) VALUE "HAND    RES".
       01  HEAD5.
           03  FILLER         PIC X(18) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(42) VALUE "DESCRIPTION".
           03  FILLER         PIC X(6) VALUE "UNIT".
           03  FILLER         PIC X(7) VALUE "BIN".
           03  FILLER         PIC X(12) VALUE "PHYSICAL".
           03  FILLER         PIC X(47) VALUE " ".
       01  HEAD6.
           03  FILLER         PIC X(73) VALUE " ".
           03  FILLER         PIC X(15) VALUE " COUNT".
           03  FILLER         PIC X(44) VALUE " ".
       01  DETAIL-LINE.
           03  D-STOCKNO      PIC X(18) VALUE " ".
           03  D-DESC1        PIC X(20) VALUE " ".
           03  D-DESC2        PIC X(22) VALUE " ".
           03  D-UNIT         PIC X(6) VALUE " ".
           03  D-BIN          PIC X(7) VALUE " ".
           03  D-LINE1        PIC X(9) VALUE " ".
           03  FILLER         PIC X(2) VALUE " ".
           03  D-QTY          PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-RES          PIC Z(5)9.
           03  FILLER         PIC X(3) VALUE " ".
           03  D-STOCKQTY     PIC X(20) VALUE " ".
           03  D-LINE2        PIC X(7) VALUE " ".
       01  RANDOM-LINE.
           03  FILLER        PIC X(21) VALUE "TOTAL ITEMS IN RANGE:".
           03  RL-NUM1       PIC Z(4)9.
           03  FILLER        PIC X(5) VALUE " ".
           03  FILLER        PIC X(21) VALUE "NO OF ITEMS TO PRINT:".
           03  RL-NUM2       PIC Z(4)9.
           03  FILLER        PIC X(5) VALUE " ".
           03  FILLER        PIC X(15) VALUE "RANGE ENTERED:".
           03  RL-RANGE1     PIC X(15).
           03  FILLER        PIC X(4) VALUE " TO ".
           03  RL-RANGE2     PIC X(15).
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 324 TO POS
           DISPLAY "** STOCK COUNT SHEETS **" AT POS
           MOVE 424 TO POS
           DISPLAY "************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           IF WS-RANDOM = "Y"
      *        PERFORM DELETE-TRANS
              PERFORM OPEN-501
              PERFORM READ-ALL-STOCK
              PERFORM COMPUTE-RANDOM
      *        PERFORM DELETE-TRANS
           ELSE
              PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO WS-RANGE1 WS-RANGE2
            MOVE 910 TO POS
            DISPLAY "SEARCH BY N=NUMBER OR D-DESCR: [ ]" AT POS
            MOVE 942 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 6         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE-BY-DESC.

            IF WS-RANGE-BY-DESC NOT = "D" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
            IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-001
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-001.
            IF WS-RANGE-BY-DESC = "N"
                GO TO GET-008.
       GET-002.
            MOVE " " TO WS-RANGE1 WS-RANGE2
            MOVE 1010 TO POS
            DISPLAY "PRINT FOR DESCRIPTION        : [               ]"
                      AT POS
            MOVE 1110 TO POS
            DISPLAY "LEAVE BLANK FOR ALL ITEMS." AT POS
            MOVE 1042 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

            IF W-ESCAPE-KEY = 4
               GO TO GET-000.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-003
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-002.
       GET-003.
            IF WS-CO-NUMBER NOT = 2
                GO TO GET-015.
            MOVE 1210 TO POS
            DISPLAY "SKIP GRC & RSC CATEGORIES    : [ ]" AT POS
            MOVE 1242 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SKIP.

            IF W-ESCAPE-KEY = 4
               GO TO GET-002.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-003.
       GET-008.
            MOVE " " TO WS-RANGE1 WS-RANGE2
            MOVE 1010 TO POS
            DISPLAY "            FROM STOCK NUMBER: [               ]"
                      AT POS
            MOVE 1042 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

            IF W-ESCAPE-KEY = 4
               GO TO GET-000.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-008.
         GET-010.
            MOVE 1210 TO POS
            DISPLAY "              TO STOCK NUMBER: [               ]"
                      AT POS
            MOVE 1242 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

            IF W-ESCAPE-KEY = 4
               GO TO GET-008.
            IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-015.
            MOVE 1410 TO POS
            DISPLAY "Print Qty On Hand YES Or NO  : [ ]" AT POS
            MOVE 1442 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-PRINT-ON-HAND.

            IF W-ESCAPE-KEY = 4
             IF WS-RANGE-BY-DESC = "N"
               GO TO GET-010
             ELSE
               GO TO GET-002.
            IF WS-PRINT-ON-HAND NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-020
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
       GET-020.
            MOVE 1610 TO POS
            DISPLAY "Print Only if OnHand > 0     : [ ]" AT POS
            MOVE 1642 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 13        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ONLY-ON-HAND.

            IF W-ESCAPE-KEY = 4
               GO TO GET-015.
            IF WS-ONLY-ON-HAND NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-030.
            MOVE 1810 TO POS
            DISPLAY "Should This Be A Random Selection [ ]" AT POS
            MOVE 1845 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 15        TO CDA-ROW.
           MOVE 44        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANDOM.

            IF W-ESCAPE-KEY = 4
               GO TO GET-020.
            IF WS-RANDOM NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
            IF WS-RANDOM = "Y"
             IF WS-RANGE-BY-DESC = "D"
                MOVE "YOU CAN ONLY DO RANDOM SELECTION FOR NUMBER ORDER"
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-030.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-035
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-035.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
            IF WS-RANDOM = "N"
               GO TO GET-040.
            MOVE 2010 TO POS
            DISPLAY "Enter How Many Random N0's to Print [     ]"
                  AT POS
            MOVE 2047 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 5         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANDOM-NUM.

            IF W-ESCAPE-KEY = 4
               GO TO GET-030.
            IF WS-RANDOM-NUM = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-035.
               
            MOVE WS-RANDOM-NUM TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-NUMBER WS-NUM-DIS
            DISPLAY WS-NUM-DIS AT POS.
              
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-040
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-035.
       GET-040.
            IF WS-RANDOM = "Y"
               GO TO GET-050.
            MOVE 2210 TO POS
            DISPLAY "Print CATEGORIES On A New Page:[ ]" AT POS
            MOVE 2242 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 19        TO CDA-ROW.
           MOVE 41        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-CAT-NEW-PAGE.

            IF W-ESCAPE-KEY = 4
             IF WS-RANDOM = "N"
               GO TO GET-030
             ELSE
               GO TO GET-035.
            IF WS-CAT-NEW-PAGE NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-050
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
       GET-050.
            MOVE 2710 TO POS
            DISPLAY "The Report Is Being Compiled, Please Be Patient."
            AT POS.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            IF WS-RANGE-BY-DESC = "N"
               MOVE WS-RANGE1 TO ST-STOCKNUMBER
            START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-RANGE-BY-DESC = "D"
               MOVE WS-RANGE1 TO ST-ALT-KEY
                                 WS-SPLIT-INPUT-DESC
            START STOCK-MASTER KEY NOT < ST-ALT-KEY
               INVALID KEY NEXT SENTENCE.
            MOVE " " TO WS-SPLIT-INPUT-DESC
                        WS-SPLIT-DESC.
            MOVE "Y" TO WS-1ST.
       PRR-002.
            READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE. 
            IF WS-STOCK-ST1 = 10
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRR-999.
            IF WS-STOCK-ST1 NOT = 0
             MOVE "STOCK BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRR-002.
            IF WS-RANGE-BY-DESC = "N"
               MOVE 2310 TO POS
               DISPLAY "Stocknumber Being Read:" AT POS
               ADD 25 TO POS
               DISPLAY ST-STOCKNUMBER AT POS
            ELSE
               MOVE 2310 TO POS
               DISPLAY "Stock Description Read:" AT POS
               ADD 25 TO POS
               MOVE ST-DESCRIPTION1 TO WS-DESC1
               MOVE ST-DESCRIPTION2 TO WS-DESC2
               DISPLAY WS-DESCRIPTION AT POS.
            
            IF WS-RANGE-BY-DESC = "D"
             IF WS-SKIP = "Y"
              IF ST-CATEGORY = "GRC" OR = "RSC"
               GO TO PRR-002.

            IF WS-RANGE-BY-DESC = "D"
             IF WS-RANGE1 NOT = " "
             IF WS-1ST = "Y"
              IF ST-DESCRIPTION1 NOT = " "
                MOVE ST-DESCRIPTION1 TO WS-SPLIT-INPUT-DESC
                MOVE "N" TO WS-1ST.
           IF WS-RANGE-BY-DESC = "D"
            IF WS-RANGE1 NOT = " "
               MOVE ST-DESCRIPTION1 TO WS-SPLIT-DESC
             IF WS-SP-1 NOT = WS-SP-I-1
               GO TO PRR-999.
            
           IF WS-RANGE-BY-DESC = "N"
            IF ST-STOCKNUMBER < WS-RANGE1
               GO TO PRR-002.
           IF WS-RANGE-BY-DESC = "N"
            IF ST-STOCKNUMBER > WS-RANGE2
               GO TO PRR-999.
            IF WS-ONLY-ON-HAND = "Y"
             IF ST-QTYONHAND + ST-QTYONRESERVE NOT > 0
               GO TO PRR-002.
           IF WS-RANGE-BY-DESC = "N"
            IF WS-CAT = "   "
               MOVE ST-CATEGORY TO WS-CAT.
           IF WS-RANGE-BY-DESC = "N"
            IF ST-CATEGORY NOT = WS-CAT
               MOVE ST-CATEGORY TO WS-CAT
             IF WS-CAT-NEW-PAGE = "Y"
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               MOVE "** END OF CATEGORY **" TO PRINT-REC
               WRITE PRINT-REC
               PERFORM PRR-010
             ELSE
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               MOVE "** END OF CATEGORY **" TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               WRITE PRINT-REC
               ADD 3 TO LINE-CNT.
            IF LINE-CNT < 60
               GO TO PRR-020.
       PRR-010.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE
            MOVE " "      TO PRINT-REC

           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
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
            WRITE PRINT-REC.
            IF WS-PRINT-ON-HAND = "Y"
               WRITE PRINT-REC FROM HEAD3
            ELSE
               WRITE PRINT-REC FROM HEAD5.
            MOVE " " TO PRINT-REC.
            IF WS-PRINT-ON-HAND = "Y"
               WRITE PRINT-REC FROM HEAD4
            ELSE
               WRITE PRINT-REC FROM HEAD6.
            MOVE " " TO PRINT-REC
            MOVE 7 TO LINE-CNT.
       PRR-020.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE ST-STOCKNUMBER     TO D-STOCKNO
           MOVE ST-DESCRIPTION1    TO D-DESC1
           MOVE ST-DESCRIPTION2    TO D-DESC2
           MOVE ST-UNITOFMEASURE   TO D-UNIT
           MOVE ST-BINLOCATION     TO D-BIN
           MOVE "_________"        TO D-LINE1
           MOVE "_______"          TO D-LINE2.
           IF WS-PRINT-ON-HAND = "Y"
              MOVE ST-QTYONHAND    TO D-QTY
              MOVE ST-QTYONRESERVE TO D-RES
              MOVE ST-STOCKNUMBER  TO D-STOCKQTY.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           GO TO PRR-002.
       PRR-999.
           EXIT.
      *
       PRINT-RANDOM-ROUTINE SECTION.
       PRSR-002.
            READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE. 
            IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRSR-999.
            IF WS-STOCK-ST1 NOT = 0
            MOVE "STOCK BUSY ON READ PRSR-002, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO PRSR-002.
            MOVE 2510 TO POS
            DISPLAY "Random Stock Being Read:" AT POS
            ADD 25 TO POS
            DISPLAY ST-STOCKNUMBER AT POS.
            IF WS-ONLY-ON-HAND = "Y"
             IF ST-QTYONHAND + ST-QTYONRESERVE NOT > 0
               MOVE "N" TO WS-VALID
               GO TO PRSR-999.
            IF LINE-CNT < 60
               GO TO PRSR-020.
       PRSR-010.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE
            MOVE " "      TO PRINT-REC
            WRITE PRINT-REC.

           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
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
            WRITE PRINT-REC.
            IF WS-PRINT-ON-HAND = "Y"
               WRITE PRINT-REC FROM HEAD3
            ELSE
               WRITE PRINT-REC FROM HEAD5.
            MOVE " " TO PRINT-REC.
            IF WS-PRINT-ON-HAND = "Y"
               WRITE PRINT-REC FROM HEAD4
            ELSE
               WRITE PRINT-REC FROM HEAD6.
            MOVE " " TO PRINT-REC
            MOVE 7 TO LINE-CNT.
       PRSR-020.
           MOVE " " TO PRINT-REC
                       DETAIL-LINE.
           MOVE ST-STOCKNUMBER     TO D-STOCKNO
           MOVE ST-DESCRIPTION1    TO D-DESC1
           MOVE ST-DESCRIPTION2    TO D-DESC2
           MOVE ST-UNITOFMEASURE   TO D-UNIT
           MOVE ST-BINLOCATION     TO D-BIN
           MOVE "_________"        TO D-LINE1
           MOVE "_______"          TO D-LINE2.
           IF WS-PRINT-ON-HAND = "Y"
              MOVE ST-QTYONHAND    TO D-QTY
              MOVE ST-QTYONRESERVE TO D-RES
              MOVE ST-STOCKNUMBER  TO D-STOCKQTY.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           MOVE "Y" TO WS-VALID
           ADD 1 TO LINE-CNT.
       PRSR-999.
           EXIT.
      *
       COMPUTE-RANDOM SECTION.
       CRAN-005.
           MOVE 2310 TO POS
           DISPLAY "Random computation and Read....." AT POS.
           PERFORM OPEN-500.
           MOVE WS-DATETIME TO W-SEED.
       CRAN-010.
           COMPUTE W-SEED = W-SEED * 134775813 + 1.
           DIVIDE W-SEED BY 65536 GIVING W-RANDOM.
           DIVIDE W-RANDOM BY W-LIMIT
                GIVING W-JUNK
                 REMAINDER W-RANDOM.
           MOVE W-RANDOM TO RANDOM-NUMBER.
            
           IF W-RANDOM > W-LIMIT
               GO TO CRAN-010.
            
           PERFORM READ-RANDOM-RECORD.
           IF WS-RANDOM-ST1 = 0
              PERFORM PRINT-RANDOM-ROUTINE
            IF WS-VALID NOT = "N"
              ADD 1 TO WS-RANDOM-PRINTED.
           ACCEPT WS-TIME FROM TIME
           MOVE WS-TIME TO WS-TIMESEC
           ADD WS-TIMESEC TO W-SEED.
           IF WS-NUMBER > WS-RANDOM-PRINTED
              GO TO CRAN-010.
       CRAN-999.
            EXIT.
      *
       READ-RANDOM-RECORD SECTION.
       RRR-002.
            READ RANDOM-FILE
               INVALID KEY NEXT SENTENCE. 
            IF WS-RANDOM-ST1 = 23 OR 35 OR 49
               MOVE "NO SUCH RANDOM RECORD ON READ-23." TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE RANDOM-REC TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RRR-999.
            IF WS-RANDOM-ST1 NOT = 0
               MOVE "RANDOM RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE RANDOM-REC TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-RANDOM-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RRR-002.
            MOVE 2510 TO POS
            DISPLAY "Random Stock Being Read:" AT POS
            ADD 25 TO POS
            DISPLAY RANDOM-STOCK AT POS.
            MOVE RANDOM-STOCK TO ST-STOCKNUMBER.
       RRR-999.
            EXIT.
      *
       WRITE-RANDOM-RECORD SECTION.
       WRR-005.
           MOVE WS-TOTAL-ITEMS TO RANDOM-NUMBER
           MOVE ST-STOCKNUMBER TO RANDOM-STOCK.
           WRITE RANDOM-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "RANDOM RECORD INVALID ON WRITE" TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
       WRR-999.
            EXIT.
      *
       READ-ALL-STOCK SECTION.
       RAS-000.
            MOVE 2310 TO POS
            DISPLAY "Reading ALL Items In Range......" AT POS.
            MOVE WS-RANGE1 TO ST-STOCKNUMBER.
               START STOCK-MASTER KEY NOT < ST-KEY
                  INVALID KEY NEXT SENTENCE.
       RAS-001.
           IF WS-RANGE8 = "Y"
              START STOCK-MASTER KEY < ST-SALESRANDSYTD.
           IF WS-STOCK-ST1 NOT = 0
              GO TO RAS-900.
       RAS-002.
            READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE. 
            IF WS-STOCK-ST1 = 10
               MOVE 0 TO WS-STOCK-ST1
               GO TO RAS-900.
            IF WS-STOCK-ST1 NOT = 0
             MOVE "STOCK BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO RAS-002.
            MOVE 2510 TO POS
            DISPLAY "Stocknumber Being Read:" AT POS
            ADD 25 TO POS
            DISPLAY ST-STOCKNUMBER AT POS.
            IF ST-STOCKNUMBER < WS-RANGE1
               GO TO RAS-002.
            IF ST-STOCKNUMBER > WS-RANGE2
               GO TO RAS-900.
           ADD 1 TO WS-TOTAL-ITEMS
           PERFORM WRITE-RANDOM-RECORD.
           IF WS-RANGE8 = "Y"
              GO TO RAS-001
           ELSE
              GO TO RAS-002.
       RAS-900.
           CLOSE RANDOM-FILE.
           MOVE WS-TOTAL-ITEMS TO W-LIMIT.
           PERFORM ERROR-020
           MOVE 2510 TO POS
           DISPLAY WS-MESSAGE AT POS.
       RAS-999.
           EXIT.
      *
       CHECK-DATA-SIZE SECTION.         
       CDS-005.
           Move " " To Alpha-Rate.
           Move 0   To Sub-1.
       CDS-015.
           Add 1 To Sub-1.
           If Al-Rate (Sub-1) Not = " "
            If Sub-1 Not > 60
            Go To CDS-015.
          Subtract 1 from Sub-1.
       CDS-999.
          EXIT.
      *
       DELETE-TRANS SECTION.
       DST-010.
           CLOSE RANDOM-FILE.
           PERFORM CDS-005.
           Move Ws-Random-file To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE WS-random-file   TO F-FILENAME
           MOVE SUB-1            TO F-CBFILENAME.
           CALL "OPENFILE" USING   F-ERROR1
                                   F-FH
                                   F-FILENAME
                                   F-CBFILENAME
                                   F-FILENAME
                                   F-INTEGERZERO
                                   F-OPENMODE-MM.
           CALL "DELETEFILE" USING F-ERROR1
                                   F-FH.
              
           PERFORM CDS-005.
           Move Ws-Random-file-IND To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE Ws-Random-file-Ind TO F-FILENAME
           MOVE Sub-1              TO F-CBFILENAME.
           CALL "OPENFILE" USING     F-ERROR1
                                     F-FH
                                     F-FILENAME
                                     F-CBFILENAME
                                     F-FILENAME
                                     F-INTEGERZERO
                                     F-OPENMODE-MM.
           CALL "DELETEFILE" USING   F-ERROR1
                                     F-FH.
       DST-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0 
              MOVE 0 TO WS-STOCK-ST1
              MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
           GO TO OPEN-020.
       OPEN-500.
           OPEN I-O RANDOM-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE 0 TO WS-RANDOM-ST1
              MOVE
             "RANDOM OPEN I-O AT ANOTHER COMPUTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-500.
       OPEN-501.
           OPEN OUTPUT RANDOM-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE 0 TO WS-RANDOM-ST1
              MOVE
             "RANDOM OPEN OUTPUT AT ANOTHER COMPUTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-501.
        OPEN-020.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE WS-DATESEC
           
           ACCEPT WS-TIME FROM TIME
           MOVE WS-TIME TO WS-TIMESEC
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
        OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            IF WS-ONLY-ON-HAND = "Y"
                MOVE " " TO PRINT-REC
                MOVE
             "*** ONLY ITEMS WHERE ON-HAND > 0 HAVE BEEN PRINTED. ***"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 1.
            IF WS-RANDOM = "Y"
                MOVE " " TO PRINT-REC
                MOVE "*** RANDOM ITEMS ONLY HAVE BEEN PRINTED. ***"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 2.
            IF WS-RANGE8 = "Y"
                MOVE " " TO PRINT-REC
                MOVE "*** ONLY HIGH SALES ITEMS PRINTED. ***"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 1.
            IF WS-RANDOM = "Y"
                MOVE WS-RANGE1 TO RL-RANGE1
                MOVE WS-RANGE2 TO RL-RANGE2
                MOVE W-LIMIT   TO RL-NUM1
                MOVE WS-NUMBER TO RL-NUM2
                WRITE PRINT-REC FROM RANDOM-LINE AFTER 1.
            IF WS-RANGE-BY-DESC = "N"
                MOVE " " TO PRINT-REC
                MOVE "*** STOCK PRINTED IN NUMBER ORDER ***"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 2
             ELSE
                MOVE " " TO PRINT-REC
                MOVE "*** STOCK PRINTED BY DESCRIPTION ***"
                TO PRINT-REC
                WRITE PRINT-REC AFTER 2.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE STOCK-MASTER
                 PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
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
      *
      * END-OF-JOB.
