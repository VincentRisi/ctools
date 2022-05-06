        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlRandom.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
      *
        DATA DIVISION.
        FILE SECTION.
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-DUPLICATE         PIC X VALUE " ".
       77  WS-TYPE-SEL          PIC X VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  WS-FOUND             PIC 9 VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-TOTAL-ITEMS       PIC 9(5) VALUE 0.
       77  WS-RANDOM-NUMBER     PIC 9(5) VALUE 0.
       77  WS-UPPER-NUMBER      PIC 9(5) VALUE 0.
       77  WS-NUMBER            PIC 9(5) VALUE 0.
       77  WS-NUMBER-DISPLAY    PIC Z(5).
       77  WS-RANGE-BY-DESC     PIC X VALUE " ".
       77  WS-RANGE1            PIC X(2) VALUE " ".
       77  WS-RANGE2            PIC X(2) VALUE " ".
       77  WS-SKIP              PIC X VALUE " ".
       77  WS-1ST               PIC X VALUE " ".
       77  WS-VALID             PIC X VALUE " ".
       77  WS-NUM-ACCEPT        PIC X(14) VALUE " ".
       01  WS-DATETIME.
           03  WS-TIMESEC    PIC 9(6).
           03  WS-DATESEC    PIC 9(8).
       01  W-RANDOM-STUFF.
           03  W-SEED        PIC 9(9) COMP-5.
           03  W-LIMIT       PIC 9(5) COMP-5.
           03  W-RANDOM      PIC 9(5) COMP-5.
           03  W-JUNK        PIC 9(5) COMP-5.
       01  WS-NUMBERS-SHOWN.
           03  WS-NS          PIC 99 OCCURS 99.
       01  WS-NUMBERS-CHECK.
           03  WS-NC          PIC 99 OCCURS 7.
       01  WS-NUMBER-NAMES.
         02  WS-NUMBERS-GOT OCCURS 7.
           03  WS-NG          PIC 99.
           03  WS-CHECK       PIC X VALUE " ".
           
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
           DISPLAY "** RANDOM NUMBER GENERATOR **" AT POS
           MOVE 424 TO POS
           DISPLAY "*****************************" AT POS.
       CONTROL-010.
           PERFORM OPEN-FILES.
       CONTROL-020.
           PERFORM CHECK-SELECTION.
           IF WS-TYPE-SEL = "N"
              PERFORM GET-DATA
              PERFORM COMPUTE-RANDOM
              GO TO CONTROL-000.
           PERFORM ENTER-NUM-TO-CHECK
           MOVE 6  TO WS-RANDOM-NUMBER
           MOVE 50 TO W-LIMIT WS-UPPER-NUMBER.
       CONTROL-025.
           PERFORM COMPUTE-RANDOM.
           PERFORM CHECK-IF-NUM-SAME.
           IF W-ESCAPE-KEY = 3
               GO TO CONTROL-000.
           GO TO CONTROL-025.
       CONTROL-030.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       CHECK-IF-NUM-SAME SECTION.
       CINS-005.
           MOVE 0 TO SUB-1 SUB-2 WS-FOUND.
       CINS-010.
           ADD 1 TO SUB-1.
       CINS-015.
           ADD 1 TO SUB-2.
           IF WS-NS (SUB-1) = WS-NG (SUB-2)
              MOVE "Y" TO WS-CHECK (SUB-2)
              ADD 1 TO WS-FOUND.
           IF SUB-2 < 7
              GO TO CINS-015.
              
           IF SUB-1 < 6
              PERFORM CINS-010
              MOVE 0 TO SUB-2
              GO TO CINS-015.
       CINS-020.
           MOVE 2210 TO POS
           DISPLAY "# OF HITS      :" AT POS
           ADD 21 TO POS
           DISPLAY WS-FOUND AT POS.
           
           ADD 1 TO WS-TOTAL-ITEMS
           MOVE 2310 TO POS
           DISPLAY "NUMBER OF TRIES:" AT POS
           ADD 17 TO POS
           DISPLAY WS-TOTAL-ITEMS AT POS.
           
           IF WS-FOUND > 3
              MOVE 3010 TO POS
              DISPLAY 
              "HIGH NUMBER OF HITS FOUND, PRESS A KEY TO CONTINUE."
               AT POS
              ADD 55 TO POS
              DISPLAY " " AT 3079 WITH BELL
              ACCEPT WS-ACCEPT AT POS
              PERFORM ERROR-020.
       CINS-999.
           EXIT.
      *
       CHECK-SELECTION SECTION.
       CSS-010.
           MOVE "N" TO WS-TYPE-SEL.
           MOVE 710 TO POS
           DISPLAY 
           "ENTER N=NEW RANDOM NUMBERS, C=CHECK OLD SEQUENCE:[ ]" AT POS
           ADD 50 TO POS

           MOVE 'N'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 4         TO CDA-ROW.
           MOVE 59        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TYPE-SEL.

           IF W-ESCAPE-KEY = 3
               GO TO CONTROL-030.
           IF WS-TYPE-SEL NOT = "C" AND NOT = "N"
               MOVE "YOU MUST ENTER EITHER C OR N, PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO CSS-010.
       CSS-999.
           EXIT.
      *
       ENTER-NUM-TO-CHECK SECTION.
       ENTC-010.
           MOVE 910 TO POS
           DISPLAY
           "ENTER SEVEN NUMBERS IN 2 DIGIT FORMAT [              ]"
            AT POS
           ADD 39 TO POS

           MOVE WS-NUM-ACCEPT TO CDA-DATA.
           MOVE 14        TO CDA-DATALEN.
           MOVE 6         TO CDA-ROW.
           MOVE 48        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-NUM-ACCEPT.

           IF WS-NUM-ACCEPT NOT > " "
               MOVE "YOU MUST ENTER 7 NUMBERS, PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO ENTC-010.
           MOVE WS-NUM-ACCEPT TO WS-NUMBERS-CHECK
           MOVE 0 TO SUB-2.
           MOVE 1010 TO POS
           DISPLAY "NUMBERS ENTERED ARE:" AT POS
           ADD 18 TO POS.
       ENTC-020.
           ADD 1 TO SUB-2
           ADD 3 TO POS
           DISPLAY WS-NC (SUB-2) AT POS.
           IF SUB-2 < 7
             GO TO ENTC-020.
           MOVE 0 TO SUB-2.
       ENTC-030.
           ADD 1 TO SUB-2
           MOVE WS-NC (SUB-2) TO WS-NG (SUB-2)
           MOVE " " TO WS-CHECK (SUB-2).
           IF SUB-2 < 7
             GO TO ENTC-030.
       ENTC-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
      *      MOVE " " TO WS-RANGE1 WS-RANGE2
            MOVE 910 TO POS
            DISPLAY
            "ENTER THE NUMBER OF RANDOM NUMBERS TO BE GENERATED: [  ]"
              AT POS
            ADD 53 TO POS

           MOVE WS-RANGE1 TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 6         TO CDA-ROW.
           MOVE 62        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

            MOVE WS-RANGE1 TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-RANDOM-NUMBER.
            IF W-ESCAPE-KEY = 3
               GO TO CONTROL-030.
            IF WS-RANGE1 NOT > " "
               MOVE " YOU MUST ENTER A NUMBER > ZERO." TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-000.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-005
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-005.
           MOVE 1110 TO POS
           DISPLAY
           "ENTER THE UPPER LIMIT OF THE RANDOM NUMBER TO BE USED: [  ]"
             AT POS
           ADD 56 TO POS

           MOVE WS-RANGE2 TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 8         TO CDA-ROW.
           MOVE 65        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

           MOVE WS-RANGE2 TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-UPPER-NUMBER.
           MOVE WS-UPPER-NUMBER TO W-LIMIT.
           IF W-ESCAPE-KEY = 4
               GO TO GET-000.
            IF WS-RANGE2 NOT > " "
               MOVE " YOU MUST ENTER A NUMBER > ZERO." TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-005.
            IF W-ESCAPE-KEY = 3
               GO TO CONTROL-999.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-999
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-005.
       GET-999.
           EXIT.
      *
       COMPUTE-RANDOM SECTION.
       CRAN-005.
           PERFORM CLEAR-NUMBERS.
           MOVE 0 TO WS-NUMBER W-SEED.
           
           MOVE WS-DATETIME TO W-SEED.
           COMPUTE W-SEED = W-SEED + WS-SEC.
           MOVE 2510 TO POS
           DISPLAY "RANDOM NUMBER  :" AT POS
           ADD 17 TO POS.
           MOVE 0 TO SUB-1.
       CRAN-010.
      *     COMPUTE W-SEED = (W-SEED * 134775813) + 1.
      *     DIVIDE W-SEED BY 217 GIVING W-RANDOM.
           COMPUTE W-SEED = (W-SEED * 19580517) + 1.
           DIVIDE W-SEED BY 143 GIVING W-RANDOM.
           DIVIDE W-RANDOM BY W-LIMIT
                GIVING W-JUNK
                REMAINDER W-RANDOM.
                
           MOVE W-RANDOM TO WS-NUMBER-DISPLAY.
           DISPLAY WS-NUMBER-DISPLAY AT POS.
            
           IF W-RANDOM > W-LIMIT
               GO TO CRAN-010.
           IF W-RANDOM < 1
               GO TO CRAN-010.
           PERFORM CHECK-FOR-DUPLICATE-NUM.
           IF WS-DUPLICATE = "Y"
               GO TO CRAN-010.
            
           ADD 1 TO WS-NUMBER.
           ACCEPT WS-TIME FROM TIME
           MOVE WS-TIME   TO WS-TIMESEC
           ADD WS-TIMESEC TO  W-SEED.
           IF WS-NUMBER = WS-RANDOM-NUMBER
            IF WS-TYPE-SEL = "N"
              MOVE 3010 TO POS
              DISPLAY "PRESS A KEY TO EXIT AND RE-RUN" AT POS
              ADD 40 TO POS
              ACCEPT WS-ACCEPT AT POS
              GO TO CRAN-999
            ELSE
              GO TO CRAN-999.
              
           ADD 5 TO POS.
           GO TO CRAN-010.
       CRAN-999.
           EXIT.
      * 
       CHECK-FOR-DUPLICATE-NUM SECTION.
       CP-001.
           MOVE 0 TO SUB-2.
           MOVE "N" TO WS-DUPLICATE.
       CP-005.
           ADD 1 TO SUB-2.
           IF WS-NS (SUB-2) NOT = 0
            IF WS-NS (SUB-2) = W-RANDOM
               MOVE "Y" TO WS-DUPLICATE
               GO TO CP-999
            ELSE
               GO TO CP-005.
           IF WS-NS (SUB-2) = 0
              MOVE W-RANDOM TO WS-NS (SUB-2).
       CP-999.
           EXIT.
      * 
       CLEAR-NUMBERS SECTION.
       CN-001.
           MOVE 0 TO SUB-2.
           MOVE "N" TO WS-DUPLICATE.
       CN-005.
           ADD 1 TO SUB-2.
           IF WS-NS (SUB-2) NOT = 0
               MOVE 0 TO WS-NS (SUB-2)
               GO TO CN-005.
       CN-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-020.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE WS-DATESEC
           
           ACCEPT WS-TIME FROM TIME
           MOVE WS-TIME TO WS-TIMESEC
           PERFORM CONVERT-DATE-FORMAT.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-900.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "GetSystemY2KDate".
       Copy "GetUserMailName".
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
