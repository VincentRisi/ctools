        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlSBChMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectSlRegister".
         Copy "SelectSlSoldBy".
         Copy "SelectSlDaily".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdSoldBy.
           COPY ChlfdRegister.
           COPY ChlfdDaily.
           
       WORKING-STORAGE SECTION.
       77  NEW-SOLDBY          PIC X VALUE " ".      
       77  WS-END              PIC X VALUE " ".      
       77  WS-ACCEPT           PIC X VALUE " ".      
       77  WS-TYPE             PIC X VALUE " ".      
       77  WS-NUMBER           PIC X(6) VALUE " ".      
       77  WS-NUMBER-DIS       PIC Z(5)9.      
       77  WS-ANSWER2          PIC X VALUE " ".      
       77  WS-VALID            PIC X VALUE " ".      
       77  WS-NUM              PIC 9(6) VALUE 0.
       77  WS-SOLDBY-NUM       PIC XX VALUE " ".
       77  WS-NEWSOLDBY-NUM    PIC XX VALUE " ".
       77  WS-OLDSOLDBY-NUM    PIC XX VALUE " ".
       77  WS-TERM-SUB          PIC 9 VALUE 0.    
       01  SPLIT-TERMOFSALE.
           03  WSTE-CODE        PIC X VALUE " ".
           03  WSTE-REST        PIC X(10) VALUE " ".
       01  STORE-TERM.
         02  WS-TERM-OCCUR OCCURS 10.
           03  WS-ST-TYPE       PIC X.
           03  WS-ST-CODE       PIC X.
           03  WS-ST-TERM       PIC X(11).
       01  WS-SOLDBY-STATUS.
           03  WS-SOLDBY-ST1    PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1     PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
       CONTROL-010.
           PERFORM CLEAR-SCREEN.
           PERFORM GET-DATA.
           IF W-ESCAPE-KEY = 3
              PERFORM END-000
              GO TO CONTROL-000.
           PERFORM READ-REGISTER-RECORDS.
           PERFORM END-000.
           GO TO CONTROL-000.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE 0310 TO POS
           DISPLAY "** CHANGE SOLDBY INITIALS **" AT POS
           MOVE 0410 TO POS
           DISPLAY "****************************" AT POS.
       GET-010.
           MOVE 0610 TO POS
           DISPLAY "ENTER OLD SOLDBY INITIAL : [  ]" AT POS
           ADD 28 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 3         TO CDA-ROW.
           MOVE 37        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-OLDSOLDBY-NUM.

      *     ACCEPT WS-OLDSOLDBY-NUM AT POS.
           IF W-ESCAPE-KEY = 3
               PERFORM END-OFF.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-015.
           MOVE 0910 TO POS
           DISPLAY "ENTER NEW SOLDBY INITIAL : [  ]" AT POS
           ADD 28 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 6         TO CDA-ROW.
           MOVE 37        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-NEWSOLDBY-NUM.

      *     ACCEPT WS-NEWSOLDBY-NUM AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
       GET-030.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
           MOVE 1310 TO POS
           DISPLAY "ENTER 1=INVOICE, 4=P/SLIP, : [ ]" AT POS
           MOVE 1340 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 39        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TYPE.

      *     ACCEPT WS-TYPE AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-015.
           IF WS-TYPE NOT = "1" AND NOT = "4"
               MOVE "ONLY 1 & 4,  ARE VALID ENTRIES, RE-ENTER." 
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-030.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-040.
           MOVE 1510 TO POS
           DISPLAY "ENTER THE NUMBER OF THE RECORD : [      ]" AT POS
           ADD 34 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 6         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 43        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-NUMBER.

      *     ACCEPT WS-NUMBER AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-030.
           MOVE WS-NUMBER TO ALPHA-RATE
           PERFORM DECIMALISE-RATE.
           IF NUMERIC-RATE NOT > 0
               MOVE "ONLY A POSITIVE NUMBER MAY BE ENTERED, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-040.
           MOVE NUMERIC-RATE TO WS-NUM 
           MOVE WS-NUM TO WS-NUMBER-DIS
           DISPLAY WS-NUMBER-DIS AT POS.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-500
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-040.
       GET-500.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
           MOVE 2010 TO POS
           DISPLAY "PRESS <ENTER> TO CONTINUE, <END> TO ABORT."
            AT POS
           ADD 45 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 56        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

      *     ACCEPT WS-ACCEPT AT POS.
           IF W-ESCAPE-KEY NOT = 3 AND NOT = 1
               GO TO GET-500.
       GET-999.
            EXIT.
      *
       READ-REGISTER-RECORDS SECTION.
       RRR-000.
            MOVE 2910 TO POS.
            DISPLAY "CHANGING REGISTER RECORDS TO NEW SOLDBY INITIAL. "
               AT POS.

            MOVE WS-NUM  TO INCR-INVOICE
            MOVE WS-TYPE TO INCR-TRANS.
            START INCR-REGISTER KEY = INCR-KEY
                 INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 NOT = 0
               MOVE "THERE ARE NO REGISTER RECORDS FOR THAT ACCOUNT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "N" TO WS-VALID
               GO TO RRR-999.
       RRR-002.
            READ INCR-REGISTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 = 23 OR 35 OR 49
               GO TO RRR-999.
            IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RRR-002.
       RRR-010.
            MOVE WS-NEWSOLDBY-NUM          TO INCR-SB-TYPE.
            REWRITE INCR-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 NOT = 0
               MOVE INCR-KEY               TO WS-DAILY-1ST
               MOVE INCR-ALT-KEY           TO WS-DAILY-2ND
               MOVE "NO CHNGE TO REGISTER" TO WS-DAILY-3RD
               MOVE WS-NEWSOLDBY-NUM       TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.

            IF INCR-TRANS = 4
               GO TO RRR-999.
            PERFORM CHANGE-SOLDBY.
            MOVE WS-NEWSOLDBY-NUM TO SB-TYPE.
            PERFORM REWRITE-NEW-SOLDBY.
       RRR-999.
           EXIT.
      *
       CHANGE-SOLDBY SECTION.
       R-SB-000.
             MOVE INCR-INVOICE     TO SB-INVOICE-NUMBER
             MOVE WS-OLDSOLDBY-NUM TO SB-TYPE.
             
             START SOLD-BY KEY NOT < SB-KEY
                 INVALID KEY NEXT SENTENCE.
       R-SB-010.
             READ SOLD-BY WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-SOLDBY-ST1 = 23 OR 35 OR 49
                MOVE "SOLDBY RECORD DOES NOT EXIST,'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO R-SB-999.
             IF WS-SOLDBY-ST1 NOT = 0
                MOVE "SOLDBY RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-SOLDBY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-SOLDBY-ST1
                GO TO R-SB-010.
       R-SB-030.
             DELETE SOLD-BY
                INVALID KEY NEXT SENTENCE.
             IF WS-SOLDBY-ST1 NOT = 0
                MOVE "SOLDBY RECORD BUSY ON DELETE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-SOLDBY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-SOLDBY-ST1
                GO TO R-SB-030.
       R-SB-999.
             EXIT.
      *
       REWRITE-NEW-SOLDBY SECTION.
       UP-SB-030.
             WRITE SOLDBY-REC
                 INVALID KEY NEXT SENTENCE.
             IF WS-SOLDBY-ST1 = 23 OR 35 OR 49
                MOVE "SOLDBY FILE ERROR ON WRITE-23, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-SOLDBY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-SOLDBY-ST1
                GO TO UP-SB-999.
             IF WS-SOLDBY-ST1 NOT = 0
                MOVE "SOLDBY FILE BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-SOLDBY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-SOLDBY-ST1
                GO TO UP-SB-030.
       UP-SB-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O SOLD-BY.
            IF WS-SOLDBY-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-SOLDBY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-SOLDBY-ST1
                GO TO OPEN-000.
       OPEN-008.
            OPEN I-O INCR-REGISTER.
            IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-INCR-ST1
               GO TO OPEN-008.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE SOLD-BY
                 INCR-REGISTER.
       END-900.
      *      STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
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
       Copy "CTOSCobolAccept".
       Copy "WriteDailyExcep1".
      * END-OF-JOB
