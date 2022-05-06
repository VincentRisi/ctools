        IDENTIFICATION DIVISION.
        PROGRAM-ID. OFISXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT OFIS-FILE ASSIGN TO "OfisData"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS OFIS-KEY
               ALTERNATE RECORD KEY IS OFIS-AFFILIATION WITH DUPLICATES
               ALTERNATE RECORD KEY IS OFIS-CATEGORY WITH DUPLICATES
               FILE STATUS IS WS-OFIS-STATUS.
           SELECT OFIS-ASCII ASSIGN TO "OfisDataASCII"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OFIS-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdOfis.
           COPY ChlfdOfisASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  SUB-1         PIC 9(4) VALUE 0.
           77  LINE-CNT      PIC 9(6) VALUE 0.
           77  WS-COUNT      PIC 9(6) VALUE 0.
           77  WS-MESSAGE    PIC X(60) VALUE " ".
           01  F-FIELDNAME   PIC X(20).
           01  WS-TEST1.
               03  WS-TEST1-80CHAR.
                 04  WS-TEST1-NAME-TITLE.
                   05  WS-TEST1-LAST     PIC X(14).
                   05  WS-TEST1-LAST2    PIC X(3).
                 04  WS-TEST1-LINE.
                   05  WS-TEST1-NAME     PIC X(25).
      *             05  FILLER            PIC X(2).
                   05  WS-TEST1-FIRST    PIC X(7).
                   05  WS-TEST1-F-NAME   PIC X(25).
               03  FILLER                PIC X(6).
           01  WS-TEST2.
               03  WS-TEST2-80CHAR.
                 04  WS-TEST2-NAME-TITLE.
                   05  WS-TEST2-LAST     PIC X(14).
                 04  WS-TEST2-LINE.
                   05  WS-TEST2-NAME     PIC X(25).
                   05  FILLER            PIC X(2).
                   05  WS-TEST2-FIRST    PIC X(10).
                   05  WS-TEST2-F-NAME   PIC X(25).
               03  FILLER                PIC X(3).
           01  WS-OFIS-STATUS.
               03  WS-STAT1  PIC 99.
      *
        PROCEDURE DIVISION.
        CONTROL-PARAGRAPH SECTION.
           PERFORM A-ACCEPT.
           PERFORM A-INIT.
           IF WS-ACCEPT = "E"
               PERFORM B-EXPORT
           ELSE
               MOVE 0 TO LINE-CNT
               PERFORM B-IMPORT.
          PERFORM C-END.
           STOP RUN.
        CONTROL-000.
           EXIT. 
      *
       A-ACCEPT SECTION.
       A-001.
           MOVE 0810 TO POS.
           DISPLAY "** OFIS EXPORT / IMPORT OF DATA **" AT POS
           MOVE 1010 TO POS
           DISPLAY "ENTER E=EXPORT TO ASCII, I=IMPORT FROM ASCII: [ ]"
              AT POS
           MOVE 1057 TO POS
           ACCEPT WS-ACCEPT AT POS.
           IF WS-ACCEPT NOT = "E" AND NOT = "I"
              GO TO A-001.
        A-AC-EXIT.
           EXIT.
      *
        A-INIT SECTION.
        A-000.
           OPEN OUTPUT OFIS-FILE.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
              MOVE " " TO OFIS-KEY
              START OFIS-FILE KEY NOT < OFIS-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND OFIS-ASCII
           ELSE
              OPEN INPUT OFIS-ASCII.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
            
            IF WS-STAT1 NOT = 0
               MOVE "EXCLUDING IMPORT FOR THIS COMPANY" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM C-END
               STOP RUN.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ OFIS-FILE NEXT
               AT END 
               DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY OFIS-KEY.
           
           ADD 1 TO WS-COUNT.

           MOVE OFIS-RECORD    TO ASCII-RECORD.
        BE-010.
      *     WRITE ASCII-RECORD
      *           INVALID KEY
             DISPLAY "INVALID WRITE FOR ASCII FILE...."
             DISPLAY WS-STAT1
             STOP RUN.
             GO TO BE-005.
        BE-EXIT.
           EXIT.
      *
        B-IMPORT SECTION.
        BI-005.
           MOVE SPACES TO WS-TEST1.
           
           READ OFIS-ASCII NEXT
               AT END
               PERFORM BI-050
             GO TO BI-EXIT.
             
           DISPLAY "[" AT 1504
           DISPLAY ASCII-MESSAGE AT 1505.
           
           ADD 1 TO LINE-CNT
           DISPLAY "LINE-CNT:" AT 2521
           DISPLAY LINE-CNT AT 2530.
           
      *     MOVE "NEW LINE READ" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
           MOVE ASCII-MESSAGE TO WS-TEST1 WS-TEST2.
           
      *     ACCEPT WS-ACCEPT AT 2650.
        BI-010.
      *    IF WS-TEST1 = SPACES
            IF LINE-CNT = 1 OR 2 OR 3 OR = 7 OR = 12 OR = 17
      *         MOVE "LINE CNT = 1 OR 2 OR 3 OR 7 OR 12.." TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               GO TO BI-005.

      *     MOVE "GOT PAST FIRST HURDLE....." TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
           DISPLAY "[" AT 2004
           DISPLAY WS-TEST1-LAST AT 2005

           IF WS-TEST1-LAST = " Last          "
              MOVE WS-TEST2-NAME      TO OFIS-NAME
              MOVE WS-TEST2-F-NAME    TO OFIS-FIRSTNAME
      *        ADD 1 TO LINE-CNT
      *         MOVE "LAST DONE..." TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
              GO TO BI-005.
           IF WS-TEST1-LAST = " Title         "
              MOVE WS-TEST2-NAME      TO OFIS-TITLE
      *        ADD 1 TO LINE-CNT
      *         MOVE "TITLE DONE..." TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
              GO TO BI-005.
           IF WS-TEST1-LAST = " Affiliation   "
              MOVE WS-TEST2-NAME      TO OFIS-AFFILIATION
              MOVE WS-TEST2-F-NAME    TO OFIS-CATEGORY
      *        ADD 1 TO LINE-CNT
      *         MOVE "AFFILIATION DONE..." TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
              GO TO BI-005.
           IF WS-TEST1-NAME-TITLE = " Phone Number    "
            IF LINE-CNT = 8
              MOVE WS-TEST1-NAME      TO OFIS-PHONE
              MOVE WS-TEST1-F-NAME    TO OFIS-PHONE-LABEL
      *         MOVE "PHONE DONE..." TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *        MOVE 4 TO LINE-CNT
              GO TO BI-005.
           IF WS-TEST1-NAME-TITLE = " Address         "
            IF LINE-CNT = 9
              MOVE WS-TEST1-NAME      TO OFIS-ADDRESS1
      *         MOVE "ADDRESS1 DONE..." TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *        MOVE 5                  TO LINE-CNT
              GO TO BI-005.
           IF WS-TEST1-NAME-TITLE = "                 "
            IF LINE-CNT = 10
              MOVE WS-TEST1-NAME      TO OFIS-ADDRESS2
      *         MOVE "ADDRESS2 DONE..." TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *        MOVE 6                  TO LINE-CNT
              GO TO BI-005.
           IF WS-TEST1-NAME-TITLE = " City/State/Zip  "
            IF LINE-CNT = 11
              MOVE WS-TEST1-NAME      TO OFIS-ADDRESS3
      *         MOVE "ZIP DONE..." TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *        MOVE 7                  TO LINE-CNT
              GO TO BI-005.
           IF WS-TEST1-NAME-TITLE = " Phone Number    "
            IF LINE-CNT = 13
              MOVE WS-TEST1-NAME      TO OFIS-FAX
              MOVE WS-TEST1-F-NAME    TO OFIS-FAX-LABEL
      *         MOVE "FAX DONE..." TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *        MOVE 8                  TO LINE-CNT
              GO TO BI-005.
           IF WS-TEST1-NAME-TITLE = " Address         "
            IF LINE-CNT = 14
              MOVE WS-TEST1-NAME      TO OFIS-ADDRESS4
      *         MOVE "ADDRESS3 DONE..." TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *        MOVE 9                  TO LINE-CNT
              GO TO BI-005.
           IF WS-TEST1-NAME-TITLE = "                 "
            IF LINE-CNT = 15
              MOVE WS-TEST1-NAME      TO OFIS-ADDRESS5
      *         MOVE "ADDRESS4 DONE..." TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *        MOVE 10                 TO LINE-CNT
              GO TO BI-005.
           IF WS-TEST1-NAME-TITLE = " City/State/Zip  "
            IF LINE-CNT = 16
              MOVE WS-TEST1-NAME      TO OFIS-ADDRESS6
      *         MOVE "ZIP2 DONE..." TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *        MOVE 11                 TO LINE-CNT
              GO TO BI-005.
           
      *     IF WS-TEST1-LAST = "                 "
      *      IF LINE-CNT = 11
      *        MOVE 12                 TO LINE-CNT
      *        GO TO BI-005.
           IF WS-TEST1-LAST = "                 "
            IF LINE-CNT = 18
      *        MOVE 13                 TO LINE-CNT
              GO TO BI-005.
           IF WS-TEST1-LAST = "                 "
            IF LINE-CNT = 19
      *        MOVE 14                 TO LINE-CNT
              PERFORM BI-050
              MOVE 3 TO LINE-CNT
              MOVE SPACES TO OFIS-RECORD
              GO TO BI-005.
           IF WS-TEST1-LAST = " Notes           "
            IF LINE-CNT = 18
      *         MOVE "NOTES REACHED..." TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *        MOVE 13                 TO LINE-CNT
              GO TO BI-020.
        BI-020.
           PERFORM BI-005.
      *     IF WS-TEST1 = " "
            IF LINE-CNT = 19
              PERFORM BI-005.
           MOVE 1 TO SUB-1.
        BI-025.
           IF WS-TEST1-LAST = " Last            "
              PERFORM BI-050
              MOVE 4 TO LINE-CNT
              MOVE SPACES TO OFIS-RECORD
              GO TO BI-010.
           MOVE WS-TEST1-80CHAR TO OFIS-LINE-DESC (SUB-1).
           ADD 1 TO SUB-1.
           PERFORM BI-005.
           GO TO BI-025.
        BI-050.
      *     MOVE "GOING TO WRITE NEW RECORD" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

           ADD 1 TO WS-COUNT
           DISPLAY "WRITTEN:" AT 2501
           DISPLAY WS-COUNT AT 2510.

           DISPLAY "[" AT 2701
           DISPLAY OFIS-KEY AT 2702.
      *     CALL "C$SLEEP" USING 4.
           
           
           WRITE OFIS-RECORD
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..." AT 2420
             DISPLAY WS-STAT1 AT 2453
      *       CLOSE OFIS-FILE
      *             OFIS-ASCII
             CALL "C$SLEEP" USING 5
         DISPLAY "                                            " AT 2420.
      *       STOP RUN.
      *     GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE OFIS-FILE
                 OFIS-ASCII.
           MOVE "FINISHED, CLOSING AND EXIT" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        C-EXIT.
           EXIT.
        COPY "ErrorMessage".
      * END-OF-JOB.
