        IDENTIFICATION DIVISION.
        PROGRAM-ID. CODATAXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT DATA-FILE ASSIGN TO "CoDataName"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DATA-KEY
               FILE STATUS IS WS-DATA-STATUS.
           SELECT DATA-ASCII ASSIGN TO 
                         "CoDataNameASCII"
               FILE STATUS IS WS-DATA-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY "ChlfdDataName".
           COPY "ChlfdDataNameASCII".
      *
       WORKING-STORAGE SECTION.
       77  WS-EOF                PIC X(3) VALUE "   ".
       77  SUB-1                 PIC 9(4) VALUE 0.
       77  SUB-2                 PIC 9(4) VALUE 0.
       77  SUB-3                 PIC 9(4) VALUE 0.
       77  SUB-4                 PIC 9(4) VALUE 0.
       77  SUB-5                 PIC 9(4) VALUE 0.
       77  SUB-10                PIC 9(4) VALUE 0.
       77  SUB-30                PIC 9(4) VALUE 0.
       77  SUB-35                PIC 9(4) VALUE 0.
       77  POS                   PIC 9(4) VALUE 0.
       77  WS-DATA-NAME          PIC X(60) VALUE "CoDataName".
       77  WS-MESSAGE            PIC X(70) VALUE " ".
       77  WS-DIS-MSG            PIC Z9.
       77  W-ERC                 BINARY-SHORT VALUE 0.
       77  W-DELAY               BINARY-SHORT VALUE 50.
       77  W-COL                 BINARY-SHORT VALUE 0.
       77  W-FRAME               BINARY-SHORT VALUE 0.
       77  W-LINE                BINARY-SHORT VALUE 0.
       77  WS-READS              BINARY-SHORT VALUE 0.
       77  WS-ACCEPT             PIC X(20) VALUE " ".
       01  F-FIELDNAME           PIC X(20).
       01  W-ESCAPE-KEY          PIC X.
       01  W-CRTSTATUS           PIC 9(4) VALUE 0.
       01  W-MESSAGE.
           03 FILLER             PIC X(15) VALUE " ".
           03 W-MESS1            PIC X(23) VALUE " ".
           03 W-MESS2            PIC X(6) VALUE " ".
       01  ALPHA-RATE.
           03  AL-RATE           PIC X OCCURS 60.
       01  DATA-RATE.
           03  DAT-RATE          PIC X OCCURS 60.
       01  WS-MENU-STATUS        PIC 99.
       01  WS-PRINT-STATUS       PIC 99.
       01  WS-DATA-STATUS        PIC 99.
       01  W-READ-KEY.
           03  WS-PA-KEY         PIC X OCCURS 11.
       01  F-FORMS.
           03  F-ERROR1            BINARY-SHORT.
           03  F-ERROR5            BINARY-SHORT.
           03  F-FILENAME          PIC X(40) VALUE SPACES.
           03  F-CBFILENAME        BINARY-SHORT VALUE 0.
           03  F-FH                BINARY-SHORT.
           03  F-INTEGERZERO       BINARY-SHORT VALUE 0.
           03  F-OPENMODE          PIC X(2) VALUE "mr".
       01  F-EXITSTATE.
           03  F-EXIT-ICH          BINARY-SHORT.
           03  F-EXIT-CH           PIC X.
           03  FILLER              PIC X(13).
      *
        PROCEDURE DIVISION.
        CONTROL-PARAGRAPH SECTION.
           PERFORM A-ACCEPT.
           PERFORM A-INIT.
           IF WS-ACCEPT = "T"
              PERFORM T-TEST
              GO TO CONTROL-020.
           IF WS-ACCEPT = "E"
               PERFORM B-EXPORT
           ELSE
               PERFORM B-IMPORT.
        CONTROL-020.
           PERFORM C-END.
           STOP RUN.
        CONTROL-000.
           EXIT. 
      *
       A-ACCEPT SECTION.
       A-001.
           MOVE 0810 TO POS.
           DISPLAY "** DATANAME EXPORT / IMPORT OF DATA **" AT POS
           MOVE 1010 TO POS
           DISPLAY "ENTER E=EXPORT TO ASCII, I=IMPORT FROM ASCII: [ ]"
              AT POS
           MOVE 1110 TO POS
           DISPLAY "T=TEST THE IMPORTED DATA." AT POS
           MOVE 1057 TO POS
           ACCEPT WS-ACCEPT AT POS.
           IF WS-ACCEPT NOT = "E" AND NOT = "I" AND NOT = "T"
              GO TO A-001.
        A-AC-EXIT.
           EXIT.
      *
        A-INIT SECTION.
        A-000.
           IF WS-ACCEPT = "I"
              OPEN OUTPUT DATA-FILE.
           IF WS-ACCEPT = "T"
              OPEN INPUT DATA-FILE
              MOVE 0 TO DATA-KEY
              START DATA-FILE KEY NOT < DATA-KEY
              GO TO A-EXIT.

           IF WS-ACCEPT = "E"
              OPEN EXTEND DATA-ASCII
           ELSE
              OPEN INPUT DATA-ASCII.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
      *     READ DATA-FILE NEXT
      *         AT END 
      *      GO TO BE-EXIT.
               
      *     DISPLAY DATA-NUMBER.

      *    MOVE DATA-RECORD    TO ASCII-RECORD.
      * BE-010.
      *     WRITE ASCII-RECORD
      *           INVALID KEY
      *       DISPLAY "INVALID WRITE FOR ASCII FILE...."
      *       DISPLAY WS-STAT1
      *       DISPLAY WS-STAT2
      *       STOP RUN.
      *     GO TO BE-005.
        BE-EXIT.
           EXIT.
      *
        B-IMPORT SECTION.
        BI-005.
           READ DATA-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
               
           MOVE ASCII-MESSAGE TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.

            MOVE ASCII-NUMBER    TO DATA-NUMBER.
            MOVE ASCII-MESSAGE   TO DATA-NAME.
        BI-010.
           WRITE DATA-RECORD
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             DISPLAY WS-DATA-STATUS
             STOP RUN.
           GO TO BI-005.
        BI-EXIT.
           EXIT.
      *
        T-TEST SECTION.
        T-005.
           READ DATA-FILE NEXT
               AT END 
             GO TO T-EXIT.
               
           IF WS-DATA-STATUS NOT = 0
              MOVE WS-DATA-STATUS TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
               
           MOVE DATA-RECORD TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.

        T-010.
           GO TO T-005.
        T-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE DATA-FILE
                 DATA-ASCII.
        C-EXIT.
           EXIT.
           COPY "ErrorMessage".
      * END-OF-JOB.
