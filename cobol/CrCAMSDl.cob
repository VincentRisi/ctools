        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrCAMSDl.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectCrCAMSTrans".
           Copy "SelectGlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-DL-FILE
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCrCAMSTrans.
           COPY ChlfdGlParam.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(180).

       WORKING-STORAGE SECTION.
       77  WS-DL-FILE         PIC X(20) VALUE " ".
       77  WS-ANSWER1         PIC X(10) VALUE " ".      
       77  WS-ONLY-UNALLOC    PIC X VALUE " ".
       77  WS-END             PIC X VALUE " ".
       77  WS-SWAP            PIC X VALUE " ".
       77  WS-ABOVE-BODY      PIC X VALUE " ".
       77  WS-TR-AMOUNT       PIC 9(8)V99 VALUE 0.
       77  WS-ACCEPT          PIC X.
       01  WS-CRCAMSTRANS-STATUS.
           03  WS-CRCAMSTRANS-ST1   PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1   PIC 99.
       01  WS-DOWNLOAD-FILE.
           03  WS-VOL-DIR     PIC X(12) VALUE "/ctools/spl/".
           03  WS-DL-NAME     PIC X(10) VALUE " ".
       01  WS-END-MESSAGE.
           03  WS-END-1       PIC X(29) VALUE
                     "Number Of CAMS Trans Written:".
           03  WS-END-2       PIC Z(2)9.
           03  FILLER         PIC X(5) VALUE " ".
           03  WS-END-3       PIC X(8) VALUE "Value: R".
           03  WS-END-4       PIC Z(7)9.99.
       01  WS-DATE8.
           03  WS-CAMSCC      PIC 99.
           03  WS-DATE6.
               05  WS-CAMSYY      PIC 99.
               05  WS-CAMSMM      PIC 99.
               05  WS-CAMSDD      PIC 99.
       01  BATCH-REF-DETAIL.
           03  FILLER              PIC X(3) VALUE "A/C".
           03  BFD-CRED-ACC        PIC X(10).
           03  FILLER              PIC X(1) VALUE "P".
           03  BFD-CHEQUE-NUM      PIC X(6).
       01  CAMS-HEADER.
           03  CAMS-RECORD-TYPE    PIC 99 VALUE 2.
           03  FILLER              PIC X(178) VALUE ALL "0".
       01  CAMS-USER-HEADER.
           03  CAMS-HEADER-TYPE    PIC 99 VALUE 4.
           03  FILLER              PIC X(178) VALUE ALL "0".
       01  CAMS-USER-TRAILER.
           03  CAMS-TRAILER-TYPE   PIC 99 VALUE 92.
           03  FILLER              PIC X(178) VALUE ALL "0".
       01  CAMS-USER-INSTALLATION.
           03  CAMS-INSTALL-TYPE   PIC 99 VALUE 94.
           03  FILLER              PIC X(178) VALUE ALL "0".
       01  CAMS-STD-TRANS.
           03  CAMS01-IDENTIFIER         PIC 99.
           03  CAMS02-USER-BRANCH-CODE   PIC 9(6).
           03  CAMS03-USER-ACC-NUM       PIC 9(11).
           03  CAMS04-USER-CODE          PIC 9(4) VALUE 0.
           03  CAMS05-PAYMENT-NUMBER     PIC 9(6).
           03  CAMS06-CRED-BRANCH-CODE   PIC 9(6).
           03  CAMS07-CRED-ACC-NUM       PIC 9(11).
           03  CAMS08-CRED-TYPE          PIC 9.
           03  CAMS09-PMT-AMOUNT         PIC 9(11).
           03  CAMS10-PMT-DATE           PIC 9(6).
           03  CAMS11-14-FILLER          PIC 9(6) VALUE 0.
           03  CAMS15-USER-REFERENCE     PIC X(20) VALUE " ".
           03  CAMS15-USER-FILLER        PIC X(10) VALUE " ".
           03  CAMS16-HOME-ACC-NAME      PIC X(30) VALUE " ".
           03  CAMS17-20-FILLER          PIC X(50) VALUE "0".
           
       Copy "WsDateInfo".
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
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           MOVE 0410 TO POS
           DISPLAY "*** CREDITOR CAMS DOWNLOAD PROGRAM ***" AT POS
           MOVE 0510 TO POS
           DISPLAY "**************************************" AT POS.
           PERFORM GET-DATA.
           
           MOVE WS-DOWNLOAD-FILE TO WS-DL-FILE
           OPEN OUTPUT PRINT-FILE.
           WRITE PRINT-REC FROM CAMS-HEADER
           WRITE PRINT-REC FROM CAMS-USER-HEADER.
           
           PERFORM WRITE-CAMS-DOWNLOAD.
           
           WRITE PRINT-REC FROM CAMS-USER-TRAILER
           WRITE PRINT-REC FROM CAMS-USER-INSTALLATION.
           
           PERFORM END-OFF.
       CONTROL-999.
           EXIT PROGRAM.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE 810 TO POS.
           DISPLAY 
           "Enter a Batch Name for this Download :[          ]" AT POS.
           ADD 39 TO POS.

           MOVE WS-ANSWER1 TO CDA-DATA.
           MOVE 10        TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 48        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

           IF W-ESCAPE-KEY = 3
               PERFORM END-900.
           IF WS-ANSWER1 = " "
              MOVE "PLEASE ENTER A VALID FILE NAME. IT CANNOT BE BLANK."
               TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO GET-000.
              
           MOVE WS-ANSWER1 TO WS-DL-NAME.
           MOVE 920 TO POS
           DISPLAY WS-DOWNLOAD-FILE AT POS.

           IF W-ESCAPE-KEY = 1
               GO TO GET-004
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-000.
       GET-004.
           MOVE "N" TO WS-SWAP.
       GET-005.
           MOVE 1010 TO POS.
           DISPLAY 
           "Do You Want To Swap References for Online Banking: [ ]" 
               AT POS.
           ADD 52 TO POS.

           MOVE WS-SWAP   TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 61        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SWAP.

           IF WS-SWAP NOT = "Y" AND NOT = "N"
              MOVE "PLEASE ENTER EITHER 'Y' OR 'N'." TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO GET-005.
           IF W-ESCAPE-KEY = 4
               GO TO GET-000.
       GET-010.
           PERFORM ERROR-020.
           MOVE 1410 TO POS.
           DISPLAY 
           "Press <RETURN> To Continue The Conversion, <FINISH> To Exit"
           AT POS.
           ADD 60 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 70        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

           IF W-ESCAPE-KEY = 4
               GO TO GET-005.
           IF W-ESCAPE-KEY = 3
               PERFORM END-900.
           IF W-ESCAPE-KEY = 1
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-900
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-900.
           MOVE 2310 TO POS
           DISPLAY
           "CONVERSION FROM CHEQUES TO CAMS FORMAT IN PROGRESS..."
               AT POS.
       GET-999.
            EXIT.
      *
       WRITE-CAMS-DOWNLOAD SECTION.
       RONX-001.
           MOVE 2910 TO POS
           DISPLAY "READING CAMS TRANSACTIONS FOR ACCOUNT...." AT POS.
           MOVE 0 TO CR-CAMS-TRANS-KEY.
           START CR-CAMS-TRANS-FILE KEY NOT < CR-CAMS-TRANS-KEY
                 INVALID KEY NEXT SENTENCE.
           IF WS-CRCAMSTRANS-ST1 NOT = 0
              MOVE "BAD START ON READING CAMS FILE, 'ESC' TO EXIT."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-999.
           MOVE 1 TO SUB-1.
           MOVE " " TO ALPHA-RATE.
       RONX-005.
           READ CR-CAMS-TRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-CRCAMSTRANS-ST1 = 10
              GO TO RONX-900.
           IF WS-CRCAMSTRANS-ST1 NOT = 0
              MOVE 0 TO WS-CRCAMSTRANS-ST1
              MOVE "CRCAMS FILE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-005.
              
            IF CR-CAMS-TRANS-PAID NOT = "P"
                GO TO RONX-005.
       RONX-010.
            MOVE 10                       TO CAMS01-IDENTIFIER
            MOVE GLPA-CAMS-BANK-CODE      TO CAMS02-USER-BRANCH-CODE
            MOVE GLPA-CAMS-BANK-ACC       TO CAMS03-USER-ACC-NUM
            MOVE SUB-1                    TO CAMS05-PAYMENT-NUMBER
            MOVE CR-CAMS-TRANS-BRANCH-NUM TO CAMS06-CRED-BRANCH-CODE
            MOVE CR-CAMS-TRANS-BANK-NUM   TO CAMS07-CRED-ACC-NUM.
            MOVE CR-CAMS-TRANS-BANK-NUM   TO CAMS07-CRED-ACC-NUM.
            MOVE CR-CAMS-TRANS-CRED-TYPE  TO CAMS08-CRED-TYPE.
            MOVE CR-CAMS-TRANS-AMOUNT     TO ALPHA-RATE
            PERFORM REMOVE-DOT.
            MOVE ALPHA-RATE               TO CAMS09-PMT-AMOUNT
            MOVE CR-CAMS-TRANS-DATE       TO WS-DATE8
            MOVE WS-DATE6                 TO CAMS10-PMT-DATE
            MOVE CR-CAMS-TRANS-CRED-NUM   TO BFD-CRED-ACC
            MOVE CR-CAMS-TRANS-CHEQUE-NUM TO BFD-CHEQUE-NUM.
            IF WS-SWAP = "N"
               MOVE BATCH-REF-DETAIL         TO CAMS15-USER-REFERENCE
               MOVE ALL " "                  TO CAMS15-USER-FILLER
               MOVE CR-CAMS-TRANS-ACC-NAME   TO CAMS16-HOME-ACC-NAME
               MOVE ALL "0"                  TO CAMS17-20-FILLER
            ELSE
               MOVE BATCH-REF-DETAIL         TO CAMS16-HOME-ACC-NAME
               MOVE ALL " "                  TO CAMS15-USER-FILLER
               MOVE CR-CAMS-TRANS-ACC-NAME   TO CAMS15-USER-REFERENCE
               MOVE ALL "0"                  TO CAMS17-20-FILLER.
                                             
            WRITE PRINT-REC FROM CAMS-STD-TRANS.
       RONX-020.
            MOVE "Y" TO CR-CAMS-TRANS-PAID.
       RONX-022.
           REWRITE CR-CAMS-TRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CRCAMSTRANS-ST1 NOT = 0
                MOVE "CBTRANS BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RONX-022.
           ADD CR-CAMS-TRANS-AMOUNT TO WS-TR-AMOUNT.
           MOVE " "                 TO ALPHA-RATE.
           ADD 1 TO SUB-1
               GO TO RONX-005.
       RONX-900.
           MOVE SUB-1 TO SUB-2.
       RONX-999.
           EXIT.
      *
       REMOVE-DOT SECTION.
       RD-005.
           MOVE " " TO DATA-RATE.
           MOVE 1 TO SUB-4 SUB-5.
       RD-010.
           IF AL-RATE (SUB-4) = " "
              MOVE "0" TO AL-RATE (SUB-4).
           IF AL-RATE (SUB-4) NOT = "."
              MOVE AL-RATE (SUB-4) TO DAT-RATE (SUB-5).
           IF AL-RATE (SUB-4) = "."
              ADD 1 TO SUB-4
              GO TO RD-010.
           IF SUB-4 < 15
              ADD 1 TO SUB-4 SUB-5
              GO TO RD-010.
       RD-020.
      *MOVE NUMBERS TO RIGHT BY TWO CHARS
           MOVE 1 TO SUB-4 
           MOVE 3 TO SUB-5.
           MOVE ALL "0" TO ALPHA-RATE.
       RD-030.
           MOVE DAT-RATE (SUB-4) TO AL-RATE (SUB-5).
           IF SUB-4 < 15
              ADD 1 TO SUB-4 SUB-5
              GO TO RD-030. 
       RD-999.
           EXIT.
      *
       READ-GLPARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       RP-010.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE "GLPARAMETER BUSY ON READ, RP-010, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RP-010.
       RP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CR-CAMS-TRANS-FILE
            IF WS-CRCAMSTRANS-ST1 NOT = 0
               MOVE 0 TO WS-CRCAMSTRANS-ST1
               MOVE "CRCAMS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-012.

           PERFORM READ-GLPARAMETER.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           PERFORM ERROR1-020.
           
           SUBTRACT 1 FROM SUB-1
           MOVE SUB-1 TO WS-END-2
           MOVE WS-TR-AMOUNT TO WS-END-4
           MOVE 2915 TO POS
           DISPLAY WS-END-MESSAGE AT POS.
           
           MOVE 3010 TO POS
           DISPLAY 
           "WRITING OF CAMS DOWNLOAD FILE COMPLETE, <RETURN> TO EXIT."
            AT POS
           ADD 58 TO POS
           ACCEPT WS-ACCEPT AT POS.
           
           CLOSE PRINT-FILE.
       END-900.
           CLOSE CR-CAMS-TRANS-FILE
                 GLPARAMETER-FILE.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
      ******************
      *Mandatory Copies*
      ******************
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      * END-OF-JOB
