        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrCAMSCt.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectCrMaster".
           Copy "SelectCrCAMSTrans".
           Copy "SelectCrCheques".
           Copy "SelectSlDaily".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS IS WS-SPOOLER-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCrCAMSTrans.
           COPY ChlfdCrCheques.
           COPY ChlfdDaily.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-FOUND             PIC X VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-ANSWER1           PIC X(7) VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-NUM-DISP          PIC Z(3)9.
       77  WS-CONVERSION-AMT    PIC 9(7)V99.
       77  WS-VALUE             PIC Z(6)9.99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1     PIC 99.
       01  WS-CRCAMSTRANS-STATUS.
           03  WS-CRCAMSTRANS-ST1  PIC 99.
       01  WS-CRCHEQUE-STATUS.
           03  WS-CRCHEQUE-ST1     PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       01  WS-SPOOLER-STATUS.
           03  WS-SPOOLER-ST1      PIC 99.
       Copy "WsDateInfo".
       Copy "WStore".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 315 TO POS
           DISPLAY
           "** CREDITORS CAMS CONVERSION FROM CHEQUE RUN **" AT POS
           MOVE 415 TO POS
           DISPLAY 
           "***********************************************" AT POS.
           MOVE 2705 TO POS.
       CONTROL-010.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES
           PERFORM CONVERT-FILES
           PERFORM END-OFF.
       CONTROL-999.
           EXIT PROGRAM.
      *
       GET-DATA SECTION.
       GET-015.
           MOVE 810 TO POS.
           DISPLAY 
           "Press <RETURN> To Continue The Conversion, <END> To Exit."
           AT POS.
           ADD 60 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 70        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

           IF W-ESCAPE-KEY = 3
               PERFORM END-900.
           IF W-ESCAPE-KEY = 1
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-900
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
       GET-900.
           MOVE 2310 TO POS
           DISPLAY
           "CONVERSION FROM CHEQUES TO CAMS FORMAT IN PROGRESS..."
               AT POS.
       GET-999.
            EXIT.
      *
       CONVERT-FILES SECTION.
       RDT-005.
           MOVE 3010 TO POS
           DISPLAY "READING CR-CHEQUES......         " AT POS.
           MOVE 0 TO CRCH-ACC-NUMBER.
           START CRCH-FILE KEY NOT < CRCH-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-CRCAMSTRANS-ST1 NOT = 0
               MOVE 
               "CRCHEQUE FILE BUSY ON START, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDT-005.
       RDT-010.
           READ CRCH-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-CRCHEQUE-ST1 = 10
               GO TO RDT-999.
           IF WS-CRCHEQUE-ST1 NOT = 0
               MOVE "CRCHEQUE BUSY ON READ-NEXT, 'ESC' TO RETRY." 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRCHEQUE-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRCAMSTRANS-ST1
               GO TO RDT-010.

           MOVE CRCH-CHEQUE-NO     TO CR-CAMS-TRANS-NUM
           MOVE CRCH-ACC-NUMBER    TO CR-CAMS-TRANS-ACC-NUMBER
           MOVE CRCH-CHEQUE-NO     TO CR-CAMS-TRANS-CHEQUE-NUM
           MOVE CRCH-CHEQUE-AMT    TO CR-CAMS-TRANS-AMOUNT
           MOVE CRCH-DATE          TO CR-CAMS-TRANS-DATE.
           
           PERFORM READ-CREDITOR.
           
           IF CR-NAME = "** UNKNOWN **"
              MOVE "THERE IS AN ERROR IN READING THE FOLLOWING ACCOUNT"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE SPACES TO WS-MESSAGE
                MOVE CR-ACCOUNT-NUMBER TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDT-010.
                
           MOVE CR-SUPPLIER-NUMBER TO CR-CAMS-TRANS-CRED-NUM
           MOVE CR-NAME            TO CR-CAMS-TRANS-ACC-NAME
           MOVE CR-CAMS-ACC-TYPE   TO CR-CAMS-TRANS-CRED-TYPE
           MOVE CR-CAMS-BANK-NAME  TO CR-CAMS-TRANS-BANK-NAME
           MOVE CR-CAMS-BANK-NUM   TO CR-CAMS-TRANS-BANK-NUM
           MOVE CR-CAMS-BRANCH-NUM TO CR-CAMS-TRANS-BRANCH-NUM
           MOVE "N"                TO CR-CAMS-TRANS-PAID.
        RDT-020.
           PERFORM WRITE-CAMS-TRANSACTION.
        RDT-030.
      **SUB-9 = TOTAL NO OF CR-CHEQUES****
           ADD 1                TO SUB-9
           ADD CRCH-CHEQUE-AMT  TO WS-CONVERSION-AMT
           MOVE 2610 TO POS
           DISPLAY "Number of Cheques :       Value of Conversions:"
               AT POS
           ADD 20 TO POS
           MOVE SUB-9 TO WS-NUM-DISP
           DISPLAY WS-NUM-DISP AT POS
           MOVE 2658 TO POS
           MOVE WS-CONVERSION-AMT TO WS-VALUE
           DISPLAY WS-VALUE AT POS.
           
           GO TO RDT-010.
       RDT-999.
           EXIT.
      *
       WRITE-CAMS-TRANSACTION SECTION.
       WRPTR-000.
           MOVE 2810 TO POS
           DISPLAY "WRITING CAMS-TRANSACTION..........              "
             AT POS.
       WRPTR-010.
           WRITE CR-CAMS-TRANS-REC
                 INVALID KEY NEXT SENTENCE.
           IF WS-CRCAMSTRANS-ST1 = 22
               MOVE
         "CAMS FILE ALREADY POSTED THIS MONTH FOR ACCOUNT," &
         " 'ESC' TO CHECK NEXT ACCOUNT." TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO WRPTR-999.
           IF WS-CRCAMSTRANS-ST1 NOT = 0
               MOVE
            "CAMS FILE BUSY ON WRITE, ALREADY EXISTS, 'ESC' TO EXIT."
                   TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRCAMSTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRCAMSTRANS-ST1
               GO TO WRPTR-999.
           PERFORM ERROR-020.
           MOVE 2810 TO POS
           DISPLAY WS-MESSAGE AT POS.
       WRPTR-999.
              EXIT.
      *
       READ-CREDITOR SECTION.
       RCR-000.
           MOVE CR-CAMS-TRANS-ACC-NUMBER TO CR-ACCOUNT-NUMBER.
           START CREDITOR-MASTER KEY NOT < CR-KEY
               INVALID KEY NEXT SENTENCE.
       RCR-010.
           READ CREDITOR-MASTER
                INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE "** UNKNOWN **" TO CR-NAME
                MOVE 0 TO WS-CREDITOR-ST1
                GO TO RCR-999.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITOR RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RCR-010.
           MOVE 2510 TO POS.
           DISPLAY "Account Number Being Processed:" AT POS.
           ADD 32 TO POS.
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
       RCR-999.
             EXIT.
      *
      *THIS PROCESS BELOW DOESN'T CURRENTLY GET USED...
       DELETE-CHEQUES-FROM-LAST-RUN SECTION.
       DCFLR-005.
           MOVE 3010 TO POS
           DISPLAY "DELETING CHEQUES FROM LAST RUN.....       " AT POS.
           START CRCH-FILE KEY NOT < CRCH-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-CRCHEQUE-ST1 = 23 OR 35 OR 49 OR 91
               GO TO DCFLR-999.
           IF WS-CRCHEQUE-ST1 NOT = 0
               MOVE 0 TO WS-CRCHEQUE-ST1
               MOVE "CHEQUE DELETE BUSY AT START,  'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DCFLR-005.
       DCFLR-010.
           READ CRCH-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-CRCHEQUE-ST1 = 10
               CLOSE CRCH-FILE
               PERFORM OPEN-015
               GO TO DCFLR-999.
           IF WS-CRCHEQUE-ST1 NOT = 0
               MOVE
               "CRCHEQUE BUSY ON READ-NEXT, DCFLR-010, 'ESC' TO RETRY" 
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DCFLR-010.
       DCFLR-020.
           DELETE CRCH-FILE
             INVALID KEY NEXT SENTENCE.
           IF WS-CRCHEQUE-ST1 NOT = 0
               MOVE "CRCHEQUE BUSY ON DELETE, 'ESC' TO RETRY" 
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DCFLR-020.
           GO TO DCFLR-010.
       DCFLR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O CREDITOR-MASTER.
           IF WS-CREDITOR-ST1 NOT = 0
              MOVE 0 TO WS-CREDITOR-ST1
              MOVE "CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-014.
           OPEN I-O CR-CAMS-TRANS-FILE.
           IF WS-CRCAMSTRANS-ST1 NOT = 0 
              MOVE "CRCAMS FILE BUSY ON OPEN I-O,  'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRCAMSTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE WS-CAMS-DATA TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-0141.
           GO TO OPEN-015.
       OPEN-0141.
           OPEN OUTPUT CR-CAMS-TRANS-FILE.
           IF WS-CRCAMSTRANS-ST1 NOT = 0 
              MOVE "CRCAMS FILE BUSY ON OPEN OUTPUT,  'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRCAMSTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE WS-CAMS-DATA TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
       OPEN-015.
           OPEN I-O CRCH-FILE.
           IF WS-CRCHEQUE-ST1 NOT = 0 
              MOVE 0 TO WS-CRCHEQUE-ST1
              MOVE "CRCHEQUE FILE BUSY ON OPEN,  'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE "THERE ARE NO CRCHEQUES ON FILE,  'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM END-OFF.
      *
       END-OFF SECTION.
       END-100.
           CLOSE CREDITOR-MASTER
                 CR-CAMS-TRANS-FILE
                 CRCH-FILE.
                 
           MOVE 3010 TO POS
           DISPLAY "CONVERSION FINISHED, PRESS <RETURN> TO EXIT." AT POS
           ADD 45 TO POS
           ACCEPT WS-ACCEPT AT POS.
       END-900.
           EXIT PROGRAM.
      *    STOP RUN.
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
      * END-OF-PROGRAM
