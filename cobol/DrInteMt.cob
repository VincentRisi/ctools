        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrInteMt.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrTrans".
         Copy "SelectSlParameter".
         Copy "SelectSlDistributions".
         Copy "SelectSlDaily".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdParam.
           COPY ChlfdDrTrans.
           COPY ChlfdDisTot.
           COPY ChlfdDaily.
      *
       WORKING-STORAGE SECTION.
       77  WS-LINE-CNT          PIC 9(3) VALUE 66.
       77  WS-PAGE-CNT          PIC 9(3) VALUE 0.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "DrAcStIq".
       77  WS-ACCOUNT-NUMBER    PIC 9(7) VALUE 0.
       77  WS-INTERESTAMT       PIC 9(7)V99 VALUE 0.
       77  WS-PERCENTAGE        PIC 9(2)V9999 VALUE 0.
       77  WS-PERIOD            PIC 9 VALUE 0.
       77  WS-WORKTOTAL         PIC 9(7)V99 VALUE 0.
       77  WS-BODY-LINE         PIC Z9.
       77  WS-ANSWER            PIC X VALUE " ".
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1       PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1  PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1      PIC 99.
       01  WS-DISTRIBUTION-STATUS.
           03  WS-DISTRIBUTION-ST1 PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       01  WS-TYPES.
           03  FILLER          PIC X(7) VALUE "INVOICE".
           03  FILLER          PIC X(7) VALUE "PAYMENT".
           03  FILLER          PIC X(7) VALUE "PAYMENT".
           03  FILLER          PIC X(7) VALUE "JRN.DR.".
           03  FILLER          PIC X(7) VALUE "JRN.CR.".
           03  FILLER          PIC X(7) VALUE "C/NOTE ".
           03  FILLER          PIC X(7) VALUE "INTREST".
       01  WS-TYPES-RED REDEFINES WS-TYPES.
           03  WS-TYPE-DESC    PIC X(7) OCCURS 7.
       Copy "WsDateInfo".
      *
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONT-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           PERFORM UPDATE-DISTRIBUTION.
           GO TO CONT-010.
      *
        GET-DATA SECTION.
        GET-000.
            MOVE "                          " TO F-NAMEFIELD.
            MOVE "ACCOUNT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 GO TO GET-999.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO DR-ACCOUNT-NUMBER.
            IF DR-ACCOUNT-NUMBER = 0
                PERFORM CLEAR-SCREEN
                CLOSE DEBTOR-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE.
       GET-002.
            IF DR-ACCOUNT-NUMBER = 0
                OPEN I-O DEBTOR-MASTER
              IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO GET-002
              ELSE
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                GO TO GET-000.

            PERFORM READ-DEBTORS.
            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE DR-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            IF DR-NAME = "UNKNOWN"
                GO TO GET-000.
       GET-010.
            MOVE "                          " TO F-NAMEFIELD.
            MOVE "PERCENTAGE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-000.
            MOVE 7           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-PERCENTAGE.

            MOVE "PERCENTAGE"  TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE WS-PERCENTAGE TO F-EDNAMEFIELDFACTOR
            MOVE 7             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FACTOR.

            IF WS-PERCENTAGE > 0
                GO TO GET-020.
            MOVE "DON'T BOTHER ME WITH A ZERO INTEREST PERCENTAGE!!!"
                TO WS-MESSAGE.
            PERFORM ERROR-000.
            GO TO GET-010.
       GET-020.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
            MOVE "                          " TO F-NAMEFIELD.
            MOVE "PERIOD" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-010.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-PERIOD.
            IF WS-PERIOD = 1 OR = 2 OR = 3 OR = 4
               GO TO GET-030.
            MOVE "ENTER A PERIOD BETWEEN 1 AND 4, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-000.
            GO TO GET-020.
       GET-030.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
            PERFORM CLEAR-010.
            MOVE 2525 TO POS.
            DISPLAY "IS EVERYTHING CORRECT????" AT POS.
            MOVE 2725 TO POS.
            DISPLAY "ENTER (Y/N): [ ]" AT POS.
            ADD 14 TO POS.
            ACCEPT WS-ANSWER AT POS.
            IF W-ESCAPE-KEY = 4
                GO TO GET-020.
            IF W-ESCAPE-KEY = 0 OR = 1 OR = 2 OR = 5
                GO TO GET-035
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-030.
       GET-035.
            MOVE " " TO WS-MESSAGE
            MOVE 2510 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE 2710 TO POS
            DISPLAY WS-MESSAGE AT POS
            IF WS-ANSWER = "Y"
               GO TO GET-040.
            IF WS-ANSWER = "N"
               GO TO GET-999.
            MOVE "TRY AND GIVE ME AN UNDERSTANDABLE ANSWER!!!"
               TO WS-MESSAGE.
            PERFORM ERROR-MESSAGE.
            GO TO GET-030.
       GET-040.
            PERFORM READ-DEBTORS-LOCK.
            PERFORM UPDATE-DEBTOR.
            PERFORM REWRITE-DEBTOR.
            PERFORM WRITE-TRANSACTIONS.
            MOVE " " TO WS-ANSWER.
       GET-999.
            EXIT.
      *
       WRITE-TRANSACTIONS SECTION.
       WRTR-000.
             PERFORM READ-PARAMETER-LOCK.
             MOVE PA-INVOICE-NUMBER TO DRTR-TRANSACTION-NUMBER.
             ADD 1 TO PA-INVOICE-NUMBER.
             PERFORM REWRITE-PARAMETER.
             MOVE 7                 TO DRTR-TYPE.
             MOVE DR-ACCOUNT-NUMBER TO DRTR-ACCOUNT-NUMBER.
             IF WS-PERIOD = 1
                MOVE "INTEREST ON 30 DAYS " TO DRTR-REFERENCE1.
             IF WS-PERIOD = 2
                MOVE "INTEREST ON 60 DAYS " TO DRTR-REFERENCE1.
             IF WS-PERIOD = 3
                MOVE "INTEREST ON 90 DAYS " TO DRTR-REFERENCE1.
             IF WS-PERIOD = 4
                MOVE "INTEREST ON 121+DAYS" TO DRTR-REFERENCE1.
             MOVE DRTR-TRANSACTION-NUMBER   TO DRTR-REFERENCE2.
             MOVE WS-DATE        TO DRTR-DATE
                                    DRTR-DEL-DATE.
             MOVE WS-INTERESTAMT TO DRTR-AMT-OF-INVOICE
                                    DRTR-AMT-OUTSTANDING.
       WRTR-010.
             WRITE DEBTOR-TRANS-REC
                 INVALID KEY NEXT SENTENCE.
             IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
                 GO TO WRTR-000.
             IF WS-DRTRANS-ST1 NOT = 0
                 MOVE "DEBTOR TRANS. BUSY ON WRITE, 'ESC' TO RETRY." 
                    TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-DRTRANS-ST1
                 GO TO WRTR-010.
                 
           MOVE " " TO WS-MESSAGE
           PERFORM ERROR-020
           MOVE "INTEREST INVOICE:" TO WS-DAILY-1ST
           MOVE DRTR-REFERENCE2     TO WS-DAILY-2ND
           MOVE " "                 TO WS-DAILY-3RD WS-DAILY-4TH
           MOVE WS-DAILY-MESSAGE TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
       WRTR-999.
              EXIT.
      *
       UPDATE-DEBTOR SECTION.
       UPDR-000.
           COMPUTE WS-PERCENTAGE ROUNDED = WS-PERCENTAGE / 12.
           
           IF WS-PERIOD = 1
              COMPUTE WS-PERCENTAGE ROUNDED = WS-PERCENTAGE * 1.
           IF WS-PERIOD = 2
              COMPUTE WS-PERCENTAGE ROUNDED = WS-PERCENTAGE * 2.
           IF WS-PERIOD = 3
              COMPUTE WS-PERCENTAGE ROUNDED = WS-PERCENTAGE * 3.
           IF WS-PERIOD = 4
              COMPUTE WS-PERCENTAGE ROUNDED = WS-PERCENTAGE * 4.
       UPDR-005.
           IF WS-PERIOD = 1
              COMPUTE WS-INTERESTAMT ROUNDED = 
                 ((WS-PERCENTAGE * DR-30DAY) / 100)
              GO TO UPDR-010.
           IF WS-PERIOD = 2
              COMPUTE WS-INTERESTAMT ROUNDED = 
                 ((WS-PERCENTAGE * DR-60DAY) / 100)
              GO TO UPDR-010.
           IF WS-PERIOD = 3
              COMPUTE WS-INTERESTAMT ROUNDED = 
                 ((WS-PERCENTAGE * DR-90DAY) / 100)
              GO TO UPDR-010.
           IF WS-PERIOD = 4
               COMPUTE WS-INTERESTAMT ROUNDED = 
               ((WS-PERCENTAGE * DR-120DAY) / 100).
       UPDR-010.
           ADD WS-INTERESTAMT TO DR-CURRENT
                                 DR-BALANCE.
      *     COMPUTE DR-BALANCE = DR-CURRENT + DR-30DAY + DR-60DAY
      *             + DR-90DAY + DR-120DAY.
       UPDR-999.
           EXIT.
      *
       REWRITE-DEBTOR SECTION.
       REWRDB-000.
             REWRITE DEBTOR-RECORD
                 INVALID KEY NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                 MOVE "ACCOUNT NUMBER: INT." TO WS-DAILY-1ST
                 MOVE DR-ACCOUNT-NUMBER      TO WS-DAILY-2ND
                 MOVE "NOT UPDATED"          TO WS-DAILY-3RD
                 MOVE " "                    TO WS-DAILY-4TH
                 PERFORM WRITE-DAILY
                 GO TO REWRDB-999.
             IF WS-DEBTOR-ST1 NOT = 0
                 MOVE "DEBTOR FILE BUSY ON REWRITE, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-DEBTOR-ST1
                 GO TO REWRDB-000.
       REWRDB-999.
            EXIT.
      *
       UPDATE-DISTRIBUTION SECTION.
       UPDIS-000.
            MOVE "1" TO DIST-KEY.
       UPDIS-010.
            READ DISTRIBUTIONS WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-DISTRIBUTION-ST1 = 23 OR 35 OR 49
               MOVE "DISTRIBUTION TOTAL RECORD NOT FOUND!!!!"
                  TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPDIS-900.
            IF WS-DISTRIBUTION-ST1 NOT = 0
               MOVE "DISTRIBUTION BUSY ON READ, 'ESC' TO RETRY"
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DISTRIBUTION-ST1
               GO TO UPDIS-010.
            ADD WS-INTERESTAMT TO DIST-INTERESTWEEK
                                  DIST-INTERESTPTD
                                  DIST-INTERESTYTD
                                  DIST-ACCRECWEEK
                                  DIST-ACCRECPTD
                                  DIST-ACCRECYTD.
       UPDIS-020.
            REWRITE DIST-REC
               INVALID KEY NEXT SENTENCE.
            IF WS-DISTRIBUTION-ST1 = 23 OR 35 OR 49
               MOVE "DISTRIBUTIONS NOT UPDATED, 'ESC' TO EXIT."
                  TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPDIS-900.
            IF WS-DISTRIBUTION-ST1 NOT = 0
               MOVE "DISTRIBUTIONS BUSY ON REWRITE, 'ESC' TO RETRY." 
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DISTRIBUTION-ST1
               GO TO UPDIS-020.
       UPDIS-900.
            MOVE 0 TO WS-INTERESTAMT
                      WS-PERIOD
                      WS-PERCENTAGE.
       UPDIS-999.
           EXIT.
      *
       READ-DEBTORS SECTION.
       RD-000.
           READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO DR-NAME DR-ADDRESS1 DR-ADDRESS2
                        DR-ADDRESS3 DR-DEL-ADDRESS1 DR-DEL-ADDRESS2
                        DR-DEL-ADDRESS3
               MOVE "UNKNOWN" TO DR-NAME
               MOVE 0 TO DR-POST-CODE
               GO TO RD-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTORS BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RD-000.
       RD-999.
           EXIT.
      *
       READ-DEBTORS-LOCK SECTION.
       RD-000.
           READ DEBTOR-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO DR-NAME DR-ADDRESS1 DR-ADDRESS2
                        DR-ADDRESS3 DR-DEL-ADDRESS1 DR-DEL-ADDRESS2
                        DR-DEL-ADDRESS3
               MOVE "UNKNOWN" TO DR-NAME
               MOVE 0 TO DR-POST-CODE
               GO TO RD-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTORS READ-LOCK BUSY, PRESS 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RD-000.
       RD-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           READ PARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO PARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       READ-PARAMETER-LOCK SECTION.
       RP-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           READ PARAMETER-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO PARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       REWRITE-PARAMETER SECTION.
       REWP-000.
           REWRITE PARAMETER-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "PARAMETER RECORD NOT UPDATED!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO REWP-000.
       REWP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *      ACCEPT WS-DATE FROM DATE.
       OPEN-010.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-010.
       OPEN-011.
            OPEN I-O PARAMETER-FILE.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
            PERFORM ERROR-020.
       OPEN-012.
            OPEN I-O DEBTOR-TRANS-FILE.
            IF WS-DRTRANS-ST1 NOT = 0
               MOVE " " TO WS-DRTRANS-ST1
               MOVE "DRTRANS BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-012.
       OPEN-013.
            OPEN I-O DISTRIBUTIONS.
            IF WS-DISTRIBUTION-ST1 NOT = 0
               MOVE 0 TO WS-DISTRIBUTION-ST1
               MOVE "DISTRIBUTIONS BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-013.
            MOVE " " TO WS-MESSAGE.
            PERFORM ERROR-020.
       OPEN-020.
            MOVE Ws-Forms-Name   TO F-FILENAME
            MOVE Ws-cbForms-name TO F-CBFILENAME.
            MOVE "DrInteMt" TO F-FORMNAME
            MOVE 8          TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
        END-OFF SECTION.
        END-000.
            CLOSE DEBTOR-MASTER
                  PARAMETER-FILE
                  DEBTOR-TRANS-FILE
                  DISTRIBUTIONS.
            EXIT PROGRAM.
        END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldFactor".
       Copy "GetSystemY2KDate".
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
       Copy "WriteDailyExcep1".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
