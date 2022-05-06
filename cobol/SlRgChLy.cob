        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlRgChLy.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrTrans".
         Copy "SelectSlRegLy".
         Copy "SelectStTransLy".
         Copy "SelectSlParameter".
         Copy "SelectSlDaily".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdDrTrans.
           COPY ChlfdRegisterLy.
           COPY ChlfdStTransLy.
           COPY ChlfdParam.
           COPY ChlfdDaily.
           
       WORKING-STORAGE SECTION.
       77  NEW-DEBTORNO        PIC X VALUE " ".      
       77  WS-END              PIC X VALUE " ".      
       77  WS-TYPE             PIC X VALUE " ".      
       77  WS-AMT-OF-INVOICE   PIC S9(8)V99 VALUE 0.
       77  WS-SALEAMT          PIC S9(8)V99 VALUE 0.
       77  WS-COST             PIC S9(8)V99 VALUE 0.
       77  WS-NUMBER           PIC X(6) VALUE " ".      
       77  WS-NUMBER-DIS       PIC Z(5)9.      
       77  WS-ANSWER1          PIC X VALUE " ".      
       77  WS-ANSWER2          PIC X VALUE " ".      
       77  WS-SALES-UPDATE     PIC X VALUE " ".      
       77  WS-VALID            PIC X VALUE " ".      
       77  WS-NUM              PIC 9(6) VALUE 0.
       77  WS-DEBTORNUMBER     PIC 9(7) VALUE 0.
       77  WS-NEWDEBTORNUMBER  PIC 9(7) VALUE 0.
       77  WS-OLDDEBTORNUMBER  PIC 9(7) VALUE 0.
       77  WS-TERM-SUB          PIC 9 VALUE 0.    
       01  SPLIT-TERMOFSALE.
           03  WSTE-CODE        PIC X VALUE " ".
           03  WSTE-REST        PIC X(10) VALUE " ".
       01  STORE-TERM.
         02  WS-TERM-OCCUR OCCURS 10.
           03  WS-ST-TYPE       PIC X.
           03  WS-ST-CODE       PIC X.
           03  WS-ST-TERM       PIC X(11).
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1      PIC 99.
       01  WS-INCR-LY-STATUS.
           03  WS-INCR-LY-ST1     PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1        PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  WS-STTRANSLY-STATUS.
           03  WS-STTRANSLY-ST1     PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1       PIC 99.
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
           PERFORM READ-REGISTER-RECORDS.
           IF WS-VALID NOT = "N"
              PERFORM READ-BACK-ORDERS.
           PERFORM END-000.
           GO TO CONTROL-000.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE 0310 TO POS
           DISPLAY "* TRANSFER L/Y REGISTER RECORDS TO A NEW ACCOUNT *"
           AT POS
           MOVE 0410 TO POS
           DISPLAY "**************************************************"
           AT POS.
       GET-010.
           MOVE 0610 TO POS
           DISPLAY "ENTER OLD ACCOUNT NUMBER : [       ]" AT POS
           ADD 28 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 3         TO CDA-ROW.
           MOVE 37        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-OLDDEBTORNUMBER.

      *     ACCEPT WS-OLDDEBTORNUMBER AT POS.
           IF W-ESCAPE-KEY = 3
               PERFORM END-OFF.
           MOVE WS-OLDDEBTORNUMBER TO DR-ACCOUNT-NUMBER.
           PERFORM READ-DEBTOR.
           IF NEW-DEBTORNO = "Y"
               MOVE "THIS IS AN INVALID ACCOUNT, RE-ENTER."
                TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-011
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-011.
           MOVE 0738 TO POS
           DISPLAY DR-NAME AT POS.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       GET-015.
           MOVE 0910 TO POS
           DISPLAY "ENTER NEW ACCOUNT NUMBER : [       ]" AT POS
           ADD 28 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 7         TO CDA-DATALEN.
           MOVE 6         TO CDA-ROW.
           MOVE 37        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-NEWDEBTORNUMBER.

      *     ACCEPT WS-NEWDEBTORNUMBER AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-010.
           MOVE WS-NEWDEBTORNUMBER TO DR-ACCOUNT-NUMBER.
           PERFORM READ-DEBTOR.
           IF NEW-DEBTORNO = "Y"
               MOVE "THIS IS AN INVALID ACCOUNT, RE-ENTER" TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-016
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-015.
       GET-016.
           MOVE 0838 TO POS
           DISPLAY DR-NAME AT POS.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       GET-020.
           MOVE 1110 TO POS
           DISPLAY "CHANGE ALL REGISTER RECORDS, Y OR N: [ ]" AT POS
           ADD 38 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 8         TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

      *     ACCEPT WS-ANSWER1 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-015.
           IF WS-ANSWER1 NOT = "Y" AND NOT = "N"
               MOVE "ONLY Y OR N ARE VALID ENTRIES, RE-ENTER." 
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-030.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
           IF WS-ANSWER1 = "Y"
               GO TO GET-500.
           MOVE 1310 TO POS
           DISPLAY "ENTER 1=INVOICE, 3=REPAIR, : [ ]" AT POS
           MOVE 1410 TO POS
           DISPLAY "4=P/SLIP, 6=C/NOTES, 8=QUOTATION" AT POS
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
               GO TO GET-020.
           IF WS-TYPE NOT = "1" AND NOT = "3" AND NOT = "4"
                  AND NOT = "6" AND NOT = "8"
               MOVE "ONLY 1, 3, 4, 6 AND 8 ARE VALID ENTRIES, RE-ENTER."
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
       GET-550.
           MOVE 1710 TO POS
           DISPLAY "CHANGE THE ADDRESS TO NEW ACCOUNT ADDRESS : [ ]"
           AT POS
           MOVE 1810 TO POS
           DISPLAY
             "ENTER P=POSTAL ONLY, D=DEL ONLY, N=NO CHANGE, Y=BOTH."
           AT POS
           MOVE  1755 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 54        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER2.

      *     ACCEPT WS-ANSWER2 AT POS.
           IF W-ESCAPE-KEY = 4
            IF WS-ANSWER1 = "Y"
               GO TO GET-020
            ELSE
               GO TO GET-040.
           IF WS-ANSWER2 NOT = "Y" AND NOT = "N"
                     AND NOT = "D" AND NOT = "P"
               MOVE "ONLY D, N, P & Y ARE VALID ENTRIES, RE-ENTER" 
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-550.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-600
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-550.
       GET-600.
           IF WS-TYPE NOT = "1" AND NOT = "6"
              GO TO GET-999.
           MOVE 2001 TO POS
           DISPLAY 
           "DO YOU WISH TO RENAME THE DR-TRANS & MOVE SALES ON " &
           "THE DR-MASTER FILE: [ ]"
           AT POS
           ADD 72 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 72        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SALES-UPDATE.

      *     ACCEPT WS-SALES-UPDATE AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO GET-550.
           IF WS-SALES-UPDATE NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-600.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-999
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-600.
       GET-999.
            EXIT.
      *
       READ-BACK-ORDERS SECTION.
       RBO-000.
            MOVE 2910 TO POS.
            DISPLAY "Changing stock TransLY To New Account...          "
            AT POS.
            IF WS-ANSWER1 = "Y"
              MOVE WS-OLDDEBTORNUMBER TO STTR-LY-ACCOUNT-NUMBER
              START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-ACCOUNT-NUMBER
                INVALID KEY NEXT SENTENCE
            ELSE
                MOVE WS-NUM  TO STTR-LY-REFERENCE1
                MOVE WS-TYPE TO STTR-LY-TYPE
                MOVE 1       TO STTR-LY-TRANSACTION-NUMBER
                START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-KEY
                    INVALID KEY NEXT SENTENCE.
            IF WS-STTRANSLY-ST1 NOT = 0
             IF WS-ANSWER1 = "Y"
               MOVE "THERE ARE NO ST-TRANS RECORDS FOR THAT ACCOUNT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "N" TO WS-VALID
               GO TO RBO-999
             ELSE
               MOVE "THERE ARE NO ST-TRANS RECORDS FOR THAT NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "N" TO WS-VALID
               GO TO RBO-999.
       RBO-002.
            READ STOCK-TRANSLY-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-STTRANSLY-ST1 = 10
               GO TO RBO-999.
            IF WS-STTRANSLY-ST1 NOT = 0
            MOVE "STTRANSLY BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO RBO-002.
            IF WS-ANSWER1 = "N"
             IF STTR-LY-REFERENCE1 NOT = WS-NUM
               GO TO RBO-999.
            IF STTR-LY-ACCOUNT-NUMBER NOT = WS-OLDDEBTORNUMBER
               GO TO RBO-999.
       RBO-005.
            MOVE WS-NEWDEBTORNUMBER TO STTR-LY-ACCOUNT-NUMBER.
            REWRITE STOCK-TRANSLY-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-STTRANSLY-ST1 NOT = 0
               MOVE STTR-LY-KEY               TO WS-DAILY-1ST
               MOVE STTR-LY-ACCOUNT-NUMBER    TO WS-DAILY-2ND
               MOVE "NO CHNG TO ST-TRANS "    TO WS-DAILY-3RD
               MOVE WS-NEWDEBTORNUMBER        TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RBO-002.
       RBO-999.
           EXIT.
      *
       READ-REGISTER-RECORDS SECTION.
       RRR-000.
            MOVE 2910 TO POS.
            DISPLAY "Changing RegisterLY To new Account...             "
               AT POS.
            IF WS-ANSWER1 = "Y"
                MOVE WS-OLDDEBTORNUMBER TO INCR-LY-ACCOUNT
                MOVE " "                TO INCR-LY-PORDER
                START INCR-LY-REGISTER KEY NOT < INCR-LY-ALT-KEY
                     INVALID KEY NEXT SENTENCE
            ELSE
                MOVE WS-NUM  TO INCR-LY-INVOICE
                MOVE WS-TYPE TO INCR-LY-TRANS
                START INCR-LY-REGISTER KEY NOT < INCR-LY-KEY
                     INVALID KEY NEXT SENTENCE.
            IF WS-INCR-LY-ST1 NOT = 0
             IF WS-ANSWER1 = "Y"
               MOVE "THERE ARE NO REGISTER RECORDS FOR THAT ACCOUNT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "N" TO WS-VALID
               GO TO RRR-999
             ELSE
               MOVE "THAT IS NOT A VALID RECORD FOR THAT ACCOUNT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "N" TO WS-VALID
               GO TO RRR-999.
       RRR-002.
            READ INCR-LY-REGISTER NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-INCR-LY-ST1 = 10
               GO TO RRR-999.
            IF WS-INCR-LY-ST1 NOT = 0
             MOVE "REG-LY BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-INCR-LY-ST1
               GO TO RRR-002.
            IF INCR-LY-ACCOUNT NOT = WS-OLDDEBTORNUMBER
               GO TO RRR-999.
       RRR-010.
            IF WS-ANSWER2 = "Y" OR = "P"
               MOVE DR-ADDRESS1     TO INCR-LY-ADD1
               MOVE DR-ADDRESS2     TO INCR-LY-ADD2
               MOVE DR-ADDRESS3     TO INCR-LY-ADD3
               MOVE DR-POST-CODE    TO INCR-LY-CODE.
            IF WS-ANSWER2 = "Y" OR = "D"
               MOVE DR-DEL-ADDRESS1 TO INCR-LY-DEL1
               MOVE DR-DEL-ADDRESS2 TO INCR-LY-DEL2
               MOVE DR-DEL-ADDRESS3 TO INCR-LY-DEL3.
            IF WS-ANSWER2 = "Y"
               MOVE DR-TELEPHONE             TO INCR-LY-PHONE
               MOVE DR-TERMS-CODE            TO WS-TERM-SUB
               MOVE WS-ST-TERM (WS-TERM-SUB) TO INCR-LY-TERMS.
            MOVE DR-NAME                     TO INCR-LY-NAME.
            MOVE WS-NEWDEBTORNUMBER          TO INCR-LY-ACCOUNT.
            REWRITE INCR-LY-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-INCR-LY-ST1 NOT = 0
               MOVE INCR-LY-KEY               TO WS-DAILY-1ST
               MOVE INCR-LY-ALT-KEY           TO WS-DAILY-2ND
               MOVE "NO CHNGE TO REGISTER"    TO WS-DAILY-3RD
               MOVE WS-NEWDEBTORNUMBER        TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
       RRR-020.
           IF WS-SALES-UPDATE = "Y"
              PERFORM READ-DR-TRANS.
               
            IF WS-ANSWER1 = "Y"
               GO TO RRR-002.
       RRR-999.
           EXIT.
      *
       READ-DEBTOR SECTION.
       R-DR-000.
             START DEBTOR-MASTER KEY NOT < DR-KEY
                 INVALID KEY NEXT SENTENCE.
       R-DR-010.
             READ DEBTOR-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE "Y" TO NEW-DEBTORNO
                MOVE WS-DEBTORNUMBER TO DR-ACCOUNT-NUMBER
                GO TO R-DR-999.
             IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO R-DR-010.
             MOVE "N" TO NEW-DEBTORNO.
       R-DR-999.
             EXIT.
      *
       START-DEBTOR SECTION.
       DR-DR-000.
              MOVE WS-DEBTORNUMBER TO DR-ACCOUNT-NUMBER.
              START DEBTOR-MASTER KEY NOT LESS DR-ACCOUNT-NUMBER.
       DR-DR-999.
             EXIT.
      *
       READ-DR-TRANS SECTION.
       RDRTR-000.
           MOVE WS-TYPE         TO DRTR-TYPE
           MOVE INCR-LY-DRTRANS-NO TO DRTR-TRANSACTION-NUMBER.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE
              "NO SUCH DRTRANS NUMBER ON START, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDRTR-999.
       RDRTR-002.
           READ DEBTOR-TRANS-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
               GO TO RDRTR-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DRTRANS BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO RDRTR-002.
            MOVE WS-NEWDEBTORNUMBER TO DRTR-ACCOUNT-NUMBER.
       RDRTR-020.
           REWRITE DEBTOR-TRANS-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
               GO TO RDRTR-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DR-TRANS FILE BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO RDRTR-020.
       RDRTR-030.
           IF WS-TYPE = "6"
               GO TO RDRTR-035.
           COMPUTE WS-SALEAMT =
               INCR-LY-INVCRED-AMT - INCR-LY-TAX - INCR-LY-ADDONS.
           MOVE DRTR-AMT-OUTSTANDING TO WS-AMT-OF-INVOICE.
           MOVE INCR-LY-INVCRED-COST TO WS-COST.
           MOVE WS-NEWDEBTORNUMBER   TO DR-ACCOUNT-NUMBER.
           PERFORM UPDATE-DEBTOR.
           COMPUTE WS-SALEAMT = WS-SALEAMT * -1.
           COMPUTE WS-AMT-OF-INVOICE = WS-AMT-OF-INVOICE * -1.
           COMPUTE WS-COST = WS-COST * -1.

           GO TO RDRTR-050.
       RDRTR-035.
           COMPUTE WS-SALEAMT =
              (INCR-LY-INVCRED-AMT - INCR-LY-TAX - INCR-LY-ADDONS)
                  * -1.
           MOVE DRTR-AMT-OUTSTANDING TO WS-AMT-OF-INVOICE.
           MOVE INCR-LY-INVCRED-COST TO WS-COST.
           MOVE WS-NEWDEBTORNUMBER   TO DR-ACCOUNT-NUMBER.
           COMPUTE WS-AMT-OF-INVOICE = WS-AMT-OF-INVOICE * -1.
           COMPUTE WS-COST = WS-COST * -1.
           PERFORM UPDATE-DEBTOR.
           COMPUTE WS-SALEAMT = WS-SALEAMT * -1.
           COMPUTE WS-AMT-OF-INVOICE = WS-AMT-OF-INVOICE * -1.
           COMPUTE WS-COST = WS-COST * -1.
       RDRTR-050.
           MOVE WS-OLDDEBTORNUMBER TO DR-ACCOUNT-NUMBER.
           PERFORM UPDATE-DEBTOR.
       RDRTR-999.
           EXIT.
      *
       UPDATE-DEBTOR SECTION.
       UP-DR-001.
             START DEBTOR-MASTER KEY NOT < DR-KEY
                 INVALID KEY NEXT SENTENCE.
       UP-DR-010.
             READ DEBTOR-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                GO TO UP-DR-999.
             IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO UP-DR-010.
       UP-DR-020.
           IF WS-OLDDEBTORNUMBER = DR-ACCOUNT-NUMBER
            IF DR-DATE-LAST-SALE < INCR-LY-DATE
               MOVE INCR-LY-DATE TO DR-DATE-LAST-SALE.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE INCR-LY-DATE TO WS-AGE-DATE.
           IF WS-AGE-YY NOT = WS-YY
               COMPUTE WS-MM = (((WS-YY - WS-AGE-YY) * 12)
                                   + WS-MM).
           SUBTRACT WS-AGE-MM FROM WS-MM.

           IF WS-MM = 0
               ADD WS-AMT-OF-INVOICE TO DR-CURRENT
                                        DR-BALANCE
               ADD WS-SALEAMT        TO DR-SALES-LAST
               GO TO UP-DR-030.
           IF WS-MM = 1
               ADD WS-AMT-OF-INVOICE TO DR-30DAY
                                        DR-BAL-LAST-STATE
                                        DR-BALANCE
               ADD WS-SALEAMT        TO DR-SALES-LAST
               GO TO UP-DR-030.
           IF WS-MM = 2
               ADD WS-AMT-OF-INVOICE TO DR-60DAY
                                        DR-BAL-LAST-STATE
                                        DR-BALANCE
               ADD WS-SALEAMT        TO DR-SALES-LAST
               GO TO UP-DR-030.
           IF WS-MM = 3
               ADD WS-AMT-OF-INVOICE TO DR-90DAY
                                        DR-BAL-LAST-STATE
                                        DR-BALANCE
               ADD WS-SALEAMT        TO DR-SALES-LAST
               GO TO UP-DR-030.
           IF WS-MM > 3
               ADD WS-AMT-OF-INVOICE TO DR-120DAY
                                        DR-BAL-LAST-STATE
                                        DR-BALANCE
               ADD WS-SALEAMT        TO DR-SALES-LAST.
       UP-DR-030.
             REWRITE DEBTOR-RECORD
                 INVALID KEY NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                GO TO UP-DR-999.
             IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO UP-DR-030.
       UP-DR-999.
           EXIT.
      *
       READ-TERMS-FILE SECTION.
       RTERM-000.
            MOVE 1 TO SUB-1
                      PA-RECORD.
            MOVE 2 TO PA-TYPE.
       RTERM-010.
            READ PARAMETER-FILE NEXT
                AT END NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 10
                GO TO RTERM-999.
            IF PA-TYPE < 2
                GO TO RTERM-010.
            IF PA-TYPE > 2
                GO TO RTERM-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER TERMS BUSY, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RTERM-010.
            IF PARAMETER-REC = "           "
               GO TO RTERM-010.           
            MOVE PARAMETER-REC TO WS-TERM-OCCUR (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 = 10
               PERFORM ERROR-020
               GO TO RTERM-999.
            GO TO RTERM-010.
       RTERM-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO OPEN-000.
       OPEN-006.
            OPEN I-O STOCK-TRANSLY-FILE.
            IF WS-STTRANSLY-ST1 NOT = 0
               MOVE 0 TO WS-STTRANSLY-ST1
               MOVE "STTRANSLY FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-006.
       OPEN-008.
            OPEN I-O INCR-LY-REGISTER.
            IF WS-INCR-LY-ST1 NOT = 0
               MOVE 0 TO WS-INCR-LY-ST1
               MOVE "REGISTERLY-FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-008.
       OPEN-014.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-SLPARAMETER-ST1
              MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-014.
           MOVE ALL "X" TO STORE-TERM.
           PERFORM READ-TERMS-FILE.
           CLOSE PARAMETER-FILE.
       OPEN-030.
           OPEN I-O DEBTOR-TRANS-FILE.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-DRTRANS-ST1
               MOVE "DR-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-030.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE DEBTOR-MASTER
                 DEBTOR-TRANS-FILE
                 INCR-LY-REGISTER
                 STOCK-TRANSLY-FILE.
       END-900.
      *      STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
      ******************
      *Mandatory Copies*
      ******************
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "WriteDailyExcep1".
       Copy "CTOSCobolAccept".
      * END-OF-JOB
