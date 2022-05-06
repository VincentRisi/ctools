        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrLablRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
           SELECT PRINT-FILE ASSIGN TO "/ctools/spl/LabelPrint"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SPL-STATUS.
           Select HIGH-File Assign To WS-HIGH-FILE
               Organization Is Indexed
               Access Mode Is Dynamic
               File Status Is Ws-RANDOM-Status
               Record Key Is HIGH-Key.
           Select RANDOM-File Assign To WS-RANDOM-FILE
               Organization Is Indexed
               Access Mode Is Dynamic
               File Status Is Ws-RANDOM-Status
               Record Key Is RANDOM-Key.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
       FD  RANDOM-FILE.
       01  RANDOM-REC.
           03  RANDOM-KEY.
              05  RANDOM-NUMBER     PIC 9(7).
              05  RANDOM-INDEX      PIC 9(5).
           03  RANDOM-ACCOUNT       PIC 9(7).
           
       FD  HIGH-FILE.
       01  HIGH-REC.
           03  HIGH-KEY.
              05  HIGH-NUMBER     PIC 9(7).
           03  HIGH-ACCOUNT       PIC 9(7).
       FD  PRINT-FILE.
       01  PRINT-REC               PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-RANDOM-FILE       PIC X(35) VALUE
              "/ctools/spl/RandomHighDrLabels".
       77  WS-RANDOM-FILE-ind   PIC X(35) VALUE
              "/ctools/spl/RandomHighDrLabels.Ind".
       77  WS-High-FILE         PIC X(35) VALUE
              "/ctools/spl/DebtorHighLabel".
       77  WS-High-FILE-ind     PIC X(35) VALUE
              "/ctools/spl/DebtorHighLabel.Ind".
       77  WS-RANDOM-WRITTEN    PIC X.
       77  WS-TOP               PIC X(5) VALUE " ".
       77  WS-NO-ACC            PIC 9(4) VALUE 0.
       77  WS-NO-PRINTED        PIC 9(4) VALUE 0.
       77  WS-FOUND             PIC X VALUE " ".
       77  W-CALC               PIC 9(8) VALUE 0.
       77  COL-CNT              PIC 9(3) VALUE 0.
       77  WS-ACCNOBEGIN        PIC 9(7) VALUE 0.
       77  WS-ACCNOEND          PIC 9(7) VALUE 0.
       77  WS-END-OF-FILE       PIC X VALUE " ".
       77  WS-PRINT-BY-HIGH     PIC X VALUE " ".
       77  WS-MARGIN-SALES      PIC X VALUE " ".
       77  WS-TOTAL-SALES       PIC S9(9)V99 VALUE 0.
       77  WS-MARGIN            PIC S9(9)V99 VALUE 0.
       77  WS-DISPLAY-PRINTED   PIC Z(4)9.
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-TOP-DIS           PIC Z(4)9.
       77  WS-SALES-CODE        PIC 99 VALUE 0.
       77  WS-PERIOD            PIC X VALUE " ".
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-RANGE1            PIC 9(7) VALUE 0.
       77  WS-RANGE2            PIC 9(7) VALUE 0.
       77  WS-RANGE3            PIC 9(2) VALUE 0.
       77  WS-PRINT-COD         PIC X VALUE " ".
       77  WS-SUPPLY            PIC X VALUE " ".
       77  WS-CONTACT           PIC X VALUE " ".
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-Spl-STATUS.
           03  WS-Spl-ST1       PIC 99.
       01  WS-RANDOM-STATUS.
           03  WS-RANDOM-ST1    PIC 99.
       01  PLINE1.
           03  PNAME1           PIC X(37) VALUE " ".
           03  FILLER           PIC X(2) VALUE " ".
           03  PNAME2           PIC X(37) VALUE " ".
           03  FILLER           PIC X(2) VALUE " ".
           03  PNAME3           PIC X(37) VALUE " ".
           03  FILLER           PIC X(9) VALUE " ".
       01  PLINE2.
           03  PADD11           PIC X(39) VALUE " ".
           03  PADD12           PIC X(39) VALUE " ".
           03  PADD13           PIC X(39) VALUE " ".
           03  FILLER           PIC X(9) VALUE " ".
       01  PLINE3.
           03  PADD21           PIC X(39) VALUE " ".
           03  PADD22           PIC X(39) VALUE " ".
           03  PADD23           PIC X(39) VALUE " ".
           03  FILLER           PIC X(9) VALUE " ".
       01  PLINE4.
           03  PADD31           PIC X(39) VALUE " ".
           03  PADD32           PIC X(39) VALUE " ".
           03  PADD33           PIC X(39) VALUE " ".
           03  FILLER           PIC X(9) VALUE " ".
       01  PLINE5.
           03  PADD41           PIC X(39) VALUE " ".
           03  PADD42           PIC X(39) VALUE " ".
           03  PADD43           PIC X(39) VALUE " ".
           03  FILLER           PIC X(9) VALUE " ".
       01  PLINE6.
           03  PCONTACT1        PIC X(39) VALUE " ".
           03  PCONTACT2        PIC X(39) VALUE " ".
           03  PCONTACT3        PIC X(39) VALUE " ".
           03  FILLER           PIC X(9) VALUE " ".
       Copy "WsDateInfo".
       Copy "WStore".
      *
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
           PERFORM Open-Files.
           PERFORM CLEAR-SCREEN
           PERFORM DISPLAY-FORM.
       CONTROL-010.
           PERFORM DELETE-TRANS.
           PERFORM OPEN-036
           PERFORM OPEN-038
           PERFORM GET-DATA.
           IF WS-PRINT-BY-HIGH = "Y"
              PERFORM READ-ALL-ACCOUNTS
              PERFORM READ-RANDOM-FILE.
           
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           IF WS-SPL-ST1 NOT = 0
               MOVE 0 TO WS-SPL-ST1
               MOVE "THE PRINT FILE IS ALREADY OPEN, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM END-OFF.
           MOVE WTELL-PAUSE TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC.
           PERFORM PRINT-LABELS.
       CONTROL-020.
           PERFORM ERROR1-020
           PERFORM ERROR-020
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
           PERFORM CHECK-SPOOLER.
           IF WS-PRINT-BY-HIGH = "Y"
              PERFORM DELETE-TRANS.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-030.
            MOVE 0 TO WS-ACCNOBEGIN
                      WS-ACCNOEND.
            MOVE "RANGE1" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF F-EXIT-CH = X"0A"
               GO TO GET-035
            ELSE
               GO TO GET-030.
       GET-035.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-ACCNOBEGIN.
            IF WS-ACCNOBEGIN NOT > 0
                GO TO GET-040.
       GET-040.
            MOVE "RANGE2" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-030.
            IF F-EXIT-CH = X"0A"
               GO TO GET-045
            ELSE
               GO TO GET-040.
       GET-045.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-ACCNOEND.
            IF WS-ACCNOEND < WS-ACCNOBEGIN
                GO TO GET-040.
       GET-050.
            MOVE "RANGE3" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-040.
            IF F-EXIT-CH = X"1B" OR = X"0A"
               GO TO GET-055
            ELSE
               GO TO GET-050.
       GET-055.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-SALES-CODE.
            IF WS-SALES-CODE < 0 OR > 99
               GO TO GET-050.
       GET-080.
            MOVE "RANGE4" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-050.
            IF F-EXIT-CH = X"0A" OR = X"1B"
               GO TO GET-085
            ELSE
               GO TO GET-080.
       GET-085.
            MOVE 1 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO WS-PRINT-COD.
            IF WS-PRINT-COD NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-080.
       GET-090.
            MOVE "RANGE5" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-080.
            IF F-EXIT-CH = X"0A" OR = X"1B"
               GO TO GET-095
            ELSE
               GO TO GET-090.
       GET-095.
            MOVE 1 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO WS-SUPPLY.
            IF WS-SUPPLY NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-090.
       GET-100.
            MOVE "RANGE6" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-090.
            IF F-EXIT-CH = X"0A" OR = X"1B"
               GO TO GET-105
            ELSE
               GO TO GET-100.
       GET-105.
      *WS-CONTACT A=ACCOUNT
      *           N=NO CONTACT PRINT
      *           S=SALES
            MOVE 1 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO WS-CONTACT.
            IF WS-CONTACT NOT = "A" AND NOT = "S" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-100.
       GET-120.
            MOVE "RANGE7" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-100.
            IF F-EXIT-CH = X"0A" OR = X"1B"
               GO TO GET-125
            ELSE
               GO TO GET-120.
       GET-125.
            MOVE 1 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO WS-PRINT-BY-HIGH.
            IF WS-PRINT-BY-HIGH NOT = "Y" AND NOT = "N"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-120.
            IF WS-PRINT-BY-HIGH = "N"
               GO TO GET-999.
       GET-130.
            MOVE "RANGE8" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-120.
            IF F-EXIT-CH = X"0A" OR = X"1B"
               GO TO GET-135
            ELSE
               GO TO GET-130.
       GET-135.
            MOVE 4 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-NO-ACC.
            
            IF WS-NO-ACC NOT > 0
                GO TO GET-130.
            MOVE "RANGE8"   TO F-FIELDNAME.
            MOVE 6          TO F-CBFIELDNAME.
            MOVE WS-NO-ACC  TO F-EDNAMEFIELDCHANGE.
            MOVE 4          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-CHANGE.
       GET-140.
            MOVE "RANGE9" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-130.
            IF F-EXIT-CH = X"0A" OR = X"1B"
               GO TO GET-145
            ELSE
               GO TO GET-140.
       GET-145.
            MOVE 1 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO WS-PERIOD.
            IF WS-PERIOD NOT = "T" AND NOT = "L" AND NOT = "B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-140.
       GET-150.
            MOVE "RANGE0" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-140.
            IF F-EXIT-CH = X"0A" OR = X"1B"
               GO TO GET-155
            ELSE
               GO TO GET-150.
       GET-155.
            MOVE 1 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO WS-MARGIN-SALES.
            IF WS-MARGIN-SALES NOT = "P" AND NOT = "S"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-150.
       GET-999.
            EXIT.
      *
       PRINT-LABELS SECTION.
       PR-000.
           MOVE 2510 TO POS
           DISPLAY "The Report is being compiled........." AT POS.
           
           IF WS-PRINT-BY-HIGH = "N"
              MOVE WS-ACCNOBEGIN TO DR-ACCOUNT-NUMBER
              START DEBTOR-MASTER KEY NOT < DR-KEY
              MOVE " " TO WS-MESSAGE
              PERFORM ERROR-020
              GO TO PR-010.

           PERFORM ERROR1-020.
           MOVE 2510 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 2910 TO POS.
           DISPLAY "Processing of HIGHEST accounts in progress." AT POS.
           MOVE 0 TO HIGH-NUMBER WS-NO-PRINTED
           START HIGH-FILE KEY NOT < HIGH-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "BAD START ON HIGH-FILE" TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              EXIT PROGRAM.
       PR-010.
           IF WS-END-OF-FILE = " "
            IF WS-PRINT-BY-HIGH = "N"
               PERFORM READ-MASTER
            ELSE
               PERFORM PRINT-ROUTINE.
       PR-020.
           WRITE PRINT-REC FROM PLINE1 AFTER 1
           MOVE " " TO PRINT-REC PLINE1
           WRITE PRINT-REC FROM PLINE2 AFTER 1
           MOVE " " TO PRINT-REC PLINE2
           WRITE PRINT-REC FROM PLINE3 AFTER 1
           MOVE " " TO PRINT-REC PLINE3
           WRITE PRINT-REC FROM PLINE4 AFTER 1
           MOVE " " TO PRINT-REC PLINE4
           WRITE PRINT-REC FROM PLINE5 AFTER 1
           MOVE " " TO PRINT-REC PLINE5
           WRITE PRINT-REC FROM PLINE6 AFTER 2
           MOVE " " TO PRINT-REC PLINE6
           WRITE PRINT-REC AFTER 2.
           IF WS-END-OF-FILE = " "
              GO TO PR-010.
       PR-900.
           MOVE 2510 TO POS
           DISPLAY "                                         " AT POS.
           CLOSE DEBTOR-MASTER.
           MOVE WTELL-PAUSE TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       PR-999.
           EXIT.
      *
       READ-MASTER SECTION.
       RM-000.
           READ DEBTOR-MASTER NEXT
              AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
              PERFORM END-OFF.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE 
               "DEBTOR BUSY ON READ-NEXT1, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
              GO TO RM-000.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           IF DR-ACCOUNT-NUMBER < WS-ACCNOBEGIN
              GO TO RM-000.
           IF DR-ACCOUNT-NUMBER > WS-ACCNOEND
              MOVE "1" TO WS-END-OF-FILE
              MOVE " " TO PNAME1 PNAME2 PNAME3
                          PADD11 PADD12 PADD13
                          PADD21 PADD22 PADD23
                          PADD31 PADD32 PADD33
                          PADD41 PADD42 PADD43
              GO TO RM-999.
              
           MOVE 2610 TO POS
           DISPLAY "Account Number :" AT POS
           ADD 17 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
              
           IF WS-SALES-CODE NOT = 0
              IF WS-SALES-CODE NOT = DR-SALES-ANALYSIS
                 GO TO RM-000.
           IF WS-PRINT-COD = "N"
            IF DR-TERMS-CODE = "2"
                 GO TO RM-000.
           IF WS-SUPPLY = "N"
            IF DR-SUPPLY-Y-N NOT = "Y"
                 GO TO RM-000.

           MOVE DR-NAME     TO PNAME1
           MOVE DR-ADDRESS1 TO PADD11
           MOVE DR-ADDRESS2 TO PADD21.
           IF DR-ADDRESS3 = " "
               MOVE DR-POST-CODE TO PADD31
               MOVE " " TO PADD41
               GO TO RM-010
           ELSE
               MOVE DR-ADDRESS3 TO PADD31.
           MOVE DR-POST-CODE TO PADD41.
           IF WS-CONTACT = "A"
              MOVE DR-ACCOUNTS-CONTACT TO PCONTACT1.
           IF WS-CONTACT = "N"
              MOVE "TECHNICAL MANAGER" TO PCONTACT1.
           IF WS-CONTACT = "S"
              MOVE DR-SALES-CONTACT    TO PCONTACT1.
       RM-010.
           READ DEBTOR-MASTER NEXT
              AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
              OR DR-ACCOUNT-NUMBER > WS-ACCNOEND
                 MOVE "1" TO WS-END-OF-FILE
                 MOVE " " TO PNAME2 PNAME3
                             PADD12 PADD13
                             PADD22 PADD23
                             PADD32 PADD33
                             PADD42 PADD43
                 GO TO RM-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE 
               "DEBTOR BUSY ON READ-NEXT2, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               GO TO RM-010.
              
           MOVE 2610 TO POS
           DISPLAY "Account Number :" AT POS
           ADD 17 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.

           IF WS-SALES-CODE NOT = 0
              IF WS-SALES-CODE NOT = DR-SALES-ANALYSIS
                 GO TO RM-010.
           IF WS-PRINT-COD = "N"
            IF DR-TERMS-CODE = "2"
                 GO TO RM-010.
           IF WS-SUPPLY = "N"
            IF DR-SUPPLY-Y-N NOT = "Y"
                 GO TO RM-010.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.

           MOVE DR-NAME     TO PNAME2
           MOVE DR-ADDRESS1 TO PADD12
           MOVE DR-ADDRESS2 TO PADD22.
           IF DR-ADDRESS3 = " "
               MOVE DR-POST-CODE TO PADD32
               MOVE " "          TO PADD42
               GO TO RM-020
           ELSE
               MOVE DR-ADDRESS3 TO PADD32.
           MOVE DR-POST-CODE    TO PADD42.
           IF WS-CONTACT = "A"
              MOVE DR-ACCOUNTS-CONTACT TO PCONTACT2.
           IF WS-CONTACT = "N"
              MOVE "TECHNICAL MANAGER" TO PCONTACT2.
           IF WS-CONTACT = "S"
              MOVE DR-SALES-CONTACT    TO PCONTACT2.
       RM-020.
           READ DEBTOR-MASTER NEXT
              AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
              OR DR-ACCOUNT-NUMBER > WS-ACCNOEND
                 MOVE "1" TO WS-END-OF-FILE
                 MOVE " " TO PNAME3
                             PADD13
                             PADD23
                             PADD33
                             PADD43
                 GO TO RM-999.
           IF WS-DEBTOR-ST1 NOT = 0
                MOVE 
               "DEBTOR BUSY ON READ-NEXT3, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RM-020.
              
           MOVE 2610 TO POS
           DISPLAY "Account Number :" AT POS
           ADD 17 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.

           IF WS-SALES-CODE NOT = 0
              IF WS-SALES-CODE NOT = DR-SALES-ANALYSIS
                 GO TO RM-020.
           IF WS-PRINT-COD = "N"
            IF DR-TERMS-CODE = "2"
                 GO TO RM-020.
           IF WS-SUPPLY = "N"
            IF DR-SUPPLY-Y-N NOT = "Y"
                 GO TO RM-020.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.

           MOVE DR-NAME     TO PNAME3
           MOVE DR-ADDRESS1 TO PADD13
           MOVE DR-ADDRESS2 TO PADD23.
           IF DR-ADDRESS3 = " "
               MOVE DR-POST-CODE TO PADD33
               MOVE " " TO PADD43
               GO TO RM-999
           ELSE
               MOVE DR-ADDRESS3 TO PADD33.
           MOVE DR-POST-CODE TO PADD43.
           IF WS-CONTACT = "A"
              MOVE DR-ACCOUNTS-CONTACT TO PCONTACT3.
           IF WS-CONTACT = "N"
              MOVE "TECHNICAL MANAGER" TO PCONTACT3.
           IF WS-CONTACT = "S"
              MOVE DR-SALES-CONTACT    TO PCONTACT3.
       RM-999.
           EXIT.
      *
       CHECK-SPOOLER SECTION.
       CP-000.
           PERFORM OPEN-SPOOLER-FILES
           PERFORM QUEUE-PRINT-FILE
           MOVE SPACE TO W-SPOOLST
           MOVE SPACE TO W-SPOOLST2
           PERFORM CHECK-FOR-PAUSE.
             
           MOVE "Load Gum Labels, Then Press 'ESC'." TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.

           PERFORM SEND-CONTROL-CHAR.
      *
      * PRINTING NOW STARTED
      *
           PERFORM CHECK-PAUSE-PRINT.
       
          MOVE "Load Normal Paper, Then Press 'ESC' To Exit Program."
           TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.

           PERFORM SEND-CONTROL-CHAR.
       CP-999.
           EXIT.
      *
       READ-ALL-ACCOUNTS SECTION.
       RAA-001.
           MOVE 2910 TO POS
           DISPLAY "Reading ALL debtors for GROSS totals." AT POS
           START DEBTOR-MASTER KEY NOT < DR-KEY.
       RAA-005.
           READ DEBTOR-MASTER NEXT
               AT END NEXT SENTENCE.
               
           IF WS-DEBTOR-ST1 = 10
               GO TO RAA-900.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE
           "ACC NUMBER LOCKED AT ANOTHER TERMINAL, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RAA-005.
           IF WS-PERIOD = "B"
              ADD DR-BALANCE    TO WS-TOTAL-SALES.
           IF WS-PERIOD = "T"
              ADD DR-SALES-YTD  TO WS-TOTAL-SALES.
           IF WS-PERIOD = "L"
              ADD DR-SALES-LAST TO WS-TOTAL-SALES.
              
           MOVE 2510 TO POS
           DISPLAY "ACCOUNT BEING READ:" AT POS
           ADD 20 TO POS
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
 
               
           IF WS-PERIOD = "B"
               MOVE DR-BALANCE    TO RANDOM-NUMBER.
               
           IF WS-PERIOD = "T"
            IF WS-MARGIN-SALES = "S"
               MOVE DR-SALES-YTD  TO RANDOM-NUMBER
            ELSE
               COMPUTE WS-MARGIN = DR-SALES-YTD - DR-COST-YTD
               MOVE WS-MARGIN     TO RANDOM-NUMBER.
               
           IF WS-PERIOD = "L"
            IF WS-MARGIN-SALES = "S"
               MOVE DR-SALES-LAST TO RANDOM-NUMBER
            ELSE
               COMPUTE WS-MARGIN = DR-SALES-LAST - DR-COST-LAST
               MOVE WS-MARGIN     TO RANDOM-NUMBER.
               
           IF RANDOM-NUMBER > 0
               MOVE 1 TO RANDOM-INDEX
               PERFORM WRITE-RANDOM-RECORD.
           GO TO RAA-005.
       RAA-900.
           CLOSE DEBTOR-MASTER.
           PERFORM OPEN-000.
       RAA-999.
           EXIT.
      *
       READ-RANDOM-FILE SECTION.
       RRF-000.
           PERFORM ERROR1-020.
           IF WS-RANDOM-WRITTEN NOT = "Y"
              MOVE "NOTHING TO PRINT IN THAT RANGE." TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              CLOSE DEBTOR-MASTER
              EXIT PROGRAM.
           CLOSE RANDOM-FILE.
           PERFORM OPEN-035.
           MOVE 2910 TO POS
           DISPLAY "READING ACCOUNTS BY LOWEST VALUES.          " AT POS.
           MOVE 99999 TO HIGH-NUMBER.
           MOVE 0     TO RANDOM-NUMBER
                         RANDOM-INDEX.
           START RANDOM-FILE KEY NOT < RANDOM-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "BAD START ON RANDOM READ RRF-000, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              EXIT PROGRAM.
       RRF-005.
           READ RANDOM-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-RANDOM-ST1 = 10
              GO TO RRF-999.
           IF WS-RANDOM-ST1 NOT = 0
               MOVE "RANDOM BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-RANDOM-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               GO TO RRF-005.
              
           SUBTRACT 1 FROM HIGH-NUMBER
              
           MOVE 2510 TO POS
           DISPLAY "RANDOM NUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY RANDOM-ACCOUNT AT POS
           ADD 10 TO POS
           DISPLAY "COUNT DOWN" AT POS
           ADD 11 TO POS
           DISPLAY HIGH-NUMBER AT POS.
           
           PERFORM WRITE-HIGH-RECORD.
           GO TO RRF-005.
       RRF-999.
           EXIT.
      *
       WRITE-RANDOM-RECORD SECTION.
       WRR-005.
           IF WS-RANDOM-WRITTEN NOT = "Y"
               MOVE "Y" TO WS-RANDOM-WRITTEN.
           MOVE DR-ACCOUNT-NUMBER TO RANDOM-ACCOUNT.
           WRITE RANDOM-REC
              INVALID KEY NEXT SENTENCE.
              
           IF WS-RANDOM-ST1 NOT = 0
              ADD 1 TO RANDOM-INDEX
              GO TO WRR-005.
              
           GO TO WRR-999.
              
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "RANDOM RECORD INVALID ON WRITE, 'ESC' TO EXIT"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
       WRR-999.
            EXIT.
      *
       WRITE-HIGH-RECORD SECTION.
       WRR-005.
           MOVE RANDOM-ACCOUNT TO HIGH-ACCOUNT.
           WRITE HIGH-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "HIGH RECORD INVALID ON WRITE, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
       WRR-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-006.
           READ HIGH-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-RANDOM-ST1 = 10
              MOVE "1" TO WS-END-OF-FILE
              MOVE " " TO PNAME1 PNAME2 PNAME3
                          PADD11 PADD12 PADD13
                          PADD21 PADD22 PADD23
                          PADD31 PADD32 PADD33
                          PADD41 PADD42 PADD43
               GO TO PRR-999.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "RANDOM BUSY ON READ-NEXT, 'ESC' TO SEE MESSAGES."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 3010 TO POS
              DISPLAY "DEBTOR RECORD BUSY PRR-006 :" AT POS
              ADD 28 TO POS
              DISPLAY DR-ACCOUNT-NUMBER AT POS
              ADD 20 TO POS
              DISPLAY WS-RANDOM-ST1 AT POS
              ADD 5 TO POS
              PERFORM ERROR-010
              MOVE 0 TO WS-RANDOM-ST1
              GO TO PRR-006.

           MOVE 2610 TO POS
           DISPLAY "DEBTOR NUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY HIGH-ACCOUNT AT POS.
           
           MOVE HIGH-ACCOUNT TO DR-ACCOUNT-NUMBER
           PERFORM READ-DEBTOR.

           IF WS-PERIOD = "B"
            IF DR-BALANCE NOT > 0
              GO TO PRR-006.
           IF WS-PERIOD = "T"
            IF DR-SALES-YTD < 0
              GO TO PRR-006.
           IF WS-PERIOD = "L"
            IF DR-SALES-LAST < 0
              GO TO PRR-006.
              
           IF WS-SALES-CODE NOT = 0
              IF WS-SALES-CODE NOT = DR-SALES-ANALYSIS
                 GO TO PRR-006.
           IF WS-PRINT-COD = "N"
            IF DR-TERMS-CODE = "2"
                 GO TO PRR-006.
           IF WS-SUPPLY = "N"
            IF DR-SUPPLY-Y-N NOT = "Y"
                 GO TO PRR-006.
           MOVE DR-NAME     TO PNAME1
           MOVE DR-ADDRESS1 TO PADD11
           MOVE DR-ADDRESS2 TO PADD21.
           IF DR-ADDRESS3 = " "
               MOVE DR-POST-CODE TO PADD31
               MOVE " " TO PADD41
           ELSE
               MOVE DR-ADDRESS3 TO PADD31.
           MOVE DR-POST-CODE TO PADD41.
           IF WS-CONTACT = "A"
              MOVE DR-ACCOUNTS-CONTACT TO PCONTACT1.
           IF WS-CONTACT = "N"
              MOVE "TECHNICAL MANAGER" TO PCONTACT1.
           IF WS-CONTACT = "S"
              MOVE DR-SALES-CONTACT    TO PCONTACT1.
              
           ADD 1              TO WS-NO-PRINTED.
           MOVE 3010 TO POS
           DISPLAY "TOTAL NUMBER OF ACCOUNTS READ:" AT POS
           MOVE WS-NO-PRINTED TO WS-DISPLAY-PRINTED
           ADD 31 TO POS
           DISPLAY WS-DISPLAY-PRINTED AT POS.
           IF WS-NO-PRINTED = WS-NO-ACC
               MOVE "1" TO WS-END-OF-FILE
               GO TO PRR-999.
       PRR-010.
           READ HIGH-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-RANDOM-ST1 = 10
                 MOVE "1" TO WS-END-OF-FILE
                 MOVE " " TO PNAME2 PNAME3
                             PADD12 PADD13
                             PADD22 PADD23
                             PADD32 PADD33
                             PADD42 PADD43
                 GO TO PRR-999.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "HIGH-FILE BUSY ON READ-NEXT, 'ESC' TO SEE MESSAGES."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 3010 TO POS
              DISPLAY "DEBTOR RECORD BUSY PRR-006 :" AT POS
              ADD 28 TO POS
              DISPLAY DR-ACCOUNT-NUMBER AT POS
              ADD 20 TO POS
              DISPLAY WS-RANDOM-ST1 AT POS
              ADD 5 TO POS
              PERFORM ERROR-010
              MOVE 0 TO WS-RANDOM-ST1
              GO TO PRR-010.

           MOVE 2610 TO POS
           DISPLAY "DEBTOR NUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY HIGH-ACCOUNT AT POS.
           
           MOVE HIGH-ACCOUNT TO DR-ACCOUNT-NUMBER
           PERFORM READ-DEBTOR.

           IF WS-PERIOD = "B"
            IF DR-BALANCE NOT > 0
              GO TO PRR-010.
           IF WS-PERIOD = "T"
            IF DR-SALES-YTD < 0
              GO TO PRR-010.
           IF WS-PERIOD = "L"
            IF DR-SALES-LAST < 0
              GO TO PRR-010.
              
           IF WS-SALES-CODE NOT = 0
              IF WS-SALES-CODE NOT = DR-SALES-ANALYSIS
                 GO TO PRR-010.
           IF WS-PRINT-COD = "N"
            IF DR-TERMS-CODE = "2"
                 GO TO PRR-010.
           IF WS-SUPPLY = "N"
            IF DR-SUPPLY-Y-N NOT = "Y"
                 GO TO PRR-010.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.

           MOVE DR-NAME     TO PNAME2
           MOVE DR-ADDRESS1 TO PADD12
           MOVE DR-ADDRESS2 TO PADD22.
           IF DR-ADDRESS3 = " "
               MOVE DR-POST-CODE TO PADD32
               MOVE " "          TO PADD42
           ELSE
               MOVE DR-ADDRESS3 TO PADD32.
           MOVE DR-POST-CODE    TO PADD42.
           IF WS-CONTACT = "A"
              MOVE DR-ACCOUNTS-CONTACT TO PCONTACT2.
           IF WS-CONTACT = "N"
              MOVE "TECHNICAL MANAGER" TO PCONTACT2.
           IF WS-CONTACT = "S"
              MOVE DR-SALES-CONTACT    TO PCONTACT2.
           ADD 1              TO WS-NO-PRINTED.
           MOVE 3010 TO POS
           DISPLAY "TOTAL NUMBER OF ACCOUNTS READ:" AT POS
           MOVE WS-NO-PRINTED TO WS-DISPLAY-PRINTED
           ADD 31 TO POS
           DISPLAY WS-DISPLAY-PRINTED AT POS.
           IF WS-NO-PRINTED = WS-NO-ACC
               MOVE "1" TO WS-END-OF-FILE
               GO TO PRR-999.
       PRR-020.
           READ HIGH-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
                 MOVE "1" TO WS-END-OF-FILE
                 MOVE " " TO PNAME3
                             PADD13
                             PADD23
                             PADD33
                             PADD43
                 GO TO PRR-999.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE "DR-RECORD BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 3010 TO POS
              DISPLAY "DEBTOR RECORD BUSY PRR-006 :" AT POS
              ADD 28 TO POS
              DISPLAY DR-ACCOUNT-NUMBER AT POS
              ADD 20 TO POS
              DISPLAY WS-RANDOM-ST1 AT POS
              ADD 5 TO POS
              PERFORM ERROR-010
              MOVE 0 TO WS-RANDOM-ST1
              GO TO PRR-020.

           MOVE 2610 TO POS
           DISPLAY "DEBTOR NUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY HIGH-ACCOUNT AT POS.
           
           MOVE HIGH-ACCOUNT TO DR-ACCOUNT-NUMBER
           PERFORM READ-DEBTOR.

           IF WS-PERIOD = "B"
            IF DR-BALANCE NOT > 0
              GO TO PRR-020.
           IF WS-PERIOD = "T"
            IF DR-SALES-YTD < 0
              GO TO PRR-020.
           IF WS-PERIOD = "L"
            IF DR-SALES-LAST < 0
              GO TO PRR-020.

           IF WS-SALES-CODE NOT = 0
              IF WS-SALES-CODE NOT = DR-SALES-ANALYSIS
                 GO TO PRR-020.
           IF WS-PRINT-COD = "N"
            IF DR-TERMS-CODE = "2"
                 GO TO PRR-020.
           IF WS-SUPPLY = "N"
            IF DR-SUPPLY-Y-N NOT = "Y"
                 GO TO PRR-020.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.

           MOVE DR-NAME     TO PNAME3
           MOVE DR-ADDRESS1 TO PADD13
           MOVE DR-ADDRESS2 TO PADD23.
           IF DR-ADDRESS3 = " "
               MOVE DR-POST-CODE TO PADD33
               MOVE " " TO PADD43
           ELSE
               MOVE DR-ADDRESS3 TO PADD33.
           MOVE DR-POST-CODE TO PADD43.
           IF WS-CONTACT = "A"
              MOVE DR-ACCOUNTS-CONTACT TO PCONTACT3.
           IF WS-CONTACT = "N"
              MOVE "TECHNICAL MANAGER" TO PCONTACT3.
           IF WS-CONTACT = "S"
              MOVE DR-SALES-CONTACT    TO PCONTACT3.
              
           ADD 1              TO WS-NO-PRINTED.
           MOVE 3010 TO POS
           DISPLAY "TOTAL NUMBER OF ACCOUNTS READ:" AT POS
           MOVE WS-NO-PRINTED TO WS-DISPLAY-PRINTED
           ADD 31 TO POS
           DISPLAY WS-DISPLAY-PRINTED AT POS.
           IF WS-NO-PRINTED = WS-NO-ACC
               MOVE "1" TO WS-END-OF-FILE.
       PRR-999.
           EXIT.
      *
       READ-DEBTOR SECTION.
       RD-005.
           READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE DR-ACCOUNT-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020.
       RD-999.
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
           
           MOVE WS-random-file TO F-FILENAME
           MOVE SUB-1          TO F-CBFILENAME.
           CALL "OPENFILE" USING  F-ERROR1
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
           CALL "OPENFILE" USING  F-ERROR1
                                  F-FH
                                  F-FILENAME
                                  F-CBFILENAME
                                  F-FILENAME
                                  F-INTEGERZERO
                                  F-OPENMODE-MM.
           CALL "DELETEFILE" USING F-ERROR1
                                   F-FH.
       DST-020.
           CLOSE HIGH-FILE.
           PERFORM CDS-005.
           Move Ws-HIGH-file To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE WS-HIGH-file   TO F-FILENAME
           MOVE SUB-1          TO F-CBFILENAME.
           CALL "OPENFILE" USING  F-ERROR1
                                  F-FH
                                  F-FILENAME
                                  F-CBFILENAME
                                  F-FILENAME
                                  F-INTEGERZERO
                                  F-OPENMODE-MM.
           CALL "DELETEFILE" USING F-ERROR1
                                   F-FH.
              
           PERFORM CDS-005.
           Move Ws-HIGH-file-IND To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE Ws-HIGH-file-Ind TO F-FILENAME
           MOVE Sub-1        TO F-CBFILENAME.
           CALL "OPENFILE" USING  F-ERROR1
                                  F-FH
                                  F-FILENAME
                                  F-CBFILENAME
                                  F-FILENAME
                                  F-INTEGERZERO
                                  F-OPENMODE-MM.
           CALL "DELETEFILE" USING F-ERROR1
                                   F-FH.
       DST-999.
           EXIT.
      *
       END-OFF SECTION.
       END-900.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0
              MOVE 0 TO WS-DEBTOR-ST1
              MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
            
            GO TO OPEN-036.
       OPEN-035.
           OPEN I-O RANDOM-FILE.
           IF WS-RANDOM-ST1 NOT = 0
              MOVE
             "RANDOM FILE OPEN AT I-O ANOTHER COMPUTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-RANDOM-ST1
              GO TO OPEN-035.
       OPEN-036.
           OPEN OUTPUT RANDOM-FILE.
              MOVE 0 TO WS-RANDOM-ST1
              MOVE
             "RANDOM FILE OPEN OUTPUT ANOTHER COMPUTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
           IF WS-RANDOM-ST1 NOT = 0 
              GO TO OPEN-036.
              
             GO TO OPEN-038.
       OPEN-037.
           OPEN I-O HIGH-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE
             "HIGH FILE OPEN I-O AT ANOTHER COMPUTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-RANDOM-ST1
              GO TO OPEN-037.
       OPEN-038.
           OPEN OUTPUT HIGH-FILE.
           IF WS-RANDOM-ST1 NOT = 0 
              MOVE
             "HIGH FILE OPEN OUTPUT AT ANOTHER COMPUTER, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-RANDOM-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO OPEN-038.
       Open-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "DrLablRp"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       Open-999.
           EXIT.
      *
       Copy "ReadFieldNumeric".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldChange".
       Copy "WriteFieldNumeric".
       Copy "CheckForPause".
       Copy "QueuePrintFileLabel".
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
      *
      * END-OF-JOB
