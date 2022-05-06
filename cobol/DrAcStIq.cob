        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrAcStIq.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrTrans".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdDrTrans.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM       PIC X(8) VALUE "DrNameIq".
       77  WS-INV-INQ-PROGRAM       PIC X(8) VALUE "SlInOrIq".
       77  WS-INV-INQ-LYR-PROGRAM   PIC X(8) VALUE "SlInOrLy".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-ANSWER            PIC X VALUE "Y".
       77  WS-DEBTORNUMBER      PIC X(7) VALUE " ".
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  LINE-TOT             PIC 9(5) VALUE 0.
       77  LINE-UNAPPLIED-TOT   PIC 9(5) VALUE 0.
       77  WS-BODY-LINE         PIC ZZ9.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1   PIC 99.
       01  WS-SCROLL-NUMBERS.
           03  WS-SCROLL-NUM OCCURS 1000.
             05  WS-DR-TYPE       PIC 99.
             05  WS-DR-TRANS      PIC 9(6).
             05  WS-DR-REF2       PIC 9(6).
             05  WS-DR-DATE       PIC 9(8).
       01  WS-TYPES.
           03  FILLER          PIC X(7) VALUE "INVOICE".
           03  FILLER          PIC X(7) VALUE "PAYMENT".
           03  FILLER          PIC X(7) VALUE "R/D CHQ".
           03  FILLER          PIC X(7) VALUE "JNL.DR.".
           03  FILLER          PIC X(7) VALUE "JNL.CR.".
           03  FILLER          PIC X(7) VALUE "C/NOTE ".
           03  FILLER          PIC X(7) VALUE "INTREST".
           03  FILLER          PIC X(7) VALUE "DISCNT.".
           03  FILLER          PIC X(7) VALUE "B-DEBT.".
           03  FILLER          PIC X(7) VALUE "CH. REF".
           03  FILLER          PIC X(7) VALUE "INT REV".
       01  WS-TYPES-RED REDEFINES WS-TYPES.
           03  WS-TYPE-DESC    PIC X(7) OCCURS 11.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(15).
           03  FILLER         PIC X(45) VALUE
           "** ACCOUNT STATUS INQUIRY BY ACCOUNT **".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(1) VALUE " ".
           03  FILLER         PIC X(10) VALUE "ACC # :".
           03  H3-ACCOUNT     PIC X(20).
           03  FILLER         PIC X(10) VALUE "ACC NAME:".
           03  H3-NAME        PIC X(40).
       01  HEAD3.
           03  FILLER         PIC X(15) VALUE " TYPE & NUMBER".
           03  FILLER         PIC X(15) VALUE "TRANS & NUM".
           03  FILLER         PIC X(9) VALUE "DATE".
           03  FILLER         PIC X(31) VALUE "DATE DEL  ORDER NUMBER".
           03  FILLER         PIC X(11) VALUE "BEGIN AMT".
           03  FILLER         PIC X(10) VALUE "REMAIN AMT".
       01  HEAD4.
           03  FILLER         PIC X(8) VALUE "BALANCE:".
           03  H4-BALANCE     PIC Z(6)9.99-.
       01  DETAIL-LINE.
           03  D-TYPE-NAME    PIC X(7).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-REFNUM       PIC Z(5)9.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-TYPE         PIC 99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-TRANS        PIC Z(5)9.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-TRANS-DATE   PIC X(11) VALUE " ".
           03  D-DEL-DATE     PIC X(11) VALUE " ".
           03  D-PONO         PIC X(20) VALUE " ".
           03  D-BEGIN        PIC Z(6)9.99.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-END          PIC Z(6)9.99.
       01  TOTAL-LINE.
           03  FILLER         PIC X(14).
           03  T-NAME         PIC X(24) VALUE
            "TOTALS LINES ON ACCOUNT:".
           03  T-LINE-TOT     PIC Z(4)9.
           03  FILLER         PIC X(5) VALUE " ".
           03  FILLER         PIC X(27) VALUE
            "UNAPPLIED LINES ON ACCOUNT:".
           03  T-UNAPPLIED-TOT PIC Z(4)9.
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
           PERFORM CLEAR-SCREEN.
           MOVE 0310 TO POS
           DISPLAY "*** ACCOUNT STATUS INQUIRY BY ACCOUNT ***" AT POS
           MOVE 0410 TO POS
           DISPLAY "*****************************************" AT POS.
       CONTROL-003.
           MOVE 2610 TO POS
           DISPLAY 
           "*** DO NOT PRINT A/C STATUS REPORT IF YOU ARE RUNNING THIS"
             AT POS
             MOVE 2710 TO POS
             DISPLAY 
           "    PROGRAM FROM WITHIN THE DEBTOR PAYMENTS PROGRAM. ***"
             AT POS.
           Copy "PrinterAcceptDr".
       CONTROL-009.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
            PERFORM DISPLAY-FORM.
            PERFORM GET-DATA.
            GO TO CONTROL-010.
       CONTROL-999.
            EXIT.
      *
        GET-DATA SECTION.
        GET-000.
            PERFORM ERROR1-020.
            PERFORM ERROR-020.

            PERFORM CLEAR-MEMORY.
            MOVE "N" TO WS-ANSWER.
            MOVE "                         " TO F-NAMEFIELD.

            MOVE 2905 TO POS
            DISPLAY 
           "Press 'PgDn' For Next Account, 'PgUp' For Previous Account,"
            AT POS
            MOVE 3005 TO POS
            DISPLAY " Or Enter Account Number." AT POS

            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 PERFORM READ-DEBTOR-NEXT
                 GO TO GET-010.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-DEBTOR-PREVIOUS
                 GO TO GET-010.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO DR-ACCOUNT-NUMBER.
            IF DR-ACCOUNT-NUMBER = 0
                CLOSE DEBTOR-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                OPEN I-O DEBTOR-MASTER
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            PERFORM READ-DEBTORS.
        GET-010.
            PERFORM ERROR1-020.
            PERFORM ERROR-020.
            MOVE 2701 TO POS 
            DISPLAY WS-MESSAGE AT POS
            MOVE 2801 TO POS 
            DISPLAY WS-MESSAGE AT POS.
        GET-011.
            MOVE "ACCNO"           TO F-FIELDNAME.
            MOVE 5                 TO F-CBFIELDNAME.
            MOVE DR-ACCOUNT-NUMBER TO F-NAMEFIELD
            MOVE 7                 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEBTORNAME" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            MOVE DR-NAME      TO F-NAMEFIELD
            MOVE 40           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            IF DR-NAME = "UNKNOWN"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
            MOVE " " TO F-EXIT-CH.

            MOVE "POSTADD1" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-ADDRESS1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD2" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-ADDRESS2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD3" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-ADDRESS3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PHONENUM"   TO F-FIELDNAME.
            MOVE 8            TO F-CBFIELDNAME.
            MOVE DR-ACC-PHONE TO F-NAMEFIELD.
            MOVE 20           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCCONTACT"        TO F-FIELDNAME.
            MOVE 10                  TO F-CBFIELDNAME.
            MOVE DR-ACCOUNTS-CONTACT TO F-NAMEFIELD.
            MOVE 20                  TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCBAL"   TO F-FIELDNAME.
            MOVE 6          TO F-CBFIELDNAME.
            MOVE DR-BALANCE TO F-EDNAMEFIELDNUM6.
            MOVE 11         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "BAL.LAST" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-BAL-LAST-STATE TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "CREDITLIMIT"   TO F-FIELDNAME.
            MOVE 11              TO F-CBFIELDNAME.
            MOVE DR-CREDIT-LIMIT TO F-EDNAMEFIELDNUM.
            MOVE 7               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "DATELASTSALE"    TO F-FIELDNAME.
            MOVE 12                TO F-CBFIELDNAME.
            MOVE DR-DATE-LAST-SALE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE      TO F-NAMEFIELD.
            MOVE 10                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATECREATED"   TO F-FIELDNAME.
            MOVE 11              TO F-CBFIELDNAME.
            MOVE DR-DATE-CREATED TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATELASTPAY"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            MOVE DR-DATE-LAST-PAY TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE     TO F-NAMEFIELD.
            MOVE 10               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESANALYSIS" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE DR-SALES-ANALYSIS TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REPNUM"    TO F-FIELDNAME.
            MOVE 6           TO F-CBFIELDNAME.
            MOVE DR-SALESMAN TO F-NAMEFIELD.
            MOVE 1           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CURRENTBAL" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DR-CURRENT TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "3060DAYBAL" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DR-30DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "6190DAYBAL" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DR-60DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "91120DAYBAL" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE DR-90DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "121DAY" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE DR-120DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "SALESPTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-SALES-PTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "SALESYTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-SALES-YTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "SALESLAST" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DR-SALES-LAST TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "COSTSPTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-COST-PTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "COSTSYTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-COST-YTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "DISC-CODE"      TO F-FIELDNAME.
            MOVE 9                TO F-CBFIELDNAME.
            MOVE DR-DISCOUNT-CODE TO F-NAMEFIELD.
            MOVE 1                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUPPLY"         TO F-FIELDNAME.
            MOVE 6                TO F-CBFIELDNAME.
            MOVE DR-SUPPLY-Y-N    TO F-NAMEFIELD.
            MOVE 1                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-015.
            PERFORM READ-ALL-TRANSACTIONS.
            PERFORM FILL-BODY.
            IF F-EXIT-CH = X"07" OR = X"09" OR = X"1F"
                PERFORM CLEAR-TRANSACTIONS
                MOVE 1   TO F-INDEX
                MOVE "Y" TO WS-ANSWER
                GO TO GET-999.
      *      PERFORM READ-TRANSACTIONS.
       GET-040.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
        
            IF WS-ANSWER = "Y"
                CLOSE DEBTOR-TRANS-FILE
                GO TO GET-999.
            IF F-INDEX > 10
                GO TO GET-999.
            MOVE 2905 TO POS.
            DISPLAY "No More Transactions for Account,  " AT POS.
            ADD 34 TO POS.
            DISPLAY "Press 'ESC' To Clear The Screen " AT POS.
            MOVE 3010 TO POS
            DISPLAY "Or Press 'F10' To Print All Transactions !" AT POS
            MOVE 10 TO F-INDEX
            PERFORM USER-FILL-FIELD.
            MOVE 2905 TO POS.
            DISPLAY "                                          " AT POS.
            MOVE 2935 TO POS.
            DISPLAY "                                          " AT POS.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                CLOSE DEBTOR-TRANS-FILE
                MOVE 1 TO F-INDEX
                MOVE "Y" TO WS-ANSWER
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
                OPEN I-O DEBTOR-TRANS-FILE
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE DEBTOR-TRANS-FILE
                MOVE " " TO WS-MESSAGE
                PERFORM ERROR-020
                GO TO GET-999.
            GO TO GET-040.
       GET-999.
            EXIT.
      *
       FILL-BODY SECTION.
       FILL-000.
           OPEN INPUT DEBTOR-TRANS-FILE.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DRTRANS BUSY ON OPEN, IN 1 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              CLOSE DEBTOR-TRANS-FILE
              GO TO FILL-000.

           MOVE 1 TO F-INDEX.
           MOVE 1 TO SUB-1 SUB-2 SUB-3.
      *     PERFORM SCROLL-NEXT.
           PERFORM SCROLL-PREVIOUS-PAGE.
       FILL-001.
           MOVE 2702 TO POS
           DISPLAY "Press 'PgDn' For More, 'PgUp' For Prev,"
           AT POS
           ADD 40 TO POS
           DISPLAY "'F12' OR 'F11' to Scroll Up/Down," AT POS
           MOVE 2803 TO POS
           DISPLAY 
        "'ESC' OR 'TAB' To Clear The Screen, 'F10' To Print All" &
           " Transactions." AT POS.
           MOVE 2950 TO POS
           DISPLAY "'Alt-Z' = Zoom on trans." AT POS.
       FILL-010.
           MOVE 3015 TO POS 
           DISPLAY "Current Line#: " AT POS
           ADD 16 TO POS
           MOVE SUB-1 TO WS-BODY-LINE
           DISPLAY WS-BODY-LINE AT POS.

           IF SUB-1 < 1
              MOVE 1 TO SUB-1 F-INDEX.
           MOVE "                   "             TO F-NAMEFIELD.
           MOVE "TYPEOFTRANS"                     TO F-FIELDNAME.
           MOVE 11                                TO F-CBFIELDNAME.
           MOVE WS-TYPE-DESC (DRTR-TYPE)          TO F-NAMEFIELD.
           MOVE 7                                 TO F-CBFIELDLENGTH.
           PERFORM USER-FILL-FIELD.
           PERFORM READ-FIELD-ALPHA.
      *UP-ARROW
           IF F-EXIT-CH = X"01" AND F-INDEX = 1
            IF SUB-1 = 1
              GO TO FILL-010
            ELSE
              PERFORM SCROLL-PREVIOUS
              MOVE 10 TO F-INDEX
              GO TO FILL-010.
           IF F-EXIT-CH = X"01" AND F-INDEX > 1
              SUBTRACT 1 FROM F-INDEX 
                              SUB-1
            IF F-INDEX > 0
              GO TO FILL-010
            ELSE
              MOVE 1 TO F-INDEX
              PERFORM SCROLL-PREVIOUS
              MOVE 1 TO F-INDEX
              GO TO FILL-010.
      *SCROLL-UP
           IF F-EXIT-CH = X"11"
            IF SUB-1 NOT > SUB-9
              PERFORM SCROLL-NEXT
              MOVE 1 TO F-INDEX
              GO TO FILL-010
            ELSE
              GO TO FILL-010.
      *SCROLL-DOWN
           IF F-EXIT-CH = X"13"
            IF SUB-1 NOT > SUB-9
              PERFORM SCROLL-PREVIOUS
              MOVE 1 TO F-INDEX
              COMPUTE SUB-1 = SUB-1 - 9
             IF SUB-1 NOT > 1
                 MOVE 1 TO SUB-1
                 GO TO FILL-010
             ELSE 
                 GO TO FILL-010.
      *NEXT-PAGE
           IF F-EXIT-CH = X"0C"
            IF SUB-1 NOT > SUB-9
              PERFORM SCROLL-NEXT-PAGE
              MOVE 1 TO F-INDEX
              GO TO FILL-010
            ELSE
              GO TO FILL-010.
      *PREV-PAGE
           IF F-EXIT-CH = X"05"
              PERFORM SCROLL-PREVIOUS-PAGE
              MOVE 1 TO F-INDEX
              GO TO FILL-010.
      *TAB - <ALT-F8>
           IF F-EXIT-CH = X"09"
              GO TO FILL-900.
      *ESC
           IF F-EXIT-CH = X"07"
              GO TO FILL-900.
      * <f10> to print
           IF F-EXIT-CH = X"1F"
                CLOSE DEBTOR-TRANS-FILE
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE DEBTOR-TRANS-FILE
                MOVE " " TO WS-MESSAGE
                PERFORM ERROR-020
                GO TO FILL-900.
      *DOWN-ARROW
           IF F-EXIT-CH = X"0B" AND F-INDEX < 10
            IF SUB-1 NOT = SUB-9
              ADD 1 TO F-INDEX SUB-1
              GO TO FILL-010
            ELSE
              GO TO FILL-010.
           MOVE 7 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
      *RETURN
           IF F-EXIT-CH = X"0A" AND F-INDEX < 10
             IF SUB-1 NOT = SUB-9
              ADD 1 TO F-INDEX SUB-1
              GO TO FILL-010
             ELSE
              GO TO FILL-010.
      *FINISH
           IF F-EXIT-CH = X"04"
              PERFORM END-OFF.
      ***********************************************
      *ZOOMBOX MODE                                 *
      * <CODE-z> = X"FA"  <CODE-SHIFT-Z> = X"DA"    *
      ***********************************************
      *IN CTOS: <CODE-Z>;  IN LINUX: <ALT-Z>
           IF F-EXIT-CH = X"FA" OR = X"DA"
            IF WS-DR-TYPE (SUB-1) = 1 OR = 6
                PERFORM ADD-TYPE-TO-NUMBER
                MOVE ALPHA-RATE TO WS-LINK-ACCOUNT
            ELSE
                MOVE 0          TO WS-LINK-ACCOUNT.
                
           IF F-EXIT-CH = X"FA" OR = X"DA"
                PERFORM CHECK-YEAR-OF-TRANS.

           IF F-EXIT-CH = X"FA" OR = X"DA"
             IF WS-DR-DATE (SUB-1) < WS-BEG-DATE
                MOVE SUB-1        TO SUB-1SAVE
                MOVE F-INDEX      TO F-INDEXSAVE
                PERFORM CLEAR-SCREEN
                CALL WS-INV-INQ-LYR-PROGRAM USING WS-LINKAGE
                CANCEL WS-INV-INQ-LYR-PROGRAM
                MOVE 0 TO WS-LINK-ACCOUNT
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                PERFORM GET-011
                PERFORM FILL-001
                PERFORM CALC-POS-OF-CURSOR
                SUBTRACT 1 FROM SUB-9
                PERFORM RDALL-910
                GO TO FILL-010.
           IF F-EXIT-CH = X"FA" OR = X"DA"
                MOVE SUB-1        TO SUB-1SAVE
                MOVE F-INDEX      TO F-INDEXSAVE
                PERFORM CLEAR-SCREEN
                CALL WS-INV-INQ-PROGRAM USING WS-LINKAGE
                CANCEL WS-INV-INQ-PROGRAM
                MOVE 0 TO WS-LINK-ACCOUNT
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                PERFORM GET-011
                PERFORM FILL-001
                PERFORM CALC-POS-OF-CURSOR
                SUBTRACT 1 FROM SUB-9
                PERFORM RDALL-910
                GO TO FILL-010.
            IF F-NAMEFIELD = " "
                GO TO FILL-010.
       FILL-050.
           ADD 1 TO SUB-1 F-INDEX.
           IF SUB-1 > 1000
               MOVE "1000 LINES ARE UP, 'ESC' TO <TAB>."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-900.
           IF F-INDEX < 11
               GO TO FILL-010.
           SUBTRACT 1 FROM SUB-1.
           IF SUB-1 < 1
              MOVE 1 TO SUB-1.
           PERFORM SCROLL-NEXT.
           MOVE 1 TO F-INDEX.
           GO TO FILL-010.
       FILL-900.
           CLOSE DEBTOR-TRANS-FILE.
       FILL-999.
           EXIT.
      *
       CHECK-YEAR-OF-TRANS SECTION.
       CYOT-000.
            MOVE WS-DATE TO WS-BEG-DATE.
            MOVE 01      TO WS-BEG-DD
            MOVE 03      TO WS-BEG-MM.
       CYOT-999.
           EXIT.
      *
       ADD-TYPE-TO-NUMBER SECTION.
       ATTN-005.
           MOVE SPACES TO ALPHA-RATE
                           DATA-RATE.
           MOVE WS-DR-REF2 (SUB-1) TO DATA-RATE.
           IF WS-DR-TYPE (SUB-1) = 1
               MOVE 1 TO AL-RATE (1)
           ELSE
               MOVE 6 TO AL-RATE (1).
           MOVE 1 TO SUB-10
           MOVE 2 TO SUB-15.
       ATTN-010.
           IF SUB-15 < 8
              MOVE DAT-RATE (SUB-10) TO AL-RATE (SUB-15)
              ADD 1 TO SUB-10 SUB-15
              GO TO ATTN-010.
       ATTN-999.
           EXIT.
      *
       CALC-POS-OF-CURSOR SECTION.
       CPOC-010.
            COMPUTE SUB-1 = SUB-1SAVE - F-INDEXSAVE.
            IF SUB-1 < 0
               MOVE 0 TO SUB-1.
            PERFORM SCROLL-NEXT.
       CPOC-500.
            MOVE SUB-1SAVE   TO SUB-1
            MOVE F-INDEXSAVE TO F-INDEX.
       CPOC-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-0000.
           PERFORM ERROR-020
           MOVE 2701 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2801 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2810 TO POS
           DISPLAY "Printing In Progress, Please Be Patient." AT POS
           
           OPEN I-O DEBTOR-TRANS-FILE
           MOVE 0 TO PAGE-CNT
                     LINE-TOT
           MOVE 66 TO LINE-CNT.
           PERFORM GET-USER-PRINT-NAME
           OPEN OUTPUT PRINT-FILE
           MOVE DR-ACCOUNT-NUMBER TO DRTR-ACCOUNT-NUMBER.
           MOVE 0                 TO DRTR-DATE.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACC-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
                GO TO PRR-999.
           IF WS-DRTRANS-ST1 NOT = 0
               GO TO PRR-900.
       PRR-002.
            READ DEBTOR-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-DRTRANS-ST1 = 10
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO PRR-900.
            IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DRTRANS BUSY ON READ-PRN, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO PRR-002.
            IF DRTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
               GO TO PRR-900.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           MOVE WS-TYPE-DESC (DRTR-TYPE) TO D-TYPE-NAME
           MOVE DRTR-REFERENCE2          TO D-REFNUM
           MOVE DRTR-TYPE                TO D-TYPE
           MOVE DRTR-TRANSACTION-NUMBER  TO D-TRANS
           
           MOVE DRTR-DATE                TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE             TO D-TRANS-DATE.
           
           IF DRTR-DEL-DATE > 0
             MOVE DRTR-DEL-DATE            TO SPLIT-DATE
             PERFORM CONVERT-DATE-FORMAT
             MOVE DISPLAY-DATE             TO D-DEL-DATE
           ELSE
             MOVE SPACES                   TO D-DEL-DATE.
           
           MOVE DRTR-REFERENCE1          TO D-PONO
           MOVE DRTR-AMT-OF-INVOICE      TO D-BEGIN
           MOVE DRTR-AMT-OUTSTANDING     TO D-END.
           
           WRITE PRINT-REC FROM DETAIL-LINE

           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT LINE-TOT
           IF DRTR-AMT-OUTSTANDING > 0
              ADD 1 TO LINE-UNAPPLIED-TOT.
           GO TO PRR-002.
       PRR-060.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " " TO PRINT-REC
            MOVE DR-ACCOUNT-NUMBER TO H3-ACCOUNT
            MOVE DR-NAME           TO H3-NAME
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            MOVE DR-BALANCE TO H4-BALANCE
            WRITE PRINT-REC FROM HEAD4 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 2
            
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE " " TO PRINT-REC
            MOVE 7 TO LINE-CNT.
       PRR-900.
           MOVE LINE-TOT           TO T-LINE-TOT
           MOVE LINE-UNAPPLIED-TOT TO T-UNAPPLIED-TOT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2.
       
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       PRR-999.
           EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           OPEN INPUT DEBTOR-TRANS-FILE.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DRTRANS BUSY ON OPEN, IN 1 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              CLOSE DEBTOR-TRANS-FILE
              GO TO RDTR-000.
           MOVE 1 TO F-INDEX.
       RDTR-005.
           MOVE DR-ACCOUNT-NUMBER TO DRTR-ACCOUNT-NUMBER.
           MOVE 0                 TO DRTR-DATE.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACC-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
                GO TO RDTR-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DRTRANS BUSY ON START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
              GO TO RDTR-005.
       RDTR-010.
           IF F-EXIT-CH NOT = 1
           READ DEBTOR-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = 1
             READ DEBTOR-TRANS-FILE PREVIOUS
                AT END NEXT SENTENCE.
           IF F-EXIT-CH = 1
            IF WS-DRTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               MOVE " " TO F-EXIT-CH
               CLOSE DEBTOR-TRANS-FILE
               GO TO RDTR-000.
           IF F-EXIT-CH NOT = 1
            IF WS-DRTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE DEBTOR-TRANS-FILE
               GO TO RDTR-999.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DRTRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               GO TO RDTR-010.
           IF F-EXIT-CH = 1
            IF DRTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
               MOVE 1 TO F-INDEX
               CLOSE DEBTOR-TRANS-FILE
               MOVE " " TO F-EXIT-CH
               GO TO RDTR-000.
           IF F-EXIT-CH NOT = 1
            IF DRTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
               MOVE 1 TO F-INDEX
               CLOSE DEBTOR-TRANS-FILE
               GO TO RDTR-999.
       RDTR-020.
           IF F-INDEX > 10
                MOVE 2905 TO POS
                DISPLAY "Press 'PgDn' For More, OR 'PgUp' For Previous"
                  AT POS
                ADD 46 TO POS
                DISPLAY "'ESC' To Clear The Screen,  " AT POS
                MOVE 3010 TO POS
                DISPLAY 
                 "Or Press 'F10' To Print All Transactions !" AT POS
                MOVE 10 TO F-INDEX
                PERFORM USER-FILL-FIELD.
            PERFORM ERROR1-020
            PERFORM ERROR-020
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-TRANSACTIONS
                MOVE " " TO F-EXIT-CH
                MOVE 1 TO F-INDEX.
            IF F-EXIT-CH = X"05"
                PERFORM CLEAR-TRANSACTIONS
                MOVE 1 TO F-EXIT-CH
                MOVE 1 TO F-INDEX.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                CLOSE DEBTOR-TRANS-FILE
                MOVE 15 TO F-INDEX
                GO TO RDTR-999.
            IF F-EXIT-CH = X"1F"
                CLOSE DEBTOR-TRANS-FILE
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE DEBTOR-TRANS-FILE
                MOVE "Y" TO WS-ANSWER
                MOVE " " TO WS-MESSAGE
                PERFORM ERROR-020
                PERFORM ERROR1-020
                GO TO RDTR-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                    AND NOT = X"07" AND NOT = " "   AND NOT = X"1F"
                    AND NOT = 1
                MOVE 11 TO F-INDEX
                GO TO RDTR-020.
           PERFORM SCROLLING.
           ADD 1 TO F-INDEX.
           GO TO RDTR-010.
       RDTR-999.
           EXIT.
      *
       READ-ALL-TRANSACTIONS SECTION.
       RDALL-000.
           PERFORM OPEN-006.
           MOVE 1 TO F-INDEX SUB-1.
       RDALL-005.
           MOVE DR-ACCOUNT-NUMBER TO DRTR-ACCOUNT-NUMBER.
           MOVE 0                 TO DRTR-DATE.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACC-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
                GO TO RDALL-950.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DRTRANS BUSY ON START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
              GO TO RDALL-005.
       RDALL-010.
          READ DEBTOR-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
      *         CLOSE DEBTOR-TRANS-FILE
               GO TO RDALL-900.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE 
              "DRTRANS BUSY ON READ-ALL-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               GO TO RDALL-010.
           IF DRTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
               GO TO RDALL-900.
       RDALL-020.
           MOVE DRTR-TYPE               TO WS-DR-TYPE (SUB-1)
           MOVE DRTR-TRANSACTION-NUMBER TO WS-DR-TRANS (SUB-1)
           MOVE DRTR-REFERENCE2         TO WS-DR-REF2 (SUB-1)
           MOVE DRTR-DATE               TO WS-DR-DATE (SUB-1).
           
           IF SUB-1 < 1000
              ADD 1 TO SUB-1 F-INDEX
              GO TO RDALL-010.
              
           MOVE "THERE ARE MORE THAN 1000 TRANSACTIONS ON THIS ACCOUNT"
             TO WS-MESSAGE
             PERFORM ERROR1-000
           MOVE "PRESS 'Esc' TO EXIT THE READ-ALL SECTION."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
       RDALL-900.
           SUBTRACT 1 FROM SUB-1
           MOVE SUB-1 TO SUB-9.
           IF SUB-9 < 0
               MOVE 0 TO SUB-9.
       RDALL-910.
           MOVE 2912 TO POS.
           DISPLAY "Total # of Lines:" AT POS
           ADD 19 TO POS.
           MOVE SUB-9 TO WS-BODY-LINE.
           DISPLAY WS-BODY-LINE AT POS.
           ADD 1 TO SUB-9.
       RDALL-950.
           CLOSE DEBTOR-TRANS-FILE.
       RDALL-999.
           EXIT.
      *
       READ-DR-TRANS-ONLY SECTION.
       RD-TR-ONLY-005.
           IF SUB-1 > SUB-9
      *         PERFORM CLEAR-020
               GO TO RD-TR-ONLY-999.
               
           MOVE WS-DR-TYPE (SUB-1)  TO DRTR-TYPE.
           MOVE WS-DR-TRANS (SUB-1) TO DRTR-TRANSACTION-NUMBER.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 23 OR 35 OR 49
                GO TO RD-TR-ONLY-999.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE "DRTRANS-ONLY BUSY START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
              GO TO RD-TR-ONLY-005.
       RD-TR-ONLY-010.
          READ DEBTOR-TRANS-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE DEBTOR-TRANS-FILE
               GO TO RD-TR-ONLY-999.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE 
              "DRTRANS BUSY ON READ-ONLY, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               GO TO RD-TR-ONLY-010.
       RD-TR-ONLY-999.
           EXIT.
      *
       SCROLL-NEXT SECTION.
       NEXT-000.
            ADD 1  TO SUB-1.
            IF SUB-1 > SUB-9
               MOVE SUB-9 TO SUB-1.
            IF SUB-1 < 1
               MOVE 1 TO SUB-1 F-INDEX.
            MOVE 1 TO F-INDEX.
            PERFORM CLEAR-TRANSACTIONS.
            MOVE 1 TO F-INDEX.
            IF SUB-1 > 989
                 MOVE 989 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 11
                GO TO NEXT-010.
            IF SUB-1 > 989  
                GO TO NEXT-025.
            MOVE 1 TO F-INDEX.
       NEXT-025.
            SUBTRACT 10 FROM SUB-1.
            IF SUB-1 > 989
              IF SUB-25 > 989
               COMPUTE F-INDEX = 10 - (1001 - SUB-9)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX. 
            IF F-INDEX > 10
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3015 TO POS.
            DISPLAY "Current Line#: " AT POS
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-999.
             EXIT.
      *
       SCROLL-NEXT-PAGE SECTION.
       NEXT-PAGE-000.
            ADD 10  TO SUB-1.
            IF SUB-1 > SUB-9
               MOVE SUB-9 TO SUB-1.
            IF SUB-1 < 1
               MOVE 1 TO SUB-1 F-INDEX.
            MOVE 1 TO F-INDEX.
            PERFORM CLEAR-TRANSACTIONS.
            MOVE 1 TO F-INDEX.
            IF SUB-1 > 989
                 MOVE 989 TO SUB-1.
       NEXT-PAGE-010.
            PERFORM SCROLLING.
       NEXT-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 11
                GO TO NEXT-PAGE-010.
            IF SUB-1 > 989  
                GO TO NEXT-PAGE-025.
            MOVE 1 TO F-INDEX.
       NEXT-PAGE-025.
            SUBTRACT 10 FROM SUB-1.
            IF SUB-1 > 989
              IF SUB-25 > 989
               COMPUTE F-INDEX = 10 - (1001 - SUB-9)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX. 
            IF F-INDEX > 10
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3015 TO POS.
            DISPLAY "Current Line#: " AT POS
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-PAGE-999.
             EXIT.
      *
       SCROLL-PREVIOUS-PAGE SECTION.
       PREV-PAGE-000.
            PERFORM CLEAR-TRANSACTIONS.
            SUBTRACT 10 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-PAGE-010.
            PERFORM SCROLLING.
       PREV-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 11
                GO TO PREV-PAGE-010.
            MOVE 1 TO F-INDEX.
            SUBTRACT 10 FROM SUB-1.
       PREV-PAGE-025.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.
            MOVE 3015 TO POS.
            DISPLAY "Current Line#: " AT POS
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       PREV-PAGE-999.
            EXIT.
      *
       SCROLL-PREVIOUS SECTION.
       PREV-000.
            PERFORM CLEAR-TRANSACTIONS.
            IF F-EXIT-CH = X"01"
               SUBTRACT 10 FROM SUB-1
            ELSE
               SUBTRACT 1 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-010.
            PERFORM SCROLLING.
       PREV-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 11
                GO TO PREV-010.
            MOVE 1 TO F-INDEX.
            SUBTRACT 1 FROM SUB-1.
       PREV-025.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.
            MOVE 3015 TO POS.
            DISPLAY "Current Line#: " AT POS
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       PREV-999.
            EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            IF SUB-1 < SUB-9
               PERFORM READ-DR-TRANS-ONLY
            ELSE
               GO TO SCROLL-999.
       
            MOVE "TYPEOFTRANS"            TO F-FIELDNAME.
            MOVE 11                       TO F-CBFIELDNAME.
            MOVE WS-TYPE-DESC (DRTR-TYPE) TO F-NAMEFIELD.
            MOVE 7                        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REFNO"         TO F-FIELDNAME.
            MOVE 5               TO F-CBFIELDNAME.
            MOVE DRTR-REFERENCE2 TO F-EDNAMEFIELDNUM.
            MOVE 6               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "DATEOFREF"  TO F-FIELDNAME.
            MOVE 9            TO F-CBFIELDNAME.
            MOVE DRTR-DATE    TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE-DEL"       TO F-FIELDNAME.
            MOVE 8                TO F-CBFIELDNAME.
            IF DRTR-DEL-DATE = 0
               MOVE " "           TO F-NAMEFIELD
            ELSE
               MOVE DRTR-DEL-DATE TO SPLIT-DATE
               PERFORM CONVERT-DATE-FORMAT
               MOVE DISPLAY-DATE  TO F-NAMEFIELD.
            MOVE 10               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDERNUM"      TO F-FIELDNAME.
            MOVE 8               TO F-CBFIELDNAME.
            MOVE DRTR-REFERENCE1 TO F-NAMEFIELD.
            MOVE 18              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BEGINAMT"          TO F-FIELDNAME
            MOVE 8                   TO F-CBFIELDNAME
            MOVE DRTR-AMT-OF-INVOICE TO F-EDNAMEFIELD9MIL
            MOVE 10                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.

            MOVE "REMAININGAMT"       TO F-FIELDNAME
            MOVE 12                   TO F-CBFIELDNAME
            MOVE DRTR-AMT-OUTSTANDING TO F-EDNAMEFIELD9MIL
            MOVE 10                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-9MIL.
       SCROLL-999.
             EXIT.
      *
       CLEAR-MEMORY SECTION.
       CMS-005.
            MOVE 1 TO SUB-1.
            MOVE 0 TO SUB-9.
       CMS-010.
            IF WS-DR-TYPE (SUB-1) NOT = 0
                MOVE 0 TO WS-DR-TYPE (SUB-1)
                          WS-DR-TRANS (SUB-1)
                          WS-DR-REF2 (SUB-1)
                          WS-DR-DATE (SUB-1)
            ELSE
                GO TO CMS-900.
            IF SUB-1 < 1000
               ADD 1 TO SUB-1
               GO TO CMS-010.
       CMS-900.
            MOVE 1 TO SUB-1.
       CMS-999.
            EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO F-INDEX.
      *      MOVE 1 TO SUB-1 F-INDEX.
       CLTR-010.
            IF F-INDEX > 10
                GO TO CLTR-999.
       CLTR-020.
            MOVE "TYPEOFTRANS" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE " "           TO F-NAMEFIELD
            MOVE 7             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REFNO" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 7       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATEOFREF" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE-DEL" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 10         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDERNUM" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 18         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BEGINAMT" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 10         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "REMAININGAMT" TO F-FIELDNAME
            MOVE 12             TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 10             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       CLTR-900.
      *      ADD 1 TO SUB-1 F-INDEX.
            ADD 1 TO F-INDEX.
            GO TO CLTR-010.
       CLTR-999.
            EXIT.
      *
       READ-DEBTORS SECTION.
       RD-000.
            READ DEBTOR-MASTER
                INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 35 OR 49 OR 23
                MOVE " " TO DR-NAME DR-ADDRESS1 DR-ADDRESS2
                         DR-ADDRESS3 DR-DEL-ADDRESS1 DR-DEL-ADDRESS2
                         DR-DEL-ADDRESS3
                MOVE "UNKNOWN" TO DR-NAME
                MOVE 0 TO DR-POST-CODE
                GO TO RD-999.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "RECORD LOCKED AT ANOTHER STATION, 'ESC' TO RETRY"
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                GO TO RD-000.
       RD-999.
            EXIT.
      *
       START-DEBTOR SECTION.
       ST-ST-000.
              MOVE WS-DEBTORNUMBER TO DR-ACCOUNT-NUMBER.
              MOVE 0               TO DRTR-DATE.
              START DEBTOR-MASTER KEY NOT < DR-ACCOUNT-NUMBER.
       ST-ST-999.
             EXIT.
      *
       READ-DEBTOR-NEXT SECTION.
       R-ST-NX-000.
             MOVE 0 TO WS-DEBTOR-ST1.
             MOVE " " TO WS-MESSAGE
             MOVE 3010 TO POS
             DISPLAY WS-MESSAGE AT POS.
       R-ST-NX-005. 
             READ DEBTOR-MASTER NEXT
                 AT END NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 0
                 GO TO R-ST-NX-999
             ELSE
               MOVE "DRTRANS BUSY ON START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DEBTOR-ST1
               PERFORM START-DEBTOR
               GO TO R-ST-NX-005.
       R-ST-NX-999.
             EXIT.
      *
       READ-DEBTOR-PREVIOUS SECTION.
       RDPR-000.
             MOVE 0 TO WS-DEBTOR-ST1.
             MOVE " " TO WS-MESSAGE
             MOVE 3010 TO POS
             DISPLAY WS-MESSAGE AT POS.
       RDPR-005. 
             READ DEBTOR-MASTER PREVIOUS
                 AT END NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 0
                 GO TO RDPR-999
             ELSE
               MOVE 
               "DRTRANS BUSY ON START-PREV, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DEBTOR-ST1
               PERFORM START-DEBTOR
               GO TO RDPR-005.
       RDPR-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO OPEN-000.
           MOVE Ws-Co-Name to CO-NAME.
           GO TO OPEN-008.
       OPEN-006.
           OPEN INPUT DEBTOR-TRANS-FILE.
           IF WS-DRTRANS-ST1 NOT = 0
              MOVE "DRTRANS BUSY ON OPEN, IN 1 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              CLOSE DEBTOR-TRANS-FILE
              GO TO OPEN-006.
       open-008.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "DrAcStIq" TO F-FORMNAME
           MOVE 8          TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE DEBTOR-MASTER.
       END-900.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldNumeric5".
       Copy "WriteField9Mil".
       Copy "WriteFieldNumeric6".
       Copy "DisplayForm".
       Copy "UserFillField".
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
      *
      * END-OF-JOB
