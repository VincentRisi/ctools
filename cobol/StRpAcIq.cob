        IDENTIFICATION DIVISION.
        PROGRAM-ID. StRpAcIq.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectDrMaster".
          Copy "SelectStTrans".
          Copy "SelectSlRegister".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdRegister.
           COPY ChlfdStTrans.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "DrNameIq".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-ACCOUNTNUMBER     PIC X(7) VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1   PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(75) VALUE
           "** REPAIR ITEM INQUIRY BY ACCOUNT **".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(1) VALUE " ".
           03  FILLER         PIC X(10) VALUE "ACCOUNT :".
           03  H3-ACCOUNT     PIC X(10).
           03  FILLER         PIC X(7) VALUE "NAME :".
           03  H3-NAME        PIC X(52).
       01  HEAD3.
           03  FILLER         PIC X(16) VALUE "STOCK".
           03  FILLER         PIC X(30) VALUE "DESCRIPTION".
           03  FILLER         PIC X(21) VALUE "ORDER".
           03  FILLER         PIC X(28) VALUE "PRICE P/ORDER NUMBER".
           03  FILLER         PIC X(15) VALUE "ORDER".
           03  FILLER         PIC X(16) VALUE " DATE".
           03  FILLER         PIC X(6) VALUE "TRANS".
       01  DETAIL-LINE.
           03  D-STOCKNO      PIC X(16).
           03  D-COMMENT.
             04  D-DESC1        PIC X(20).
             04  D-DESC2        PIC X(10).
             04  D-ORDERQTY     PIC Z(4)9.
             04  FILLER         PIC X(12) VALUE " ".
             04  D-PRICE        PIC Z(5)9.99 BLANK WHEN ZERO.
             04  FILLER         PIC X(1) VALUE " ".
           03  D-PONO         PIC X(21).
           03  D-1STINV       PIC Z(5)9.
           03  FILLER         PIC X(7) VALUE " ".
           03  D-1STDATE      PIC X(10).
           03  FILLER         PIC X(9) VALUE " ".
           03  D-TRANS        PIC Z(5)9.
       Copy "WsDateInfo".
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
           PERFORM CLEAR-SCREEN.
           MOVE 0310 TO POS
           DISPLAY "** REPAIRS INQUIRY BY ACCOUNT NUMBER **" AT POS
           MOVE 0410 TO POS
           DISPLAY "***************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-020.
           MOVE 2910 TO POS
           DISPLAY "Program loading, please be patient...." AT POS.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-050.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-050.
       CONTROL-999.
           Exit.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE "N" TO WS-ANSWER.
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
                PERFORM OPEN-000
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            PERFORM READ-DEBTORS.
            GO TO GET-020.
       GET-010.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DR-ACCOUNT-NUMBER TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-020.
            MOVE "NAME"  TO F-FIELDNAME.
            MOVE 4       TO F-CBFIELDNAME.
            MOVE DR-NAME TO F-NAMEFIELD.
            MOVE 40      TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE DR-ADDRESS1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE DR-ADDRESS2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE DR-ADDRESS3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            IF DR-NAME = "UNKNOWN"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
            MOVE " " TO F-EXIT-CH.
            PERFORM READ-TRANSACTIONS.
       GET-900.
            IF WS-ANSWER = "Y"
                CLOSE STOCK-TRANS-FILE
                GO TO GET-999.
            IF F-INDEX < 15
             IF F-INDEX NOT = X"07"
               MOVE 2910 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen." AT POS
               MOVE 3010 TO POS
               DISPLAY "Or Press 'F10' To Print All Quote Items" AT POS
               ADD 40 TO POS
               DISPLAY "For This Account." AT POS
               MOVE 15 TO F-INDEX
               PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH NOT = X"07" AND NOT = X"1F" AND NOT = X"04"
                MOVE 1 TO F-INDEX
                GO TO GET-900.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
                MOVE "Printing In Progress, Please Be Patient." 
                   TO WS-MESSAGE
                PERFORM ERROR-000
                OPEN I-O STOCK-TRANS-FILE
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE STOCK-TRANS-FILE
                MOVE " " TO WS-MESSAGE
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO GET-999.
       GET-999.
            EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0
               MOVE "ST-TRANS FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANS-ST1
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-000.
       RDTR-001.
           MOVE 1 TO F-INDEX.
       RDTR-005.
           MOVE "R"               TO STTR-AC-COMPLETE.
           MOVE DR-ACCOUNT-NUMBER TO STTR-ACCOUNT-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-AC-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              CLOSE STOCK-TRANS-FILE
              PERFORM RDTR-999.
           MOVE " " TO F-EXIT-CH.
       RDTR-010.
           IF F-EXIT-CH = " "
             READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = 1
             READ STOCK-TRANS-FILE PREVIOUS
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = " "
            IF WS-STTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-999.
           IF F-EXIT-CH = 1
            IF WS-STTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-000.
           IF WS-STTRANS-ST1 NOT = 0
             MOVE "STTRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RDTR-010.
           IF F-EXIT-CH = " "
            IF STTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-999.
           IF F-EXIT-CH = 1
            IF STTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-000.
           IF STTR-TYPE NOT = 3
               GO TO RDTR-010.
           MOVE STTR-STOCK-NUMBER TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               GO TO RDTR-010.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       RDTR-020.
           IF F-INDEX > 15
                MOVE 2905 TO POS
                DISPLAY "Press 'PgDn' For More, 'PgUp' For Previous,"
                 AT POS
                ADD 44 TO POS
                DISPLAY "OR 'ESC' To Clear The Screen !" AT POS
                MOVE 3010 TO POS
                DISPLAY "Or Press 'F10' To Print All Quote Items"
                   AT POS
                ADD 40 TO POS
                DISPLAY "For This Account." AT POS
                MOVE 15 TO F-INDEX
                PERFORM USER-FILL-FIELD.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
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
                MOVE "Y" TO WS-ANSWER
                GO TO RDTR-999.
           IF F-EXIT-CH = X"1F"
                MOVE "Printing In Progress, Please Be Patient."
                   TO WS-MESSAGE
                PERFORM ERROR1-000
                PERFORM ERROR-000
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                MOVE " " TO WS-MESSAGE
                PERFORM ERROR-020
                GO TO RDTR-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                    AND NOT = X"07" AND NOT = " "   AND NOT = X"1F"
                    AND NOT = 1
                MOVE 16 TO F-INDEX
                GO TO RDTR-020.
           IF STTR-REFERENCE1 NOT = INCR-INVOICE
              PERFORM READ-ORDER-REGISTER.
           PERFORM SCROLLING.
           ADD 1 TO F-INDEX.
           GO TO RDTR-010.
       RDTR-999.
           EXIT.
      *
       READ-DEBTORS SECTION.
       RD-000.
            MOVE DR-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER.
            START DEBTOR-MASTER KEY NOT < DR-KEY.
       RD-010.
            READ DEBTOR-MASTER
                INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE WS-ACCOUNTNUMBER TO DR-ACCOUNT-NUMBER
                MOVE " " TO DR-NAME DR-ADDRESS1 DR-ADDRESS2
                         DR-ADDRESS3
                MOVE "UNKNOWN" TO DR-NAME
                MOVE 0 TO DR-POST-CODE
                GO TO RD-999.
            IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DR RECORD BUSY ON READ, 'ESC' TO RE-TRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RD-010.
       RD-999.
            EXIT.
      *
       START-DEBTOR SECTION.
       DR-DR-000.
              MOVE WS-ACCOUNTNUMBER TO DR-ACCOUNT-NUMBER.
              START DEBTOR-MASTER KEY NOT LESS DR-KEY.
       DR-DR-999.
             EXIT.
      *
       READ-DEBTOR-NEXT SECTION.
       R-DR-NX-005. 
             READ DEBTOR-MASTER NEXT
                 AT END NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 10
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM START-DEBTOR
                 GO TO R-DR-NX-999.

             IF WS-DEBTOR-ST1 = 0
                 GO TO R-DR-NX-999
             ELSE
               MOVE "DEBTOR FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               PERFORM START-DEBTOR
               GO TO R-DR-NX-005.
       R-DR-NX-999.
             EXIT.
      *
       READ-DEBTOR-PREVIOUS SECTION.
       RPREV-005. 
             READ DEBTOR-MASTER PREVIOUS
                 AT END NEXT SENTENCE.
             IF WS-DEBTOR-ST1 = 10
               MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM START-DEBTOR
                 GO TO RPREV-999.

             IF WS-DEBTOR-ST1 = 0
                 GO TO RPREV-999
             ELSE
               MOVE "DEBTOR FILE BUSY ON READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               PERFORM START-DEBTOR
               GO TO RPREV-005.
       RPREV-999.
             EXIT.
      *
       READ-ORDER-REGISTER SECTION.
       ROR-000.
            MOVE STTR-REFERENCE1 TO INCR-INVOICE.
            MOVE STTR-TYPE       TO INCR-TRANS.
            START INCR-REGISTER KEY NOT < INCR-KEY.
       ROR-010.
            READ INCR-REGISTER
                INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 = 23 OR 35 OR 49
                MOVE 0 TO INCR-INVOICE
                          INCR-DATE
                          INCR-BO-INV-NO
                          INCR-BO-DATE
                MOVE "*REGISTER NOT FOUND*" TO INCR-PORDER
                GO TO ROR-999.
            IF WS-INCR-ST1 NOT = 0
                MOVE 0 TO WS-INCR-ST1
                MOVE "*P/O NOT AVAILABLE*" TO INCR-PORDER.
       ROR-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE 0  TO PAGE-CNT
           MOVE 66 TO LINE-CNT.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE "R"               TO STTR-AC-COMPLETE.
           MOVE DR-ACCOUNT-NUMBER TO STTR-ACCOUNT-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-AC-KEY
                INVALID KEY NEXT SENTENCE.
            IF WS-STTRANS-ST1 NOT = 0
               MOVE 0 TO WS-STTRANS-ST1
               GO TO PRR-900.
       PRR-002.
            READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-STTRANS-ST1 = 10
               MOVE 0 TO WS-STTRANS-ST1
               GO TO PRR-900.
            IF WS-STTRANS-ST1 NOT = 0
              MOVE "STTRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO PRR-002.
            IF STTR-AC-COMPLETE NOT = "R"
               GO TO PRR-900.
            IF STTR-TYPE NOT = 3
               GO TO PRR-002.
            MOVE STTR-STOCK-NUMBER TO SPLIT-STOCK.
      *      IF SP-1STCHAR = "*"
      *         GO TO PRR-002.
            IF STTR-ACCOUNT-NUMBER < DR-ACCOUNT-NUMBER
               GO TO PRR-002.
            IF STTR-ACCOUNT-NUMBER > DR-ACCOUNT-NUMBER
               GO TO PRR-900.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           IF STTR-REFERENCE1 NOT = INCR-INVOICE
              PERFORM READ-ORDER-REGISTER.
           MOVE STTR-STOCK-NUMBER       TO D-STOCKNO.
           IF SP-1STCHAR = "*"
               MOVE COMMENT-FIELDS      TO D-COMMENT
               GO TO PRR-025.
           MOVE STTR-DESC1              TO D-DESC1
           MOVE STTR-DESC2              TO D-DESC2
           MOVE STTR-ORDERQTY           TO D-ORDERQTY
           MOVE STTR-PRICE              TO D-PRICE.
       PRR-025.
           MOVE INCR-PORDER             TO D-PONO
           MOVE STTR-REFERENCE1         TO D-1STINV
           MOVE STTR-DATE               TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE            TO D-1STDATE
           MOVE STTR-TRANSACTION-NUMBER TO D-TRANS
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE
           ADD 1 TO LINE-CNT
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
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE " " TO PRINT-REC
            MOVE 5 TO LINE-CNT.
       PRR-900.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       PRR-999.
           EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            MOVE "STOCKNO"         TO F-FIELDNAME.
            MOVE 7                 TO F-CBFIELDNAME.
            MOVE STTR-STOCK-NUMBER TO F-NAMEFIELD.
            MOVE 15                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PORDERNO"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE INCR-PORDER TO F-NAMEFIELD.
            MOVE 15          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QTY"         TO F-FIELDNAME.
            MOVE 3             TO F-CBFIELDNAME.
            MOVE STTR-ORDERQTY TO F-EDNAMEFIELDQTY.
            MOVE 5             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-QTY.

            MOVE "PRICE"    TO F-FIELDNAME.
            MOVE 5          TO F-CBFIELDNAME.
            MOVE " "        TO F-NAMEFIELD.
            MOVE 9          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDER"      TO F-FIELDNAME.
            MOVE 5            TO F-CBFIELDNAME.
            MOVE INCR-INVOICE TO F-EDNAMEFIELDNUM.
            MOVE 6            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "ORDDATE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE INCR-DATE    TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO SUB-1 F-INDEX.
       CLTR-010.
            IF SUB-1 > 15
                GO TO CLTR-999.
            MOVE "STOCKNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PORDERNO" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE " "        TO F-NAMEFIELD.
            MOVE 15         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QTY" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PRICE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDER" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDDATE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 10        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            ADD 1 TO SUB-1 F-INDEX.
            GO TO CLTR-010.
       CLTR-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR-MASTER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-003.
            OPEN I-O INCR-REGISTER.
            IF WS-INCR-ST1 NOT = 0
               MOVE 0 TO WS-INCR-ST1
               MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-003.
       OPEN-006.
           Move Ws-Co-Name to Co-Name.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME
           MOVE "StRpAcIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE DEBTOR-MASTER
                 INCR-REGISTER.
       END-900.
           EXIT PROGRAM.
      *      STOP RUN.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldDate".
       Copy "WriteFieldPrice".
       Copy "WriteFieldQty".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
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
      * END-OF-JOB.
