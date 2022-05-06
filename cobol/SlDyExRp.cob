        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlDyExRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectSlDaily".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDaily.
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  WS-PRINTED           PIC X VALUE "N".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-TYPE-OF-PRINT     PIC X VALUE " ".
       77  WS-LINE1             PIC X VALUE "N".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1     PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST     PIC X(20) VALUE " ".
           03  WS-DAILY-2ND     PIC X(20) VALUE " ".
           03  WS-DAILY-3RD     PIC X(20) VALUE " ".
           03  WS-DAILY-4TH     PIC X(20) VALUE " ".
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(23) VALUE " ".
           03  FILLER         PIC X(15) VALUE "D A I L Y   E X".
           03  FILLER         PIC X(9) VALUE " C E P T ".
           03  FILLER         PIC X(7) VALUE "I O N  ".
           03  FILLER         PIC X(37) VALUE " L O G".
           03  FILLER         PIC X(6) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(14) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(40) VALUE " ".
           03  FILLER         PIC X(37) VALUE ALL "*".
           03  FILLER         PIC X(55) VALUE " ".
       01  DETAIL-LINE.
           03  FILLER         PIC X(9) VALUE " ".
           03  D-1ST-PART     PIC X(21).
           03  D-2ND-PART     PIC X(21).
           03  D-3RD-PART     PIC X(21).
           03  D-4TH-PART     PIC X(21).
           03  FILLER         PIC X(39) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 325 TO POS
           DISPLAY "** DAILY EXCEPTIONS REPORT **" AT POS
           MOVE 425 TO POS
           DISPLAY "*****************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
            MOVE 1010 TO POS.
            DISPLAY 
            "ENTER A TYPE OF EXCEPTION, LEAVE BLANK FOR ALL.[ ]"
             AT POS.
            MOVE 1110 TO POS 
            DISPLAY 
            "1=DEBTOR, 2=CREDITOR, 3=BOTH DEBTOR & CREDITOR EXCEPTIONS"
              AT POS
            MOVE 1058 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 57        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TYPE-OF-PRINT.

            IF W-ESCAPE-KEY = 0 OR 1 OR 2
                GO TO CONTROL-020
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO CONTROL-010.
       CONTROL-020.
           IF WS-TYPE-OF-PRINT NOT = " " AND NOT = "1" AND NOT = "2"
                           AND NOT = "3"
                DISPLAY " " AT 3079 WITH BELL
                DISPLAY "      " AT POS
                GO TO CONTROL-010.
       CONTROL-050.
            MOVE 2610 TO POS.
            DISPLAY "The Report Is Being Compiled, Please Be Patient."
              AT POS.
           PERFORM OPEN-FILES
           PERFORM PRINT-ROUTINE.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           READ DAILY-EXCEPTIONS
               AT END
               PERFORM END-OFF.
           MOVE DAILY-EX-REC TO WS-DAILY-MESSAGE
           IF WS-LINE1 = "Y"
              MOVE "2" TO WS-LINE1.

           MOVE 1501 TO POS
           DISPLAY WS-DAILY-MESSAGE AT POS.

       PRR-002.
           IF WS-TYPE-OF-PRINT = " "
                GO TO PRR-005.
           IF WS-TYPE-OF-PRINT = "1" OR = "2" OR = "3"
            IF WS-LINE1 = "2" 
                GO TO PRR-005.
                
           IF WS-TYPE-OF-PRINT = "1" OR = "3"
              IF WS-DAILY-1ST = "ACCOUNT OVER LIMIT  "
                 GO TO PRR-005.
           IF WS-TYPE-OF-PRINT = "1" OR = "3"
              IF WS-DAILY-1ST = "CREDIT LIMIT CHANGED"
                 GO TO PRR-005.
           IF WS-TYPE-OF-PRINT = "1" OR = "3"
              IF WS-DAILY-1ST = "SUPPLY YES / NO ON  "
                 GO TO PRR-005.
           IF WS-TYPE-OF-PRINT = "1" OR = "3"
              IF WS-DAILY-1ST = "SUPPLY YES/NO AUTO  "
                 GO TO PRR-005.
           IF WS-TYPE-OF-PRINT = "2" OR = "3"
              IF WS-DAILY-1ST = "ACCOUNT CREDIT TERMS"
                 GO TO PRR-005.
           IF WS-TYPE-OF-PRINT = "2" OR = "3"
              IF WS-DAILY-1ST = "ACCOUNT CREDIT TERMS"
                 GO TO PRR-005.
           IF WS-TYPE-OF-PRINT = "2" OR = "3"
              IF WS-DAILY-1ST = "FOREIGN / LOCAL FLAG"
                 GO TO PRR-005.
           IF WS-TYPE-OF-PRINT = "2" OR = "3"
              IF WS-DAILY-1ST = "AUTOMATIC / MANUAL  "
                 GO TO PRR-005.
           IF WS-TYPE-OF-PRINT = "2" OR = "3"
              IF WS-DAILY-1ST = "BANK NAME CHECK   : "
                 GO TO PRR-005.
           IF WS-TYPE-OF-PRINT = "2" OR = "3"
              IF WS-DAILY-1ST = "BANK ACC NUMBER CHCK"
                 GO TO PRR-005.
           IF WS-TYPE-OF-PRINT = "2" OR = "3"
              IF WS-DAILY-1ST = "BANK BRANCH NUM CHCK"
                 GO TO PRR-005.
           IF WS-TYPE-OF-PRINT = "2" OR = "3"
              IF WS-DAILY-1ST = "BANK BRANCH ACC TYPE"
                 GO TO PRR-005.

           GO TO PRR-000.
       PRR-005.
           IF LINE-CNT < 60
              GO TO PRR-010.
           ADD 1 TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC.
           IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 2
           MOVE 4 TO LINE-CNT.
       PRR-010.
           MOVE "Y"          TO WS-PRINTED
           MOVE DAILY-EX-REC TO WS-DAILY-MESSAGE
           MOVE WS-DAILY-1ST TO D-1ST-PART
           MOVE WS-DAILY-2ND TO D-2ND-PART
           MOVE WS-DAILY-3RD TO D-3RD-PART
           MOVE WS-DAILY-4TH TO D-4TH-PART
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT.
           
           IF WS-TYPE-OF-PRINT = "1" OR = "2" OR = "3"
            IF WS-LINE1 = "2"
               MOVE "N" TO WS-LINE1
               GO TO PRR-000.
           IF WS-TYPE-OF-PRINT = "1" OR = "2" OR = "3"
            IF WS-LINE1 = "N"
               MOVE "Y" TO WS-LINE1.
           
           GO TO PRR-000.
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN INPUT DAILY-EXCEPTIONS.
            IF WS-DAILY-ST1 NOT = 0
                MOVE 0 TO WS-DAILY-ST1
                GO TO END-900.
            MOVE " " TO DAILY-MESSAGE.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE DAILY-EXCEPTIONS.
           IF WS-PRINTED = "N"
                 PERFORM PRR-005
                 MOVE "NO EXCEPTIONS TO PRINT FOR THIS PERIOD"
                    TO DETAIL-LINE
                 WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
                 MOVE " " TO PRINT-REC
                 WRITE PRINT-REC AFTER 1.
           IF WS-TYPE-OF-PRINT = " "
                 MOVE "ALL EXCEPTIONS FOR THIS PERIOD PRINTED."
                    TO DETAIL-LINE
                 WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
                 MOVE " " TO PRINT-REC
                 WRITE PRINT-REC AFTER 1.
           IF WS-TYPE-OF-PRINT = "1"
                 MOVE "ONLY DEBTOR EXCEPTIONS FOR THIS PERIOD PRINTED."
                    TO DETAIL-LINE
                 WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
                 MOVE " " TO PRINT-REC
                 WRITE PRINT-REC AFTER 1.
           IF WS-TYPE-OF-PRINT = "2"
                 MOVE 
                 "ONLY CREDITOR EXCEPTIONS FOR THIS PERIOD PRINTED."
                    TO DETAIL-LINE
                 WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
                 MOVE " " TO PRINT-REC
                 WRITE PRINT-REC AFTER 1.
           IF WS-TYPE-OF-PRINT = "3"
                 MOVE 
            "ALL DEBTOR & CREDITOR EXCEPTIONS FOR THIS PERIOD PRINTED."
                    TO DETAIL-LINE
                 WRITE PRINT-REC FROM DETAIL-LINE AFTER 1
                 MOVE " " TO PRINT-REC
                 WRITE PRINT-REC AFTER 1.
       END-020.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
               
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-030.
            MOVE "N" TO WS-ANSWER.
            MOVE 2210 TO POS.
            DISPLAY "DO YOU WISH TO DELETE THE FILE :[ ]" AT POS.
            MOVE 2243 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 19        TO CDA-ROW.
           MOVE 42        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

      *      ACCEPT WS-ANSWER AT POS.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2
                GO TO END-040
            ELSE
                DISPLAY " " AT 3079 WITH BELL
                GO TO END-030.
       END-040.
           IF WS-ANSWER = "N"
                GO TO END-900.
           IF WS-ANSWER NOT = "Y"
                MOVE 1033 TO POS
                DISPLAY " " AT 3079 WITH BELL
                DISPLAY "      " AT POS
                GO TO END-030.
           OPEN OUTPUT DAILY-EXCEPTIONS.
           CLOSE DAILY-EXCEPTIONS.
       END-900.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *      
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
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
