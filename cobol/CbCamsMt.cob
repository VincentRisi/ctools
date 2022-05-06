        IDENTIFICATION DIVISION.
        PROGRAM-ID. CbCamsMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectCbMaster".
        Copy "SelectCbTrans".
        Copy "SelectGlParameter".
        Copy "SelectGlMaster".
        Copy "SelectBankCamsTrans".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCbTrans.
           COPY ChlfdCbMast.
           COPY ChlfdGlMast.
           COPY ChlfdGlParam.
           COPY ChlfdBankCAMS.

       WORKING-STORAGE SECTION.
       77  Ws-CbMast          PIC X(12).
       77  WS-REFERENCE       PIC X(10) VALUE " ".      
       77  WS-ONLY-UNALLOC    PIC X VALUE " ".
       77  WS-END             PIC X VALUE " ".
       77  WS-CHEQUE-SAME     PIC X VALUE " ".
       77  WS-SAL-FOUND       PIC X VALUE " ".
       77  WS-CAMS-DATA-START PIC X VALUE " ".
       77  WS-VAT-POSTED      PIC X VALUE " ".
       77  WS-NUMBER-OF-READS PIC 9(8) VALUE 0.
       77  WS-CB-CHEQUE-NUM   PIC 9(8) VALUE 0.
       77  WS-CHEQUE-NUMBER   PIC 9(8) VALUE 0.
       77  WS-CAMS-AMOUNT     PIC S9(8)V99 VALUE 0.
       77  WS-TOTAL-VAT-AMT   PIC S9(8)V99 VALUE 0.
       77  WS-SALARY-AMT      PIC S9(8)V99.
       01  WS-PERIOD.
           03  WS-FUTURE      PIC X.
           03  WS-NO          PIC 99.
       01  WS-BATCH.
           03  WS-BATCH-1STCHAR  PIC X(2) VALUE "CB".
           03  WS-BATCH-REST     PIC X(8).
       01  WS-TRANS-DATE.
           03  WS-TD-DD   PIC 99.
           03  FILLER     PIC X.
           03  WS-TD-MM   PIC 99.
       01  WS-CAMS-SPLIT-DATE.
           03  WS-CS-DD   PIC 99.
           03  FILLER     PIC X.
           03  WS-CS-MM   PIC 99.
           03  FILLER     PIC X.
           03  WS-CS-YYYY PIC 9999.
       01  WS-CBTRANS-STATUS.
           03  WS-CBTRANS-ST1       PIC 99.
       01  WS-CB-STATUS.
           03  WS-CB-ST1            PIC 99.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1        PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1   PIC 99.
       01  WS-CAMS-STATUS.
           03  WS-CAMS-ST1          PIC 99.
       01  WS-BANK-VAT-ENTRY.
           03  WS-BANK-GROSS-AMT     PIC S9(8)V99.
           03  WS-BANK-VAT-AMT       PIC S9(8)V99.
           03  WS-BANK-NETT-AMT      PIC S9(8)V99.
       01  WS-FOREX-ENTRY-NAMES.
         02  WS-FOREX-ENTRY OCCURS 3.
           03  WS-FX-ENTRY         PIC X(20).
           03  WS-FX-FNL           PIC X(2).
           03  WS-FX-GROSS-AMT     PIC S9(8)V99.
           03  WS-FX-VAT-AMT       PIC S9(8)V99.
           03  WS-FX-NETT-AMT      PIC S9(8)V99.
       01  BANK-CAMS-LINE.
           03  BANK-CAMS03-LINE.
               05  WS-BANK-CAMS-AMT    PIC S9(8)V99 VALUE 0.
       01  WS-NARRATIVE.
           03  WS-NAR-1-21.
               05  WS-NAR-1ST      PIC X.
               05  WS-NAR-20.
                   07  WS-NAR2-3   PIC X(2).
                   07  WS-NAR4-21  PIC X(18).
           03  WS-NAR-22-45        PIC X(24).
           03  WS-NAR-46-47        PIC X(2).
       01  WS-DINERS-NARRATIVE.
           03  WS-NAR-1-14.
             05  WS-NAR-1-10       PIC X(10).
             05  WS-NAR-11-14      PIC X(4).
       01  WS-SALARY-NARRATIVE.
           03  WS-SAL-1-47.
             05  WS-SAL-1-15      PIC X(15).
             05  WS-SAL-1-12      PIC X(12).
             05  WS-SAL-1-20      PIC X(20).
       01  WS-SALARY-OB-NARRATIVE.
           03  WS-SALOB-1-47.
             05  WS-SALOB-1-10      PIC X(10).
             05  WS-SALOB-1-7       PIC X(7).
             05  WS-SALOB-1-13      PIC X(13).
             05  WS-SALOB-1-20      PIC X(17).
       01  WS-LOANS-OB-NARRATIVE.
           03  WS-LOAN-1-47.
             05  WS-LOAN-1-10      PIC X(10).
             05  WS-LOAN-2-1       PIC X.
             05  WS-LOAN-3-10      PIC X(10).
             05  WS-LOAN-4-26      PIC X(26).
       01  WS-TRANS-TYPE.
           03  FILLER        PIC X(7) VALUE "CHQ PAY".
           03  FILLER        PIC X(7) VALUE "MTX PAY".
           03  FILLER        PIC X(7) VALUE "TRN PAY".
           03  FILLER        PIC X(7) VALUE "R/D DEP".
           03  FILLER        PIC X(7) VALUE "STL CHQ".
           03  FILLER        PIC X(7) VALUE "BNK CHG".
           03  FILLER        PIC X(7) VALUE "BNK INT".
           03  FILLER        PIC X(7) VALUE "TRN 1DY".
           03  FILLER        PIC X(7) VALUE "CR ADJ ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "CHQ DEP".
           03  FILLER        PIC X(7) VALUE "TRN DEP".
           03  FILLER        PIC X(7) VALUE "R/D PAY".
           03  FILLER        PIC X(7) VALUE "STL CHQ".
           03  FILLER        PIC X(7) VALUE "BNK REV".
           03  FILLER        PIC X(7) VALUE "BNK INT".
           03  FILLER        PIC X(7) VALUE "TRN 1DY".
           03  FILLER        PIC X(7) VALUE "DR ADJ ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "       ".
       01  WS-TRANS-DESCRED REDEFINES WS-TRANS-TYPE.
           03  WS-TRANS-DESC   PIC X(7) OCCURS 30.
       01  WS-ALL-LINES.
           03  WS-LINES OCCURS 1000.
               05  WS-TRANS-NUM     PIC 9(6) VALUE 0.
               05  WS-TYPE-NUM      PIC 99 VALUE 0.
               05  WS-CB-AMOUNT     PIC S9(8)V99 VALUE 0.
               05  WS-CB-LINE-DESC  PIC X(25).
               05  WS-CAMS-FOUND    PIC X.
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
           PERFORM DISPLAY-FORM.
           PERFORM DISPLAY-CB-NO-TOP-INFO.
           PERFORM CLEAR-TRANS-IN-MEM.
           PERFORM GET-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO CBTRANS-REC
                        WS-END.
       GET-002.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
               EXIT PROGRAM.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF ALPHA-RATE > SPACES
               PERFORM NUMBER-CHECK
               MOVE WS-GLNO-CHECK TO Ws-CbMast
            IF SIGN-FOUND = 9
               MOVE 0 TO SIGN-FOUND
               GO TO GET-002.
            MOVE Ws-CbMast TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-002.

            PERFORM READ-CB.
            IF CB-DESCRIPTION = " "
               MOVE "ENTER AN EXISTING ACCOUNT NUMBER." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-002.

            MOVE "NAME"         TO F-FIELDNAME
            MOVE 4              TO F-CBFIELDNAME
            MOVE CB-DESCRIPTION TO F-NAMEFIELD
            MOVE 40             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       GET-005.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "REFR"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            IF F-EXIT-CH = X"01"
               GO TO GET-002.
            MOVE F-NAMEFIELD TO WS-BATCH-REST.
            IF WS-BATCH-REST = " "
               MOVE "THE REFERENCE CANNOT BE BLANK, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-005.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-005.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       GET-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ONLY-UNALLOC" TO F-FIELDNAME
            MOVE 12             TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            MOVE 2              TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            IF F-EXIT-CH = X"01"
               GO TO GET-005.
            MOVE F-NAMEFIELD  TO WS-ONLY-UNALLOC.
            IF WS-ONLY-UNALLOC NOT = "N" AND NOT = "Y"
               MOVE "ENTER ONLY 'Y' OR 'N' - PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       GET-500.
            PERFORM FILL-DATA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-000.
      **************************************************
      * SUB-20 = TOTAL NUMBER OF CB-TRANS IN MEMORY    *
      * SUB-21 = TOTAL NUMBER OF CAMS FOUND TRANS      *
      **************************************************
           PERFORM READ-TO-FIX-DATES.
           PERFORM CLEAR-TRANS-IN-MEM.

           MOVE 0 TO SUB-20 SUB-21.
           PERFORM READ-ALL-TRANSACTIONS.
           PERFORM ERROR1-020.
       FILL-001.
           MOVE 1 TO SUB-1 SUB-2 F-INDEX.
           MOVE "TOTLINENO"  TO F-FIELDNAME
           MOVE 9            TO F-CBFIELDNAME
           MOVE SUB-20       TO F-EDNAMEFIELDNUM
           MOVE 6            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-NUMERIC.
       FILL-005.
           MOVE 1 TO F-INDEX SUB-4.
           PERFORM READ-CAMS-FILE.
       FILL-999.
            EXIT.
      *
       READ-TO-FIX-DATES SECTION.
       RTFD-000.
           MOVE 2725 TO POS
           DISPLAY "FIXING DATE SEQUENCES....." AT POS.
           
           MOVE "N" TO WS-CAMS-DATA-START.
           MOVE 0   TO SUB-21.
       RTFD-001.
           READ BANK-CAMS-FILE
               AT END
               GO TO RTFD-900.
       RTFD-005.
      ******************************************************************
      * READING TO SEE WHERE THE ACTUAL DATA STARTS, WHEN THIS IS TRUE *
      * NEXT LINE IS THE ACTUAL START OF INFO.                         *
      ******************************************************************
           IF WS-CAMS-DATA-START = "N"
            IF BANK-CAMS-STATEMENT-DATE NOT = "  DATE    "
              GO TO RTFD-001
            ELSE
              MOVE "Y" TO WS-CAMS-DATA-START
              GO TO RTFD-001.
      ******************************************************************
      * READING TO SEE WHERE THE ACTUAL DATA STOPS, WHEN THIS IS TRUE  *
      * THE CAMS FILE IS AT AN END.                                    *
      ******************************************************************
           IF BANK-CAMS-AMOUNT = "      CLOSING "
            OR = "      CLOSING B"
              MOVE " " TO BANK-CAMS-VAT-AMT
                          BANK-CAMS-FILL7
              GO TO RTFD-900.
       RTFD-010.
           PERFORM MAKE-BANK-DATE-CORRECT.
           IF WS-CAMS-DATA-START = "F"
              GO TO RTFD-900.
           PERFORM ERROR1-020.
           PERFORM ERROR-020.

           PERFORM RTFD-035.
           PERFORM RTFD-030.               
           GO TO RTFD-001.
       RTFD-030.
           ADD 1             TO SUB-21.
           
           MOVE "LINENO"     TO F-FIELDNAME
           MOVE 6            TO F-CBFIELDNAME
           MOVE SUB-21       TO F-EDNAMEFIELDNUM
           MOVE 6            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-NUMERIC.
       RTFD-035.
           REWRITE BANK-CAMS-REC.
           IF WS-CAMS-ST1 NOT = 0
              MOVE "CAMS-FILE ERC ON REWRITE RTFD, 'ESC' FOR STATUS."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-CAMS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RTFD-035.
       RTFD-040.
           GO TO RTFD-001.
       RTFD-900.
           MOVE "CAMS-FILE DATE RE-FORMAT FINISHED, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 2710 TO POS
              DISPLAY WS-MESSAGE AT POS.
              
           PERFORM CLEAR-FIELDS
           PERFORM CLEAR-BODY.
            
           PERFORM END-500.
           PERFORM OPEN-015.
           
           MOVE "LINENO"     TO F-FIELDNAME
           MOVE 6            TO F-CBFIELDNAME
           MOVE 0            TO F-EDNAMEFIELDNUM
           MOVE 6            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-NUMERIC.
       RTFD-999.
           EXIT.
      *
       MAKE-BANK-DATE-CORRECT SECTION.
       MBDC-001.
      *****************************************************************
      * NEW SECTION ADDED ONCE THE CAMS MONTHLY STATEMENT HAD BEEN    *
      * PUSHED TO ONCE PER MONTH, THEREBY MAKING THE STATEMENT-DATE   *
      * ALL 31ST OF THE MONTH.  THIS NOW FIXES THIS BY MOVING THE     *
      * TRANS-DATE (DD & MM) TO THE STATEMENT-DATE SO THAT THE ACTUAL *
      * TRANS-DATE, AS OPPOSED TO THE STATEMENT-DATE, IS POSTED TO    *
      * THE CBTRANS RECORD AS THE DATE.                               *
      *****************************************************************
      
            IF BANK-CAMS-DATE = " "
                MOVE "F" TO WS-CAMS-DATA-START
                GO TO MBDC-999.
      
            MOVE BANK-CAMS-DATE           TO WS-TRANS-DATE
            MOVE BANK-CAMS-STATEMENT-DATE TO WS-CAMS-SPLIT-DATE
            MOVE WS-TD-DD                 TO WS-CS-DD
            MOVE WS-TD-MM                 TO WS-CS-MM
            MOVE WS-CAMS-SPLIT-DATE       TO BANK-CAMS-STATEMENT-DATE.
       MBDC-999.
            EXIT.
      *
       READ-CAMS-FILE SECTION.
       RCF-000.
           MOVE "N" TO WS-CAMS-DATA-START.
           MOVE 0   TO SUB-21.
       RCF-001.
           READ BANK-CAMS-FILE
               AT END
               GO TO RCF-900.
       RCF-005.
      ******************************************************************
      * READING TO SEE WHERE THE ACTUAL DATA STARTS, WHEN THIS IS TRUE *
      * NEXT LINE IS THE ACTUAL START OF INFO.                         *
      ******************************************************************
           IF WS-CAMS-DATA-START = "N"
            IF BANK-CAMS-STATEMENT-DATE NOT = "  DATE    "
              GO TO RCF-001
            ELSE
              MOVE "Y" TO WS-CAMS-DATA-START
              GO TO RCF-001.
      ******************************************************************
      * READING TO SEE WHERE THE ACTUAL DATA STOPS, SO WE CAN GRAB THE *
      * BLANK LINE TO PRINT THE VAT IN BEFORE "Closing Bal" ENTRY.     *
      ******************************************************************
           IF WS-CAMS-DATA-START = "Y"
            IF BANK-CAMS-STATEMENT-DATE = " "
             IF BANK-CAMS-STATEMENT-NUM = " "
              IF BANK-CAMS-NARRATIVE = " "
               IF BANK-CAMS-AMOUNT NOT = "      CLOSING "
                IF WS-VAT-POSTED NOT = "Y"
                 MOVE "BANK CHRG VAT"   TO BANK-CAMS-FILL7
                 MOVE WS-TOTAL-VAT-AMT  TO F-EDRUNNINGAMOUNT
                 MOVE F-EDRUNNINGAMOUNT TO BANK-CAMS-VAT-AMT
                 PERFORM WRITE-FINAL-BANK-CBTRANS
                 PERFORM UPDATE-CASHBOOK
                 GO TO RCF-035.
      ******************************************************************
      * READING TO SEE WHERE THE ACTUAL DATA STOPS, WHEN THIS IS TRUE  *
      * THE CAMS FILE IS AT AN END.                                    *
      ******************************************************************
           IF BANK-CAMS-AMOUNT = "      CLOSING "
            OR = "      CLOSING B"
              MOVE " " TO BANK-CAMS-VAT-AMT
                          BANK-CAMS-FILL7
              GO TO RCF-900.
       RCF-010.
      ******************************************************************
      * CHECKING IN CASE THE NEXT LINE AFTER READING IN SALARIES IS    *
      * A FLAGGED ITEM. THIS WILL ALLOW THE SALARY TRANS TO BE         *
      * WRITTEN IN THE CB.                                             *
      ******************************************************************
      * CAMS
      *     IF BANK-CAMS-FOUND NOT = "  "
      *      IF WS-SAL-FOUND = "Y"
      *       IF WS-SAL-1-15 NOT = "Cams payment   "
      *        IF WS-SAL-1-20 NOT = "salaries paid       "
      *         MOVE " Salaries Paid                               "
      *            TO WS-NARRATIVE 
      *         PERFORM ASV-020
      *         PERFORM WRITE-SALARY-CBTRANS
      *         PERFORM UPDATE-CASHBOOK
      *         GO TO RCF-020.
      * ONLINE
           IF BANK-CAMS-FOUND NOT = "  "
            IF WS-SAL-FOUND = "Y"
             IF WS-SALOB-1-10 NOT = "FNB OB 000"
              IF WS-SALOB-1-13 NOT = "SALARIES PAID"
               MOVE " Salaries Paid                               "
                  TO WS-NARRATIVE 
               PERFORM ASV-020
               PERFORM WRITE-SALARY-CBTRANS
               PERFORM UPDATE-CASHBOOK
               GO TO RCF-020.
      ******************************************************************
      * CHECKING IN CASE A LINE HAS ALREADY BEEN CHECKED IN A PREVIOUS *
      * RUN SO WE DON'T RECHECK IT. COULD BE * OR NUMBERS FROM FEC'S   *
      ******************************************************************
           IF BANK-CAMS-FOUND NOT = "  "
               GO TO RCF-001.
       RCF-020.
           PERFORM CHECK-FOR-COMMAS
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-CAMS-AMOUNT.
       RCF-021.
           MOVE BANK-CAMS-NARRATIVE TO WS-NARRATIVE
                                       WS-DINERS-NARRATIVE
                                       WS-SALARY-NARRATIVE
                                       WS-SALARY-OB-NARRATIVE
                                       WS-LOANS-OB-NARRATIVE.
              
      ***************************************************************
      * CHECKING FOR "salaries paid".  SYSTEM WILL ADD ALL SALARIES *
      * TOGETHER AND WILL WRITE ONE CBTRANS RECORD. THE SECOND PART *
      * HERE ONLY WORKS IF THE NEXT ITEM IS NOT FLAGGED BY A RUN OR *
      * IS NOT THE LAST ITEM ON THE BANK STATEMENT                  *
      ***************************************************************
      * CAMS
      *     IF WS-SAL-1-15 = "Cams payment   "
      *      IF WS-SAL-1-20 = "salaries paid       "
      *        PERFORM ASV-005
      *        PERFORM CIM-950
      *        GO TO RCF-035.
      *     IF WS-SAL-FOUND = "Y"
      *      IF WS-SAL-1-15 NOT = "Cams payment   "
      *       IF WS-SAL-1-20 NOT = "salaries paid       "
      *        MOVE " Salaries Paid                               "
      *            TO WS-NARRATIVE 
      *        PERFORM ASV-020
      *        PERFORM WRITE-SALARY-CBTRANS
      *        PERFORM UPDATE-CASHBOOK
      *        GO TO RCF-020.
      * ONLINE
           IF WS-SALOB-1-10 = "FNB OB 000"
            IF WS-SALOB-1-13 = "SALARIES PAID"
              PERFORM ASV-005
              PERFORM CIM-950
              GO TO RCF-035.
           IF WS-SAL-FOUND = "Y"
            IF WS-SALOB-1-10 NOT = "FNB OB 000"
             IF WS-SALOB-1-13 NOT = "SALARIES PAID"
              MOVE " Salaries Paid                               "
                  TO WS-NARRATIVE 
              PERFORM ASV-020
              PERFORM WRITE-SALARY-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-020.

      ***********************************************************
      *CHECKING HERE FOR "#fx" WHICH = A BANK CHARGE ON THE FEC *
      * AND ALSO     FOR "Fx " WHICH = THE ACTUAL FEC ITSELF.   *
      *                                                         *
      * THE WAY THIS PROGRAM IS SET UP WE MUST HAVE 2x #fx FIRST*
      * AND THEN THE Fx ENTRY.  THIS WAY THE CORRECT VAT IS     *
      * CALCULATED ON THE CHARGES, STRIPPED OFF THE CHARGE AND  *
      * THEN THE NETT CHARGE IS ADDED TO THE Fx FEC.            *
      ***********************************************************
           IF WS-NAR-1ST = "#"
            IF WS-NAR2-3 = "FX"
              PERFORM CIM-950
              ADD 1 TO SUB-3
              PERFORM ADD-FOREX-VALUES
              GO TO RCF-035.
           IF WS-NAR-1ST = "F"
            IF WS-NAR2-3 = "X "
              PERFORM CIM-950
              ADD 1 TO SUB-3
              PERFORM ADD-FOREX-VALUES.
      ****************************************************************
      * THIS FX ENTRY IS FOR LATE ENTRIES THAT ARE NOT ALLOCATED TO  *
      * A SPECIFIC FEC.  WE SPLIT THE VAT AND WRITE AN ENTRY FOR THE *
      * CHARGE TO 50-200-15-00                                       *
      ****************************************************************
           IF WS-NAR-1ST = "@"
            IF WS-NAR2-3 = "FX"
              PERFORM ADD-BANK-VAT
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              PERFORM WRITE-BANK-FEC-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.

           IF WS-NAR-1ST = "#"
            IF WS-NAR2-3 NOT = "FX"
              PERFORM ADD-BANK-VAT
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              PERFORM WRITE-BANK-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      ****************************************
      * BOND PAYMENTS IN PROPERTY COMPANIES  *
      ****************************************
      * FOR QTM, HKY & KRS & KGI
           IF WS-SALOB-1-10 = "FNB OB 000"
            IF WS-SALOB-1-13 = "BOND PAYMENT"
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Bond Payment FNB                            "
                  TO WS-NARRATIVE 
              MOVE "75-030-30-05" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      * FOR SFI
           IF WS-LOAN-1-47 = 
                "FNB/PROPLN   00003000009962594                 "
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Bond Payment FNB                            "
                  TO WS-NARRATIVE 
              MOVE "75-030-30-05" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.

      ***************************************************************
      * CHECK IF ENTRIES MADE FOR INTER GROUP LOANS OR LOANS PD.    *
      * THIS HOLDS VALID ONLY FOR ONLINE BANKING.  CAMS BANKING     *
      * HAS DIFFERENT FIELD VALUES - SEE BELOW SECTION - TAKEN OUT. *
      ***************************************************************
      * LOANS PROCESSED IN CTJ
      * GRC=2
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "CTJ-GRC LOAN              "
             IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-20-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "GRC-CTJ LO"
             IF WS-LOAN-4-26 = "AN PD                     "
              IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-20-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      * CTN=3
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "CTJ-CTN LOAN              "
             IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-25-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "CTN-CTJ LO"
             IF WS-LOAN-4-26 = "AN PD                     "
              IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-25-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      * ORX=5
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "CTJ-ORX LOAN              "
             IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-35-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "ORX-CTJ LO"
             IF WS-LOAN-4-26 = "AN PD"
              IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-35-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      * SFJ=7
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "CTJ-SFJ LOAN              "
             IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-45-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "SFJ-CTJ LO"
             IF WS-LOAN-4-26 = "AN PD                     "
              IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-45-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      * CSC=8
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "CTJ-CSC LOAN              "
             IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-50-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "CSC-CTJ LO"
             IF WS-LOAN-4-26 = "AN PD                     "
              IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-50-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      * SFI=9
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "CTJ-SFI LOAN              "
             IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-55-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "SFI-CTJ LO"
             IF WS-LOAN-4-26 = "AN PD                     "
              IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-55-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      * QTM=10
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "CTJ-QTM LOAN              "
             IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-60-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "QTM-CTJ LO"
             IF WS-LOAN-4-26 = "AN PD                     "
              IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-60-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      * HKY=11
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "CTJ-HKY LOAN              "
             IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-65-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "HKY-CTJ LO"
             IF WS-LOAN-4-26 = "AN PD                     "
              IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-65-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      * KRS=12
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "CTJ-KRS LOAN              "
             IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-70-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "KRS-CTJ LO"
             IF WS-LOAN-4-26 = "AN PD                     "
              IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-70-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      * KGI=4
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "CTJ-KGI LOAN              "
             IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-75-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "KGI-CTJ LO"
             IF WS-LOAN-4-26 = "AN PD                     "
              IF WS-CO-NUMBER = 1
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-75-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
              
      * LOANS PROCESSED IN GRC
      * GRC=2
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "CTJ-GRC LO"
             IF WS-LOAN-4-26 = "AN                        "
              IF WS-CO-NUMBER = 2
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "GRC-CTJ LOAN PD           "
             IF WS-CO-NUMBER = 2
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
              
      * LOANS PROCESSED IN CTN
      * CTN=3
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "CTJ-CTN LO"
             IF WS-LOAN-4-26 = "AN                        "
              IF WS-CO-NUMBER = 3
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "CTN-CTJ LOAN PD           "
             IF WS-CO-NUMBER = 3
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
              
      * LOANS PROCESSED IN ORX
      * ORX=5
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "CTJ-ORX LO"
             IF WS-LOAN-4-26 = "AN                        "
              IF WS-CO-NUMBER = 5
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "ORX-CTJ LOAN PD"
             IF WS-CO-NUMBER = 5
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.

      * LOANS PROCESSED IN SFJ
      * SFJ=7
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "CTJ-SFJ LO"
             IF WS-LOAN-4-26 = "AN                        "
              IF WS-CO-NUMBER = 7
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan to Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "SFJ-CTJ LOAN PD           "
             IF WS-CO-NUMBER = 7
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "SFJ-SFI LOAN"
              IF WS-CO-NUMBER = 7
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan to Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-55-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "SFI-SFJ LO"
             IF WS-LOAN-4-26 = "AN PD                     "
             IF WS-CO-NUMBER = 7
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-55-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      * CSC=8
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "SFJ-CSC LO"
             IF WS-LOAN-4-26 = "AN                        "
              IF WS-CO-NUMBER = 7
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-50-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "CSC-SFJ LOAN PD           "
             IF WS-CO-NUMBER = 7
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-50-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      * SFI=9
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "SFJ-SFI LO"
             IF WS-LOAN-4-26 = "AN                        "
              IF WS-CO-NUMBER = 7
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-55-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "SFI-SFJ LOAN PD           "
             IF WS-CO-NUMBER = 7
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-55-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
              
      * LOANS PROCESSED IN CSC
      * CTJ=1
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "CTJ-CSC LO"
             IF WS-LOAN-4-26 = "AN                        "
              IF WS-CO-NUMBER = 8
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan to Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "CSC-CTJ LOAN PD           "
             IF WS-CO-NUMBER = 8
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      * SFJ=7
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "SFJ-CSC LO"
             IF WS-LOAN-4-26 = "AN                        "
              IF WS-CO-NUMBER = 8
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-45-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "SFJ-CSC LOAN PD           "
             IF WS-CO-NUMBER = 8
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-45-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
              
      * LOANS PROCESSED IN SFI
      * SFJ=7
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "SFJ-SFI LO"
             IF WS-LOAN-4-26 = "AN                        "
              IF WS-CO-NUMBER = 9
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan to Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-45-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "SFI-SFJ LOAN PD           "
             IF WS-CO-NUMBER = 9
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-45-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
              
      * LOANS PROCESSED IN QTM
      * CTJ=1
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "CTJ-QTM LO"
             IF WS-LOAN-4-26 = "AN                        "
              IF WS-CO-NUMBER = 10
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "QTM-CTJ LOAN PD           "
             IF WS-CO-NUMBER = 10
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.

      * PROCESSED IN HKY
      * CTJ=1
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "CTJ-HKY LO"
             IF WS-LOAN-4-26 = "AN                        "
              IF WS-CO-NUMBER = 11
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF     "
            IF WS-LOAN-4-26 = "HKY-CTJ LOAN PD"
             IF WS-CO-NUMBER = 11
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
              
      * PROCESSED IN KRS
      * CTJ=1
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "CTJ-KRS LO"
             IF WS-LOAN-4-26 = "AN                        "
              IF WS-CO-NUMBER = 12
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "KRS-CTJ LOAN PD           "
             IF WS-CO-NUMBER = 12
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      * PROCESSED IN KGI
      * CTJ=1
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "CTJ-KGI LO"
             IF WS-LOAN-4-26 = "AN                        "
              IF WS-CO-NUMBER = 4
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan To Company                             "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "KGI-CTJ LOAN PD           "
             IF WS-CO-NUMBER = 4
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Loan Paid                                   "
                  TO WS-NARRATIVE 
              MOVE "75-040-05-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.

      ********************************************
      * CALL ACCOUNT TRANSFERS - ONLINE BANKING  *
      ********************************************
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-4-26 = "CHEQUE TO CALL            "
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Cheque to Call                              "
                  TO WS-NARRATIVE 
              MOVE "75-020-35-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-LOAN-1-10 = "FNB OB TRF"
            IF WS-LOAN-3-10 = "CALL TO CH"
             IF WS-LOAN-4-26 = "EQUE                      "
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Call to Cheque                              "
                  TO WS-NARRATIVE 
              MOVE "75-020-35-00" TO CBTRANS-ACCOUNT-NUMBER
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-LOAN-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
              
      ****************************************
      * GENERAL BANK DEBITS & CREDITS        *              
      ****************************************
           IF WS-NAR-1-21 = "GENERAL DEBIT        "
              PERFORM ADD-BANK-VAT
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " General debit                               "
                  TO WS-NARRATIVE 
              PERFORM WRITE-BANK-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
              
           IF WS-NAR-1-21 = "GENERAL CREDIT       "
              PERFORM ADD-BANK-VAT
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " General credit                              "
                  TO WS-NARRATIVE 
              PERFORM WRITE-BANK-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.

           IF WS-NAR-1-21 = "FIRSTCARD     0000000"
            IF WS-CAMS-AMOUNT < 0
              PERFORM ADD-BANK-VAT
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Firstcard Charges                           "
                  TO WS-NARRATIVE 
              PERFORM WRITE-CARDCHARGE-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-NAR-1-21 = "SPEEDPOINT    0000000"
            IF WS-CAMS-AMOUNT < 0
              PERFORM ADD-BANK-VAT
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Speedpoint Charges                          "
                  TO WS-NARRATIVE 
              PERFORM WRITE-CARDCHARGE-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-NAR-1-21 = "F/CARD COMSPEEDPOINT0"
            IF WS-CAMS-AMOUNT < 0
              PERFORM ADD-BANK-VAT
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Speedpoint Charges                          "
                  TO WS-NARRATIVE 
              PERFORM WRITE-CARDCHARGE-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-NAR-1-21 = "F/CARD COMSPEEDPOINT "
            IF WS-CAMS-AMOUNT < 0
              PERFORM ADD-BANK-VAT
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Speedpoint Charges                          "
                  TO WS-NARRATIVE 
              PERFORM WRITE-CARDCHARGE-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-NAR-1-21 = "F/CARD COMSPEEDPOINT4"
            IF WS-CAMS-AMOUNT < 0
              PERFORM ADD-BANK-VAT
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Speedpoint Charges                          "
                  TO WS-NARRATIVE 
              PERFORM WRITE-CARDCHARGE-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-NAR-1-21 = "F/CARD COMSPEEDPOINT9"
            IF WS-CAMS-AMOUNT < 0
              PERFORM ADD-BANK-VAT
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Speedpoint Charges                          "
                  TO WS-NARRATIVE 
              PERFORM WRITE-CARDCHARGE-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-NAR-1-10 = "DINERSCLUB"
            IF WS-CAMS-AMOUNT < 0
              PERFORM ADD-BANK-VAT
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Dinersclub Charges                       "
                  TO WS-NARRATIVE 
              PERFORM WRITE-CARDCHARGE-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-NAR-1-10 = "AMEXCARD  " OR = "AMEX MERCH"
                       OR = "MERCH DISC"
            IF WS-CAMS-AMOUNT < 0
              PERFORM ADD-BANK-VAT
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE " Amexcard Charges                         "
                  TO WS-NARRATIVE 
              PERFORM WRITE-CARDCHARGE-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
              
           IF WS-NAR-1-21 = "GOVERNMENT DUTY      "
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              MOVE " Government Duty                          "
                  TO WS-NARRATIVE
              PERFORM WRITE-BANK-CBTRANS
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
              
           IF WS-NAR-1-21 = "PENALTY INTEREST     "
            OR = "INTEREST ON DEBIT BAL"
             OR = "INT ON DEBIT BALANCE "
              MOVE " *"           TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-BANK-DR-INTEREST
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-NAR-1-21 = "INTEREST ON CREDIT BA"
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-BANK-CR-INTEREST
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
           IF WS-NAR-1-21 = "INT ON CREDIT BALANCE"
              MOVE " *" TO BANK-CAMS-FOUND
              PERFORM CIM-950
              MOVE WS-CAMS-AMOUNT TO WS-BANK-NETT-AMT
              PERFORM WRITE-BANK-CR-INTEREST
              PERFORM UPDATE-CASHBOOK
              GO TO RCF-030.
      ***********************************************************
      * CHECKING HERE FOR "Cheque" = A CHEQUE WRITTEN OUT       *
      * IF TRUE THEN DO ROUTINE FOR CHECKING THE CHEQUE NUMBER. *
      ***********************************************************
           IF WS-NAR-1-21 = "CHEQUE"
               MOVE BANK-CAMS-CH-REFNUM TO ALPHA-RATE
               PERFORM DECIMALISE-RATE
               MOVE NUMERIC-RATE TO WS-CHEQUE-NUMBER.
           
           PERFORM CHECK-IN-MEMORY.
           PERFORM ERROR1-020.

           IF BANK-CAMS-FOUND NOT = "  "
               GO TO RCF-030.

           PERFORM RCF-030.               
           GO TO RCF-001.
       RCF-030.
           ADD 1             TO SUB-21.
           
           MOVE "LINENO"     TO F-FIELDNAME
           MOVE 6            TO F-CBFIELDNAME
           MOVE SUB-21       TO F-EDNAMEFIELDNUM
           MOVE 6            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-NUMERIC.
       RCF-035.
           REWRITE BANK-CAMS-REC.
           IF WS-CAMS-ST1 NOT = 0
              MOVE "CAMS-FILE ERROR ON REWRITE, 'ESC' FOR STATUS."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CAMS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RCF-035.
       RCF-040.
           GO TO RCF-001.
       RCF-900.
           MOVE "CAMS-FILE FINISHED, 'ESC' TO REWRITE THE CB-TRANS."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
              
            MOVE 2910 TO POS
            DISPLAY "WRITING CHANGES TO CBTRANS-FILE....." AT POS
            PERFORM WRITE-CBTRANS
            PERFORM CLEAR-FIELDS
            PERFORM CLEAR-BODY.
       RCF-999.
           EXIT.
      *
       CHECK-IN-MEMORY SECTION.
       CIM-001.
           MOVE 2910 TO POS
           DISPLAY "CHECKING IN MEMORY ....             " AT POS.
           MOVE 1 TO SUB-10.
           MOVE WS-CAMS-AMOUNT TO WS-BANK-CAMS-AMT.
           
      *     MOVE WS-CAMS-AMOUNT TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE WS-BANK-CAMS-AMT TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     MOVE WS-CB-AMOUNT (SUB-10) TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
       CIM-005.
      * INCASE A TRANSACTION HAS A ZERO VALUE.  SHOULDN'T HAPPEN
      * BUT IT HAS IN THE PAST ON THE ODD OCCASSION.
      
           IF WS-CB-AMOUNT (SUB-10) = 0
            IF WS-TRANS-NUM (SUB-10) = 0
              GO TO CIM-950
            ELSE
              ADD 1 TO SUB-10
              GO TO CIM-006.
              
           IF WS-BANK-CAMS-AMT = WS-CB-AMOUNT (SUB-10)
              GO TO CIM-010.

           IF WS-BANK-CAMS-AMT NOT = WS-CB-AMOUNT (SUB-10)
              ADD 1 TO SUB-10.

       CIM-006.
           IF SUB-10 < 1001
              GO TO CIM-005
           ELSE
              GO TO CIM-950.
       CIM-010.
      ***********************************************************
      * THIS HAPPENS IF WS-CAMS-AMOUNT = WS-CB-AMOUNT (SUB-10)  *
      * BUT IF ALREADY = Y THEN THIS WAS PREVIOUSLY ALLOC. IN   *
      * THIS RUN - POSSIBLY DUPLICATE ENTRIES IN THE CB-TRANS   *
      * FILE WHICH SEEMS TO HAPPEN. THEREFORE GO BACK AND CHECK *
      * FOR THE NEXT ENTRY WHICH MAY HAVE THE SAME AMOUNT       *
      ***********************************************************
           IF WS-CAMS-FOUND (SUB-10) = "Y"
                ADD 1 TO SUB-10
                GO TO CIM-005.
       CIM-020.
      ***********************************************************
      * HERE WE ARE GOING TO CHECK FOR THE CHEQUE NUMBER TO SEE *
      * IF WE HAVE THE CORRECT CHEQUE INCASE THERE ARE TWO OR   *
      * MORE CHEQUE'S WITH THE SAME RAND AMOUNT.                *
      ***********************************************************
           IF WS-CAMS-AMOUNT < 0
            IF WS-NAR-1-21 = "CHEQUE"
              PERFORM STRIP-CHEQUE-NUMBER-FROM-CB
            IF WS-CHEQUE-SAME = "Y"
              MOVE "Y"  TO WS-CAMS-FOUND (SUB-10)
              MOVE " *" TO BANK-CAMS-FOUND
              GO TO CIM-950.
       CIM-030.
           IF WS-BANK-CAMS-AMT = WS-CB-AMOUNT (SUB-10)
              MOVE "Y" TO WS-CAMS-FOUND (SUB-10)
            IF BANK-CAMS-FOUND = "  "
              MOVE " *" TO BANK-CAMS-FOUND
              GO TO CIM-950.
           IF WS-BANK-CAMS-AMT = WS-CB-AMOUNT (SUB-10)
              MOVE "Y" TO WS-CAMS-FOUND (SUB-10)
           IF WS-NAR-1ST = "F"
            IF WS-NAR2-3 = "X "
              MOVE "*" TO BANK-CAMS-FEC-FOUND.
       CIM-950.
           IF F-INDEX > 15
              PERFORM CLEAR-BODY
              MOVE 1 TO F-INDEX.
           PERFORM MBDC-001.
           PERFORM SCROLL-010.
           ADD 1 TO F-INDEX.
       CIM-999.
           EXIT.
      *
       CHECK-FOR-COMMAS SECTION.
       CFC-005.
           MOVE 2910 TO POS
           DISPLAY "CHECKING FOR COMMAS ....             " AT POS.
           MOVE " "              TO ALPHA-RATE FAX-RATE.
           MOVE BANK-CAMS-AMOUNT TO ALPHA-RATE
           MOVE 1                TO SUB-1 SUB-2.
       CFC-010.
           IF AL-RATE (SUB-1) NOT = ","
              MOVE AL-RATE (SUB-1) TO FX-RATE (SUB-2)
              ADD 1 TO SUB-1 SUB-2
            IF SUB-1 < 15
              GO TO CFC-010
            ELSE
              GO TO CFC-900.
              
           IF AL-RATE (SUB-1) = ","
              ADD 1 TO SUB-1
              MOVE AL-RATE (SUB-1) TO FX-RATE (SUB-2)
              ADD 1 TO SUB-1 SUB-2
            IF SUB-1 < 15
              GO TO CFC-010
            ELSE
              GO TO CFC-900.
       CFC-900.
           MOVE FAX-RATE TO ALPHA-RATE.
       CFC-999.
           EXIT.
      *
       STRIP-CHEQUE-NUMBER-FROM-CB SECTION.
       SCNFC-005.
           MOVE 2910 TO POS
           DISPLAY "STRIPPING CHEQUE NUMBER ....        " AT POS.
           MOVE " "                       TO ALPHA-RATE FAX-RATE
           MOVE WS-CB-LINE-DESC (SUB-10)  TO ALPHA-RATE
           MOVE 1                         TO SUB-7 SUB-8.
       SCNFC-010.
           IF AL-RATE (SUB-7) NOT = "P"
              ADD 1 TO SUB-7
            IF SUB-7 < 25
              GO TO SCNFC-010
            ELSE
              GO TO SCNFC-900.
       SCNFC-020.
           IF AL-RATE (SUB-7) = "P"
                ADD 1 TO SUB-7
            IF AL-RATE (SUB-7) = "M"
                ADD 1 TO SUB-7
             IF AL-RATE (SUB-7) = "T"
                ADD 1 TO SUB-7
                GO TO SCNFC-030.
            GO TO SCNFC-900.
       SCNFC-030.
            MOVE AL-RATE (SUB-7) TO FX-RATE (SUB-8)
              ADD 1 TO SUB-7 SUB-8
            IF SUB-7 < 25
              GO TO SCNFC-030
            ELSE
              MOVE 1 TO SUB-8
              GO TO SCNFC-500.
       SCNFC-500.
           IF FX-RATE (SUB-7) NOT = "0" AND NOT = "1" AND NOT = "3"
              AND NOT = "4" AND NOT = "5" AND NOT = "6" AND NOT = "7"
              AND NOT = "8" AND NOT = "9"
              GO TO SCNFC-900.
       
           ADD 1 TO SUB-7.
           IF SUB-7 < 26
              GO TO SCNFC-500.
           MOVE FAX-RATE TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-CB-CHEQUE-NUM.
           
           IF WS-CB-CHEQUE-NUM = WS-CHEQUE-NUMBER
               MOVE "Y" TO WS-CHEQUE-SAME
               GO TO SCNFC-999.
       SCNFC-900.
           MOVE "N" TO WS-CHEQUE-SAME.
       SCNFC-999.
           EXIT.
      *
       ADD-BANK-VAT SECTION.
       ABV-005.
      *    IF WS-NAR-1ST = "#"
             MOVE WS-CAMS-AMOUNT TO WS-BANK-GROSS-AMT
             COMPUTE WS-BANK-VAT-AMT ROUNDED
              = (WS-BANK-GROSS-AMT * 15) / 115
             COMPUTE WS-BANK-NETT-AMT
              = WS-BANK-GROSS-AMT - WS-BANK-VAT-AMT.
           
             MOVE WS-BANK-VAT-AMT   TO F-EDRUNNINGAMOUNT
             MOVE F-EDRUNNINGAMOUNT TO BANK-CAMS-VAT-AMT
             ADD WS-BANK-VAT-AMT    TO WS-TOTAL-VAT-AMT.
       ABV-999.
           EXIT.
      *
       ADD-FOREX-VALUES SECTION.
       AFV-005.
      **************************************************************
      * SUB-3 IS USED TO KEEP TRACK OF THE THREE ENTRIES THAT MAKE *
      *       UP THE FEC.  2 X #fx CHARGES AND THE Fx ENTRY ITSELF *
      *                                                            *
      * SUB-4 WILL BE THE COUNTER OF FEC ENTRIES TO BE PRINTED     *
      *       IN BANK-CAMS-FOUND IN PLACE OF THE ' *'              *
      **************************************************************
           IF SUB-3 = 1 OR = 2
             MOVE WS-NAR-20      TO WS-FX-ENTRY (SUB-3)
             MOVE WS-NAR-46-47   TO WS-FX-FNL (SUB-3)
             MOVE WS-CAMS-AMOUNT TO WS-FX-GROSS-AMT (SUB-3)
             COMPUTE WS-FX-VAT-AMT (SUB-3) ROUNDED
              = (WS-FX-GROSS-AMT (SUB-3) * 15) / 115
             COMPUTE WS-FX-NETT-AMT (SUB-3)
              = WS-FX-GROSS-AMT (SUB-3) - WS-FX-VAT-AMT (SUB-3)
             GO TO AFV-020.

           IF SUB-3 = 3
             MOVE WS-NAR-1-21              TO WS-FX-ENTRY (SUB-3)
             MOVE WS-NAR-46-47             TO WS-FX-FNL (SUB-3)
             MOVE WS-CAMS-AMOUNT           TO WS-FX-GROSS-AMT (SUB-3)
             MOVE 0                        TO WS-FX-VAT-AMT (SUB-3)
             MOVE WS-CAMS-AMOUNT           TO WS-FX-NETT-AMT (SUB-3).
       AFV-010.
           IF SUB-3 = 3
              ADD WS-FX-VAT-AMT (1)        TO WS-FX-VAT-AMT (3)
              ADD WS-FX-VAT-AMT (2)        TO WS-FX-VAT-AMT (3)
              ADD WS-FX-NETT-AMT (1)       TO WS-FX-NETT-AMT (3)
              ADD WS-FX-NETT-AMT (2)       TO WS-FX-NETT-AMT (3)
              MOVE WS-FX-NETT-AMT (3)      TO WS-CAMS-AMOUNT
              MOVE WS-CAMS-AMOUNT          TO WS-BANK-CAMS-AMT.
       AFV-020.
           MOVE SUB-4                      TO WS-CALC-PERIOD
           MOVE WS-CALC-PERIOD             TO BANK-CAMS-FOUND.
           
           IF SUB-3 = 1 OR = 2
               MOVE WS-FX-VAT-AMT (SUB-3)  TO F-EDRUNNINGAMOUNT
               MOVE F-EDRUNNINGAMOUNT      TO BANK-CAMS-VAT-AMT
               ADD WS-FX-VAT-AMT (SUB-3)   TO WS-TOTAL-VAT-AMT.
           IF SUB-3 = 3
               MOVE WS-FX-NETT-AMT (SUB-3) TO F-EDRUNNINGAMOUNT
               MOVE F-EDRUNNINGAMOUNT      TO BANK-CAMS-VAT-AMT.
       AFV-500.
           IF SUB-3 = 3
              ADD 1  TO SUB-4
              MOVE 0 TO SUB-3.
       AFB-999.
           EXIT.
      *
       ADD-SALARY-VALUES SECTION.
       ASV-005.
           IF WS-SAL-FOUND NOT = "Y"
              MOVE "Y" TO WS-SAL-FOUND.
           ADD WS-CAMS-AMOUNT  TO WS-SALARY-AMT.
           
           MOVE "Sy"           TO BANK-CAMS-FOUND.
       ASV-020.
           MOVE WS-SALARY-AMT  TO WS-CAMS-AMOUNT
           MOVE WS-CAMS-AMOUNT TO WS-BANK-CAMS-AMT.
           
           MOVE 0              TO WS-SALARY-AMT.
           MOVE " "            TO WS-SAL-FOUND.
       ASV-999.
           EXIT.
      *
       WRITE-CBTRANS SECTION.
       WCBT-001.
           PERFORM ERROR1-020.
           
           MOVE 0 TO SUB-20.
           MOVE 1 TO SUB-1.
           IF WS-TRANS-NUM (SUB-1) = " "
              GO TO WCBT-900.
       WCBT-002.
           MOVE WS-TRANS-NUM (SUB-1) TO CBTRANS-TRANS
           MOVE WS-TYPE-NUM (SUB-1)  TO CBTRANS-TYPE.
           START CBTRANS-FILE KEY NOT < CBTRANS-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
                MOVE "CBTRANS BUSY ON START, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO WCBT-002.
           IF WS-CAMS-FOUND (SUB-1) NOT = "Y"
               GO TO WCBT-020.
           GO TO WCBT-010.
       WCBT-005.
           MOVE WS-TRANS-NUM (SUB-1) TO CBTRANS-TRANS
           MOVE WS-TYPE-NUM (SUB-1)  TO CBTRANS-TYPE.
           IF WS-CAMS-FOUND (SUB-1) NOT = "Y"
               GO TO WCBT-020.
       WCBT-010.
           READ CBTRANS-FILE WITH LOCK
                  INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
               MOVE "CBTRANS BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO WCBT-010.
           MOVE "Y" TO CBTRANS-ALLOCATED.
           
           ADD 1 TO SUB-20.
           MOVE 2910 TO POS
           DISPLAY "REWRITING RECORD NUMBER:" AT POS
           ADD 25 TO POS
           MOVE SUB-20 TO F-EDNAMEFIELDCRED
           DISPLAY F-EDNAMEFIELDCRED AT POS.
       WCBT-018.
           REWRITE CBTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
                MOVE "CBTRANS BUSY ON REWRITE, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO WCBT-018.
       WCBT-020.
            ADD 1 TO SUB-1.
            IF SUB-1 < 1001
             IF WS-TRANS-NUM (SUB-1) = 0
                GO TO WCBT-900
             ELSE
                GO TO WCBT-005.
       WCBT-900.
            PERFORM ERROR1-020
            MOVE 1 TO SUB-1.
       WCBT-999.
            EXIT.
     *
       UPDATE-CASHBOOK SECTION.
       UCB-000.
           MOVE Ws-CbMast TO CB-NUMBER.
           START CB-MASTER KEY NOT < CB-KEY
              INVALID KEY NEXT SENTENCE.
       UCB-010.
           READ CB-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
                MOVE " " TO CB-DESCRIPTION
                GO TO UCB-999.
           IF WS-CB-ST1 NOT = 0
                MOVE "CBMASTER BUSY ON READ, PRESS 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CB-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CB-ST1
                GO TO UCB-010.
       UCB-020.
           ADD CBTRANS-AMOUNT TO CB-BALANCE
                                 CB-PER (WS-NO).
       UCB-030.
            REWRITE CB-RECORD
               INVALID KEY NEXT SENTENCE.
            IF WS-CB-ST1 NOT = 0
                MOVE "CBMASTER BUSY ON REWRITE, PRESS 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CB-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-CB-ST1
                GO TO UCB-030.
       UCB-999.
           EXIT.
      *
       WRITE-LOAN-CBTRANS SECTION.
       W-LOAN-001.
           PERFORM READ-GLPARAMETER-LOCK.

           MOVE GLPA-CBTRANSNO            TO CBTRANS-TRANS
           ADD 1                          TO GLPA-CBTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 6                         TO CBTRANS-TYPE
           MOVE WS-BATCH                  TO CBTRANS-REFERENCE
           MOVE WS-PERIOD                 TO CBTRANS-PERIOD
           MOVE Ws-CbMast                 TO CBTRANS-CBMASTER
           PERFORM CONVERT-CAMS-DATE-FORMAT.
           MOVE SPLIT-DATE                TO CBTRANS-DATE
           MOVE "S"                       TO CBTRANS-TYPE-OF-POST
           MOVE "Y"                       TO CBTRANS-ALLOCATED
      *     MOVE "50-025-05-00"            TO CBTRANS-ACCOUNT-NUMBER
           MOVE WS-BANK-NETT-AMT          TO CBTRANS-AMOUNT.
           PERFORM SET-BATCH-LINE-DETAILS.
           MOVE FAX-RATE                  TO CBTRANS-LINE-DESC.
       W-LOAN-018.
           WRITE CBTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
                MOVE "CBTRANS BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO W-LOAN-018.
       W-LOAN-999.
            EXIT.
      *
       WRITE-BANK-CBTRANS SECTION.
       WBANKTR-001.
           PERFORM READ-GLPARAMETER-LOCK.

           MOVE GLPA-CBTRANSNO            TO CBTRANS-TRANS
           ADD 1                          TO GLPA-CBTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 6                         TO CBTRANS-TYPE
           MOVE WS-BATCH                  TO CBTRANS-REFERENCE
           MOVE WS-PERIOD                 TO CBTRANS-PERIOD
           MOVE Ws-CbMast                 TO CBTRANS-CBMASTER
           PERFORM CONVERT-CAMS-DATE-FORMAT.
           MOVE SPLIT-DATE                TO CBTRANS-DATE
           MOVE "S"                       TO CBTRANS-TYPE-OF-POST
           MOVE "Y"                       TO CBTRANS-ALLOCATED
           MOVE "50-025-05-00"            TO CBTRANS-ACCOUNT-NUMBER
           MOVE WS-BANK-NETT-AMT          TO CBTRANS-AMOUNT.
           PERFORM SET-BATCH-LINE-DETAILS.
           MOVE FAX-RATE                  TO CBTRANS-LINE-DESC.
       WBANKTR-018.
           WRITE CBTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
                MOVE "CBTRANS BUSY ON WRITE-BANK, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO WBANKTR-018.
       WBANKTR-999.
            EXIT.
      *
       WRITE-BANK-FEC-CBTRANS SECTION.
       WBANKFEC-001.
           PERFORM READ-GLPARAMETER-LOCK.

           MOVE GLPA-CBTRANSNO            TO CBTRANS-TRANS
           ADD 1                          TO GLPA-CBTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 6                         TO CBTRANS-TYPE
           MOVE WS-BATCH                  TO CBTRANS-REFERENCE
           MOVE WS-PERIOD                 TO CBTRANS-PERIOD
           MOVE Ws-CbMast                 TO CBTRANS-CBMASTER
           PERFORM CONVERT-CAMS-DATE-FORMAT.
           MOVE SPLIT-DATE                TO CBTRANS-DATE
           MOVE "S"                       TO CBTRANS-TYPE-OF-POST
           MOVE "Y"                       TO CBTRANS-ALLOCATED
           MOVE "50-200-15-00"            TO CBTRANS-ACCOUNT-NUMBER
           MOVE WS-BANK-NETT-AMT          TO CBTRANS-AMOUNT.
           PERFORM SET-BATCH-LINE-DETAILS.
           MOVE FAX-RATE                  TO CBTRANS-LINE-DESC.
       WBANKFEC-018.
           WRITE CBTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
                MOVE "CBTRANS BUSY ON WRITE-FEC-FBC, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO WBANKFEC-018.
       WBANKFEC-999.
            EXIT.
      *
       WRITE-SALARY-CBTRANS SECTION.
       W-SAL-001.
           PERFORM READ-GLPARAMETER-LOCK.

           MOVE GLPA-CBTRANSNO            TO CBTRANS-TRANS
           ADD 1                          TO GLPA-CBTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 1                         TO CBTRANS-TYPE
           MOVE WS-BATCH                  TO CBTRANS-REFERENCE
           MOVE WS-PERIOD                 TO CBTRANS-PERIOD
           MOVE Ws-CbMast                 TO CBTRANS-CBMASTER
           PERFORM CONVERT-CAMS-DATE-FORMAT.
           MOVE SPLIT-DATE                TO CBTRANS-DATE
           MOVE "S"                       TO CBTRANS-TYPE-OF-POST
           MOVE "Y"                       TO CBTRANS-ALLOCATED.
           
           MOVE "50-106-02-00"            TO CBTRANS-ACCOUNT-NUMBER
           MOVE WS-BANK-CAMS-AMT          TO CBTRANS-AMOUNT.
           PERFORM SET-BATCH-LINE-DETAILS.
           MOVE FAX-RATE                  TO CBTRANS-LINE-DESC.
       W-SAL-018.
           WRITE CBTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
                MOVE "CBTRANS BUSY ON SALARY WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO W-SAL-018.
       W-SAL-999.
            EXIT.
      *
       WRITE-CARDCHARGE-CBTRANS SECTION.
       WFCC-001.
           PERFORM READ-GLPARAMETER-LOCK.

           MOVE GLPA-CBTRANSNO            TO CBTRANS-TRANS
           ADD 1                          TO GLPA-CBTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 6                         TO CBTRANS-TYPE
           MOVE WS-BATCH                  TO CBTRANS-REFERENCE
           MOVE WS-PERIOD                 TO CBTRANS-PERIOD
           MOVE Ws-CbMast                 TO CBTRANS-CBMASTER
           PERFORM CONVERT-CAMS-DATE-FORMAT.
           MOVE SPLIT-DATE                TO CBTRANS-DATE
           MOVE "S"                       TO CBTRANS-TYPE-OF-POST
           MOVE "Y"                       TO CBTRANS-ALLOCATED
           MOVE "50-025-15-00"            TO CBTRANS-ACCOUNT-NUMBER
           MOVE WS-BANK-NETT-AMT          TO CBTRANS-AMOUNT.
           PERFORM SET-BATCH-LINE-DETAILS.
           MOVE FAX-RATE                  TO CBTRANS-LINE-DESC.
       WFCC-018.
           WRITE CBTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
                MOVE "CBTRANS BUSY ON WRITE-CARDCHRG, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO WFCC-018.
       WFCC-999.
            EXIT.
      *
       WRITE-FINAL-BANK-CBTRANS SECTION.
       WFBNK-001.
           PERFORM READ-GLPARAMETER-LOCK.

           MOVE GLPA-CBTRANSNO            TO CBTRANS-TRANS
           ADD 1                          TO GLPA-CBTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 6                         TO CBTRANS-TYPE
           MOVE WS-BATCH                  TO CBTRANS-REFERENCE
           MOVE WS-PERIOD                 TO CBTRANS-PERIOD
           MOVE Ws-CbMast                 TO CBTRANS-CBMASTER
           PERFORM DTI-010
           MOVE SPLIT-DATE                TO CBTRANS-DATE
           MOVE "S"                       TO CBTRANS-TYPE-OF-POST
           MOVE "Y"                       TO CBTRANS-ALLOCATED
           MOVE "75-030-05-20"            TO CBTRANS-ACCOUNT-NUMBER
           MOVE WS-TOTAL-VAT-AMT          TO CBTRANS-AMOUNT.
           MOVE " VAT On Bank Chrges"     TO WS-NARRATIVE
           PERFORM SET-BATCH-LINE-DETAILS.
           MOVE FAX-RATE                  TO CBTRANS-LINE-DESC.
       WFBNK-018.
           WRITE CBTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
                MOVE "CBTRANS BUSY ON WRITE-FINAL-BANK, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO WFBNK-018.
       WFBNK-999.
            EXIT.
      *
       WRITE-BANK-DR-INTEREST SECTION.
       WBDI-001.
           PERFORM READ-GLPARAMETER-LOCK.

           MOVE GLPA-CBTRANSNO            TO CBTRANS-TRANS
           ADD 1                          TO GLPA-CBTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 7                         TO CBTRANS-TYPE
           MOVE WS-BATCH                  TO CBTRANS-REFERENCE
           MOVE WS-PERIOD                 TO CBTRANS-PERIOD
           MOVE Ws-CbMast                 TO CBTRANS-CBMASTER
           PERFORM CONVERT-CAMS-DATE-FORMAT.
           MOVE SPLIT-DATE                TO CBTRANS-DATE
           MOVE "S"                       TO CBTRANS-TYPE-OF-POST
           MOVE "Y"                       TO CBTRANS-ALLOCATED
           MOVE "50-060-05-00"            TO CBTRANS-ACCOUNT-NUMBER
           MOVE WS-BANK-NETT-AMT          TO CBTRANS-AMOUNT.
           IF WS-NAR-1-21 = "PENALTY INTEREST     "
               MOVE " Penalty interest     " TO WS-NARRATIVE.
           IF WS-NAR-1-21 = "INTEREST ON DEBIT BAL"
               MOVE " Interest On Dr Bal.  " TO WS-NARRATIVE.
           IF WS-NAR-1-21 = "INT ON DEBIT BALANCE "
               MOVE " Interest On Dr Bal.  " TO WS-NARRATIVE.
           PERFORM SET-BATCH-LINE-DETAILS.
           MOVE FAX-RATE                  TO CBTRANS-LINE-DESC.
       WBDI-018.
           WRITE CBTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
                MOVE "CBTRANS BUSY ON WRITEBANK-DR-INT, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO WBDI-018.
       WBDI-999.
            EXIT.
      *
       WRITE-BANK-CR-INTEREST SECTION.
       WBCI-001.
           PERFORM READ-GLPARAMETER-LOCK.

           MOVE GLPA-CBTRANSNO            TO CBTRANS-TRANS
           ADD 1                          TO GLPA-CBTRANSNO.
           PERFORM REWRITE-GLPARAMETER.
           MOVE 25                        TO CBTRANS-TYPE
           MOVE WS-BATCH                  TO CBTRANS-REFERENCE
           MOVE WS-PERIOD                 TO CBTRANS-PERIOD
           MOVE Ws-CbMast                 TO CBTRANS-CBMASTER
           PERFORM CONVERT-CAMS-DATE-FORMAT.
           MOVE SPLIT-DATE                TO CBTRANS-DATE
           MOVE "S"                       TO CBTRANS-TYPE-OF-POST
           MOVE "Y"                       TO CBTRANS-ALLOCATED
           MOVE "60-015-05-00"            TO CBTRANS-ACCOUNT-NUMBER
           MOVE WS-BANK-NETT-AMT          TO CBTRANS-AMOUNT.
           IF WS-NAR-1-21 = "INTEREST ON CREDIT BA"
               MOVE " Interest On Cr Bal.  " TO WS-NARRATIVE.
           IF WS-NAR-1-21 = "INT ON CREDIT BALANCE"
               MOVE " Interest On Cr Bal.  " TO WS-NARRATIVE.
           PERFORM SET-BATCH-LINE-DETAILS.
           MOVE FAX-RATE                  TO CBTRANS-LINE-DESC.
       WBCI-018.
           WRITE CBTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
                MOVE "CBTRANS BUSY ON WRITEBANK-CR-INT, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO WBCI-018.
       WBCI-999.
            EXIT.
      *
       SET-BATCH-LINE-DETAILS SECTION.
       SBLD-005.
           MOVE " "       TO ALPHA-RATE FAX-RATE
           MOVE WS-BATCH  TO ALPHA-RATE
           MOVE 1         TO SUB-7 SUB-8.
       SBLD-010.
           IF AL-RATE (SUB-7) NOT = " "
              MOVE AL-RATE (SUB-7) TO FX-RATE (SUB-8)
           ELSE
              MOVE " "          TO ALPHA-RATE
              MOVE WS-NARRATIVE TO ALPHA-RATE
              MOVE 2            TO SUB-7
              ADD 1             TO SUB-8
              GO TO SBLD-500.

            IF SUB-7 < 10
              ADD 1 TO SUB-7 SUB-8
              GO TO SBLD-010.
       SBLD-500.
           IF AL-RATE (SUB-7) NOT = " "
              MOVE AL-RATE (SUB-7) TO FX-RATE (SUB-8).

           IF SUB-8 < 25
              ADD 1 TO SUB-7 SUB-8
              GO TO SBLD-500.
       SBLD-999.
           EXIT.
      *
       CONVERT-CAMS-DATE-FORMAT SECTION.
       CDFS-001.
           MOVE BANK-CAMS-STATEMENT-DATE  TO WS-CAMS-SPLIT-DATE
           MOVE WS-CS-DD   TO SPLIT-DD
           MOVE WS-CS-MM   TO SPLIT-MM
           MOVE WS-CS-YYYY TO SPLIT-YY.
       CDFS-999.
           EXIT.
      *
       READ-ALL-TRANSACTIONS SECTION.
       RALL-001.
           MOVE 2910 TO POS
           DISPLAY 
           "READING ALL CASHBOOK TRANSACTIONS FOR ACCOUNT...." AT POS.
           PERFORM START-TRANS-BY-DATE.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS-FILE BAD START, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RALL-999.
           MOVE 1 TO SUB-20.
       RALL-005.
           READ CBTRANS-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-CBTRANS-ST1  = 10
              SUBTRACT 1 FROM SUB-20
              GO TO RALL-900.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS-FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RALL-005.

            IF CBTRANS-FUTURE = "F"              
      *      IF CBTRANS-NO > WS-NO
              GO TO RALL-005.
            IF CBTRANS-CBMASTER NOT = Ws-CbMast
              GO TO RALL-005.
            IF WS-ONLY-UNALLOC = "Y"
             IF CBTRANS-ALLOCATED = "Y"
              GO TO RALL-005.
       RALL-010.
            MOVE CBTRANS-TRANS     TO WS-TRANS-NUM (SUB-20)
            MOVE CBTRANS-TYPE      TO WS-TYPE-NUM (SUB-20)
            MOVE CBTRANS-AMOUNT    TO WS-CB-AMOUNT (SUB-20)
            MOVE CBTRANS-LINE-DESC TO WS-CB-LINE-DESC (SUB-20).
            
      *      MOVE WS-LINES (SUB-20) TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       RALL-020.
            IF SUB-20 < 1000
               ADD 1 TO SUB-20
               GO TO RALL-005.
       RALL-900.
           CLOSE CBTRANS-FILE.
           PERFORM OPEN-000.
       RALL-999.
           EXIT.
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE 0 TO WS-CBTRANS-ST1.
           START CBTRANS-FILE KEY NOT < CBTRANS-KEY
                 INVALID KEY NEXT SENTENCE.
       RO-010.
           READ CBTRANS-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-CBTRANS-ST1
                GO TO RO-999.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON READ, PRESS 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RO-010.
       RO-999.
           EXIT.
     *
       READ-CB SECTION.
       RD-000.
           MOVE Ws-CbMast TO CB-NUMBER.
           START CB-MASTER KEY NOT < CB-KEY.
       RD-010.
           READ CB-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
                MOVE " " TO CB-DESCRIPTION
                GO TO RD-999.
           IF WS-CB-ST1 NOT = 0
              MOVE "CASH BOOK BUSY ON READ, PRESS 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CB-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO RD-010.
       RD-999.
           EXIT.
      *
       START-TRANS SECTION.
       ST-OO-000.
           START CBTRANS-FILE KEY NOT < CBTRANS-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "THERE ARE NO TRANSACTIONS FOR THIS PERIOD."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
       ST-OO-999.
             EXIT.
      *
       START-TRANS-BY-DATE SECTION.
       ST-DATE-000.
           START CBTRANS-FILE KEY NOT < CBTRANS-ALT-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "THERE ARE NO TRANSACTIONS BY DATE FOR THIS PERIOD."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020.
       ST-DATE-999.
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
              MOVE "GLPARAMETER BUSY ON READ RP-010, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RP-010.
       RP-999.
           EXIT.
      *
       READ-GLPARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 1 TO GLPA-RECORD.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       RPL-010.
           READ GLPARAMETER-FILE WITH LOCK
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RPL-010.
       RPL-999.
           EXIT.
      *
       REWRITE-GLPARAMETER SECTION.
       REWP-000.
           REWRITE GLPARAMETER-REC
               INVALID KEY NEXT SENTENCE.
            IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RP-000.
       REWP-999.
           EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            MOVE 1 TO SUB-1 F-INDEX.
       SCROLL-010.
            MOVE "DATE"                   TO F-FIELDNAME
            MOVE 4                        TO F-CBFIELDNAME
            MOVE BANK-CAMS-STATEMENT-DATE TO F-NAMEFIELD
            MOVE 10                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "NARRATIVE"           TO F-FIELDNAME
            MOVE 9                     TO F-CBFIELDNAME
            MOVE BANK-CAMS-NARRATIVE   TO F-NAMEFIELD60
            MOVE 47                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA60.

            MOVE "ALLOCATED"           TO F-FIELDNAME
            MOVE 9                     TO F-CBFIELDNAME
            MOVE BANK-CAMS-FOUND       TO F-NAMEFIELD
            MOVE 2                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT"              TO F-FIELDNAME
            MOVE 6                     TO F-CBFIELDNAME
            MOVE WS-CAMS-AMOUNT        TO F-EDNAMEFIELDREC
            MOVE 12                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.
       SCROLL-020.
            IF SUB-10 = 15
              GO TO SCROLL-999.
            ADD 1 TO SUB-10 F-INDEX.
            GO TO SCROLL-010.
       SCROLL-999.
             EXIT.
      *
       CLEAR-BODY SECTION.
       CLEAR-000.
            MOVE 0 TO F-INDEX.
       CLEAR-002.
            ADD 1 TO F-INDEX.
            IF F-INDEX > 15
               GO TO CLEAR-BODY-999.

            MOVE "DATE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 10     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "NARRATIVE" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD60
            MOVE 47          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA60.

            MOVE "ALLOCATED" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 2           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 12       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            GO TO CLEAR-002.
       CLEAR-BODY-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
           MOVE 1 TO SUB-1.
       CF-010.
           ADD 1 TO SUB-1.
           MOVE " " TO WS-CB-LINE-DESC (SUB-1)
                       WS-CAMS-FOUND (SUB-1)
           MOVE 0   TO WS-TRANS-NUM (SUB-1)
                       WS-TYPE-NUM (SUB-1)
                       WS-CB-AMOUNT (SUB-1).
           IF SUB-1 < 15
                GO TO CF-010.
           MOVE 1 TO SUB-1.
       CF-999.
             EXIT.
      *
       CLEAR-TRANS-IN-MEM SECTION.
       CTIM-000.
           MOVE 1 TO SUB-1.
       CTIM-010.
           ADD 1 TO SUB-1.
           MOVE " " TO WS-CB-LINE-DESC (SUB-1)
                       WS-CAMS-FOUND (SUB-1).
           MOVE 0   TO WS-TRANS-NUM (SUB-1)
                       WS-TYPE-NUM (SUB-1)
                       WS-CB-AMOUNT (SUB-1).
           IF SUB-1 < 1000
                GO TO CTIM-010.
           MOVE 1 TO SUB-1.
       CTIM-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CBTRANS-FILE
            IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CB-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO OPEN-000.
        OPEN-008.
            OPEN I-O CB-MASTER.
            IF WS-CB-ST1 NOT = 0
               MOVE "CB-MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CB-ST1 TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CB-ST1
               GO TO OPEN-008.
        OPEN-009.
            OPEN I-O GL-MASTER.
            IF WS-GLMAST-ST1 NOT = 0
              MOVE "GL-MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO OPEN-009.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-012.
       OPEN-015.
            OPEN I-O BANK-CAMS-FILE.
            IF WS-CAMS-ST1 NOT = 0
              MOVE "BANK CAMS FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CAMS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CAMS-ST1
              GO TO OPEN-015.
       OPEN-016.
           PERFORM READ-GLPARAMETER.
           MOVE GLPA-CURRENT-CBPER TO WS-NO.
           PERFORM ENTER-PERIOD-DATES.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CbCamsMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CBTRANS-FILE
                 CB-MASTER
                 GLPARAMETER-FILE
                 GL-MASTER.
       END-500.
           CLOSE BANK-CAMS-FILE.
       END-600.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAlpha60".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldDate".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldRec".
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "NumberCheck".
       Copy "DisplayCBNoTopInfo".
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
      * END-OF-JOB
