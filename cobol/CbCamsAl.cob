        IDENTIFICATION DIVISION.
        PROGRAM-ID. CbCamsAl.
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
       77  WS-END             PIC X VALUE " ".
       77  WS-ABOVE-BODY      PIC X VALUE " ".
       77  WS-CAMS-DATA-START PIC X VALUE " ".
       77  WS-CAMS-AMOUNT     PIC S9(8)V99 VALUE 0.
       01  WS-PERIOD.
           03  WS-FUTURE      PIC X.
           03  WS-NO          PIC 99.
       01  WS-BATCH.
           03  WS-BATCH-1STCHAR  PIC X(2) VALUE "CB".
           03  WS-BATCH-REST     PIC X(8).
       01  WS-CBTRANS-STATUS.
           03  WS-CBTRANS-ST1     PIC 99.
       01  WS-CB-STATUS.
           03  WS-CB-ST1          PIC 99.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1      PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  WS-CAMS-STATUS.
           03  WS-CAMS-ST1        PIC 99.
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
       01  WS-NARRATIVE.
           03  WS-NAR-1-21.
               05  WS-NAR-1ST      PIC X.
               05  WS-NAR-20.
                   07  WS-NAR2-3   PIC X(2).
                   07  WS-NAR4-21  PIC X(18).
       01  BODY-FIELDS.
           03  BODY-LINE OCCURS 500.
               05  WS-TRANS          PIC 9(6).
               05  WS-TYPE           PIC 99.
               05  WS-TRANS-DATE     PIC 9(8).
               05  WS-TYPE-OF-POST   PIC X.
               05  WS-ALLOCATED      PIC X.
               05  WS-ACCOUNT-NUMBER PIC X(12).
               05  WS-TRANS-AMT      PIC S9(7)V99.
               05  WS-GLDESC         PIC X(40).
               05  WS-LINE-DESC      PIC X(25).
               05  WS-NEWLINE        PIC X.
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
           PERFORM CLEAR-FIELDS.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM DISPLAY-TOP-INFO.
           PERFORM GET-DATA.
           PERFORM END-OFF.
           GO TO CONTROL-000.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO CBTRANS-REC.
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
               MOVE "ENTER AN EXISTING ACCOUNT NUMBER" TO WS-MESSAGE
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
       GET-500.
            MOVE 1 TO SUB-20 SUB-21 F-INDEX.
            PERFORM READ-FOR-BEGIN-OF-CAMS-FILE.
                
            PERFORM FILL-DATA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
           PERFORM READ-CAMS-FILE.
           IF WS-CAMS-DATA-START = "N"
               GO TO FILL-001.
           IF WS-CAMS-DATA-START = "F"
               GO TO FILL-600.
       FILL-005.
      **********************************************
      *SUB-21 = LINE COUNT ON THE SCREEN           *
      *SUB-20 = TOTAL NUMBER OF LINES              *
      **********************************************
           IF SUB-20 < 1
              MOVE 1 TO SUB-20 SUB-21 F-INDEX.
           MOVE "LINENO"     TO F-FIELDNAME
           MOVE 6            TO F-CBFIELDNAME
           MOVE SUB-20       TO F-EDNAMEFIELDGROUP
           MOVE 3            TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-GROUP.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
                
           PERFORM SCROLL-CAMS.
       FILL-010.
           MOVE "                   " TO F-NAMEFIELD.
           MOVE "TYPE"                TO F-FIELDNAME
           MOVE 4                     TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
              GO TO FILL-010.
      **********************************************************
      * <Esc> KEY IF YOU WANT TO IGNORE AN ITEM CALLED UP.  *
      **********************************************************
           IF F-EXIT-CH = X"07"
              GO TO FILL-001.
      ************************************
      * <TAB> KEY TO FINISH THE SESSION  * 
      ************************************
           IF F-EXIT-CH = X"09"
              GO TO FILL-600.

           MOVE 2           TO F-CBFIELDLENGTH
           PERFORM READ-FIELD-ALPHA. 
           IF F-NAMEFIELD = " "
              GO TO FILL-010.
           IF F-EXIT-CH = X"0B" AND F-INDEX < 15
              ADD 1 TO F-INDEX SUB-20 SUB-21
              GO TO FILL-005.
       FILL-0110.
           IF F-EXIT-CH = X"0C"
            IF SUB-3 < 27
              ADD 1 TO SUB-3
              IF WS-TRANS-DESC (SUB-3) = " "
                 GO TO FILL-0110
              ELSE
              MOVE SUB-3 TO WS-TYPE (SUB-20) F-EDNAMEFIELDANAL
              MOVE 2     TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-ANALYSIS
              MOVE "TYPEDESC"            TO F-FIELDNAME
              MOVE 8                     TO F-CBFIELDNAME
              MOVE WS-TRANS-DESC (SUB-3) TO F-NAMEFIELD
              MOVE 7                     TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-ALPHA
              GO TO FILL-010.
           MOVE F-NAMEFIELD TO ALPHA-RATE
           PERFORM DECIMALISE-RATE.
           IF NUMERIC-RATE > 27
              MOVE "ENTRY CANNOT BE > 27, PLEASE RE-ENTER."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO FILL-010.
           IF NUMERIC-RATE < 0
              MOVE "ENTRY CANNOT BE < 0, PLEASE RE-ENTER."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO FILL-010.
           MOVE NUMERIC-RATE TO WS-TYPE (SUB-20) SUB-3
                                 F-EDNAMEFIELDANAL
           MOVE 2            TO  F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ANALYSIS.
           IF WS-TYPE (SUB-20) NOT > 0
              MOVE "ENTRY IS INCORRECT, PLEASE RE-ENTER." TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO FILL-010.

            MOVE "TYPEDESC"            TO F-FIELDNAME
            MOVE 8                     TO F-CBFIELDNAME
            MOVE WS-TRANS-DESC (SUB-3) TO F-NAMEFIELD
            MOVE 7                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            IF F-NAMEFIELD = " "
                MOVE "THE CODE ENTERED IS NOT VALID, RE-ENTER"
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO FILL-010.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
            IF SUB-3 < 20
             IF WS-TRANS-AMT (SUB-20) NOT < 0
               MOVE "FOR TRANS TYPE < 20, AMOUNT MUST BE < 0, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-010.
            IF SUB-3 > 19
             IF WS-TRANS-AMT (SUB-20) NOT > 0
               MOVE "FOR TRANS TYPE > 19, AMOUNT MUST BE > 0, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-010.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-011.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TYPE-OF-POST" TO F-FIELDNAME
            MOVE 12             TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            MOVE 1              TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD    TO WS-TYPE-OF-POST (SUB-20)
            PERFORM WRITE-FIELD-ALPHA.

            IF F-EXIT-CH NOT = X"1D"
             IF WS-TYPE-OF-POST (SUB-20) NOT = "S"
               MOVE "THE FIELD MUST BE 'S', RE-ENTER." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-011.
            IF F-EXIT-CH = X"1D"
             IF WS-TYPE-OF-POST (SUB-20) NOT = "S" AND NOT = "G"
                                     AND NOT = "D" AND NOT = "C"
               MOVE "THE FIELD MUST BE 'C' 'D' 'G' OR 'S', RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-011.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
       FILL-013.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ALLOCATED" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO FILL-011.
            MOVE 1           TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO WS-ALLOCATED (SUB-20)
            PERFORM WRITE-FIELD-ALPHA.

            IF WS-ALLOCATED (SUB-20) NOT = "N" AND NOT = "Y"
                                 AND NOT = "H"
               MOVE "THE FIELD MUST BE EITHER 'H' 'N' OR 'Y', RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-013.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO FILL-013.
            MOVE 10     TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-015.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE
            MOVE DISPLAY-DATE    TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA
            PERFORM CONVERT-SPLIT-FORMAT
            MOVE SPLIT-DATE TO WS-TRANS-DATE (SUB-20)
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-015.
            MOVE GL-BEGDATE (WS-NO) TO WS-BEG-DATE.
            MOVE GL-ENDDATE (WS-NO) TO WS-END-DATE.
            IF WS-TRANS-DATE (SUB-20) > WS-END-DATE
            MOVE "THE JOURNAL DATE MUST BE < OR = THE END PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-015.
            IF WS-TRANS-DATE (SUB-20) < WS-BEG-DATE
            MOVE "THE JOURNAL DATE MUST > OR = THE BEG. PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-015.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "GLACCNO"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
      ***********************
      *X"8A" = <CODE-RETURN>*
      ***********************
            IF F-EXIT-CH = X"8A"
             IF SUB-20 > 1
             COMPUTE SUB-4 = SUB-20 - 1
             MOVE WS-ACCOUNT-NUMBER (SUB-4) TO 
                            WS-ACCOUNT-NUMBER (SUB-20) F-NAMEFIELD
              MOVE 12 TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-ALPHA.
      **************************
      *X"8C" = <CODE-NEXT-PAGE>*
      **************************
           IF F-EXIT-CH = X"8C" OR = X"85"
            IF SUB-1 = 1
              PERFORM FILL-021
              PERFORM START-GL-NEXT-ONE.
            IF F-EXIT-CH = X"8C"
      *       IF SUB-20 > 1
              PERFORM READ-NEXT-GLNUMBER
               IF WS-GLMAST-ST1 = 0
                MOVE GL-NUMBER TO WS-ACCOUNT-NUMBER (SUB-20)
                                   F-NAMEFIELD
                MOVE 12         TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                GO TO FILL-023
               ELSE
                MOVE "READ-NEXT FAILED, 'ESC' TO RE-ENTER GL-NUMBER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-020.
      ************************************************
      *<CODE-PREV-PAGE> TO READ PREVIOUS STOCK ITEM. *
      ************************************************
            IF F-EXIT-CH = X"85"
              PERFORM READ-PREV-GLNUMBER
              IF WS-GLMAST-ST1 = 0
                MOVE GL-NUMBER TO WS-ACCOUNT-NUMBER (SUB-1)
                                   F-NAMEFIELD
                MOVE 12        TO  F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                GO TO FILL-023
              ELSE
                MOVE "READ-PREV FAILED, 'ESC' TO RE-ENTER GL-NUMBER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-020.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
       FILL-021.
            MOVE 12          TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF ALPHA-RATE > SPACES
               PERFORM NUMBER-CHECK
               MOVE WS-GLNO-CHECK TO WS-ACCOUNT-NUMBER (SUB-20)
            IF SIGN-FOUND = 9
               MOVE 0 TO SIGN-FOUND
               GO TO FILL-020.
            MOVE WS-ACCOUNT-NUMBER (SUB-20) TO F-NAMEFIELD
            PERFORM WRITE-FIELD-ALPHA.
       FILL-022.
            PERFORM READ-GLNUMBER.
            IF GL-DESCRIPTION = " "
               MOVE "THIS MUST BE AN EXISTING GL-ACCOUNT NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-020.
       FILL-023.
            MOVE GL-DESCRIPTION    TO WS-GLDESC (SUB-20)
            MOVE "GLACCNAME"       TO F-FIELDNAME
            MOVE 9                 TO F-CBFIELDNAME
            MOVE WS-GLDESC (SUB-20) TO F-NAMEFIELD
            MOVE 40                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-025.
            IF F-EXIT-CH NOT = X"1D"
                GO TO FILL-026.
       
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "AMOUNT"     TO F-FIELDNAME
            MOVE 6            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            MOVE 12           TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 WS-TRANS-AMT (SUB-20)
            PERFORM WRITE-FIELD-REC.
       FILL-026.
            IF SUB-3 < 20
             IF WS-TRANS-AMT (SUB-20) NOT < 0
               MOVE "FOR TRANS TYPE < 20, AMOUNT MUST BE < 0, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-010.
            IF SUB-3 > 19
             IF WS-TRANS-AMT (SUB-20) NOT > 0
               MOVE "FOR TRANS TYPE > 19, AMOUNT MUST BE > 0, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-010.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DESC"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF F-EXIT-CH = X"8A"
               MOVE WS-BATCH TO WS-LINE-DESC (SUB-20) F-NAMEFIELD
               MOVE 25       TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               GO TO FILL-030.
            IF F-EXIT-CH = X"8B"
             IF SUB-20 > 1
               COMPUTE SUB-4 = SUB-20 - 1
               MOVE WS-LINE-DESC (SUB-4) TO WS-LINE-DESC (SUB-20)
                                          F-NAMEFIELD
               MOVE 25 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               GO TO FILL-030.
            MOVE 25          TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO WS-LINE-DESC (SUB-20).
            IF WS-LINE-DESC (SUB-20) = " "
               MOVE WS-BATCH TO WS-LINE-DESC (SUB-20) F-NAMEFIELD
               MOVE 25       TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       FILL-050.
            PERFORM RCF-035 THRU RCF-036.
            
            IF SUB-21 > 14
               PERFORM FILL-060
               MOVE 0 TO SUB-21 F-INDEX.
               
            ADD 1 TO SUB-20 SUB-21 F-INDEX.
      *      IF SUB-20 > SUB-21
      *         MOVE SUB-20 TO SUB-21.
            GO TO FILL-001.
       FILL-060.
            PERFORM CLEAR-BODY.
            PERFORM ERROR1-020.
       FILL-600.
            MOVE 2910 TO POS
            DISPLAY "WRITING TRANSACTIONS........." AT POS.
            PERFORM WRITE-CBTRANS.
            PERFORM CLEAR-FIELDS.
            PERFORM CLEAR-BODY.
            PERFORM ERROR1-020.
            GO TO FILL-999.
       FILL-999.
            EXIT.
      *
       READ-FOR-BEGIN-OF-CAMS-FILE SECTION.
       RFBOCF-000.
           MOVE "N" TO WS-CAMS-DATA-START.
       RFBOCF-001.
           READ BANK-CAMS-FILE
               AT END
               GO TO RFBOCF-999.
       RFBOCF-005.
      ******************************************************************
      * READING TO SEE WHERE THE ACTUAL DATA STARTS, WHEN THIS IS TRUE *
      * NEXT LINE IS THE ACTUAL START OF INFO.                         *
      ******************************************************************
           IF WS-CAMS-DATA-START = "N"
            IF BANK-CAMS-STATEMENT-DATE NOT = "  DATE    "
              GO TO RFBOCF-001
            ELSE
              MOVE "Y" TO WS-CAMS-DATA-START.
       RFBOCF-999.
            EXIT.
      *
       READ-CAMS-FILE SECTION.
       RCF-001.
           READ BANK-CAMS-FILE
               AT END
               GO TO RCF-900.
      ******************************************************************
      * READING TO SEE WHERE THE ACTUAL DATA STOPS, SO WE CAN GRAB THE *
      * BLANK LINE TO PRINT THE VAT IN BEFORE "Closing Bal" ENTRY.     *
      ******************************************************************
           IF WS-CAMS-DATA-START = "Y"
            IF BANK-CAMS-STATEMENT-DATE = " "
             IF BANK-CAMS-STATEMENT-NUM = " "
              IF BANK-CAMS-NARRATIVE = " "
               IF BANK-CAMS-AMOUNT NOT = "     CLOSING B"
                GO TO RCF-001.
      ******************************************************************
      * READING TO SEE WHERE THE ACTUAL DATA STOPS, WHEN THIS IS TRUE  *
      * THE CAMS FILE IS AT AN END.                                    *
      ******************************************************************
           IF BANK-CAMS-AMOUNT = "     CLOSING B"
              MOVE " " TO BANK-CAMS-VAT-AMT
                          BANK-CAMS-FILL7
              GO TO RCF-900.
       RCF-010.
      ******************************************************************
      * CHECKING IN CASE A LINE HAS ALREADY BEEN CHECKED IN A PREVIOUS *
      * RUN SO WE DON'T RECHECK IT. COULD BE :                         *
      * ' *' = FOUND IN PREVIOUS RUN OF CbCamsMt                       *
      * '**' = MANUAL ENTRY DONE BY OPERATOR IN CB AND CbTranMt        *
      * '01' = NUMBERS FROM FEC'S ( 01, 02, 03 etc.)                   *
      * '$$' = ALLOCATIONS FROM PREVIOUS RUN OF THIS PROGRAM           *
      * 'Sy' = SALARY ALLOCATIONS DONE IN CbCamsMt                     *
      ******************************************************************
           MOVE BANK-CAMS-NARRATIVE TO WS-NARRATIVE.
           
           IF BANK-CAMS-FOUND NOT = "  "
            IF BANK-CAMS-FOUND = "$$" OR = " *" OR = "**" OR = "Sy"
               GO TO RCF-001.
      *************************************************************
      * THIS SECTION IS USED TO FIND THE NETT FEC CHARGE AND THEN *
      * TO CHECK IF THIS AMOUNT WAS FOUND IN THE CB.  IF NOT THEN *
      * DISPLAY ON SCREEN SO WE CAN ALLOCATE IT TO AN ACCOUNT     *
      * THE #fx ENTRIES ARE THEREBY OMMITTED.                     *
      *************************************************************
           IF BANK-CAMS-FOUND NOT = "  "
            IF WS-NAR-1ST = "#"
             IF WS-NAR2-3 = "fx"
               GO TO RCF-001.
           IF BANK-CAMS-FOUND NOT = "  "
            IF WS-NAR-1ST = "F"
             IF WS-NAR2-3 = "x "
              IF BANK-CAMS-FEC-FOUND NOT = " "
               GO TO RCF-001.
   
           PERFORM CHECK-FOR-COMMAS
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO WS-CAMS-AMOUNT.

           GO TO RCF-999.
       RCF-035.
           IF WS-NAR-1ST = "F"
            IF WS-NAR2-3 = "x "
                MOVE "$" TO BANK-CAMS-FILL8
                MOVE "$" TO BANK-CAMS-FEC-FOUND
                GO TO RCF-036
            ELSE
                MOVE "$$" TO BANK-CAMS-FOUND
                GO TO RCF-036.
           MOVE "$$" TO BANK-CAMS-FOUND.
       RCF-036.
           REWRITE BANK-CAMS-REC.
           IF WS-CAMS-ST1 NOT = 0
              MOVE "CAMS-FILE ERROR ON REWRITE, 'ESC' FOR STATUS."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-CAMS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RCF-035.
       RCF-900.
           MOVE "F" TO WS-CAMS-DATA-START.
           MOVE "CAMS-FILE FINISHED, 'ESC' TO REWRITE THE CB-TRANS."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
       RCF-999.
           EXIT.
      *
       CHECK-FOR-COMMAS SECTION.
       CFC-005.
           MOVE 2910 TO POS
           DISPLAY "CHECKING FOR COMMAS ....             " AT POS.
           MOVE " "              TO ALPHA-RATE FAX-RATE.
           IF WS-NAR-1ST = "F"
            IF WS-NAR2-3 = "x "
                MOVE BANK-CAMS-VAT-AMT TO ALPHA-RATE
                GO TO CFC-008
            ELSE
                MOVE BANK-CAMS-AMOUNT  TO ALPHA-RATE
                GO TO CFC-008.

           MOVE BANK-CAMS-AMOUNT  TO ALPHA-RATE.
       CFC-008.
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
           PERFORM ERROR1-020.
       CFC-999.
           EXIT.
      *
       WRITE-CBTRANS SECTION.
       WCBT-00000.
           PERFORM ERROR1-020.
           MOVE 1                 TO SUB-20.
           MOVE WS-TRANS (SUB-20) TO CBTRANS-TRANS.
           MOVE WS-TYPE (SUB-20)  TO CBTRANS-TYPE.
           START CBTRANS-FILE KEY NOT < CBTRANS-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-ACCOUNT-NUMBER (SUB-20) = " "
              GO TO WCBT-999.
       WCBT-000.
            IF WS-NEWLINE (SUB-20) NOT = " "
             IF WS-TRANS (SUB-20) NOT = 0
                 MOVE WS-TRANS (SUB-20) TO CBTRANS-TRANS
                 GO TO WCBT-005.
             PERFORM READ-GLPARAMETER-LOCK.
             MOVE GLPA-CBTRANSNO TO CBTRANS-TRANS.
             ADD 1               TO GLPA-CBTRANSNO.
             PERFORM REWRITE-GLPARAMETER.
       WCBT-005.
            IF WS-NEWLINE (SUB-20) NOT = " "
                READ CBTRANS-FILE WITH LOCK
                   INVALID KEY NEXT SENTENCE.
            MOVE WS-TYPE (SUB-20)           TO CBTRANS-TYPE.
            MOVE WS-BATCH                   TO CBTRANS-REFERENCE.
            IF GLPA-CURRENT-CBPER = WS-NO
                MOVE " "                    TO CBTRANS-FUTURE
            ELSE
                MOVE "F"                    TO CBTRANS-FUTURE.
            MOVE WS-NO                      TO CBTRANS-NO.
            MOVE Ws-CbMast                  TO CBTRANS-CBMASTER.
            MOVE WS-TRANS-DATE (SUB-20)     TO CBTRANS-DATE.
            MOVE WS-TYPE-OF-POST (SUB-20)   TO CBTRANS-TYPE-OF-POST.
            MOVE WS-ALLOCATED (SUB-20)      TO CBTRANS-ALLOCATED.
            MOVE WS-ACCOUNT-NUMBER (SUB-20) TO CBTRANS-ACCOUNT-NUMBER.
            MOVE WS-TRANS-AMT (SUB-20)      TO CBTRANS-AMOUNT.
            MOVE WS-LINE-DESC (SUB-20)      TO CBTRANS-LINE-DESC.
       WCBT-018.
            IF WS-NEWLINE (SUB-20) = " "
               WRITE CBTRANS-REC
                  INVALID KEY NEXT SENTENCE
            ELSE
               REWRITE CBTRANS-REC
                  INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 = 0
                GO TO WCBT-020.
            GO TO WCBT-000.
       WCBT-020.
            IF CBTRANS-FUTURE = " "
               PERFORM UPDATE-CASHBOOK.
           MOVE 2910 TO POS
           DISPLAY "WRITING RECORD NUMBER:" AT POS
           ADD 23 TO POS
           MOVE SUB-20 TO F-EDNAMEFIELDCRED
           DISPLAY F-EDNAMEFIELDCRED AT POS.
            ADD 1 TO SUB-20.
            IF SUB-20 < 500
             IF WS-ACCOUNT-NUMBER (SUB-20) = " "
                GO TO WCBT-999.

            IF SUB-20 < 500
                GO TO WCBT-000.

           MOVE 1 TO SUB-20.
       WCBT-999.
            EXIT.
      *
       DELETE-TRANS SECTION.
       DO-010.
            DELETE CBTRANS-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
              GO TO DO-010.
       DO-999.
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
              MOVE "CBTRANS BUSY ON READ, PRESS 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
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
                PERFORM ERROR1-MESSAGE
                PERFORM ERROR1-020
                PERFORM ERROR-MESSAGE
                MOVE 0 TO WS-CB-ST1
                GO TO RD-010.
       RD-999.
           EXIT.
     *
       UPDATE-CASHBOOK SECTION.
       UCB-000.
           MOVE Ws-CbMast TO CB-NUMBER.
           START CB-MASTER KEY NOT < CB-KEY.
       UCB-010.
           READ CB-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 = 23 OR 35 OR 49
                MOVE " " TO CB-DESCRIPTION
                GO TO UCB-999.
           IF WS-CB-ST1 NOT = 0
                MOVE "CASH BOOK BUSY ON READ, PRESS 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CB-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM ERROR1-020
                PERFORM ERROR-MESSAGE
                MOVE 0 TO WS-CB-ST1
                GO TO UCB-010.
       UCB-020.
           ADD WS-TRANS-AMT (SUB-20) TO CB-BALANCE
                                        CB-PER (WS-NO).
       UCB-030.
            REWRITE CB-RECORD
               INVALID KEY NEXT SENTENCE.
            IF WS-CB-ST1 NOT = 0
                MOVE "CASH BOOK BUSY ON REWRITE, PRESS 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-CB-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM ERROR1-020
                PERFORM ERROR-MESSAGE
                MOVE 0 TO WS-CB-ST1
                GO TO UCB-030.
       UCB-999.
           EXIT.
      *
       START-TRANS SECTION.
       ST-OO-000.
           MOVE WS-TRANS (SUB-20) TO CBTRANS-TRANS.
           MOVE WS-TYPE (SUB-20)  TO CBTRANS-TYPE.
           START CBTRANS-FILE KEY NOT < CBTRANS-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
       ST-OO-999.
             EXIT.
      *
       READ-TRANS-NEXT SECTION.
       RONX-001.
           MOVE 0 TO WS-CBTRANS-ST1.
       RONX-005.
           IF WS-END = "Y"
              GO TO RONX-999.
           READ CBTRANS-FILE NEXT WITH LOCK
            AT END NEXT SENTENCE.
           IF WS-CBTRANS-ST1 = 10
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RONX-999.
           IF WS-CBTRANS-ST1 NOT = 0
              MOVE "CBTRANS BUSY ON READ-NEXT-LOCK, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CBTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CBTRANS-ST1
      *        PERFORM START-TRANS
              GO TO RONX-005.
       RONX-999.
           EXIT.
      *
       START-GL-NEXT SECTION.
       SGN-000.
           MOVE WS-ACCOUNT-NUMBER (SUB-20) TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE " " TO WS-ACCOUNT-NUMBER (SUB-20)
              MOVE "GLMASTER BUSY ON START, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO SGN-000.
       SGN-999.
           EXIT.
      *
       START-GL-NEXT-ONE SECTION.
       SGNO-000.
           MOVE WS-ACCOUNT-NUMBER (SUB-1) TO GL-NUMBER.
           START GL-MASTER KEY > GL-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE " " TO WS-ACCOUNT-NUMBER (SUB-1)
              MOVE "GLMASTER BUSY ON START-NEXT-ONE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO SGNO-000.
       SGNO-999.
           EXIT.
      *
       READ-GLNUMBER SECTION.
       RD-000.
           MOVE WS-ACCOUNT-NUMBER (SUB-20) TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       RD-010.
           READ GL-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
                MOVE " " TO GL-DESCRIPTION
                GO TO RD-999.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER BUSY ON READ, PRESS 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO RD-010.
       RD-999.
           EXIT.
     *
       READ-NEXT-GLNUMBER SECTION.
       RNGL-010.
           READ GL-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 10
               MOVE "END OF NEXT-PAGE SEQUENCE, ENTER A NEW NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
                GO TO RNGL-999.
           IF WS-GLMAST-ST1 = 91
                MOVE "GLMASTER BUSY 91 ON READ-NEXT, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-GLMAST-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM ERROR1-020
                PERFORM ERROR-MESSAGE
                MOVE GL-NUMBER TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE 0 TO WS-GLMAST-ST1
                PERFORM START-GL-NEXT
                GO TO RNGL-010.
           IF WS-GLMAST-ST1 NOT = 0
                MOVE "GLMASTER BUSY ON READ-NEXT, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-GLMAST-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM ERROR1-020
                PERFORM ERROR-MESSAGE
                MOVE GL-NUMBER TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
      *          PERFORM START-GL-NEXT
                MOVE 0 TO WS-GLMAST-ST1
                GO TO RNGL-010.
       RNGL-999.
           EXIT.
      *
       READ-PREV-GLNUMBER SECTION.
       RDPRV-010.
           READ GL-MASTER PREVIOUS
               AT END NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 10
               MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
                GO TO RDPRV-999.
           IF WS-GLMAST-ST1 = 91
                MOVE "GLMASTER BUSY 91 ON READ-PREV, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-GLMAST-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-GLMAST-ST1
                PERFORM START-GL-NEXT
                GO TO RDPRV-010.
           IF WS-GLMAST-ST1 NOT = 0
                MOVE "GLMASTER BUSY ON READ-PREV, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-GLMAST-ST1 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                MOVE GL-NUMBER TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-GLMAST-ST1
                GO TO RDPRV-010.
       RDPRV-999.
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
              MOVE "GLPARAMETER BUSY ON READ, 'ESC' TO RETRY."
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
       MAKE-BANK-DATE-CORRECT SECTION.
       MBDC-001.
            MOVE BANK-CAMS-STATEMENT-DATE TO ALPHA-RATE
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               MOVE "ERROR IN BANK DATE CONVERSION, 'ESC' TO EXIT."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
            MOVE WS-NEW-DATE     TO WS-CH-DATE CONVERT-DATE
            PERFORM CONVERT-SPLIT-FORMAT
            MOVE SPLIT-DATE      TO WS-TRANS-DATE (SUB-20).
       MBDC-999.
            EXIT.
      *
       SCROLL-CAMS SECTION.
       SCROLL-CAMS-010.
            MOVE "TYPE-OF-POST"          TO  F-FIELDNAME
            MOVE 12                      TO  F-CBFIELDNAME
            MOVE "S"                     TO WS-TYPE-OF-POST (SUB-20)
                                             F-NAMEFIELD
            MOVE 1                       TO  F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ALLOCATED"          TO F-FIELDNAME
            MOVE 9                    TO F-CBFIELDNAME
            MOVE "Y"                  TO WS-ALLOCATED (SUB-20)
                                         F-NAMEFIELD
            MOVE 1                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"                   TO F-FIELDNAME
            MOVE 4                        TO F-CBFIELDNAME.
            PERFORM MAKE-BANK-DATE-CORRECT.
            MOVE BANK-CAMS-STATEMENT-DATE TO F-NAMEFIELD
            MOVE 10                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT"              TO F-FIELDNAME
            MOVE 6                     TO F-CBFIELDNAME
            MOVE WS-CAMS-AMOUNT        TO WS-TRANS-AMT (SUB-20)
                                          F-EDNAMEFIELDREC
            MOVE 12                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.

            MOVE "DESC"                TO F-FIELDNAME
            MOVE 4                     TO F-CBFIELDNAME
            PERFORM CONVERT-BANK-NARRATIVE.
            
            MOVE DATA-RATE             TO WS-LINE-DESC (SUB-20)
                                           F-NAMEFIELD
            MOVE 25                    TO  F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-CAMS-999.
             EXIT.
      *
       CONVERT-BANK-NARRATIVE SECTION.
       CBN-010.
           MOVE 1 TO SUB-5 SUB-6
           MOVE SPACES TO ALPHA-RATE DATA-RATE
           MOVE BANK-CAMS-NARRATIVE TO ALPHA-RATE.
       CBN-015.
           IF SUB-5 > 48
              GO TO CBN-999.
           IF AL-RATE (SUB-5) = " "
             ADD 1 TO SUB-5
            IF AL-RATE (SUB-5) NOT = " "
              SUBTRACT 1 FROM SUB-5
               MOVE AL-RATE (SUB-5) TO DAT-RATE (SUB-6)
            ELSE
               ADD 1 TO SUB-5
               GO TO CBN-015.
           MOVE AL-RATE (SUB-5) TO DAT-RATE (SUB-6).
 
           ADD 1 TO SUB-5 SUB-6.
           GO TO CBN-015.
       CBN-999.
           EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            MOVE 1 TO SUB-1 F-INDEX.
       SCROLL-010.
            IF WS-TYPE (SUB-1) = 0
                 GO TO SCROLL-999.
            MOVE "TYPE"          TO F-FIELDNAME
            MOVE 4               TO F-CBFIELDNAME
            MOVE WS-TYPE (SUB-1) TO F-EDNAMEFIELDANAL
            MOVE 2               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE WS-TYPE (SUB-1)       TO SUB-3.
            MOVE "TYPEDESC"            TO F-FIELDNAME
            MOVE 8                     TO F-CBFIELDNAME
            MOVE WS-TRANS-DESC (SUB-3) TO F-NAMEFIELD
            MOVE 7                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPE-OF-POST"          TO F-FIELDNAME
            MOVE 12                      TO F-CBFIELDNAME
            MOVE WS-TYPE-OF-POST (SUB-1) TO F-NAMEFIELD
            MOVE 1                       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ALLOCATED"          TO F-FIELDNAME
            MOVE 9                    TO F-CBFIELDNAME
            MOVE WS-ALLOCATED (SUB-1) TO F-NAMEFIELD
            MOVE 1                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLACCNO"                  TO F-FIELDNAME
            MOVE 7                          TO F-CBFIELDNAME
            MOVE WS-ACCOUNT-NUMBER  (SUB-1) TO F-NAMEFIELD
            MOVE 12                         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"                TO F-FIELDNAME
            MOVE 4                     TO F-CBFIELDNAME
            MOVE WS-TRANS-DATE (SUB-1) TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE DISPLAY-DATE          TO F-NAMEFIELD
            MOVE 10                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT"             TO F-FIELDNAME
            MOVE 6                    TO F-CBFIELDNAME
            MOVE WS-TRANS-AMT (SUB-1) TO F-EDNAMEFIELDREC
            MOVE 12                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.

            MOVE "DESC"               TO F-FIELDNAME
            MOVE 4                    TO F-CBFIELDNAME
            MOVE WS-LINE-DESC (SUB-1) TO F-NAMEFIELD
            MOVE 25                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-020.
            IF SUB-1 = 15
              GO TO SCROLL-999.
            ADD 1 TO SUB-1 F-INDEX.
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
               GO TO CLEAR-BODY-900.

            MOVE "TYPE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 2      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPEDESC" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 7          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPE-OF-POST" TO F-FIELDNAME
            MOVE 12             TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 1              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ALLOCATED" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 1           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 10     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLACCNO" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 12        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESC" TO F-FIELDNAME
            MOVE 4      TO F-CBFIELDNAME
            MOVE " "    TO F-NAMEFIELD
            MOVE 25     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 12       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            GO TO CLEAR-002.
       CLEAR-BODY-900.
            MOVE "GLACCNAME" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       CLEAR-BODY-999.
             EXIT.
      *
       CANCEL-TRANSACTION SECTION.
       CAN-005.
           COMPUTE SUB-2 = SUB-1 + 1.
       CAN-008.
           PERFORM READ-TRANS.
           PERFORM DELETE-TRANS.
       CAN-010.
            IF SUB-2 > 15
               MOVE 15 TO SUB-1 SUB-2
               GO TO CAN-090.
             IF WS-ACCOUNT-NUMBER (SUB-2) = " "
                 MOVE " " TO WS-TYPE-OF-POST (SUB-1)
                             WS-ALLOCATED (SUB-1)
                             WS-ACCOUNT-NUMBER (SUB-1)
                             WS-GLDESC (SUB-1)
                             WS-LINE-DESC (SUB-1)
                             WS-NEWLINE (SUB-1)
                 MOVE 0   TO WS-TRANS (SUB-1)
                             WS-TYPE (SUB-1)
                             WS-TRANS-DATE (SUB-1)
                             WS-TRANS-AMT (SUB-1)
                 GO TO CAN-090.
             MOVE BODY-LINE (SUB-2) TO BODY-LINE (SUB-1).
             ADD 1 TO SUB-1 SUB-2.
             GO TO CAN-010.
       CAN-090.
                 MOVE " " TO WS-TYPE-OF-POST (SUB-1)
                             WS-ALLOCATED (SUB-1)
                             WS-ACCOUNT-NUMBER (SUB-1)
                             WS-GLDESC (SUB-1)
                             WS-LINE-DESC (SUB-1)
                             WS-NEWLINE (SUB-1)
                 MOVE 0   TO WS-TRANS (SUB-1)
                             WS-TYPE (SUB-1)
                             WS-TRANS-DATE (SUB-1)
                             WS-TRANS-AMT (SUB-1).
       CAN-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
           MOVE 0 TO SUB-1.
       CF-010.
           ADD 1 TO SUB-1.
           MOVE " " TO WS-TYPE-OF-POST (SUB-1)
                       WS-ALLOCATED (SUB-1)
                       WS-ACCOUNT-NUMBER (SUB-1)
                       WS-GLDESC (SUB-1)
                       WS-LINE-DESC (SUB-1)
                       WS-NEWLINE (SUB-1)
           MOVE 0   TO WS-TRANS (SUB-1)
                       WS-TYPE (SUB-1)
                       WS-TRANS-DATE (SUB-1)
                       WS-TRANS-AMT (SUB-1).
           IF SUB-1 < 500
               GO TO CF-010.
       CF-999.
             EXIT.
      *
       DISPLAY-TOP-INFO SECTION.
       DTI-005.
           MOVE "PERIOD" TO F-FIELDNAME
           MOVE 6        TO F-CBFIELDNAME
           MOVE WS-NO    TO F-NAMEFIELD
           MOVE 2        TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE WS-NO     TO SUB-1
           MOVE "BEGDATE" TO F-FIELDNAME
           MOVE 7         TO F-CBFIELDNAME
           MOVE GL-BEGDATE (SUB-1) TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO F-NAMEFIELD
           MOVE 10           TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.

           MOVE WS-NO     TO SUB-1
           MOVE "ENDDATE" TO F-FIELDNAME
           MOVE 7         TO F-CBFIELDNAME
           MOVE GL-ENDDATE (SUB-1) TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO F-NAMEFIELD
           MOVE 10           TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
       DTI-999.
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
              PERFORM ERROR1-MESSAGE
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
              MOVE "CAMS FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CAMS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CAMS-ST1
              GO TO OPEN-015.

           PERFORM READ-GLPARAMETER.
           MOVE GLPA-CURRENT-CBPER TO WS-NO.
           PERFORM ENTER-PERIOD-DATES.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CbCamsAl"      TO F-FORMNAME
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
                 GL-MASTER
                 BANK-CAMS-FILE.
           EXIT PROGRAM.
       END-999.
           EXIT.
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldGroup".
       Copy "WriteFieldDate".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldRec".
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "NumberCheck".
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
      *
      * END-OF-JOB
