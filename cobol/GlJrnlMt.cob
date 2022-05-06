       IDENTIFICATION DIVISION.
       PROGRAM-ID. GlJrnlMt.
       AUTHOR.  CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. B20.
       OBJECT-COMPUTER. B20.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
        Copy "SelectGlMaster".
        Copy "SelectGlMasterLy".
        Copy "SelectGlParameter".
        Copy "SelectGlJrn".
        Copy "SelectGlTrans".
        Copy "SelectGlTransLy".
        Copy "SelectSlDaily".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdGlMast.
           COPY ChlfdGlMastLy.
           COPY ChlfdGlParam.
           COPY ChlfdGlTrans.
           COPY ChlfdGlTransLy.
           COPY ChlfdGlJrn.
           COPY ChlfdDaily.
      *
       WORKING-STORAGE SECTION.
       77  WS-NEWORDER          PIC X VALUE " ".
       77  WS-CURRENTPER        PIC 99 VALUE 0.
       77  WS-TYPE-OF-END       PIC X VALUE " ".
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-BODY-LINE         PIC ZZ9.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1       PIC 99.
       01  WS-GL-LY-STATUS.
           03  WS-GL-LY-ST1        PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
       01  WS-GLJRN-STATUS.
           03  WS-GLJRN-ST1        PIC 99.
       01  WS-GLTRANS-STATUS.
           03  WS-GLTRANS-ST1      PIC 99.
       01  WS-GLTRANS-LY-STATUS.
           03  WS-GLTRANS-LY-ST1   PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1        PIC 99.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-GLHEADER     PIC X(2).
               05  WS-GLSUBHEADER  PIC X(4).
           03  WS-RESTOFACCOUNT    PIC X(6).
       01  JOURNAL-DATA.
           03  WS-JRN               PIC X(10).
           03  WS-JRNDESC           PIC X(25).
           03  WS-JRNACTION         PIC X.
           03  WS-JRNDATE           PIC 9(8).
           03  WS-BALANCE           PIC S9(8)V99.
           03  WS-DEBIT-TOTAL       PIC 9(9)V99.
           03  WS-CREDIT-TOTAL      PIC 9(9)V99.
           03  WS-JRNAMOUNT         PIC 9(8)V99.
       01  WS-JRNPERIOD.
           03  WS-1STPER           PIC X.
           03  WS-REST             PIC 99.
       01  WS-JRN-LY-PERIOD        PIC X(3).
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
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
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONT-010.
           PERFORM CLEAR-FIELDS.
           PERFORM DISPLAY-FORM.
           PERFORM DISPLAY-FORM-GL-TOP-INFO.
           PERFORM GET-DATA.
           GO TO CONT-010.
      *
       GET-DATA SECTION.
       GET-010.
            MOVE SPACES TO F-NAMEFIELD.
            MOVE 0 TO SUB-20 SUB-25.
            MOVE "Y" TO WS-NEWORDER.
            MOVE " " TO WS-MESSAGE
                        WS-JRN
                        WS-TYPE-OF-END
                        WS-JRN-LY-PERIOD.
            MOVE WS-DATE TO WS-JRNDATE.

            MOVE "JRNNUMBER" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                PERFORM DISPLAY-FORM
                PERFORM DISPLAY-FORM-GL-TOP-INFO
                GO TO GET-010.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = " "
                GO TO GET-010.
            MOVE F-NAMEFIELD TO WS-JRN.
            PERFORM READ-JOURNAL.
            IF F-EXIT-CH = X"1F"
                PERFORM GET-015 THRU GET-140
                PERFORM DELETE-JOURNAL
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                PERFORM DISPLAY-FORM-GL-TOP-INFO
                GO TO GET-010.
            IF WS-GLJRN-ST1 = 88
                MOVE 0 TO WS-GLJRN-ST1
                GO TO GET-010.
            IF WS-NEWORDER = "Y"
                PERFORM CLEAR-FIELDS.
       GET-015.
            MOVE "JRNNUMBER" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE WS-JRN TO GLJRN-REFERENCE F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-016.
            MOVE "JRNDATE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            IF WS-NEWORDER = "Y"
                MOVE WS-DATE TO SPLIT-DATE
                PERFORM CONVERT-DATE-FORMAT
                MOVE DISPLAY-DATE TO F-NAMEFIELD
            ELSE
                MOVE WS-JRNDATE TO SPLIT-DATE
                PERFORM CONVERT-DATE-FORMAT
                MOVE DISPLAY-DATE TO F-NAMEFIELD.
            IF WS-NEWORDER = "Y"
                MOVE WS-DATE TO GLJRN-DATE.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-120.
            MOVE "JRNDESC" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE GLJRN-MAIN-DESC TO F-NAMEFIELD WS-JRNDESC.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACTIONIND" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE GLJRN-ACTION TO F-NAMEFIELD WS-JRNACTION.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PERIODIND" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE GLJRN-PERIOD TO F-NAMEFIELD WS-JRNPERIOD.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "FILLERDESC" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            IF GLJRN-COMPLETE = " "
               MOVE "*NEW JOURNAL ENTRY *" TO F-NAMEFIELD.
            IF GLJRN-COMPLETE = "P"
               MOVE "*JRN PENDING CHANGE*" TO F-NAMEFIELD.
            IF GLJRN-COMPLETE = "R"
               MOVE "*READY TO BATCH POST" TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-140.
            MOVE "DEBITTOTAL" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE WS-DEBIT-TOTAL TO F-EDNAMEFIELDFORTOTAL.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-FORTOTAL.

            MOVE "CREDITTOTAL" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE WS-CREDIT-TOTAL TO F-EDNAMEFIELDFORTOTAL.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-FORTOTAL.

            MOVE "BALANCE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-BALANCE TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.
       GET-150.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "JRNDATE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01" OR = X"07"
                PERFORM DISPLAY-FORM
                PERFORM DISPLAY-FORM-GL-TOP-INFO
                GO TO GET-010.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO GET-150.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO WS-JRNDATE GLJRN-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
                 GO TO GET-150.

            MOVE WS-CURRENTPER TO SUB-1.
            MOVE GL-BEGDATE (SUB-1) TO WS-BEG-DATE.
            MOVE GL-ENDDATE (SUB-1) TO WS-END-DATE.
            MOVE GLJRN-DATE TO WS-CH-DATE.
            IF WS-CH-DATE NOT < WS-BEG-DATE
             IF WS-CH-DATE NOT > WS-END-DATE
                GO TO GET-160.
            IF GLJRN-PERIOD NOT = "LYR"
             IF WS-CH-DATE > WS-END-DATE
            MOVE "THE JOURNAL DATE MUST BE < OR = THE END PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-150.
            IF GLJRN-PERIOD NOT = "LYR"
             IF WS-CH-DATE < WS-BEG-DATE
            MOVE "THE JOURNAL DATE MUST > OR = THE BEG. PERIOD DATE."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-150.
       GET-160.
            MOVE "                                " TO F-NAMEFIELD.
            MOVE "JRNDESC" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-150.
            IF F-EXIT-CH = X"07"
                PERFORM DISPLAY-FORM
                PERFORM DISPLAY-FORM-GL-TOP-INFO
                GO TO GET-010.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-JRNDESC GLJRN-MAIN-DESC.
            IF WS-JRNDESC = "     "
                MOVE "THIS FIELD SHOULD HAVE A DESCRIPTION ENTERED"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-160.
       GET-170.
            MOVE "                                " TO F-NAMEFIELD.
            MOVE "ACTIONIND" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-160.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-JRNACTION GLJRN-ACTION.
            IF WS-JRNACTION NOT = " " AND NOT = "R"
               MOVE
           "THE ACTION IND. SHOULD BE BLANK OR = 'R', <Esc> TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-170.
       GET-180.
            MOVE "                                " TO F-NAMEFIELD.
            MOVE "PERIODIND" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-170.
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = "LYR"
                MOVE F-NAMEFIELD TO WS-JRN-LY-PERIOD
                PERFORM CHANGE-DATE-AND-PERIOD
                GO TO GET-190.
                
            IF WS-1STPER = "L"
             IF F-NAMEFIELD NOT = "LYR"
               MOVE
            "THE PERIOD IND1. SHOULD BE BLANK, 'Fxx, 'Pxx' OR LYR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-180.
               
            MOVE F-NAMEFIELD TO WS-JRNPERIOD GLJRN-PERIOD.
            IF WS-1STPER NOT = " " AND NOT = "0" 
                     AND NOT = "P" AND NOT = "F"
               MOVE
            "THE PERIOD IND2. SHOULD BE BLANK, 'Fxx, 'Pxx' OR LYR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-180.
               
           IF WS-REST NOT = 0 AND NOT = 1 AND NOT = 2
                  AND NOT = 3 AND NOT = 4 AND NOT = 5
                  AND NOT = 6 AND NOT = 7 AND NOT = 8
                  AND NOT = 9 AND NOT = 10 AND NOT = 11
                  AND NOT = 12 AND NOT = " "
                 MOVE 
            "THE PERIOD IND3. SHOULD BE BLANK, 'Fxx, 'Pxx' OR LYR"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-JRNPERIOD TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE GLJRN-PERIOD TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-180.
            IF WS-1STPER = "P"
             IF WS-REST NOT < GLPA-CURRENT-GLPER
               MOVE
                "TO POST TO PERIOD 12 IN THE LAST YEAR USE 'LYR'."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-180.
       GET-190.
            MOVE 1 TO SUB-1 F-INDEX.
            PERFORM SCROLL-NEXT.
            PERFORM SCROLL-PREVIOUS.
            PERFORM FILL-BODY.
            IF WS-ABOVE-BODY = "1"
                GO TO GET-180.
       GET-200.
            IF WS-BALANCE = 0
               GO TO GET-250.
            MOVE "JOURNAL DOES NOT BALANCE, PRESS 'F8' TO SUSPEND,"
               TO WS-MESSAGE.
            PERFORM ERROR1-000.
            MOVE "        OR PRESS 'F7' TO CHANGE JOURNAL."
                TO WS-MESSAGE.
            PERFORM ERROR-000.
            MOVE "1" TO WS-TYPE-OF-END.
            GO TO GET-300.
       GET-250.
            IF GLPA-RECJRN-POST = "Y"
               MOVE "PRESS 'F8' TO SUSPEND, 'F9' TO POST IMMEDIATELTY,"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE "    OR PRESS 'F10' TO STORE FOR BATCH POSTING."
               TO WS-MESSAGE
               PERFORM ERROR-000
               MOVE "2" TO WS-TYPE-OF-END
               GO TO GET-300.
       GET-260.
            IF GLPA-RECJRN-POST NOT = "Y"
               MOVE
             "PRESS 'F8' TO SUSPEND OR 'F10' TO STORE FOR BATCH POSTING"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE "3" TO WS-TYPE-OF-END.
       GET-300.
            MOVE "FINAL" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF WS-TYPE-OF-END = "1"
             IF F-EXIT-CH NOT = X"1C" AND NOT = X"1D"
               GO TO GET-300.
            IF WS-TYPE-OF-END = "2"
             IF F-EXIT-CH NOT = X"1D" AND NOT = X"1E" AND NOT = X"1F"
               GO TO GET-300.
            IF WS-TYPE-OF-END = "3"
             IF F-EXIT-CH NOT = X"1D" AND NOT = X"1F"
               GO TO GET-300.
      *
      *WS-TYPE-OF-END: 1=AMEND 2=SUSPEND 3=IMM-POST 4=BATCH-POST
      * <F7>
            IF F-EXIT-CH = X"1C"
               MOVE "1" TO WS-TYPE-OF-END.
      * <F8>
            IF F-EXIT-CH = X"1D"
               MOVE "2" TO WS-TYPE-OF-END.
      * <F9>
            IF F-EXIT-CH = X"1E"
               MOVE "3" TO WS-TYPE-OF-END.
      * <f10>
            IF F-EXIT-CH = X"1F"
               MOVE "4" TO WS-TYPE-OF-END.
       GET-350.
           PERFORM ERROR-020.
           PERFORM ERROR1-020.
           IF WS-TYPE-OF-END = "1"
              GO TO GET-190.
           IF WS-TYPE-OF-END = "2" OR = "4"
              PERFORM REWRITE-JOURNAL
              GO TO GET-999.
           IF WS-TYPE-OF-END = "3"
            IF WS-JRN-LY-PERIOD NOT = "LYR"
              MOVE "POSTING OF JOURNAL IN PROGRESS......."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              PERFORM REWRITE-JOURNAL
              PERFORM UPDATE-GLMASTER
              PERFORM WRITE-GLTRANS
              PERFORM ERROR1-020
              PERFORM ERROR-020
              GO TO GET-999.
           IF WS-TYPE-OF-END = "3"
            IF WS-JRN-LY-PERIOD = "LYR"
              MOVE "POSTING OF JOURNAL INTO L/YEAR IN PROGRESS......."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              PERFORM REWRITE-JOURNAL
              PERFORM UPDATE-GLMASTER
              PERFORM UPDATE-GLMASTER-LY
              PERFORM WRITE-GLTRANS-LY
              PERFORM ERROR-020
              PERFORM ERROR1-020.
       GET-999.
            EXIT.
      *
       CHANGE-DATE-AND-PERIOD SECTION.
       CDAP-150.
            MOVE 12           TO WS-CURRENTPER.
            
            MOVE "JRNDATE"    TO F-FIELDNAME
            MOVE 7            TO F-CBFIELDNAME
            MOVE 10           TO F-CBFIELDLENGTH.
            
            MOVE 28    TO WS-CH-DD
            MOVE 2     TO WS-CH-MM
            MOVE WS-YY TO WS-CH-YY
            MOVE WS-CH-DATE TO WS-NEW-DATE.
            
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO WS-JRNDATE GLJRN-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
                 GO TO CDAP-150.
       CDAP-999.
           EXIT.
      *
       FILL-BODY SECTION.
       FILL-000.
           MOVE " " TO WS-ABOVE-BODY.
           MOVE 1 TO SUB-1 SUB-2 SUB-3.
       FILL-005.
           PERFORM ERROR-020.
           MOVE 3010 TO POS.
           DISPLAY "    BODY LINE: " AT POS.
           ADD 16 TO POS.
           MOVE SUB-1 TO WS-BODY-LINE.
           DISPLAY WS-BODY-LINE AT POS.

           PERFORM RUNNING-TOTAL.
       FILL-010.
           MOVE "                              " TO F-NAMEFIELD.
           MOVE "ACCNUM" TO F-FIELDNAME.
           MOVE 6 TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.

           IF F-EXIT-CH = X"0B" 
            IF GLJRN-GLNUMBER (SUB-1) = " "
               GO TO FILL-010.

           IF F-EXIT-CH = X"01" AND F-INDEX = 1
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM READ-FIELD-ALPHA
            IF F-NAMEFIELD = GLJRN-GLNUMBER (SUB-1)
               MOVE "1" TO WS-ABOVE-BODY
               GO TO FILL-999
            ELSE
               MOVE GLJRN-GLNUMBER (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               MOVE "1" TO WS-ABOVE-BODY
               GO TO FILL-999.

           MOVE 12 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           IF F-EXIT-CH = X"01"
            IF GLJRN-GLNUMBER (SUB-1) = "  "
               SUBTRACT 1 FROM F-INDEX SUB-1
               GO TO FILL-010.
           
           IF F-EXIT-CH = X"01" AND F-INDEX > 1
            IF F-NAMEFIELD = GLJRN-GLNUMBER (SUB-1)
               SUBTRACT 1 FROM F-INDEX SUB-1
               PERFORM SCROLL-050
               GO TO FILL-005
            ELSE
               MOVE GLJRN-GLNUMBER (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               SUBTRACT 1 FROM F-INDEX SUB-1
               PERFORM SCROLL-050
               GO TO FILL-005.

           IF F-EXIT-CH = X"0B" AND F-INDEX < 15
            IF F-NAMEFIELD = GLJRN-GLNUMBER (SUB-1)
               ADD 1 TO F-INDEX SUB-1
               PERFORM SCROLL-050
               GO TO FILL-005
            ELSE
               MOVE GLJRN-GLNUMBER (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               ADD 1 TO F-INDEX SUB-1
               PERFORM SCROLL-050
               GO TO FILL-005.
           IF F-EXIT-CH = X"0B" AND F-INDEX = 15
            IF F-NAMEFIELD = GLJRN-GLNUMBER (SUB-1)
               PERFORM SCROLL-NEXT
               PERFORM SCROLL-050
               GO TO FILL-005
            ELSE
               MOVE GLJRN-GLNUMBER (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               PERFORM SCROLL-NEXT
               PERFORM SCROLL-050
               GO TO FILL-005.
      ****  USED TO CREATE A SPACE BETWEEN LINES *****      
      ****<CODE-TAB>=X"89" IN CTOS, <ALT-F8>X"9d" IN LINUX
           IF F-EXIT-CH = X"89" OR = X"9D"
                AND SUB-25 < 50
                PERFORM EMPTY-LINE
                MOVE SUB-3 TO SUB-1
              IF SUB-1 > 3
                SUBTRACT 3 FROM SUB-1
                PERFORM SCROLL-NEXT
                ADD 1 TO SUB-25
                GO TO FILL-005 
              ELSE
                PERFORM SCROLL-NEXT
                ADD 1 TO SUB-25
                GO TO FILL-005.

           IF F-EXIT-CH = X"11"
               PERFORM SCROLL-NEXT
               GO TO FILL-005.
           IF F-EXIT-CH = X"0C"
               PERFORM SCROLL-NEXT-PAGE
               GO TO FILL-005.
           IF F-EXIT-CH = X"05"
               PERFORM SCROLL-PREVIOUS
               GO TO FILL-005.
           IF F-EXIT-CH = X"13"
               PERFORM SCROLL-DOWN
               GO TO FILL-005.
      * TAB CHARACTER
           IF F-EXIT-CH = X"09"
      *        move "tab pressed" to ws-message
      *        perform error-message
            IF F-NAMEFIELD = GLJRN-GLNUMBER (SUB-1)
               MOVE " " TO WS-ABOVE-BODY GL-DESCRIPTION
               PERFORM FILL-015
               PERFORM ERROR-020
               PERFORM CHECK-SUB1-TOTAL
               GO TO FILL-999
            ELSE
               MOVE GLJRN-GLNUMBER (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               MOVE " " TO WS-ABOVE-BODY GL-DESCRIPTION
               PERFORM FILL-015
               PERFORM ERROR-020
               PERFORM CHECK-SUB1-TOTAL
               GO TO FILL-999.

      *****************************************************************
      *CREATE A BLANK SPACE INTO WHICH A NEW LINE OF STOCK IS ENTERED *
      *****************************************************************
      * <CODE-TAB> (CTOS); <ALT-F8> =X"9D" IN LINUX
      * CTOS SECTION
           IF F-EXIT-CH = X"89"
            IF F-NAMEFIELD = GLJRN-GLNUMBER (SUB-1)
               MOVE " " TO WS-ABOVE-BODY GL-DESCRIPTION
               PERFORM FILL-015
               PERFORM ERROR-020
               PERFORM CHECK-SUB1-TOTAL
               GO TO FILL-999
            ELSE
               MOVE GLJRN-GLNUMBER (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               MOVE " " TO WS-ABOVE-BODY GL-DESCRIPTION
               PERFORM FILL-015
               PERFORM ERROR-020
               PERFORM CHECK-SUB1-TOTAL
               GO TO FILL-999.
      * LINUX SECTION
           IF F-EXIT-CH = X"9D"
            IF F-NAMEFIELD = GLJRN-GLNUMBER (SUB-1)
               MOVE " " TO WS-ABOVE-BODY GL-DESCRIPTION
               PERFORM FILL-015
               PERFORM ERROR-020
               PERFORM CHECK-SUB1-TOTAL
               GO TO FILL-999
            ELSE
               MOVE GLJRN-GLNUMBER (SUB-1) TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               MOVE " " TO WS-ABOVE-BODY GL-DESCRIPTION
               PERFORM FILL-015
               PERFORM ERROR-020
               PERFORM CHECK-SUB1-TOTAL
               GO TO FILL-999.
           IF F-EXIT-CH = X"07"
               AND GLJRN-GLNUMBER (SUB-1) = " "
               GO TO FILL-010.
           IF F-EXIT-CH = X"87" OR = X"9F"
               MOVE SUB-1 TO SUB-7
               PERFORM CANCEL-TRANSACTION
               MOVE 1 TO SUB-1
                         F-INDEX
               PERFORM SCROLL-NEXT
               PERFORM SCROLL-PREVIOUS
            IF SUB-25 > 8
               SUBTRACT 5 FROM SUB-25
               MOVE SUB-25 TO SUB-1
               PERFORM SCROLL-NEXT
               ADD 4 TO SUB-25
               GO TO FILL-005
            ELSE
               GO TO FILL-005.
      *
      *<CODE-RETURN> TO USE LAST-LINE ACCOUNT
           IF F-EXIT-CH = X"8A"
               MOVE WS-GLNUMBER TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               GO TO FILL-012.
      *
      *<CODE-NEXT> TO READ-NEXT ACCOUNT
           IF F-EXIT-CH = X"8C"
               PERFORM READ-NEXT-GLMASTER
            IF WS-GLMAST-ST1 = 0
               MOVE GL-NUMBER TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               GO TO FILL-012
            ELSE
               GO TO FILL-010.
      *<CODE-PREV> TO READ-PREV ACCOUNT
           IF F-EXIT-CH = X"85"
               PERFORM READ-PREV-GLMASTER
            IF WS-GLMAST-ST1 = 0
               MOVE GL-NUMBER TO F-NAMEFIELD
               MOVE 12 TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               GO TO FILL-012
            ELSE
               GO TO FILL-010.
           IF F-NAMEFIELD = " "
               GO TO FILL-010.
           IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B"
                GO TO FILL-010.
       FILL-012.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM NUMBER-CHECK.
           MOVE WS-GLNO-CHECK TO GLJRN-GLNUMBER (SUB-1) WS-GLNUMBER.
           IF WS-GLSUBHEADER = "    "
              MOVE "YOU CAN ONLY POST TO A DETAIL NOT A HEADER ACCOUNT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE " " TO GLJRN-GLNUMBER (SUB-1)
                GO TO FILL-005.
           IF WS-RESTOFACCOUNT = "      "
               MOVE "YOU CAN ONLY POST TO A DETAIL NOT A SUBHEADER ACC."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE " " TO GLJRN-GLNUMBER (SUB-1)
                GO TO FILL-005.
           PERFORM READ-GLMASTER.
           IF GL-DESCRIPTION = "INVALID"
                MOVE "INVALID GLMASTER NUMBER !!" TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO FILL-010.

            MOVE "ACCNUM"      TO F-FIELDNAME
            MOVE 6             TO F-CBFIELDNAME
            MOVE WS-GLNO-CHECK TO F-NAMEFIELD
            MOVE 12            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       FILL-015.
            MOVE "ACCDESC"      TO F-FIELDNAME
            MOVE 7              TO F-CBFIELDNAME
            MOVE GL-DESCRIPTION TO F-NAMEFIELD
            MOVE 40             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       FILL-020.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "AMOUNT" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 12       TO F-CBFIELDLENGTH.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.
            MOVE "                       " TO F-NAMEFIELD.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO GLJRN-AMOUNT (SUB-1).
            IF GLJRN-AMOUNT (SUB-1) = 0
                DISPLAY " " AT 3079 WITH BELL
                GO TO FILL-020.
            MOVE GLJRN-AMOUNT (SUB-1) TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.
       FILL-042.
            IF GLJRN-AMOUNT (SUB-1) < 0
               MOVE "CREDITAMT"          TO F-FIELDNAME
               MOVE 9                    TO F-CBFIELDNAME
               MOVE GLJRN-AMOUNT (SUB-1) TO WS-JRNAMOUNT
               MOVE WS-JRNAMOUNT         TO F-EDNAMEFIELDFORTOTAL
               MOVE 11                   TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-FORTOTAL

               MOVE "DEBITAMT" TO F-FIELDNAME
               MOVE 8          TO F-CBFIELDNAME
               MOVE " "        TO F-NAMEFIELD
               MOVE 11         TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA.

            IF GLJRN-AMOUNT (SUB-1) > 0
               MOVE "DEBITAMT"           TO F-FIELDNAME
               MOVE 8                    TO F-CBFIELDNAME
               MOVE GLJRN-AMOUNT (SUB-1) TO WS-JRNAMOUNT
               MOVE WS-JRNAMOUNT         TO F-EDNAMEFIELDFORTOTAL
               MOVE 11                   TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-FORTOTAL

               MOVE "CREDITAMT" TO F-FIELDNAME
               MOVE 9           TO F-CBFIELDNAME
               MOVE " "         TO F-NAMEFIELD
               MOVE 11          TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA.
       FILL-050.
           MOVE "                                " TO F-NAMEFIELD.
           MOVE "TRANSDESC" TO F-FIELDNAME.
           MOVE 9 TO F-CBFIELDNAME.
           MOVE 25 TO F-CBFIELDLENGTH.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
               GO TO FILL-020.
      *<CODE-RETURN>
           IF F-EXIT-CH = X"8A"
              MOVE WS-JRNDESC TO GLJRN-LINE-DESC (SUB-1) F-NAMEFIELD
              MOVE 25 TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-ALPHA
              GO TO FILL-050.
      *<CODE-DOWN-ARROW>
           IF F-EXIT-CH = X"8B"
            IF SUB-1 NOT = 1
              COMPUTE SUB-2 = SUB-1 - 1
              MOVE GLJRN-LINE-DESC (SUB-2) TO GLJRN-LINE-DESC (SUB-1)
                                              F-NAMEFIELD
              MOVE 25 TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-ALPHA
              GO TO FILL-050
            ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO FILL-050.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO GLJRN-LINE-DESC (SUB-1).
           IF GLJRN-LINE-DESC (SUB-1) = "     "
              MOVE WS-JRNDESC TO GLJRN-LINE-DESC (SUB-1) F-NAMEFIELD
              MOVE 25 TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-ALPHA.
       FILL-090.
           ADD 1 TO SUB-1 F-INDEX.
           IF SUB-25 < SUB-1
               MOVE SUB-1 TO SUB-25.
           IF SUB-1 > 50             
               PERFORM RUNNING-TOTAL
               MOVE 50 TO SUB-1 SUB-25
               MOVE "50 LINES ARE UP, PRESS 'ESC' TO TAB."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-999.
           IF F-INDEX < 16
               GO TO FILL-005.
           SUBTRACT 1 FROM SUB-1. 
           PERFORM SCROLL-NEXT.
           GO TO FILL-005.
       FILL-999.
           EXIT.
      *
       CHECK-SUB1-TOTAL SECTION.
       CHK-000.
            MOVE 1 TO SUB-1.
       CHK-010.
           IF SUB-1 < 50
             IF GLJRN-GLNUMBER (SUB-1) NOT = " "
                ADD 1 TO SUB-1
                MOVE SUB-1 TO SUB-20 SUB-25
                GO TO CHK-010.
       CHK-EXIT.
           EXIT.
      *
       RUNNING-TOTAL SECTION.
       RUN-000.
           MOVE 1 TO SUB-3.
           MOVE 0 TO WS-BALANCE
                     WS-JRNAMOUNT
                     WS-DEBIT-TOTAL
                     WS-CREDIT-TOTAL.
       RUN-010.
           IF GLJRN-GLNUMBER (SUB-3) = " "
              MOVE SUB-3 TO SUB-25
              GO TO RUN-020.
           ADD GLJRN-AMOUNT (SUB-3) TO WS-BALANCE.
           MOVE GLJRN-AMOUNT (SUB-3) TO WS-JRNAMOUNT.
           IF GLJRN-AMOUNT (SUB-3) < 0
              ADD WS-JRNAMOUNT TO WS-CREDIT-TOTAL
           ELSE
              ADD WS-JRNAMOUNT TO WS-DEBIT-TOTAL.
       RUN-011.
           IF WS-CREDIT-TOTAL > 99999999.99
              MOVE "** THE CREDIT AMOUNT HAS EXCEEDED R99 999 999.99 **"
              TO WS-MESSAGE
              PERFORM ERROR-000
              MOVE " **  YOU MUST CANCEL YOUR LAST LINE ENTERED ! **"
              TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              MOVE " " TO WS-MESSAGE
              PERFORM ERROR-020
              SUBTRACT 1 FROM SUB-1
            IF F-INDEX > 1
              SUBTRACT 1 FROM F-INDEX
              PERFORM SCROLLING
            ELSE
              PERFORM SCROLLING.
           IF WS-DEBIT-TOTAL > 99999999.99
              MOVE "** THE DEBIT AMOUNT HAS EXCEEDED R99 999 999.99 **"
              TO WS-MESSAGE
              PERFORM ERROR-000
              MOVE " **  YOU MUST CANCEL YOUR LAST LINE ENTERED ! **"
              TO WS-MESSAGE
              PERFORM ERROR1-MESSAGE
              MOVE " " TO WS-MESSAGE
              PERFORM ERROR-020
              SUBTRACT 1 FROM SUB-1
            IF F-INDEX > 1
              SUBTRACT 1 FROM F-INDEX
              PERFORM SCROLLING
            ELSE
              PERFORM SCROLLING.
       RUN-015.
           ADD 1 TO SUB-3.
           IF SUB-3 > 50
               MOVE SUB-3 TO SUB-25
               GO TO RUN-020.
           GO TO RUN-010.
       RUN-020.
           PERFORM GET-140.
       RUN-999.
           EXIT.
      *
       READ-GLMASTER SECTION.
       RD-000.
           MOVE WS-GLNUMBER TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       RD-010.
           READ GL-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "INVALID" TO GL-DESCRIPTION
               GO TO RD-999.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER RECORD BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO RD-010.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       RD-999.
           EXIT.
      *
       START-GL-NEXT SECTION.
       SGN-000.
           MOVE WS-GLNUMBER TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER RECORD BUSY ON START, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              MOVE " " TO WS-GLNUMBER
              GO TO SGN-000.
       SGN-999.
           EXIT.
      *
       READ-NEXT-GLMASTER SECTION.
       RDN-010.
           READ GL-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 10
               MOVE "END OF NEXT-PAGE SEQUENCE, ENTER A NEW NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDN-999.
           IF WS-GLMAST-ST1 = 91
               PERFORM START-GL-NEXT
               GO TO RDN-010.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO RDN-010.
           MOVE GL-NUMBER TO WS-GLNUMBER.
           IF WS-RESTOFACCOUNT = " "
               GO TO RDN-010.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       RDN-999.
           EXIT.
      *
       READ-PREV-GLMASTER SECTION.
       RPREV-010.
           READ GL-MASTER PREVIOUS
               AT END NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 10
               MOVE "END OF NEXT-PAGE SEQUENCE, ENTER A NEW NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RPREV-999.
           IF WS-GLMAST-ST1 = 91
               PERFORM START-GL-NEXT
               GO TO RPREV-010.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER BUSY ON READ-PREV, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO RPREV-010.
           MOVE GL-NUMBER TO WS-GLNUMBER.
           IF WS-RESTOFACCOUNT = " "
               GO TO RPREV-010.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       RPREV-999.
           EXIT.
      *
       UPDATE-GLMASTER SECTION.
       UPGL-000.
           IF WS-1STPER = "F"
              GO TO UPGL-999.
           IF WS-JRN-LY-PERIOD = "LYR"
              MOVE 1 TO WS-REST SUB-3
              MOVE 1 TO SUB-1
              GO TO UPGL-005.
           
           IF WS-REST NOT = 0 AND NOT = " "
              MOVE WS-REST TO SUB-3
           ELSE
              MOVE WS-CURRENTPER TO WS-REST SUB-3.
           MOVE 1 TO SUB-1.
       UPGL-005.
           IF GLJRN-GLNUMBER (SUB-1) = "   "
                 GO TO UPGL-999.
           MOVE GLJRN-GLNUMBER  (SUB-1) TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGL-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "GLMASTER FILE DOES NOT EXIST, CALL THE SUPERVISOR."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE GL-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO UPGL-950.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
               GO TO UPGL-010.
               
           IF WS-JRN-LY-PERIOD NOT = "LYR"
               ADD GLJRN-AMOUNT (SUB-1) TO GL-BALANCE
               ADD GLJRN-AMOUNT (SUB-1) TO GL-PER (SUB-3).
           IF WS-1STPER = "P"
               ADD GLJRN-AMOUNT (SUB-1) TO GL-OPEN-PER-BAL.
               
           IF WS-JRN-LY-PERIOD = "LYR"
            IF GL-P-B = "B"
               ADD GLJRN-AMOUNT (SUB-1) TO GL-BALANCE
                                           GL-OPEN-PER-BAL
                                           GL-OPEN-YEAR-BAL
               ADD GLJRN-AMOUNT (SUB-1) TO GL-LAST-PER (12)
            ELSE
               ADD GLJRN-AMOUNT (SUB-1) TO GL-LAST-PER (12).
       UPGL-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGL-999.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGL-900.
       UPGL-950.
           PERFORM UPDATE-GLHEADER.
           PERFORM UPDATE-GLSUBHEADER.
           ADD 1 TO SUB-1.
           IF SUB-1 NOT > 50
               GO TO UPGL-005.
           MOVE 1 TO SUB-1.
       UPGL-999.
           EXIT.
     *
       UPDATE-GLHEADER SECTION.
       UPGLH-005.
           MOVE GLJRN-GLNUMBER (SUB-1) TO WS-GLNUMBER.
           MOVE WS-GLHEADER TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
              MOVE "GLHEADER FILE DOES NOT EXIST, CALL YOUR SUPERVISOR"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGLH-999.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLHEADER FILE BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGLH-010.
           IF WS-JRN-LY-PERIOD NOT = "LYR"
               ADD GLJRN-AMOUNT (SUB-1) TO GL-BALANCE
               ADD GLJRN-AMOUNT (SUB-1) TO GL-PER (SUB-3).
           IF WS-1STPER = "P"
               ADD GLJRN-AMOUNT (SUB-1) TO GL-OPEN-PER-BAL.
            IF WS-JRN-LY-PERIOD = "LYR"
             IF GL-P-B = "B"
               ADD GLJRN-AMOUNT (SUB-1) TO GL-BALANCE
                                           GL-OPEN-PER-BAL
                                           GL-OPEN-YEAR-BAL
               ADD GLJRN-AMOUNT (SUB-1) TO GL-LAST-PER (12)
            ELSE
               ADD GLJRN-AMOUNT (SUB-1) TO GL-LAST-PER (12).
       UPGLH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLH-999.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLHEADER RECORD BUSY ON WRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGLH-900.
       UPGLH-999.
           EXIT.
     *
       UPDATE-GLSUBHEADER SECTION.
       UPGLSH-005.
           MOVE GLJRN-GLNUMBER (SUB-1) TO WS-GLNUMBER.
           MOVE WS-HEAD-SUB TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       UPGLSH-010.
           READ GL-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
              MOVE "GLSUBHEAD FILE DOES'NT EXIST, CALL YOUR SUPERVISOR"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGLSH-999.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLSUBHEADER FILE BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGLSH-010.
            IF WS-JRN-LY-PERIOD NOT = "LYR"
               ADD GLJRN-AMOUNT (SUB-1) TO GL-BALANCE
               ADD GLJRN-AMOUNT (SUB-1) TO GL-PER (SUB-3).
           IF WS-1STPER = "P"
               ADD GLJRN-AMOUNT (SUB-1) TO GL-OPEN-PER-BAL.
            IF WS-JRN-LY-PERIOD = "LYR"
             IF GL-P-B = "B"
               ADD GLJRN-AMOUNT (SUB-1) TO GL-BALANCE
                                           GL-OPEN-PER-BAL
                                           GL-OPEN-YEAR-BAL
               ADD GLJRN-AMOUNT (SUB-1) TO GL-LAST-PER (12)
            ELSE
               ADD GLJRN-AMOUNT (SUB-1) TO GL-LAST-PER (12).
       UPGLSH-900.
           REWRITE GL-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               GO TO UPGLSH-999.
           IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLSUBHEAD FILE BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO UPGLSH-900.
       UPGLSH-999.
           EXIT.
      *
       UPDATE-GLMASTER-LY SECTION.
       UPGL-LY000.
           MOVE 12 TO WS-REST SUB-3.
           MOVE 1  TO SUB-1.
       UPGL-LY005.
           IF GLJRN-GLNUMBER (SUB-1) = "   "
                 GO TO UPGL-LY999.
           MOVE GLJRN-GLNUMBER  (SUB-1) TO GL-LY-NUMBER.
           START GL-LY-MASTER KEY NOT < GL-LY-KEY.
       UPGL-LY010.
           READ GL-LY-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GL-LY-ST1 = 23 OR 35 OR 49
              MOVE "GLMASTER-LY DOES NOT EXIST, CALL YOUR SUPERVISOR"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GL-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GL-LY-ST1
              MOVE GL-LY-NUMBER TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO UPGL-LY950.
           IF WS-GL-LY-ST1 NOT = 0
              MOVE "GLMASTER-LY BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GL-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GL-LY-ST1
              GO TO UPGL-LY010.
           ADD GLJRN-AMOUNT (SUB-1) TO GL-LY-BALANCE.
           ADD GLJRN-AMOUNT (SUB-1) TO GL-LY-PER (SUB-3).
       UPGL-LY900.
           REWRITE GL-LY-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GL-LY-ST1 = 23 OR 35 OR 49
               GO TO UPGL-LY999.
           IF WS-GL-LY-ST1 NOT = 0
              MOVE "GLMASTER-LY BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GL-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GL-LY-ST1
              GO TO UPGL-LY900.
       UPGL-LY950.
           PERFORM UPDATE-GLHEADER-LY.
           PERFORM UPDATE-GLSUBHEADER-LY.
           ADD 1 TO SUB-1.
           IF SUB-1 NOT > 50
               GO TO UPGL-LY005.
           MOVE 1 TO SUB-1.
       UPGL-LY999.
           EXIT.
     *
       UPDATE-GLHEADER-LY SECTION.
       UPGLH-LY005.
           MOVE GLJRN-GLNUMBER (SUB-1) TO WS-GLNUMBER.
           MOVE WS-GLHEADER TO GL-LY-NUMBER.
           START GL-LY-MASTER KEY NOT < GL-LY-KEY.
       UPGLH-LY010.
           READ GL-LY-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GL-LY-ST1 = 23 OR 35 OR 49
               MOVE "GLHEADER-LY DOES NOT EXIST, CALL YOUR SUPERVISOR"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GL-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GL-LY-ST1
              GO TO UPGLH-LY999.
           IF WS-GL-LY-ST1 NOT = 0
              MOVE "GLHEADER-LY BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GL-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GL-LY-ST1
              GO TO UPGLH-LY010.
           ADD GLJRN-AMOUNT (SUB-1) TO GL-LY-BALANCE.
           ADD GLJRN-AMOUNT (SUB-1) TO GL-LY-PER (SUB-3).
       UPGLH-LY900.
           REWRITE GL-LY-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GL-LY-ST1 = 23 OR 35 OR 49
               GO TO UPGLH-LY999.
           IF WS-GL-LY-ST1 NOT = 0
              MOVE "GLHEADER-LY BUSY ON REWRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GL-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GL-LY-ST1
              GO TO UPGLH-LY900.
       UPGLH-LY999.
           EXIT.
     *
       UPDATE-GLSUBHEADER-LY SECTION.
       UPGLSH-LY005.
           MOVE GLJRN-GLNUMBER (SUB-1) TO WS-GLNUMBER.
           MOVE WS-HEAD-SUB TO GL-LY-NUMBER.
           START GL-LY-MASTER KEY NOT < GL-LY-KEY.
       UPGLSH-LY010.
           READ GL-LY-MASTER WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-GL-LY-ST1 = 23 OR 35 OR 49
               MOVE "GLSUBHEAD-LY DOESN'T EXIST, CALL YOUR SUPERVISOR"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GL-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GL-LY-ST1
              GO TO UPGLSH-LY999.
           IF WS-GL-LY-ST1 NOT = 0
              MOVE "GLSUBHEADER-LY BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GL-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GL-LY-ST1
              GO TO UPGLSH-LY010.
           ADD GLJRN-AMOUNT (SUB-1) TO GL-LY-BALANCE.
           ADD GLJRN-AMOUNT (SUB-1) TO GL-LY-PER (SUB-3).
       UPGLSH-LY900.
           REWRITE GL-LY-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-GL-LY-ST1 = 23 OR 35 OR 49
               GO TO UPGLSH-LY999.
           IF WS-GL-LY-ST1 NOT = 0
              MOVE "GLSUBHEAD FILE BUSY ON REWRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GL-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GL-LY-ST1
              GO TO UPGLSH-LY900.
       UPGLSH-LY999.
           EXIT.
      *
       READ-JOURNAL SECTION.
       RSTT-000.
           MOVE 1 TO SUB-1.
           MOVE WS-JRN TO GLJRN-REFERENCE.
           START GLJRN-FILE KEY NOT < GLJRN-KEY
              INVALID KEY NEXT SENTENCE.
       RSTT-010.
           READ GLJRN-FILE WITH LOCK
              INVALID KEY NEXT SENTENCE.
           IF WS-GLJRN-ST1 = 23 OR 35 OR 49
              GO TO RSTT-999.
           IF WS-GLJRN-ST1 NOT = 0
              MOVE "GLJRN RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLJRN-ST1
              GO TO RSTT-010.
           IF GLJRN-REFERENCE NOT = WS-JRN
              GO TO RSTT-999.
           IF GLJRN-COMPLETE = "Y"
              MOVE 88 TO WS-GLJRN-ST1
              MOVE "THIS JOURNAL HAS ALREADY BEEN POSTED." TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSTT-999.
           MOVE GLJRN-MAIN-DESC      TO WS-JRNDESC.
           IF GLJRN-PERIOD = "LYR"
              MOVE GLJRN-PERIOD      TO WS-JRN-LY-PERIOD
           ELSE
              MOVE GLJRN-PERIOD      TO WS-JRNPERIOD.
           MOVE GLJRN-ACTION         TO WS-JRNACTION.
           MOVE GLJRN-DATE           TO WS-JRNDATE.
       RSTT-050.
           IF WS-NEWORDER = "Y"
              MOVE "N" TO WS-NEWORDER.
           PERFORM RUNNING-TOTAL.
       RSTT-999.
           EXIT.
      *
       REWRITE-JOURNAL SECTION.
       RWST-000.
            MOVE 1 TO SUB-1.
            MOVE WS-JRN TO GLJRN-REFERENCE.
            START GLJRN-FILE KEY NOT < GLJRN-KEY
                INVALID KEY NEXT SENTENCE.
       RWST-006.
            MOVE WS-JRN                TO GLJRN-REFERENCE.
            MOVE WS-JRNDESC            TO GLJRN-MAIN-DESC.
            IF WS-JRN-LY-PERIOD NOT = "LYR"
                MOVE WS-JRNPERIOD      TO GLJRN-PERIOD
            ELSE
                MOVE WS-JRN-LY-PERIOD  TO GLJRN-PERIOD.
                
            MOVE WS-JRNACTION          TO GLJRN-ACTION.
            MOVE WS-JRNDATE            TO GLJRN-DATE.
      *
      *WS-TYPE-OF-END: 2=SUSPEND(P) 3=IMM-POST(Y) 4=BATCH-POST(R)
      *
           IF WS-TYPE-OF-END = "2"
              MOVE "P"              TO GLJRN-COMPLETE.
           IF WS-TYPE-OF-END = "4"
              MOVE "R"              TO GLJRN-COMPLETE.
           IF WS-TYPE-OF-END = "3" 
              MOVE "Y"              TO GLJRN-COMPLETE.
           IF WS-NEWORDER = "Y"
              GO TO RWST-019.
       RWST-018.
           REWRITE GLJRN-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-GLJRN-ST1 NOT = 0
              MOVE "GLJRN REWRITE ERROR - RWST-018, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLJRN-ST1
              GO TO RWST-018.
           GO TO RWST-999.
       RWST-019.
            WRITE GLJRN-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-GLJRN-ST1 NOT = 0
              MOVE "GLJRN WRITE ERROR - RWST-019, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLJRN-ST1
              GO TO RWST-019.
       RWST-999.
            EXIT.
      *
       DELETE-JOURNAL SECTION.
       DJRN-000.
           PERFORM CLEAR-010.
       DJRN-005.
           MOVE 3010 TO POS.
           DISPLAY
            "ENTER Y=YES TO DELETE, N=NO TO STOP THE DELETION. [ ]"
               AT POS
           ADD 51 TO POS

           MOVE 'N'       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 60        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

           IF WS-ANSWER NOT = "N" AND NOT = "Y"
              DISPLAY " " AT 3079 WITH BELL
              GO TO DJRN-005.
           IF WS-ANSWER = "N"
              GO TO DJRN-999.
       DJRN-018.
           DELETE GLJRN-FILE
              INVALID KEY NEXT SENTENCE.
           IF WS-GLJRN-ST1 NOT = 0
              MOVE "GLJRN BUSY ON DELETE, 'ESC' TO RETRY." 
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLJRN-ST1
              GO TO DJRN-018.
       DJRN-999.
            EXIT.
      *
       WRITE-GLTRANS SECTION.
       WRTR-000.
           OPEN I-O GLTRANS-FILE.
           IF WS-GLTRANS-ST1 NOT = 0 
              MOVE "GL TRANS. BUSY ON OPEN, PRESS 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO WRTR-000.
           MOVE 1 TO SUB-1.
       WRTR-010.
            IF GLJRN-GLNUMBER (SUB-1) = "   "
                 GO TO WRTR-900.
            PERFORM READ-PARAMETER-LOCK.
            MOVE GLPA-GLTRANSNO          TO GLTRANS-TRANS.
            ADD 1                        TO GLPA-GLTRANSNO.
            PERFORM REWRITE-PARAMETER.
            MOVE WS-JRN                  TO GLTRANS-REFERENCE.
            MOVE 1                       TO GLTRANS-TYPE.
            IF WS-1STPER = "P"
                MOVE " "                 TO GLTRANS-FUTURE
                MOVE WS-REST             TO GLTRANS-NO
            ELSE
                MOVE WS-JRNPERIOD        TO GLTRANS-PERIOD.
            MOVE WS-JRNDATE              TO GLTRANS-DATE.
            MOVE GLJRN-GLNUMBER (SUB-1)  TO GLTRANS-ACCOUNT-NUMBER.
            MOVE GLJRN-AMOUNT (SUB-1)    TO GLTRANS-AMOUNT.
            MOVE GLJRN-LINE-DESC (SUB-1) TO GLTRANS-LINE-DESC.
       WRTR-015.
            WRITE GLTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-GLTRANS-ST1 = 23 OR 35 OR 49
              MOVE "GLTRANS BUSY23 ON WRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO WRTR-015.
            IF WS-GLTRANS-ST1 NOT = 0
              MOVE "GLTRANS FILE BUSY ON WRITE, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO WRTR-015.
       WRTR-020.
            ADD 1 TO SUB-1.
            IF SUB-1 < 51
                GO TO WRTR-010.
            MOVE 1 TO SUB-1.
       WRTR-900.
            CLOSE GLTRANS-FILE.
       WRTR-999.
            EXIT.
      *
       WRITE-GLTRANS-LY SECTION.
       WRTR-LY-000.
           OPEN I-O GLTRANS-LY-FILE.
           IF WS-GLTRANS-LY-ST1 NOT = 0
              MOVE "GLTRANS-LY BUSY ON OPEN, PRESS 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
              GO TO WRTR-LY-000.
           MOVE 1 TO SUB-1.
       WRTR-LY-010.
            IF GLJRN-GLNUMBER (SUB-1) = "   "
                 GO TO WRTR-LY-900.
            PERFORM READ-PARAMETER-LOCK.
            MOVE GLPA-GLTRANSNO          TO GLTRANS-LY-TRANS.
            ADD 1                        TO GLPA-GLTRANSNO.
            PERFORM REWRITE-PARAMETER.
            MOVE WS-JRN                  TO GLTRANS-LY-REFERENCE
            MOVE 1                       TO GLTRANS-LY-TYPE
            MOVE WS-REST                 TO GLTRANS-LY-NO
            MOVE WS-JRNDATE              TO GLTRANS-LY-DATE
            MOVE GLJRN-GLNUMBER (SUB-1)  TO GLTRANS-LY-ACCNO
            MOVE GLJRN-AMOUNT (SUB-1)    TO GLTRANS-LY-AMOUNT
            MOVE GLJRN-LINE-DESC (SUB-1) TO GLTRANS-LY-LINE-DESC.
       WRTR-LY-015.
            WRITE GLTRANS-LY-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-GLTRANS-LY-ST1 = 23 OR 35 OR 49
              MOVE "GLTRANS-LY BUSY23 ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
              GO TO WRTR-LY-015.
            IF WS-GLTRANS-LY-ST1 NOT = 0
              MOVE "GLTRANS-LY BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
              GO TO WRTR-LY-015.
       WRTR-LY-020.
            ADD 1 TO SUB-1.
            IF SUB-1 < 51
                GO TO WRTR-LY-010.
            MOVE 1 TO SUB-1.
       WRTR-LY-900.
            CLOSE GLTRANS-LY-FILE.
       WRTR-LY-999.
            EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
              MOVE "NO GLPARAMETER RECORD READ, CALL THE SUPERVISOR."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       READ-PARAMETER-LOCK SECTION.
       RPL-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE WITH LOCK          
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
              MOVE "NO GLPARAMETER23 ON READ-LOCK, CALL THE SUPERVISOR."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON READ-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RPL-000.
       RPL-999.
           EXIT.
      *
       REWRITE-PARAMETER SECTION.
       REWP-000.
           REWRITE GLPARAMETER-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
              MOVE "NO GLPARAMETER23 ON REWRITE, CALL THE SUPERVISOR."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO REWP-000.
       REWP-999.
           EXIT.
      *
       SCROLL-NEXT SECTION.
       NEXT-000.
            ADD 1 TO SUB-1.
            IF SUB-1 > SUB-25
               MOVE SUB-25 TO SUB-1.
            IF SUB-1 < 1
               MOVE 1 TO SUB-1 SUB-25.
            MOVE 1 TO F-INDEX. 
            PERFORM CLEAR-BODY.
            MOVE 1 TO F-INDEX. 
            IF SUB-1 > 36
               MOVE 36 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 50  
                GO TO NEXT-030.
            IF F-INDEX < 16
                GO TO NEXT-010.
       NEXT-030.
            SUBTRACT 15 FROM SUB-1.
            IF SUB-1 > 35
              IF SUB-25 > 35
               COMPUTE F-INDEX = 15 - (50 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX. 
            IF F-INDEX > 15
                MOVE 1 TO F-INDEX.
            IF F-INDEX < 1
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3010 TO POS.
            DISPLAY "    BODY LINE: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-999.
             EXIT.
      *
       SCROLL-NEXT-PAGE SECTION.
       NEXT-PAGE-000.
            ADD 15 TO SUB-1.
            IF SUB-1 > SUB-25
               MOVE SUB-25 TO SUB-1.
            MOVE 1 TO F-INDEX. 
            PERFORM CLEAR-BODY.
            MOVE 1 TO F-INDEX. 
            IF SUB-1 > 36
               MOVE 36 TO SUB-1.
       NEXT-PAGE-010.
            PERFORM SCROLLING.
       NEXT-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 50  
                GO TO NEXT-PAGE-030.
            IF F-INDEX < 16
                GO TO NEXT-PAGE-010.
       NEXT-PAGE-030.
            SUBTRACT 15 FROM SUB-1.
            IF SUB-1 > 35
              IF SUB-25 > 35
               COMPUTE F-INDEX = 15 - (50 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX.
            IF SUB-1 > 50
               MOVE 36 TO SUB-1.
            IF F-INDEX > 15
                MOVE 1 TO F-INDEX.
            IF F-INDEX < 1
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3010 TO POS.
            DISPLAY "    BODY LINE: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-PAGE-999.
             EXIT.
      *
       SCROLL-PREVIOUS SECTION.
       PREV-000.
            SUBTRACT 15 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-010.
            PERFORM SCROLLING.
       PREV-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 50   
                GO TO PREV-030.
            IF F-INDEX < 16
                GO TO PREV-010.
       PREV-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 15 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

             MOVE 3010 TO POS.
             DISPLAY "    BODY LINE: " AT POS.
             ADD 16 TO POS.
             MOVE SUB-1 TO WS-BODY-LINE.
             DISPLAY WS-BODY-LINE AT POS.
       PREV-999.
             EXIT.
      *
       SCROLL-DOWN SECTION.
       SCROLL-DOWN-000.
            SUBTRACT 1 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       SCROLL-DOWN-010.
            PERFORM SCROLLING.
       SCROLL-DOWN-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 50
                GO TO SCROLL-DOWN-030.
            IF F-INDEX < 16
                GO TO SCROLL-DOWN-010.
       SCROLL-DOWN-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 15 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

             MOVE 3010 TO POS.
             DISPLAY "    BODY LINE: " AT POS.
             ADD 16 TO POS.
             MOVE SUB-1 TO WS-BODY-LINE.
             DISPLAY WS-BODY-LINE AT POS.
       SCROLL-DOWN-999.
             EXIT.
      *
       SCROLLING SECTION.
       SCROLL-010.
            MOVE "ACCNUM"               TO F-FIELDNAME
            MOVE 6                      TO F-CBFIELDNAME
            MOVE GLJRN-GLNUMBER (SUB-1) TO F-NAMEFIELD
            MOVE 12                     TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-015.
            MOVE "AMOUNT" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE 12       TO F-CBFIELDLENGTH.
            IF GLJRN-GLNUMBER (SUB-1) = " "
                MOVE " "  TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-020.
            MOVE GLJRN-AMOUNT (SUB-1) TO F-EDNAMEFIELDREC.
            PERFORM WRITE-FIELD-REC.
       SCROLL-020.
            MOVE "TRANSDESC"             TO F-FIELDNAME
            MOVE 9                       TO F-CBFIELDNAME
            MOVE GLJRN-LINE-DESC (SUB-1) TO F-NAMEFIELD
            MOVE 25                      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEBITAMT"           TO F-FIELDNAME
            MOVE 8                    TO F-CBFIELDNAME
            MOVE 11                   TO F-CBFIELDLENGTH
            MOVE GLJRN-AMOUNT (SUB-1) TO WS-JRNAMOUNT.
            IF GLJRN-AMOUNT (SUB-1) > 0
                MOVE WS-JRNAMOUNT TO F-EDNAMEFIELDFORTOTAL
                PERFORM WRITE-FIELD-FORTOTAL
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.

            MOVE "CREDITAMT" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE 11          TO F-CBFIELDLENGTH.
            IF GLJRN-AMOUNT (SUB-1) < 0
                MOVE WS-JRNAMOUNT TO F-EDNAMEFIELDFORTOTAL
                PERFORM WRITE-FIELD-FORTOTAL
            ELSE
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA.
            GO TO SCROLL-999.
       SCROLL-050.
           IF GLJRN-GLNUMBER (SUB-1) NOT = " "
               MOVE GLJRN-GLNUMBER (SUB-1) TO WS-GLNUMBER
               PERFORM READ-GLMASTER
               MOVE "ACCDESC"      TO F-FIELDNAME
               MOVE 7              TO F-CBFIELDNAME
               MOVE GL-DESCRIPTION TO F-NAMEFIELD
               MOVE 40             TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA.
       SCROLL-999.
           EXIT.
      *
       CANCEL-JOURNAL SECTION.
       CI-000.
             MOVE 1 TO SUB-1.
       CI-010.
             IF GLJRN-GLNUMBER (SUB-1) = " "
                IF GLJRN-AMOUNT (SUB-1) = 0
                 GO TO CI-900.
             PERFORM CANCEL-TRANSACTION.
             MOVE 1 TO SUB-1.
             GO TO CI-010.
       CI-900.
             PERFORM CLEAR-FIELDS.
             PERFORM DISPLAY-FORM.
             MOVE 2801 TO POS.
             MOVE " " TO WS-MESSAGE.
             DISPLAY WS-MESSAGE AT POS.
       CI-999.
             EXIT.
      *
       CANCEL-TRANSACTION SECTION.
       CAN-005.
           COMPUTE SUB-2 = SUB-1 + 1.
       CAN-010.
           IF SUB-2 > 50 
               GO TO CAN-090.
           IF GLJRN-GLNUMBER (SUB-2) = " "
                 MOVE " " TO GLJRN-GLNUMBER (SUB-1)
                             GLJRN-LINE-DESC (SUB-1)
                 MOVE 0   TO GLJRN-AMOUNT (SUB-1)
                 GO TO CAN-090.
             MOVE GLJRN-LINE-ITEMS (SUB-2) TO GLJRN-LINE-ITEMS (SUB-1).
             ADD 1 TO SUB-1 SUB-2.
             GO TO CAN-010.
       CAN-090.
             MOVE " " TO GLJRN-GLNUMBER (SUB-1)
                         GLJRN-LINE-DESC (SUB-1).
             MOVE 0   TO GLJRN-AMOUNT (SUB-1).
       CAN-999.
             EXIT.
      *
       EMPTY-LINE SECTION.
       EMPTY-LINE-005.
           MOVE SUB-1 TO SUB-3
           COMPUTE SUB-3 = SUB-3 + 1
           MOVE SUB-25 TO SUB-2
           COMPUTE SUB-1 = SUB-2 - 1.
           IF SUB-1 < 1
              MOVE 1 TO SUB-1.
       EMPTY-LINE-010.
           MOVE GLJRN-LINE-ITEMS (SUB-1) TO GLJRN-LINE-ITEMS (SUB-2)
           IF SUB-1 NOT > SUB-3
               GO TO EMPTY-LINE-090.
           SUBTRACT 1 FROM SUB-1 SUB-2
           GO TO EMPTY-LINE-010.
       EMPTY-LINE-090.
           MOVE " " TO GLJRN-GLNUMBER (SUB-1)
                       GLJRN-LINE-DESC (SUB-1)
           MOVE 0   TO GLJRN-AMOUNT (SUB-1).
       EMPTY-LINE-999.
           EXIT.
      *
       CLEAR-BODY SECTION.
       CLEAR-000.
            MOVE 0 TO F-INDEX.
       CLEAR-002.
            ADD 1 TO F-INDEX.
            IF F-INDEX > 15
               GO TO CLEAR-BODY-999.

            MOVE "ACCNUM" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AMOUNT" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE 12 TO F-CBFIELDLENGTH.
            MOVE " " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TRANSDESC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEBITAMT" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            MOVE " " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CREDITAMT" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            MOVE " " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            GO TO CLEAR-002.
       CLEAR-BODY-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
             MOVE 1 TO SUB-1.
       CF-010.
             MOVE " " TO GLJRN-GLNUMBER (SUB-1)
                         GLJRN-LINE-DESC (SUB-1).
             MOVE 0   TO GLJRN-AMOUNT (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 51
                 GO TO CF-010.
       CF-020.
             MOVE " " TO GLJRN-REFERENCE
                         GLJRN-MAIN-DESC
                         GLJRN-PERIOD
                         GLJRN-ACTION
                         GLJRN-COMPLETE.
             MOVE 0 TO   GLJRN-DATE.
       CF-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-010.
           OPEN I-O GL-LY-MASTER.
           IF WS-GLMAST-ST1 NOT = 0 
              MOVE "GLMASTER-LY FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GL-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GL-LY-ST1
              GO TO OPEN-010.
       OPEN-011.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0 
              MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO OPEN-011.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-012.

           PERFORM READ-PARAMETER.
           PERFORM ENTER-PERIOD-DATES.
       OPEN-015.
           OPEN I-O GLJRN-FILE.
           IF WS-GLJRN-ST1 NOT = 0 
              MOVE "GLJRN FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLJRN-ST1
              GO TO OPEN-015.
       OPEN-016.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "GlJrnlMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE GL-MASTER
                 GLJRN-FILE
                 GLPARAMETER-FILE.
           EXIT PROGRAM.
      *      STOP RUN.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldForTotal".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldRec".
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "NumberCheck".
       Copy "DisplayFormGLTopInfo".
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
       Copy "CTOSCobolAccept".
      * END-OF-JOB
