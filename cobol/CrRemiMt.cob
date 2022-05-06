        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrRemiMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           Copy "SelectCrMaster".
           Copy "SelectCrRemittance".
           Copy "SelectGlParameter".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCreditor.
           COPY ChlfdCrRemit.
           COPY ChlfdGlParam.

       WORKING-STORAGE SECTION.
       77  WS-TYPE            PIC X VALUE " ".
       77  WS-BUDGET          PIC S9(8)V99 VALUE 0.
       77  WS-YEAR-BU         PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN          PIC S9(8)V99 VALUE 0.
       77  WS-TOTALADJ        PIC S9(8)V99 VALUE 0.
       77  WS-TOTAL-GROSS     PIC S9(8)V99 VALUE 0.
       77  WS-DISC-PERC       PIC S9(2)V99 VALUE 0.
       77  WS-DISC-AMT        PIC S9(8)V99 VALUE 0.
       77  WS-TOTAL-NETT      PIC S9(8)V99 VALUE 0.
       77  WS-INQUIRY         PIC X(8) VALUE "CrMastIq".
       77  NEW-CRREMNO        PIC X VALUE " ".      
       77  WS-HEAD-VALID      PIC X VALUE " ".
       77  WS-SUB-VALID       PIC X VALUE " ".
       77  WS-END             PIC X VALUE " ".      
       77  WS-LINECHANGED     PIC X VALUE " ".      
       77  WS-REMIT-F-L       PIC X VALUE " ".      
       77  WS-NUMBER          PIC 9(7) VALUE 0.
       77  SUB-1-SAVE         PIC S9(5) VALUE 0.
       77  WS-BODY-LINE       PIC ZZ9.
       01  WS-REMIT-PERIOD.
           03  WS-REMI-YY          PIC 99.
           03  WS-REMI-MM          PIC 99.
       01  WS-MONTH-BUDGET.
           03  WS-MONTH-BU   PIC S9(8)V99 OCCURS 13.
       01  WS-REMI-STATUS.
           03  WS-REMI-ST1       PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-PAR-ST1        PIC 99.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1   PIC 99.
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
       CONTROL-005.
           MOVE 0310 TO POS.
           DISPLAY "*** CREDITOR REMITTANCE ENTRY / EDIT ****" AT POS
           MOVE 0610 TO POS
           DISPLAY 
           "ENTER YY=YEAR, MM=MONTH FOR THE REMITTANCE : [    ]" AT POS
           ADD 46 TO POS

           MOVE WS-REMIT-PERIOD TO CDA-DATA.
           MOVE 4         TO CDA-DATALEN.
           MOVE 3         TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-REMIT-PERIOD.

      *     ACCEPT WS-REMIT-PERIOD AT POS.
           IF W-ESCAPE-KEY = 3
               PERFORM END-OFF.
       CONTROL-006.
           MOVE "L" TO WS-REMIT-F-L.
           MOVE 0810 TO POS
           DISPLAY 
           "                  ENTER L=LOCAL, F=FOREIGN : [ ]" AT POS
           ADD 46 TO POS

           MOVE WS-REMIT-F-L TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-REMIT-F-L.

      *     ACCEPT WS-REMIT-F-L AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-005.
           IF WS-REMIT-F-L NOT = "F" AND NOT = "L"
              MOVE "FOREIGN / LOCAL FIELD MUST BE F OR L, RE-ENTER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO CONTROL-006.
       CONTROL-010.
           PERFORM CLEAR-SCREEN.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           PERFORM FILL-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO CRREM-RECORD.
            MOVE "N" TO NEW-CRREMNO
                        WS-END.
            MOVE 0 TO WS-TOTALADJ 
                      WS-TOTAL-GROSS
                      WS-DISC-PERC
                      WS-DISC-AMT.

            MOVE "                        " TO F-NAMEFIELD ALPHA-RATE.

            MOVE "PERIOD"         TO F-FIELDNAME
            MOVE 6                TO F-CBFIELDNAME
            MOVE WS-REMIT-PERIOD  TO F-NAMEFIELD
            MOVE 4                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "F-L"         TO F-FIELDNAME
            MOVE 3             TO F-CBFIELDNAME
            MOVE WS-REMIT-F-L  TO F-NAMEFIELD
            MOVE 1             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            PERFORM START-CRREMIT.
       GET-001.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "ACCNUM" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7        TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF ALPHA-RATE > SPACES
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO CRREM-ACC-NUMBER.
            IF SIGN-FOUND = 9
               MOVE 0 TO SIGN-FOUND
               GO TO GET-001.
            IF F-EXIT-CH = X"0C"
                MOVE WS-NUMBER TO CRREM-ACC-NUMBER
                PERFORM START-CRREMIT
                PERFORM READ-REMIT-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-FORM
               GO TO GET-000.
             IF F-NAMEFIELD = " "
               CLOSE CRREMIT-FILE
               CALL WS-INQUIRY USING WS-LINKAGE
               PERFORM OPEN-000
               PERFORM CLEAR-SCREEN
               PERFORM DISPLAY-FORM
               GO TO GET-000.
          IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-REMIT.
            IF NEW-CRREMNO = "Y"
                PERFORM CLSC-000 THRU CLSC-010
                MOVE "N" TO CRREM-COMPLETE
                PERFORM GET-003 THRU GET-005
             IF CR-NAME = "** UNKNOWN **"
                GO TO GET-001
             ELSE
                GO TO GET-999.
        GET-003.
            MOVE "PERIOD"         TO F-FIELDNAME
            MOVE 6                TO F-CBFIELDNAME
            MOVE WS-REMIT-PERIOD  TO F-NAMEFIELD
            MOVE 4                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "F-L"         TO F-FIELDNAME
            MOVE 3             TO F-CBFIELDNAME
            MOVE WS-REMIT-F-L  TO F-NAMEFIELD
            MOVE 1             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNUM"         TO F-FIELDNAME
            MOVE 6                TO F-CBFIELDNAME
            MOVE CRREM-ACC-NUMBER TO F-NAMEFIELD
            MOVE 7                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            PERFORM READ-CREDITOR.
            IF CR-NAME = "** UNKNOWN **"
               MOVE "INVALID ACCOUNT ENTERED, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "** UNKNOWN **" TO CR-ACC-EMAIL.
      *         GO TO GET-001.

            MOVE "NAME"           TO F-FIELDNAME
            MOVE 4                TO F-CBFIELDNAME
            MOVE CR-NAME          TO F-NAMEFIELD
            MOVE 40               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DISC-PERC"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE WS-DISC-PERC TO F-EDNAMEFIELDPERC
            MOVE 5            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PERC.

            MOVE "BAL-FROM-REMIT"     TO F-FIELDNAME
            MOVE 14                   TO F-CBFIELDNAME
            MOVE CRREM-BAL-FROM-REMIT TO F-EDNAMEFIELDREC
            MOVE 12                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.

            MOVE "COMPLETE"       TO F-FIELDNAME
            MOVE 8                TO F-CBFIELDNAME
            MOVE CRREM-COMPLETE   TO F-NAMEFIELD
            MOVE 1                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "EMAIL"          TO F-FIELDNAME
            MOVE 5                TO F-CBFIELDNAME.
            IF CR-ACC-EMAIL > " "
                MOVE CR-ACC-EMAIL                TO F-NAMEFIELD
            ELSE
                MOVE "**** NO EMAIL ON ACC ****" TO F-NAMEFIELD.
            MOVE 40               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            MOVE "OPENBAL"          TO F-FIELDNAME
            MOVE 7                  TO F-CBFIELDNAME.
            IF CRREM-BAL-LAST = 0
                MOVE " "            TO F-NAMEFIELD
                MOVE 12             TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
            ELSE
                MOVE CRREM-BAL-LAST TO F-EDNAMEFIELDREC
                MOVE 12             TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-REC.

            MOVE "PREV-PAID"         TO F-FIELDNAME
            MOVE 9                   TO F-CBFIELDNAME.
            IF CRREM-PREV-PAID = 0
                MOVE " "             TO F-NAMEFIELD
                MOVE 12              TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
            ELSE
                MOVE CRREM-PREV-PAID TO F-EDNAMEFIELDREC
                MOVE 12              TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-REC.

            MOVE "BAL-NOW"           TO F-FIELDNAME
            MOVE 7                   TO F-CBFIELDNAME.
            IF CRREM-BAL-NOW = 0
                MOVE " "             TO F-NAMEFIELD
                MOVE 12              TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
            ELSE
                MOVE CRREM-BAL-NOW   TO F-EDNAMEFIELDREC
                MOVE 12              TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-REC.

            MOVE 1 TO SUB-1 F-INDEX.
            PERFORM CHECK-NO-OF-LINES.
       GET-006.
            IF CRREM-DESC (SUB-1) = " "
                 MOVE 1 TO SUB-1 F-INDEX
                 GO TO GET-050.
            MOVE "DESC"             TO F-FIELDNAME
            MOVE 4                  TO F-CBFIELDNAME
            MOVE CRREM-DESC (SUB-1) TO F-NAMEFIELD60
            MOVE 50                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA60.

            MOVE "AMT"             TO F-FIELDNAME
            MOVE 3                 TO F-CBFIELDNAME
            MOVE CRREM-AMT (SUB-1) TO F-EDNAMEFIELDREC
            MOVE 12                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 16
                GO TO GET-006.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-050.
            PERFORM ADD-TOTALS.

            MOVE "TOTALADJ"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-TOTALADJ TO F-EDNAMEFIELDREC
            MOVE 12          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.

            MOVE "TOTALGROSS"  TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE WS-TOTAL-GROSS TO F-EDNAMEFIELDREC
            MOVE 12            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.

            MOVE "DISC-PERC"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE WS-DISC-PERC TO F-EDNAMEFIELDPERC
            MOVE 5            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PERC.

            MOVE WS-DISC-AMT  TO CRREM-DISC-AMT
            MOVE "DISC-AMT"   TO F-FIELDNAME
            MOVE 8            TO F-CBFIELDNAME
            MOVE WS-DISC-AMT  TO F-EDNAMEFIELDREC
            MOVE 12           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.

            MOVE "TOTAL-NETT"  TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE WS-TOTAL-NETT TO F-EDNAMEFIELDREC
            MOVE 12            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.
       GET-055.
            GO TO GET-999.
       GET-060.
            PERFORM ATS-020.
            
            MOVE "TOTALGROSS"  TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE WS-TOTAL-GROSS TO F-EDNAMEFIELDREC
            MOVE 12             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.

            MOVE "DISC-AMT"   TO F-FIELDNAME
            MOVE 8            TO F-CBFIELDNAME
            MOVE WS-DISC-AMT  TO F-EDNAMEFIELDREC
            MOVE 12           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.

            MOVE "TOTAL-NETT"  TO F-FIELDNAME
            MOVE 10            TO F-CBFIELDNAME
            MOVE WS-TOTAL-NETT TO F-EDNAMEFIELDREC
            MOVE 12            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.

            IF CRREM-BAL-FROM-REMIT NOT = WS-TOTAL-NETT
               MOVE 
           "THE AMOUNT ENTERED IS NOT = THE REMITTANCE AMT GENERATED."
               TO WS-MESSAGE
               PERFORM ERROR-000.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                  GO TO FILL-999.
        FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "OPENBAL"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 CRREM-BAL-LAST.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CRREM-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CRREM-RECORD
               PERFORM READ-REMIT-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CRREM-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CRREM-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.
               
            PERFORM ADD-TOTALS.
            PERFORM GET-005.
        FILL-025.
            MOVE 3010 TO POS
            DISPLAY 
            "NB. PAYMENTS SHOULD ALWAYS BE ENTERED IN AS A NEGATIVE."
             AT POS.
        
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PREV-PAID"  TO F-FIELDNAME.
            MOVE 9            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 CRREM-PREV-PAID.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-025.
               
            PERFORM ERROR-020.
            PERFORM ADD-TOTALS.
            PERFORM GET-005.
            
            IF F-EXIT-CH = X"1D"
                GO TO FILL-030
            ELSE
                GO TO FILL-040.
        FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BAL-NOW"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 CRREM-BAL-NOW.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-030.
               
            PERFORM ADD-TOTALS.
            PERFORM GET-005.
        FILL-040.
            MOVE 1 TO SUB-1 F-INDEX.
            PERFORM SCROLL-PREVIOUS.
            MOVE 1 TO SUB-1 F-INDEX.
            MOVE "N" TO WS-LINECHANGED.

            PERFORM GET-050.
        FILL-045.
            MOVE 2504 TO POS.
            DISPLAY "BODY LINE:" AT POS.
            ADD 11 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.

            MOVE "                          " TO F-NAMEFIELD.
            MOVE "DESC"        TO F-FIELDNAME.
            MOVE 4             TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 50            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA60.
            
            IF CRREM-DESC (SUB-1) NOT = " "
                MOVE "Y" TO WS-LINECHANGED.
            
            MOVE F-NAMEFIELD60 TO CRREM-DESC (SUB-1).
            PERFORM WRITE-FIELD-ALPHA60.
            
      * <NEXT-PAGE> KEY
            IF F-EXIT-CH = X"0C"
                PERFORM SCROLL-NEXT-PAGE
                GO TO FILL-045.
      * <SCROLL-UP> KEY
            IF F-EXIT-CH = X"05"
                PERFORM SCROLL-PREVIOUS
                GO TO FILL-045.
      * <DOWN-ARROW> KEY
            IF F-EXIT-CH = X"0B" 
             IF CRREM-DESC (SUB-1) = " "
                GO TO FILL-045.
           IF F-EXIT-CH = X"0B"
             IF CRREM-DESC (SUB-1) NOT = " "
              IF F-INDEX = 15
                PERFORM SCROLL-NEXT
                GO TO FILL-045
              ELSE
                ADD 1 TO SUB-1 F-INDEX
                GO TO FILL-045.
      * 'ESC' = X"07"; 'CODE-CANCEL' = X"87"
            IF F-EXIT-CH = X"07" OR = X"87"
             IF CRREM-DESC (SUB-1) = "  "
                GO TO FILL-045.
            IF F-EXIT-CH = X"07"
                MOVE "TO DELETE A LINE-ITEM PRESS 'ALT-ESC'"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-045.
            IF F-EXIT-CH = X"87"
                MOVE SUB-1 TO SUB-7
                PERFORM CANCEL-TRANSACTION
                SUBTRACT 1 FROM SUB-25
                MOVE 1 TO SUB-1
                          F-INDEX
                PERFORM SCROLL-NEXT
                PERFORM SCROLL-PREVIOUS
              IF SUB-25 > 16
                SUBTRACT 10 FROM SUB-25
                MOVE SUB-25 TO SUB-1
                PERFORM SCROLL-NEXT
                ADD 10 TO SUB-25
                PERFORM GET-050
                GO TO FILL-045
              ELSE
                PERFORM GET-050
                GO TO FILL-045.
            
            IF F-EXIT-CH = X"01"
             IF SUB-1 = 1
               GO TO FILL-030.
            IF F-EXIT-CH = X"01"
             IF SUB-1 > 1
                SUBTRACT 1 FROM SUB-1 F-INDEX
              IF F-INDEX < 1
                MOVE 1 TO F-INDEX
                GO TO FILL-045
              ELSE
                GO TO FILL-045.
                
            IF CRREM-DESC (SUB-1) = " "
                 MOVE 1 TO SUB-1 F-INDEX
                 GO TO FILL-165.
      * TAB CHARACTER
            IF F-EXIT-CH = X"09"
             IF F-NAMEFIELD = CRREM-DESC (SUB-1)
                PERFORM ERROR-020
                GO TO FILL-165
             ELSE
                MOVE CRREM-DESC (SUB-1) TO F-NAMEFIELD60
                MOVE 50                 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA60
                PERFORM ERROR-020
                GO TO FILL-165.

            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-045.
        FILL-100.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "AMT"        TO F-FIELDNAME.
            MOVE 3            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 CRREM-AMT (SUB-1).
            PERFORM WRITE-FIELD-REC.

            PERFORM GET-050.

            IF F-EXIT-CH = X"01"
               GO TO FILL-045.
            IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-100.

            ADD 1 TO SUB-1 F-INDEX.
            IF WS-LINECHANGED = "N"
                MOVE SUB-1 TO SUB-25.
            MOVE "N" TO WS-LINECHANGED.
            MOVE 0 TO SUB-3.
            IF SUB-1 > 40
                MOVE 40 TO SUB-1 SUB-25
                MOVE "40 LINES ARE UP, PRESS 'ESC' TO TAB."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-165.
                
            IF F-INDEX < 16
                GO TO FILL-045.
            SUBTRACT 1 FROM SUB-1. 
            PERFORM SCROLL-NEXT.
            GO TO FILL-045.
        FILL-165.
            IF CRREM-BAL-FROM-REMIT NOT = WS-TOTAL-NETT
               MOVE 
           "THE AMOUNT ENTERED IS NOT = THE REMITTANCE AMT GENERATED"
               TO WS-MESSAGE
               PERFORM ERROR-000
            ELSE
               PERFORM ERROR-020.

               MOVE 2910 TO POS
               DISPLAY
           "Press <GO> To File The Current Input, <PgDn> for Next."
               AT POS.

            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TOTAL-NETT" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.

            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CRREM-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CRREM-RECORD
               PERFORM READ-REMIT-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               MOVE 1 TO SUB-1 F-INDEX
               PERFORM ERROR1-020
               PERFORM SCROLL-PREVIOUS
               GO TO FILL-045.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CRREM-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B" AND NOT = X"1D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-165.

            
            IF F-EXIT-CH = X"1D"
                GO TO FILL-170
            ELSE
                GO TO FILL-175.
        FILL-170.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DISC-AMT" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12         TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 WS-DISC-AMT
                                 CRREM-DISC-AMT
            PERFORM WRITE-FIELD-REC.

            IF F-EXIT-CH = X"01"
               MOVE 1 TO SUB-1 F-INDEX
               PERFORM ERROR1-020
               GO TO FILL-165.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B" AND NOT = X"1D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-170.
               
            PERFORM GET-060.
            
            IF F-EXIT-CH = X"1D"
                GO TO FILL-175
            ELSE
                GO TO FILL-165.
        FILL-175.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COMPLETE" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12         TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO CRREM-COMPLETE.
            
            IF CRREM-COMPLETE NOT = "N" AND NOT = "P" AND NOT = "Y"
               MOVE "THIS FIELD MUST BE EITHER N, P OR Y, RE-ENTER."
                  TO WS-MESSAGE
                  PERFORM ERROR-MESSAGE
                  GO TO FILL-175.

            IF F-EXIT-CH = X"01"
               MOVE 1 TO SUB-1 F-INDEX
               PERFORM ERROR1-020
               GO TO FILL-165.
             IF F-EXIT-CH = X"1D"
               GO TO FILL-190.
           IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-175.
               
            GO TO FILL-165.
        FILL-190.
            MOVE "                    " TO F-NAMEFIELD.

            MOVE "BAL-FROM-REMIT"     TO F-FIELDNAME
            MOVE 14                   TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            MOVE 12                   TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 CRREM-BAL-FROM-REMIT.
            PERFORM WRITE-FIELD-REC.

            IF F-EXIT-CH = X"01"
               MOVE 1 TO SUB-1 F-INDEX
               PERFORM ERROR1-020
               GO TO FILL-175.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CRREM-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CRREM-RECORD
               PERFORM READ-REMIT-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-190.
               
            GO TO FILL-165.
       FILL-999.
             EXIT.
      *
       ADD-TOTALS SECTION.
       ATS-001.
            MOVE 0 TO WS-TOTALADJ WS-TOTAL-GROSS WS-TOTAL-NETT.
            MOVE 1 TO SUB-10.
       ATS-002.
            COMPUTE CRREM-BAL-NOW = CRREM-BAL-LAST + CRREM-PREV-PAID.
       ATS-005.
            COMPUTE WS-TOTALADJ = WS-TOTALADJ + CRREM-AMT (SUB-10).
            ADD 1 TO SUB-10.
            IF SUB-10 < 41
                GO TO ATS-005.
            MOVE 1 TO SUB-10.
       ATS-010.
            COMPUTE WS-TOTAL-GROSS = CRREM-BAL-NOW + WS-TOTALADJ.
 
             IF CRREM-DISC-AMT NOT = 0
               GO TO ATS-020
             ELSE
               COMPUTE WS-DISC-AMT ROUNDED =
                  WS-TOTAL-GROSS * WS-DISC-PERC / 100.
       ATS-020.
            COMPUTE WS-TOTAL-NETT = WS-TOTAL-GROSS - WS-DISC-AMT.
       ATS-999.
            EXIT.
      *
       DELETE-CRREM-RECORD SECTION.
       DSR-000.
            IF NEW-CRREMNO = "Y"
               GO TO DSR-999.
       DSR-010.
            DELETE CRREMIT-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-REMI-ST1 NOT = 0
              MOVE "CRREM RECORD BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-REMI-ST1
              GO TO DSR-010. 
       DSR-999.
            EXIT. 
      *
       RELEASE-CRREM-RECORD SECTION.
       REL-000.
           UNLOCK CRREMIT-FILE.
       REL-999.
           EXIT.
      *
       REWRITE-CRREM-RECORD SECTION.
       RSR-005.
          IF NEW-CRREMNO = "Y"
              GO TO RSR-020.
       RSR-010.
          REWRITE CRREM-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-REMI-ST1 NOT = 0
              MOVE "CRREMI RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-REMI-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE CRREM-KEY TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-REMI-ST1
              GO TO RSR-020.
          GO TO RSR-999.
       RSR-020.
          WRITE CRREM-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-REMI-ST1 NOT = 0
              MOVE "CRREMI RECORD BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-REMI-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE CRREM-KEY TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-REMI-ST1
              GO TO RSR-010.
       RSR-999.
          EXIT.
      *
       READ-REMIT SECTION.
       R-GL-000.
             MOVE "Y"              TO NEW-CRREMNO.
             
             MOVE WS-REMI-YY       TO CRREM-YY
             MOVE WS-REMI-MM       TO CRREM-MM
             MOVE WS-REMIT-F-L     TO CRREM-F-L
             MOVE CRREM-ACC-NUMBER TO WS-NUMBER.
             START CRREMIT-FILE KEY NOT < CRREM-KEY
                INVALID KEY NEXT SENTENCE.
             IF WS-REMI-ST1 NOT = 0
                GO TO R-GL-999.
       R-GL-010.
             READ CRREMIT-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-REMI-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FORM
                MOVE "Y"       TO NEW-CRREMNO
                MOVE WS-NUMBER TO CRREM-ACC-NUMBER
                GO TO R-GL-999.
             IF WS-REMI-ST1 NOT = 0
                MOVE "CRREMIT RECORD BUSY ON READ-LOCK, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO R-GL-010.
                
             IF CRREM-F-L NOT = WS-REMIT-F-L
                PERFORM CLEAR-FORM
                MOVE "Y"       TO NEW-CRREMNO
                MOVE WS-NUMBER TO CRREM-ACC-NUMBER
                GO TO R-GL-999.

             MOVE "N"            TO NEW-CRREMNO.
             MOVE CRREM-DISC-AMT TO WS-DISC-PERC.
       R-GL-999.
             EXIT.
      *
       START-CRREMIT SECTION.
       GL-GL-000.
              MOVE WS-REMI-YY   TO CRREM-YY
              MOVE WS-REMI-MM   TO CRREM-MM
              MOVE WS-REMIT-F-L TO CRREM-F-L
              MOVE WS-NUMBER    TO CRREM-ACC-NUMBER.
              START CRREMIT-FILE KEY NOT LESS CRREM-KEY
                  INVALID KEY NEXT SENTENCE.
           IF WS-REMI-ST1 NOT = 0
               MOVE 88 TO WS-REMI-ST1
              MOVE "CRREMIT-FILE BAD START, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
       GL-GL-999.
             EXIT.
      *
       READ-REMIT-NEXT SECTION.
       RSN-000.
           IF WS-REMI-ST1 = 88
               MOVE 0 TO CRREM-ACC-NUMBER
                            WS-NUMBER
               MOVE "Y" TO WS-END
               PERFORM CLEAR-FORM
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
       RSN-005. 
           READ CRREMIT-FILE NEXT WITH LOCK
             AT END 
               MOVE 0 TO CRREM-ACC-NUMBER
                            WS-NUMBER
               MOVE "Y" TO WS-END
               PERFORM CLEAR-FORM
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-REMI-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 
               "CRREMIT-FILE BUSY ON READ-NEXT-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-005.
           IF WS-REMI-ST1 NOT = 0
              MOVE "CR-REMI BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-REMI-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-REMI-ST1
              PERFORM START-CRREMIT
              GO TO RSN-005.
                
             IF CRREM-F-L NOT = WS-REMIT-F-L
                GO TO RSN-005.
             IF CRREM-YY NOT = WS-REMI-YY
               MOVE 0 TO CRREM-ACC-NUMBER
                            WS-NUMBER
               MOVE "Y" TO WS-END
               PERFORM CLEAR-FORM
               MOVE
               "NEXT PAGE SEQUENCE YEAR NOT =, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
             IF CRREM-MM NOT = WS-REMI-MM
               MOVE 0 TO CRREM-ACC-NUMBER
                            WS-NUMBER
               MOVE "Y" TO WS-END
               PERFORM CLEAR-FORM
               MOVE
              "NEXT PAGE SEQUENCE MONTHS NOT =, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.

           MOVE "N"              TO NEW-CRREMNO.
           MOVE CRREM-ACC-NUMBER TO WS-NUMBER.
           MOVE CRREM-DISC-AMT   TO WS-DISC-AMT.
       RSN-999.
             EXIT.
      *
       READ-PARAMETER-RECORD SECTION.
       RPR-000.
           MOVE 1 TO GLPA-KEY.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       RPR-010.
           READ GLPARAMETER-FILE
                 INVALID KEY NEXT SENTENCE.
           IF WS-PAR-ST1 = 23 OR 35 OR 49
                MOVE "GLPARAMETER RECORD NOT THERE, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RPR-999.
           IF WS-PAR-ST1 NOT = 0
                MOVE 0 TO WS-PAR-ST1
                MOVE "GLPARAMETER RECORD BUSY, PRESS 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RPR-010.
       RPR-999.
            EXIT.
      *
       READ-CREDITOR SECTION.
       RCR-000.
           MOVE WS-NUMBER TO CR-ACCOUNT-NUMBER
           START CREDITOR-MASTER KEY NOT < CR-KEY
               INVALID KEY NEXT SENTENCE.
       RCR-010.
           READ CREDITOR-MASTER
                INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE "** UNKNOWN **" TO CR-NAME
                GO TO RCR-999.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITOR BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RCR-010.
            MOVE CR-SETT-DISC TO WS-DISC-PERC.
       RCR-999.
             EXIT.
      *
       SCROLLING SECTION.
       SCROLL-010.
            MOVE "DESC"              TO F-FIELDNAME
            MOVE 4                   TO F-CBFIELDNAME
            MOVE CRREM-DESC (SUB-1)  TO F-NAMEFIELD60
            MOVE 50                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA60.

            MOVE "AMT"              TO F-FIELDNAME
            MOVE 3                  TO F-CBFIELDNAME
            MOVE CRREM-AMT (SUB-1)  TO F-EDNAMEFIELDREC
            MOVE 12                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-REC.
       SCROLL-999.
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
            IF SUB-1 > 26
               MOVE 26 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 40  
                GO TO NEXT-030.
            IF F-INDEX < 16
                GO TO NEXT-010.
       NEXT-030.
            SUBTRACT 15 FROM SUB-1.
            IF SUB-1 > 25
             IF SUB-25 > 25
               COMPUTE F-INDEX = 15 - (40 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX. 
            IF F-INDEX > 15
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 2504 TO POS.
            DISPLAY "BODY LINE:" AT POS.
            ADD 11 TO POS.
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
            IF SUB-1 > 26
               MOVE 26 TO SUB-1.
       NEXT-PAGE-010.
            PERFORM SCROLLING.
       NEXT-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 40  
                GO TO NEXT-PAGE-030.
            IF F-INDEX < 16
                GO TO NEXT-PAGE-010.
       NEXT-PAGE-030.
            SUBTRACT 15 FROM SUB-1.
            IF SUB-1 > 25
             IF SUB-25 > 25
               COMPUTE F-INDEX = 15 - (40 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX.
            IF SUB-1 > 40
               MOVE 26 TO SUB-1.
            IF F-INDEX > 15
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 2504 TO POS.
            DISPLAY "BODY LINE:" AT POS.
            ADD 11 TO POS.
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
            IF SUB-1 > 40
                GO TO PREV-030.
            IF F-INDEX < 16
                GO TO PREV-010.
       PREV-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 15 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

             MOVE 2504 TO POS.
             DISPLAY "BODY LINE: " AT POS.
             ADD 11 TO POS.
             MOVE SUB-1 TO WS-BODY-LINE.
             DISPLAY WS-BODY-LINE AT POS.
       PREV-999.
             EXIT.
      *
       CLEAR-BODY SECTION.
       CLEAR-000.
            MOVE 0 TO F-INDEX.
       CLEAR-002.
            ADD 1 TO F-INDEX.
            IF F-INDEX > 15
               GO TO CLEAR-BODY-999.

            MOVE "DESC"     TO F-FIELDNAME
            MOVE 4          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD60
            MOVE 50         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA60.

            MOVE "AMT"    TO F-FIELDNAME
            MOVE 3        TO F-CBFIELDNAME
            MOVE " "      TO F-NAMEFIELD
            MOVE 12       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            GO TO CLEAR-002.
       CLEAR-BODY-999.
             EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
            MOVE 1 TO SUB-1.
       CLSC-005.
            MOVE 0   TO CRREM-AMT (SUB-1).
            MOVE " " TO CRREM-DESC (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 41
                 GO TO CLSC-005.
            MOVE 1 TO SUB-1.
       CLSC-010.
            MOVE 0 TO  CRREM-BAL-LAST
                       CRREM-PREV-PAID
                       CRREM-BAL-NOW
                       CRREM-BAL-FROM-REMIT.
           MOVE " " TO CRREM-COMPLETE.
       CLSC-500.
            PERFORM RELEASE-CRREM-RECORD.
       CLSC-999.
           EXIT.
      *
       CHECK-NO-OF-LINES SECTION.
       CNOL-000.
            MOVE 1 TO SUB-1.
       CNOL-005.
            IF CRREM-DESC (SUB-1) NOT = " "
               ADD 1 TO SUB-1
             IF SUB-1 < 41
                 GO TO CNOL-005.
            MOVE SUB-1 TO SUB-25.
            MOVE 1 TO SUB-1.
       CNOL-999.
           EXIT.
      *
       CANCEL-TRANSACTION SECTION.
       CAN-005.
            COMPUTE SUB-2 = SUB-1 + 1.
       CAN-010.
            IF SUB-2 > 40
               MOVE 40 TO SUB-1 SUB-2
               GO TO CAN-090.
             IF CRREM-DESC (SUB-2) = " "
                 MOVE " " TO CRREM-DESC (SUB-1)
                 MOVE 0   TO CRREM-AMT (SUB-1)
                 GO TO CAN-090.
             MOVE CRREM-DESC (SUB-2)TO CRREM-DESC (SUB-1)
             MOVE CRREM-AMT (SUB-2) TO CRREM-AMT (SUB-1)
             ADD 1 TO SUB-1 SUB-2
             GO TO CAN-010.
       CAN-090.
             MOVE " " TO CRREM-DESC (SUB-1)
             MOVE 0   TO CRREM-AMT (SUB-1).
       CAN-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O CRREMIT-FILE.
           IF WS-REMI-ST1 NOT = 0
               MOVE "CRREMIT-FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-CRREMIT TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
        OPEN-001.
            OPEN I-O GLPARAMETER-FILE.
            IF WS-PAR-ST1 NOT = 0
               MOVE 0 TO WS-PAR-ST1
               MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-001.
            PERFORM READ-PARAMETER-RECORD.
            IF WS-PAR-ST1 = 23 OR 35 OR 49
               CLOSE GLPARAMETER-FILE
               PERFORM END-OFF.
            CLOSE GLPARAMETER-FILE.
       OPEN-004.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-004.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *      ACCEPT WS-DATE FROM DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrRemiMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CRREMIT-FILE
                 CREDITOR-MASTER.
           EXIT PROGRAM.
      *      STOP RUN.
       END-999.
          EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldAlpha60".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAlpha60".
       Copy "WriteFieldDate".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldRec".
       Copy "WriteFieldPerc".
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "NumberCheck".
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
