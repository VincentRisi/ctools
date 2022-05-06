        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlNoChMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlMaster".
        Copy "SelectGlMasterLy".
        Copy "SelectGlTrans".
        Copy "SelectGlTransLy".
        Copy "SelectCrJrn".
        Copy "SelectGlParameter".
        Copy "SelectSlDaily".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdGlMast.
           COPY ChlfdGlMastLy.
           COPY ChlfdGlTrans.
           COPY ChlfdGlTransLy.
           COPY ChlfdCrJrn.
           COPY ChlfdGlParam.
           COPY ChlfdDaily.

       WORKING-STORAGE SECTION.
       77  WS-NEWGL-NO           PIC X VALUE " ".      
       77  WS-END                PIC X VALUE " ".      
       77  WS-INVALID            PIC X VALUE " ".      
       77  WS-GL-NUMBER          PIC X(12) VALUE " ".
       77  WS-NEWGL-NUMBER       PIC X(12) VALUE " ".
       77  WS-CREATE-ONE-ACC     PIC X VALUE " ".      
       77  WS-OLDGL-NUMBER       PIC X(12) VALUE " ".
       77  WS-BALANCE            PIC S9(8)V99.
       77  WS-OPEN-PER-BAL       PIC S9(8)V99.
       77  WS-OPEN-YEAR-BAL      PIC S9(8)V99.
       77  WS-LAST-YEAR-BAL      PIC S9(8)V99.
       77  WS-BUDGET             PIC S9(8)V99 VALUE 0.
       77  WS-YEAR-BU            PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN             PIC S9(8)V99 VALUE 0.
       77  WS-TOTALMV            PIC S9(8)V99 VALUE 0.
       77  WS-TOTALBU            PIC S9(8)V99 VALUE 0.
       77  WS-TOTALVA            PIC S9(8)V99 VALUE 0.
       77  WS-TOTALLY            PIC S9(8)V99 VALUE 0.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1      PIC 99.
       01  WS-GL-LY-STATUS.
           03  WS-GL-LY-ST1       PIC 99.
       01  WS-GLTRANS-STATUS.
           03  WS-GLTRANS-ST1     PIC 99.
       01  WS-GLTRANS-LY-STATUS.
           03  WS-GLTRANS-LY-ST1  PIC 99.
       01  WS-CRJRN-STATUS.
           03  WS-CRJRN-ST1       PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1       PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       01  WS-PERIOD-NAMES.
         02  WS-PERIODS OCCURS 12.
           03  WS-PER            PIC S9(8)V99.
           03  WS-PER-BU         PIC S9(8)V99.
           03  WS-LAST-PER       PIC S9(8)V99.
       
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
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           IF WS-INVALID NOT = "Y"
              PERFORM READ-GLMASTLY
              PERFORM READ-CRJRN
              PERFORM READ-GLMAST-TRANS
              PERFORM READ-GLMASTLY-TRANS.
           PERFORM ERROR1-020.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO GL-RECORD.
            MOVE "N" TO WS-NEWGL-NO
                        WS-INVALID
                        WS-END.
       GET-001.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "OLDNUMBER" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 12          TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF ALPHA-RATE > SPACES
                PERFORM NUMBER-CHECK
                MOVE WS-GLNO-CHECK TO GL-NUMBER.
            IF F-EXIT-CH = X"0C"
                MOVE WS-GL-NUMBER TO GL-NUMBER
                PERFORM START-GLMAST
                PERFORM READ-GLMAST-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-FORM
               GO TO GET-000.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO GET-000.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-GLMAST.
            IF WS-NEWGL-NO = "Y"
               MOVE "YOU CAN ONLY CHANGE AN EXISTING GL-MAST NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-000.
        GET-003.
            MOVE "OLDNUMBER" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            MOVE GL-NUMBER   TO F-NAMEFIELD.
            MOVE 12          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            MOVE "DESCRIPTION"  TO F-FIELDNAME.
            MOVE 11             TO F-CBFIELDNAME.
            MOVE GL-DESCRIPTION TO F-NAMEFIELD.
            MOVE 40             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PB"   TO F-FIELDNAME.
            MOVE 2      TO F-CBFIELDNAME.
            MOVE GL-P-B TO F-NAMEFIELD.
            MOVE 1      TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CRDATE"     TO F-FIELDNAME.
            MOVE 6            TO F-CBFIELDNAME.
            MOVE GL-DATE      TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BALANCE"  TO F-FIELDNAME.
            MOVE 7          TO F-CBFIELDNAME.
            MOVE GL-BALANCE TO F-EDNAMEFIELDREC.
            MOVE 12         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "OPENPERBAL"    TO F-FIELDNAME.
            MOVE 10              TO F-CBFIELDNAME.
            MOVE GL-OPEN-PER-BAL TO F-EDNAMEFIELDREC.
            MOVE 12              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "OPENYEARBAL"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            MOVE GL-OPEN-YEAR-BAL TO F-EDNAMEFIELDREC.
            MOVE 12               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "LASTYEARBAL"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            MOVE GL-LAST-YEAR-BAL TO F-EDNAMEFIELDREC.
            MOVE 12               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE 1 TO SUB-1 F-INDEX.
       GET-006.
            MOVE "PER"          TO F-FIELDNAME.
            MOVE 3              TO F-CBFIELDNAME.
            MOVE GL-PER (SUB-1) TO F-EDNAMEFIELDREC.
            MOVE 12             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
                GO TO GET-006.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-020.
            MOVE "PERBU"           TO F-FIELDNAME.
            MOVE 5                 TO F-CBFIELDNAME.
            MOVE GL-PER-BU (SUB-1) TO F-EDNAMEFIELDREC.
            MOVE 12                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
                GO TO GET-020.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-025.
            IF SUB-1 = 1
               MOVE 0 TO WS-TOTALVA.

            MOVE "VAR" TO F-FIELDNAME.
            MOVE 3     TO F-CBFIELDNAME.
            IF SUB-1 NOT > GLPA-CURRENT-GLPER
               COMPUTE WS-MARGIN = GL-PER (SUB-1) - GL-PER-BU (SUB-1)
               MOVE WS-MARGIN TO F-EDNAMEFIELDREC
               ADD WS-MARGIN TO WS-TOTALVA
            ELSE
               MOVE 0 TO F-EDNAMEFIELDREC.
            MOVE 12   TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
                GO TO GET-025.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-030.
            MOVE "LASTPER"           TO F-FIELDNAME.
            MOVE 7                   TO F-CBFIELDNAME.
            MOVE GL-LAST-PER (SUB-1) TO F-EDNAMEFIELDREC.
            MOVE 12                  TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
                GO TO GET-030.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-050.
            PERFORM ADD-TOTALS.

            MOVE "TOTALMV"  TO F-FIELDNAME.
            MOVE 7          TO F-CBFIELDNAME.
            MOVE WS-TOTALMV TO F-EDNAMEFIELDREC.
            MOVE 12         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "TOTALBU"  TO F-FIELDNAME.
            MOVE 7          TO F-CBFIELDNAME.
            MOVE WS-TOTALBU TO F-EDNAMEFIELDREC.
            MOVE 12         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "TOTALVA"  TO F-FIELDNAME.
            MOVE 7          TO F-CBFIELDNAME.
            MOVE WS-TOTALVA TO F-EDNAMEFIELDREC.
            MOVE 12         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "TOTALLY"  TO F-FIELDNAME.
            MOVE 7          TO F-CBFIELDNAME.
            MOVE WS-TOTALLY TO F-EDNAMEFIELDREC.
            MOVE 12         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.
       GET-500.
            MOVE GL-NUMBER         TO WS-OLDGL-NUMBER.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NEWNUMBER"       TO F-FIELDNAME
            MOVE 9                 TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 12                TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD       TO ALPHA-RATE.
            IF ALPHA-RATE > SPACES
               PERFORM NUMBER-CHECK
               MOVE WS-GLNO-CHECK TO GL-NUMBER.
        GET-501.
            MOVE "NEWNUMBER" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            MOVE GL-NUMBER   TO F-NAMEFIELD.
            MOVE 12          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            
            IF F-EXIT-CH = X"01" OR = X"07"
               PERFORM CLEAR-FORM
               PERFORM DISPLAY-FORM
               GO TO GET-000.
            IF F-NAMEFIELD = "    "
               MOVE "YOU MUST ENTER A NEW GL-MAST NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-500.

            MOVE GL-BALANCE        TO WS-BALANCE
            MOVE GL-OPEN-PER-BAL   TO WS-OPEN-PER-BAL
            MOVE GL-OPEN-YEAR-BAL  TO WS-OPEN-YEAR-BAL
            MOVE GL-LAST-YEAR-BAL  TO WS-LAST-YEAR-BAL.
            MOVE 0 TO SUB-1.
       GET-505.
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
               MOVE GL-PER (SUB-1)      TO WS-PER (SUB-1)
               MOVE GL-PER-BU (SUB-1)   TO WS-PER-BU (SUB-1)
               MOVE GL-LAST-PER (SUB-1) TO WS-LAST-PER (SUB-1)
               GO TO GET-505.
            MOVE 1 TO SUB-1.
       GET-550.
            PERFORM CLEAR-010.
            MOVE 3010 TO POS
            DISPLAY "ENTER Y=MERGE TWO A/C'S, N=RENAME A/C ONLY" AT POS
            MOVE 2910 TO POS
            DISPLAY "DO YOU WISH TO CREATE ONE A/C FROM TWO : [ ]"
              AT POS
            ADD 42 TO POS
            ACCEPT WS-CREATE-ONE-ACC AT POS.
            IF WS-CREATE-ONE-ACC NOT = "N" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-550.
               
            PERFORM ERROR1-020
            PERFORM ERROR-020.
               
            MOVE WS-GLNO-CHECK TO GL-NUMBER 
                                  WS-NEWGL-NUMBER.
            IF WS-CREATE-ONE-ACC = "Y"
               PERFORM READ-GLMAST.
               
            IF WS-CREATE-ONE-ACC = "N"
                MOVE "Y" TO WS-NEWGL-NO.
            PERFORM REWRITE-GL-RECORD.
            IF WS-INVALID = "Y"
                MOVE "THIS TRY HAS BEEN ABORTED, PRESS 'Esc' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM CLEAR-FORM
                GO TO GET-999.
            MOVE WS-OLDGL-NUMBER TO GL-NUMBER.
            PERFORM READ-GLMAST.
            PERFORM DELETE-GL-RECORD.
       GET-999.
            EXIT.
      *
       READ-CRJRN SECTION.
       RCRJRN-000.
            MOVE 2910 TO POS
            DISPLAY "Changing CrJrn GLTrans to new ACCOUNT.       "
            AT POS.
            MOVE SPACES TO CRJRN-REFERENCE
            MOVE 0      TO CRJRN-TRANS
                           CRJRN-TYPE.
            START CRJRN-FILE KEY NOT < CRJRN-KEY
                 INVALID KEY NEXT SENTENCE.
            IF WS-CRJRN-ST1 NOT = 0
               MOVE "BAD START TO CRJRN READ, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRJRN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRJRN-ST1
               GO TO RCRJRN-999.
       RCRJRN-002.
            READ CRJRN-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-CRJRN-ST1 = 10
               GO TO RCRJRN-999.
            IF WS-CRJRN-ST1 NOT = 0
               MOVE "CRJRN FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CRJRN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CRJRN-ST1
               GO TO RCRJRN-002.
            MOVE 0 TO SUB-1.
       RCRJRN-003.
            ADD 1 TO SUB-1.
            IF SUB-1 > 10
               GO TO RCRJRN-005.
            IF CRJRN-GLACC (SUB-1) = " "
               GO TO RCRJRN-005.
            IF CRJRN-GLACC (SUB-1) NOT = WS-OLDGL-NUMBER
               GO TO RCRJRN-003.
            IF CRJRN-GLACC (SUB-1) = WS-OLDGL-NUMBER
               MOVE WS-NEWGL-NUMBER TO CRJRN-GLACC (SUB-1)
               GO TO RCRJRN-003.
       RCRJRN-005.
            MOVE 1 TO SUB-1.
            REWRITE CRJRN-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-CRJRN-ST1 NOT = 0
               MOVE CRJRN-KEY              TO WS-DAILY-1ST
               MOVE CRJRN-CRACC-NUMBER     TO WS-DAILY-2ND
               MOVE "NO CHNG TO CRJRN    " TO WS-DAILY-3RD
               MOVE WS-NEWGL-NUMBER        TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RCRJRN-002.
       RCRJRN-999.
           EXIT.
      *
       READ-GLMAST-TRANS SECTION.
       RDTR-000.
            MOVE 2910 TO POS.
            DISPLAY "Changing GL-Trans to new ACCOUNT.          "
               AT POS.
            MOVE WS-OLDGL-NUMBER TO GLTRANS-ACCOUNT-NUMBER.
            START GLTRANS-FILE KEY NOT < GLTRANS-ACCOUNT-NUMBER
                 INVALID KEY NEXT SENTENCE.
            IF WS-GLTRANS-ST1 NOT = 0
               MOVE "BAD START TO GLTRANS FILE, 'ESC' TO END."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLTRANS-ST1
               GO TO RDTR-999.
       RDTR-002.
            READ GLTRANS-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-GLTRANS-ST1 = 10
               GO TO RDTR-999.
            IF WS-GLTRANS-ST1 NOT = 0
               MOVE "GL-TRANS FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLTRANS-ST1
               GO TO RDTR-002.
            IF GLTRANS-ACCOUNT-NUMBER NOT = WS-OLDGL-NUMBER
               GO TO RDTR-999.
       RDTR-005.
            MOVE WS-NEWGL-NUMBER TO GLTRANS-ACCOUNT-NUMBER.
            REWRITE GLTRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-GLTRANS-ST1 NOT = 0
               MOVE GLTRANS-KEY            TO WS-DAILY-1ST
               MOVE GLTRANS-ACCOUNT-NUMBER TO WS-DAILY-2ND
               MOVE "NO CHNG TO GLTRANS  " TO WS-DAILY-3RD
               MOVE WS-OLDGL-NUMBER        TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RDTR-002.
       RDTR-999.
           EXIT.
      *
       READ-GLMASTLY-TRANS SECTION.
       RDTRLY-000.
            MOVE 2910 TO POS.
            DISPLAY "Changing GL-TransLY to new ACCOUNT.          "
               AT POS.
            MOVE WS-OLDGL-NUMBER TO GLTRANS-LY-ACCNO.
            START GLTRANS-LY-FILE KEY NOT < GLTRANS-LY-ACCNO
                 INVALID KEY NEXT SENTENCE.
            IF WS-GLTRANS-LY-ST1 NOT = 0
               MOVE "BAD START TO GLTRANS-LY FILE, 'ESC' TO END."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLTRANS-LY-ST1
               GO TO RDTRLY-999.
       RDTRLY-002.
            READ GLTRANS-LY-FILE NEXT WITH LOCK
               AT END NEXT SENTENCE.
            IF WS-GLTRANS-LY-ST1 = 10
               GO TO RDTRLY-999.
            IF WS-GLTRANS-LY-ST1 NOT = 0
               MOVE "GLTRANS-LY FILE BUSY READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLTRANS-LY-ST1
               GO TO RDTRLY-002.
            IF GLTRANS-LY-ACCNO NOT = WS-OLDGL-NUMBER
               GO TO RDTRLY-999.
       RDTRLY-005.
            MOVE WS-NEWGL-NUMBER TO GLTRANS-LY-ACCNO.
            REWRITE GLTRANS-LY-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-GLTRANS-LY-ST1 NOT = 0
               MOVE GLTRANS-LY-KEY         TO WS-DAILY-1ST
               MOVE GLTRANS-LY-ACCNO       TO WS-DAILY-2ND
               MOVE "NO CHNG TO GLTRANSLY" TO WS-DAILY-3RD
               MOVE WS-OLDGL-NUMBER        TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RDTRLY-002.
       RDTRLY-999.
           EXIT.
      *
       DELETE-GL-RECORD SECTION.
       DSR-000.
            IF WS-NEWGL-NO = "Y"
               GO TO DSR-999.
       DSR-010.
            DELETE GL-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-GLMAST-ST1 NOT = 0
              MOVE "GL-MAST BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO DSR-010. 
       DSR-999.
           EXIT. 
      *
       RELEASE-GL-RECORD SECTION.
       REL-000.
           UNLOCK GL-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-GL-RECORD SECTION.
       RSR-010.
            MOVE 2910 TO POS.
            DISPLAY "Writing GlMast-New Account.........          "
               AT POS.
          IF WS-NEWGL-NO = "Y"
           IF WS-CREATE-ONE-ACC = "N"
              GO TO RSR-020.
            ADD WS-BALANCE        TO GL-BALANCE
            ADD WS-OPEN-PER-BAL   TO GL-OPEN-PER-BAL
            ADD WS-OPEN-YEAR-BAL  TO GL-OPEN-YEAR-BAL
            ADD WS-LAST-YEAR-BAL  TO GL-LAST-YEAR-BAL.
           MOVE 0 TO SUB-1.
       RSR-011.
           ADD 1 TO SUB-1.
           IF SUB-1 < 13
               ADD WS-PER (SUB-1)      TO GL-PER (SUB-1)
               ADD WS-PER-BU (SUB-1)   TO GL-PER-BU (SUB-1)
               ADD WS-LAST-PER (SUB-1) TO GL-LAST-PER (SUB-1)
               GO TO RSR-011.
           MOVE 1 TO SUB-1.
       RSR-015.
          REWRITE GL-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-GLMAST-ST1 = 23 OR 35 OR 49
              MOVE 
           "GL-MAST DOESN'T EXIST ON REWRITE, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              MOVE "Y" TO WS-INVALID
              GO TO RSR-999.
          IF WS-GLMAST-ST1 NOT = 0
              MOVE "GL-MAST RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO RSR-015.
          GO TO RSR-999.
       RSR-020.
          WRITE GL-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-GLMAST-ST1 = 23 OR 35 OR 49
              MOVE 
           "GL-MAST ALREADY EXISTS ON WRITE, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              MOVE "Y" TO WS-INVALID
              GO TO RSR-999.
          IF WS-GLMAST-ST1 NOT = 0
              MOVE "GL-RECORD BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO RSR-020.
       RSR-999.
          EXIT.
      *
       READ-GLMAST SECTION.
       R-GL-000.
             MOVE GL-NUMBER TO WS-GL-NUMBER.
             START GL-MASTER KEY NOT < GL-KEY
                 INVALID KEY NEXT SENTENCE.
             IF WS-GLMAST-ST1 NOT = 0
                MOVE "BAD START ON GLMASTER, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-GLMAST-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-GLMAST-ST1
                MOVE "Y" TO WS-NEWGL-NO
                GO TO R-GL-999.
       R-GL-010.
             READ GL-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-GLMAST-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FORM
                MOVE "Y" TO WS-NEWGL-NO
                MOVE WS-GL-NUMBER TO GL-NUMBER
                MOVE "GL-MAST BUSY ON READ-LOCK, ERC23, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-GLMAST-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-GLMAST-ST1
                GO TO R-GL-999.
             IF WS-GLMAST-ST1 NOT = 0
                MOVE "GL-MAST RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-GLMAST-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-GLMAST-ST1
                GO TO R-GL-010.
            MOVE "N" TO WS-NEWGL-NO.
       R-GL-999.
            EXIT.
      *
       READ-GLMASTLY SECTION.
       RGL-LY000.
            MOVE 2910 TO POS.
            DISPLAY "Deleting Old MastLY Account.........          "
               AT POS.
             
             MOVE WS-OLDGL-NUMBER TO GL-LY-NUMBER.
             START GL-LY-MASTER KEY NOT < GL-LY-KEY
                 INVALID KEY NEXT SENTENCE.
            IF WS-GL-LY-ST1 NOT = 0
               MOVE "BAD START ON GLMASTLY, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GL-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GL-LY-ST1
               GO TO RGL-LY020.
       RGL-LY010.
             READ GL-LY-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-GL-LY-ST1 = 23 OR 35 OR 49
                MOVE 
                "GLMAST-LY BUSY ON READ23, OLD GLMASTLY DOESN'T EXIST."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-GL-LY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-GL-LY-ST1
                GO TO RGL-LY999.
             IF WS-GL-LY-ST1 NOT = 0
                MOVE "GL-MASTLY BUSY ON READ-LOCK, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-GL-LY-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-GL-LY-ST1
                PERFORM ERROR-MESSAGE
                GO TO RGL-LY010.
       RGL-LY008.
            DELETE GL-LY-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-GL-LY-ST1 NOT = 0
               MOVE "ERROR ON DELETING GLMAST-LY, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GL-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020 
               MOVE 0 TO WS-GL-LY-ST1
               GO TO RGL-LY008. 
             MOVE WS-NEWGL-NUMBER TO GL-LY-NUMBER.
       RGL-LY020.
          WRITE GL-LY-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-GL-LY-ST1 = 23 OR 35 OR 49
              MOVE 
           "GL-MASTLY ALREADY EXISTS ON WRITE, 'ESC' TO EXIT."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GL-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GL-LY-ST1
               GO TO RGL-LY999.
          IF WS-GL-LY-ST1 NOT = 0
              MOVE "GL-MASTLY BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GL-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GL-LY-ST1
              GO TO RGL-LY020.
       RGL-LY999.
             EXIT.
      *
       START-GLMAST SECTION.
       GL-GL-000.
              MOVE WS-GL-NUMBER TO GL-NUMBER.
              START GL-MASTER KEY NOT LESS GL-NUMBER.
       GL-GL-999.
             EXIT.
      *
       READ-GLMAST-NEXT SECTION.
       RSN-005. 
           READ GL-MASTER NEXT WITH LOCK
             AT END 
               MOVE 0 TO GL-NUMBER
                         WS-GL-NUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE 
               "GL-MAST FILE BUSY23 ON READ-NEXT-LOCK, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO RSN-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 
               "GL-MAST FILE BUSY ON READ-NEXT-LOCK, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               PERFORM START-GLMAST
               GO TO RSN-005.
           MOVE GL-NUMBER TO WS-GL-NUMBER.
           MOVE "N" TO WS-NEWGL-NO.
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
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
              MOVE "PARAMETER RECORD NOT THERE, 'ESC' TO EXIT"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RPR-999.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER RECORD BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RPR-010.
       RPR-999.
            EXIT.
      *
       ADD-TOTALS SECTION.
       ATS-001.
            MOVE 0 TO WS-TOTALMV WS-TOTALBU WS-TOTALLY.
            MOVE 1 TO SUB-1.
       ATS-005.
            COMPUTE WS-TOTALMV = WS-TOTALMV + GL-PER (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                GO TO ATS-005.
            MOVE 1 TO SUB-1.
       ATS-010.
            COMPUTE WS-TOTALBU = WS-TOTALBU + GL-PER-BU (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                GO TO ATS-010.
            MOVE 1 TO SUB-1.
       ATS-030.
            COMPUTE WS-TOTALLY = WS-TOTALLY + GL-LAST-PER (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                GO TO ATS-030.
            MOVE 1 TO SUB-1.
       ATS-999.
            EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO GL-NUMBER
                         GL-DESCRIPTION
                         GL-P-B.
             MOVE 0 TO   GL-DATE
                         GL-BALANCE
                         GL-OPEN-PER-BAL
                         GL-OPEN-YEAR-BAL
                         GL-LAST-YEAR-BAL.
             MOVE 0 TO SUB-1.
       CLSC-010.
             ADD 1 TO SUB-1.
             IF SUB-1 < 13
                MOVE 0 TO GL-PER (SUB-1)
                          GL-PER-BU (SUB-1)
                          GL-LAST-PER (SUB-1)
                GO TO CLSC-010.
             MOVE 1 TO SUB-1.
       CLSC-500.
           PERFORM RELEASE-GL-RECORD.
       CLSC-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O GL-MASTER.
            IF WS-GLMAST-ST1 NOT = 0
               MOVE "GL-MAST FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO OPEN-000.
       OPEN-001.
            OPEN I-O GL-LY-MASTER.
            IF WS-GL-LY-ST1 NOT = 0
               MOVE "GL-MASTLY FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GL-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GL-LY-ST1
               GO TO OPEN-001.
       OPEN-003.
            OPEN I-O GLTRANS-FILE.
            IF WS-GLTRANS-ST1 NOT = 0
              MOVE "GLTRANS-FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO OPEN-003.
       OPEN-005.
            OPEN I-O GLTRANS-LY-FILE.
            IF WS-GLTRANS-LY-ST1 NOT = 0
              MOVE "GLTRANSLY-FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-LY-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-LY-ST1
              GO TO OPEN-005.
       OPEN-006.
            OPEN I-O CRJRN-FILE.
            IF WS-CRJRN-ST1 NOT = 0
              MOVE "CRJRN-FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-CRJRN-ST1
              GO TO OPEN-006.
        OPEN-009.
            OPEN I-O GLPARAMETER-FILE.
            IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-009.
            PERFORM READ-PARAMETER-RECORD.
            IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               CLOSE GLPARAMETER-FILE
               PERFORM END-OFF.
            CLOSE GLPARAMETER-FILE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "GlNoChMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE GL-MASTER
                 GL-LY-MASTER
                 GLTRANS-FILE
                 GLTRANS-LY-FILE
                 GLPARAMETER-FILE
                 CRJRN-FILE.
      *     STOP RUN.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAmount1".
       Copy "WriteFieldRec".
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
       Copy "WriteDailyExcep1".
      * END-OF-JOB
