        IDENTIFICATION DIVISION.
        PROGRAM-ID. CbMastMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectCbMaster".
        Copy "SelectGlParameter".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCbMast.
           COPY ChlfdGlParam.

       WORKING-STORAGE SECTION.
       77  WS-TYPE            PIC X VALUE " ".
       77  WS-MARGIN          PIC S9(8)V99 VALUE 0.
       77  WS-TOTALMV         PIC S9(8)V99 VALUE 0.
       77  WS-TOTALLY         PIC S9(8)V99 VALUE 0.
       77  NEW-GLNO           PIC X VALUE " ".      
       77  WS-HEAD-VALID      PIC X VALUE " ".
       77  WS-SUB-VALID       PIC X VALUE " ".
       77  WS-END             PIC X VALUE " ".      
       77  WS-NUMBER          PIC X(12) VALUE " ".
       77  SUB-1-SAVE         PIC S9(5) VALUE 0.
       01  WS-CB-STATUS.
           03  WS-CB-ST1       PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1  PIC 99.
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
           PERFORM GET-DATA.
           PERFORM FILL-DATA.
           GO TO CONTROL-010.
      *
        GET-DATA SECTION.
        GET-000.
            MOVE " " TO CB-RECORD.
            MOVE "N" TO NEW-GLNO
                        WS-END.
            MOVE 0 TO WS-TOTALMV.

            MOVE "                        " TO F-NAMEFIELD ALPHA-RATE.
        GET-001.
            MOVE "ACCNUM" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF ALPHA-RATE > SPACES
               PERFORM NUMBER-CHECK
               MOVE WS-GLNO-CHECK TO CB-NUMBER
            IF SIGN-FOUND = 9
               MOVE 0 TO SIGN-FOUND
               GO TO GET-001.
            IF F-EXIT-CH = X"0C"
                MOVE WS-NUMBER TO CB-NUMBER
                PERFORM START-GL
                PERFORM READ-CB-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-FORM
               GO TO GET-000.
          IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-CB.
            IF NEW-GLNO = "Y"
                PERFORM CLSC-000 THRU CLSC-010
                MOVE WS-GLNO-CHECK TO CB-NUMBER
                PERFORM GET-003 THRU GET-005
                GO TO GET-999.
        GET-003.
            MOVE "ACCNUM" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE CB-NUMBER TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            MOVE "DESCRIPTION" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE CB-DESCRIPTION TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BALANCE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CB-BALANCE TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "OPENPERBAL" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE CB-OPEN-PER-BAL TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "OPENYEARBAL" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE CB-OPEN-YEAR-BAL TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "LYOPENBAL" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE CB-LY-OPEN-BAL TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE 1 TO SUB-1 F-INDEX.
       GET-006.
            MOVE "PER" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE CB-PER (SUB-1) TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
                GO TO GET-006.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-030.
            MOVE "LASTPER" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CB-LAST-PER (SUB-1) TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
                GO TO GET-030.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-050.
            PERFORM ADD-TOTALS.

            MOVE "TOTALMV" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-TOTALMV TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "TOTALLY" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-TOTALLY TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                  GO TO FILL-999.
            MOVE "DESCRIPTION" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO CB-DESCRIPTION.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CB-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF CB-DESCRIPTION = " "
               MOVE "This Field May Not Be Blank, Enter A Character."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CB-RECORD
               PERFORM READ-CB-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CB-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CB-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B" AND NOT = X"1D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.
            IF NEW-GLNO = "N"
             IF F-EXIT-CH NOT = X"1D"
                GO TO FILL-001.
        FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BALANCE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 CB-BALANCE.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CB-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CB-RECORD
               PERFORM READ-CB-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CB-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CB-RECORD
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
        FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "OPENPERBAL" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 CB-OPEN-PER-BAL.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CB-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CB-RECORD
               PERFORM READ-CB-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CB-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CB-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-025.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-025.
        FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "OPENYEARBAL" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 CB-OPEN-YEAR-BAL.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CB-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CB-RECORD
               PERFORM READ-CB-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CB-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CB-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-030.
        FILL-035.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LYOPENBAL" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 CB-LY-OPEN-BAL.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CB-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CB-RECORD
               PERFORM READ-CB-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CB-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CB-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-035.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B" AND NOT = X"1D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-035.
        FILL-040.
            IF NEW-GLNO = "N"
             IF F-EXIT-CH NOT = X"1D"
                MOVE 1 TO SUB-1 F-INDEX
                GO TO FILL-001.
            MOVE 1 TO SUB-1 F-INDEX.
        FILL-045.
            MOVE "                          " TO F-NAMEFIELD.
            MOVE "PER" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 CB-PER (SUB-1).
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CB-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CB-RECORD
               PERFORM READ-CB-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
             IF SUB-1 = 1
               GO TO FILL-035
             ELSE
               SUBTRACT 1 FROM SUB-1 F-INDEX
               GO TO FILL-045.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CB-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CB-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-040.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B" AND NOT = X"1D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-045.
            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
               GO TO FILL-045.

            MOVE 1 TO SUB-1 F-INDEX.
        FILL-160.
            IF NEW-GLNO = "N"
             IF F-EXIT-CH NOT = X"1D"
                GO TO FILL-001.
        FILL-165.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LASTPER" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 CB-LAST-PER (SUB-1).
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CB-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CB-RECORD
               PERFORM READ-CB-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
             IF SUB-1 = 1
               GO TO FILL-045
             ELSE
               SUBTRACT 1 FROM SUB-1 F-INDEX
               GO TO FILL-165.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CB-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CB-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-160.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-160.
            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
               GO TO FILL-165.
            MOVE 1 TO SUB-1 F-INDEX.

            GO TO FILL-001.
       FILL-999.
             EXIT.
      *
       ADD-TOTALS SECTION.
       ATS-001.
            MOVE 0 TO WS-TOTALMV WS-TOTALLY.
            MOVE 1 TO SUB-1.
       ATS-005.
            COMPUTE WS-TOTALMV = WS-TOTALMV + CB-PER (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                GO TO ATS-005.
            MOVE 1 TO SUB-1.
       ATS-030.
            COMPUTE WS-TOTALLY = WS-TOTALLY + CB-LAST-PER (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                GO TO ATS-030.
            MOVE 1 TO SUB-1.
       ATS-999.
            EXIT.
      *
       DELETE-CB-RECORD SECTION.
       DSR-000.
           IF NEW-GLNO = "Y"
               GO TO DSR-999.
       DSR-010.
      *     MOVE "DELETE NOT ALLOWED, PRESS 'ESC' TO CLEAR."
      *         TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         GO TO DSR-999.
           DELETE CB-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-CB-ST1 NOT = 0
               MOVE "CBMASTER BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CB-ST1 TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CB-ST1
               GO TO DSR-010. 
       DSR-999.
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
              MOVE "PARAMETER RECORD BUSY, PRESS 'ESC' TO RETRY."
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
       RELEASE-CB-RECORD SECTION.
       REL-000.
           UNLOCK CB-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-CB-RECORD SECTION.
       RSR-010.
          IF NEW-GLNO = "Y"
              GO TO RSR-020.
          REWRITE CB-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-CB-ST1 NOT = 0
              MOVE "CB RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CB-ST1 TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CB-ST1
               GO TO RSR-010.
          GO TO RSR-999.
       RSR-020.
          WRITE CB-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-CB-ST1 NOT = 0
              MOVE "CB RECORD BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CB-ST1 TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CB-ST1
               GO TO RSR-020.
       RSR-999.
          EXIT.
      *
       READ-CB SECTION.
       R-CB-000.
             MOVE CB-NUMBER TO WS-NUMBER.
             START CB-MASTER KEY NOT < CB-KEY.
       R-CB-010.
             READ CB-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-CB-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-GLNO
                MOVE WS-NUMBER TO CB-NUMBER
                GO TO R-CB-999.
             IF WS-CB-ST1 NOT = 0
               MOVE "CB RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CB-ST1 TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CB-ST1
               GO TO R-CB-010.
             MOVE "N" TO NEW-GLNO.
       R-CB-999.
             EXIT.
      *
       START-GL SECTION.
       CB-CB-000.
              MOVE WS-NUMBER TO CB-NUMBER.
              START CB-MASTER KEY NOT LESS CB-NUMBER
                 INVALID KEY NEXT SENTENCE.
       CB-CB-999.
             EXIT.
      *
       READ-CB-NEXT SECTION.
       RSN-005. 
           READ CB-MASTER NEXT WITH LOCK
             AT END 
               MOVE " " TO CB-NUMBER
                           WS-NUMBER
               MOVE "Y" TO WS-END
               PERFORM CLEAR-FORM
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-CB-ST1 = 23 OR 35 OR 49
               MOVE "CB-MASTER FILE BUSY23, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CB-ST1 TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CB-ST1
               GO TO RSN-005.
           IF WS-CB-ST1 NOT = 0
               MOVE "CB-MASTER FILE BUSY, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CB-ST1 TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CB-ST1
      *         PERFORM START-GL
               GO TO RSN-005.
           MOVE CB-NUMBER TO WS-NUMBER.
           MOVE "N" TO NEW-GLNO.
       RSN-999.
             EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO CB-NUMBER
                         CB-DESCRIPTION.
             MOVE 1 TO SUB-1.
       CLSC-005.
             MOVE 0   TO CB-PER (SUB-1)
                         CB-LAST-PER (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                 GO TO CLSC-005.
             MOVE 1 TO SUB-1.
       CLSC-010.
           MOVE 0 TO CB-BALANCE
                     CB-OPEN-PER-BAL
                     CB-OPEN-YEAR-BAL
                     CB-LY-OPEN-BAL.
       CLSC-500.
           PERFORM RELEASE-CB-RECORD.
       CLSC-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O CB-MASTER.
           IF WS-CB-ST1 NOT = 0
               MOVE "CASH BOOK FILE BUSY ON OPEN, 'ESC' TO RETRY"
               TO WS-MESSAGE
               MOVE "CB-MASTER FILE BUSY, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-CB-ST1 TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-CB-ST1
               GO TO OPEN-000.
        OPEN-001.
            OPEN I-O GLPARAMETER-FILE.
            IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-001.
            PERFORM READ-PARAMETER-RECORD.
            IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               CLOSE GLPARAMETER-FILE
               PERFORM END-OFF.
            CLOSE GLPARAMETER-FILE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CbMaster"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CB-MASTER.
           EXIT PROGRAM.
      *      STOP RUN.
       END-999.
          EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
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
      * END-OF-JOB
