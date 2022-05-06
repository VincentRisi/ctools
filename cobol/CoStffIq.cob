        IDENTIFICATION DIVISION.
        PROGRAM-ID. CoStffIq.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT STAFF-FILE ASSIGN TO Ws-CoStaffInOut
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-STAFF-STATUS
               RECORD KEY IS STAFF-KEY.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCoStaff.

       WORKING-STORAGE SECTION.
       77  NEW-NO               PIC X VALUE " ".      
       77  WS-END               PIC X VALUE " ".      
       77  WS-NAME              PIC X(25) VALUE " ".
       77  PSW-SUB1             PIC S9(5) VALUE 0.
       77  PSW-SUB2             PIC S9(5) VALUE 0.
       77  WS-BODY-LINE         PIC ZZ9.
       77  WS-AGE               PIC 9(3).
       01  WS-AGE-SPLIT.
           03  WS-AGE12         PIC 99.
           03  WS-AGE3          PIC 9.
       01  WS-TIME-DISPLAY.
           03  SPLIT-HR           PIC 99.
           03  SPLIT-HR-FIL       PIC X.
           03  SPLIT-MN           PIC 99.
       01  WS-STAFF-STATUS.
           03  WS-STAFF-ST1       PIC 99.
       01  W-READ-KEY             PIC X.
       01  WS-PASSWORD-KEY.
           03  WS-PA-KEY          PIC X OCCURS 11.
       01  B-STAFF.
         02  B-STAFF-LINES OCCURS 50.
           03  B-NAME        PIC X(25) VALUE " ".
           03  B-DATE-IN     PIC 9(8) VALUE 0.
           03  B-TIME-IN     PIC X(5) VALUE " ".
           03  B-STATUS      PIC X VALUE " ".
           03  B-DOB         PIC 9(8) VALUE 0.
       01  WS-DOB-MESSAGE.
           03  WS-DOB1       PIC X(8) VALUE "IT'S THE".
           03  WS-DOB2       PIC Z(3).
           03  WS-DOB3       PIC X(3) VALUE " ".
           03  WS-DOB4       PIC X(14) VALUE "BIRTHDAY OF :".
           03  WS-DOB5       PIC X(25) VALUE " ".
       01  INDEX-SAVE.
           03  F-INDEX-SAVE           PIC 9(4) COMP VALUE 1.
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
           PERFORM CLEAR-STAFF-RECORDS.
           PERFORM DISPLAY-FORM.
           MOVE 1 TO SUB-1.
           
           MOVE SPACES TO STAFF-NAME
                             WS-NAME.
           START STAFF-FILE KEY NOT < STAFF-KEY.
           PERFORM READ-NEXT-STAFF.
           
           MOVE 1 TO SUB-1 F-INDEX.
           PERFORM SCROLL-PREVIOUS.
           
           PERFORM FILL-DATA.
           GO TO CONTROL-010.
      *
       FILL-DATA SECTION.
       FILL-010.
           MOVE 1 TO SUB-1 F-INDEX.

           MOVE 2710 TO POS
           DISPLAY "Press 'PgDn' For More, 'PgUp' For Prev,"
           AT POS
           ADD 40 TO POS
           DISPLAY "'F12' To Scroll Up" AT POS
           MOVE 2811 TO POS
           DISPLAY 
           "'F11' to Scroll Down, Or 'Alt-F8' To Change Your Status."
            AT POS.

           MOVE 2915 TO POS
           DISPLAY 
           "N=NOT YET IN, I=IN, O=OUT, S=SICK, L=ON LEAVE, D=SMOKING"
           AT POS.
        FILL-015.
            MOVE 3110 TO POS.
            DISPLAY "    BODY LINE: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.

            MOVE "NAME"      TO F-FIELDNAME.
            MOVE 4           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
      *
      *<CODE-GO>  <ALT-G> USED TO RE-READ AND UPDATE INFO
            IF F-EXIT-CH = X"9B" OR = X"8C"
               GO TO FILL-999.
               
      *<CANCEL> AND <PREVIOUS-PAGE>
            IF F-EXIT-CH = X"07" OR = X"05"
               PERFORM CLEAR-BODY
               PERFORM SCROLL-PREVIOUS
               GO TO FILL-015.
               
      *<NEXT-PAGE>
            IF F-EXIT-CH = X"0C"
               PERFORM SCROLL-NEXT-PAGE
               GO TO FILL-015.
               
      *<SCROLL-DOWN>
            IF F-EXIT-CH = X"13"
               PERFORM SCROLL-PREVIOUS
               GO TO FILL-015.
      *<SCROLL-UP>
            IF F-EXIT-CH = X"11"
               PERFORM SCROLL-NEXT
               GO TO FILL-015.
               
      *<UP-ARROW>
            IF F-EXIT-CH = X"01"
             IF F-INDEX = 1
              IF SUB-1 > 1
                PERFORM SCROLL-PREVIOUS-PAGE
                GO TO FILL-015.
            IF F-EXIT-CH = X"01"
             IF F-INDEX = 1
              IF SUB-1 = 1
               GO TO FILL-015
             ELSE
               PERFORM SCROLL-PREVIOUS-PAGE
               GO TO FILL-015.
            IF F-EXIT-CH = X"01"
               SUBTRACT 1 FROM SUB-1 F-INDEX
               GO TO FILL-015.
               
      *<DOWN-ARROW> AND <RETURN>
            IF F-EXIT-CH = X"0B" OR = X"0A"
             IF F-NAMEFIELD = " "
               GO TO FILL-015.
            IF F-EXIT-CH = X"0B" OR = X"0A"
             IF F-INDEX = 20
               PERFORM SCROLL-NEXT
               GO TO FILL-015
             ELSE
               ADD 1 TO SUB-1 F-INDEX
               GO TO FILL-015.
      *<END>
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
      *
      *<CODE-MOVE> - CTOS. USED TO CHANGE IN/OUT TIMES ETC
      *<ALT-F8> IN LINUX
            MOVE 25          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"8F" OR = X"9D"
             IF F-NAMEFIELD = WS-TERMINALNAME
                 GO TO FILL-030.
            IF F-EXIT-CH = X"8F" OR = X"9D"
             IF F-NAMEFIELD NOT = WS-TERMINALNAME
                 PERFORM CHECK-PASSWORD
              IF WS-PASSWORD-VALID = "N"
                 GO TO FILL-015
              ELSE
                 MOVE F-NAMEFIELD TO WS-TERMINALNAME
                 GO TO FILL-030.

            GO TO FILL-015.
       FILL-030.
            PERFORM CL-BODY-020
            PERFORM READ-RECORD.
       
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "STATUS" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STAFF-STATUS B-STATUS (SUB-1).
            
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-BODY
               GO TO FILL-999.
            IF STAFF-STATUS NOT = "I" AND NOT = "O" AND NOT = "L"
                        AND NOT = "S" AND NOT = "N" AND NOT = "D"
               MOVE
          "THIS FIELD MUST BE D, I, O, L, N OR S, <CANCEL> TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-030.
            IF F-EXIT-CH = X"1B" OR = X"0A"
               PERFORM REWRITE-RECORD
               MOVE SUB-1 TO SUB-2
               MOVE F-INDEX TO F-INDEX-SAVE
               PERFORM SCROLLING
               MOVE SUB-2 TO SUB-1
               MOVE F-INDEX-SAVE TO F-INDEX
               PERFORM OPEN-005
               GO TO FILL-015.

            GO TO FILL-030.
       FILL-999.
            EXIT.
      *
       CLEAR-STAFF-RECORDS SECTION. 
       CSR-005.
            MOVE 1 TO SUB-2.
       CSR-010.
            MOVE SPACES TO B-NAME (SUB-2)
                           B-STATUS (SUB-2)
                           B-TIME-IN (SUB-2)
            MOVE 0      TO B-DATE-IN (SUB-2).
            IF SUB-2 < 50
               ADD 1 TO SUB-2
               GO TO CSR-010.
       CSR-999.
            EXIT.
      *
       DELETE-RECORD SECTION.
       DDR-000.
            IF NEW-NO = "Y"
                GO TO DDR-999.
       DDR-010.
            DELETE STAFF-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-STAFF-ST1 NOT = 0
               MOVE 0 TO WS-STAFF-ST1
               GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       REWRITE-RECORD SECTION.
       RDR-010.
            REWRITE STAFF-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-STAFF-ST1 NOT = 0
                MOVE 0 TO WS-STAFF-ST1
                MOVE "STAFF FILE BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-010.
       RDR-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE WS-TERMINALNAME TO STAFF-KEY.
           START STAFF-FILE KEY NOT < STAFF-KEY
              INVALID KEY NEXT SENTENCE.
       RD-010.
           READ STAFF-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-STAFF-ST1 NOT = 0
                MOVE 0 TO WS-STAFF-ST1
                MOVE "STAFF FILE Busy ON READ,  'ESC' To Retry."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-010.

           MOVE WS-DATE         TO STAFF-LAST-DATE
           ACCEPT WS-TIME FROM TIME
           MOVE WS-TIME         TO WS-TIME
           MOVE WS-HR           TO SPLIT-HR
           MOVE ":"             TO SPLIT-HR-FIL
           MOVE WS-MIN          TO SPLIT-MN
           MOVE WS-TIME-DISPLAY TO STAFF-LAST-TIME.
           
           MOVE STAFF-LAST-DATE TO B-DATE-IN (SUB-1)
           MOVE STAFF-LAST-TIME TO B-TIME-IN (SUB-1).
       RD-999.
             EXIT.
      *
       START-RECORD SECTION.
       STR-000.
           MOVE WS-NAME   TO STAFF-NAME.
           START STAFF-FILE KEY NOT < STAFF-KEY
              INVALID KEY
              MOVE SPACES TO STAFF-NAME
                                WS-NAME
           START STAFF-FILE KEY NOT < STAFF-KEY.
       STR-999.
             EXIT.
      *
       READ-NEXT-STAFF SECTION.
       RNN-005.
           READ STAFF-FILE NEXT
             AT END
               GO TO RNN-900.
           IF WS-STAFF-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-STAFF-ST1
               MOVE "STAFF FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNN-005.
           IF WS-STAFF-ST1 NOT = 0
               MOVE 0 TO WS-STAFF-ST1
               PERFORM START-RECORD
               GO TO RNN-005.
           MOVE STAFF-NAME      TO B-NAME (SUB-1)
           MOVE STAFF-LAST-DATE TO B-DATE-IN (SUB-1)
           MOVE STAFF-LAST-TIME TO B-TIME-IN (SUB-1)
           MOVE STAFF-STATUS    TO B-STATUS (SUB-1)
           MOVE STAFF-DOB       TO B-DOB (SUB-1).
           
           IF SUB-1 < 50
               ADD 1 TO SUB-1
               GO TO RNN-005.
       RNN-900.
           MOVE SUB-1 TO SUB-25.
       RNN-999.
           EXIT.
      *
       SCROLLING SECTION.
       SCROLL-003.
            IF B-NAME (SUB-1) NOT >  "       "
               GO TO SCROLL-999.

            MOVE "NAME"         TO F-FIELDNAME.
            MOVE 4              TO F-CBFIELDNAME.
            MOVE B-NAME (SUB-1) TO F-NAMEFIELD.
            MOVE 25             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"            TO F-FIELDNAME.
            MOVE 4                 TO F-CBFIELDNAME.
            MOVE B-DATE-IN (SUB-1) TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE      TO F-NAMEFIELD.
            MOVE 10                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TIME"            TO F-FIELDNAME.
            MOVE 4                 TO F-CBFIELDNAME.
            MOVE B-TIME-IN (SUB-1) TO F-NAMEFIELD
            MOVE 5                 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STATUS"     TO F-FIELDNAME.
            MOVE 6            TO F-CBFIELDNAME.
            IF B-STATUS (SUB-1) = "D"
                MOVE "SMOKING" TO F-NAMEFIELD.
            IF B-STATUS (SUB-1) = "I"
                MOVE "IN" TO F-NAMEFIELD.
            IF B-STATUS (SUB-1) = "O"
                MOVE "OUT" TO F-NAMEFIELD.
            IF B-STATUS (SUB-1) = "L"
                MOVE "ON LEAVE" TO F-NAMEFIELD.
            IF B-STATUS (SUB-1) = "N"
                MOVE "NOT YET IN" TO F-NAMEFIELD.
            IF B-STATUS (SUB-1) = "S"
                MOVE "SICK LEAVE" TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            
      *      IF WS-TERMINALNAME = "Steve Christensen" 
      *      OR = "John Christensen" or = "Admin Server" or "Ray Engela"
            MOVE B-DOB (SUB-1) TO SPLIT-DATE
             IF WS-MM = SPLIT-MM
              IF WS-DD = SPLIT-DD
               COMPUTE WS-AGE = WS-YY - SPLIT-YY
               MOVE WS-AGE         TO WS-DOB2
               PERFORM CHECK-DAY-DESC
               MOVE B-NAME (SUB-1) TO WS-DOB5
               MOVE WS-DOB-MESSAGE TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
       SCROLL-999.
            EXIT.
      *
       CHECK-DAY-DESC SECTION.
       DD-005.
           MOVE WS-AGE TO WS-AGE-SPLIT.
           IF WS-AGE3 = 0
               MOVE "th" TO WS-DOB3.
           IF WS-AGE3 = 1
               MOVE "st" TO WS-DOB3.
           IF WS-AGE3 = 2
               MOVE "nd" TO WS-DOB3.
           IF WS-AGE3 = 3
               MOVE "rd" TO WS-DOB3.
           IF WS-AGE3 > 3
               MOVE "th" TO WS-DOB3.
       DD-999.
           EXIT.
      *
       CLEAR-BODY SECTION.
       CL-BODY-000.
            MOVE 1 TO SUB-2 F-INDEX.
       CL-BODY-010.
            MOVE "NAME"        TO F-FIELDNAME.
            MOVE 4             TO F-CBFIELDNAME.
            MOVE " "           TO F-NAMEFIELD.
            MOVE 25            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"            TO F-FIELDNAME.
            MOVE 4                 TO F-CBFIELDNAME.
            MOVE " "               TO F-NAMEFIELD.
            MOVE 10                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TIME"            TO F-FIELDNAME.
            MOVE 4                 TO F-CBFIELDNAME.
            MOVE " "               TO F-NAMEFIELD.
            MOVE 10                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       CL-BODY-020.
            MOVE "STATUS"     TO F-FIELDNAME.
            MOVE 6            TO F-CBFIELDNAME.
            MOVE " "          TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       CL-BODY-030.
            IF SUB-2 < 20
               ADD 1 TO SUB-2 F-INDEX
               GO TO CL-BODY-010.
       CL-BODY-999.
            EXIT.
      *
       SCROLL-NEXT SECTION.
       NEXT-000.
            ADD 1 TO SUB-1.
            IF SUB-1 > SUB-25
               MOVE SUB-25 TO SUB-1.
            IF SUB-1 < 1
               MOVE 1 TO SUB-1.
            MOVE 1 TO F-INDEX. 
            PERFORM CLEAR-BODY.
            MOVE 1 TO F-INDEX. 
            IF SUB-1 > 31
               MOVE 31 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 50  
                GO TO NEXT-030.
            IF F-INDEX < 21
                GO TO NEXT-010.
       NEXT-030.
            SUBTRACT 20 FROM SUB-1.
            IF SUB-1 > 30
             IF SUB-25 > 30
               COMPUTE F-INDEX = 20 - (50 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX. 
            IF F-INDEX > 20
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3110 TO POS.
            DISPLAY "    BODY LINE: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-999.
             EXIT.
      *
       SCROLL-NEXT-PAGE SECTION.
       NEXT-PAGE-000.
            ADD 20 TO SUB-1.
            IF SUB-1 > SUB-25
               MOVE SUB-25 TO SUB-1.
            MOVE 1 TO F-INDEX. 
            PERFORM CLEAR-BODY.
            MOVE 1 TO F-INDEX. 
            IF SUB-1 > 31
               MOVE 31 TO SUB-1.
       NEXT-PAGE-010.
            PERFORM SCROLLING.
       NEXT-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 50  
                GO TO NEXT-PAGE-030.
            IF F-INDEX < 21
                GO TO NEXT-PAGE-010.
       NEXT-PAGE-030.
            SUBTRACT 20 FROM SUB-1.
            IF SUB-1 > 30
             IF SUB-25 > 30
               COMPUTE F-INDEX = 20 - (50 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX.
            IF SUB-1 > 50
               MOVE 31 TO SUB-1.
            IF F-INDEX > 20
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.
            MOVE 1 TO F-INDEX. 

            MOVE 3110 TO POS.
            DISPLAY "    BODY LINE: " AT POS.
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-PAGE-999.
             EXIT.
      *
       SCROLL-PREVIOUS SECTION.
       PREV-000.
            SUBTRACT 1 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-010.
            PERFORM SCROLLING.
       PREV-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 50
                GO TO PREV-030.
            IF F-INDEX < 21
                GO TO PREV-010.
       PREV-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 20 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

             MOVE 3110 TO POS.
             DISPLAY "    BODY LINE: " AT POS.
             ADD 16 TO POS.
             MOVE SUB-1 TO WS-BODY-LINE.
             DISPLAY WS-BODY-LINE AT POS.
       PREV-999.
             EXIT.
      *
       SCROLL-PREVIOUS-PAGE SECTION.
       PREV-PAGE-000.
            SUBTRACT 20 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-PAGE-010.
            PERFORM SCROLLING.
       PREV-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 50
                GO TO PREV-PAGE-030.
            IF F-INDEX < 21
                GO TO PREV-PAGE-010.
       PREV-PAGE-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 20 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

             MOVE 3110 TO POS.
             DISPLAY "    BODY LINE: " AT POS.
             ADD 16 TO POS.
             MOVE SUB-1 TO WS-BODY-LINE.
             DISPLAY WS-BODY-LINE AT POS.
       PREV-PAGE-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STAFF-FILE.
            IF WS-STAFF-ST1 NOT = 0
               MOVE 0 TO WS-STAFF-ST1
               MOVE "STAFF FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT.

           PERFORM GET-USER-MAIL-NAME.
           MOVE WS-USERNAME     TO WS-TerminalName.
           
      *     MOVE WS-TERMINALNAME TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CoStffIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STAFF-FILE.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldDate".
       Copy "StaffPassword".
       Copy "ReadKBD".
       Copy "GetSystemY2KDate".
       Copy "GetUserMailName".
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
