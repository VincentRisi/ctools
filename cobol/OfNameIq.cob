        IDENTIFICATION DIVISION.
        PROGRAM-ID. OfNameIq.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectOfis".
      *
        DATA DIVISION.
        FILE SECTION.
         COPY ChlfdOfis.

       WORKING-STORAGE SECTION.
       77  WS-SHORTNAME         PIC X(10) VALUE " ".
       77  WS-WORK              PIC X(25) VALUE " ".
       77  WS-ACC               PIC 9(7) VALUE 0.
       77  WS-1ST               PIC X VALUE " ".
       77  SUB-DIS              PIC 9(4) VALUE 0.
       77  WS-MIDDLE            PIC X(79) VALUE " ".
       77  WS-NEWORDER          PIC X VALUE " ".
       77  WS-OFISNAME          PIC X(50) VALUE " ".
       77  WS-END               PIC X VALUE " ".      
       77  WS-BODY-LINE         PIC ZZ9.
       77  NEW-OFISNO           PIC X VALUE " ".      
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       01  WS-NAME-PRINT.
           03  WS-NAME          PIC X(79) VALUE " ".
       01  WS-LASTNAME-PRINT.
           03  WS-LAST.
              05  WS-LINE          PIC 99.
              05  FILLER           PIC X VALUE " ".
              05  WS-LASTNAME      PIC X(18) VALUE " ".
           03  WS-FIRST.
              05  FILLER           PIC X VALUE " ".
              05  WS-FIRSTNAME     PIC X(14) VALUE " ".
              05  WS-ADD1          PIC X(26) VALUE " ".
              05  WS-PHONE1        PIC X(18) VALUE " ".
       01  WS-LINE-PRINT.
           03  FILLER           PIC X(26) VALUE " ".
           03  WS-ADD           PIC X(26) VALUE " ".
           03  WS-PHONE         PIC X(27) VALUE " ".
       01  WS-READ-LINES.
           03  WS-NAMES OCCURS 25.
              05  WS-NAME-SEARCH PIC X(50).
       01  WS-OFIS-STATUS.
           03  WS-OFIS-ST1      PIC 99.
       01  WS-SPLIT-ACCOUNT.
           03  WS-SP-1          PIC X VALUE " ".
           03  WS-SP-REST       PIC X(39) VALUE " ".
       01  WS-SPLIT-INPUT-ACC.
           03  WS-SP-I-1        PIC X VALUE " ".
           03  WS-SP-I-REST     PIC X(9) VALUE " ".
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
      *     PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           MOVE "OfNameIq" TO F-FORMNAME.
           
           PERFORM OPEN-0000.
           
           PERFORM OPEN-010 THRU OPEN-020.
           PERFORM DISPLAY-FORM.
       CONTROL-020.
           PERFORM GET-DATA.
           PERFORM READ-MASTER-DISPLAY.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE 2910 TO POS
            DISPLAY
           "ENTER A SHORTNAME & <RETURN> OR <F6> TO CREATE A NEW ENTRY."
               AT POS.       
            MOVE " " TO WS-SPLIT-INPUT-ACC
                        WS-SPLIT-ACCOUNT.
            MOVE "Y" TO WS-1ST.
            MOVE "SHORTNAME" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            PERFORM ERROR-020.
            MOVE 7           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
      * <F6> TO CREATE A NEW RECORD
            IF F-EXIT-CH = X"1A"
                MOVE F-NAMEFIELD TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO WS-ACC
                PERFORM CLEAR-SCREEN
                MOVE "OfNameMt" TO F-FORMNAME
                PERFORM OPEN-010 THRU OPEN-020
                PERFORM DISPLAY-FORM
                PERFORM OPEN-000
                PERFORM GET-OFIS-DATA
                PERFORM END-000
                MOVE "OfNameIq" TO F-FORMNAME
                PERFORM OPEN-010 THRU OPEN-020
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1D"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
            MOVE SPACES      TO ALPHA-RATE.
            IF F-EXIT-CH = X"1D"
               MOVE F-NAMEFIELD TO ALPHA-RATE
               PERFORM CHECK-ENTRY
               MOVE ALPHA-RATE TO WS-ACC
            ELSE
               MOVE F-NAMEFIELD TO WS-SHORTNAME.
      *      MOVE 0              TO F-EXIT-CH.
       GET-999.
            EXIT.
      *
       CHECK-ENTRY SECTION.
       CE-001.
           MOVE 0 TO SUB-1.
       CE-010.
           IF SUB-1 < 7
               ADD 1 TO SUB-1
           ELSE
               GO TO CE-999.
           IF AL-RATE (SUB-1) = " "
               MOVE "0" TO AL-RATE (SUB-1).
           GO TO CE-010.
       CE-999.
           EXIT.
      *
       READ-MASTER-DISPLAY SECTION.
       READ-000.
            PERFORM OPEN-000.
            IF F-EXIT-CH = X"0A"
                MOVE WS-SHORTNAME TO OFIS-NAME WS-SPLIT-INPUT-ACC
               START OFIS-FILE KEY NOT < OFIS-KEY
                  INVALID KEY NEXT SENTENCE.
            IF F-EXIT-CH = X"1D"
               MOVE WS-ACC       TO OFIS-CATEGORY
               START OFIS-FILE KEY NOT < OFIS-CATEGORY
                   INVALID KEY NEXT SENTENCE.
                   
            MOVE 0 TO F-EXIT-CH.
            IF WS-OFIS-ST1 NOT = 0
                MOVE 3010 TO POS
                DISPLAY "RECORD IN USE TRY AGAIN LATER !!!!!" AT POS
                ADD 30 TO POS
                DISPLAY WS-OFIS-ST1 AT POS
                MOVE 0 TO WS-OFIS-STATUS
                PERFORM CLEAR-MIDDLE
                CLOSE OFIS-FILE
                GO TO READ-999.
            IF WS-OFIS-ST1 = 23 OR 35 OR 49
                MOVE 3010 TO POS
                DISPLAY "TRY ENTERING A CREDITOR THAT THAT EXISTS!!!"
                AT POS
                MOVE 0 TO WS-OFIS-STATUS
                PERFORM CLEAR-MIDDLE
                CLOSE OFIS-FILE
                GO TO READ-999.
            MOVE 0 TO SUB-2 SUB-3.
            MOVE 800 TO SUB-DIS.
        READ-010.
            IF F-EXIT-CH = 0
              READ OFIS-FILE NEXT
                AT END
                 GO TO READ-020.
      *          MOVE "END OF FILE, 'ESC' TO THE CLEAR SCREEN."
      *          TO WS-MESSAGE
      *          PERFORM ERROR-MESSAGE
      *          PERFORM CLEAR-MIDDLE
      *          CLOSE OFIS-FILE
      *          GO TO READ-999.
            IF F-EXIT-CH = 1
               READ OFIS-FILE PREVIOUS.
            IF WS-OFIS-ST1 = 91
                MOVE 0 TO WS-OFIS-STATUS
                CLOSE OFIS-FILE
                MOVE "THERE IS A SYSTEM ERROR 91, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO READ-999.
            IF WS-1ST = "Y"
              IF OFIS-NAME NOT = " "
                MOVE OFIS-NAME TO WS-SPLIT-INPUT-ACC
                MOVE "N" TO WS-1ST.
            MOVE OFIS-NAME TO WS-SPLIT-ACCOUNT.
      *      IF WS-SP-1 NOT = WS-SP-I-1
      *          MOVE 2910 TO POS
      *          DISPLAY "No More CREDITORS With That SHORT-NAME," AT POS
      *          MOVE 2820 TO POS
      *          DISPLAY "Press 'ESC' To Clear The Screen !" AT POS
      *          CALL "&LOCKKBD" USING F-FIELDNAME
      *          MOVE 2910 TO POS
      *          DISPLAY "                                      " AT POS
      *          MOVE 2820 TO POS
      *          DISPLAY "                                      " AT POS
      *          PERFORM CLEAR-MIDDLE
      *          GO TO READ-999.
        READ-020.
            ADD 1 TO SUB-3.
            MOVE OFIS-KEY TO WS-NAME-SEARCH (SUB-3).
        READ-025.
            IF SUB-3 > 20
                MOVE 2905 TO POS
                DISPLAY 
                  "Press 'PgDn' For More, 'PgUp' For Previous, " & 
                  "'ESC' To Clear The Screen, OR" AT POS
                MOVE 3015 TO POS 
                DISPLAY
               "Enter A Number & Press <GO> To Edit The Entry." AT POS
                MOVE " "         TO F-NAMEFIELD WS-SHORTNAME
                MOVE 7           TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM USER-FILL-FIELD
                MOVE 7           TO F-CBFIELDLENGTH
                PERFORM READ-FIELD-ALPHA.
            IF WS-OFIS-ST1 = 10
                MOVE 2905 TO POS
                DISPLAY 
            "Press 'PgUp' For Previous, 'Esc' To Clear The Screen, OR"
                AT POS
                MOVE 3015 TO POS 
                DISPLAY
                "Enter A Number & Press <GO> To Edit The Entry." AT POS
                MOVE " "         TO F-NAMEFIELD WS-SHORTNAME
                MOVE 7           TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM USER-FILL-FIELD
                MOVE 7           TO F-CBFIELDLENGTH
                PERFORM READ-FIELD-ALPHA.
            
            PERFORM ERROR1-020.
            PERFORM ERROR-020.
            
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-MIDDLE
                MOVE 0 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-3
                MOVE 800 TO SUB-DIS
            IF WS-OFIS-ST1 = 10
                 GO TO READ-020.

            IF F-EXIT-CH = X"05"
                PERFORM CLEAR-MIDDLE
                MOVE 1 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-3
                MOVE 800 TO SUB-DIS.
            IF F-EXIT-CH = X"07"
                MOVE " " TO WS-SHORTNAME
                MOVE 0 TO WS-OFIS-STATUS
                PERFORM CLEAR-MIDDLE
                CLOSE OFIS-FILE
                GO TO READ-999.
      * <GO>
            IF F-EXIT-CH = X"1B"
                MOVE F-NAMEFIELD TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO WS-ACC
                PERFORM CLEAR-SCREEN
                MOVE "OfNameMt" TO F-FORMNAME
                PERFORM OPEN-010 THRU OPEN-020
                PERFORM DISPLAY-FORM
                PERFORM GET-OFIS-DATA
                PERFORM END-000
                MOVE 0 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-3
                MOVE 800 TO SUB-DIS
                GO TO READ-999.
                
            IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                     AND NOT = X"07" AND NOT = 0     AND NOT = 1
                     AND NOT = X"1B"
                GO TO READ-025.

            ADD 1      TO SUB-DIS.
   
            MOVE SUB-3              TO WS-LINE.
            MOVE OFIS-NAME          TO WS-LASTNAME.
            DISPLAY WS-LAST AT SUB-DIS WITH REVERSE-VIDEO.
            ADD 21 TO SUB-DIS.

            MOVE OFIS-FIRSTNAME     TO WS-FIRSTNAME
            MOVE OFIS-ADDRESS1      TO WS-ADD1
            MOVE OFIS-PHONE         TO WS-PHONE1
            DISPLAY WS-FIRST        AT SUB-DIS.
            
            ADD 78                  TO SUB-DIS
            MOVE " "                TO WS-LINE-PRINT
            DISPLAY WS-LINE-PRINT   AT SUB-DIS.
            
            GO TO READ-010.
        READ-999.
            EXIT.
      *
       GET-OFIS-DATA SECTION.
       GET-OFIS-000.
            MOVE " " TO OFIS-RECORD.
            MOVE "N" TO NEW-OFISNO
                        WS-END.
            MOVE 200 TO SUB-25.
       GET-OFIS-001.
            MOVE SPACES TO F-NAMEFIELD.

            IF WS-NAME-SEARCH (WS-ACC) > " "
                MOVE WS-NAME-SEARCH (WS-ACC) TO OFIS-KEY
                PERFORM READ-OFIS
                GO TO GET-OFIS-003.
            
            MOVE "NAME"        TO F-FIELDNAME.
            MOVE 4             TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD   TO OFIS-NAME.
            IF F-EXIT-CH = X"0C"
                MOVE WS-OFISNAME TO OFIS-KEY
                PERFORM START-OFIS
                PERFORM READ-OFIS-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-OFIS-003
             ELSE
               PERFORM CLSC-010
               GO TO GET-OFIS-000.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO GET-OFIS-999.
            IF F-EXIT-CH = X"05"
                PERFORM READ-OFIS-PREVIOUS
             IF WS-END NOT = "Y"
               GO TO GET-OFIS-003
             ELSE
               PERFORM CLSC-010
               GO TO GET-OFIS-000.
            IF F-EXIT-CH = X"04"
               GO TO GET-OFIS-999.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-OFIS-001.
       GET-OFIS-002.
            MOVE "FIRSTNAME"   TO F-FIELDNAME.
            MOVE 9             TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD   TO OFIS-FIRSTNAME.
            IF F-EXIT-CH = X"01"
               GO TO GET-OFIS-001.
            IF F-EXIT-CH = X"0C"
                MOVE WS-OFISNAME TO OFIS-KEY
                PERFORM START-OFIS
                PERFORM READ-OFIS-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-OFIS-003
             ELSE
               PERFORM CLSC-010
               GO TO GET-OFIS-000.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO GET-OFIS-999.
            IF F-EXIT-CH = X"05"
                PERFORM READ-OFIS-PREVIOUS
             IF WS-END NOT = "Y"
               GO TO GET-OFIS-003
             ELSE
               PERFORM CLSC-010
               GO TO GET-OFIS-000.
            IF F-EXIT-CH = X"04"
               GO TO GET-OFIS-999.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-OFIS-001.

            PERFORM ERROR-020.
            PERFORM READ-OFIS.
            
            IF NEW-OFISNO = "Y"
                PERFORM CLSC-010
                PERFORM FILL-BODY
                GO TO GET-OFIS-999.
                
      *      GO TO GET-OFIS-005.
       GET-OFIS-003.
            MOVE "NAME"     TO F-FIELDNAME.
            MOVE 4          TO F-CBFIELDNAME.
            MOVE OFIS-NAME  TO F-NAMEFIELD.
            MOVE 25         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "FIRSTNAME"    TO F-FIELDNAME.
            MOVE 9              TO F-CBFIELDNAME.
            MOVE OFIS-FIRSTNAME TO F-NAMEFIELD.
            MOVE 25             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-OFIS-005.
            MOVE "TITLE"     TO F-FIELDNAME.
            MOVE 5           TO F-CBFIELDNAME.
            MOVE OFIS-TITLE  TO F-NAMEFIELD.
            MOVE 10          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "AFFILIATION"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            MOVE OFIS-AFFILIATION TO F-NAMEFIELD.
            MOVE 25               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CATEGORY"     TO F-FIELDNAME.
            MOVE 8              TO F-CBFIELDNAME.
            MOVE OFIS-CATEGORY  TO F-NAMEFIELD.
            MOVE 25             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PHONE"     TO F-FIELDNAME.
            MOVE 5           TO F-CBFIELDNAME.
            MOVE OFIS-PHONE  TO F-NAMEFIELD.
            MOVE 25          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PHONE-LABEL"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            MOVE OFIS-PHONE-LABEL TO F-NAMEFIELD
            MOVE 10               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADDRESS1"      TO F-FIELDNAME.
            MOVE 8               TO F-CBFIELDNAME.
            MOVE OFIS-ADDRESS1   TO F-NAMEFIELD
            MOVE 50              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADDRESS2"      TO F-FIELDNAME.
            MOVE 8               TO F-CBFIELDNAME.
            MOVE OFIS-ADDRESS2   TO F-NAMEFIELD
            MOVE 50              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADDRESS3"      TO F-FIELDNAME.
            MOVE 8               TO F-CBFIELDNAME.
            MOVE OFIS-ADDRESS3   TO F-NAMEFIELD
            MOVE 50              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "FAX"       TO F-FIELDNAME.
            MOVE 3           TO F-CBFIELDNAME.
            MOVE OFIS-FAX    TO F-NAMEFIELD.
            MOVE 25          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "FAX-LABEL"      TO F-FIELDNAME.
            MOVE 9                TO F-CBFIELDNAME.
            MOVE OFIS-FAX-LABEL   TO F-NAMEFIELD
            MOVE 10               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADDRESS4"      TO F-FIELDNAME.
            MOVE 8               TO F-CBFIELDNAME.
            MOVE OFIS-ADDRESS4   TO F-NAMEFIELD
            MOVE 50              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADDRESS5"      TO F-FIELDNAME.
            MOVE 8               TO F-CBFIELDNAME.
            MOVE OFIS-ADDRESS5   TO F-NAMEFIELD
            MOVE 50              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADDRESS6"      TO F-FIELDNAME.
            MOVE 8               TO F-CBFIELDNAME.
            MOVE OFIS-ADDRESS6   TO F-NAMEFIELD
            MOVE 50              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-OFIS-177.
            MOVE 1 TO SUB-1 F-INDEX.
            PERFORM SCROLL-PREVIOUS.

            MOVE 2910 TO POS
            DISPLAY 
          "Press <GO> to File the Changes or <Esc> to Clear The Screen."
                AT POS.
            PERFORM FILL-BODY.
      *      IF WS-ABOVE-BODY = "1"
      *          GO TO GET-OFIS-001.
       GET-OFIS-999.
            EXIT.
      *
       FILL-BODY SECTION.
       FILL-002.
      *      IF NEW-OFISNO NOT = "Y"
                 GO TO FILL-006.
       
            MOVE "NAME"         TO F-FIELDNAME.
            MOVE 4              TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD    TO OFIS-NAME.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-NEXT
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-PREVIOUS
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-002.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-002.
        FILL-003.
            MOVE "FIRST-NAME"   TO F-FIELDNAME.
            MOVE 10             TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD    TO OFIS-FIRSTNAME.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-NEXT
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-PREVIOUS
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"01"
               GO TO FILL-002.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-003.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-003.

            PERFORM ERROR-020.
            PERFORM READ-OFIS.
            
            IF NEW-OFISNO NOT = "Y"
                PERFORM GET-OFIS-005
                MOVE 1 TO SUB-1 F-INDEX.
                PERFORM SCROLL-PREVIOUS.
       FILL-006.
            MOVE "TITLE"     TO F-FIELDNAME.
            MOVE 5           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO OFIS-TITLE.
            IF F-EXIT-CH = X"09"
               GO TO FILL-450.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-065.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-NEXT
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-PREVIOUS
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-006.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-006.
        FILL-010.
            MOVE "AFFILIATION"  TO F-FIELDNAME.
            MOVE 11             TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD    TO OFIS-AFFILIATION.
            IF F-EXIT-CH = X"09"
               GO TO FILL-450.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-NEXT
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-PREVIOUS
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"01"
               GO TO FILL-006.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-010.
        FILL-015.
            MOVE "CATEGORY"     TO F-FIELDNAME.
            MOVE 8              TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD    TO OFIS-CATEGORY.
            IF F-EXIT-CH = X"09"
               GO TO FILL-450.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-NEXT
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-PREVIOUS
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-015.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-015.
        FILL-020.
            MOVE "PHONE"        TO F-FIELDNAME.
            MOVE 5              TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD    TO OFIS-PHONE.
            IF F-EXIT-CH = X"09"
               GO TO FILL-450.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-NEXT
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-PREVIOUS
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
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
            MOVE "PHONE-LABEL"  TO F-FIELDNAME.
            MOVE 11             TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD    TO OFIS-PHONE-LABEL.
            IF F-EXIT-CH = X"09"
               GO TO FILL-450.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-NEXT
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-PREVIOUS
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
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
            MOVE "ADDRESS1"     TO F-FIELDNAME.
            MOVE 8              TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 50             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD    TO OFIS-ADDRESS1.
            IF F-EXIT-CH = X"09"
               GO TO FILL-450.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-NEXT
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-PREVIOUS
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
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
            MOVE "ADDRESS2"     TO F-FIELDNAME.
            MOVE 8              TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 50             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD    TO OFIS-ADDRESS2.
            IF F-EXIT-CH = X"09"
               GO TO FILL-450.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-NEXT
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-PREVIOUS
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-035.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-035.
        FILL-040.
            MOVE "ADDRESS3"     TO F-FIELDNAME.
            MOVE 8              TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 50             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD    TO OFIS-ADDRESS3.
            IF F-EXIT-CH = X"09"
               GO TO FILL-450.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-NEXT
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-PREVIOUS
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"01"
               GO TO FILL-035.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-040.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-040.
        FILL-045.
            MOVE "FAX"          TO F-FIELDNAME.
            MOVE 3              TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD    TO OFIS-FAX.
            IF F-EXIT-CH = X"09"
               GO TO FILL-450.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-NEXT
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-PREVIOUS
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"01"
               GO TO FILL-040.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-045.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-045.
        FILL-050.
            MOVE "FAX-LABEL"    TO F-FIELDNAME.
            MOVE 9              TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD    TO OFIS-FAX-LABEL.
            IF F-EXIT-CH = X"09"
               GO TO FILL-450.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-NEXT
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-PREVIOUS
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"01"
               GO TO FILL-045.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-050.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-050.
        FILL-055.
            MOVE "ADDRESS4"     TO F-FIELDNAME.
            MOVE 8              TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 50             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD    TO OFIS-ADDRESS4.
            IF F-EXIT-CH = X"09"
               GO TO FILL-450.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-NEXT
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-PREVIOUS
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"01"
               GO TO FILL-050.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-055.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-055.
        FILL-060.
            MOVE "ADDRESS5"     TO F-FIELDNAME.
            MOVE 8              TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 50             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD    TO OFIS-ADDRESS5.
            IF F-EXIT-CH = X"09"
               GO TO FILL-450.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-NEXT
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-PREVIOUS
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"01"
               GO TO FILL-055.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-060.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-060.
        FILL-065.
            MOVE "ADDRESS6"     TO F-FIELDNAME.
            MOVE 8              TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 50             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD    TO OFIS-ADDRESS6.
            IF F-EXIT-CH = X"09"
               GO TO FILL-450.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-NEXT
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM READ-OFIS-PREVIOUS
               PERFORM GET-OFIS-003 THRU GET-OFIS-005
               GO TO FILL-006.
            IF F-EXIT-CH = X"01"
               GO TO FILL-060.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-065.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-065.
       FILL-450.
      *      MOVE " " TO WS-ABOVE-BODY.
            MOVE 1 TO SUB-1 F-INDEX.
            PERFORM SCROLL-PREVIOUS.
       FILL-455.
            MOVE 3010 TO POS
            DISPLAY "Press <PgDn> to See More Note Fields." AT POS.
            PERFORM ERROR-020.

            MOVE 3010 TO POS
            DISPLAY "    BODY LINE: " AT POS
            ADD 15 TO POS
            MOVE SUB-1 TO WS-BODY-LINE
            DISPLAY WS-BODY-LINE AT POS.
       FILL-500.
      *      IF OFIS-LINE-DESC (SUB-1) NOT = " "
      *          PERFORM SCROLL-050.
                
            MOVE ALL SPACES    TO F-NAMEFIELD
            MOVE "LINE-DESC"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            MOVE 80            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD79-ALPHA.
            
            MOVE F-NAMEFIELD79 TO OFIS-LINE-DESC (SUB-1).

            IF F-EXIT-CH = X"0A" OR = X"0B"
             IF F-INDEX = 10
                PERFORM SCROLL-NEXT
                GO TO FILL-455
              ELSE
                ADD 1 TO SUB-1 F-INDEX
                GO TO FILL-455.

            IF F-EXIT-CH = X"01" AND F-INDEX = 1
                MOVE "1" TO WS-ABOVE-BODY
                GO TO FILL-065.

            IF F-EXIT-CH = X"01" AND F-INDEX > 1
                SUBTRACT 1 FROM F-INDEX SUB-1
                GO TO FILL-455.
                
            IF F-EXIT-CH = X"11"
                PERFORM SCROLL-NEXT
                GO TO FILL-455.
            IF F-EXIT-CH = X"0C"
                PERFORM SCROLL-NEXT-PAGE
                GO TO FILL-455.
            IF F-EXIT-CH = X"05"
                PERFORM SCROLL-PREVIOUS
                GO TO FILL-455.
            IF F-EXIT-CH = X"13"
                PERFORM SCROLL-DOWN
                GO TO FILL-455.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-OFIS-RECORD
               PERFORM CLEAR-SCREEN-FORM
               GO TO FILL-999.
      ****************************
      * <TAB> OR <GO> CHARACTER  *
      ****************************
            IF F-EXIT-CH = X"09"
               GO TO FILL-006.
      
            IF F-EXIT-CH = X"1B"
                PERFORM ERROR-020
                PERFORM REWRITE-OFIS-RECORD
                PERFORM CLEAR-SCREEN-FORM
                GO TO FILL-999.
                
            ADD 1 TO SUB-1 F-INDEX.
               MOVE SUB-1 TO SUB-25.
            IF SUB-1 > 200             
                MOVE 200 TO SUB-1 SUB-25
                MOVE
              "200 LINES ARE UP, PRESS 'ESC' TO TAB & END THE ENTRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-999.
            IF F-INDEX < 11
                GO TO FILL-455.
            SUBTRACT 1 FROM SUB-1
            PERFORM SCROLL-NEXT
            GO TO FILL-455.
       FILL-999.
            EXIT.
      *
       DELETE-OFIS-RECORD SECTION.
       DSR-000.
      *      MOVE "DELETE NOT ALLOWED IN THIS PROGRAM" TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         GO TO DSR-999.
            IF NEW-OFISNO = "Y"
               GO TO DSR-999.
       DSR-010.
            DELETE OFIS-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-OFIS-ST1 NOT = 0
            MOVE "STATUS NOT 0 ON DELETE, 'ESC' TO RETRY." TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-OFIS-ST1
               GO TO DSR-010. 
       DSR-999.
            EXIT. 
      *
       RELEASE-OFIS-RECORD SECTION.
       REL-000.
           UNLOCK OFIS-FILE.
       REL-999.
           EXIT.
      *
       REWRITE-OFIS-RECORD SECTION.
       RSR-010.
          IF NEW-OFISNO = "Y"
              GO TO RSR-020.
          REWRITE OFIS-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-OFIS-ST1 NOT = 0
              MOVE 0 TO WS-OFIS-ST1
              MOVE "OFIS RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSR-010.
              
      *    MOVE "REWRITE DONE." TO WS-MESSAGE
      *    PERFORM ERROR-MESSAGE.
              
          GO TO RSR-999.
       RSR-020.
          WRITE OFIS-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-OFIS-ST1 NOT = 0
              MOVE 0 TO WS-OFIS-ST1
              MOVE "OFIS RECORD BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSR-020.
              
      *    MOVE "WRITE DONE." TO WS-MESSAGE
      *    PERFORM ERROR-MESSAGE.
              
       RSR-999.
          EXIT.
      *
       READ-OFIS SECTION.
       R-ST-000.
             MOVE OFIS-KEY TO WS-OFISNAME.
             START OFIS-FILE KEY NOT < OFIS-KEY
                 INVALID KEY NEXT SENTENCE.
       R-ST-010.
             READ OFIS-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-OFIS-ST1 = 23 OR = 35 OR = 49 OR = 51
                PERFORM CLEAR-SCREEN-FORM
                MOVE "Y" TO NEW-OFISNO
                MOVE WS-OFISNAME TO OFIS-KEY
                GO TO R-ST-999.
             IF WS-OFIS-ST1 NOT = 0
                MOVE "OFIS RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-OFIS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE 0 TO WS-OFIS-ST1
                GO TO R-ST-010.
       R-ST-999.
             EXIT.
      *
       START-OFIS SECTION.
       ST-ST-000.
              MOVE WS-OFISNAME TO OFIS-KEY.
              START OFIS-FILE KEY NOT LESS OFIS-KEY.
       ST-ST-999.
             EXIT.
      *
       READ-OFIS-NEXT SECTION.
       RSN-005. 
           READ OFIS-FILE NEXT WITH LOCK
             AT END 
               MOVE " " TO OFIS-KEY
                           WS-OFISNAME
               MOVE "Y" TO WS-END
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-OFIS-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-OFIS-ST1
               MOVE "OFIS FILE BUSY ON READ-NEXT, PRESS 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-005.
           IF WS-OFIS-ST1 NOT = 0
               MOVE 0 TO WS-OFIS-ST1
               PERFORM START-OFIS
               GO TO RSN-005.
           MOVE OFIS-KEY TO WS-OFISNAME.
           MOVE "N"       TO NEW-OFISNO.
       RSN-999.
             EXIT.
      *
       READ-OFIS-PREVIOUS SECTION.
       RSPREV-005. 
           READ OFIS-FILE PREVIOUS WITH LOCK
             AT END 
               MOVE " " TO OFIS-KEY
                           WS-OFISNAME
               MOVE "Y" TO WS-END
            MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSPREV-999.
           IF WS-OFIS-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-OFIS-ST1
               MOVE "OFIS FILE BUSY ON READ PREV, PRESS 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSPREV-005.
           IF WS-OFIS-ST1 NOT = 0
               MOVE 0 TO WS-OFIS-ST1
               PERFORM START-OFIS
               GO TO RSPREV-005.
           MOVE OFIS-KEY TO WS-OFISNAME.
           MOVE "N"       TO NEW-OFISNO.
       RSPREV-999.
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
            IF SUB-1 > 194
               MOVE 194 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 200  
                GO TO NEXT-030.
      *      IF OFIS-LINE-DESC (SUB-1) = " "
            IF F-INDEX < 11
                GO TO NEXT-010.
       NEXT-030.
            SUBTRACT 10 FROM SUB-1.
            IF SUB-1 > 190
             IF SUB-25 > 190
               COMPUTE F-INDEX = 10 - (200 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX. 
            IF F-INDEX > 10
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.
            
            PERFORM FILL-455.
       NEXT-999.
             EXIT.
      *
       SCROLL-NEXT-PAGE SECTION.
       NEXT-PAGE-000.
            ADD 10 TO SUB-1.
            IF SUB-1 > SUB-25
               MOVE SUB-25 TO SUB-1.
            MOVE 1 TO F-INDEX. 
            PERFORM CLEAR-BODY.
            MOVE 1 TO F-INDEX. 
            IF SUB-1 > 191
               MOVE 191 TO SUB-1.
       NEXT-PAGE-010.
            PERFORM SCROLLING.
       NEXT-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 200
                GO TO NEXT-PAGE-030.
      *      IF OFIS-LINE-DESC (SUB-1) = " "
            IF F-INDEX < 10
                GO TO NEXT-PAGE-010.
       NEXT-PAGE-030.
            SUBTRACT 10 FROM SUB-1.
            IF SUB-1 > 190
             IF SUB-25 > 190
               COMPUTE F-INDEX = 10 - (200 - SUB-25)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX.
            IF SUB-1 > 200
               MOVE 194 TO SUB-1.
            IF F-INDEX > 10
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            PERFORM FILL-455.
       NEXT-PAGE-999.
             EXIT.
      *
       SCROLL-PREVIOUS SECTION.
       PREV-000.
            SUBTRACT 10 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-010.
            PERFORM SCROLLING.
       PREV-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 200   
                GO TO PREV-030.
      *      IF OFIS-LINE-DESC (SUB-1) = " "
             IF F-INDEX < 11
                GO TO PREV-010.
       PREV-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 10 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            PERFORM FILL-455.
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
            IF SUB-1 > 200   
                GO TO SCROLL-DOWN-030.
      *       IF OFIS-LINE-DESC (SUB-1) = " "
            IF F-INDEX < 11
                GO TO SCROLL-DOWN-010.
       SCROLL-DOWN-030.
            MOVE 1 TO F-INDEX.
            SUBTRACT 10 FROM SUB-1.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            PERFORM FILL-455.
       SCROLL-DOWN-999.
             EXIT.
      *
       SCROLLING SECTION.
       SCROLL-010.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.
                
            MOVE "LINE-DESC"            TO F-FIELDNAME
            MOVE 9                      TO F-CBFIELDNAME
            MOVE OFIS-LINE-DESC (SUB-1) TO F-NAMEFIELD79
            MOVE 80                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD79-ALPHA.
       SCROLL-999.
             EXIT.
      *
       CLEAR-BODY SECTION.
       CLEAR-000.
            MOVE 0 TO F-INDEX.
       CLEAR-002.
            ADD 1 TO F-INDEX.
            IF F-INDEX > 10
               GO TO CLEAR-BODY-999.

            MOVE "LINE-DESC"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE " "          TO F-NAMEFIELD79
            MOVE 80           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD79-ALPHA.

            GO TO CLEAR-002.
       CLEAR-BODY-999.
             EXIT.
      *
       CLEAR-MIDDLE SECTION.
       CM-010.
           MOVE " " TO WS-MIDDLE
           MOVE 801 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 901 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1001 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1101 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1201 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1301 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1401 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1501 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1601 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1701 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1801 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1901 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2001 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2101 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2201 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2301 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2401 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2501 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2601 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2701 TO POS
           DISPLAY WS-MIDDLE AT POS.
       CM-999.
           EXIT.
      *
       CHECK-FOR-OFISDATA-NAME SECTION.
       CFODN-000.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
           MOVE WS-OFISDATA TO ALPHA-RATE
           IF AL-RATE (24) NOT = " "
      *         MOVE WS-OFISDATA TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
               GO TO CFODN-999.
       CFODN-010.
           ACCEPT WS-USERNAME FROM ENVIRONMENT "USER".
      *      MOVE "steve" TO WS-USERNAME.
           MOVE WS-USERNAME TO WS-MESSAGE.
           MOVE WS-USERNAME TO DATA-RATE.
           MOVE 1 TO SUB-1
           MOVE 24 TO SUB-2.
           MOVE "_" TO AL-RATE (SUB-2).
           ADD 1 TO SUB-2.
       CFODN-015.
           MOVE DAT-RATE (SUB-1) TO AL-RATE (SUB-2).
           IF SUB-2 < 60
               ADD 1 TO SUB-1 SUB-2
               GO TO CFODN-015.

           MOVE ALPHA-RATE TO WS-OFISDATA.
      *     MOVE WS-OFISDATA TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       CFODN-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-0000.
      *      GO TO OPEN-010.
      *      MOVE WS-OFISDATA TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
            
            PERFORM CHECK-FOR-OFISDATA-NAME.
       OPEN-000.
            OPEN I-O OFIS-FILE.
            IF WS-OFIS-ST1 = 35
               MOVE "OFIS FILE DOESN'T YET EXIST, 'ESC' TO CREATE NEW."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM OPEN-005.
               
            IF WS-OFIS-ST1 NOT = 0
               MOVE "OFIS BUSY ON OPEN, 'ESC' TO RETRY." TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OFIS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-OFIS-ST1
               GO TO OPEN-000.
       OPEN-005.
            OPEN OUTPUT OFIS-FILE.
            IF WS-OFIS-ST1 NOT = 0
               MOVE "OFIS BUSY ON OPEN, 'ESC' TO RETRY." TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OFIS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-OFIS-ST1
               GO TO  OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           CALL "OPENFILE" USING   F-ERROR1
                                   F-FH
                                   F-FILENAME
                                   F-CBFILENAME
                                   F-FILENAME
                                   F-INTEGERZERO
                                   F-OPENMODE.
            IF F-ERROR1 NOT = 0
                DISPLAY "OPEN-FILE FORM @ OPEN-010 ERROR"
                DISPLAY F-ERROR1
                STOP RUN.
       OPEN-020.
            MOVE 8 TO F-CBFORMNAME.
            CALL "OPENFORM" USING  F-ERROR2
                                   F-FH
                                   F-FORMNAME
                                   F-CBFORMNAME
                                   F-FORM
                                   F-CBMAX.
            IF F-ERROR2 NOT = 0
                MOVE "OPEN-FORM @ OPEN-020 ERROR" TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE F-ERROR2 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                STOP RUN.
            CALL "CLOSEFILE" USING F-ERROR1
                                   F-FH.
            IF F-ERROR1 NOT = 0
                DISPLAY "CLOSEFILE ERROR"
                DISPLAY F-ERROR1
                STOP RUN.
      * OPEN-010.
      *      MOVE Ws-Forms-Name   TO F-FILENAME
      *      MOVE Ws-cbForms-name TO F-CBFILENAME.
      *      MOVE "OfNameIq"      TO F-FORMNAME
      *      MOVE 8               TO F-CBFORMNAME.
      * OPEN-011.
      *      GO TO OPEN-900.
      * OPEN-020.
      *      Move Ws-Forms-Name   TO F-FileName
      *      Move Ws-cbForms-name TO F-CbFileName.
      *      MOVE "OfNameMt"      TO F-FORMNAME
      *      MOVE 8               TO F-CBFORMNAME.
      * Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
        END-OFF SECTION.
        END-000.
           CLOSE OFIS-FILE.
        END-900.
      *    STOP RUN.
           EXIT PROGRAM.
        END-999.
            EXIT.
      *
       Copy "ClearFormOfis".
       Copy "ReadFieldAlpha".
       Copy "ReadField79Alpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteField79Alpha".
       Copy "WriteFieldNumeric".
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
