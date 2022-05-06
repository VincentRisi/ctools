        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrSaCoMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrContact".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdDrCont.

       WORKING-STORAGE SECTION.
       77  NEW-NO             PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-YN              PIC X VALUE " ".      
       77  WS-NUMBER          PIC 9(7) VALUE 0.
       77  WS-DCNUM           PIC 9(7) VALUE 0.
       77  WS-DC-SMAN         PIC X VALUE " ".
       77  WS-DC-SMAN-NUM     PIC 9 VALUE 0.
       77  WS-AREA            PIC X(2) VALUE " ".
       77  WS-AREA-NAME       PIC X(40) VALUE " ".
       01  WS-ENT-DISPLAY.
           03  FILLER        PIC X(12) VALUE "CONTACT REP:".
           03  WS-ENT-DC     PIC X.
           03  FILLER        PIC X(5) VALUE " ".
           03  FILLER        PIC X(12) VALUE "ENTERED REP:".
           03  WS-ENT-DR     PIC X.
       01  WS-DC-DISPLAY.
           03  FILLER        PIC X(12) VALUE "CONTACT REP:".
           03  WS-DC-CONT    PIC X.
           03  FILLER        PIC X(5) VALUE " ".
           03  FILLER        PIC X(12) VALUE "ACCOUNT REP:".
           03  WS-DR-CONT    PIC X.
       01  WS-CONTACT-STATUS.
           03  WS-DC-ST1     PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1 PIC 99.
       Copy "WsDateInfo".
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN.
       CONT-010.
           PERFORM DISPLAY-FORM
           PERFORM GET-DATA
           PERFORM FILL-DATA
           GO TO CONT-010.
      *
        GET-DATA SECTION.
        GET-000.
            MOVE "N" TO NEW-NO
                        WS-END.
            MOVE 2310 TO POS
            DISPLAY
            "ENTER SALESMAN No & PRESS <F10> TO DELETE ALL RECORDS"
                AT POS.
            MOVE 2410 TO POS
            DISPLAY
            "  OR PRESS <F1> TO DELETE ALL ACCOUNTS NOW NOT YOURS."
                AT POS.
            MOVE 2510 TO POS
            DISPLAY
        "OR ENTER SALESMAN No & PRESS <Alt-PgDn> TO READ BY REP#."
                AT POS.
        GET-001.
            MOVE SPACES       TO F-NAMEFIELD.
            MOVE "ACC"        TO F-FIELDNAME.
            MOVE 3            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE WS-DC-SMAN.
            PERFORM DECIMALISE-RATE.
            IF NUMERIC-RATE > 0
              MOVE NUMERIC-RATE TO WS-NUMBER DC-ACCOUNT-NUMBER 
                                   WS-DCNUM.
      *<END>
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
      *<'ESC'>
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
      *<F10>
            IF F-EXIT-CH = X"1F"
                 PERFORM DELETE-ALL-RECORDS
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
      *<F1>
            IF F-EXIT-CH = X"15"
                 PERFORM DELETE-OLD-RECORDS
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
      *<PAGE-DOWN>
            IF F-EXIT-CH = X"0C"
                 MOVE WS-DCNUM TO WS-NUMBER
                 PERFORM START-RECORD
                 PERFORM READ-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-003 THRU GET-006
                 GO TO GET-999.
      *<ALT-PAGE-DOWN>
            IF F-EXIT-CH = X"8C"
                 PERFORM START-PAGE-DOWN-RECORD
                 PERFORM READ-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-003 THRU GET-006
                 GO TO GET-999.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-000.
      *<RETURN - READ THE DEBTOR RECORD>
            PERFORM READ-RECORD.
            IF NEW-NO = "Y"
                MOVE WS-AREA      TO DC-AREA
                MOVE WS-AREA-NAME TO DC-AREA-NAME.
            IF DC-AREA-NAME = " "
                MOVE WS-AREA-NAME TO DC-AREA-NAME.
      *      GO TO GET-005.
        GET-003.
            MOVE "ACC"             TO F-FIELDNAME
            MOVE 3                 TO F-CBFIELDNAME
            MOVE DC-ACCOUNT-NUMBER TO F-NAMEFIELD
            MOVE 7                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACCNAME"         TO F-FIELDNAME
            MOVE 7                 TO F-CBFIELDNAME
            MOVE DR-NAME           TO F-NAMEFIELD
            MOVE 40                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            MOVE "NAME"  TO F-FIELDNAME
            MOVE 4       TO F-CBFIELDNAME
            MOVE DC-NAME TO F-NAMEFIELD
            MOVE 40      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "SALESMAN"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE DR-SALESMAN TO F-NAMEFIELD
            MOVE 1           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
           
            IF NEW-NO NOT = "Y" 
             IF DC-SALESMAN = " "
               MOVE
              "NB.  THIS CALL SCHEDULE DOES NOT HAVE A SALESMAN NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE
              "     <ESC> THEN <F10> TO DELETE THIS RECORD."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
            IF NEW-NO NOT = "Y" 
             IF DR-SALESMAN = " "
               MOVE
              "NB.  THIS ACCOUNT DOES NOT HAVE A SALESMAN NUMBER."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE
              "PRESS <ESC> THEN <F10> TO DELETE THIS RECORD."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
               
            IF DC-SALESMAN NOT = " "
             IF DC-SALESMAN NOT = DR-SALESMAN   
               MOVE "OLDCODEDESC"       TO F-FIELDNAME
               MOVE 11                  TO F-CBFIELDNAME
               MOVE "OLD SALESMAN CODE" TO F-NAMEFIELD
               MOVE 17                  TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               MOVE "OLDCODE"       TO F-FIELDNAME
               MOVE 7               TO F-CBFIELDNAME
               MOVE DC-SALESMAN     TO F-NAMEFIELD
               MOVE 1               TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               GO TO GET-006.

               MOVE "OLDCODEDESC"       TO F-FIELDNAME
               MOVE 11                  TO F-CBFIELDNAME
               MOVE "                 " TO F-NAMEFIELD
               MOVE 17                  TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               MOVE "OLDCODE" TO F-FIELDNAME
               MOVE 7         TO F-CBFIELDNAME
               MOVE " "       TO F-NAMEFIELD
               MOVE 1         TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA.
       GET-006.
            MOVE "AREA"  TO F-FIELDNAME
            MOVE 4       TO F-CBFIELDNAME
            MOVE DC-AREA TO F-NAMEFIELD
            MOVE 2       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            IF DC-AREA-NAME = " "
                MOVE WS-AREA-NAME TO DC-AREA-NAME.
            MOVE "AREA-NAME"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE DC-AREA-NAME TO F-NAMEFIELD
            MOVE 40           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                GO TO FILL-999.
            IF NEW-NO = "X"
             IF F-EXIT-CH NOT = X"0C"
                GO TO FILL-999.
            PERFORM ERROR1-020
            PERFORM ERROR-020
            MOVE 2310 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE 2410 TO POS
            DISPLAY WS-MESSAGE AT POS.
            MOVE 2510 TO POS
            DISPLAY WS-MESSAGE AT POS.

            MOVE 2410 TO POS
            DISPLAY 
            "ENTER THE CONTACT NAME/S & CELL NUMBERS FOR THE CLIENT...." 
                  AT POS.

            MOVE "                                   " TO F-NAMEFIELD.
            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DC-NAME.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF DC-NAME = " "
               MOVE "THIS FIELD MUST BE > SPACES, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-006
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.
       FILL-015.
            MOVE 2410 TO POS
            DISPLAY 
            "ENTER THE WEEK & DAY NUMBER FOR THIS CLIENT..............." 
                  AT POS.

            MOVE "                    " TO F-NAMEFIELD.
            MOVE "AREA" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-AREA.
            MOVE WS-AREA TO DC-AREA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF DC-AREA = 0
               MOVE "THIS FIELD MAY NOT BE BLANK, ENTER AN AREA."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-015.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-006
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-015.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-015.
       FILL-020.
            MOVE 2410 TO POS
            DISPLAY 
            "ENTER THE AREA-NAME FOR THE WEEK/DAY NUMBER..............." 
                  AT POS.

            MOVE "                                   " TO F-NAMEFIELD.
            MOVE "AREA-NAME" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DC-AREA-NAME WS-AREA-NAME.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF DC-AREA-NAME = " "
               MOVE "THIS FIELD MUST BE > SPACES, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
               PERFORM GET-003 THRU GET-006
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       DELETE-RECORD SECTION.
       DDR-000.
            IF NEW-NO = "Y"
                MOVE "NEW-NO = 'Y'" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO DDR-999.
       DDR-010.
            DELETE DRCONT-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-DC-ST1 NOT = 0
               MOVE "DR-CONT FILE BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DC-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       DELETE-ALL-RECORDS SECTION.
       DALL-000.
           PERFORM ERROR1-020
           PERFORM ERROR-020
           PERFORM CLEAR-010
           MOVE 2710 TO POS
           DISPLAY 
           "ENTER 'Y' TO CONTINUE DELETE OF ALL YOUR CALL RECORDS : [ ]"
            AT POS
           ADD 57 TO POS
           
           MOVE "N"  TO CDA-DATA
           MOVE 24   TO CDA-ROW
           MOVE 66   TO CDA-COL
           MOVE 1    TO CDA-DATALEN
           MOVE "A"  TO CDA-ATTR
           MOVE 3    TO CDA-COLOR
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-YN.

           IF WS-YN NOT = "N" AND NOT = "Y"
              DISPLAY " " AT 3079 WITH BELL
              GO TO DALL-000.
           IF WS-YN = "N"
              GO TO DALL-999.
              
           PERFORM ERROR1-020
           
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
           
           MOVE 0 TO DC-ACCOUNT-NUMBER.
           START DRCONT-MASTER KEY NOT < DC-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DC-ST1 NOT = 0
                MOVE "DR-CONT FILE BUSY ON START, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DC-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                PERFORM ERROR-MESSAGE
                MOVE 0 TO WS-DC-ST1
                GO TO DALL-999.
       DALL-005.
           READ DRCONT-MASTER NEXT WITH LOCK
             AT END
               MOVE 0 TO DC-ACCOUNT-NUMBER
                         WS-NUMBER
                         WS-DCNUM
               PERFORM ERROR-020
               MOVE 2610 TO POS
               DISPLAY WS-MESSAGE AT POS
               GO TO DALL-999.
           IF WS-DC-ST1 NOT = 0
              MOVE "CALL SCHEDULE BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DC-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                PERFORM ERROR-MESSAGE
                MOVE 0 TO WS-DC-ST1
                GO TO DALL-005.
            IF DC-SALESMAN NOT = WS-DC-SMAN
               GO TO DALL-005.
            MOVE 2610 TO POS
            DISPLAY "CALL SCHEDULE ACCOUNT BEING DELETED:" AT POS
            ADD 36 TO POS
            DISPLAY DC-ACCOUNT-NUMBER AT POS.
       DALL-010.
            DELETE DRCONT-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-DC-ST1 NOT = 0
              MOVE "CALL SCHEDULE BUSY ON DELETE-ALL, 'ESC' TO RETRY."
              TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DC-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO DALL-010.
            GO TO DALL-005.
       DALL-999.
           EXIT.
      *
       DELETE-OLD-RECORDS SECTION.
       DOLD-000.
           PERFORM ERROR1-020
           PERFORM ERROR-020
           PERFORM CLEAR-010
           MOVE 2710 TO POS
           DISPLAY 
           "ENTER 'Y' TO CONTINUE DELETE OF YOUR NON CALL RECORDS : [ ]"
            AT POS
           ADD 57 TO POS

           MOVE "N"  TO CDA-DATA
           MOVE 24   TO CDA-ROW
           MOVE 66   TO CDA-COL
           MOVE 1    TO CDA-DATALEN
           MOVE "A"  TO CDA-ATTR
           MOVE 3    TO CDA-COLOR
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-YN.
           
           IF WS-YN NOT = "N" AND NOT = "Y"
              DISPLAY " " AT 3079 WITH BELL
              GO TO DOLD-000.
           IF WS-YN = "N"
              GO TO DOLD-999.
              
           PERFORM ERROR1-020

           START DEBTOR-MASTER KEY NOT < DR-KEY
               INVALID KEY NEXT SENTENCE.
               
           PERFORM ERROR-020
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
           
           MOVE 0 TO DC-ACCOUNT-NUMBER.
           START DRCONT-MASTER KEY NOT < DC-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DC-ST1 NOT = 0
                MOVE
                 "CALL SCHEDULE NOT THERE ON START, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DC-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO DOLD-999.
       DOLD-005.
           READ DRCONT-MASTER NEXT WITH LOCK
             AT END
               MOVE 0 TO DC-ACCOUNT-NUMBER
                         WS-NUMBER
                         WS-DCNUM
               PERFORM ERROR-020
               MOVE 2610 TO POS
               DISPLAY WS-MESSAGE AT POS
               GO TO DOLD-999.
           IF WS-DC-ST1 NOT = 0
              MOVE "CALL SCHEDULE BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-DC-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-DC-ST1
              GO TO DOLD-005.
              
            MOVE 2610 TO POS
            DISPLAY "CALL SCHEDULE ACCOUNT BEING READ:" AT POS
            ADD 33 TO POS
            DISPLAY DC-ACCOUNT-NUMBER AT POS.

            IF DC-SALESMAN NOT = WS-DC-SMAN
               GO TO DOLD-005.
       DOLD-006.
           MOVE DC-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
           READ DEBTOR-MASTER
                 INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                GO TO DOLD-010.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO DOLD-006.
           IF DC-SALESMAN = DR-SALESMAN
              GO TO DOLD-005.
            MOVE 2710 TO POS
            DISPLAY "CALL SCHEDULE ACCOUNT BEING DELETED:" AT POS
            ADD 37 TO POS
            DISPLAY DC-ACCOUNT-NUMBER AT POS.
       DOLD-010.
            DELETE DRCONT-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-DC-ST1 NOT = 0
               MOVE "DR-CONT FILE BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DC-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO DOLD-010.
            GO TO DOLD-005.
       DOLD-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO DC-NAME
                         DC-AREA-NAME.
             MOVE 0   TO DC-AREA.
       CLSC-999.
             EXIT.      
      *
       REWRITE-RECORD SECTION.
       RDR-010.
            IF NEW-NO = "Y"
               GO TO RDR-020.
            REWRITE DRCONT-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-DC-ST1 NOT = 0
                MOVE "DR-CONTACT BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DC-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DC-ST1
                GO TO RDR-010.
            GO TO RDR-999.
       RDR-020.
            WRITE DRCONT-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-DC-ST1 NOT = 0
                MOVE "DR-CONTACT BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DC-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DC-ST1
                GO TO RDR-020.
       RDR-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE 0 TO WS-DC-ST1.
           START DRCONT-MASTER KEY NOT < DC-KEY
                INVALID KEY NEXT SENTENCE.
       RD-010.
           READ DRCONT-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-DC-ST1 = 23 OR 35 OR 49
                MOVE " " TO DC-NAME
                MOVE 0   TO WS-DC-ST1
                            DC-AREA
                MOVE "Y" TO NEW-NO
                MOVE WS-NUMBER TO DC-ACCOUNT-NUMBER WS-DCNUM
                GO TO RD-900.
           IF WS-DC-ST1 NOT = 0
                MOVE "DR-CONTACT BUSY ON READ-LOCK, 'ESC' TO RETRY"
                  TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DC-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DC-ST1
                GO TO RD-010.
           MOVE "N"               TO NEW-NO
           MOVE DC-ACCOUNT-NUMBER TO WS-NUMBER WS-DCNUM
           MOVE DC-AREA           TO WS-AREA.
           IF DC-AREA-NAME NOT = " "
               MOVE DC-AREA-NAME  TO WS-AREA-NAME.
       RD-900.
           PERFORM READ-DEBTOR-FILE.
            IF DC-SALESMAN NOT = " "
             IF DR-SALESMAN NOT = DC-SALESMAN
               PERFORM GET-003 THRU GET-006
               MOVE 
          "THE DEBTOR FILE & CALL SCHEDULE HAVE DIFFERENT SALESMAN No's"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE
             "<PAGE-DOWN> TO ENTER NEW No, <F10> TO DELETE THIS RECORD."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE.
           MOVE DR-SALESMAN TO DC-SALESMAN.
       RD-999.
           EXIT.
      *
       READ-DEBTOR-FILE SECTION.
       RDF-010.
           MOVE DC-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
                MOVE "X" TO NEW-NO
                MOVE "DR RECORD BUSY ON START, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO RD-999.
       RDF-015.
           READ DEBTOR-MASTER
                 INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE "X" TO NEW-NO
                MOVE "DR RECORD NOT THERE ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO RDF-999.
           IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DR RECORD BUSY ON READ, PRESS 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO RDF-015.
       RDF-999.
           EXIT.
      *
       START-RECORD SECTION.
       STR-000.
           MOVE WS-NUMBER TO DC-ACCOUNT-NUMBER.
           START DRCONT-MASTER KEY NOT < DC-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-DC-ST1 NOT = 0
                MOVE "DRCONT NOT THERE ON START, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DC-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DC-ST1.
       STR-999.
             EXIT.
      *
       START-PAGE-DOWN-RECORD SECTION.
       STNXR-000.
           MOVE WS-NUMBER      TO WS-DC-SMAN-NUM
           MOVE WS-DC-SMAN-NUM TO DC-SALESMAN
           MOVE 0              TO DC-AREA.
       
           START DRCONT-MASTER KEY NOT < DC-ALT-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-DC-ST1 NOT = 0
                MOVE 
          "DRCONT RECORD NOT THERE ON START-PAGE-DOWN, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DC-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DC-ST1.
       STNXR-999.
             EXIT.
      *
       READ-NEXT SECTION.
       RNX-005.
           READ DRCONT-MASTER NEXT WITH LOCK
             AT END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO DC-ACCOUNT-NUMBER
                         WS-NUMBER
                         WS-DCNUM
               MOVE "X" TO NEW-NO
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF WS-DC-ST1 NOT = 0
              MOVE "DR-CONT BUSY ON READ-NEXT RNX-005, 'ESC' TO RETRY."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DC-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DC-ST1
                PERFORM START-RECORD
                GO TO RNX-005.
           MOVE DC-ACCOUNT-NUMBER TO WS-NUMBER WS-DCNUM.
           MOVE DC-AREA           TO WS-AREA.
           IF DC-AREA-NAME NOT = " "
               MOVE DC-AREA-NAME  TO WS-AREA-NAME.
           MOVE "N" TO NEW-NO.
       RNX-900.
           PERFORM READ-DEBTOR-FILE.
            IF DC-SALESMAN NOT = " "
             IF DR-SALESMAN NOT = DC-SALESMAN
               PERFORM GET-003 THRU GET-006
               MOVE 
          "THE DEBTOR FILE & CALL SCHEDULE HAVE DIFFERENT SALESMAN No's"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE
             "<PAGE-DOWN> TO ENTER NEW No, <F10> TO DELETE THIS RECORD."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE.
           MOVE DR-SALESMAN TO DC-SALESMAN.
       RNX-999.
           EXIT.
      *
        OPEN-FILES SECTION.
        OPEN-000.
            OPEN I-O DRCONT-MASTER.
            IF WS-DC-ST1 NOT = 0
               MOVE 0 TO WS-DC-ST1
               MOVE "DR-CONTACT FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
        OPEN-005.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY." 
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
            MOVE Ws-Forms-Name   TO F-FILENAME
            MOVE Ws-cbForms-name TO F-CBFILENAME.
            MOVE "DrContact"     TO F-FORMNAME.
            MOVE 9               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE DRCONT-MASTER
                  DEBTOR-MASTER.
      *      STOP RUN.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldAccount".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
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
