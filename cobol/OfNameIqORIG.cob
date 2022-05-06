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
       77  WS-SHORTNAME         PIC X(7) VALUE " ".
       77  WS-WORK              PIC X(25) VALUE " ".
       77  WS-ACC               PIC 9(7) VALUE 0.
       77  WS-1ST               PIC X VALUE " ".
       77  SUB-DIS              PIC 9(6) VALUE 0.
       77  SUB-333              PIC 9(4) VALUE 0.
       77  WS-MIDDLE            PIC X(79) VALUE " ".
       01  WS-LASTNAME-PRINT.
           03  WS-FIRST.
              05  WS-LINE          PIC 99.
              05  FILLER           PIC X VALUE " ".
              05  WS-LASTNAME      PIC X(18) VALUE " ".
           03  FILLER           PIC X VALUE " ".
           03  WS-FIRSTNAME     PIC X(14) VALUE " ".
           03  WS-ADD1          PIC X(26) VALUE " ".
           03  WS-PHONE1        PIC X(18) VALUE " ".
       01  WS-LINE-PRINT.
           03  FILLER           PIC X(26) VALUE " ".
           03  WS-ADD           PIC X(26) VALUE " ".
           03  WS-PHONE         PIC X(25) VALUE " ".
       01  WS-READ-LINES.
           03  WS-NAMES OCCURS 25.
              05  WS-NAME-SEARCH    PIC X(50).
       01  WS-OFIS-STATUS.
           03  WS-OFIS-ST1      PIC 99.
       01  WS-SPLIT-ACCOUNT.
           03  WS-SP-1          PIC X VALUE " ".
           03  WS-SP-REST       PIC X(49) VALUE " ".
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
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           PERFORM READ-MASTER-DISPLAY.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
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
               MOVE WS-SHORTNAME  TO OFIS-CATEGORY
               START OFIS-FILE KEY NOT < OFIS-CATEGORY
                   INVALID KEY NEXT SENTENCE.
                   
            MOVE 0 TO F-EXIT-CH.
            IF WS-OFIS-ST1 NOT = 0
                MOVE "BAD START, RECORD IN USE TRY AGAIN LATER !!!!!"
                 TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-OFIS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM CLEAR-MIDDLE
                CLOSE OFIS-FILE
                GO TO READ-999.
            IF WS-OFIS-ST1 = 23 OR 35 OR 49
              MOVE "BAD START, TRY ENTERING A CONTACT THAT EXISTS!!!"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-OFIS-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM CLEAR-MIDDLE
                CLOSE OFIS-FILE
                GO TO READ-999.
            MOVE 0 TO SUB-2 SUB-333.
            MOVE 800 TO SUB-DIS.
            MOVE 0 TO F-EXIT-CH.
        READ-010.
            IF F-EXIT-CH = 0
              READ OFIS-FILE NEXT
                AT END
                MOVE "END OF FILE, 'ESC' TO THE CLEAR SCREEN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM CLEAR-MIDDLE
                CLOSE OFIS-FILE
                GO TO READ-999.
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
      *         DISPLAY "No More CREDITORS With That SHORT-NAME," AT POS
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
            ADD 1 TO SUB-333.
            IF SUB-333 NOT > 0
               MOVE 1 TO SUB-333.
        READ-025.
            IF SUB-333 > 20
                MOVE 2910 TO POS
                DISPLAY "Press 'PgDn' For More, 'PgUp' For Previous,"
                  AT POS
                MOVE 3020 TO POS
                DISPLAY "Or 'ESC' To Clear The Screen !" AT POS
                MOVE "SHORTNAME" TO F-FIELDNAME
                MOVE 9           TO F-CBFIELDNAME
                MOVE " "         TO F-NAMEFIELD WS-SHORTNAME
                MOVE 7           TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM USER-FILL-FIELD
                MOVE 7           TO F-CBFIELDLENGTH
                PERFORM READ-FIELD-ALPHA.
            MOVE 2910 TO POS
            DISPLAY "                                        " AT POS
            MOVE 3020 TO POS
            DISPLAY "                                        " AT POS.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-MIDDLE
                MOVE 0 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-333
                MOVE 800 TO SUB-DIS.
            IF F-EXIT-CH = X"05"
                PERFORM CLEAR-MIDDLE
                MOVE 1 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-333
                MOVE 800 TO SUB-DIS.
            IF F-EXIT-CH = X"07"
                MOVE " " TO WS-SHORTNAME
                MOVE 0   TO WS-OFIS-STATUS
                PERFORM CLEAR-MIDDLE
                CLOSE OFIS-FILE
                GO TO READ-999.
      * <F6>
            IF F-EXIT-CH = X"1A"
                MOVE F-NAMEFIELD TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO WS-ACC
                MOVE ALPHA-RATE TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE WS-ACC TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE WS-NAME-SEARCH (WS-ACC) TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM OPEN-020 THRU OPEN-900
                PERFORM DISPLAY-FORM.

            IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                     AND NOT = X"07" AND NOT = 0     AND NOT = 1
                     AND NOT = X"1A"
                GO TO READ-025.
      *      IF OFIS-FIRSTNAME = " "
      *          GO TO READ-010.

            ADD 1 TO SUB-333.

            MOVE SUB-333              TO WS-LINE.
            MOVE OFIS-NAME            TO WS-LASTNAME
            MOVE OFIS-FIRSTNAME       TO WS-FIRSTNAME
            MOVE OFIS-ADDRESS1        TO WS-ADD1
            MOVE OFIS-PHONE           TO WS-PHONE1
            DISPLAY WS-LASTNAME-PRINT AT SUB-DIS.
      *      ADD 10                    TO SUB-DIS.
            
      *      ADD 100                 TO SUB-DIS
      *      MOVE OFIS-ADDRESS1      TO WS-ADD
      *      MOVE OFIS-PHONE         TO WS-PHONE
      *      DISPLAY WS-LINE-PRINT   AT SUB-DIS
            
      *      ADD 100                 TO SUB-DIS
      *      MOVE " "                TO WS-LINE-PRINT
      *      MOVE OFIS-ADDRESS2      TO WS-ADD
      *      DISPLAY WS-LINE-PRINT   AT SUB-DIS
            
      *      ADD 100                 TO SUB-DIS
      *      MOVE " "                TO WS-LINE-PRINT
      *      MOVE OFIS-ADDRESS3      TO WS-ADD
      *      DISPLAY WS-LINE-PRINT   AT SUB-DIS
            
            ADD 87                  TO SUB-DIS
            MOVE " "                TO WS-LINE-PRINT
            DISPLAY WS-LINE-PRINT   AT SUB-DIS.
            
      *      MOVE SUB-DIS TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.

            GO TO READ-010.
        READ-999.
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
           DISPLAY WS-MIDDLE AT POS.
       CM-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-0000.
            GO TO OPEN-010.
       OPEN-000.
            OPEN INPUT OFIS-FILE.
            IF WS-OFIS-ST1 NOT = 0
               MOVE "OFIS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OFIS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE Ws-OfisData TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-OFIS-ST1
               GO TO OPEN-000.
       OPEN-010.
            Move Ws-Forms-Name   TO F-FileName
            Move Ws-cbForms-name TO F-CbFileName.
            MOVE "OfNameIq"      TO F-FORMNAME
            MOVE 8               TO F-CBFORMNAME.
            
            GO TO OPEN-900.
       OPEN-020.
            Move Ws-Forms-Name   TO F-FileName
            Move Ws-cbForms-name TO F-CbFileName.
            MOVE "OfNameMt"      TO F-FORMNAME
            MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
        END-OFF SECTION.
        END-000.
           CLOSE OFIS-FILE.
      *    STOP RUN.
           EXIT PROGRAM.
        END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
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
