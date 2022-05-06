        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrNameIq.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           
       WORKING-STORAGE SECTION.
       77  WS-SHORTNAME         PIC X(10) VALUE " ".
       77  WS-WORK              PIC X(25) VALUE " ".
       77  WS-ACC               PIC X(7) VALUE " ".
       77  WS-1ST               PIC X VALUE " ".
       77  SUB-DIS              PIC 9(4) VALUE 0.
       77  WS-MIDDLE            PIC X(79) VALUE " ".
       01  WS-NAME-PRINT.
           03  WS-NAME          PIC X(79) VALUE " ".
       01  WS-LINE-PRINT.
      *     03  FILLER           PIC X(10) VALUE " ".
           03  WS-ADD           PIC X(26) VALUE " ".
           03  WS-PHONE         PIC X(40) VALUE " ".
           03  WS-DISC          PIC X(2) VALUE " ".
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-SPLIT-ACCOUNT.
           03  WS-SP-1          PIC X VALUE " ".
           03  WS-SP-REST       PIC X(24) VALUE " ".
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

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONT-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           PERFORM READ-MASTER-DISPLAY.
           GO TO CONT-010.
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
                MOVE "INVALID KEY PRESSED, MUST BE <Return> OR <F8>."
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-000.

            PERFORM ERROR-020.
            MOVE SPACES TO ALPHA-RATE.
            IF F-EXIT-CH = X"1D"
               MOVE F-NAMEFIELD TO ALPHA-RATE
               PERFORM CHECK-ENTRY
               MOVE ALPHA-RATE TO WS-ACC
            ELSE
               MOVE F-NAMEFIELD TO WS-SHORTNAME.
      *      MOVE 0           TO F-EXIT-CH.
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
               MOVE WS-SHORTNAME TO DR-NAME WS-SPLIT-INPUT-ACC
               START DEBTOR-MASTER KEY NOT < DR-ALT-KEY
                   INVALID KEY NEXT SENTENCE.
            IF F-EXIT-CH = X"1D"
               MOVE WS-ACC       TO DR-ACCOUNT-NUMBER
               START DEBTOR-MASTER KEY NOT < DR-KEY
                   INVALID KEY NEXT SENTENCE.
                   
            MOVE 0 TO F-EXIT-CH.
            IF WS-DEBTOR-ST1 NOT = 0
                MOVE "BAD START, 'ESC' TO EXIT AND RETRY."
                TO WS-MESSAGE
      *          PERFORM ERROR1-000
      *          MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
      *          MOVE WS-DEBTOR TO WS-MESSAGE
      *          PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                PERFORM CLEAR-MIDDLE
                CLOSE DEBTOR-MASTER
                GO TO READ-999.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "NO DEBTORS WITH THAT SHORT NAME, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-STATUS
               PERFORM CLEAR-MIDDLE
               CLOSE DEBTOR-MASTER
               GO TO READ-999.
 
            MOVE 0 TO SUB-2 SUB-3.
            MOVE 800 TO SUB-DIS.
        READ-010.
            IF F-EXIT-CH = 0
              READ DEBTOR-MASTER NEXT
                AT END
                MOVE "END OF FILE, 'ESC' TO THE CLEAR SCREEN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM CLEAR-MIDDLE
                CLOSE DEBTOR-MASTER
                GO TO READ-999.
            IF F-EXIT-CH = 1
              READ DEBTOR-MASTER PREVIOUS.
      *          AT END
      *          MOVE "BEGINNING OF FILE, 'ESC' TO THE CLEAR SCREEN."
      *          TO WS-MESSAGE
      *          PERFORM ERROR-MESSAGE
      *          PERFORM CLEAR-MIDDLE
      *          CLOSE DEBTOR-MASTER
      *          GO TO READ-999.
        
            IF WS-DEBTOR-ST1 = 91
                MOVE 0 TO WS-DEBTOR-STATUS
                CLOSE DEBTOR-MASTER
                MOVE "THERE IS A SYSTEM ERROR91, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO READ-999.
            IF WS-1ST = "Y"
              IF DR-NAME NOT = " "
                MOVE DR-NAME TO WS-SPLIT-INPUT-ACC
                MOVE "N" TO WS-1ST.
            MOVE DR-NAME TO WS-SPLIT-ACCOUNT.
      *      IF WS-SP-1 NOT = WS-SP-I-1
      *          MOVE 2710 TO POS
      *          DISPLAY "No More DEBTORS With That SHORT-NAME," AT POS
      *          MOVE 2820 TO POS
      *          DISPLAY "Press 'ESC' To Clear The Screen !" AT POS
      *          CALL "&LOCKKBD" USING F-FIELDNAME
      *          PERFORM ERROR1-020
      *          PERFORM ERROR-020
      *          PERFORM CLEAR-MIDDLE
      *          CLOSE DEBTOR-MASTER
      *          GO TO READ-999.
        READ-020.
            ADD 4 TO SUB-3.
        READ-025.
            IF SUB-3 > 20
                MOVE 2910 TO POS
             DISPLAY "Press 'PgDn' For More, Or 'PgUp' to See Previous,"
                 AT POS
                MOVE 3020 TO POS
                DISPLAY "'ESC' To Clear The Screen !" AT POS
                MOVE " " TO WS-SHORTNAME
                PERFORM USER-FILL-FIELD.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.

            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-MIDDLE
                MOVE 0 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 3 TO SUB-3
                MOVE 800 TO SUB-DIS.
            IF F-EXIT-CH = X"05"
                PERFORM CLEAR-MIDDLE
                MOVE 1 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 3 TO SUB-3
                MOVE 800 TO SUB-DIS.
            IF F-EXIT-CH = X"07"
                MOVE " " TO WS-SHORTNAME
                MOVE 0   TO WS-DEBTOR-STATUS
                PERFORM CLEAR-MIDDLE
                CLOSE DEBTOR-MASTER
                GO TO READ-999.
            IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                     AND NOT = X"07" AND NOT = 0 AND NOT = 1
                GO TO READ-025.
            IF DR-ACCOUNT-NUMBER = 0
                GO TO READ-010.
      *      DISPLAY "ÿAF" AT SUB-DIS.
            ADD 1 TO SUB-DIS.
            DISPLAY DR-ACCOUNT-NUMBER AT SUB-DIS WITH REVERSE-VIDEO.
            ADD 10 TO SUB-DIS.
      *      DISPLAY "ÿAA" AT SUB-DIS.
      *      ADD 1 TO SUB-DIS.

            MOVE DR-NAME          TO WS-NAME
            DISPLAY WS-NAME-PRINT AT SUB-DIS
            ADD 100               TO SUB-DIS
            MOVE " "              TO WS-NAME-PRINT
            MOVE DR-ADDRESS1      TO WS-ADD
            MOVE DR-TELEPHONE     TO WS-PHONE
            MOVE DR-DISCOUNT-CODE TO WS-DISC
            DISPLAY WS-LINE-PRINT AT SUB-DIS
            ADD 100               TO SUB-DIS
            MOVE " "              TO WS-LINE-PRINT
            MOVE DR-ADDRESS2      TO WS-ADD
            DISPLAY WS-LINE-PRINT AT SUB-DIS
            ADD 100               TO SUB-DIS
            MOVE " "              TO WS-LINE-PRINT
            MOVE DR-ADDRESS3      TO WS-ADD
            DISPLAY WS-LINE-PRINT AT SUB-DIS
            ADD 89                TO SUB-DIS
            MOVE " "              TO WS-LINE-PRINT
            DISPLAY WS-LINE-PRINT AT SUB-DIS

            GO TO READ-010.
        READ-999.
            EXIT.
      *
       CLEAR-MIDDLE SECTION.
       CM-010.
           MOVE " " TO WS-MIDDLE
           MOVE 801 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 901 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 1001 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 1101 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 1201 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 1301 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 1401 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 1501 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 1601 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 1701 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 1801 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 1901 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 2001 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 2101 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 2201 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 2301 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 2401 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 2501 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS
           MOVE 2601 TO SUB-DIS
           DISPLAY WS-MIDDLE AT SUB-DIS.
       CM-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-0000.
            GO TO OPEN-001.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO OPEN-000.
       OPEN-001.
            MOVE Ws-Forms-Name   TO F-FILENAME
            MOVE Ws-cbForms-name TO F-CBFILENAME.
            MOVE "DrNameIq"      TO F-FORMNAME
            MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE DEBTOR-MASTER.
      *    STOP RUN.
           EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
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
