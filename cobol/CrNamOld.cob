        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrNamOld.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT CREDITOROLD-MASTER ASSIGN TO Ws-CrMasterOld
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CROLD-KEY
               FILE STATUS IS WS-CREDITOROLD-STATUS
               ALTERNATE RECORD KEY IS CROLD-ALT-KEY WITH DUPLICATES.
      *
        DATA DIVISION.
        FILE SECTION.
            COPY ChlfdCreditorOld.

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
           03  FILLER           PIC X(26) VALUE " ".
           03  WS-ADD           PIC X(26) VALUE " ".
           03  WS-PHONE         PIC X(20) VALUE " ".
       01  WS-CREDITOROLD-STATUS.
           03  WS-CREDITOROLD-ST1    PIC 99.
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
            MOVE 0420 TO POS
            DISPLAY "*** OLD / DELETED RECORDS ONLY ***" AT POS.
            
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
                MOVE WS-SHORTNAME TO CROLD-NAME WS-SPLIT-INPUT-ACC
               START CREDITOROLD-MASTER KEY NOT < CROLD-ALT-KEY
                  INVALID KEY NEXT SENTENCE.
            IF F-EXIT-CH = X"1D"
               MOVE WS-ACC       TO CROLD-ACCOUNT-NUMBER
               START CREDITOROLD-MASTER KEY NOT < CROLD-KEY
                   INVALID KEY NEXT SENTENCE.
                   
            MOVE 0 TO F-EXIT-CH.
            IF WS-CREDITOROLD-ST1 NOT = 0
                MOVE 3010 TO POS
                DISPLAY "RECORD IN USE TRY AGAIN LATER !!!!!" AT POS
                ADD 30 TO POS
                DISPLAY WS-CREDITOROLD-ST1 AT POS
                MOVE 0 TO WS-CREDITOROLD-STATUS
                PERFORM CLEAR-MIDDLE
                CLOSE CREDITOROLD-MASTER
                GO TO READ-999.
            IF WS-CREDITOROLD-ST1 = 23 OR 35 OR 49
                MOVE 3010 TO POS
                DISPLAY "TRY ONE THAT EXISTS!!!" AT POS
                MOVE 0 TO WS-CREDITOROLD-STATUS
                PERFORM CLEAR-MIDDLE
                CLOSE CREDITOROLD-MASTER
                GO TO READ-999.
            MOVE 0 TO SUB-2 SUB-3.
            MOVE 800 TO SUB-DIS.
        READ-010.
            IF F-EXIT-CH = 0
              READ CREDITOROLD-MASTER NEXT
                AT END
                MOVE "END OF FILE, 'ESC' TO THE CLEAR SCREEN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM CLEAR-MIDDLE
                CLOSE CREDITOROLD-MASTER
                GO TO READ-999.
            IF F-EXIT-CH = 1
              READ CREDITOROLD-MASTER PREVIOUS.
            IF WS-CREDITOROLD-ST1 = 91
                MOVE 0 TO WS-CREDITOROLD-STATUS
                CLOSE CREDITOROLD-MASTER
                MOVE "THERE IS A SYSTEM ERROR, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO READ-999.
            IF WS-1ST = "Y"
              IF CROLD-NAME NOT = " "
                MOVE CROLD-NAME TO WS-SPLIT-INPUT-ACC
                MOVE "N" TO WS-1ST.
            MOVE CROLD-NAME TO WS-SPLIT-ACCOUNT.
      *      IF WS-SP-1 NOT = WS-SP-I-1
      *        MOVE 2910 TO POS
      *        DISPLAY "No More CREDITORS With That SHORT-NAME," AT POS
      *        MOVE 2820 TO POS
      *        DISPLAY "Press 'ESC' To Clear The Screen !" AT POS
      *        CALL "&LOCKKBD" USING F-FIELDNAME
      *        MOVE 2910 TO POS
      *        DISPLAY "                                      " AT POS
      *        MOVE 2820 TO POS
      *        DISPLAY "                                      " AT POS
      *        PERFORM CLEAR-MIDDLE
      *        GO TO READ-999.
        READ-020.
            ADD 4 TO SUB-3.
        READ-025.
            IF SUB-3 > 18
                MOVE 2910 TO POS
                DISPLAY "Press 'PgDn' For More, Or 'PgUp' For Previous,"
                 AT POS
                MOVE 3020 TO POS
                DISPLAY "'ESC' To Clear The Screen !" AT POS
                MOVE " " TO WS-SHORTNAME
                PERFORM USER-FILL-FIELD.
            PERFORM ERROR1-020.
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
                MOVE 0 TO WS-CREDITOROLD-STATUS
                PERFORM CLEAR-MIDDLE
                CLOSE CREDITOROLD-MASTER
                GO TO READ-999.
            IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                     AND NOT = X"07" AND NOT = 0     AND NOT = 1
                GO TO READ-025.
            IF CROLD-ACCOUNT-NUMBER = 0
                GO TO READ-010.
      *      DISPLAY "ÿAN" AT SUB-DIS.
            ADD 1 TO SUB-DIS.
            DISPLAY CROLD-ACCOUNT-NUMBER AT SUB-DIS WITH REVERSE-VIDEO.
            ADD 10 TO SUB-DIS.
      *      DISPLAY "ÿAA" AT SUB-DIS.
      *      ADD 1 TO SUB-DIS.

            MOVE CROLD-NAME          TO WS-NAME
            DISPLAY WS-NAME-PRINT AT SUB-DIS
            ADD 100 TO SUB-DIS
            MOVE CROLD-ADDRESS1      TO WS-ADD
            MOVE CROLD-TELEPHONE     TO WS-PHONE
            DISPLAY WS-LINE-PRINT AT SUB-DIS
            ADD 100 TO SUB-DIS
            MOVE " " TO WS-LINE-PRINT
            MOVE CROLD-ADDRESS2 TO WS-ADD
            DISPLAY WS-LINE-PRINT AT SUB-DIS
            ADD 100 TO SUB-DIS
            MOVE " " TO WS-LINE-PRINT
            MOVE CROLD-ADDRESS3 TO WS-ADD
            DISPLAY WS-LINE-PRINT AT SUB-DIS
            ADD 89 TO SUB-DIS
            MOVE " " TO WS-LINE-PRINT
            DISPLAY WS-LINE-PRINT AT SUB-DIS.

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
            OPEN I-O CREDITOROLD-MASTER.
            IF WS-CREDITOROLD-ST1 NOT = 0
               MOVE "CREDITOROLD BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-CREDITOROLD-ST1
               GO TO  OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrNameIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
        END-OFF SECTION.
        END-000.
           CLOSE CREDITOROLD-MASTER.
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
