        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrMailRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMail".
           SELECT PRINT-FILE ASSIGN TO "/ctools/spl/DrMailList"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SPL-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdMailList.
      *
       FD  PRINT-FILE.
       01  PRINT-REC               PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-FOUND             PIC X VALUE " ".
       77  W-CALC               PIC 9(8) VALUE 0.
       77  COL-CNT              PIC 9(3) VALUE 0.
       77  WS-ACCNOBEGIN        PIC 9(7) VALUE 0.
       77  WS-ACCNOEND          PIC 9(7) VALUE 0.
       77  WS-END-OF-FILE       PIC X VALUE " ".
       77  WS-SALES-CODE        PIC 99 VALUE 0.
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-RANGE1            PIC 9(7) VALUE 0.
       77  WS-RANGE2            PIC 9(7) VALUE 0.
       77  WS-RANGE3            PIC 9(2) VALUE 0.
       77  WS-RANGE4            PIC X VALUE " ".
       01  Ws-DrMail-STATUS.
           03  Ws-DrMail-ST1    PIC 99.
       01  WS-Spl-STATUS.
           03  WS-Spl-ST1       PIC 99.
       01  PLINE1.
           03  FILLER           PIC X(3) VALUE " ".
           03  PNAME1           PIC X(40) VALUE " ".
           03  PNAME2           PIC X(40) VALUE " ".
           03  PNAME3           PIC X(40) VALUE " ".
           03  FILLER           PIC X(9) VALUE " ".
       01  PLINE2.
           03  FILLER           PIC X(3) VALUE " ".
           03  PADD11           PIC X(40) VALUE " ".
           03  PADD12           PIC X(40) VALUE " ".
           03  PADD13           PIC X(40) VALUE " ".
           03  FILLER           PIC X(9) VALUE " ".
       01  PLINE3.
           03  FILLER           PIC X(3) VALUE " ".
           03  PADD21           PIC X(40) VALUE " ".
           03  PADD22           PIC X(40) VALUE " ".
           03  PADD23           PIC X(40) VALUE " ".
           03  FILLER           PIC X(9) VALUE " ".
       01  PLINE4.
           03  FILLER           PIC X(3) VALUE " ".
           03  PADD31           PIC X(40) VALUE " ".
           03  PADD32           PIC X(40) VALUE " ".
           03  PADD33           PIC X(40) VALUE " ".
           03  FILLER           PIC X(9) VALUE " ".
       01  PLINE5.
           03  FILLER           PIC X(3) VALUE " ".
           03  P-ATT1           PIC X(10) VALUE " ".
           03  P-NAME1          PIC X(30) VALUE " ".
           03  P-ATT2           PIC X(10) VALUE " ".
           03  P-NAME2          PIC X(30) VALUE " ".
           03  P-ATT3           PIC X(10) VALUE " ".
           03  P-NAME3          PIC X(30) VALUE " ".
           03  FILLER           PIC X(9) VALUE " ".
       01  PLINE6.
           03  FILLER           PIC X(3) VALUE " ".
           03  P-NUM1           PIC Z(6)9.
           03  FILLER           PIC X(33) VALUE " ".
           03  P-NUM2           PIC Z(6)9.
           03  FILLER           PIC X(33) VALUE " ".
           03  P-NUM3           PIC Z(6)9.
           03  FILLER           PIC X(39) VALUE " ".
       Copy "WsDateInfo".
       Copy "WStore".
      *
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
           PERFORM CLEAR-SCREEN
           PERFORM DISPLAY-FORM.
       CONTROL-010.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           IF WS-Spl-ST1 NOT = 0
               MOVE 0 TO WS-Spl-ST1
               MOVE "The Print File is already Open, 'ESC' To Exit."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM END-OFF.
       CONTROL-015.
      *     MOVE WTELL-PAUSE TO PRINT-REC
      *     WRITE PRINT-REC
           MOVE " " TO PRINT-REC.
        CONTROL-020.
           PERFORM GET-DATA.
           PERFORM PRINT-LABELS.
      *     PERFORM CHECK-SPOOLER.
           PERFORM END-OFF.
      *
       GET-DATA SECTION.
       GET-030.
            MOVE 0 TO WS-ACCNOBEGIN
                      WS-ACCNOEND.
            MOVE "RANGE1" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF F-EXIT-CH = X"0A"
               GO TO GET-035
            ELSE
               GO TO GET-030.
       GET-035.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-NUMERIC.
            MOVE F-NAMEFIELDNUM TO W-CALC.
            PERFORM JUSTIFY-FIELDS.
            MOVE W-CALC TO WS-ACCNOBEGIN.
            IF WS-ACCNOBEGIN NOT > 0
                GO TO GET-040.
       GET-040.
            MOVE "RANGE2" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-030.
            IF F-EXIT-CH = X"0A"
               GO TO GET-045
            ELSE
               GO TO GET-040.
       GET-045.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-NUMERIC.
            MOVE F-NAMEFIELDNUM TO W-CALC.
            PERFORM JUSTIFY-FIELDS.
            MOVE W-CALC TO WS-ACCNOEND.
            IF WS-ACCNOEND NOT > 0
                GO TO GET-040.
       GET-999.
            EXIT.
      *
       PRINT-LABELS SECTION.
       PR-000.
           MOVE 2510 TO POS
           DISPLAY "The Report is being compiled........." AT POS.
           MOVE WS-ACCNOBEGIN TO ML-NUMBER.
           START MAIL-MASTER KEY NOT < ML-KEY.
           MOVE " " TO WS-MESSAGE.
           PERFORM ERROR-020.
       PR-010.
           IF WS-END-OF-FILE = " "
               PERFORM READ-MASTER.
       PR-020.
           WRITE PRINT-REC FROM PLINE1 AFTER 1
           MOVE " " TO PRINT-REC PLINE1
           WRITE PRINT-REC FROM PLINE2 AFTER 1
           MOVE " " TO PRINT-REC PLINE2
           WRITE PRINT-REC FROM PLINE3 AFTER 1
           MOVE " " TO PRINT-REC PLINE3
           WRITE PRINT-REC FROM PLINE4 AFTER 1
           MOVE " " TO PRINT-REC PLINE4
           WRITE PRINT-REC FROM PLINE5 AFTER 2
           MOVE " " TO PRINT-REC PLINE5
           WRITE PRINT-REC FROM PLINE5 AFTER 1
           MOVE " " TO PRINT-REC PLINE6
           WRITE PRINT-REC AFTER 2.
           IF WS-END-OF-FILE = " "
              GO TO PR-010.
       PR-900.
           MOVE 2510 TO POS
           DISPLAY "                                           " AT POS.
           CLOSE Mail-Master.
      *     MOVE WTELL-PAUSE TO PRINT-REC
      *     WRITE PRINT-REC
           MOVE " " TO PRINT-REC
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       PR-999.
           EXIT.
      *
       READ-MASTER SECTION.
       RM-000.
           READ MAIL-MASTER NEXT
              AT END NEXT SENTENCE.
           IF WS-DRMAIL-ST1 = 10
              PERFORM END-OFF.
           IF WS-DRMAIL-ST1 NOT = 0
              MOVE 0 TO WS-DRMAIL-ST1
              MOVE "MAIL RECORD BUSY ON READ-NEXT, 'ESC' TO RE-READ."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RM-000.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.
           IF ML-NUMBER < WS-ACCNOBEGIN
              GO TO RM-000.
           IF ML-NUMBER > WS-ACCNOEND
              MOVE "1" TO WS-END-OF-FILE
              MOVE " " TO PNAME1  PNAME2  PNAME3
                          PADD11  PADD12  PADD13
                          PADD21  PADD22  PADD23
                          PADD31  PADD32  PADD33
                          P-ATT1  P-ATT2  P-ATT3
                          P-NAME1 P-NAME2 P-NAME3
                          P-NUM1 P-NUM2 P-NUM3
              GO TO RM-999.

           PERFORM CHECK-INITIAL-LENGTH.

           MOVE ML-CO-NAME   TO PNAME1
           MOVE ML-ADDRESS1  TO PADD11
           MOVE ML-ADDRESS2  TO PADD21
           MOVE ML-POST-CODE TO PADD31
           MOVE "ATTENTION:" TO P-ATT1
           MOVE ML-NAME      TO P-NAME1
           MOVE ML-NUMBER    TO P-NUM1.
       RM-010.
           READ Mail-Master NEXT
              AT END NEXT SENTENCE.
           IF Ws-DrMail-ST1 = 10
              OR ML-NUMBER > WS-ACCNOEND
                 MOVE "1" TO WS-END-OF-FILE
                 MOVE " " TO PNAME2 PNAME3
                             PADD12 PADD13
                             PADD22 PADD23
                             PADD32 PADD33
                             P-ATT2  P-ATT3
                             P-NAME2 P-NAME3
                             P-NUM2 P-NUM3
                 GO TO RM-999.
           IF Ws-DrMail-ST1 NOT = 0
              MOVE 0 TO WS-DrMail-ST1
              MOVE "MAIL RECORD BUSY ON READ-NEXT, 'ESC TO RE-TRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RM-010.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.

           PERFORM CHECK-INITIAL-LENGTH.

           MOVE ML-CO-NAME   TO PNAME2
           MOVE ML-ADDRESS1  TO PADD12
           MOVE ML-ADDRESS2  TO PADD22
           MOVE ML-POST-CODE TO PADD32
           MOVE "ATTENTION:" TO P-ATT2
           MOVE ML-NAME      TO P-NAME2
           MOVE ML-NUMBER    TO P-NUM2.
       RM-020.
           READ Mail-Master NEXT
              AT END NEXT SENTENCE.
           IF Ws-DrMail-ST1 = 10
              OR ML-NUMBER > WS-ACCNOEND
                 MOVE "1" TO WS-END-OF-FILE
                 MOVE " " TO PNAME3
                             PADD13
                             PADD23
                             PADD33
                             P-ATT3
                             P-NAME3
                             P-NUM3
                 GO TO RM-999.
           IF Ws-DrMail-ST1 NOT = 0
              MOVE 0 TO WS-DrMail-ST1
              MOVE "MAIL RECORD BUSY ON READ-NEXT, 'ESC' TO RE-TRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RM-020.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.

           PERFORM CHECK-INITIAL-LENGTH.

           MOVE ML-CO-NAME   TO PNAME3
           MOVE ML-ADDRESS1  TO PADD13
           MOVE ML-ADDRESS2  TO PADD23
           MOVE ML-POST-CODE TO PADD33
           MOVE "ATTENTION:" TO P-ATT3
           MOVE ML-NAME      TO P-NAME3
           MOVE ML-NUMBER    TO P-NUM3.
       RM-999.
           EXIT.
      *
       CHECK-INITIAL-LENGTH SECTION.
       CIL-005.
           MOVE 0 TO SUB-1.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
           IF ML-INITIALS = " "
              GO TO CIL-999.
           MOVE ML-INITIALS TO ALPHA-RATE.
       CIL-010.
           ADD 1 TO SUB-1.
           IF AL-RATE (SUB-1) NOT = " "
            IF SUB-1 < 10
              GO TO CIL-010.
           IF SUB-1 < 10
              ADD 1 TO SUB-1
            IF AL-RATE (SUB-1) = " "
              SUBTRACT 1 FROM SUB-1
            ELSE
             IF SUB-1 < 10
               GO TO CIL-010.
           SUBTRACT 1 FROM SUB-1
           MOVE SUB-1   TO SUB-3
           MOVE 10      TO SUB-2.
       CIL-020.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2).
           IF SUB-1 = 1
             GO TO CIL-030.
           SUBTRACT 1 FROM SUB-1 SUB-2
           GO TO CIL-020.
       CIL-030.
           MOVE 12 TO SUB-2
           MOVE SPACES  TO ALPHA-RATE
           MOVE ML-NAME TO ALPHA-RATE.
       CIL-040.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2).
           IF SUB-2 NOT > 30
              ADD 1 TO SUB-1 SUB-2
              GO TO CIL-040.
           COMPUTE SUB-2 = 9 - SUB-3
           MOVE 1 TO SUB-1
           MOVE SPACES TO ALPHA-RATE.
       CIL-050.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 NOT > 40
              ADD 1 TO SUB-1 SUB-2
              GO TO CIL-050.
           MOVE ALPHA-RATE TO ML-NAME.
       CIL-999.
           EXIT.
      *
       CHECK-SPOOLER SECTION.
       CP-000.
           PERFORM OPEN-SPOOLER-FILES
           PERFORM QUEUE-PRINT-FILE
           MOVE SPACE TO W-SPOOLST
           MOVE SPACE TO W-SPOOLST2
           PERFORM CHECK-FOR-PAUSE.
             
           MOVE "Load Gum Labels, Then Press 'ESC'." TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.

           PERFORM SEND-CONTROL-CHAR.
      *
      * PRINTING COMPLETE
      *
           PERFORM CHECK-PAUSE-PRINT.
       
           MOVE "Load Normal Paper, Then Press 'ESC'." TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.

           PERFORM SEND-CONTROL-CHAR.
       CP-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
       END-900.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN INPUT MAIL-MASTER.
           IF WS-DRMAIL-ST1 NOT = 0
              MOVE 0 TO WS-DRMAIL-ST1
              MOVE "MAILLIST FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "DrMailRp"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
       Copy "ReadFieldNumeric".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "JustifyFields".
       Copy "CheckForPause".
       Copy "QueuePrintFileMail".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
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
