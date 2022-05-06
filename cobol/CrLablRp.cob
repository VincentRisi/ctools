        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrLablRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectCrMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCreditor.

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
       77  WS-FOR-LOCAL         PIC X VALUE " ".
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-RANGE1            PIC 9(7) VALUE 0.
       77  WS-RANGE2            PIC 9(7) VALUE 0.
       77  WS-RANGE3            PIC 9(2) VALUE 0.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1  PIC 99.
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
           03  PADD41           PIC X(40) VALUE " ".
           03  PADD42           PIC X(40) VALUE " ".
           03  PADD43           PIC X(40) VALUE " ".
           03  FILLER           PIC X(9) VALUE " ".
       Copy "WsDateInfo".
       Copy "WStore".
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
           PERFORM OPEN-DATA-FILES
           PERFORM CLEAR-SCREEN
           PERFORM DISPLAY-FORM.
       CONTROL-010.
           PERFORM GET-DATA.
           MOVE "/ctools/spl/CrLabelPrt" TO WS-PRINTER.
           OPEN OUTPUT PRINT-FILE.
      *     MOVE WTELL-PAUSE TO PRINT-REC.
      *     WRITE PRINT-REC.
           MOVE " " TO PRINT-REC.
           PERFORM PRINT-LABELS.
           IF WS-CREDITOR-ST1 = 88
              GO TO CONTROL-020.
      *     PERFORM CHECK-SPOOLER.
       CONTROL-020.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
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
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-ACCNOBEGIN.
            IF WS-ACCNOBEGIN NOT > 0
                GO TO GET-030.
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
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-ACCNOEND.
            IF WS-ACCNOEND NOT > 0
                GO TO GET-040.
       GET-050.
            MOVE "RANGE3" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-040.
            IF F-EXIT-CH = X"1B" OR = X"0A"
               GO TO GET-900
            ELSE
               GO TO GET-030.
       GET-900.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-FOR-LOCAL.
            IF WS-FOR-LOCAL NOT = "F" AND NOT = "L" AND NOT = " "
               GO TO GET-050.
       GET-999.
            EXIT.
      *
        PRINT-LABELS SECTION.
        PR-000.
           MOVE WS-ACCNOBEGIN TO CR-ACCOUNT-NUMBER.
           START CREDITOR-MASTER KEY NOT < CR-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 NOT = 0
              MOVE "BAD START ON CREDITOR RECORD, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 88 TO WS-CREDITOR-ST1
              GO TO PR-999.
           PERFORM ERROR-020.
        PR-010.
           IF WS-END-OF-FILE = " "
               PERFORM READ-MASTER
           ELSE
               GO TO PR-900.
        PR-020.
           WRITE PRINT-REC FROM PLINE1 AFTER 1
           MOVE " " TO PRINT-REC PLINE1
           WRITE PRINT-REC FROM PLINE2 AFTER 1
           MOVE " " TO PRINT-REC PLINE2
           WRITE PRINT-REC FROM PLINE3 AFTER 1
           MOVE " " TO PRINT-REC PLINE3
           WRITE PRINT-REC FROM PLINE4 AFTER 1
           MOVE " " TO PRINT-REC PLINE4
           WRITE PRINT-REC FROM PLINE5 AFTER 1
           MOVE " " TO PRINT-REC PLINE5
           WRITE PRINT-REC AFTER 4.

           GO TO PR-010.
         PR-900.
      *     MOVE WTELL-PAUSE TO PRINT-REC.
      *     WRITE PRINT-REC.
           MOVE " " TO PRINT-REC.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE CREDITOR-MASTER.
           PERFORM ERROR-020
           MOVE 2510 TO POS
           DISPLAY WS-MESSAGE AT POS.
        PR-999.
           EXIT.
      *
       READ-MASTER SECTION.
       RM-000.
           READ CREDITOR-MASTER NEXT
              AT END NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 10
                 MOVE "1" TO WS-END-OF-FILE
                 GO TO RM-999.
           IF WS-CREDITOR-ST1 NOT = 0
              MOVE
               "CREDITOR BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CREDITOR-ST1
              GO TO RM-000.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.

           MOVE 2510 TO POS
           DISPLAY "Account Number :" AT POS
           ADD 17 TO POS
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
        
           IF CR-ACCOUNT-NUMBER < WS-ACCNOBEGIN
              GO TO RM-000.
           IF CR-ACCOUNT-NUMBER > WS-ACCNOEND
              MOVE "1" TO WS-END-OF-FILE
              MOVE " " TO PNAME1 PNAME2 PNAME3
                          PADD11 PADD12 PADD13
                          PADD21 PADD22 PADD23
                          PADD31 PADD32 PADD33
                          PADD41 PADD42 PADD43
              GO TO RM-999.
           IF WS-FOR-LOCAL NOT = " "
            IF WS-FOR-LOCAL NOT = CR-FOREIGN-LOCAL
                 GO TO RM-000.

           MOVE CR-NAME     TO PNAME1.
           MOVE CR-ADDRESS1 TO PADD11.
           MOVE CR-ADDRESS2 TO PADD21.
           IF CR-ADDRESS3 = " "
               MOVE CR-POST-CODE TO PADD31
               MOVE " " TO PADD41
               GO TO RM-010
           ELSE
               MOVE CR-ADDRESS3 TO PADD31.
           MOVE CR-POST-CODE TO PADD41.
       RM-010.
           READ CREDITOR-MASTER NEXT
              AT END NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 10
              OR CR-ACCOUNT-NUMBER > WS-ACCNOEND
                 MOVE "1" TO WS-END-OF-FILE
                 MOVE " " TO PNAME2 PNAME3
                             PADD12 PADD13
                             PADD22 PADD23
                             PADD32 PADD33
                             PADD42 PADD43
                 GO TO RM-999.
           IF WS-CREDITOR-ST1 NOT = 0
              MOVE 
               "CREDITOR BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CREDITOR-ST1
              GO TO RM-010.

           MOVE 2510 TO POS
           DISPLAY "Account Number :" AT POS
           ADD 17 TO POS
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
        
           IF WS-FOR-LOCAL NOT = " "
              IF WS-FOR-LOCAL NOT = CR-FOREIGN-LOCAL
                 GO TO RM-010.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.

           MOVE CR-NAME     TO PNAME2.
           MOVE CR-ADDRESS1 TO PADD12.
           MOVE CR-ADDRESS2 TO PADD22.
           IF CR-ADDRESS3 = " "
               MOVE CR-POST-CODE TO PADD32
               MOVE " "          TO PADD42
               GO TO RM-020
           ELSE
               MOVE CR-ADDRESS3 TO PADD32.
           MOVE CR-POST-CODE TO PADD42.
       RM-020.
           READ CREDITOR-MASTER NEXT
              AT END NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 10
              OR CR-ACCOUNT-NUMBER > WS-ACCNOEND
                 MOVE "1" TO WS-END-OF-FILE
                 MOVE " " TO PNAME3
                             PADD13
                             PADD23
                             PADD33
                             PADD43
                 GO TO RM-999.
           IF WS-CREDITOR-ST1 NOT = 0
              MOVE 
               "CREDITOR BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CREDITOR-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CREDITOR-ST1
              GO TO RM-020.

           MOVE 2510 TO POS
           DISPLAY "Account Number :" AT POS
           ADD 17 TO POS
           DISPLAY CR-ACCOUNT-NUMBER AT POS.
        
           IF WS-FOR-LOCAL NOT = " "
              IF WS-FOR-LOCAL NOT = CR-FOREIGN-LOCAL
                 GO TO RM-020.
           IF WS-MESSAGE NOT = " "
              PERFORM ERROR-020.

           MOVE CR-NAME     TO PNAME3.
           MOVE CR-ADDRESS1 TO PADD13.
           MOVE CR-ADDRESS2 TO PADD23.
           IF CR-ADDRESS3 = " "
               MOVE CR-POST-CODE TO PADD33
               MOVE " " TO PADD43
               GO TO RM-999
           ELSE
               MOVE CR-ADDRESS3 TO PADD33.
           MOVE CR-POST-CODE TO PADD43.
       RM-999.
           EXIT.
      *
       CHECK-SPOOLER SECTION.
       CP-000.
           PERFORM OPEN-SPOOLER-FILES.
           MOVE 3010 TO POS
           DISPLAY "Queuing Print File............" AT POS.

           PERFORM QUEUE-PRINT-FILE.
           MOVE SPACE TO W-SPOOLST.
           MOVE SPACE TO W-SPOOLST2.
           MOVE 3010 TO POS
           DISPLAY "Checking Queue for File......." AT POS.
           PERFORM CHECK-FOR-PAUSE.
             
           MOVE "Load Gum Labels, Then Press 'ESC'." TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.

           PERFORM SEND-CONTROL-CHAR.
           MOVE 3010 TO POS
           DISPLAY "File Printing................." AT POS.
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
       QUEUE-PRINT-FILE SECTION.
           MOVE 20                     TO W-CBSPOOLERFILESPEC
           MOVE "/ctools/spl/CrLabelPrt" TO W-SPOOLERFILESPEC.
           CALL "ADDQUEUEENTRY" USING
                                W-ERROR
                                W-QUEUENAME
                                W-QUEUENAMELENGTH
                                W-Q-NO-SERVER
                                W-PRIORITY
                                W-QUEUETYPE
                                W-QUEUEENTRYBUFFERSEND
                                W-QUEUEENTRYBUFFERLENGTH
                                W-DATE-TIME
                                W-REPEATTIME.
       QUEUE-PRINT-EXIT.
           EXIT.
      *
       OPEN-SPOOLER-FILES SECTION.
           MOVE SPACE                  TO W-QUEUEENTRYBUFFERSEND.
           MOVE "/ctools/spl/CrLabelPrt" TO W-FILENAME.
           MOVE X"00"                  TO W-FDELETEAFTERPROC.
           MOVE 0     TO W-CBFORMNAME.
           MOVE 0     TO W-CBWHEELNAME.
           MOVE 1     TO W-BYTE1.
           MOVE 0     TO W-BYTE2.
           MOVE X"00" TO W-BPRINTMODE.
           MOVE X"00" TO W-FALIGNFORM.
           MOVE X"00" TO W-FSECURITYMODE.
           MOVE "SPL" TO W-QUEUENAME.
           MOVE 3     TO W-QUEUENAMELENGTH.
           MOVE 0     TO W-QUEUEENTRYHANDLE.
           MOVE 123   TO W-QUEUEENTRYBUFFERLENGTH.
           MOVE 11    TO W-STATUSBLOCKLENGTH.
           MOVE "PARALLELCONTROL" TO W-PAR-QUEUENAME.
           MOVE 15                TO W-PAR-QUEUENAMELENGTH.
           MOVE "SPOOLERSTATUS"   TO W-STATUS-QUEUENAME.
           MOVE 13                TO W-STATUS-QUEUENAMELEN.
           MOVE "PARALLEL"  TO W-PRINTERNAME.
           MOVE 8           TO W-PRINTERNAMELEN.
           MOVE X"FF"       TO W-Q-NO-SERVER.
           MOVE 5           TO W-PRIORITY.
           MOVE 1           TO W-QUEUETYPE.
           MOVE X"00000000" TO W-DATE-TIME.
           MOVE 0           TO W-REPEATTIME.
           MOVE 150         TO W-DELAY.
           MOVE 0           TO W-ZERO.
           MOVE 255         TO W-SPOOLST-LEN.
       OPEN-SP-999.
           EXIT.
      *
        OPEN-DATA-FILES SECTION.
        OPEN-DATA-000.
           OPEN I-O CREDITOR-MASTER.
           IF WS-CREDITOR-ST1 NOT = 0
              MOVE 0 TO WS-CREDITOR-ST1
              MOVE "CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-DATA-000.
        OPEN-DATA-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrLablRp"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-DATA-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "JustifyFields".
       Copy "CheckForPause".
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
      * END-OF-JOB
       
