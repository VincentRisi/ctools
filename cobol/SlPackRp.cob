       IDENTIFICATION DIVISION.
       PROGRAM-ID. SlPackRp.
       AUTHOR.  CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. B20.
       OBJECT-COMPUTER. B20.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
         Copy "SelectSlRegister".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdRegister.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(80).
      *
       WORKING-STORAGE SECTION.
       77  WS-COMMENT           PIC X(25) VALUE " ".
       77  WS-INVOICE           PIC 9(6) VALUE 0.
       77  WS-COPIES            PIC 9(2) VALUE 0.
       77  WS-COPIES-PRINTED    PIC 9(2) VALUE 0.
       77  WS-PARCEL            PIC 9(2) VALUE 0.
       77  WS-PARCEL-PRINTED    PIC 9(2) VALUE 0.
       77  WS-SEND-CHARS        PIC X VALUE "N".
       77  WS-TYPE              PIC 9 VALUE 0.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  PARCEL-LINE.
           03  PL-DIG1              PIC X.
           03  PL-REST.
              05  PL-DESC           PIC X(12) VALUE " ".
              05  PL-REST-DIG1      PIC X.
              05  PL-NO1            PIC XX.
              05  PL-FILLER         PIC X VALUE "/".
              05  PL-NO2            PIC XX.
              05  PL-DIG2           PIC X.
       01  PLINE2.
           03  P-DIG1               PIC X.
           03  P-REST.
              05  P-DESC            PIC X(12) VALUE " ".
              05  P-REST-DIG1       PIC X.
              05  P-ADD             PIC X(30) VALUE " ".
              05  P-DIG2            PIC X.
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
           Move Spaces to Ws-Printer.
           Move 5 To Ws-PrinterNumber (21).
      *     Move 2 To Ws-PrinterNumber (21).
           Move 4 To Ws-PrinterType (21).
           Copy "PrinterSpecial".
       CONTROL-001.
           PERFORM CLEAR-SCREEN.
           PERFORM OPEN-FILES.
       CONT-010.
           MOVE 0310 TO POS.
           DISPLAY "** SALES INVOICE LABEL PRINT PROGRAM **" AT POS
           MOVE 0410 TO POS
           DISPLAY "***************************************" AT POS.
           
           MOVE 0710 TO POS
           DISPLAY "IF THE PRINTER CANNOT BE SET UP FOR THE CORRECT"
             AT POS
           MOVE 0810 TO POS
           DISPLAY "PAGE LENGTH OF 4INCHES FROM THE FRONT PANEL"
             AT POS
           MOVE 0910 TO POS
           DISPLAY "ENTER 'Y' TO THE QUESTION ASKED OTHERWISE LEAVE"
             AT POS
           MOVE 1010 TO POS
           DISPLAY "THE ANSWER AS 'N' TO IGNORE THIS QUESTION." AT POS.
       CONT-012.
           MOVE 1210 TO POS
           DISPLAY 
           "SEND 4INCH SET UP CHARACTERS TO THE PRINTER, [ ] " AT POS.
           
           MOVE 1410 TO POS
           DISPLAY "THE PAPER WILL FEED UP SOME AND THEN STOP.  PLEASE"
             AT POS
           MOVE 1510 TO POS
           DISPLAY 
           "THEN SET THE PAPER IN THE CORRECT POSITION TO PRINT."
             AT POS.
           MOVE 1710 TO POS
           DISPLAY "IF FOR ANY REASON YOU SWITCH OFF THE PRINTER POWER"
             AT POS.
           MOVE 1810 TO POS
           DISPLAY "YOU MUST RERUN THIS PROCESS TO SET UP PAGE LENGTH."
             AT POS.
           MOVE 1256 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 55        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SEND-CHARS.

           IF W-ESCAPE-KEY = 3
              PERFORM END-OFF.
           IF WS-SEND-CHARS NOT = "Y" AND NOT = "N"
              GO TO CONT-012.
           IF WS-SEND-CHARS = "N"
              GO TO CONT-015.
      *NEXT LINES USED FOR SETTING PAGE LENGTH TO 4 INCH 
      *
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           MOVE WS-PRINT-4 TO PRINT-REC
           WRITE PRINT-REC
           MOVE " "        TO PRINT-REC.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           
           PERFORM CONTROL-000.
           
            CALL "C$SLEEP" USING 1.
      *     MOVE 10 TO W-DELAY.
      *     CALL "&DELAY" USING W-ERROR
      *                         W-DELAY.
           PERFORM CLEAR-SCREEN.
       CONT-015.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           PERFORM PRINT-LABEL.
           GO TO CONT-015.
      *
       END-OFF SECTION.
       END-000.
           CLOSE INCR-REGISTER.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       PRINT-LABEL SECTION.
       PR-005.
           MOVE 0  TO WS-COPIES-PRINTED
                      WS-PARCEL-PRINTED.
      *     MOVE 10 TO W-DELAY.
       PR-012.
      *     MOVE "PR-005" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      *     MOVE WS-PRINTER TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.

      *     MOVE WS-PRINTER-SAVE TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
           
           PERFORM CONTROL-000
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
      *NEW SECTION TO PRINT 4" CHAR BEFORE THE PAGE IS PRINTED
      *SO THAT ANY PRINTER CAN BE USED WITHOUT HAVING TO SET UP
      *THE PAGE SIZE ON THE PRINTER.  ESPECIALLY USEFUL WHEN THE 
      *PRINTER CANNOT BE SET TO 4" FROM ITS FRONT PANEL OR DIP SWITCHES
      *
      *NEXT TWO LINES USED FOR SETTING PAGE LENGTH TO 4 INCH 
      *
      *     MOVE WS-PRINT-4      TO PARCEL-LINE
      *     WRITE PRINT-REC FROM PARCEL-LINE.
      *     MOVE " "             TO PARCEL-LINE.
           
           MOVE WS-PRINT-COMP   TO PRINT-REC
           WRITE PRINT-REC.
           MOVE " "             TO PLINE2 PRINT-REC
           MOVE INCR-NAME       TO P-REST
           MOVE Ws-Print-Bold   TO P-DIG1
           WRITE PRINT-REC FROM PLINE2.
           
           MOVE WS-PRINT-NORMAL TO PRINT-REC
           WRITE PRINT-REC.

           MOVE " "             TO PLINE2 PRINT-REC
           MOVE INCR-DEL1       TO P-REST
           WRITE PRINT-REC FROM PLINE2
           MOVE " "             TO PLINE2 PRINT-REC
           MOVE INCR-DEL2       TO P-REST
           WRITE PRINT-REC FROM PLINE2
           MOVE " "             TO PLINE2 PRINT-REC
           MOVE INCR-DEL3       TO P-REST
           MOVE Ws-Print-Unbold TO P-DIG2
           WRITE PRINT-REC FROM PLINE2
           MOVE " "             TO PLINE2 PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC.

           MOVE Ws-Print-Bold   TO P-REST-DIG1
           MOVE "ORDER     #:"  TO P-DESC
           MOVE INCR-PORDER     TO P-ADD
           MOVE Ws-Print-Unbold TO P-DIG2
           WRITE PRINT-REC FROM PLINE2.
           
           MOVE " "             TO PLINE2 PRINT-REC
           MOVE Ws-Print-Bold   TO P-REST-DIG1.
           IF WS-TYPE = 1
               MOVE "INVOICE   #:"  TO P-DESC.
           IF WS-TYPE = 3
               MOVE "REPAIR    #:"  TO P-DESC.
           IF WS-TYPE = 4
               MOVE "P/SLIP    #:"  TO P-DESC.
           MOVE INCR-INVOICE    TO P-ADD
           MOVE Ws-Print-Unbold TO P-DIG2
           WRITE PRINT-REC FROM PLINE2.
           
           MOVE " "             TO PLINE2 PRINT-REC
           MOVE Ws-Print-Bold   TO P-REST-DIG1
           MOVE "INTERNAL  #:"  TO P-DESC
           MOVE INCR-BO-INV-NO  TO P-ADD
           MOVE Ws-Print-Unbold TO P-DIG2
           WRITE PRINT-REC FROM PLINE2.
           
           MOVE " "             TO PLINE2 PRINT-REC.
           MOVE Ws-Print-Bold   TO P-REST-DIG1
           MOVE "DELIVERY   :"  TO P-DESC
           MOVE INCR-DELIVERY   TO P-ADD
           MOVE Ws-Print-Unbold TO P-DIG2
           WRITE PRINT-REC FROM PLINE2.
           
           MOVE " "             TO PLINE2 PRINT-REC
           MOVE Ws-Print-Bold   TO P-REST-DIG1
           MOVE "CONTACT    :"  TO P-DESC
           MOVE Ws-Print-Unbold TO P-DIG2
           MOVE INCR-CONTACT    TO P-ADD
           WRITE PRINT-REC FROM PLINE2.
           
           MOVE " "               TO PLINE2 PRINT-REC
           ADD 1                  TO WS-PARCEL-PRINTED
           MOVE Ws-Print-Bold     TO PL-REST-DIG1
           MOVE "PARCEL    #:"    TO PL-DESC
           MOVE WS-PARCEL-PRINTED TO PL-NO1
           MOVE WS-PARCEL         TO PL-NO2
           MOVE Ws-Print-Unbold   TO PL-DIG2
           WRITE PRINT-REC FROM PARCEL-LINE.
           
           MOVE " "               TO PRINT-REC
           SUBTRACT 1 FROM WS-PARCEL-PRINTED
           WRITE PRINT-REC
           WRITE PRINT-REC
           MOVE Ws-Print-Bold   TO P-DIG1
           MOVE WS-COMMENT      TO P-REST
           WRITE PRINT-REC FROM PLINE2.

           MOVE " "             TO PLINE2 PRINT-REC
           WRITE PRINT-REC
           MOVE "SUPPLIED BY:"  TO P-DESC
           MOVE Ws-Print-Unbold TO P-DIG2
           WRITE PRINT-REC FROM PLINE2.

           MOVE " "             TO PLINE2 PRINT-REC
           WRITE PRINT-REC BEFORE PAGE
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.

            CALL "C$SLEEP" USING 1.
      *     CALL "&DELAY" USING W-ERROR 
      *                         W-DELAY.

           ADD 1 TO WS-PARCEL-PRINTED.
           IF WS-PARCEL-PRINTED NOT = WS-PARCEL
               GO TO PR-012
           ELSE
               ADD 1  TO WS-COPIES-PRINTED
               MOVE 0 TO WS-PARCEL-PRINTED.

           IF WS-COPIES NOT = WS-COPIES-PRINTED
               GO TO PR-012.
       PR-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
      ***********************
      *TYPE 1=INVOICE       *
      *     3=REPAIR        *
      *     4=P/SLIP        *
      ***********************
            MOVE 1 TO INCR-TRANS
                        WS-TYPE.
       GET-001.
            MOVE "TYPE"  TO F-FIELDNAME.
            MOVE 4       TO F-CBFIELDNAME.
            MOVE WS-TYPE TO F-NAMEFIELD
            MOVE 1       TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            GO TO GET-010.
       GET-002. 
            MOVE "TYPE" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            MOVE 1            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO INCR-TRANS
                                   WS-TYPE.
            IF WS-TYPE NOT = 1 AND NOT = 3 AND NOT = 4
                MOVE "THE ONLY VALID ENTRY IS 1,3 OR 4, RE-ENTER"
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-002.
       GET-010.
            MOVE "                                     " TO F-NAMEFIELD.
            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-002.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM READ-NEXT-INVOICE
                MOVE INCR-INVOICE TO WS-INVOICE
                PERFORM GET-012 THRU GET-013
                GO TO GET-010.
            IF F-EXIT-CH = X"05"
                PERFORM READ-PREV-INVOICE
                MOVE INCR-INVOICE TO WS-INVOICE
                PERFORM GET-012 THRU GET-013
                GO TO GET-010.
            MOVE 6            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-INVOICE.
            PERFORM READ-INVOICE-REGISTER.
           IF WS-INCR-ST1 = 23 OR 35 OR 49 OR = 10
                MOVE "ENTER AN EXISTING INVOICE NUMBER." TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-010.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
            GO TO GET-013.
       GET-012.
            MOVE "INVOICE"  TO F-FIELDNAME.
            MOVE 7          TO F-CBFIELDNAME.
            MOVE WS-INVOICE TO F-NAMEFIELD.
            MOVE 6          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-013.
            MOVE "PORDER"    TO F-FIELDNAME.
            MOVE 6           TO F-CBFIELDNAME.
            MOVE INCR-PORDER TO F-NAMEFIELD.
            MOVE 25          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CONAME"  TO F-FIELDNAME.
            MOVE 6         TO F-CBFIELDNAME.
            MOVE INCR-NAME TO F-NAMEFIELD.
            MOVE 40        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD1"    TO F-FIELDNAME.
            MOVE 4         TO F-CBFIELDNAME.
            MOVE INCR-DEL1 TO F-NAMEFIELD.
            MOVE 25        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD2"    TO F-FIELDNAME.
            MOVE 4         TO F-CBFIELDNAME.
            MOVE INCR-DEL2 TO F-NAMEFIELD.
            MOVE 25        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD3"    TO F-FIELDNAME.
            MOVE 4         TO F-CBFIELDNAME.
            MOVE INCR-DEL3 TO F-NAMEFIELD.
            MOVE 25        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CONTACT"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE INCR-CONTACT TO F-NAMEFIELD.
            MOVE 20           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELIVERY"    TO F-FIELDNAME.
            MOVE 8             TO F-CBFIELDNAME.
            MOVE INCR-DELIVERY TO F-NAMEFIELD.
            MOVE 20            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-015.
            IF F-EXIT-CH NOT = X"01"
                 GO TO GET-070.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "CONAME" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                PERFORM DISPLAY-FORM
                GO TO GET-010.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR-NAME.
       GET-020.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "ADD1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-015.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR-DEL1.
       GET-030.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "ADD2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-020.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR-DEL2.
       GET-040.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "ADD3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-030.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR-DEL3.
       GET-050.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "CONTACT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-040.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR-CONTACT.
       GET-060.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "DELIVERY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-050.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR-DELIVERY.
       GET-070.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "COMMENT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-060.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-COMMENT.
       GET-080.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "COPIES" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-070.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-COPIES
                                 F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH. 
            PERFORM WRITE-FIELD-ANALYSIS.
            IF NUMERIC-RATE NOT > 0
               MOVE "ENTER A NUMBER GREATER THAN ZERO." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-080.
            IF NUMERIC-RATE > 5
               MOVE 
         "ARE YOU SURE YOU WANT SO MANY LABELS, 'ESC' THEN RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
       GET-090.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "PARCELS" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-080.
            MOVE 2         TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-PARCEL
                                 F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH. 
            PERFORM WRITE-FIELD-ANALYSIS.
            IF WS-PARCEL NOT > 0
               MOVE "ENTER A NUMBER GREATER THAN ZERO." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-090.
            IF NUMERIC-RATE > 10
               MOVE 
             "ARE YOU SURE ABOUT THIS NUMBER, 'ESC' THEN RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
            IF WS-MESSAGE NOT = " "
                PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       READ-INVOICE-REGISTER SECTION.
           MOVE WS-INVOICE TO INCR-INVOICE.
           MOVE WS-TYPE    TO INCR-TRANS.
           START INCR-REGISTER KEY NOT < INCR-KEY.
       RIR-005.
           READ INCR-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-ST1 = 23 OR 35 OR 49
               MOVE "NO SUCH INVOICE TO PRINT, 'ESC' TO EXIT."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RIR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "INV/CR. REGISTER BUSY ON READ, RIR-005" 
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RIR-005.
       RIR-999.
           EXIT.
      *
       READ-NEXT-INVOICE SECTION.
       RNR-005.
           READ INCR-REGISTER NEXT
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10
               GO TO RNR-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "INV/CR. REGISTER BUSY READ-NEXT, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RNR-005.
       RNR-999.
           EXIT.
      *
       READ-PREV-INVOICE SECTION.
       RPREV-005.
           READ INCR-REGISTER PREVIOUS
               AT END NEXT SENTENCE.
           IF WS-INCR-ST1 = 10
               GO TO RPREV-999.
           IF WS-INCR-ST1 NOT = 0
               MOVE "INV/CR. REGISTER BUSY READ-PREV, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO RPREV-005.
       RPREV-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-016.
           OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0 
              MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO OPEN-016.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlPackRp"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       Copy "ReadFieldNumeric".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldAnalysis".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
