        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrDelvMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectDrDeliver".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDrDelAddress.
           COPY ChlfdDebtor.
           
       WORKING-STORAGE SECTION.
       77  NEW-DEBTORNO       PIC X VALUE " ".      
       77  NEW-DELNO          PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-ACCOUNTNUMBER   PIC 9(7) VALUE 0.
       77  WS-INQUIRY-PROGRAM PIC X(8) VALUE "DrNameIq".
       77  WS-ACC-SAVE        PIC 9(7) VALUE 0.
       77  WS-DEL-SAVE        PIC 9(2) VALUE 0.
       77  WS-ALL-ENTERED     PIC X VALUE " ".
       01  WS-DRDEL-STATUS.
           03  WS-DRDEL-ST1   PIC 99.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1  PIC 99.
       COPY "WsDateInfo".
      **************************************************************
      *                  FORMS WORK FIELDS                         *
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
            MOVE " " TO DEBTOR-DEL-RECORD.
            MOVE "N" TO NEW-DEBTORNO
                        NEW-DELNO
                        WS-ALL-ENTERED
                        WS-END.
        GET-0000.
            MOVE SPACES      TO F-NAMEFIELD.
            MOVE "DELNUMBER" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-0000.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-ACC-SAVE TO DRDEL-ACCOUNT-NUMBER
                 MOVE WS-DEL-SAVE TO DRDEL-NUMBER
                 PERFORM START-DELIVERY
                 PERFORM READ-DELIVERY-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 GO TO GET-000.
            MOVE 2            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO DRDEL-NUMBER.
            IF DRDEL-NUMBER = 0
                 CLOSE DEBTOR-DELIVERY
                 PERFORM CLEAR-SCREEN
                 CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                 CANCEL WS-INQUIRY-PROGRAM
                 PERFORM CLEAR-SCREEN
                 PERFORM OPEN-000
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
        GET-001.
            MOVE SPACES          TO F-NAMEFIELD.
            MOVE "ACCOUNTNUMBER" TO F-FIELDNAME.
            MOVE 13              TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7               TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ACCOUNT.
            MOVE F-NAMEFIELDACC  TO DRDEL-ACCOUNT-NUMBER.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-ACC-SAVE TO DRDEL-ACCOUNT-NUMBER
                 MOVE WS-DEL-SAVE TO DRDEL-NUMBER
                 PERFORM START-DELIVERY
                 PERFORM READ-DELIVERY-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-0000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
            PERFORM READ-DEBTOR.
            IF NEW-DEBTORNO = "Y"
               MOVE
           "YOU CAN ONLY HAVE A DELIVERY ADDRESS FOR EXISTING ACCOUNTS"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-001.
            PERFORM READ-DELIVERY.
      *      IF NEW-DELNO = "Y"
      *          GO TO GET-005.
        GET-003.
            MOVE "DELNUMBER"  TO F-FIELDNAME
            MOVE 9            TO F-CBFIELDNAME
            MOVE DRDEL-NUMBER TO F-NAMEFIELD
            MOVE 2            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            MOVE "ACCOUNTNUMBER"      TO F-FIELDNAME
            MOVE 13                   TO F-CBFIELDNAME
            MOVE DRDEL-ACCOUNT-NUMBER TO F-NAMEFIELD
            MOVE 7                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            MOVE "CONAME" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE DR-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE DRDEL-NAME TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DRDEL-ADDRESS1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD2" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DRDEL-ADDRESS2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DRDEL-ADDRESS3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       DELETE-DELIVERY-RECORD SECTION.
       DDRDEL-000.
            IF NEW-DEBTORNO = "Y"
                GO TO DDRDEL-999.
       DDRDEL-010.
            DELETE DEBTOR-DELIVERY
               INVALID KEY NEXT SENTENCE.
            IF WS-DRDEL-ST1 NOT = 0
               MOVE "DRDEL FILE BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRDEL-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRDEL-ST1
               GO TO DDRDEL-010.
       DDRDEL-999.
           EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                 GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20     TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DRDEL-NAME.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DELIVERY-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF DRDEL-NAME = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, ENTER A CHARACTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-001.
            IF NEW-DEBTORNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-040.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DELIVERY-RECORD
               PERFORM READ-DELIVERY-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DELIVERY-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DELIVERY-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DRDEL-ADDRESS1.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DELIVERY-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF NEW-DEBTORNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-030.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DELIVERY-RECORD
               PERFORM READ-DELIVERY-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DELIVERY-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DELIVERY-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-030.
       FILL-035.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DELADD2" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DRDEL-ADDRESS2.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DELIVERY-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF NEW-DEBTORNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-035.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DELIVERY-RECORD
               PERFORM READ-DELIVERY-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DELIVERY-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DELIVERY-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-035.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-035.
       FILL-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DRDEL-ADDRESS3.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DELIVERY-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
               
            MOVE "Y" TO WS-ALL-ENTERED.
            
            IF NEW-DEBTORNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-040.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DELIVERY-RECORD
               PERFORM READ-DELIVERY-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-035.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DELIVERY-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DELIVERY-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-040.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-040.

            GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       RELEASE-DELIVERY-RECORD SECTION.
       REL-000.
           UNLOCK DEBTOR-DELIVERY.
       REL-999.
           EXIT.
      *
       REWRITE-DELIVERY-RECORD SECTION.
       RDRDEL-010.
            IF NEW-DELNO = "Y"
               GO TO RDRDEL-020.
            REWRITE DEBTOR-DEL-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-DRDEL-ST1 NOT = 0
                MOVE "DELIVERY RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DRDEL-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DRDEL-ST1
                GO TO RDRDEL-020.
            GO TO RDRDEL-999.
       RDRDEL-020.
            WRITE DEBTOR-DEL-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-DRDEL-ST1 NOT = 0
                MOVE "DELIVERY RECORD BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DRDEL-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DRDEL-ST1
                GO TO RDRDEL-010.
       RDRDEL-999.
            EXIT.
      *
       READ-DELIVERY SECTION.
       RDEL-000.
           MOVE DRDEL-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER.
           START DEBTOR-DELIVERY KEY NOT < DRDEL-KEY
               INVALID KEY NEXT SENTENCE.
       RDEL-010.
           READ DEBTOR-DELIVERY WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-DRDEL-ST1 = 23 OR 35 OR 49
                MOVE DRDEL-ACCOUNT-NUMBER TO WS-ACC-SAVE
                MOVE DRDEL-NUMBER         TO WS-DEL-SAVE
                MOVE "Y" TO NEW-DELNO
                GO TO RDEL-999.
           IF WS-DRDEL-ST1 NOT = 0
                MOVE "DR-DEL RECORD BUSY, PRESS 'ESC' TO RETRY"
                  TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DRDEL-ST1
                GO TO RDEL-010.
           MOVE "N" TO NEW-DELNO.
           MOVE DRDEL-ACCOUNT-NUMBER TO WS-ACC-SAVE.
       RDEL-999.
             EXIT.
      *
       READ-DEBTOR SECTION.
       RD-000.
           MOVE DRDEL-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT < DR-ACCOUNT-NUMBER.
       RD-010.
           READ DEBTOR-MASTER
                 INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE "Y" TO NEW-DEBTORNO
                GO TO RD-999.
           IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR BUSY ON READ, PRESS 'ESC' TO RETRY"
                  TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO RD-010.
           MOVE "N" TO NEW-DEBTORNO.
       RD-999.
             EXIT.
      *
       START-DELIVERY SECTION.
       ST-DRDEL-000.
      *        MOVE WS-ACCOUNTNUMBER TO DRDEL-ACCOUNT-NUMBER
      *        MOVE 1                TO DRDEL-NUMBER.
              START DEBTOR-DELIVERY KEY NOT < DRDEL-KEY
                   INVALID KEY NEXT SENTENCE.
       ST-DRDEL-999.
             EXIT.
      *
       READ-DELIVERY-NEXT SECTION.
       RDNX-001.
           MOVE 0 TO Ws-DrDel-ST1.
       RDNX-005. 
           READ DEBTOR-DELIVERY NEXT WITH LOCK
            AT END
              MOVE 0 TO DRDEL-ACCOUNT-NUMBER
                        DRDEL-NUMBER
                        WS-ACCOUNTNUMBER
              MOVE "Y" TO WS-END
              PERFORM CLEAR-FORM
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDNX-999.
           IF WS-DRDEL-ST1 = 23 OR 35 OR 49 OR 51
               MOVE "DEBTOR FILE BUSY, PRESS 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRDEL-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DRDEL-ST1
               GO TO RDNX-005.
           IF WS-DRDEL-ST1 NOT = 0
               MOVE "DR-DEL BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRDEL-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-ACCOUNTNUMBER
               MOVE 0 TO WS-DRDEL-ST1
               PERFORM START-DELIVERY
               GO TO RDNX-005.
           MOVE DRDEL-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER
                                        WS-ACC-SAVE.
           MOVE DRDEL-NUMBER         TO WS-DEL-SAVE.
           PERFORM READ-DEBTOR.
           MOVE "N" TO NEW-DELNO.
       RDNX-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE 0 TO DRDEL-ACCOUNT-NUMBER
                       DRDEL-NUMBER.
       CLSC-010.
             MOVE " " TO DRDEL-NAME
                         DRDEL-ADDRESS1
                         DRDEL-ADDRESS2
                         DRDEL-ADDRESS3.
       CLSC-500.
             IF F-EXIT-CH = X"07"
                PERFORM RELEASE-DELIVERY-RECORD.
       CLSC-999.
             EXIT.      
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-DELIVERY.
            IF WS-DRDEL-ST1 NOT = 0
               MOVE 0 TO WS-DRDEL-ST1
               MOVE "DELIVERY FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-002.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *      ACCEPT WS-DATE FROM DATE.
            MOVE Ws-Forms-Name   TO F-FILENAME
            MOVE Ws-cbForms-name TO F-CBFILENAME.
            MOVE "DrDelvMt" TO F-FORMNAME
            MOVE 8          TO F-CBFORMNAME.
            Copy "OpenForms".
        OPEN-999.
            EXIT.
      *
        END-OFF SECTION.
        END-000.
            CLOSE DEBTOR-DELIVERY
                  DEBTOR-MASTER.
            EXIT PROGRAM.
      *      STOP RUN.
        END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldAccount".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldNumeric6".
       Copy "GetSystemY2KDate".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
      *
      * END-OF-JOB
