       IDENTIFICATION DIVISION.
       PROGRAM-ID. Company.
       AUTHOR. CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. B20.
       OBJECT-COMPUTER. B20.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COMPANY-MENU ASSIGN TO WS-CoCompany
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-MENU-STATUS
               RECORD KEY IS PTY-KEY.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCompany.

       WORKING-STORAGE SECTION.
       77  WS-ANSWER            PIC XX VALUE " ".
       77  WS-PROGRAM           PIC X(12) VALUE " ".
       77  Ws-Com-NUMBER        PIC 99 VALUE 0.
       77  Ws-Com-NAME          PIC X(40) VALUE " ".
       77  Ws-ComVol-DIR        PIC X(25) VALUE " ".
       01  WS-NUMBER-CHECK.
           03  WS-NU1           PIC x.
           03  WS-NU2           PIC X.
       01  WS-MENU-STATUS.
           03  WS-MENU-ST1       PIC 99.
       Copy "WsDateInfo".
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
          PERFORM CLEAR-SCREEN.
       CONTROL-010.
          PERFORM DISPLAY-FORM.
          PERFORM GET-DATA.
          GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-020.
            MOVE "                      " TO F-NAMEFIELD
            MOVE "SELECTION" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            MOVE 2           TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"0C"
                PERFORM START-COMPANY
                PERFORM READ-NEXT-COMPANY
             IF WS-MENU-ST1 NOT = "1"
               GO TO GET-025
             ELSE
               GO TO GET-999.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"1D"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-020.
            IF F-NAMEFIELD = " "
                GO TO GET-020.
                
            MOVE F-NAMEFIELD   TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE  TO WS-COM-NUMBER
            MOVE WS-COM-NUMBER TO F-EDNAMEFIELDPTY
            MOVE 2             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PTY.
      *      MOVE F-NAMEFIELD TO Ws-Com-NUMBER
            PERFORM READ-COMPANY.
       GET-025.
            MOVE "SELECTION"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME
            MOVE WS-COM-NUMBER TO F-EDNAMEFIELDPTY
            MOVE 2             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PTY.

            MOVE "NAME"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            MOVE WS-COM-NAME TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            MOVE "VOL-DIR"     TO F-FIELDNAME
            MOVE 7             TO F-CBFIELDNAME
            MOVE WS-COMVOL-DIR TO F-NAMEFIELD
            MOVE 25            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-030.
            MOVE "NAME"     TO F-FIELDNAME
            MOVE 4          TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-020.
            IF F-EXIT-CH = X"07"
                UNLOCK COMPANY-MENU
                GO TO GET-999.
            IF F-EXIT-CH = X"0C"
                PERFORM REWRITE-COMPANY
                PERFORM READ-NEXT-COMPANY
             IF WS-MENU-ST1 NOT = "1"
               PERFORM GET-025
               GO TO GET-030
             ELSE
               GO TO GET-999.
            MOVE 40 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO Ws-Com-NAME PTY-CO-NAME.
            IF F-EXIT-CH = X"1B"
                PERFORM REWRITE-COMPANY
                GO TO GET-999.
       GET-050.
            MOVE "VOL-DIR"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-030.
            IF F-EXIT-CH = X"07"
                UNLOCK COMPANY-MENU
                GO TO GET-999.
            IF F-EXIT-CH = X"0C"
                PERFORM REWRITE-COMPANY
                PERFORM READ-NEXT-COMPANY
             IF WS-MENU-ST1 NOT = "1"
               PERFORM GET-025
               GO TO GET-030
             ELSE
               GO TO GET-999.
            MOVE 25 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO Ws-ComVol-DIR PTY-VOL-DIR.
            IF F-EXIT-CH = X"1B"
                PERFORM REWRITE-COMPANY
                GO TO GET-999.
            GO TO GET-030.
       GET-999.
           EXIT.
      *
       READ-COMPANY SECTION.
       RC-005.
           MOVE WS-COM-NUMBER TO PTY-NUMBER.
           START COMPANY-MENU KEY NOT < PTY-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-MENU-ST1 NOT = 0
               MOVE SPACES TO WS-COMVOL-DIR
                              WS-COM-NAME
               GO TO RC-999.
       RC-010.
           READ COMPANY-MENU WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-MENU-ST1 = 23 OR 35 OR 49
               MOVE 0   TO WS-COM-NUMBER
               MOVE SPACES TO WS-COMVOL-DIR
                              WS-COM-NAME
               GO TO RC-999.
           IF WS-MENU-ST1 NOT = 0
               MOVE "COMPANY NUMBER FILE BUSY ON READ, GOING TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO RC-010.
           MOVE PTY-VOL-DIR TO WS-COMVOL-DIR
           MOVE PTY-CO-NAME TO WS-COM-NAME.
       RC-999.
           EXIT.
      *
       START-COMPANY SECTION.
       ST-CO-000.
           MOVE Ws-Com-NUMBER TO PTY-NUMBER.
           START COMPANY-MENU KEY NOT < PTY-KEY.
      *         INVALID KEY NEXT SENTENCE.
       ST-CO-999.
             EXIT.
      *
       READ-NEXT-COMPANY SECTION.
       RNC-010.
           READ COMPANY-MENU NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-MENU-ST1 = 10
              MOVE 0   TO WS-COM-NUMBER
              MOVE " " TO Ws-ComVol-DIR
                          Ws-Com-NAME
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RNC-999.
           IF WS-MENU-ST1 NOT = 0
             MOVE "COMPANY FILE BUSY ON READ-NEXT, 'CANCEL' TO RE-TRY."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE WS-MENU-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO RNC-010.
           MOVE PTY-NUMBER  TO WS-COM-NUMBER
           MOVE PTY-VOL-DIR TO WS-COMVOL-DIR
           MOVE PTY-CO-NAME TO WS-COM-NAME.
       RNC-999.
           EXIT.
      *
       REWRITE-COMPANY SECTION.
       RWC-005.
           IF WS-MENU-ST1 = 35
               GO TO RWC-020.
       RWC-010.
           REWRITE COMPANY-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-MENU-ST1 NOT = 0
               MOVE "COMPANY FILE BUSY ON REWRITE, 'CANCEL' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RWC-020.
           GO TO RWC-999.
       RWC-020.
           WRITE COMPANY-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-MENU-ST1 NOT = 0
               MOVE "COMPANY FILE BUSY ON WRITE, 'CANCEL' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RWC-010.
       RWC-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
           OPEN I-O COMPANY-MENU.
           IF WS-MENU-ST1 NOT = 0
               MOVE 0 TO WS-MENU-ST1
               MOVE "COMPANY FILE BUSY ON OPEN, 'CANCEL' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-010.
            MOVE Ws-Forms-Name   TO F-FILENAME
            MOVE Ws-cbForms-name TO F-CBFILENAME.
            MOVE "Company"       TO F-FORMNAME
            MOVE 7               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE COMPANY-MENU.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldPty".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "MenuClearScreen".
       Copy "ErrorMessage".
       Copy "DecimaliseRate".
      *
      * END-OF-JOB
