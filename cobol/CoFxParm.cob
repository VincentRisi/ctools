        IDENTIFICATION DIVISION.
        PROGRAM-ID. CoFxParm.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT FAX-PARAMETER ASSIGN TO Ws-CoFaxParam
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-FAX-STATUS
               RECORD KEY IS Fax-PaKey.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       COPY ChlfdFaxParam.
      *
       WORKING-STORAGE SECTION.
       77  WS-FaxKey            PIC 9 VALUE 0.
       77  WS-Fax-Save          PIC 9 VALUE 0.
       77  WS-FaxNumber         PIC 9 VALUE 0.
       77  New-FaxNo            PIC X VALUE " ".
       77  Ws-End               PIC X VALUE " ".
       01  WS-Fax-STATUS.
           03  WS-Fax-ST1     PIC 99.
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
           PERFORM CLEAR-SCREEN
           PERFORM OPEN-FILES.
       CONT-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA
           PERFORM FILL-BODY.
           GO TO CONT-010.
       CONT-040.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-001.
            Move "N" to Ws-End.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "FAX-PAKEY" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE "1"         TO F-EDNAMEFIELDFAX
            MOVE 1           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FAX.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            MOVE 1            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-FAXKEY.
            IF WS-FAXKEY = 0
               MOVE "THIS FIELD SHOULD BE ONE ONLY, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-001.
            PERFORM ERROR-020.
            Move Ws-FaxKey to Fax-ParmKey
            Perform Read-Fax.
        GET-003.
            MOVE "FAX-PAKEY" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE Fax-ParmKey TO F-EDNAMEFIELDFAX
            MOVE 1           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FAX.
        
            MOVE "FAX-PANUMBER" TO F-FIELDNAME
            MOVE 12             TO F-CBFIELDNAME
            MOVE Fax-PaNumber   TO F-EDNAMEFIELDFAX
            MOVE 1              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FAX.

            MOVE "FAX-PANAME"   TO F-FIELDNAME
            MOVE 10             TO F-CBFIELDNAME
            MOVE Fax-PaName     TO F-NAMEFIELD
            MOVE 25             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-BODY SECTION.
       FILL-005.
            IF WS-END = "Y"
                GO TO FILL-999.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "FAX-PANUMBER" TO F-FIELDNAME
            MOVE 12             TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 1              TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDFax
                                 Fax-PaNumber.
            PERFORM WRITE-FIELD-Fax.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-Fax-ParamRecord
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-Fax-ParamRecord
               PERFORM READ-Fax-NEXT
               PERFORM GET-003
               GO TO FILL-005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-Fax-ParamRecord
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-Fax-ParamRecord
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO Fill-005.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO Fill-005.
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD
            MOVE "FAX-PANAME" TO F-FIELDNAME
            MOVE 10           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 25           TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO Fax-PaName.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-Fax-ParamRecord
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-Fax-ParamRecord
               PERFORM READ-Fax-NEXT
               PERFORM GET-003
               GO TO FILL-005.
            IF F-EXIT-CH = X"01"
               GO TO Fill-005.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-Fax-ParamRecord
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-Fax-ParamRecord
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-030.
            GO TO FILL-005.
       FILL-999.
           Exit.
      *
       DELETE-FAX-PARAMRECORD SECTION.
       DDR-000.
            MOVE "DELETE NOT ALLOWED IN THIS PROGRAM" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO DDR-999.
            IF NEW-FAXNO = "Y"
                GO TO DDR-999.
       DDR-010.
            DELETE FAX-PARAMETER
               INVALID KEY NEXT SENTENCE.
            IF WS-FAX-ST1 NOT = 0
               MOVE " " TO WS-FAX-ST1
               GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       RELEASE-FAX-PARAMRECORD SECTION.
       REL-000.
           UNLOCK FAX-PARAMETER.
       REL-999.
           EXIT.
      *
       REWRITE-FAX-PARAMRECORD SECTION.
       RDR-005.
            IF NEW-FaxNO = "Y"
               GO TO RDR-020.
       RDR-010.
            REWRITE FAX-PARAMRECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-FAX-ST1 NOT = 0
                MOVE 0 TO WS-FAX-ST1
                MOVE "FAX RECORD BUSY ON REWRITE, BE PATIENT"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-020.
            GO TO RDR-999.
       RDR-020.
            WRITE FAX-PARAMRECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-FAX-ST1 NOT = 0
                MOVE 0 TO WS-FAX-ST1
                MOVE "FAX RECORD BUSY ON WRITE, BE PATIENT"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE WS-FAX-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-010.
       RDR-999.
            EXIT.
      *
       READ-FAX SECTION.
       RD-000.
           MOVE 0       TO WS-FAX-ST1.
           MOVE FAX-PAKEY TO WS-FAXNUMBER.
           START FAX-PARAMETER KEY NOT < FAX-PAKEY.
        RD-010.
           READ FAX-PARAMETER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-FAX-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-FAX-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-FAXNO
                MOVE WS-FAXNUMBER TO FAX-PAKEY
                GO TO RD-999.
           IF WS-FAX-ST1 NOT = 0
                MOVE 0 TO WS-FAX-ST1
                MOVE "FAX BUSY ON READ, PRESS 'ESC' TO RETRY"
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE WS-FAX-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-010.
           MOVE "N" TO NEW-FAXNO.
           MOVE FAX-PAKEY TO WS-FAX-SAVE.
       RD-999.
             EXIT.
      *
       START-Fax SECTION.
       ST-DR-000.
              MOVE Ws-FaxNumber TO Fax-PaKey.
              START Fax-Parameter KEY NOT < Fax-PaKey.
       ST-DR-999.
             EXIT.
      *
       READ-Fax-NEXT SECTION.
       RDNX-001.
           MOVE 0 TO WS-Fax-ST1.
       RDNX-005. 
           READ Fax-Parameter NEXT WITH LOCK
            AT END
              MOVE 0 TO Fax-PaKey
                        Ws-FaxNumber
              MOVE "Y" TO WS-END
              PERFORM CLEAR-FORM
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDNX-999.
           IF WS-Fax-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-Fax-ST1
               MOVE "FAX BUSY ON READ-NEXT, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDNX-005.
           IF WS-Fax-ST1 NOT = 0
               MOVE 0 TO WS-Fax-ST1
               PERFORM START-Fax
               GO TO RDNX-005.
           MOVE Fax-PaKey TO Ws-FaxNumber
                             Ws-Fax-Save.
           MOVE "N" TO NEW-FaxNO.
       RDNX-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO Fax-PaName
             MOVE 0 TO   Fax-PaKey
                         Fax-PaNumber.
             MOVE Ws-FaxNumber TO Fax-PaKey.
       CLSC-999.
             EXIT.      
      *
       OPEN-FILES SECTION.
       OPEN-055.
           OPEN I-O FAX-PARAMETER.
           IF WS-FAX-ST1 = 35
           OPEN OUTPUT FAX-PARAMETER.
           
           IF WS-FAX-ST1 NOT = 0
               MOVE "FAXPARAMETER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-FAX-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-055.
       OPEN-010.
            MOVE Ws-Forms-Name   TO F-FILENAME
            MOVE Ws-cbForms-name TO F-CBFILENAME.
            MOVE "CoFxParm"      TO F-FORMNAME
            MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       Open-999.
           Exit.
      *
       END-OFF SECTION.
       END-800.
           CLOSE FAX-PARAMETER.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldFax".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
      *
      * END-OF-JOB
