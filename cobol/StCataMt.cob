        IDENTIFICATION DIVISION.
        PROGRAM-ID. StCataMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStCatalogue".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStCatalogue.

       WORKING-STORAGE SECTION.
       77  NEW-STOCKNO        PIC X VALUE " ".      
       77  WS-STOCK-CHANGE    PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-ALL-ENTERED     PIC X VALUE " ".
       77  WS-STOCKNUMBER     PIC X(15) VALUE " ".
       77  WS-VATPRICE        PIC 9(6)V99 VALUE 0.
       77  WS-INQUIRY-PROGRAM PIC X(8) VALUE "StDescIq".
       77  WS-OLDPRICE        PIC 9(6)V99.
       01  WS-STDESC.
           03  WS-DESC1       PIC X(20) VALUE " ".
           03  WS-DESC2       PIC X(20) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1   PIC 99.
       01  WS-STCAT-STATUS.
           03  WS-STCAT-ST1   PIC 99.
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
           PERFORM FILL-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO STCAT-RECORD.
            MOVE "N" TO NEW-STOCKNO
                        WS-END.
       GET-001.              
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STCAT-STOCKNUMBER.
            IF F-EXIT-CH = X"0C"
                MOVE WS-STOCKNUMBER TO STCAT-STOCKNUMBER
                PERFORM START-CATALOGUE
                PERFORM READ-CATALOGUE-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               GO TO GET-000.
            IF F-EXIT-CH = X"05"
                PERFORM READ-CATALOGUE-PREVIOUS
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               GO TO GET-000.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF STCAT-STOCKNUMBER = 0 OR = "   "
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-CATALOGUE.
            IF NEW-STOCKNO = "Y"
                GO TO GET-999.
            GO TO GET-005.
       GET-003.
            MOVE "STOCKNUMBER"     TO F-FIELDNAME
            MOVE 11                TO F-CBFIELDNAME
            MOVE STCAT-STOCKNUMBER TO F-NAMEFIELD
            MOVE 25                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-005.
            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.
            
            MOVE "DESC"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            MOVE WS-STDESC   TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CAT-PAGE"     TO F-FIELDNAME
            MOVE 8              TO F-CBFIELDNAME
            MOVE STCAT-PAGE-NUM TO F-NAMEFIELD
            MOVE 5              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-Alpha.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                GO TO FILL-999.
            MOVE "CAT-PAGE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STCAT-PAGE-NUM.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-CATALOGUE-RECORD
               GO TO FILL-999.
      *      IF STCAT-PAGE-NUM = " "
      *         MOVE "This Field May Not Be BLANK, Enter A Character"
      *         TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE
      *         GO TO FILL-001.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-CATALOGUE-RECORD
               PERFORM READ-CATALOGUE-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-CATALOGUE-RECORD
               PERFORM READ-CATALOGUE-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-CATALOGUE-RECORD
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-CATALOGUE-RECORD
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.
 
            GO TO FILL-001.
       FILL-999.
             EXIT.
      *
       DELETE-CATALOGUE-RECORD SECTION.
       DSR-000.
            IF NEW-STOCKNO = "Y"
               GO TO DSR-999.
       DSR-010.
            DELETE STCAT-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-STCAT-ST1 NOT = 0
              MOVE "ST-CATALOGUE BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCAT-ST1
               GO TO DSR-010. 
       DSR-999.
            EXIT. 
      *
       RELEASE-CATALOGUE-RECORD SECTION.
       REL-000.
           UNLOCK STCAT-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-CATALOGUE-RECORD SECTION.
       RSR-010.
          IF NEW-STOCKNO = "Y"
              GO TO RSR-020.
          REWRITE STCAT-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STCAT-ST1 NOT = 0
              MOVE "ST-CATALOGUE BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCAT-ST1
               GO TO RSR-010.
          GO TO RSR-999.
       RSR-020.
          WRITE STCAT-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STCAT-ST1 NOT = 0
              MOVE "ST-CATALOGUE BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCAT-ST1
               GO TO RSR-020.
       RSR-999.
          EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
             START STOCK-MASTER KEY NOT < ST-KEY.
       R-ST-010.
             READ STOCK-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                GO TO R-ST-999.
             IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO R-ST-010.
       R-ST-999.
             EXIT.
      *
       READ-CATALOGUE SECTION.
       R-CAT-000.
             MOVE STCAT-STOCKNUMBER TO WS-STOCKNUMBER
                                       ST-STOCKNUMBER.
             PERFORM READ-STOCK.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE "THIS IS NOT A VALID STOCK-NUMBER." TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO R-CAT-999.
             START STCAT-MASTER KEY NOT < STCAT-KEY
                INVALID KEY NEXT SENTENCE.
       R-CAT-010.
             READ STCAT-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-STCAT-ST1 = 23 OR 35 OR 49
                MOVE "Y" TO NEW-STOCKNO
                MOVE WS-STOCKNUMBER TO STCAT-STOCKNUMBER
                GO TO R-CAT-999.
             IF WS-STCAT-ST1 NOT = 0
               MOVE "ST-CAT RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCAT-ST1
               GO TO R-CAT-010.
       R-CAT-999.
             EXIT.
      *
       START-CATALOGUE SECTION.
       ST-ST-000.
              MOVE WS-STOCKNUMBER TO STCAT-STOCKNUMBER.
              START STCAT-MASTER KEY NOT < STCAT-KEY.
       ST-ST-999.
             EXIT.
      *
       READ-CATALOGUE-NEXT SECTION.
       RSN-005. 
           READ STCAT-MASTER NEXT WITH LOCK
             AT END 
               MOVE " " TO STCAT-STOCKNUMBER
                           WS-STOCKNUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF NEXT-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-STCAT-ST1 = 23 OR 35 OR 49
               MOVE "ST-CATALOGUE BUSY ON READ-NEXT, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCAT-ST1
               GO TO RSN-999.
           IF WS-STCAT-ST1 NOT = 0
               MOVE "ST-CATALOGUE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCAT-ST1
               PERFORM START-CATALOGUE
               GO TO RSN-005.
           MOVE "N" TO NEW-STOCKNO.
           MOVE STCAT-STOCKNUMBER TO WS-STOCKNUMBER
                                     ST-STOCKNUMBER.
           PERFORM READ-STOCK.
       RSN-999.
             EXIT.
      *
       READ-CATALOGUE-PREVIOUS SECTION.
       RPREV-005. 
           READ STCAT-MASTER PREVIOUS WITH LOCK
             AT END 
               MOVE " " TO STCAT-STOCKNUMBER
                           WS-STOCKNUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RPREV-999.
           IF WS-STCAT-ST1 = 23 OR 35 OR 49
               MOVE "ST-CATALOGUE BUSY ON READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCAT-ST1
               GO TO RPREV-999.
           IF WS-STCAT-ST1 NOT = 0
               MOVE "ST-CATALOGUE BUSY ON READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCAT-ST1
               PERFORM START-CATALOGUE
               GO TO RPREV-005.
           MOVE "N" TO NEW-STOCKNO.
           MOVE STCAT-STOCKNUMBER TO WS-STOCKNUMBER
                                     ST-STOCKNUMBER.
           PERFORM READ-STOCK.
       RPREV-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-005.
            OPEN I-O STCAT-MASTER.
            IF WS-STCAT-ST1 NOT = 0
               MOVE 0 TO WS-STCAT-ST1
               MOVE "ST-CATALOGUE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StCataMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STOCK-MASTER
                 STCAT-MASTER.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "GetSystemY2KDate".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
      *
      * END-OF-JOB
