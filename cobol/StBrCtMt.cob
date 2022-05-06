        IDENTIFICATION DIVISION.
        PROGRAM-ID. StBrCtMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStBranchCat".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStBranchCat.

       WORKING-STORAGE SECTION.
       77  WS-CATEGORY        PIC X(3) VALUE " ".
       77  NEW-STOCKNO        PIC X VALUE " ".      
       77  WS-STOCK-CHANGE    PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-ALL-ENTERED     PIC X VALUE " ".
       77  WS-INQUIRY-PROGRAM PIC X(8) VALUE "StDescIq".
       01  WS-STBRCAT-STATUS.
           03  WS-STBRCAT-ST1   PIC 99.
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
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO STBRANCHCAT-RECORD.
            MOVE "N" TO NEW-STOCKNO
                        WS-END.
       GET-001.              
            MOVE "ST-CATEGORY" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 3  TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STBRCAT-CATEGORY.
            IF F-EXIT-CH = X"0C"
                MOVE WS-CATEGORY TO STBRCAT-CATEGORY
                PERFORM START-STOCK
                PERFORM READ-CATEGORY-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               GO TO GET-999.
            IF F-EXIT-CH = X"05"
                PERFORM READ-CATEGORY-PREVIOUS
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               GO TO GET-999.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF STBRCAT-CATEGORY = 0 OR = "   "
                CLOSE STBRANCHCAT-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-CATEGORY.
            IF NEW-STOCKNO = "Y"
                PERFORM REWRITE-STBRANCHCAT-RECORD
                GO TO GET-999.
      *      GO TO GET-999.
       GET-003.
            MOVE "ST-CATEGORY"    TO F-FIELDNAME
            MOVE 11               TO F-CBFIELDNAME
            MOVE STBRCAT-CATEGORY TO F-NAMEFIELD
            MOVE 3                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-005.
            IF WS-END = "Y"
                GO TO GET-999.
            MOVE "ST-CATEGORY" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 3  TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-STBRANCHCAT-RECORD
               GO TO GET-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-STBRANCHCAT-RECORD
               PERFORM READ-CATEGORY-NEXT
               PERFORM GET-003
               GO TO GET-005.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-STBRANCHCAT-RECORD
               PERFORM READ-CATEGORY-PREVIOUS
               PERFORM GET-003
               GO TO GET-005.
            IF F-EXIT-CH = X"1B" OR = X"0A"
               PERFORM REWRITE-STBRANCHCAT-RECORD
              GO TO GET-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-STBRANCHCAT-RECORD
               GO TO GET-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-005.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-005.
       GET-999.
            EXIT.
      *
       DELETE-STBRANCHCAT-RECORD SECTION.
       DSR-000.
            IF NEW-STOCKNO = "Y"
               GO TO DSR-999.
       DSR-010.
            DELETE STBRANCHCAT-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-STBRCAT-ST1 NOT = 0
               MOVE "STOCK BR-CAT BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STBRCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STBRCAT-ST1
               GO TO DSR-010. 
       DSR-999.
            EXIT. 
      *
       RELEASE-STBRANCHCAT-RECORD SECTION.
       REL-000.
           UNLOCK STBRANCHCAT-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-STBRANCHCAT-RECORD SECTION.
       RSR-010.
          IF NEW-STOCKNO = "Y"
              GO TO RSR-020.
          REWRITE STBRANCHCAT-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STBRCAT-ST1 NOT = 0
              MOVE "STBRANCHCAT BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STBRCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STBRCAT-ST1
              GO TO RSR-010.
          GO TO RSR-999.
       RSR-020.
          WRITE STBRANCHCAT-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STBRCAT-ST1 NOT = 0
              MOVE "STBRANCHCAT BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STBRCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STBRCAT-ST1
              GO TO RSR-020.
       RSR-999.
          EXIT.
      *
       READ-CATEGORY SECTION.
       R-STBRCAT-000.
             MOVE STBRCAT-CATEGORY TO WS-CATEGORY.
             START STBRANCHCAT-MASTER KEY NOT < STBRCAT-KEY.
       R-STBRCAT-010.
             READ STBRANCHCAT-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-STBRCAT-ST1 = 23 OR 35 OR 49
                MOVE "Y" TO NEW-STOCKNO
                MOVE WS-CATEGORY TO STBRCAT-CATEGORY
                GO TO R-STBRCAT-999.
             IF WS-STBRCAT-ST1 NOT = 0
               MOVE "STBRANCHCAT BUSY ON READ-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STBRCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STBRCAT-ST1
               GO TO R-STBRCAT-010.
           MOVE STBRCAT-CATEGORY    TO WS-CATEGORY.
       R-STBRCAT-999.
             EXIT.
      *
       START-STOCK SECTION.
       STBRCAT-STBRCAT-000.
              MOVE WS-CATEGORY TO STBRCAT-CATEGORY.
              START STBRANCHCAT-MASTER KEY NOT LESS STBRCAT-CATEGORY.
       STBRCAT-STBRCAT-999.
             EXIT.
      *
       READ-CATEGORY-NEXT SECTION.
       RSN-005. 
           READ STBRANCHCAT-MASTER NEXT WITH LOCK
             AT END 
               MOVE " " TO STBRCAT-CATEGORY
                           WS-CATEGORY
               MOVE "Y" TO WS-END
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-STBRCAT-ST1 = 23 OR 35 OR 49
               MOVE "STBRANCHCAT BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STBRCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STBRCAT-ST1
               MOVE "Y" TO WS-END
               GO TO RSN-999.
           IF WS-STBRCAT-ST1 NOT = 0
               MOVE "STBRANCHCAT BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STBRCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STBRCAT-ST1
               PERFORM START-STOCK
               GO TO RSN-005.
           MOVE STBRCAT-CATEGORY TO WS-CATEGORY.
           MOVE "N" TO NEW-STOCKNO.
       RSN-999.
             EXIT.
      *
       READ-CATEGORY-PREVIOUS SECTION.
       RPREV-005. 
           READ STBRANCHCAT-MASTER PREVIOUS WITH LOCK
             AT END 
               MOVE " " TO STBRCAT-CATEGORY
                           WS-CATEGORY
               MOVE "Y" TO WS-END
               MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RPREV-999.
           IF WS-STBRCAT-ST1 = 23 OR 35 OR 49
              MOVE "STBRANCHCAT BUSY ON READ-PREVIOUS, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STBRCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STBRCAT-ST1
               GO TO RPREV-999.
           IF WS-STBRCAT-ST1 NOT = 0
              MOVE "STBRANCHCAT BUSY ON READ-PREVIOUS, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STBRCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STBRCAT-ST1
               PERFORM START-STOCK
               GO TO RPREV-005.
           MOVE STBRCAT-CATEGORY TO WS-CATEGORY.
           MOVE "N" TO NEW-STOCKNO.
       RPREV-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STBRANCHCAT-MASTER.
            IF WS-STBRCAT-ST1 NOT = 0
               MOVE 0 TO WS-STBRCAT-ST1
               MOVE "STBRANCHCAT FILE BUSY ON OPEN I-O, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-STBRANCHCAT TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
             GO TO OPEN-020.
       OPEN-005.
            OPEN OUTPUT STBRANCHCAT-MASTER.
            IF WS-STBRCAT-ST1 NOT = 0
               MOVE 0 TO WS-STBRCAT-ST1
               MOVE "STBRANCHCAT BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-STBRANCHCAT TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StBrCtMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STBRANCHCAT-MASTER.
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
