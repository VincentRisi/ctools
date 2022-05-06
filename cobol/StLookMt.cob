        IDENTIFICATION DIVISION.
        PROGRAM-ID. StLookMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStockLookup".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStockLookup.

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
       01  WS-LOOK-STATUS.
           03  WS-LOOK-ST1    PIC 99.
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
            MOVE " " TO STLOOK-RECORD.
            MOVE "N" TO NEW-STOCKNO
                        WS-END.
       GET-001.              
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD   TO STLOOK-STOCKNUMBER.
            IF F-EXIT-CH = X"0C"
                MOVE WS-STOCKNUMBER TO STLOOK-STOCKNUMBER
                PERFORM START-LOOKUP
                PERFORM READ-LOOKUP-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               GO TO GET-000.
            IF F-EXIT-CH = X"05"
                PERFORM READ-LOOKUP-PREVIOUS
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               GO TO GET-000.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF STLOOK-STOCKNUMBER = 0 OR = "   "
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-LOOKUP.
            IF NEW-STOCKNO = "Y"
                PERFORM GET-005
                GO TO GET-999.
            GO TO GET-005.
       GET-003.
            MOVE "STOCKNUMBER"      TO F-FIELDNAME
            MOVE 11                 TO F-CBFIELDNAME
            MOVE STLOOK-STOCKNUMBER TO F-NAMEFIELD
            MOVE 15                 TO F-CBFIELDLENGTH
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

            MOVE "LOOKUP-NUM"          TO F-FIELDNAME
            MOVE 10                    TO F-CBFIELDNAME
            MOVE STLOOK-SUPPLIERNUMBER TO F-NAMEFIELD
            MOVE 15                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-Alpha.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-END = "Y"
                GO TO FILL-999.
            MOVE "LOOKUP-NUM" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STLOOK-SUPPLIERNUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-LOOKUP-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-LOOKUP-RECORD
               GO TO FILL-999.
            IF STLOOK-SUPPLIERNUMBER = " "
               MOVE "Field May Not Be BLANK, Enter A Supplier Num."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-LOOKUP-RECORD
               PERFORM READ-LOOKUP-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-LOOKUP-RECORD
               PERFORM READ-LOOKUP-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-LOOKUP-RECORD
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
       DELETE-LOOKUP-RECORD SECTION.
       DSR-000.
            IF NEW-STOCKNO = "Y"
               GO TO DSR-999.
       DSR-010.
            DELETE STLOOK-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-LOOK-ST1 NOT = 0
              MOVE "ST-LOOKUP BUSY ON DELETE, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-LOOK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-LOOK-ST1
               GO TO DSR-010. 
       DSR-999.
            EXIT. 
      *
       RELEASE-LOOKUP-RECORD SECTION.
       REL-000.
           UNLOCK STLOOK-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-LOOKUP-RECORD SECTION.
       RSR-010.
          IF NEW-STOCKNO = "Y"
              GO TO RSR-020.
          REWRITE STLOOK-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-LOOK-ST1 NOT = 0
              MOVE 0 TO WS-LOOK-ST1
              MOVE "ST-LOOKUP BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-LOOK-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-LOOK-ST1
              GO TO RSR-010.
          GO TO RSR-999.
       RSR-020.
          WRITE STLOOK-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-LOOK-ST1 NOT = 0
              MOVE "ST-LOOKUP BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-LOOK-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-LOOK-ST1
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
                MOVE "NO LONGER VALID NUM " TO ST-DESCRIPTION1
                MOVE SPACES                 TO ST-DESCRIPTION2
                GO TO R-ST-999.
             IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-010.
       R-ST-999.
             EXIT.
      *
       READ-LOOKUP SECTION.
       R-CAT-000.
             MOVE STLOOK-STOCKNUMBER TO WS-STOCKNUMBER
                                        ST-STOCKNUMBER.
             PERFORM READ-STOCK.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE "THIS IS NOT A VALID STOCK-NUMBER." TO WS-MESSAGE
                PERFORM ERROR-000.
             START STLOOK-MASTER KEY NOT < STLOOK-KEY
                INVALID KEY NEXT SENTENCE.
       R-CAT-010.
             READ STLOOK-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-LOOK-ST1 = 23 OR 35 OR 49
                MOVE "Y" TO NEW-STOCKNO
                MOVE WS-STOCKNUMBER TO STLOOK-STOCKNUMBER
                GO TO R-CAT-999.
             IF WS-LOOK-ST1 NOT = 0
                MOVE "ST-LOOKUP BUSY ON READ-LOCK, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-LOOK-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-LOOK-ST1
                GO TO R-CAT-010.
       R-CAT-999.
             EXIT.
      *
       START-LOOKUP SECTION.
       ST-ST-000.
              MOVE WS-STOCKNUMBER TO STLOOK-STOCKNUMBER.
              START STLOOK-MASTER KEY NOT < STLOOK-STOCKNUMBER.
       ST-ST-999.
             EXIT.
      *
       READ-LOOKUP-NEXT SECTION.
       RSN-005. 
           READ STLOOK-MASTER NEXT WITH LOCK
             AT END 
               MOVE " " TO STLOOK-STOCKNUMBER
                           WS-STOCKNUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-LOOK-ST1 = 23 OR 35 OR 49
               MOVE "ST-LOOKUP BUSY ON READ-NEXT-23, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-LOOK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-LOOK-ST1
               GO TO RSN-999.
           IF WS-LOOK-ST1 NOT = 0
               MOVE "ST-LOOKUP BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-LOOK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-LOOK-ST1
               PERFORM START-LOOKUP
               GO TO RSN-005.
           MOVE "N" TO NEW-STOCKNO.
           MOVE STLOOK-STOCKNUMBER TO WS-STOCKNUMBER
                                      ST-STOCKNUMBER.
           PERFORM READ-STOCK.
       RSN-999.
             EXIT.
      *
       READ-LOOKUP-PREVIOUS SECTION.
       RPREV-005. 
           READ STLOOK-MASTER PREVIOUS WITH LOCK
             AT END 
               MOVE " " TO STLOOK-STOCKNUMBER
                           WS-STOCKNUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RPREV-999.
           IF WS-LOOK-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-LOOK-ST1
               MOVE "ST-LOOKUP BUSY ON READ-PREV-23, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-LOOK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RPREV-999.
           IF WS-LOOK-ST1 NOT = 0
               MOVE "ST-LOOKUP BUSY ON READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-LOOK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-LOOK-ST1
               PERFORM START-LOOKUP
               GO TO RPREV-005.
           MOVE "N" TO NEW-STOCKNO.
           MOVE STLOOK-STOCKNUMBER TO WS-STOCKNUMBER
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
            OPEN I-O STLOOK-MASTER.
            IF WS-LOOK-ST1 NOT = 0
               MOVE 0 TO WS-LOOK-ST1
               MOVE "ST-LOOKUP BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StLookMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STOCK-MASTER
                 STLOOK-MASTER.
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
