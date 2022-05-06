        IDENTIFICATION DIVISION.
        PROGRAM-ID. StAlteMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStCatalogue".
          Copy "SelectStAlternative".
          Copy "SelectStSpecPr".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStAlternative.
           COPY ChlfdStCatalogue.
           COPY ChlfdStPrice.

       WORKING-STORAGE SECTION.
       77  NEW-STOCKNO        PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-SPECIAL         PIC X VALUE " ".
       77  WS-STOCKNUMBER     PIC X(15) VALUE " ".
       77  WS-INQUIRY-PROGRAM PIC X(8) VALUE "StMastIq".
       01  WS-STDESC.
           03  WS-DESC1       PIC X(20) VALUE " ".
           03  WS-DESC2       PIC X(20) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1   PIC 99.
       01  WS-STALT-STATUS.
           03  WS-STALT-ST1   PIC 99.
       01  WS-STPR-STATUS.
           03  WS-STPR-ST1    PIC 99.
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
            MOVE " " TO STALT-RECORD.
            MOVE "N" TO NEW-STOCKNO
                        WS-END.
            PERFORM ERROR1-020.
            MOVE 2710 TO POS
            DISPLAY
             "DELETE; <F10>=THE WHOLE RECORD, <Alt-ESC>=THE LINE."
             AT POS.
       GET-001.              
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STALT-STOCKNUMBER.
            IF F-EXIT-CH = X"0C"
                MOVE WS-STOCKNUMBER TO STALT-STOCKNUMBER
                PERFORM START-ALTERNATIVE
                PERFORM READ-ALTERNATIVE-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               GO TO GET-000.
            IF F-EXIT-CH = X"05"
                PERFORM READ-ALTERNATIVE-PREVIOUS
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               GO TO GET-000.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF STALT-STOCKNUMBER = 0 OR = "   "
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-ALTERNATIVE.
            IF WS-STOCK-ST1 = 88
                GO TO GET-999.
            IF NEW-STOCKNO = "Y"
                PERFORM GET-003
                GO TO GET-999.
       GET-003.
            MOVE "STOCKNUMBER"     TO F-FIELDNAME
            MOVE 11                TO F-CBFIELDNAME
            MOVE STALT-STOCKNUMBER TO F-NAMEFIELD
            MOVE 25                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.
            
            MOVE "DESCRIPTION"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE WS-STDESC      TO F-NAMEFIELD
            MOVE 40             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            MOVE 1 TO SUB-1 F-INDEX.
       GET-005.
            IF STALT-NUMBER (SUB-1) > " "
               MOVE STALT-NUMBER (SUB-1) TO ST-STOCKNUMBER
               PERFORM READ-STOCK
               
            PERFORM READ-CATALOGUE-REF

            MOVE "CAT-PAGE"        TO F-FIELDNAME
            MOVE 8                 TO F-CBFIELDNAME
            MOVE STCAT-PAGE-NUM    TO F-NAMEFIELD
            MOVE 5                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "NUMBER"             TO F-FIELDNAME
            MOVE 6                    TO F-CBFIELDNAME
            MOVE STALT-NUMBER (SUB-1) TO F-NAMEFIELD
            MOVE 25                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2

            MOVE "DESC"         TO F-FIELDNAME
            MOVE 4              TO F-CBFIELDNAME
            MOVE WS-STDESC      TO F-NAMEFIELD
            MOVE 38             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA
            
            PERFORM READ-SPECIAL-PRICES

            MOVE "SPECIAL"      TO F-FIELDNAME
            MOVE 7              TO F-CBFIELDNAME
            MOVE WS-SPECIAL     TO F-NAMEFIELD
            MOVE 1              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ONHAND"       TO F-FIELDNAME
            MOVE 6              TO F-CBFIELDNAME
            MOVE ST-QTYONHAND   TO F-EDNAMEFIELDINV
            MOVE 6              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-INV

            MOVE "PRICE"        TO F-FIELDNAME
            MOVE 5              TO F-CBFIELDNAME
            MOVE ST-PRICE       TO F-EDNAMEFIELDPRICE
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PRICE.
       GET-020.
            IF SUB-1 < 10
               ADD 1 TO SUB-1 F-INDEX
               GO TO GET-005.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-STOCK-ST1 = 88
                GO TO FILL-999.
            IF WS-END = "Y"
                GO TO FILL-999.
            MOVE 1 TO SUB-1 F-INDEX.
       FILL-005.
            MOVE ALL SPACES  TO F-NAMEFIELD.
            MOVE "NUMBER"    TO F-FIELDNAME.
            MOVE 6           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STALT-NUMBER (SUB-1) ST-STOCKNUMBER.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-ALTERNATIVE-RECORD
               GO TO FILL-999.
            IF F-EXIT-CH = X"87"
               PERFORM DELETE-ITEM-FROM-RECORD
               PERFORM REWRITE-ALTERNATIVE-RECORD
               GO TO FILL-005.
            IF ST-STOCKNUMBER > " "
               PERFORM READ-STOCK
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE "A VALID STOCK NUMBER MUST BE ENTERED, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE ST-STOCKNUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-005.
            IF F-EXIT-CH = X"01"
             IF SUB-1 > 1
               SUBTRACT 1 FROM SUB-1 F-INDEX
               GO TO FILL-005
            ELSE
               GO TO FILL-005.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-ALTERNATIVE-RECORD
               PERFORM READ-ALTERNATIVE-NEXT
               PERFORM CLEAR-SCREEN-FORM
               PERFORM GET-003 THRU GET-020
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-ALTERNATIVE-RECORD
               PERFORM READ-ALTERNATIVE-PREVIOUS
               PERFORM CLEAR-SCREEN-FORM
               PERFORM GET-003 THRU GET-020
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-ALTERNATIVE-RECORD
              GO TO FILL-999.
            IF ST-STOCKNUMBER = " "
               GO TO FILL-005.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-ALTERNATIVE-RECORD
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.

            PERFORM GET-005.
            IF SUB-1 < 10
               ADD 1 TO SUB-1 F-INDEX
               GO TO FILL-005.
            GO TO FILL-005.
       FILL-999.
             EXIT.
      *
       DELETE-ALTERNATIVE-RECORD SECTION.
       DSR-000.
            IF NEW-STOCKNO = "Y"
               GO TO DSR-999.
       DSR-010.
            DELETE STALT-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-STALT-ST1 NOT = 0
               MOVE "ST-ALTERNATIVE BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STALT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STALT-ST1
               GO TO DSR-010. 
       DSR-999.
            EXIT. 
      *
       DELETE-ITEM-FROM-RECORD SECTION.
       DIFR-005.
            MOVE SPACES TO STALT-NUMBER (SUB-1).
            MOVE 1 TO SUB-1 SUB-2.
       DIFR-010.
            IF STALT-NUMBER (SUB-1) NOT = " "
             IF SUB-2 < 10
               ADD 1 TO SUB-1 SUB-2
               GO TO DIFR-010.
            ADD 1 TO SUB-2.
       DIFR-030.
            MOVE STALT-NUMBER (SUB-2) TO STALT-NUMBER (SUB-1).
       DIFR-020.
            IF SUB-2 < 10
               ADD 1 TO SUB-1 SUB-2
               GO TO DIFR-030.
            MOVE SPACES TO STALT-NUMBER (SUB-1).
            PERFORM CLEAR-SCREEN-FORM.
            MOVE 1 TO SUB-1 F-INDEX.
            PERFORM GET-005 THRU GET-020.
            MOVE 1 TO SUB-1 F-INDEX.
       DIFR-999.
            EXIT.
      *
       CLEAR-SCREEN-FORM SECTION.
       CSF-001.
            MOVE 1 TO SUB-1 F-INDEX.
       CSF-005.
            MOVE "CAT-PAGE"           TO F-FIELDNAME
            MOVE 8                    TO F-CBFIELDNAME
            MOVE " "                  TO F-NAMEFIELD
            MOVE 5                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       
            MOVE "NUMBER"             TO F-FIELDNAME
            MOVE 6                    TO F-CBFIELDNAME
            MOVE " "                  TO F-NAMEFIELD
            MOVE 25                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESC"        TO F-FIELDNAME
            MOVE 4             TO F-CBFIELDNAME
            MOVE " "           TO F-NAMEFIELD
            MOVE 38            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SPECIAL"           TO F-FIELDNAME
            MOVE 7                   TO F-CBFIELDNAME
            MOVE " "                 TO F-NAMEFIELD
            MOVE 1                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ONHAND"       TO F-FIELDNAME
            MOVE 6              TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 6              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PRICE"        TO F-FIELDNAME
            MOVE 5              TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       CSF-020.
            IF SUB-1 < 10
               ADD 1 TO SUB-1 F-INDEX
               GO TO CSF-005.
       CSF-999.
            EXIT.
      *
       RELEASE-ALTERNATIVE-RECORD SECTION.
       REL-000.
           UNLOCK STALT-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-ALTERNATIVE-RECORD SECTION.
       RSR-010.
          IF NEW-STOCKNO = "Y"
              GO TO RSR-020.
          REWRITE STALT-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STALT-ST1 NOT = 0
              MOVE "ST-ALTERNATIVE BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STALT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STALT-ST1
                GO TO RSR-010.
          GO TO RSR-999.
       RSR-020.
          WRITE STALT-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STALT-ST1 NOT = 0
              MOVE "ST-ALTERNATIVE BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STALT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STALT-ST1
                GO TO RSR-020.
       RSR-999.
          EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
             IF ST-STOCKNUMBER = " "
                 GO TO R-ST-999.
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
                MOVE 0 TO WS-STOCK-ST1
                GO TO R-ST-010.
       R-ST-999.
             EXIT.
      *
       READ-ALTERNATIVE SECTION.
       R-CAT-000.
             MOVE STALT-STOCKNUMBER TO WS-STOCKNUMBER
                                       ST-STOCKNUMBER.
             PERFORM READ-STOCK.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE "THIS IS NOT A VALID STOCK-NUMBER." TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE 88 TO WS-STOCK-ST1
                GO TO R-CAT-999.
             START STALT-MASTER KEY NOT < STALT-KEY
                INVALID KEY NEXT SENTENCE.
       R-CAT-010.
             READ STALT-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-STALT-ST1 = 23 OR 35 OR 49
                MOVE "Y" TO NEW-STOCKNO
                MOVE WS-STOCKNUMBER TO STALT-STOCKNUMBER
                GO TO R-CAT-999.
             IF WS-STALT-ST1 NOT = 0
                MOVE "ST-ALT RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STALT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STALT-ST1
                GO TO R-CAT-010.
       R-CAT-999.
             EXIT.
      *
       START-ALTERNATIVE SECTION.
       ST-ST-000.
              MOVE WS-STOCKNUMBER TO STALT-STOCKNUMBER.
              START STALT-MASTER KEY NOT < STALT-STOCKNUMBER.
       ST-ST-999.
             EXIT.
      *
       READ-ALTERNATIVE-NEXT SECTION.
       RSN-005. 
           READ STALT-MASTER NEXT WITH LOCK
             AT END 
               MOVE " " TO STALT-STOCKNUMBER
                           WS-STOCKNUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-STALT-ST1 = 23 OR 35 OR 49 OR 51
               MOVE "ST-ALTERNATIVE BUSY READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STALT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STALT-ST1
                GO TO RSN-005.
           IF WS-STALT-ST1 NOT = 0
               MOVE "ST-ALTERNATIVE BUSY READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STALT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STALT-ST1
                PERFORM START-ALTERNATIVE
                GO TO RSN-005.
           MOVE "N" TO NEW-STOCKNO.
           MOVE STALT-STOCKNUMBER TO WS-STOCKNUMBER
                                     ST-STOCKNUMBER.
           PERFORM READ-STOCK.
       RSN-999.
             EXIT.
      *
       READ-ALTERNATIVE-PREVIOUS SECTION.
       RPREV-005. 
           READ STALT-MASTER PREVIOUS WITH LOCK
             AT END 
               MOVE " " TO STALT-STOCKNUMBER
                           WS-STOCKNUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RPREV-999.
           IF WS-STALT-ST1 = 23 OR 35 OR 49 OR 51
               MOVE "ST-ALTERNATIVE BUSY PREV-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STALT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STALT-ST1
                GO TO RPREV-005.
           IF WS-STALT-ST1 NOT = 0
               MOVE 0 TO WS-STALT-ST1
               PERFORM START-ALTERNATIVE
               GO TO RPREV-005.
           MOVE "N" TO NEW-STOCKNO.
           MOVE STALT-STOCKNUMBER TO WS-STOCKNUMBER
                                     ST-STOCKNUMBER.
           PERFORM READ-STOCK.
       RPREV-999.
             EXIT.
      *
       READ-SPECIAL-PRICES SECTION.
       SPR-000.
           MOVE STALT-NUMBER (SUB-1) TO STPR-STOCKNUMBER.
           START STPR-MASTER KEY NOT < STPR-KEY
               INVALID KEY NEXT SENTENCE.
       SPR-005.
           READ STPR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STPR-ST1 = 23 OR 35 OR 49
               MOVE "N" TO WS-SPECIAL
               GO TO SPR-999.
           IF WS-STPR-ST1 NOT = 0
              MOVE "SPECIAL PRICES BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STPR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STPR-ST1
                GO TO SPR-005.
            IF STPR-PRICE > 0
                MOVE "Y"        TO WS-SPECIAL
            ELSE
                MOVE "N"        TO WS-SPECIAL.
       SPR-999.
           EXIT.
      *
       READ-CATALOGUE-REF SECTION.
       RCREF-000.
           MOVE STALT-NUMBER (SUB-1) TO STCAT-STOCKNUMBER.
           START STCAT-MASTER KEY NOT < STCAT-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-STCAT-ST1 NOT = 0
               MOVE "NONE " TO STCAT-PAGE-NUM
               GO TO RCREF-999.
       RCREF-005.
           READ STCAT-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STCAT-ST1 = 23 OR 35 OR 49
               MOVE "NONE " TO STCAT-PAGE-NUM
               GO TO RCREF-999.
           IF WS-STCAT-ST1 NOT = 0
              MOVE "ST-CATALOGUE PAGE BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STCAT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STCAT-ST1
                GO TO RCREF-005.
           IF STCAT-PAGE-NUM = " "
               MOVE "NONE " TO STCAT-PAGE-NUM.
       RCREF-999.
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
       OPEN-002.
            OPEN I-O STALT-MASTER.
            IF WS-STALT-ST1 NOT = 0
               MOVE 0 TO WS-STALT-ST1
               MOVE "ST-ALTERNATIVE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
       OPEN-005.
            OPEN I-O STPR-MASTER.
            IF WS-STPR-ST1 NOT = 0
               MOVE 0 TO WS-STPR-ST1
               MOVE "STOCK PRICE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-006.
            OPEN I-O STCAT-MASTER.
            IF WS-STCAT-ST1 NOT = 0
               MOVE 0 TO WS-STCAT-ST1
               MOVE "ST-CATALOGUE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-006.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StAlteMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STOCK-MASTER
                 STPR-MASTER
                 STCAT-MASTER
                 STALT-MASTER.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldPrice".
       Copy "WriteFieldInv".
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
