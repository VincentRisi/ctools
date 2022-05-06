        IDENTIFICATION DIVISION.
        PROGRAM-ID. StOrGnMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStOrderGen".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdOrderGen.
           COPY ChlfdStock.

       WORKING-STORAGE SECTION.
       77  NEW-NO             PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-NUMBER          PIC X(23) VALUE " ".
       77  WS-SAVE            PIC X(23) VALUE " ".
       77  CALC-FIELD         PIC 9(7)V999 VALUE 0.
       01  WS-STDESC.
           03  WS-DESC1       PIC X(20) VALUE " ".
           03  WS-DESC2       PIC X(20) VALUE " ".
       01  WS-GEN-STATUS.
           03  WS-GEN-ST1     PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1   PIC 99.
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
            MOVE " " TO ORDER-GEN-REC.
            MOVE "N" TO NEW-NO
                        WS-END.
       GET-001.
            MOVE "SUPPLIER"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO OG-SUPPLIER.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-SAVE TO OG-KEY
                 PERFORM START-RECORD
                 PERFORM READ-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-004
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-004 THRU GET-005
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-PREVIOUS
              IF WS-END NOT = "Y"
                 GO TO GET-004
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-004 THRU GET-005
                 GO TO GET-000.
            IF OG-SUPPLIER = " "
                 GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-001.
       GET-002.
            MOVE "SEA-AIR"   TO F-FIELDNAME.
            MOVE 7           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO OG-SEA-AIR.
            IF F-EXIT-CH = X"01"
                 GO TO GET-001.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-SAVE TO OG-KEY
                 PERFORM START-RECORD
                 PERFORM READ-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-004
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-004 THRU GET-005
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-PREVIOUS
              IF WS-END NOT = "Y"
                 GO TO GET-004
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-004 THRU GET-005
                 GO TO GET-000.
            IF OG-SEA-AIR = " "
                 GO TO GET-002.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-002.
       GET-003.
            MOVE "STOCK"     TO F-FIELDNAME.
            MOVE 5           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 15          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO OG-STOCK-NUMBER
                                ST-STOCKNUMBER.
            IF F-EXIT-CH = X"01"
                 GO TO GET-002.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
             IF WS-END NOT = "Y"
               PERFORM GET-004 THRU GET-005
               GO TO GET-020
             ELSE
               PERFORM CLEAR-FORM
               PERFORM GET-004 THRU GET-005
               GO TO GET-999.
            IF F-EXIT-CH = X"05"
                 PERFORM REWRITE-RECORD
                 PERFORM READ-PREVIOUS
              IF WS-END NOT = "Y"
                 GO TO GET-020
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-004 THRU GET-005
                 GO TO GET-000.
            IF ST-STOCKNUMBER NOT = " "
                 PERFORM READ-STOCK.
            IF WS-STOCK-ST1 = 23 OR 35 OR 49
                 MOVE "THIS IS AN INVALID NUMBER, 'ESC' TO RE-ENTER."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-003.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-003.
            PERFORM READ-RECORD.
            IF NEW-NO = "Y"
               GO TO GET-020.
            GO TO GET-005.
       GET-004.
            MOVE "SUPPLIER"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE OG-SUPPLIER TO F-NAMEFIELD.
            MOVE 7           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SEA-AIR"  TO F-FIELDNAME.
            MOVE 7          TO F-CBFIELDNAME.
            MOVE OG-SEA-AIR TO F-NAMEFIELD.
            MOVE 1          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCK"         TO F-FIELDNAME.
            MOVE 5               TO F-CBFIELDNAME.
            MOVE OG-STOCK-NUMBER TO F-NAMEFIELD.
            MOVE 15              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            
            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.
            
            MOVE "DESC"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            MOVE WS-STDESC   TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-005.
            MOVE "QTY"       TO F-FIELDNAME.
            MOVE 3           TO F-CBFIELDNAME.
            MOVE OG-QUANTITY TO F-EDNAMEFIELDQTY.
            MOVE 5           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-QTY.
       GET-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QTY"        TO F-FIELDNAME.
            MOVE 3            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 5            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO OG-QUANTITY.
            IF NUMERIC-RATE NOT > 0
               MOVE
              "THIS FIELD CAN'T BE NEGATIVE OR ZERO, 'ESC' TO RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-020.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO GET-999.
            IF F-EXIT-CH = X"01"
                 GO TO GET-003.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
             IF WS-END NOT = "Y"
               PERFORM GET-004 THRU GET-005
               GO TO GET-020
             ELSE
               PERFORM CLEAR-FORM
               PERFORM GET-004 THRU GET-005
               GO TO GET-999.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-RECORD
               PERFORM READ-PREVIOUS
             IF WS-END NOT = "Y"
               PERFORM GET-004 THRU GET-005
               GO TO GET-020
             ELSE
               PERFORM CLEAR-FORM
               PERFORM GET-004 THRU GET-005
               GO TO GET-999.
            IF F-EXIT-CH = X"0A" OR = X"0B" OR = X"1B"
               PERFORM REWRITE-RECORD
               MOVE "N" TO NEW-NO
                           WS-END
               PERFORM CLSC-010
               PERFORM GET-004 THRU GET-005
               GO TO GET-003.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO GET-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               GO TO GET-020.
            GO TO GET-020.
       GET-999.
            EXIT.
      *
       READ-STOCK SECTION.
       RS-001.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
       RS-0021.
           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
       RS-999.
            EXIT.
      *
       DELETE-RECORD SECTION.
       DDR-000.
            IF NEW-NO = "Y"
                GO TO DDR-999.
       DDR-010.
            DELETE ORDER-GEN-FILE
               INVALID KEY NEXT SENTENCE.
            IF WS-GEN-ST1 NOT = 0
               MOVE "ST-ORDERGEN BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GEN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GEN-ST1
               GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       REWRITE-RECORD SECTION.
       RDR-010.
            IF OG-STOCK-NUMBER = " "
               GO TO RDR-999.
            IF NEW-NO = "Y"
               GO TO RDR-020.
       RDR-015.
            REWRITE ORDER-GEN-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-GEN-ST1 NOT = 0
               MOVE "ORDER-GEN BUSY ON REWRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GEN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GEN-ST1
               GO TO RDR-020.
            GO TO RDR-999.
       RDR-020.
            WRITE ORDER-GEN-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-GEN-ST1 NOT = 0
               MOVE "ORDER-GEN BUSY ON WRITE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GEN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GEN-ST1
               GO TO RDR-015.
       RDR-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE OG-KEY TO WS-NUMBER.
           START ORDER-GEN-FILE KEY NOT < OG-KEY.
        RD-010.
           READ ORDER-GEN-FILE WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-GEN-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-GEN-ST1
                MOVE "Y" TO NEW-NO
                MOVE WS-NUMBER TO OG-KEY
                GO TO RD-999.
           IF WS-GEN-ST1 NOT = 0
                MOVE "ORDER-GEN BUSY ON READ, PRESS 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-GEN-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-GEN-ST1
                GO TO RD-010.
           MOVE "N" TO NEW-NO.
           MOVE OG-KEY TO WS-SAVE.
           MOVE OG-STOCK-NUMBER TO ST-STOCKNUMBER
           PERFORM RS-0021.
       RD-999.
             EXIT.
      *
       START-RECORD SECTION.
       STR-000.
           MOVE WS-NUMBER TO OG-KEY.
           START ORDER-GEN-FILE KEY NOT < OG-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-GEN-ST1 NOT = 0
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO OG-KEY
               MOVE "Y" TO WS-END.
       STR-999.
             EXIT.
      *
       READ-NEXT SECTION.
       RNX-001.
           MOVE 0 TO WS-GEN-ST1.
       RNX-005.
           IF WS-END = "Y"
               GO TO RNX-999.
           READ ORDER-GEN-FILE NEXT WITH LOCK
             AT END
              MOVE "END OF NEXT-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO OG-KEY
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF WS-GEN-ST1 = 23 OR 35 OR 49
               MOVE "ORDER-GEN BUSY ON READ-NEXT-23, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GEN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GEN-ST1
               GO TO RNX-005.
           IF WS-GEN-ST1 NOT = 0
               MOVE "ORDER-GEN BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GEN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GEN-ST1
               PERFORM START-RECORD
               GO TO RNX-005.
           MOVE OG-KEY TO WS-NUMBER
                          WS-SAVE.
           MOVE "N" TO NEW-NO.
           MOVE OG-STOCK-NUMBER TO ST-STOCKNUMBER
           PERFORM RS-0021.
       RNX-999.
           EXIT.
      *
       READ-PREVIOUS SECTION.
       RPREV-001.
           MOVE 0 TO WS-GEN-ST1.
       RPREV-005.
           IF WS-END = "Y"
               GO TO RPREV-999.
           READ ORDER-GEN-FILE PREVIOUS WITH LOCK
             AT END
              MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO OG-KEY
               MOVE "Y" TO WS-END
               GO TO RPREV-999.
           IF WS-GEN-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-GEN-ST1
               MOVE "ORDER-GEN BUSY ON READ-PREV-23, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RPREV-005.
           IF WS-GEN-ST1 NOT = 0
               MOVE "ORDER-GEN BUSY ON READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GEN-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GEN-ST1
               PERFORM START-RECORD
               GO TO RPREV-005.
           MOVE OG-KEY TO WS-NUMBER
                          WS-SAVE.
           MOVE "N" TO NEW-NO.
           MOVE OG-STOCK-NUMBER TO ST-STOCKNUMBER
           PERFORM RS-0021.
       RPREV-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO OG-SUPPLIER
                         OG-SEA-AIR.
       CLSC-010.
             MOVE " " TO OG-STOCK-NUMBER
                         ST-DESCRIPTION1
                         ST-DESCRIPTION2.
             MOVE 0   TO OG-QUANTITY.
       CLSC-020.
             IF WS-END NOT = "Y"
                MOVE WS-NUMBER TO OG-KEY.
             IF WS-END = "Y"
                MOVE " " TO WS-SAVE WS-NUMBER.
             UNLOCK ORDER-GEN-FILE.
       CLSC-999.
             EXIT.      
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O ORDER-GEN-FILE.
            IF WS-GEN-ST1 = 49
               MOVE "ORDER-GEN BUSY I-O 49 ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-001.
 
      *      MOVE "ORDER GEN FILE OPENED I-O" TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
            GO TO OPEN-005.
       OPEN-001.
            OPEN OUTPUT ORDER-GEN-FILE.
            IF WS-GEN-ST1 NOT = 0
              MOVE "ORDER-GEN-FILE BUSY ON OPEN OUTPUT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-001.
 
      *      MOVE "ORDER GEN FILE OPENED OUPUT." TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       OPEN-005.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK-FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-STOCK-ST1
               GO TO OPEN-005.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StOrGnMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE ORDER-GEN-FILE
                  STOCK-MASTER.
      *      STOP RUN.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldQty".
       Copy "DisplayForm".
       Copy "UserFillField".
      *      
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      * END-OF-JOB
