        IDENTIFICATION DIVISION.
        PROGRAM-ID. BmKitCop.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
       Copy "SelectStMaster".
       Copy "SelectBmMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdToolkit.
      *
       WORKING-STORAGE SECTION.
       77  WS-QTY               PIC 999 VALUE 0.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "StMastIq". 
       77  WS-STOCKNUMBER       PIC X(34) VALUE " ".
       77  WS-TOOL-VALID        PIC X VALUE " ".
       77  WS-TOOL-INVALID      PIC X VALUE " ".
       77  WS-TOOL-NUMBER       PIC X(15) VALUE " ".
       77  WS-NEW-KIT           PIC X(15) VALUE " ".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-STOCK-STORE       PIC X(15) VALUE " ".
       01  WS-STDESC.
           03  WS-DESC1         PIC X(20) VALUE " ".
           03  WS-DESC2         PIC X(20) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-TOOLKIT-STATUS.
           03  WS-TOOLKIT-ST1   PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST     PIC X(20) VALUE " ".
           03  WS-DAILY-2ND     PIC X(20) VALUE " ".
           03  WS-DAILY-3RD     PIC X(20) VALUE " ".
           03  WS-DAILY-4TH     PIC X(20) VALUE " ".
       Copy "WsDateInfo".
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
           PERFORM OPEN-FILES.
       CONT-010.
           PERFORM CLEAR-SCREEN.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           PERFORM WRITE-NEW-KIT.
           GO TO CONT-010.
      *
       GET-DATA SECTION.
       GET-010.
           PERFORM SKL-020.
           MOVE "TOOLKIT" TO F-FIELDNAME.
           MOVE 7 TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
           IF F-EXIT-CH = X"0B"
               GO TO GET-010.
           MOVE 15 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO TO-TOOLKIT-NUMBER.
       GET-013.
           IF TO-TOOLKIT-NUMBER = " "
               CLOSE STOCK-MASTER
               PERFORM CLEAR-SCREEN
               CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
               CANCEL WS-INQUIRY-PROGRAM
               PERFORM OPEN-000
               PERFORM DISPLAY-FORM
               GO TO GET-010.
           PERFORM ERROR-020.
           MOVE TO-TOOLKIT-NUMBER TO WS-STOCKNUMBER
                                     WS-TOOL-NUMBER.
           PERFORM READ-STOCK.
           IF ST-DESCRIPTION1 = "NOT THERE!!!"
               MOVE "TOOLKIT NOT FOUND ON STOCKMASTER, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM START-KIT
               GO TO GET-010.

           PERFORM DNK-015.
           PERFORM READ-TOOLKIT-HEADER.
           IF WS-TOOL-INVALID = " "
              PERFORM SKL-010
              GO TO GET-050.
           IF WS-TOOL-INVALID NOT = " "
              MOVE "YOU CAN ONLY COPY AN EXISTING TOOLKIT, RE-ENTER"
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO GET-010.
       GET-050.
           PERFORM ERROR-020.
           MOVE "NEWKIT" TO F-FIELDNAME
           MOVE 6        TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
               GO TO GET-010.
           MOVE 15 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD       TO TO-TOOLKIT-NUMBER.
           MOVE TO-TOOLKIT-NUMBER TO WS-STOCKNUMBER
                                     WS-NEW-KIT.
           PERFORM READ-STOCK.
           IF ST-DESCRIPTION1 = "NOT THERE!!!"
                MOVE "TOOLKIT NOT FOUND ON STOCKMASTER, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-050.

            PERFORM DNK-025.
            PERFORM READ-NEW-TOOLKIT-HEADER.
            IF WS-TOOL-INVALID = "1"
               PERFORM SKL-020
               GO TO GET-999.
            IF WS-TOOL-INVALID = " "
               MOVE "THERE ALREADY IS A BILL-OF-MATERIAL FOR THIS KIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE TO-KEY TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-010.
       GET-999.
           EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
             MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
             START STOCK-MASTER KEY NOT < ST-KEY
                INVALID KEY NEXT SENTENCE.
       R-ST-010.
             READ STOCK-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                 MOVE " " TO ST-DESCRIPTION2
                             WS-TOOL-VALID
                 MOVE "NOT THERE!!!" TO ST-DESCRIPTION1
                 MOVE 0 TO ST-PRICE
                 GO TO R-ST-999.
             IF WS-STOCK-ST1 NOT = 0
                MOVE 0 TO WS-STOCK-ST1
                MOVE "STOCK BUSY ON READ, PRESS 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO R-ST-010.
             MOVE "1" TO WS-TOOL-VALID.
       R-ST-999.
             EXIT.
      *
       READ-NEXT-KIT SECTION.
       RNK-000.
           READ TOOLKITS NEXT
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               GO TO RNK-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "KIT BUSY ON READ NEXT, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNK-000.
       RNK-999.
           EXIT.
      *
       START-KIT SECTION.
       SK-010.
           MOVE WS-TOOL-NUMBER TO TO-TOOLKIT-NUMBER.
           MOVE " "            TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT LESS TO-KEY
                INVALID KEY NEXT SENTENCE.
       SK-999.
           EXIT.
      *
       READ-TOOLKIT-HEADER SECTION.
       RTH-000.
           MOVE WS-TOOL-NUMBER TO TO-TOOLKIT-NUMBER.
           MOVE " "            TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-KEY
                INVALID KEY NEXT SENTENCE.
       RTH-010.
           READ TOOLKITS
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               MOVE "1" TO WS-TOOL-INVALID
               MOVE " " TO WS-STOCKNUMBER
               GO TO RTH-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "TOOLKIT HEADER BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-010
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO RTH-000.
           MOVE " " TO WS-TOOL-INVALID.
       RTH-999.
           EXIT.
      *
       READ-NEW-TOOLKIT-HEADER SECTION.
       RNTH-000.
           MOVE WS-NEW-KIT TO TO-TOOLKIT-NUMBER.
           MOVE " "        TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-KEY
                INVALID KEY NEXT SENTENCE.
       RNTH-010.
           READ TOOLKITS
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               MOVE " " TO WS-STOCKNUMBER
               MOVE "1" TO WS-TOOL-INVALID
               GO TO RNTH-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLKIT HEADER BUSY ON READ-NEW, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-010
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RNTH-000.
           MOVE " " TO WS-TOOL-INVALID.
       RNTH-999.
           EXIT.
      *
       WRITE-NEW-KIT SECTION.
       WNK-005.
           MOVE 2910 TO POS.
           DISPLAY "WRITING NEW BILL-OF-MATERIAL......" AT POS.
           PERFORM START-KIT.
       WNK-010.
           PERFORM READ-NEXT-KIT.
           IF WS-TOOLKIT-ST1 = 10
              GO TO WNK-999.
           IF TO-TOOLKIT-NUMBER NOT = WS-TOOL-NUMBER
              GO TO WNK-999.
           MOVE WS-NEW-KIT TO TO-TOOLKIT-NUMBER.
           PERFORM WRITE-TOOLKIT.
           GO TO WNK-010.
       WNK-999.
           EXIT.
      *
       WRITE-TOOLKIT SECTION.
       WRT-010.
           WRITE TOOL-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              MOVE "TOOLKITS ST1=2 ON WRITE, 'ESC' FOR MORE."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE TO-COMPONENT-NUMBER TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRT-999.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE "TOOLKITS BUSY ON WRITE, WRT-010, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-TOOLKIT-ST1
              GO TO WRT-010.
       WRT-999.
           EXIT.
      *
       DISPLAY-NEXT-KIT SECTION.
       DNK-010.
           MOVE "TOOLKIT"         TO F-FIELDNAME.
           MOVE 7                 TO F-CBFIELDNAME.
           MOVE 15                TO F-CBFIELDLENGTH.
           MOVE TO-TOOLKIT-NUMBER TO F-NAMEFIELD.
           PERFORM WRITE-FIELD-ALPHA.
       DNK-015.
            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.
            
            MOVE "KT-DESC"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            MOVE WS-STDESC   TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       DNK-025.
            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.
            
            MOVE "ST-DESC"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            MOVE WS-STDESC   TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       DNK-999.
            EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-011.
            MOVE "TOOLKIT" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE 15        TO F-CBFIELDLENGTH.
            MOVE " "       TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "KT-DESC" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE 40        TO F-CBFIELDLENGTH.
            MOVE " "       TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
       CF-020.
            MOVE "ST-DESC" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE 40        TO F-CBFIELDLENGTH.
            MOVE " "       TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
       CF-999.
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
            OPEN I-O TOOLKITS.
            IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLFILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "BmKitCop"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STOCK-MASTER
                 TOOLKITS.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       SET-KBD-LED SECTION.
       SKL-010.
           DISPLAY "EXISTING ITEM" AT 0825.
       SKL-020.
           DISPLAY "NEW ITEM     " AT 0825.
       SKL-999.
          EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
      *
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
