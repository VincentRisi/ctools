        IDENTIFICATION DIVISION.
        PROGRAM-ID. BmKitsMt.
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
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-STOCK-STORE       PIC X(15) VALUE " ".
       01  WS-BLINKING-NAMES.
           03  WS-BLINK-F10-ON  PIC X(20) VALUE "* EXISTING ITEM *".
           03  WS-BLINK-F10-OFF PIC X(20) VALUE "**  NEW ITEM  ** ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-TOOLKIT-STATUS.
           03  WS-TOOLKIT-ST1   PIC 99.
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST     PIC X(20) VALUE " ".
           03  WS-DAILY-2ND     PIC X(20) VALUE " ".
           03  WS-DAILY-3RD     PIC X(20) VALUE " ".
           03  WS-DAILY-4TH     PIC X(20) VALUE " ".
       01  WS-STDESC.
           03  WS-DESC1          PIC X(20) VALUE " ".
           03  WS-DESC2          PIC X(20) VALUE " ".
       01 STOCK-STORES.
           03  Ws-StockLine         PIC X(15) OCCURS 200.
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
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-010.
      *
       SET-KBD-LED SECTION.
       SKL-010.
           DISPLAY WS-BLINK-F10-ON AT 1030 WITH
                 FOREGROUND-COLOR IS 4.
       SKL-020.
           DISPLAY WS-BLINK-F10-OFF AT 1030
                 WITH FOREGROUND-COLOR IS 4.
       SKL-999.
          EXIT.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE " " TO STOCK-STORES.
           MOVE 2410 TO POS.
           DISPLAY
           "Press 'F10' at the COMPONENT field to DELETE the Component"
                 AT POS.
           MOVE 2520 TO POS.
           DISPLAY "Or at the TOOLKIT field to DELETE the Toolkit"
                AT POS.
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

      *DELETE COMPLETE TOOLKIT
            IF F-EXIT-CH = X"1F"
                MOVE TO-TOOLKIT-NUMBER TO WS-TOOL-NUMBER
                PERFORM DELETE-TOOLKIT
                PERFORM CLEAR-FIELDS
                GO TO GET-010.
       GET-013.
            IF TO-TOOLKIT-NUMBER = " "
                CLOSE STOCK-MASTER
                PERFORM CLEAR-SCREEN
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE.
       GET-014.
            IF TO-TOOLKIT-NUMBER = " "
              OPEN I-O STOCK-MASTER
                IF WS-TOOLKIT-ST1 NOT = 0
                   MOVE 0 TO WS-TOOLKIT-ST1
                   MOVE "STOCK MASTER BUSY ON OPEN, 'ESC' TO RETRY."
                   TO WS-MESSAGE
                   PERFORM ERROR-MESSAGE
                   GO TO GET-014
                ELSE
                   PERFORM DISPLAY-FORM
                   GO TO GET-000.
            MOVE " " TO WS-MESSAGE.
            PERFORM ERROR-020.
            MOVE TO-TOOLKIT-NUMBER TO WS-STOCKNUMBER
                                      WS-TOOL-NUMBER.
            PERFORM READ-STOCK.
            IF ST-DESCRIPTION1 = "NOT THERE!!!"
                MOVE
                 "TOOLKIT NOT FOUND ON STOCKMASTER, 'ESC' TO RE-ENTER." 
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM START-KIT
      *          PERFORM READ-NEXT-KIT
                GO TO GET-010.

            PERFORM DNK-015.
            PERFORM READ-TOOLKIT-HEADER.
       GET-015.
            PERFORM SKL-020.

            MOVE "COMPONENT" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            MOVE 15          TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-010.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-FIELDS
                GO TO GET-010.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"0C"
             IF F-NAMEFIELD = " "
                GO TO GET-016.
            IF F-EXIT-CH = X"0C"
              IF WS-TOOL-INVALID = "1"
                PERFORM WRITE-TOOLKIT
              ELSE
                PERFORM REWRITE-TOOLKIT.
        GET-016.
            IF F-EXIT-CH NOT = X"0C"
                GO TO GET-017.
            MOVE WS-TOOL-NUMBER TO TO-TOOLKIT-NUMBER.
            PERFORM READ-NEXT-KIT.
            MOVE TO-TOOLKIT-NUMBER TO WS-STOCKNUMBER.
            PERFORM READ-STOCK.
            PERFORM DNK-010 THRU DNK-015.
            MOVE TO-COMPONENT-NUMBER TO WS-STOCKNUMBER.
            PERFORM READ-STOCK.
            PERFORM DNK-020 THRU DNK-025.
            GO TO GET-020.
         GET-017.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0C" AND NOT = X"1F"
               GO TO GET-030.
            IF F-NAMEFIELD = " "
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-015.
            MOVE F-NAMEFIELD TO TO-COMPONENT-NUMBER
                                WS-STOCKNUMBER.
            PERFORM READ-TOOLKIT.
            PERFORM READ-STOCK.
            IF ST-DESCRIPTION1 = "NOT THERE!!!"
                MOVE "INVALID STOCK ITEM !!" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-015.
       GET-020.
            IF WS-TOOL-INVALID = "1"
               PERFORM SKL-020
            ELSE
               PERFORM SKL-010.

            IF WS-TOOL-INVALID = "1"
                MOVE WS-QTY TO TO-QUANTITY.
            PERFORM DNK-025.
       GET-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "QUANTITY" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE 3          TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-015.
            IF F-EXIT-CH = X"07"
                PERFORM CF-020
                GO TO GET-015.
      *DELETE COMPONENT
            IF F-EXIT-CH = X"1F"
                PERFORM DELETE-COMPONENT
                PERFORM CF-020
                GO TO GET-015.
       GET-040.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO TO-QUANTITY.
            MOVE 3            TO F-CBFIELDLENGTH.
            MOVE TO-QUANTITY  TO F-EDNAMEFIELDKITQTY WS-QTY.
            PERFORM WRITE-FIELD-KITQTY.
            
            IF TO-COMPONENT-NUMBER NOT = " "
             IF TO-QUANTITY = 0
               MOVE "QUANTITY MUST BE > THAN 0" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO GET-030.
      *<RETURN/GO>
            IF F-EXIT-CH = X"0A" OR = X"1B"
              IF WS-TOOL-INVALID = "1"
                PERFORM WRITE-TOOLKIT
                GO TO GET-050
            ELSE
                PERFORM REWRITE-TOOLKIT
                GO TO GET-050.
      *<NEXT-PAGE>
            IF F-EXIT-CH = X"0C"
              IF WS-TOOL-INVALID = "1"
                PERFORM WRITE-TOOLKIT
            ELSE
                PERFORM REWRITE-TOOLKIT.
            MOVE WS-TOOL-NUMBER TO TO-TOOLKIT-NUMBER.
            PERFORM READ-NEXT-KIT.
            MOVE TO-TOOLKIT-NUMBER TO WS-STOCKNUMBER.
            PERFORM READ-STOCK.
            PERFORM DNK-010 THRU DNK-015.
            MOVE TO-COMPONENT-NUMBER TO WS-STOCKNUMBER.
            PERFORM READ-STOCK.
            PERFORM DNK-020 THRU DNK-025.
            GO TO GET-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0C" AND NOT = X"1F"
               GO TO GET-030.
       GET-050.
            MOVE " " TO WS-TOOL-INVALID.
            PERFORM CF-020.
            GO TO GET-015.
       GET-999.
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
            
            MOVE "KITDESC" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE WS-STDESC TO F-NAMEFIELD
            MOVE 40        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       DNK-020.
            MOVE "COMPONENT"         TO F-FIELDNAME.
            MOVE 9                   TO F-CBFIELDNAME.
            MOVE 15                  TO F-CBFIELDLENGTH.
            MOVE TO-COMPONENT-NUMBER TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
       DNK-025.
            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.
            
            MOVE "DESC"    TO F-FIELDNAME.
            MOVE 4         TO F-CBFIELDNAME.
            MOVE WS-STDESC TO F-NAMEFIELD
            MOVE 40        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QUANTITY"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE TO-QUANTITY TO F-EDNAMEFIELDKITQTY.
            MOVE 3           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-KITQTY.
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

            MOVE "KITDESC"  TO F-FIELDNAME.
            MOVE 7          TO F-CBFIELDNAME.
            MOVE 40         TO F-CBFIELDLENGTH.
            MOVE " "        TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
       CF-020.
            MOVE "COMPONENT" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            MOVE 15          TO F-CBFIELDLENGTH.
            MOVE " "         TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESC"  TO F-FIELDNAME.
            MOVE 4       TO F-CBFIELDNAME.
            MOVE 40      TO F-CBFIELDLENGTH.
            MOVE " "     TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QUANTITY" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE 3          TO F-CBFIELDLENGTH.
            MOVE " "        TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            
           IF WS-TOOLKIT-ST1 = 51
               UNLOCK TOOLKITS.
       CF-999.
            EXIT.
      *
       START-STOCK SECTION.
       S-ST-000.
             MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
             START STOCK-MASTER KEY NOT < ST-KEY
                 INVALID KEY NEXT SENTENCE.
       S-ST-999.
             EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
             MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
             START STOCK-MASTER KEY NOT < ST-KEY.
       R-ST-010.
             READ STOCK-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                 MOVE " " TO ST-DESCRIPTION2
                             WS-TOOL-VALID
                 MOVE "NOT THERE!!!" TO ST-DESCRIPTION1
                 MOVE 0              TO ST-PRICE
                 PERFORM START-KIT-AGAIN
                 GO TO R-ST-999.
             IF WS-STOCK-ST1 NOT = 0
                MOVE 0 TO WS-STOCK-ST1
                MOVE "STOCK BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO R-ST-010.
             MOVE "1" TO WS-TOOL-VALID.
       R-ST-999.
             EXIT.
      *
       READ-TOOLKIT SECTION.
       RT-000.
           MOVE WS-TOOL-NUMBER TO TO-TOOLKIT-NUMBER.
           MOVE WS-STOCKNUMBER TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-KEY
             INVALID KEY NEXT SENTENCE.
      *     IF WS-TOOLKIT-ST1 NOT = 0
      *         MOVE "BAD START ON READING KIT, PRESS 'ESC' TO EXIT."
      *         TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE.
       RT-010.
           READ TOOLKITS WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               MOVE "1" TO WS-TOOL-INVALID
               GO TO RT-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLKIT BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RT-010.
           MOVE " " TO WS-TOOL-INVALID.
       RT-999.
           EXIT.
      *
       READ-NEXT-KIT SECTION.
       RNK-000.
           READ TOOLKITS NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 51
               MOVE "1" TO WS-TOOL-INVALID
               GO TO RT-999.
           IF WS-TOOLKIT-ST1 = 10
               GO TO RNK-999.
           IF WS-TOOLKIT-ST1 NOT = 0
           MOVE 
              "TOOLKIT BUSY ON READ-NEXT - RNK-000, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               PERFORM START-KIT
               GO TO RNK-000.
       RNK-010.
           MOVE TO-TOOLKIT-NUMBER   TO WS-TOOL-NUMBER
           MOVE TO-COMPONENT-NUMBER TO WS-STOCKNUMBER.
       RNK-020.
           MOVE " "                 TO WS-TOOL-INVALID.
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
       START-KIT-AGAIN SECTION.
       SKA-010.
           IF TO-COMPONENT-NUMBER = " "
               GO TO SKA-999.
           MOVE WS-TOOL-NUMBER TO TO-TOOLKIT-NUMBER.
           MOVE WS-STOCKNUMBER TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT LESS TO-KEY
                INVALID KEY NEXT SENTENCE.
       SKA-999.
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
               MOVE " " TO WS-STOCKNUMBER
               PERFORM WRITE-TOOLKIT
               GO TO RTH-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLKIT HEADER BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RTH-000.
           PERFORM REWRITE-TOOLKIT.
       RTH-999.
           EXIT.
      *
       WRITE-TOOLKIT SECTION.
       WRT-000.
           IF WS-TOOL-VALID = " "
            IF TO-COMPONENT-NUMBER NOT = " "
               GO TO WRT-999.
           MOVE WS-TOOL-NUMBER TO TO-TOOLKIT-NUMBER.
           MOVE WS-STOCKNUMBER TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-KEY.
       WRT-010.
           WRITE TOOL-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              GO TO WRT-999.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE 0 TO WS-TOOLKIT-ST1
              MOVE "TOOLKITS BUSY ON WRITE, WRT-010, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRT-010.
       WRT-999.
           EXIT.
      *
       REWRITE-TOOLKIT SECTION.
       REWRT-000.
           IF WS-TOOL-VALID = " "
            IF TO-COMPONENT-NUMBER NOT = " "
               GO TO REWRT-999.
           REWRITE TOOL-REC
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              GO TO REWRT-999.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE 0 TO WS-TOOLKIT-ST1
              MOVE "TOOLKIT NOT WRITTEN, REWRT-000, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO REWRT-000.
       REWRT-999.
           EXIT.
      *
       DELETE-TOOLKIT SECTION.
       DT-000.
      *     PERFORM CLEAR-010.
           MOVE " " TO STOCK-STORES.
           MOVE 2610 TO POS.
           DISPLAY "ARE YOU SURE YOU WANT TO DELETE THE WHOLE TOOLKIT??"
              AT POS.
           MOVE 2710 TO POS.
           DISPLAY "ENTER (Y Or N) : [ ]" AT POS.
           MOVE 2727 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 24        TO CDA-ROW.
           MOVE 27        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER.

      *     ACCEPT WS-ANSWER AT POS.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2
              GO TO DT-005
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO DT-000.
       DT-005.
           IF WS-ANSWER = "Y"
              GO TO DT-010.
           IF WS-ANSWER = "N"
              MOVE 2610 TO POS
              MOVE " " TO WS-MESSAGE
              DISPLAY WS-MESSAGE AT POS
              MOVE 2710 TO POS
              DISPLAY WS-MESSAGE AT POS
              GO TO DT-999.
           DISPLAY " " AT 3079 WITH BELL.
           PERFORM ERROR1-020
           PERFORM ERROR-020.
           GO TO DT-000.
       DT-010.
           MOVE WS-TOOL-NUMBER TO TO-TOOLKIT-NUMBER.
           MOVE " "            TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               MOVE "TRY A TOOLKIT THAT EXISTS!!!!!" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO WS-MESSAGE
               MOVE 2810 TO POS
               DISPLAY WS-MESSAGE AT POS
               MOVE 2910 TO POS
               DISPLAY WS-MESSAGE AT POS
               GO TO DT-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLKITS BUSY ON START, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DT-010.
           MOVE 1 TO SUB-1.
       DT-020.
           READ TOOLKITS NEXT
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               MOVE 1 TO SUB-1
               GO TO DT-040.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLKITS BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DT-020.
           IF TO-TOOLKIT-NUMBER = WS-TOOL-NUMBER
               GO TO DT-030.
           MOVE " " TO WS-MESSAGE
           MOVE 2810 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 1 TO SUB-1
           GO TO DT-040.
       DT-030.
           MOVE TO-COMPONENT-NUMBER TO Ws-StockLine (SUB-1).
           ADD 1 TO SUB-1.
           IF SUB-1 < 200
               GO TO DT-020.
           MOVE 1 TO SUB-1.
       DT-040.
           IF SUB-1 > 200
               GO TO DT-999.
           IF SUB-1 = 1
               MOVE WS-TOOL-NUMBER TO TO-TOOLKIT-NUMBER
               MOVE " "            TO TO-COMPONENT-NUMBER
               GO TO DT-045.
           IF Ws-StockLine (SUB-1) = " "
               MOVE " " TO WS-MESSAGE
               MOVE 2810 TO POS
               DISPLAY WS-MESSAGE AT POS
               MOVE 2910 TO POS
               DISPLAY WS-MESSAGE AT POS
               GO TO DT-999.
           MOVE WS-TOOL-NUMBER       TO TO-TOOLKIT-NUMBER.
           MOVE Ws-StockLine (SUB-1) TO TO-COMPONENT-NUMBER.
       DT-045.
           READ TOOLKITS WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               ADD 1 TO SUB-1
               GO TO DT-040.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLKIT RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DT-045.
       DT-050.
           DELETE TOOLKITS
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               ADD 1 TO SUB-1
               GO TO DT-040.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLKIT RECORD BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DT-050.
           ADD 1 TO SUB-1.
           GO TO DT-040.
       DT-999.
           EXIT.
      *
       DELETE-COMPONENT SECTION.
       DC-030.
           DELETE TOOLKITS
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               MOVE "COMPONENT NOT DELETED, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DC-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLKIT RECORD BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO DC-030.
       DC-999.
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
               MOVE "TOOLKIT FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "BmKitsMt"      TO F-FORMNAME
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
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldKitQty".
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
       Copy "CTOSCobolAccept".
      * END-OF-JOB
