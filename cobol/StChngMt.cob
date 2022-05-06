        IDENTIFICATION DIVISION.
        PROGRAM-ID. StChngMt.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStChanges".
          Copy "SelectSlDaily".
          Copy "SelectCoDataName".
          Copy "SelectStOrders".
          Copy "SelectStTrans".
          Copy "SelectStImports".
          Copy "SelectStReceipt".
          Copy "SelectStReceiptLy".
          Copy "SelectBmMaster".
          Copy "SelectStBranchCat".
           SELECT COMPANY-MENU ASSIGN TO "CoCompany"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-MENU-STATUS
               RECORD KEY IS PTY-KEY.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdCompany.
           COPY ChlfdStockChanges.
           COPY ChlfdDaily.
           Copy ChlfdDataName.
           COPY ChlfdOutOrd.
           COPY ChlfdStTrans.
           COPY ChlfdImpReceipts.
           COPY ChlfdStkReceipts.
           COPY ChlfdStkReceiptsLy.
           COPY ChlfdToolkit.
           COPY ChlfdStBranchCat.
      *
       WORKING-STORAGE SECTION.
       77  WS-CBSTOCK         PIC 9(3) VALUE 11.
       77  WS-CBSTTRANS       PIC 9(3) VALUE 12.
       77  WS-CBSTRECEIPTS    PIC 9(3) VALUE 13.
       77  WS-CBSTRECEIPTSLY  PIC 9(3) VALUE 14.
       77  WS-CBSTIMPORTS     PIC 9(3) VALUE 15.
       77  WS-CBSTORDERS      PIC 9(3) VALUE 16.
       77  WS-CBSTCHANGE      PIC 9(3) VALUE 20.
       77  WS-CBDAILYEX       PIC 9(3) VALUE 21.
       77  WS-CBTOOLKIT       PIC 9(3) VALUE 60.
       77  WS-CBSTBRANCHCAT   PIC 9(3) VALUE 97.
       77  WS-COMPANY-UPDATE  PIC XX VALUE " ".      
       77  WS-UPDATE          PIC X VALUE " ".      
       77  NEW-STOCKNO        PIC X VALUE " ".      
       77  WS-NEWSTOCKNUMBER  PIC X(15) VALUE " ".
       77  WS-OLDSTOCKNUMBER  PIC X(15) VALUE " ".
       77  WS-TOOL-VALID      PIC X VALUE " ".
       77  WS-TOOLKIT-INVALID PIC X VALUE " ".
       77  WS-TOOLKITNUMBER   PIC X(15) VALUE " ".
       77  WS-NEW-KIT         PIC X(15) VALUE " ".
       77  WS-QTY             PIC 9(3) VALUE 0.
       77  WS-RANGE1          PIC X(15) VALUE " ".      
       77  WS-RANGE2          PIC X(15) VALUE " ".      
       77  WS-RANGE3          PIC X VALUE " ".      
       77  WS-RANGE4          PIC X VALUE " ".      
       77  WS-RANGE5          PIC X VALUE " ".      
       77  WS-CATEGORY        PIC X(3) VALUE " ".      
       77  WS-NUMBER          PIC 9(6) VALUE 0.
       77  WS-LEAP-YEAR       PIC X VALUE " ".
       77  WS-AMOUNT          PIC 9(3)V99 VALUE 0.
       01  WS-MERGE-STOCK.
           03  WS-QTYONHAND          PIC 9(6).
           03  WS-QTYONRESERVE       PIC 9(6).
           03  WS-QTYONORDER         PIC 9(6).
           03  WS-QTYONBORDER        PIC 9(6).
           03  WS-QTY-ST-TAKE        PIC 9(6).
           03  WS-QTYRECMTD          PIC S9(6).
           03  WS-QTYRECYTD          PIC S9(6).
           03  WS-QTYRECLAST         PIC S9(6).
           03  WS-QTYADJMTD          PIC S9(6).
           03  WS-QTYADJYTD          PIC S9(6).
           03  WS-QTYADJLAST         PIC S9(6).
           03  WS-SALESUNITMTD       PIC S9(6).
           03  WS-SALESUNITSYTD      PIC S9(6).
           03  WS-SALESUNITSLAST     PIC S9(6).
           03  WS-SALESRANDSMTD      PIC S9(7)V99.
           03  WS-SALESRANDSYTD      PIC S9(7)V99.
           03  WS-SALESRANDSLAST     PIC S9(7)V99.
           03  WS-SALESCOSTMTD       PIC S9(7)V99.
           03  WS-SALESCOSTYTD       PIC S9(7)V99.
           03  WS-SALESCOSTLAST      PIC S9(7)V99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1          PIC 99.
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1         PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1             PIC 99.
       01  WS-IMPRECEIPT-STATUS.
           03  WS-IMPRECEIPT-ST1     PIC 99.
       01  WS-STKRECEIPT-STATUS.
           03  WS-STKRECEIPT-ST1     PIC 99.
       01  WS-STKRECEIPTSLY-STATUS.
           03  WS-STKRECEIPTSLY-ST1  PIC 99.
       01  WS-STCHANGE-STATUS.
           03  WS-STCHANGE-ST1       PIC 99.
       01  WS-MENU-STATUS.
           03  WS-MENU-ST1           PIC 99.
       01  WS-TOOLKIT-STATUS.
           03  WS-TOOLKIT-ST1        PIC 99.
       01  WS-STBRCAT-STATUS.
           03  WS-STBRCAT-ST1        PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1          PIC 99.
       01  WS-DATA-STATUS.
           03  WS-DATA-ST1           PIC 99.
       01 COMANIES-NAME-LIST.
         02  COMPANIES-LIST OCCURS 20.
           03  LIST-NAME        PIC X(40) VALUE " ".
           03  LIST-NUMBER      PIC 99.
           03  LIST-VOL-DIR     PIC X(25).
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
        Copy "WsDateInfo".
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       01  F-FORMS.
           03  F-FIELDNAME            PIC X(40).
           03  F-EDNAMEFIELDAMOUNT    PIC Z(3)9.
           03  F-ERROR1               PIC 9(4) COMP.
           03  F-ERROR5               PIC 9(4) COMP-X.
           03  F-FILENAME             PIC X(40) VALUE SPACES.
           03  F-CBFILENAME           PIC 9(4) COMP VALUE 0.
           03  F-FH                   PIC 9(4) COMP.
           03  F-INTEGERZERO          PIC 9(4) COMP VALUE 0.
           03  F-OPENMODE             PIC X(2) VALUE "mr".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 0317 TO POS
           DISPLAY "*** BRANCH STOCK FILE UPDATE PROGRAM ***" AT POS
           MOVE 0417 TO POS
           DISPLAY "****************************************" AT POS.
       CONT-010.
           MOVE 0 TO F-ERROR5.
           MOVE 2610 TO POS
           DISPLAY "CHECKING DATA-NAMES........." AT POS.
           PERFORM CHECK-DATA-NAMES.
           
           MOVE 2610 TO POS
           DISPLAY "READING NEXT-COMPANY........" AT POS.
           PERFORM READ-NEXT-COMPANY.
       CONT-015.
           MOVE 2610 TO POS
           DISPLAY "SELECTING BRANCH TO UPDATE..." AT POS.
           PERFORM SELECT-BRANCH.
           
           MOVE 2610 TO POS
           DISPLAY "OPENING BRANCH FILES........" AT POS.
           PERFORM CHECK-BRANCH-DATA-NAMES.
           IF F-ERROR5 NOT = 0 AND NOT = 220
               GO TO CONT-015.
           
           PERFORM OPEN-FILES.
           MOVE 2610 TO POS
           DISPLAY "                             " AT POS.
           PERFORM GET-DATA.
           IF WS-RANGE3 NOT = "D"
               PERFORM READ-NEXT-RENAME.
           PERFORM READ-NEXT-CHANGE.
           PERFORM END-000.
           PERFORM ERROR1-020
           PERFORM ERROR-020
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2610 TO POS
           DISPLAY "OPENING HOME OFFICE FILES........" AT POS.
           PERFORM CHECK-DATA-NAMES.
           MOVE WS-VOL-DIR TO LIST-VOL-DIR (SUB-50).
           PERFORM CHECK-BRANCH-DATA-NAMES.
           PERFORM END-OFF.
       CONT-999.
           Exit.
      *
       CHECK-DATA-NAMES SECTION.
       CDN-005.
          MOVE " " TO ALPHA-RATE.
          MOVE 0   TO SUB-1.
       CDN-010.
          MOVE WS-VOL-DIR TO ALPHA-RATE.
       CDN-015.
          ADD 1 TO SUB-1.
          IF SUB-1 NOT > 60
           IF AL-RATE (SUB-1) NOT = " "
            GO TO CDN-015.
       CDN-020.
          MOVE WS-DATA-FILE TO DATA-RATE.
          MOVE 1            TO SUB-2.
       CDN-025.
          MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
          ADD 1 TO SUB-1 SUB-2.
          IF DAT-RATE (SUB-2) NOT = " "
           IF SUB-1 NOT > 60
             GO TO CDN-025.
       CDN-030.
          MOVE ALPHA-RATE TO WS-DATA
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE WS-VOL-DIR TO ALPHA-RATE.
          
          PERFORM OPEN-DATA-005
          PERFORM READ-DATAFILE.
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE WS-VOL-DIR TO ALPHA-RATE

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-STCHANGE TO DATA-RATE
          PERFORM CDN-025
          MOVE ALPHA-RATE TO WS-STCHANGE
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE WS-VOL-DIR TO ALPHA-RATE.
          
          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-DAILYEX TO DATA-RATE
          PERFORM CDN-025
          MOVE ALPHA-RATE TO WS-DAILYEX
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE WS-VOL-DIR TO ALPHA-RATE.

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-STBRANCHCAT TO DATA-RATE
          PERFORM CDN-025
          MOVE ALPHA-RATE TO WS-STBRANCHCAT
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE WS-VOL-DIR TO ALPHA-RATE.
       CDN-900.
          CLOSE DATA-FILE.
       CDN-999.
          EXIT.
      *
       CHECK-BRANCH-DATA-NAMES SECTION.
       CBDN-005.
          MOVE " " TO ALPHA-RATE.
          MOVE 0   TO SUB-1.
       CBDN-010.
          MOVE LIST-VOL-DIR (SUB-50) TO ALPHA-RATE.
       CBDN-015.
          ADD 1 TO SUB-1.
          IF SUB-1 NOT > 60
           IF AL-RATE (SUB-1) NOT = " "
            GO TO CBDN-015.
       CBDN-020.
          MOVE WS-DATA-FILE TO DATA-RATE.
          MOVE 1            TO SUB-2.
       CBDN-025.
          MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
          ADD 1 TO SUB-1 SUB-2.
          IF DAT-RATE (SUB-2) NOT = " "
           IF SUB-1 NOT > 60
             GO TO CBDN-025.
       CBDN-030.
          MOVE ALPHA-RATE TO WS-DATA
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE LIST-VOL-DIR (SUB-50) TO ALPHA-RATE.

          PERFORM OPEN-DATA
          IF F-ERROR5 NOT = 0 AND NOT = 220
             GO TO CBDN-999.
             
          PERFORM READ-BRANCH-DATAFILE.

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-STOCK TO DATA-RATE
          PERFORM CBDN-025
          MOVE ALPHA-RATE TO WS-STOCK
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE LIST-VOL-DIR (SUB-50) TO ALPHA-RATE.

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-STTRANS TO DATA-RATE
          PERFORM CBDN-025
          MOVE ALPHA-RATE TO WS-STTRANS
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE LIST-VOL-DIR (SUB-50) TO ALPHA-RATE.

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-STRECEIPT TO DATA-RATE
          PERFORM CBDN-025
          MOVE ALPHA-RATE TO WS-STRECEIPT
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE LIST-VOL-DIR (SUB-50) TO ALPHA-RATE.

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-STRECEIPTLY TO DATA-RATE
          PERFORM CBDN-025
          MOVE ALPHA-RATE TO WS-STRECEIPTLY
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE LIST-VOL-DIR (SUB-50) TO ALPHA-RATE.

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-STORDERS TO DATA-RATE
          PERFORM CBDN-025
          MOVE ALPHA-RATE TO WS-STORDERS
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE LIST-VOL-DIR (SUB-50) TO ALPHA-RATE.

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-STIMPORTS TO DATA-RATE
          PERFORM CBDN-025
          MOVE ALPHA-RATE TO WS-STIMPORTS
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE LIST-VOL-DIR (SUB-50) TO ALPHA-RATE.

          SUBTRACT SUB-2 FROM SUB-1
          ADD 1 TO SUB-1
          MOVE 1 TO SUB-2
          MOVE WS-TOOLKIT TO DATA-RATE
          PERFORM CBDN-025
          MOVE ALPHA-RATE TO WS-TOOLKIT
          MOVE " " TO ALPHA-RATE DATA-RATE
          MOVE LIST-VOL-DIR (SUB-50) TO ALPHA-RATE.
       CBDN-999.
          EXIT.
      *
       READ-DATAFILE SECTION.
       RC-005.
           MOVE WS-CBSTCHANGE TO DATA-NUMBER.
           START DATA-FILE KEY NOT < DATA-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DATA-ST1 NOT = 0
               GO TO RC-999.
       RC-010.
           READ DATA-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-DATA-ST1 NOT = 0
               MOVE "DATAFILE BUSY ON READ, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DATA-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE DATA-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RC-010.
       RC-020.
           MOVE DATA-NAME        TO WS-STCHANGE.
           
           MOVE WS-CBDAILYEX     TO DATA-NUMBER
           PERFORM RC-010
           MOVE DATA-NAME        TO WS-DAILYEX.

           MOVE WS-CBSTBRANCHCAT TO DATA-NUMBER
           PERFORM RC-010
           MOVE DATA-NAME        TO WS-STBRANCHCAT.
       RC-999.
           EXIT.
      *
       READ-BRANCH-DATAFILE SECTION.
       RBC-005.
           MOVE WS-CBSTOCK TO DATA-NUMBER.
           START DATA-FILE KEY NOT < DATA-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DATA-ST1 NOT = 0
               GO TO RBC-999.
       RBC-010.
           READ DATA-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-DATA-ST1 NOT = 0
               MOVE "BRANCH DATAFILES BUSY ON READ, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DATA-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE DATA-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RBC-010.
       RBC-020.
           MOVE DATA-NAME         TO WS-STOCK.
           
           MOVE WS-CBSTTRANS      TO DATA-NUMBER.
           PERFORM RBC-010
           MOVE DATA-NAME         TO WS-STTRANS.
           
           MOVE WS-CBSTORDERS     TO DATA-NUMBER.
           PERFORM RBC-010
           MOVE DATA-NAME         TO WS-STORDERS.
           
           MOVE WS-CBSTIMPORTS    TO DATA-NUMBER.
           PERFORM RBC-010
           MOVE DATA-NAME         TO WS-STIMPORTS.
           
           MOVE WS-CBSTRECEIPTS   TO DATA-NUMBER.
           PERFORM RBC-010
           MOVE DATA-NAME         TO WS-STRECEIPT.
           
           MOVE WS-CBSTRECEIPTSLY TO DATA-NUMBER.
           PERFORM RBC-010
           MOVE DATA-NAME         TO WS-STRECEIPTLY
           
           MOVE WS-CBTOOLKIT      TO DATA-NUMBER.
           PERFORM RBC-010
           MOVE DATA-NAME         TO WS-TOOLKIT.
       RBC-900.
           CLOSE DATA-FILE.
       RBC-999.
           EXIT.
      *
       CHECK-DATA-SIZE SECTION.         
       CDS-005.
           MOVE " " TO DATA-RATE.
           MOVE 0   TO SUB-10.
       CDS-015.
           ADD 1 TO SUB-10.
           IF DAT-RATE (SUB-10) NOT = " "
            IF SUB-10 NOT > 60
            GO TO CDS-015.
          SUBTRACT 1 FROM SUB-10.
       CDS-999.
          EXIT.
      *
       OPEN-NODE-FILE SECTION.
       ONF-001.
      * USED TO BE USED IN CTOS WHEN THE BRANCHES WERE ON SEPERATE
      * SERVERS.  NOW WE ARE ALL ON THE SAME SERVER THIS CODE BECOMES
      * REDUNDANT. 
           PERFORM CDS-005.
           MOVE WS-DATA TO DATA-RATE.
           PERFORM CDS-015.

           IF DAT-RATE (1) NOT = "{"
                MOVE 0 TO F-ERROR5
                GO TO ONF-999.

           MOVE WS-DATA         TO F-FILENAME
           MOVE SUB-10          TO F-CBFILENAME.
           CALL "OPENFILE" USING   F-ERROR5
                                   F-FH
                                   F-FILENAME
                                   F-CBFILENAME
                                   F-FILENAME
                                   F-INTEGERZERO
                                   F-OPENMODE.

           IF F-ERROR5 NOT = 0
            IF F-ERROR5 NOT = 220
               GO TO ONF-999.

           IF F-ERROR5 NOT = 210
               CALL "CLOSEFILE" USING  F-ERROR5
                                       F-FH.
       ONF-999.
            EXIT.
      *
       OPEN-DATA SECTION.
       OPEN-DATA-001.
            PERFORM OPEN-NODE-FILE.
            IF F-ERROR5 NOT = 0 AND NOT = 220
                PERFORM ERROR1-020
                MOVE
          "THE DIGINET LINK IS NOT CURRENTLY RUNNING, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE "B" TO WS-PASSWORD-VALID.
       OPEN-DATA-005.
           IF F-ERROR5 = 0 OR = 220
            OPEN I-O DATA-FILE
           IF WS-DATA-ST1 NOT = 0
               MOVE "DATAFILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DATA-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               
               MOVE DATA-NUMBER TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DATA TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO OPEN-DATA-005.
       OPEN-DATA-999.
           EXIT.
      *
       SELECT-BRANCH SECTION.
       SB-005.
           MOVE 0610 TO POS
           DISPLAY "STOCK TO BE UPDATED FOR COMPANY No ? [  ]"
           AT POS
           ADD 38 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 3         TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COMPANY-UPDATE.

           IF W-ESCAPE-KEY = 3
              EXIT PROGRAM.
           IF WS-COMPANY-UPDATE = " "
               GO TO SB-005.
           MOVE WS-COMPANY-UPDATE TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           MOVE NUMERIC-RATE TO SUB-50.
           DISPLAY SUB-50 AT POS.
           IF LIST-NAME (SUB-50) = " "
               MOVE
            "PLEASE ENTER A COMPANY THAT EXISTS, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO SB-005.
           PERFORM ERROR-020.
           IF LIST-NAME (SUB-50) = WS-CO-NUMBER
               MOVE 
            "YOU MAY NOT UPDATE YOUR OWN COMPANY, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO SB-005.
           MOVE 0810 TO POS
           DISPLAY"Company Selected :" AT POS
           ADD 19 TO POS
           DISPLAY LIST-NAME (SUB-50) AT POS
           MOVE 0929 TO POS
           DISPLAY List-VOL-DIR (SUB-50) AT POS.
       SB-020.
           MOVE 1010 TO POS
           DISPLAY "ARE YOU SURE THIS IS THE COMPANY TO UPDATE [ ]"
           AT POS
           ADD 44 TO POS 

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 53        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-UPDATE.

        IF W-ESCAPE-KEY = 4
               GO TO SB-005.
        IF WS-UPDATE NOT = "N" AND NOT = "Y"
              MOVE "THIS ENTRY MUST BE EITHER Y OR N, RE-ENTER."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO SB-020.
           PERFORM ERROR-020.
           IF WS-UPDATE = "N"
              GO TO SB-005.
       SB-999.
           EXIT.
      *
       READ-NEXT-COMPANY SECTION.
       RNC-005.
           OPEN I-O COMPANY-MENU.
           IF WS-MENU-ST1 NOT = 0
               MOVE "COMPANY FILE BUSY ON OPEN, GOING TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-MENU-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-MENU-ST1
               GO TO RNC-005.
           MOVE 1 TO PTY-NUMBER.
           START COMPANY-MENU KEY NOT < PTY-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-MENU-ST1 NOT = 0
               GO TO RNC-900.
           MOVE 1 TO SUB-50.
       RNC-010.
           READ COMPANY-MENU NEXT
               AT END NEXT SENTENCE.
           IF WS-MENU-ST1 = 10
               GO TO RNC-900.
           IF WS-MENU-ST1 NOT = 0
               MOVE "COMPANY FILE BUSY ON READ-NEXT, GOING TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-MENU-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-MENU-ST1
               GO TO RNC-010.
           MOVE PTY-VOL-DIR TO LIST-VOL-DIR (SUB-50)
           MOVE PTY-NUMBER  TO LIST-NUMBER (SUB-50)
           MOVE PTY-CO-NAME TO LIST-NAME (SUB-50).
           
           IF SUB-50 < 20
              ADD 1 TO SUB-50
              GO TO RNC-010.
       RNC-900.
           CLOSE COMPANY-MENU.
       RNC-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-005.
           MOVE 1210 TO POS
           DISPLAY "STOCK NUMBER FROM: [               ]" AT POS
           ADD 20 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 29        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-005.
       GET-010.
           MOVE 1310 TO POS
           DISPLAY "STOCK NUMBER TO  : [               ]" AT POS
           ADD 20 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 10        TO CDA-ROW.
           MOVE 29        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE2.

           IF W-ESCAPE-KEY = 4
              GO TO GET-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-020.
           MOVE 2910 TO POS
           DISPLAY "Reading Number of Stock changes to make.....  "
               AT POS.
           PERFORM READ-ALL-CHANGES.
           PERFORM ERROR-020
           MOVE 2701 TO POS
           DISPLAY WS-MESSAGE AT POS.
       GET-030.
           MOVE 1810 TO POS
           DISPLAY "ENTER Y, N OR D=DELETE ONLY - NO POSTING." AT POS
           MOVE 1710 TO POS
           DISPLAY "SHOULD WE DELETE THE CHANGE FILE  : [ ]" AT POS
           ADD 37 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE3.

           IF W-ESCAPE-KEY = 4
              GO TO GET-010.
           IF WS-RANGE3 NOT = "N" AND NOT = "Y" AND NOT = "D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-035
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-035.
           MOVE 2110 TO POS
           DISPLAY "ARE YOU SURE YOU WISH TO CONTINUE : [ ]" AT POS
           ADD 37 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 18        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE4.

           IF W-ESCAPE-KEY = 4
              GO TO GET-030.
           IF WS-RANGE4 NOT = "N" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-035.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-036
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-035.
       GET-036.
           MOVE 2310 TO POS
           DISPLAY "RUN ONLY LISTED BRANCH CATEGORIES : [ ]" AT POS
           ADD 37 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 20        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE5.

           IF W-ESCAPE-KEY = 4
              GO TO GET-035.
           IF WS-RANGE5 NOT = "N" AND NOT = "Y"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-036.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-040
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-036.
       GET-040.
           IF WS-RANGE4 = "N"
              PERFORM END-000
              PERFORM CHECK-DATA-NAMES
              MOVE WS-VOL-DIR TO LIST-VOL-DIR (SUB-50)
              PERFORM CHECK-BRANCH-DATA-NAMES
              PERFORM END-900.
       GET-999.
           EXIT.
      *
       DELETE-STOCK-RECORD SECTION.
       DSR-000.
            IF NEW-STOCKNO = "Y"
               GO TO DSR-999.
       DSR-005.
            IF ST-QTYONHAND NOT = 0
             OR ST-QTYONRESERVE NOT = 0
              OR ST-QTYONBORDER NOT = 0
               OR ST-QTYONORDER NOT = 0
               MOVE ST-STOCKNUMBER         TO WS-DAILY-1ST
               MOVE "NOT DELETED BY CHNGE" TO WS-DAILY-2ND
               MOVE "PROGRAM. QTY ON HAND" TO WS-DAILY-3RD
               MOVE "ETC. NOT = ZERO.    " TO WS-DAILY-4TH
               PERFORM WRITE-DAILY
               MOVE "D" TO ST-ANALYSIS
               PERFORM REWRITE-STOCK-RECORD
               GO TO DSR-999.
           IF ST-SALESUNITSYTD NOT = 0
            OR ST-SALESUNITSLAST NOT = 0
             OR ST-QTYRECYTD NOT = 0
              OR ST-QTYRECLAST NOT = 0
               OR ST-QTYADJYTD NOT = 0
                OR ST-QTYADJLAST NOT = 0
                 OR ST-SALESCOSTYTD NOT = 0
                  OR ST-SALESCOSTLAST NOT = 0
               MOVE ST-STOCKNUMBER         TO WS-DAILY-1ST
               MOVE "NOT DELETED BY CHNGE" TO WS-DAILY-2ND
               MOVE "PROGRAM. QTY SOLD OR" TO WS-DAILY-3RD
               MOVE "REC ETC. NOT = ZERO." TO WS-DAILY-4TH
               PERFORM WRITE-DAILY
               MOVE "D" TO ST-ANALYSIS
               PERFORM REWRITE-STOCK-RECORD
               GO TO DSR-999.
       DSR-010.
            DELETE STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 = 23
               GO TO DSR-999.
            IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK BUSY ON DELETE, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020
               PERFORM ERROR-020
             IF ST-STOCKNUMBER = "GONE           "
               GO TO DSR-999
             ELSE
               GO TO DSR-010. 
       DSR-999.
            EXIT. 
      *
       REWRITE-STOCK-RECORD SECTION.
       RSR-005.
           IF STCH-TYPE-OF-CHANGE = "M"
              GO TO RSR-010.
           IF NEW-STOCKNO = "Y"
              GO TO RSR-020.
       RSR-010.
           IF STCH-TYPE-OF-CHANGE NOT = "M"
              GO TO RSR-015.
           PERFORM READ-STOCK.
           ADD  WS-QTYONHAND      TO ST-QTYONHAND
           ADD  WS-QTYONRESERVE   TO ST-QTYONRESERVE
           ADD  WS-QTYONORDER     TO ST-QTYONORDER
           ADD  WS-QTYONBORDER    TO ST-QTYONBORDER
           ADD  WS-QTY-ST-TAKE    TO ST-QTY-ST-TAKE
           ADD  WS-QTYRECMTD      TO ST-QTYRECMTD
           ADD  WS-QTYRECYTD      TO ST-QTYRECYTD
           ADD  WS-QTYRECLAST     TO ST-QTYRECLAST
           ADD  WS-QTYADJMTD      TO ST-QTYADJMTD
           ADD  WS-QTYADJYTD      TO ST-QTYADJYTD
           ADD  WS-QTYADJLAST     TO ST-QTYADJLAST
           ADD  WS-SALESUNITMTD   TO ST-SALESUNITMTD
           ADD  WS-SALESUNITSYTD  TO ST-SALESUNITSYTD
           ADD  WS-SALESUNITSLAST TO ST-SALESUNITSLAST
           ADD  WS-SALESRANDSMTD  TO ST-SALESRANDSMTD
           ADD  WS-SALESRANDSYTD  TO ST-SALESRANDSYTD
           ADD  WS-SALESRANDSLAST TO ST-SALESRANDSLAST
           ADD  WS-SALESCOSTMTD   TO ST-SALESCOSTMTD
           ADD  WS-SALESCOSTYTD   TO ST-SALESCOSTYTD
           ADD  WS-SALESCOSTLAST  TO ST-SALESCOSTLAST.
       RSR-015.
          REWRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK BUSY ON RE-WRITE, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020
               PERFORM ERROR-020
               GO TO RSR-020.
          GO TO RSR-999.
       RSR-020.
          MOVE WS-DATE   TO ST-DATE-CREATED.
          IF WS-CO-NUMBER = 1
           IF STCH-CATEGORY NOT = "KEN" AND NOT = "FLK"
              MOVE "CTJHB  " TO ST-SUPPLIER.
          WRITE STOCK-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-STOCK-ST1 NOT = 0
              MOVE "STOCK BUSY ON WRITE, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STOCK-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR1-020
              PERFORM ERROR-020
              GO TO RSR-015.
       RSR-999.
           EXIT.
      *
       READ-NEXT-CHANGE SECTION.
       RNSC-000.
           PERFORM ERROR1-020.
           MOVE 2610 TO POS.
           DISPLAY "Checking for Stock-Changes........" AT POS.
           MOVE 0         TO WS-NUMBER.
           MOVE WS-RANGE1 TO STCH-STOCKNUMBER.
           START STOCKCHANGE-MASTER KEY NOT < STCH-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STCHANGE-ST1 NOT = 0
                GO TO RNSC-999.
       RNSC-005.
           READ STOCKCHANGE-MASTER NEXT WITH LOCK
                 AT END NEXT SENTENCE.
           IF WS-STCHANGE-ST1 = 10
               GO TO RNSC-999.
           IF WS-STCHANGE-ST1 NOT = 0
              MOVE 
              "STOCK-CHANGE BUSY ON READ, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020
               PERFORM ERROR-020
               GO TO RNSC-005.
           IF STCH-STOCKNUMBER < WS-RANGE1
               GO TO RNSC-005.
           IF STCH-STOCKNUMBER > WS-RANGE2
               GO TO RNSC-999.
               
           IF STCH-TYPE-OF-CHANGE = "R" OR = "M"
            IF WS-RANGE3 NOT = "D"
               GO TO RNSC-005.
               
           ADD 1 TO WS-NUMBER
           MOVE 2510 TO POS
           DISPLAY "StockChange #      Being Made to Stock Number:"
           AT POS
           ADD 14 TO POS
           MOVE WS-NUMBER TO F-EDNAMEFIELDAMOUNT
           DISPLAY F-EDNAMEFIELDAMOUNT AT POS
           MOVE 2557 TO POS
           DISPLAY STCH-STOCKNUMBER AT POS
           ADD 17 TO POS
           DISPLAY STCH-TYPE-OF-CHANGE AT POS.
               
           IF WS-RANGE5 = "Y"
            IF WS-CATEGORY NOT = STCH-CATEGORY
              MOVE STCH-CATEGORY TO WS-CATEGORY
              PERFORM READ-STBRANCH-CAT.
           IF WS-RANGE5 = "Y"
            IF WS-STBRCAT-ST1 NOT = 0
              MOVE 2610 TO POS
              DISPLAY "Stocknumber being excluded:" AT POS
              ADD 28 TO POS
              DISPLAY STCH-STOCKNUMBER AT POS
              GO TO RNSC-005.

           PERFORM ERROR-020
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
           
           
           IF WS-RANGE3 = "D"
               GO TO RNSC-020.
           MOVE STCH-STOCKNUMBER      TO ST-STOCKNUMBER.
           PERFORM READ-STOCK.
       RNSC-006.
           MOVE STCH-STOCKNUMBER      TO ST-STOCKNUMBER
           MOVE STCH-DESCRIPTION1     TO ST-DESCRIPTION1
           MOVE STCH-DESCRIPTION2     TO ST-DESCRIPTION2
           MOVE STCH-CATEGORY         TO ST-CATEGORY.
           
           IF NEW-STOCKNO = "Y"
              MOVE STCH-SUPPLIER      TO ST-SUPPLIER
              MOVE 1                  TO ST-MINBUYQTY.
              
      *    MOVE STCH-MINBUYQTY        TO ST-MINBUYQTY
              
           MOVE STCH-FOREIGNCOST      TO ST-FOREIGNCOST
           IF STCH-PRICE NOT = ST-PRICE
              MOVE ST-PRICE           TO ST-OLDPRICE
              MOVE WS-DATE            TO ST-LASTPRICECHANGE.
           MOVE STCH-PRICE            TO ST-PRICE
           MOVE STCH-MIN-PERC         TO ST-MIN-PERC
           MOVE STCH-SUPPLIERDISC     TO ST-SUPPLIERDISC
           MOVE STCH-CURRENCY         TO ST-CURRENCY
           MOVE STCH-CURRENCY-RATE    TO ST-CURRENCY-RATE
           
           MOVE STCH-UNITOFMEASURE    TO ST-UNITOFMEASURE
           MOVE STCH-DISCOUNT1        TO ST-DISCOUNT1
           MOVE STCH-DISCOUNT2        TO ST-DISCOUNT2
           MOVE STCH-DISCOUNT3        TO ST-DISCOUNT3
           MOVE STCH-DISCOUNT4        TO ST-DISCOUNT4
           MOVE STCH-DISCOUNT5        TO ST-DISCOUNT5
           MOVE STCH-DISCOUNT6        TO ST-DISCOUNT6
           MOVE STCH-DISCOUNT7        TO ST-DISCOUNT7
           MOVE STCH-DISCOUNT8        TO ST-DISCOUNT8
           MOVE STCH-DISCOUNT9        TO ST-DISCOUNT9.
           IF STCH-TYPE-OF-CHANGE = "N"
              MOVE STCH-AVERAGECOST   TO ST-AVERAGECOST
              MOVE STCH-LASTCOST      TO ST-LASTCOST.
           MOVE STCH-DEL-DELAY        TO ST-DEL-DELAY
           MOVE STCH-ANALYSIS         TO ST-ANALYSIS
           MOVE STCH-DUTYPERCENT      TO ST-DUTYPERCENT
           MOVE STCH-DUTYTARIFF       TO ST-DUTYTARIFF
           MOVE STCH-SURCHARGE        TO ST-SURCHARGE
           MOVE STCH-PERMIT           TO ST-PERMIT.
           IF STCH-TYPE-OF-CHANGE = "D"
             PERFORM DELETE-STOCK-RECORD
             GO TO RNSC-007.
           IF STCH-TYPE-OF-CHANGE = "N" OR = "C" OR = " "
             PERFORM REWRITE-STOCK-RECORD.
       RNSC-007.
          IF WS-RANGE3 = "N"
      *        CALL "C$SLEEP" USING 5
              GO TO RNSC-005.
       RNSC-020.
          DELETE STOCKCHANGE-MASTER
              INVALID KEY NEXT SENTENCE.
          IF WS-STCHANGE-ST1 NOT = 0
              MOVE 
              "STOCK-CHANGE BUSY ON READ, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020
               PERFORM ERROR-020
                GO TO RNSC-020.
          GO TO RNSC-005.
       RNSC-999.
           EXIT.
      *
       READ-STBRANCH-CAT SECTION.
       RSTCAT-005.
           MOVE WS-CATEGORY TO STBRCAT-CATEGORY.
           START STBRANCHCAT-MASTER KEY NOT < STBRCAT-KEY
              INVALID KEY NEXT SENTENCE.
      *     IF WS-STBRCAT-ST1 NOT = 0
      *        MOVE WS-STBRCAT-ST1 TO WS-MESSAGE
      *        PERFORM ERROR-MESSAGE
      *        MOVE WS-STBRCAT-ST2 TO WS-MESSAGE
      *        PERFORM ERROR-MESSAGE.
       RNREN-005.
           READ STBRANCHCAT-MASTER
                 INVALID KEY NEXT SENTENCE.
       RSTCAT-999.
           EXIT.
      *
       READ-NEXT-RENAME SECTION.
       RNREN-000.
           MOVE 2610 TO POS.
           DISPLAY "Checking for Stock-Renaming......." AT POS.
           MOVE 0         TO WS-NUMBER.
           MOVE WS-RANGE1 TO STCH-STOCKNUMBER.
           START STOCKCHANGE-MASTER KEY NOT < STCH-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-STCHANGE-ST1 NOT = 0
                GO TO RNREN-999.
       RNREN-005.
           READ STOCKCHANGE-MASTER NEXT WITH LOCK
                 AT END NEXT SENTENCE.
           IF WS-STCHANGE-ST1 = 10
               GO TO RNREN-999.
           IF WS-STCHANGE-ST1 NOT = 0
             MOVE "STOCKCHANGE BUSY READ-NEXT, IN 2 SEC GOING TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
                PERFORM ERROR-000
                CALL "C$SLEEP" USING 2
                PERFORM ERROR-020
                PERFORM ERROR1-020
                GO TO RNREN-005.
           IF STCH-STOCKNUMBER < WS-RANGE1
               GO TO RNREN-005.
           IF STCH-STOCKNUMBER > WS-RANGE2
               GO TO RNREN-999.
           
           IF STCH-TYPE-OF-CHANGE NOT = "R" AND NOT = "M"
               GO TO RNREN-005.
               
           ADD 1 TO WS-NUMBER
           MOVE 2310 TO POS
           DISPLAY "StockReName #      Being Made to Stock Number:"
           AT POS
           ADD 14 TO POS
           MOVE WS-NUMBER TO F-EDNAMEFIELDAMOUNT
           DISPLAY F-EDNAMEFIELDAMOUNT AT POS
           MOVE 2357 TO POS
           DISPLAY STCH-STOCKNUMBER AT POS
           ADD 17 TO POS
           DISPLAY STCH-TYPE-OF-CHANGE AT POS.

           IF WS-RANGE5 = "Y"
            IF WS-CATEGORY NOT = STCH-CATEGORY
              MOVE STCH-CATEGORY TO WS-CATEGORY
              PERFORM READ-STBRANCH-CAT.
           IF WS-RANGE5 = "Y"
            IF WS-STBRCAT-ST1 NOT = 0
              MOVE 2610 TO POS
              DISPLAY "Stocknumber being excluded:" AT POS
              ADD 28 TO POS
              DISPLAY STCH-STOCKNUMBER AT POS
              GO TO RNREN-005.

           PERFORM ERROR-020
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
       RNREN-006.
           MOVE STCH-STOCKNUMBER TO ST-STOCKNUMBER WS-OLDSTOCKNUMBER.
           PERFORM READ-STOCK-VALID.
           IF NEW-STOCKNO = "Y"
               MOVE "GONE"        TO WS-OLDSTOCKNUMBER.
           MOVE STCH-DESCRIPTION1 TO ST-STOCKNUMBER
                                     WS-NEWSTOCKNUMBER.
           PERFORM READ-STOCK-VALID.
      ******************************************************
      * IF NEW NUMBER ALREADY EXISTS DO THE FOLLOWING ONLY *
      ******************************************************
           IF STCH-TYPE-OF-CHANGE NOT = "M"
            IF NEW-STOCKNO = "N"
             IF WS-OLDSTOCKNUMBER NOT = "GONE"
              MOVE WS-OLDSTOCKNUMBER TO ST-STOCKNUMBER
              PERFORM READ-STOCK
              MOVE "D" TO ST-ANALYSIS
              PERFORM REWRITE-STOCK-RECORD
              GO TO RNREN-007.

      **************************************************
      * IF NEW NUMBER DOES NOT EXIST DO THE FOLLOWING. *
      **************************************************
           MOVE STCH-STOCKNUMBER TO ST-STOCKNUMBER.
           PERFORM READ-BACK-ORDERS.
           PERFORM READ-SUPPLIERS-ORDERS.
           PERFORM READ-STOCK-IMPORTS.
           PERFORM READ-STOCK-RECEIPTS.
           PERFORM READ-STOCK-RECEIPTSLY.
           PERFORM READ-TOOLKIT-HEADER.
           IF WS-TOOLKIT-INVALID = "N"
               PERFORM WRITE-NEW-KIT
               PERFORM DELETE-OLD-KIT.
           PERFORM READ-COMPONENT-OF-KIT.
           
           MOVE WS-OLDSTOCKNUMBER TO ST-STOCKNUMBER.
           PERFORM READ-STOCK.
           IF STCH-TYPE-OF-CHANGE = "M"
              MOVE ST-QTYONHAND      TO WS-QTYONHAND
              MOVE ST-QTYONRESERVE   TO WS-QTYONRESERVE
              MOVE ST-QTYONORDER     TO WS-QTYONORDER
              MOVE ST-QTYONBORDER    TO WS-QTYONBORDER
              MOVE ST-QTY-ST-TAKE    TO WS-QTY-ST-TAKE
              MOVE ST-QTYRECMTD      TO WS-QTYRECMTD
              MOVE ST-QTYRECYTD      TO WS-QTYRECYTD
              MOVE ST-QTYRECLAST     TO WS-QTYRECLAST
              MOVE ST-QTYADJMTD      TO WS-QTYADJMTD
              MOVE ST-QTYADJYTD      TO WS-QTYADJYTD
              MOVE ST-QTYADJLAST     TO WS-QTYADJLAST
              MOVE ST-SALESUNITMTD   TO WS-SALESUNITMTD
              MOVE ST-SALESUNITSYTD  TO WS-SALESUNITSYTD
              MOVE ST-SALESUNITSLAST TO WS-SALESUNITSLAST
              MOVE ST-SALESRANDSMTD  TO WS-SALESRANDSMTD
              MOVE ST-SALESRANDSYTD  TO WS-SALESRANDSYTD
              MOVE ST-SALESRANDSLAST TO WS-SALESRANDSLAST
              MOVE ST-SALESCOSTMTD   TO WS-SALESCOSTMTD
              MOVE ST-SALESCOSTYTD   TO WS-SALESCOSTYTD
              MOVE ST-SALESCOSTLAST  TO WS-SALESCOSTLAST.
           PERFORM DSR-010 THRU DSR-999.
           MOVE WS-NEWSTOCKNUMBER TO ST-STOCKNUMBER.
           MOVE "Y"               TO NEW-STOCKNO.
           PERFORM REWRITE-STOCK-RECORD.
       RNREN-007.
           PERFORM ERROR1-020.
           IF WS-RANGE3 = "N"
              GO TO RNREN-005.
       RNREN-020.
           DELETE STOCKCHANGE-MASTER
              INVALID KEY NEXT SENTENCE.
           IF WS-STCHANGE-ST1 NOT = 0
              MOVE 
              "STOCK-CHANGE BUSY ON DELETE, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR-020
              MOVE STCH-STOCKNUMBER TO WS-MESSAGE
              CALL "C$SLEEP" USING 2
              PERFORM ERROR-000
              PERFORM ERROR1-020
              GO TO RNREN-020.
           GO TO RNREN-005.
       RNREN-999.
           EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
             START STOCK-MASTER KEY NOT < ST-KEY
                INVALID KEY NEXT SENTENCE.
       R-ST-010.
             READ STOCK-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                PERFORM FILL-NEW-RECORD-WITH-BLANKS
      *          MOVE SPACES TO STOCK-RECORD
                MOVE "Y" TO NEW-STOCKNO
                GO TO R-ST-999.
             IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON READ-NEXT, IN 2 SECS GOING TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK-ST1 TO WS-MESSAGE
                PERFORM ERROR-000
                CALL "C$SLEEP" USING 2
                PERFORM ERROR-000
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK-ST1
                GO TO R-ST-010.
             MOVE "N" TO NEW-STOCKNO.
       R-ST-999.
             EXIT.
      *
       FILL-NEW-RECORD-WITH-BLANKS SECTION.
       FNRWB-005.
            MOVE SPACES TO  ST-DESCRIPTION1
                            ST-DESCRIPTION2
                            ST-CATEGORY
                            ST-SUPPLIER
                            ST-CURRENCY
                            ST-UNITOFMEASURE
                            ST-BINLOCATION
                            ST-ANALYSIS
                            ST-PERMIT.
                              
            MOVE 0       TO ST-FOREIGNCOST
                            ST-SUPPLIERDISC
                            ST-CURRENCY-RATE
                            ST-PRICE
                            ST-OLDPRICE
                            ST-MIN-PERC
                            ST-DISCOUNT1
                            ST-DISCOUNT2
                            ST-DISCOUNT3
                            ST-DISCOUNT4
                            ST-DISCOUNT5
                            ST-DISCOUNT6
                            ST-DISCOUNT7
                            ST-DISCOUNT8
                            ST-DISCOUNT9
                            ST-AVERAGECOST
                            ST-LASTCOST
                            ST-MAXIMUMLEVEL
                            ST-MINIMUMLEVEL
                            ST-QTYONHAND
                            ST-QTYONRESERVE
                            ST-QTYONORDER
                            ST-QTYONBORDER
                            ST-QTY-ST-TAKE
                            ST-DATE-CREATED
                            ST-LASTPRICECHANGE
                            ST-LASTSALEDATE
                            ST-LASTRECEIPTDATE
                            ST-LASTORDERDATE
                            ST-QTYRECMTD
                            ST-QTYRECYTD
                            ST-QTYRECLAST
                            ST-QTYADJMTD
                            ST-QTYADJYTD
                            ST-QTYADJLAST
                            ST-SALESUNITMTD
                            ST-SALESUNITSYTD
                            ST-SALESUNITSLAST
                            ST-SALESRANDSMTD
                            ST-SALESRANDSYTD
                            ST-SALESRANDSLAST
                            ST-SALESCOSTMTD
                            ST-SALESCOSTYTD
                            ST-SALESCOSTLAST
                            ST-DUTYPERCENT
                            ST-DUTYTARIFF
                            ST-SURCHARGE.
              MOVE 1     TO ST-MINBUYQTY
                            ST-DEL-DELAY.
       FNRWB-999.
             EXIT.
      *
       READ-STOCK-VALID SECTION.
       RSTV-000.
             START STOCK-MASTER KEY NOT < ST-KEY
                INVALID KEY NEXT SENTENCE.
       RSTV-010.
             READ STOCK-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
      *          MOVE SPACES TO STOCK-RECORD
                PERFORM FILL-NEW-RECORD-WITH-BLANKS
                MOVE "Y" TO NEW-STOCKNO
                GO TO RSTV-999.
             IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK BUSY ON READ, IN 2 SECS GOING TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK-ST1 TO WS-MESSAGE
                PERFORM ERROR-000
                CALL "C$SLEEP" USING 2
                PERFORM ERROR-000
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK-ST1
                GO TO RSTV-010.
             MOVE "N" TO NEW-STOCKNO.
       RSTV-999.
             EXIT.
      *
       READ-BACK-ORDERS SECTION.
       RBO-000.
            MOVE 2910 TO POS.
            DISPLAY "Changing Back-Orders to New NUMBER" AT POS.
            MOVE WS-OLDSTOCKNUMBER TO STTR-STOCK-NUMBER.
            START STOCK-TRANS-FILE KEY NOT < STTR-STOCK-NUMBER
                 INVALID KEY NEXT SENTENCE.
            IF WS-STTRANS-ST1 NOT = 0
               GO TO RBO-999.
       RBO-002.
            READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-STTRANS-ST1 = 10
               GO TO RBO-999.
            IF WS-STTRANS-ST1 NOT = 0
              MOVE "STTRANS BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STTRANS-ST1
               GO TO RBO-002.
            IF STTR-STOCK-NUMBER NOT = WS-OLDSTOCKNUMBER
               GO TO RBO-999.
       RBO-005.        
            MOVE WS-NEWSTOCKNUMBER TO STTR-STOCK-NUMBER.
            REWRITE STOCK-TRANS-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-STTRANS-ST1 NOT = 0
               MOVE STTR-KEY               TO WS-DAILY-1ST
               MOVE STTR-STOCK-NUMBER      TO WS-DAILY-2ND
               MOVE "NO CHANGE TO NEW NO " TO WS-DAILY-3RD
               MOVE WS-NEWSTOCKNUMBER      TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RBO-002.
       RBO-999.
           EXIT.
      *
       READ-SUPPLIERS-ORDERS SECTION.
       RSQ-000.
            MOVE 2910 TO POS.
            DISPLAY "Changing Suppliers-Orders to new NUMBER" AT POS.
            MOVE WS-OLDSTOCKNUMBER TO OO-STOCK-NUMBER.
            START OUTSTANDING-ORDERS KEY NOT < OO-STOCK-NUMBER
                 INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 NOT = 0
               GO TO RSQ-999.
       RSQ-002.
            READ OUTSTANDING-ORDERS NEXT
               AT END NEXT SENTENCE.
            IF WS-OUTORD-ST1 = 10
               GO TO RSQ-999.
            IF WS-OUTORD-ST1 NOT = 0
              MOVE "ORDERS BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020
               PERFORM ERROR-020
               GO TO RSQ-002.
            IF OO-STOCK-NUMBER NOT = WS-OLDSTOCKNUMBER
               GO TO RSQ-999.
       RSQ-005.
            DELETE OUTSTANDING-ORDERS
                INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE OO-KEY                 TO WS-DAILY-1ST
               MOVE OO-STOCK-NUMBER        TO WS-DAILY-2ND
               MOVE "DELETE OF SUPP/ORDER" TO WS-DAILY-3RD
               MOVE WS-NEWSTOCKNUMBER      TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
       RSQ-010.
            MOVE WS-NEWSTOCKNUMBER TO OO-STOCK-NUMBER.
            WRITE OUT-ORDER-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE OO-KEY                 TO WS-DAILY-1ST
               MOVE OO-STOCK-NUMBER        TO WS-DAILY-2ND
               MOVE "NO CHANGE TO NEW NO " TO WS-DAILY-3RD
               MOVE WS-NEWSTOCKNUMBER      TO WS-DAILY-4TH
               PERFORM WRITE-DAILY.
            GO TO RSQ-002.
       RSQ-999.
           EXIT.
      *
       READ-STOCK-IMPORTS SECTION.
       RSI-000.
            MOVE 2910 TO POS.
            DISPLAY "Changing Stock Imports File........       " AT POS.
            MOVE WS-OLDSTOCKNUMBER TO IMRE-STOCK-NUMBER.
            START IMPRECEIPTS-FILE KEY NOT < IMRE-STOCK-NUMBER
                 INVALID KEY NEXT SENTENCE.
           IF WS-IMPRECEIPT-ST1 NOT = 0
               GO TO RSI-999.
       RSI-030.
           READ IMPRECEIPTS-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-IMPRECEIPT-ST1 = 10
              GO TO RSI-999.
           IF WS-IMPRECEIPT-ST1 NOT = 0
              MOVE "IMPORTS BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-IMPRECEIPT-ST1
              GO TO RSI-030.
           IF IMRE-STOCK-NUMBER NOT = WS-OLDSTOCKNUMBER
              GO TO RSI-999.
       RSI-040.
           MOVE WS-NEWSTOCKNUMBER TO IMRE-STOCK-NUMBER.
       RSI-050.
           REWRITE IMPORT-RECEIPTS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-IMPRECEIPT-ST1 = 23 OR 35 OR 49
              MOVE "IMPORTS STOCK NUMBER" TO WS-DAILY-1ST
              MOVE "NOT CHANGED TO NEW :" TO WS-DAILY-2ND
              MOVE IMRE-STOCK-NUMBER      TO WS-DAILY-3RD
              MOVE " "                    TO WS-DAILY-4TH
              PERFORM WRITE-DAILY
              GO TO RSI-030.
           IF WS-IMPRECEIPT-ST1 NOT = 0
              MOVE "IMPORTS BUSY ON RE-WRITE, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-IMPRECEIPT-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-IMPRECEIPT-ST1
              GO TO RSI-050.
           GO TO RSI-030.
       RSI-999.
           EXIT.
      *
       READ-STOCK-RECEIPTS SECTION.
       RSTK-REC-000.
            PERFORM ERROR1-020.
            MOVE 2910 TO POS.
            DISPLAY "Changing Stock Receipts File........     " AT POS.
            MOVE WS-OLDSTOCKNUMBER TO STRE-STOCK-NUMBER.
            START STKRECEIPTS-FILE KEY NOT < STRE-STOCK-NUMBER
                 INVALID KEY NEXT SENTENCE.
           IF WS-STKRECEIPT-ST1 NOT = 0
               GO TO RSTK-REC-999.
       RSTK-REC-030.
           READ STKRECEIPTS-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-STKRECEIPT-ST1 = 10
              GO TO RSTK-REC-999.
           IF WS-STKRECEIPT-ST1 NOT = 0
              MOVE 
              "STRECEIPT BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPT-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STKRECEIPT-ST1
              GO TO RSTK-REC-030.
           IF STRE-STOCK-NUMBER NOT = WS-OLDSTOCKNUMBER
              GO TO RSTK-REC-999.
       RSTK-REC-040.
           MOVE WS-NEWSTOCKNUMBER TO STRE-STOCK-NUMBER.
       RSTK-REC-050.
           REWRITE STOCK-RECEIPTS-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-STKRECEIPT-ST1 = 23 OR 35 OR 49
              MOVE "ST-RECP STOCK NUMBER" TO WS-DAILY-1ST
              MOVE "NOT CHANGED TO NEW :" TO WS-DAILY-2ND
              MOVE STRE-STOCK-NUMBER      TO WS-DAILY-3RD
              MOVE " "                    TO WS-DAILY-4TH
              PERFORM WRITE-DAILY
              GO TO RSTK-REC-030.
           IF WS-STKRECEIPT-ST1 NOT = 0
             MOVE "STRECEIPT BUSY ON RE-WRITE, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STKRECEIPT-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-STKRECEIPT-ST1
              GO TO RSTK-REC-050.
           GO TO RSTK-REC-030.
       RSTK-REC-999.
           EXIT.
      *
       READ-STOCK-RECEIPTSLY SECTION.
       RSTK-RECLY-000.
            PERFORM ERROR1-020.
            MOVE 2910 TO POS.
            DISPLAY "Changing Stock ReceiptsLY File........   " AT POS.
            MOVE WS-OLDSTOCKNUMBER TO STRELY-STOCK-NUMBER.
            START STKRECEIPTSLY-FILE KEY NOT < STRELY-STOCK-NUMBER
                 INVALID KEY NEXT SENTENCE.
           IF WS-STKRECEIPTSLY-ST1 NOT = 0
               GO TO RSTK-RECLY-999.
       RSTK-RECLY-030.
           READ STKRECEIPTSLY-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-STKRECEIPTSLY-ST1 = 10
              GO TO RSTK-RECLY-999.
           IF WS-STKRECEIPTSLY-ST1 NOT = 0
              MOVE 
              "STRECEIPTLY BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPTSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STKRECEIPTSLY-ST1
              GO TO RSTK-RECLY-030.
           IF STRELY-STOCK-NUMBER NOT = WS-OLDSTOCKNUMBER
              GO TO RSTK-RECLY-999.
       RSTK-RECLY-040.
           MOVE WS-NEWSTOCKNUMBER TO STRELY-STOCK-NUMBER.
       RSTK-RECLY-050.
           REWRITE STOCK-RECEIPTSLY-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-STKRECEIPTSLY-ST1 = 23 OR 35 OR 49
              MOVE "ST-RECP STOCK NUMBER" TO WS-DAILY-1ST
              MOVE "NOT CHANGED TO NEW :" TO WS-DAILY-2ND
              MOVE STRELY-STOCK-NUMBER    TO WS-DAILY-3RD
              MOVE " "                    TO WS-DAILY-4TH
              PERFORM WRITE-DAILY
              GO TO RSTK-RECLY-030.
           IF WS-STKRECEIPTSLY-ST1 NOT = 0
              MOVE 
              "STRECEIPTLY BUSY ON RE-WRITE, IN 2 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STKRECEIPTSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 2
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STKRECEIPTSLY-ST1
              GO TO RSTK-RECLY-050.
           GO TO RSTK-RECLY-030.
       RSTK-RECLY-999.
           EXIT.
      *
       START-KIT SECTION.
       SK-010.
           MOVE WS-OLDSTOCKNUMBER TO TO-TOOLKIT-NUMBER.
           MOVE " "               TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT LESS TO-KEY
                INVALID KEY NEXT SENTENCE.
       SK-999.
           EXIT.
      *
       READ-NEXT-KIT SECTION.
       RNK-000.
           READ TOOLKITS NEXT
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               GO TO RNK-999.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE "TOOLKIT BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR1-020
              PERFORM ERROR-020
              GO TO RNK-000.
       RNK-999.
           EXIT.
      *
       READ-TOOLKIT-HEADER SECTION.
       RTH-000.
           MOVE 2910 TO POS.
           DISPLAY "WRITING NEW BILL-OF-MATERIAL......" AT POS.
           MOVE WS-OLDSTOCKNUMBER TO TO-TOOLKIT-NUMBER.
           MOVE " "               TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-KEY
                INVALID KEY NEXT SENTENCE.
       RTH-010.
           READ TOOLKITS
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               MOVE "Y" TO WS-TOOLKIT-INVALID
               GO TO RTH-999.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE 
              "TOOLKIT HEADER BUSY ON READ, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR1-020
              PERFORM ERROR-020
              GO TO RTH-000.
           MOVE "N" TO WS-TOOLKIT-INVALID.
       RTH-999.
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
           IF TO-TOOLKIT-NUMBER NOT = WS-OLDSTOCKNUMBER
              GO TO WNK-999.
           MOVE WS-NEWSTOCKNUMBER TO TO-TOOLKIT-NUMBER.
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
              MOVE "TOOLKIT ST1=2 ON WRITE, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR-020
              MOVE TO-COMPONENT-NUMBER TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR1-020
              PERFORM ERROR-020
              GO TO WRT-999.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE "TOOLKITS BUSY ON WRITE, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR-020
              MOVE TO-COMPONENT-NUMBER TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR1-020
              PERFORM ERROR-020
              GO TO WRT-010.
       WRT-999.
           EXIT.
      *
       DELETE-OLD-KIT SECTION.
       DT-010.
           MOVE WS-OLDSTOCKNUMBER TO TO-TOOLKIT-NUMBER.
           MOVE " "               TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE
               "TOOLKITS BUSY ON START-DELETE, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR-020
              MOVE TO-COMPONENT-NUMBER TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR1-020
              PERFORM ERROR-020
              GO TO DT-010.
       DT-020.
           READ TOOLKITS NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               GO TO DT-999.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE 
              "TOOLKITS BUSY ON READ-DELETE, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR-020
              MOVE TO-COMPONENT-NUMBER TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR1-020
              PERFORM ERROR-020
              GO TO DT-020.
           IF TO-TOOLKIT-NUMBER = WS-OLDSTOCKNUMBER
               GO TO DT-050.
           GO TO DT-999.
       DT-050.
           DELETE TOOLKITS
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               GO TO DT-020.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE "TOOLKITS BUSY ON DELETE, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR-020
              MOVE TO-COMPONENT-NUMBER TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR1-020
              PERFORM ERROR-020
              GO TO DT-050.
           GO TO DT-020.
       DT-999.
           EXIT.
      *
       READ-COMPONENT-OF-KIT SECTION.
       RCOK-000.
           CLOSE TOOLKITS.
           PERFORM OPEN-006.
           MOVE 2910 TO POS.
           DISPLAY "CHANGING ITEM IN BILL-OF-MATERIAL......" AT POS.
           MOVE " " TO TO-TOOLKIT-NUMBER
                       TO-COMPONENT-NUMBER.
       RCOK-005.
           MOVE WS-OLDSTOCKNUMBER TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-COMPONENT-NUMBER
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 NOT = 0
               GO TO RCOK-900.
       RCOK-010.
           READ TOOLKITS NEXT WITH LOCK
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               GO TO RCOK-900.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE 
              "TOOLKIT COMP BUSY READ-NEXT, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR-020
              MOVE TOOL-REC TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR1-020
              PERFORM ERROR-020
              GO TO RCOK-900.
           IF TO-COMPONENT-NUMBER NOT = WS-OLDSTOCKNUMBER
               GO TO RCOK-900.
       RCOK-015.
           IF TO-TOOLKIT-NUMBER NOT = WS-TOOLKITNUMBER
               MOVE TO-TOOLKIT-NUMBER TO WS-TOOLKITNUMBER
               MOVE 2610 TO POS
               DISPLAY "TOOLKIT CURRENTLY BEING CHANGED =" AT POS
               MOVE 2645 TO POS
               DISPLAY WS-TOOLKITNUMBER AT POS.
       RCOK-020.
           IF TO-COMPONENT-NUMBER = WS-OLDSTOCKNUMBER
              MOVE TO-TOOLKIT-NUMBER   TO WS-TOOLKITNUMBER
              MOVE TO-QUANTITY         TO WS-QTY
              PERFORM DELETE-OLD-COMPONENT
              PERFORM WRITE-NEW-COMPONENT.
              CLOSE TOOLKITS.
              PERFORM OPEN-006.
           GO TO RCOK-005.
       RCOK-900.
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
       RCOK-999.
           EXIT.
      *
       WRITE-NEW-COMPONENT SECTION.
       WNC-000.
           MOVE WS-TOOLKITNUMBER  TO TO-TOOLKIT-NUMBER.
           MOVE WS-NEWSTOCKNUMBER TO TO-COMPONENT-NUMBER.
           MOVE WS-QTY            TO TO-QUANTITY.
           START TOOLKITS KEY NOT < TO-KEY
               INVALID KEY NEXT SENTENCE.
       WNC-002.
           READ TOOLKITS WITH LOCK
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               GO TO WNC-010.
           ADD WS-QTY TO TO-QUANTITY.
       WNC-005.
           REWRITE TOOL-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              GO TO WNC-010.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE "TOOLKIT NOT RE-WRITTEN, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR-020
              MOVE TO-COMPONENT-NUMBER TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR1-020
              PERFORM ERROR-020
              GO TO WNC-005.
           GO TO WNC-999.
       WNC-010.
           WRITE TOOL-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              GO TO WNC-005.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE "TOOLKIT S NOT WRITTEN, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR-020
              MOVE TO-COMPONENT-NUMBER TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR1-020
              PERFORM ERROR-020
              GO TO WNC-010.
       WNC-999.
           EXIT.
      *
       DELETE-OLD-COMPONENT SECTION.
       DOC-050.
           DELETE TOOLKITS
               INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
               GO TO DOC-999.
           IF WS-TOOLKIT-ST1 NOT = 0
              MOVE 
              "TOOLKIT COMP BUSY ON DELETE, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR-020
              MOVE TOOL-REC TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR1-020
              PERFORM ERROR-020.
      *         GO TO DOC-050.
       DOC-999.
           EXIT.
      *
       READ-ALL-CHANGES SECTION.
       RSN-005. 
           READ STOCKCHANGE-MASTER NEXT
             AT END 
               GO TO RSN-900.
           IF WS-STCHANGE-ST1 = 23 OR 35 OR 49
              MOVE 
              "STOCK CHANGE ST1=2 ON READ-NEXT, IN 2 SEC GOING TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR-020
              PERFORM ERROR-020
              GO TO RSN-999.
           IF WS-STCHANGE-ST1 NOT = 0
              MOVE 
              "STOCK CHANGE BUSY ON READ-NEXT, IN 2 SEC GOING TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STCHANGE-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 2
              PERFORM ERROR-020
              PERFORM ERROR-020
              GO TO RSN-005.
           IF STCH-STOCKNUMBER < WS-RANGE1
               GO TO RSN-005.
           IF STCH-STOCKNUMBER > WS-RANGE2
               GO TO RSN-900.
           ADD 1 TO WS-NUMBER.
           GO TO RSN-005.
       RSN-900.
           MOVE 2910 TO POS
           DISPLAY "                                              "
              AT POS.
           MOVE 1510 TO POS
           DISPLAY "There are      StockChanges to make.          "
              AT POS.
           ADD 10 TO POS
           MOVE WS-NUMBER TO F-EDNAMEFIELDAMOUNT
           DISPLAY F-EDNAMEFIELDAMOUNT AT POS.
       RSN-999.
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
               
      *      MOVE WS-STOCK TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       OPEN-001.
            OPEN I-O STKRECEIPTS-FILE.
            IF WS-STKRECEIPT-ST1 NOT = 0
               MOVE "STOCK RECEIPTS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-001.
      *         MOVE WS-streceipt TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE.
       OPEN-002.
            OPEN I-O STOCKCHANGE-MASTER.
            IF WS-STCHANGE-ST1 NOT = 0
               MOVE 0 TO WS-STCHANGE-ST1
               MOVE "STCHANGE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
      *      MOVE WS-STCHANGE TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       OPEN-003.
            OPEN I-O IMPRECEIPTS-FILE.
            IF WS-IMPRECEIPT-ST1 NOT = 0
               MOVE 0 TO WS-IMPRECEIPT-ST1
               MOVE "IMPORTS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-003.
      *      MOVE WS-STIMPORTS TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       OPEN-004.
            OPEN I-O STKRECEIPTSLY-FILE.
            IF WS-STKRECEIPTSLY-ST1 NOT = 0
               MOVE "STOCK RECEIPTSLY BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-004.
      *         MOVE WS-STRECEIPTSLY TO WS-MESSAGE
      *         PERFORM ERROR-MESSAGE.
       OPEN-005.
            OPEN I-O STOCK-TRANS-FILE.
            IF WS-STTRANS-ST1 NOT = 0
               MOVE 0 TO WS-STTRANS-ST1
               MOVE "STTRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
      *      MOVE WS-STTRANS TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       OPEN-006.
            OPEN I-O TOOLKITS.
            IF WS-TOOLKIT-ST1 NOT = 0
               MOVE 0 TO WS-TOOLKIT-ST1
               MOVE "TOOLFILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-006.
      *      MOVE WS-TOOLKIT TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       OPEN-008.
            OPEN I-O OUTSTANDING-ORDERS.
            IF WS-OUTORD-ST1 NOT = 0
               MOVE 0 TO WS-OUTORD-ST1
               MOVE "SORDER-FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-008.
      *      MOVE WS-STORDERS TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       OPEN-009.
            OPEN I-O STBRANCHCAT-MASTER.
            IF WS-STBRCAT-ST1 NOT = 0
               MOVE 0 TO WS-STBRCAT-ST1
               MOVE "ST-BRANCH-CAT BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-009.
      *      MOVE WS-STBRANCHCAT TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-MASTER
                  IMPRECEIPTS-FILE
                  STKRECEIPTS-FILE
                  STKRECEIPTSLY-FILE
                  STOCK-TRANS-FILE
                  OUTSTANDING-ORDERS
                  STOCKCHANGE-MASTER
                  TOOLKITS.
       END-900.
            EXIT PROGRAM.
       END-999.
           EXIT.
      *      
       Copy "GetSystemY2KDate".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
       Copy "WriteDailyExcep1".
      *
      * END-OF-JOB
