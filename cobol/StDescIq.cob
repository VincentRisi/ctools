        IDENTIFICATION DIVISION.
        PROGRAM-ID. StDescIq.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStMaster1".
          Copy "SelectStMaster2".
          Copy "SelectStMaster3".
          Copy "SelectStMaster4".
          Copy "SelectStSpecPr".
          Copy "SelectSlParameter".
          Copy "SelectCoDataName".
          
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
           COPY ChlfdStockBr1.
           COPY ChlfdStockBr2.
           COPY ChlfdStockBr3.
           COPY ChlfdStockBr4.
           COPY ChlfdStPrice.
           Copy ChlfdDataName.
           COPY ChlfdParam.
           COPY ChlfdCompany.

       WORKING-STORAGE SECTION.
       77  WS-SHORTDESC         PIC X(10) VALUE " ".
       77  WS-SHORTDESC2        PIC X(2) VALUE " ".
       77  WS-WORK              PIC X(20) VALUE " ".
       77  WS-1ST               PIC X VALUE " ".
       77  WS-ONHAND-ONLY       PIC X VALUE " ".
       77  WS-SUB1-DIS          PIC 9(4) VALUE 0.
       77  WS-QTY               PIC Z(4)9.
       77  WS-PRICE             PIC Z(5)9.99 BLANK WHEN ZERO.
       77  WS-SP-PRICE          PIC Z(5)9.99 BLANK WHEN ZERO.
       77  WS-MIDDLE            PIC X(79) VALUE " ".
       77  Ws-cbStock           PIC 9(3) VALUE 11.
       77  WS-CATEGORY          PIC X(3) VALUE " ".      
       77  WS-NUMBER            PIC 9(4) VALUE 0.
       77  WS-COMPANY-UPDATE    PIC 9(2) VALUE 0.
       77  WS-END               PIC X VALUE " ".
       77  WS-NO-OF-READS       PIC 9(2) VALUE 0.
       77  WS-STOCKNUMBER       PIC X(15) VALUE " ".
       77  WS-BARE-STOCK        PIC X(20) VALUE " ".
       77  WS-PERCENT           PIC S9(3)V99 VALUE 0.
       77  WS-QUES-MU-GP-PERC   PIC X VALUE " ".
       77  WS-PasswordSaved     Pic X(10).
       77  PSW-SUB1             PIC S9(5) VALUE 0.
       77  PSW-SUB2             PIC S9(5) VALUE 0.
       01  W-READ-KEY           PIC X.
       01  W-CRTSTATUS          PIC 9(4) VALUE 0.
       01  WS-PASSWORD-KEY.
           03  WS-PA-KEY        PIC X OCCURS 11.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-STOCK1-STATUS.
           03  WS-STOCK1-ST1    PIC 99.
       01  WS-STOCK2-STATUS.
           03  WS-STOCK2-ST1    PIC 99.
       01  WS-STOCK3-STATUS.    
           03  WS-STOCK3-ST1    PIC 99.
       01  WS-STOCK4-STATUS.
           03  WS-STOCK4-ST1    PIC 99.
       01  WS-MENU-STATUS.
           03  WS-MENU-ST1      PIC 99.
       01  WS-STPR-STATUS.
           03  WS-STPR-ST1      PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  WS-DATA-STATUS.
           03  WS-DATA-ST1        PIC 99.
       01  WS-SPLIT-DESC.
           03  WS-SP-1          PIC X(5) VALUE " ".
           03  WS-SP-REST       PIC X(15) VALUE " ".
       01  WS-SPLIT-INPUT-DESC.
           03  WS-SP-I-1        PIC X(5) VALUE " ".
           03  WS-SP-I-REST     PIC X(15) VALUE " ".
       01 COMPANIES-NAME-LIST.
         02  COMPANIES-LIST OCCURS 20.
           03  LIST-NAME        PIC X(40).
           03  LIST-NUMBER      PIC 99.
           03  LIST-VOL-DIR     PIC X(25).
       01  WS-BRANCH-LINE-DESC.
           03  WS-LINE-DESC1    PIC X(58) VALUE 
           "Stock Number   Short Description         Price  Special".
           03  WS-LINE-OWN      PIC X(3) VALUE "OWN".
           03  FILLER           PIC X(3) VALUE " ".
           03  WS-LINE-BR1      PIC X(3) VALUE " ".
           03  FILLER           PIC X(3) VALUE " ".
           03  WS-LINE-BR2      PIC X(3) VALUE " ".
           03  FILLER           PIC X(3) VALUE " ".
           03  WS-LINE-BR3      PIC X(3) VALUE " ".
           03  FILLER           PIC X(3) VALUE " ".
           03  WS-LINE-BR4      PIC X(3) VALUE " ".
       01  BRANCH-NAME-INFO.
         02  WS-BRANCH-INFO OCCURS 10.
           03  WS-BRANCH-TYPE          PIC 9.
           03  WS-BRANCH-NUMBER        PIC 9.
           03  WS-BRANCH-NOT-THERE     PIC X(2).
           03  WS-BRANCH-NAME          PIC X(3).
           03  WS-BRANCH-STOCK-VOL-DIR PIC X(40).
           03  WS-BRANCH-STOCK         PIC X(15).
           03  WS-BRANCH-ONHAND        PIC 9(6).
           03  WS-BRANCH-ONRES         PIC 9(6).
           03  WS-BRANCH-ONORDER       PIC 9(6).
           03  WS-BRANCH-ONBO          PIC 9(6).
       01  MERGE-STOCK-NAMES.
         02  WS-MERGE-STOCK OCCURS 10.
           03  WS-QTYONHAND          PIC 9(6).
           03  WS-QTYONRESERVE       PIC 9(6).
           03  WS-QTYONORDER         PIC 9(6).
           03  WS-QTYONBORDER        PIC 9(6).
           03  WS-QTY-ST-TAKE        PIC 9(6).
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
           Move 3010 to Pos
           Display "Reading Next-Company........" At Pos.
           Perform Read-Next-Company.

           Move 3010 to Pos
           Display "Getting Branch Stock file Names.. " at Pos.
           Perform Check-Branch-Data-Names
           Perform Error-020.
           
           Move 3020 to Pos
           Display "Opening Branch Files.............." At Pos.
           Perform Open-Branch-Stock. 

           PERFORM ERROR-020.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.

           PERFORM ERROR-020.
      * <F10> TO READ ALL BRANCH STOCK FILES BY ST-DESCRIPTION
           IF F-EXIT-CH = X"1F"
               MOVE "D" TO W-READ-KEY
               PERFORM READ-BRANCH-DISPLAY
               GO TO CONTROL-010.
      * <ALT-F10> TO READ ALL BRANCH STOCK FILES BY ST-STOCKNUMBER
           IF F-EXIT-CH = X"9F"
               MOVE "N" TO W-READ-KEY
               PERFORM READ-BRANCH-DISPLAY
               GO TO CONTROL-010.
           
      * <f5> & <Code-F5>
           IF F-EXIT-CH = X"19" OR = X"99"
               PERFORM READ-SPECIAL-DISPLAY
           ELSE
               PERFORM READ-MASTER-DISPLAY.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE " " TO WS-END.
           MOVE 0   TO WS-NO-OF-READS.
           MOVE WS-LASTPASSWORD TO WS-PASSWORDSAVED.
            
           MOVE " " TO WS-SPLIT-INPUT-DESC
                       WS-SPLIT-DESC.
           MOVE "Y" TO WS-1ST.
           
           MOVE 1505 TO POS
           DISPLAY 
          "Hold The <ALT> Key While Pressing <F5> OR <F8> OR <Return>"
             AT POS
           MOVE 1605 TO POS
           DISPLAY
          "To Get The System To Display ONLY Items With On-Hand > Zero."
             AT POS.

           MOVE 1805 TO POS
           DISPLAY 
          "Press <F10> To view ALL Branch Stock By Description."
             AT POS.
           MOVE 1905 TO POS
           DISPLAY 
          "Press <ALT-F10> To view ALL Branch Stock By Stock Number."
             AT POS.

       GET-010.
           MOVE "SHORTDESC" TO F-FIELDNAME.
           MOVE 9           TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
           PERFORM ERROR-020.
           MOVE 10          TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO WS-SHORTDESC.

           MOVE SPACES TO WS-MESSAGE
           MOVE 1505 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 1605 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 1805 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 1905 TO POS
           DISPLAY WS-MESSAGE AT POS.
      * <CODE-F5> & <CODE-RETURN> & <CODE-F8>  &  <F10> & <CODE-F10>
      *     X"99" OR = X"8A"      OR = X"9D" OR = X"1F"  OR = X"9F"
      * <CODE-F5> & <CODE-RETURN> & <CODE-F8>
      * <CODE-RETURN> CHANGED TO <GO> = X"1B"
      * 
           IF F-EXIT-CH = X"1F" OR = X"9F"
              MOVE "N" TO WS-ONHAND-ONLY
              MOVE F-EXIT-CH TO F-EXIT-CH-SAVE
              GO TO GET-020.
      
           IF F-EXIT-CH = X"99" OR = X"1B" OR = X"9D"
              MOVE "Y" TO WS-ONHAND-ONLY
           ELSE
              MOVE "N" TO WS-ONHAND-ONLY.
           
           MOVE F-EXIT-CH TO F-EXIT-CH-SAVE.
       GET-020.
           MOVE "CAT" TO F-FIELDNAME.
           MOVE 3     TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
           IF F-EXIT-CH = X"01"
               GO TO GET-010.
           PERFORM ERROR-020.
           MOVE 3    TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO WS-CATEGORY.

           MOVE SPACES TO WS-MESSAGE
           MOVE 1505 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 1605 TO POS
           DISPLAY WS-MESSAGE AT POS.
           MOVE 1805 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 1905 TO POS
           DISPLAY WS-MESSAGE AT POS.
           
           MOVE F-EXIT-CH-SAVE TO F-EXIT-CH.
       GET-999.
            EXIT.
      *
       READ-MASTER-DISPLAY SECTION.
       READ-005.
            IF F-EXIT-CH = X"0A" OR = X"1B"
               MOVE WS-SHORTDESC TO ST-DESCRIPTION1
                                    WS-SPLIT-INPUT-DESC
            START STOCK-MASTER KEY NOT < ST-ALT-KEY
               INVALID KEY NEXT SENTENCE.
            IF F-EXIT-CH = X"1D" OR = X"9D"
               MOVE WS-SHORTDESC TO ST-STOCKNUMBER
                                    WS-SPLIT-INPUT-DESC
            START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
            MOVE 0 TO F-EXIT-CH.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BAD START, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO READ-999.
            MOVE 0 TO SUB-2 SUB-3.
            MOVE 800 TO WS-SUB1-DIS.
       READ-010.
            IF F-EXIT-CH = 0
              READ STOCK-MASTER NEXT
                AT END
                CLOSE STOCK-MASTER
                PERFORM OPEN-000
                MOVE "END OF NEXT-FILE, 'ESC' TO THE CLEAR SCREEN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO READ-999.
            IF F-EXIT-CH = 1
              READ STOCK-MASTER PREVIOUS
                AT END
                CLOSE STOCK-MASTER
                PERFORM OPEN-000
                MOVE "END OF PREV-FILE, 'ESC' TO THE CLEAR SCREEN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO READ-999.
            IF WS-STOCK-ST1 = 91
                CLOSE STOCK-MASTER
                MOVE "STOCK BUSY SYSTEM ERROR=91, CALL YOUR SUPERVISOR."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM OPEN-000
                GO TO READ-999.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO READ-010.
               
            IF WS-CATEGORY > " "
             IF ST-CATEGORY NOT = WS-CATEGORY
                GO TO READ-010.
                
            IF WS-ONHAND-ONLY = "Y"
             IF ST-QTYONHAND NOT > 0
                MOVE 2701 TO POS
                DISPLAY "NEXT" AT POS
                GO TO READ-010
             ELSE
                MOVE 2701 TO POS
                DISPLAY "    " AT POS.

            IF WS-1ST = "Y"
              IF ST-DESCRIPTION1 NOT = " "
                MOVE ST-DESCRIPTION1 TO WS-SPLIT-INPUT-DESC
                MOVE "N" TO WS-1ST.
            MOVE ST-DESCRIPTION1 TO WS-SPLIT-DESC.
      *      IF WS-SP-1 NOT = WS-SP-I-1
      *          CLOSE STOCK-MASTER
      *          MOVE 2710 TO POS
      *          DISPLAY "No More Items With That Description." AT POS
      *          MOVE 2816 TO POS
      *          DISPLAY "Press 'ESC' To Clear The Screen !" AT POS
      *          CALL "&LOCKKBD" USING F-FIELDNAME
      *          MOVE 2710 TO POS
      *          DISPLAY "                                    " AT POS
      *          MOVE 2816 TO POS
      *          DISPLAY "                                    " AT POS
      *          GO TO READ-999.
       READ-020.
            PERFORM READ-SPECIAL-PRICES.
            ADD 1 TO SUB-3.
       READ-025.
            IF SUB-3 > 16
                MOVE 2701 TO POS
                DISPLAY "Press 'PgDn' For More, 'PgUp' for Previous,"
                 AT POS
                MOVE 2806 TO POS
                DISPLAY "Or 'ESC' To Clear The Screen !" AT POS

                MOVE 2655 TO POS
                DISPLAY "Analysis Codes:" AT POS
                MOVE 2748 TO POS
                DISPLAY "D=DELETE, N=NON RE-ORDERABLE, " AT POS
                MOVE 2848 TO POS
                DISPLAY "T=TOOLKIT ITEM, S=SPECIAL ORDER" AT POS
                MOVE "SHORTDESC" TO F-FIELDNAME
                MOVE 9           TO F-CBFIELDNAME
                MOVE 10          TO F-CBFIELDLENGTH
                PERFORM USER-FILL-FIELD.
            MOVE 2705 TO POS
            DISPLAY "                                        " AT POS
            MOVE 2806 TO POS
            DISPLAY "                                        " AT POS.
            
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-MIDDLE
                MOVE 0 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-3
                MOVE 800 TO WS-SUB1-DIS.
            IF F-EXIT-CH = X"05"
                PERFORM CLEAR-MIDDLE
                MOVE 1 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-3
                MOVE 800 TO WS-SUB1-DIS.
            IF F-EXIT-CH = X"07"
                MOVE " " TO WS-SHORTDESC
                MOVE 0   TO WS-STOCK-STATUS
                PERFORM CLEAR-MIDDLE
                CLOSE STOCK-MASTER
                PERFORM OPEN-000
                GO TO READ-999.
            IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                     AND NOT = X"07" AND NOT = 0     AND NOT = 1
                GO TO READ-025.
            IF ST-STOCKNUMBER = " "
                GO TO READ-010.

            ADD 100 TO WS-SUB1-DIS.
      *       DISPLAY "ÿAF" AT WS-SUB1-DIS.
            ADD 1 TO WS-SUB1-DIS.
            MOVE ST-STOCKNUMBER TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS
              WITH reverse-video FOREGROUND-COLOR IS 2.
      *      ADD 13 TO WS-SUB1-DIS.
      *       DISPLAY "ÿAA" AT WS-SUB1-DIS.
            MOVE " " TO WS-WORK.
            ADD 15 TO WS-SUB1-DIS.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
      *      ADD 1 TO WS-SUB1-DIS.
            MOVE ST-DESCRIPTION1 TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
            ADD 20 TO WS-SUB1-DIS.
            MOVE ST-DESCRIPTION2 TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
            ADD 20 TO WS-SUB1-DIS.
            MOVE ST-PRICE TO WS-PRICE.
            DISPLAY WS-PRICE AT WS-SUB1-DIS.
            
            ADD 9 TO WS-SUB1-DIS.
            MOVE STPR-PRICE TO WS-SP-PRICE.
            DISPLAY WS-SP-PRICE AT WS-SUB1-DIS.
            
            ADD 9 TO WS-SUB1-DIS.
            MOVE ST-ANALYSIS TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
             ADD 1 TO WS-SUB1-DIS.
            MOVE ST-QTYONHAND TO WS-QTY.
            DISPLAY WS-QTY AT WS-SUB1-DIS.
            
            SUBTRACT 75 FROM WS-SUB1-DIS.
            GO TO READ-010.
        READ-999.
            EXIT.
      *
       READ-BRANCH-DISPLAY SECTION.
       READ-BR-005.
            MOVE 0701 TO POS
            DISPLAY WS-BRANCH-LINE-DESC AT POS.
            
            MOVE " " TO WS-END.
      * W-READ-KEY  D= BY DESC, N= BY STOCK NUMBER
            IF W-READ-KEY = "D"
               MOVE WS-SHORTDESC TO ST-DESCRIPTION1
                                    WS-SPLIT-INPUT-DESC
            START STOCK-MASTER KEY NOT < ST-ALT-KEY
               INVALID KEY NEXT SENTENCE.
            IF W-READ-KEY = "N"
               MOVE WS-SHORTDESC TO ST-STOCKNUMBER
                                    WS-SPLIT-INPUT-DESC
            START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON START, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO READ-BR-999.
            MOVE 0 TO F-EXIT-CH.
            MOVE 0 TO SUB-2 SUB-3.
            MOVE 800 TO WS-SUB1-DIS.
       READ-BR-010.
            IF F-EXIT-CH = 0
              READ STOCK-MASTER NEXT
                AT END
                CLOSE STOCK-MASTER
                PERFORM OPEN-000
                MOVE "END OF NEXT-FILE, 'ESC' TO THE CLEAR SCREEN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO READ-BR-999.
            IF F-EXIT-CH = 1
              READ STOCK-MASTER PREVIOUS
                AT END
                CLOSE STOCK-MASTER
                PERFORM OPEN-000
                MOVE "END OF PREV-FILE, 'ESC' TO THE CLEAR SCREEN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO READ-BR-999.
            IF WS-STOCK-ST1 = 91
                CLOSE STOCK-MASTER
                MOVE "STOCK BUSY SYSTEM ERROR=91, CALL YOUR SUPERVISOR."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM OPEN-000
                GO TO READ-BR-999.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO READ-BR-010.
               
            IF WS-CATEGORY > " "
             IF ST-CATEGORY NOT = WS-CATEGORY
                GO TO READ-BR-010.

            IF WS-ONHAND-ONLY = "Y"
             IF ST-QTYONHAND NOT > 0
                MOVE 2701 TO POS
                DISPLAY "NEXT" AT POS
                GO TO READ-BR-010
             ELSE
                MOVE 2701 TO POS
                DISPLAY "    " AT POS.

            IF WS-1ST = "Y"
              IF ST-DESCRIPTION1 NOT = " "
                MOVE ST-DESCRIPTION1 TO WS-SPLIT-INPUT-DESC
                MOVE "N" TO WS-1ST.
            MOVE ST-DESCRIPTION1 TO WS-SPLIT-DESC.
       READ-BR-020.
            PERFORM READ-SPECIAL-PRICES.
            ADD 1 TO SUB-3.
       READ-BR-025.
            IF SUB-3 > 16
                MOVE 2701 TO POS
                DISPLAY "Press 'PgDn' For More, 'PgUp' for Previous,"
                 AT POS
                MOVE 2806 TO POS
                DISPLAY "Or 'ESC' To Clear The Screen !" AT POS

                MOVE 2655 TO POS
                DISPLAY "Analysis Codes:" AT POS
                MOVE 2748 TO POS
                DISPLAY "D=DELETE, N=NON RE-ORDERABLE, " AT POS
                MOVE 2848 TO POS
                DISPLAY "T=TOOLKIT ITEM, S=SPECIAL ORDER" AT POS
                MOVE "SHORTDESC" TO F-FIELDNAME
                MOVE 9           TO F-CBFIELDNAME
                MOVE 10          TO F-CBFIELDLENGTH
                PERFORM USER-FILL-FIELD.
            MOVE 2705 TO POS
            DISPLAY "                                        " AT POS
            MOVE 2806 TO POS
            DISPLAY "                                        " AT POS.
            
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-MIDDLE
                MOVE 0 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-3
                MOVE 0701 TO POS
                DISPLAY WS-BRANCH-LINE-DESC AT POS
                MOVE 800 TO WS-SUB1-DIS.
            IF F-EXIT-CH = X"05"
                PERFORM CLEAR-MIDDLE
                MOVE 1 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-3
                MOVE 0701 TO POS
                DISPLAY WS-BRANCH-LINE-DESC AT POS
                MOVE 800 TO WS-SUB1-DIS.
            IF F-EXIT-CH = X"07"
                MOVE " " TO WS-SHORTDESC
                MOVE 0   TO WS-STOCK-STATUS
                PERFORM CLEAR-MIDDLE
                CLOSE STOCK-MASTER
                PERFORM OPEN-000
                GO TO READ-BR-999.
            IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                     AND NOT = X"07" AND NOT = 0     AND NOT = 1
                GO TO READ-BR-025.
            IF ST-STOCKNUMBER = " "
                GO TO READ-BR-010.

            PERFORM READ-BRANCH-STOCK.
 
            ADD 100 TO WS-SUB1-DIS.
            ADD 1 TO WS-SUB1-DIS.
            MOVE ST-STOCKNUMBER TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS
              WITH reverse-video FOREGROUND-COLOR IS 2.
            MOVE " " TO WS-WORK.
            ADD 15 TO WS-SUB1-DIS.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
            MOVE ST-DESCRIPTION1 TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
            ADD 20 TO WS-SUB1-DIS.
            MOVE ST-DESCRIPTION2 TO WS-SHORTDESC2.
            DISPLAY WS-SHORTDESC2 AT WS-SUB1-DIS.
            
            ADD 2 TO WS-SUB1-DIS.
            MOVE ST-PRICE TO WS-PRICE.
            DISPLAY WS-PRICE AT WS-SUB1-DIS.
            
            ADD 9 TO WS-SUB1-DIS.
            MOVE STPR-PRICE TO WS-SP-PRICE.
            DISPLAY WS-SP-PRICE AT WS-SUB1-DIS.

            ADD 9 TO WS-SUB1-DIS.
            MOVE ST-QTYONHAND         TO F-EDNAMEFIELDINV
            DISPLAY F-EDNAMEFIELDINV AT WS-SUB1-DIS.
            
            MOVE 1 TO SUB-1.
        READ-BR-400.
            ADD 6 TO WS-SUB1-DIS.
            IF WS-BRANCH-ONHAND (SUB-1) = 999999
               MOVE " N/STK"                 TO WS-WORK
               DISPLAY WS-WORK AT WS-SUB1-DIS
            ELSE
               MOVE WS-BRANCH-ONHAND (SUB-1) TO F-EDNAMEFIELDINV
               DISPLAY F-EDNAMEFIELDINV AT WS-SUB1-DIS.
        READ-BR-555.
            IF SUB-1 < 10
               ADD 1 TO SUB-1.
            IF WS-BRANCH-NUMBER (SUB-1) > 0
      *          ADD 10 TO WS-SUB1-DIS
                GO TO READ-BR-400.

            SUBTRACT 74 FROM WS-SUB1-DIS.
            GO TO READ-BR-010.
        READ-BR-999.
            EXIT.
      *
       READ-SPECIAL-DISPLAY SECTION.
       RDSP-005.
            IF F-EXIT-CH = X"19" OR = X"99"
               MOVE WS-SHORTDESC TO STPR-STOCKNUMBER
                                      WS-SPLIT-INPUT-DESC
            START STPR-MASTER KEY NOT < STPR-KEY
               INVALID KEY NEXT SENTENCE.
            MOVE 0 TO F-EXIT-CH.
            IF WS-STPR-ST1 NOT = 0
                MOVE 
           "THERE ARE NO SPECIALS ON START TO READ, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STPR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO RDSP-999.
            MOVE 0   TO SUB-2 SUB-3.
            MOVE 800 TO WS-SUB1-DIS.
       RDSP-010.
            IF F-EXIT-CH = 0
              READ STPR-MASTER NEXT
                 AT END
                CLOSE STPR-MASTER
                PERFORM OPEN-005
                MOVE 
                "END OF SPECIALS NEXT-READ, 'ESC' TO THE CLEAR SCREEN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDSP-999.
            IF F-EXIT-CH = 1
              READ STPR-MASTER PREVIOUS
                 AT END
                CLOSE STPR-MASTER
                PERFORM OPEN-005
                MOVE 
                "END OF SPECIALS PREV-READ, 'ESC' TO THE CLEAR SCREEN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDSP-999.
            IF WS-STPR-ST1 = 91
                MOVE 0 TO WS-STPR-STATUS
                CLOSE STPR-MASTER
                MOVE "SPECIALS SYSTEM ERROR=9, CALL YOUR SUPERVISOR."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM OPEN-005
                GO TO RDSP-010.
            IF WS-STPR-ST1 NOT = 0
                MOVE "STOCK SPECIALS BUSY ON READ-NEXT, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STPR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO RDSP-010.
                
            PERFORM READ-STOCK.
               
            IF WS-CATEGORY > " "
             IF ST-CATEGORY NOT = WS-CATEGORY
                GO TO RDSP-010.
            
            IF WS-ONHAND-ONLY = "Y"
             IF ST-QTYONHAND NOT > 0
                MOVE 2701 TO POS
                DISPLAY "NEXT" AT POS
                GO TO RDSP-010
             ELSE
                MOVE 2701 TO POS
                DISPLAY "    " AT POS.
                
            IF WS-1ST = "Y"
              IF ST-DESCRIPTION1 NOT = " "
                MOVE ST-DESCRIPTION1 TO WS-SPLIT-INPUT-DESC
                MOVE "N" TO WS-1ST.
            MOVE ST-DESCRIPTION1 TO WS-SPLIT-DESC.
      *      IF WS-SP-1 NOT = WS-SP-I-1
      *          CLOSE STOCK-MASTER
      *          MOVE 2710 TO POS
      *          DISPLAY "No More Items With That Description." AT POS
      *          MOVE 2816 TO POS
      *          DISPLAY "Press 'ESC' To Clear The Screen !" AT POS
      *          CALL "&LOCKKBD" USING F-FIELDNAME
      *          MOVE 2710 TO POS
      *          DISPLAY "                                    " AT POS
      *          MOVE 2816 TO POS
      *          DISPLAY "                                    " AT POS
      *          GO TO RDSP-999.
       RDSP-020.
            ADD 1 TO SUB-3.
       RDSP-025.
            IF SUB-3 > 16
                MOVE 2701 TO POS
                DISPLAY "Press 'PgDn' For More, 'PdUp' for Previous,"
                 AT POS
                MOVE 2806 TO POS
                DISPLAY "Or 'ESC' To Clear The Screen !" AT POS
                MOVE 2655 TO POS
                DISPLAY "Analysis Codes:" AT POS
                MOVE 2748 TO POS
                DISPLAY "D=DELETE, N=NON RE-ORDERABLE, " AT POS
                MOVE 2848 TO POS
                DISPLAY "T=TOOLKIT ITEM, S=SPECIAL ORDER" AT POS
                PERFORM USER-FILL-FIELD.
            MOVE 2705 TO POS
            DISPLAY "                              " AT POS
            MOVE 2806 TO POS
            DISPLAY "                              " AT POS.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-MIDDLE
                MOVE 0 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-3
                MOVE 800 TO WS-SUB1-DIS.
            IF F-EXIT-CH = X"05"
                PERFORM CLEAR-MIDDLE
                MOVE 1 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-3
                MOVE 800 TO WS-SUB1-DIS.
            IF F-EXIT-CH = X"07"
                MOVE " " TO WS-SHORTDESC
                MOVE " " TO WS-STOCK-STATUS
                PERFORM CLEAR-MIDDLE
      *          CLOSE STPR-MASTER
      *          PERFORM OPEN-005
                GO TO RDSP-999.
            IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                     AND NOT = X"07" AND NOT = 0     AND NOT = 1
                GO TO RDSP-025.
            IF ST-STOCKNUMBER = " "
                GO TO RDSP-010.

            ADD 100 TO WS-SUB1-DIS.
      *      DISPLAY "ÿAF" AT WS-SUB1-DIS.
            ADD 1 TO WS-SUB1-DIS.
            MOVE ST-STOCKNUMBER TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS
              WITH reverse-video FOREGROUND-COLOR IS 2.

      *      ADD 13 TO WS-SUB1-DIS.
      *      DISPLAY "ÿAA" AT WS-SUB1-DIS.
            MOVE " " TO WS-WORK.
            ADD 15 TO WS-SUB1-DIS.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
      *      ADD 1 TO WS-SUB1-DIS.
            MOVE ST-DESCRIPTION1 TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
            ADD 20 TO WS-SUB1-DIS.
            MOVE ST-DESCRIPTION2 TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
            ADD 20 TO WS-SUB1-DIS.
            MOVE ST-PRICE TO WS-PRICE.
            DISPLAY WS-PRICE AT WS-SUB1-DIS.
            
            ADD 9 TO WS-SUB1-DIS.
            MOVE STPR-PRICE TO WS-SP-PRICE.
            DISPLAY WS-SP-PRICE AT WS-SUB1-DIS.
            
            ADD 9 TO WS-SUB1-DIS.
            MOVE ST-ANALYSIS TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
             ADD 1 TO WS-SUB1-DIS.
            MOVE ST-QTYONHAND TO WS-QTY.
            DISPLAY WS-QTY AT WS-SUB1-DIS.
            
            SUBTRACT 75 FROM WS-SUB1-DIS.
            GO TO RDSP-010.
        RDSP-999.
            EXIT.
      *
       READ-SPECIAL-PRICES SECTION.
       SPR-000.
           MOVE ST-STOCKNUMBER TO STPR-STOCKNUMBER.
           START STPR-MASTER KEY NOT < STPR-KEY
               INVALID KEY NEXT SENTENCE.
       SPR-005.
           READ STPR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STPR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO STPR-PRICE
               GO TO SPR-999.
           IF WS-STPR-ST1 NOT = 0
              Move "SPECIAL PRICES BUSY ON READ, 'ESC' to RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STPR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STPR-ST1
              GO TO SPR-005.
       SPR-999.
           EXIT.
      *
       READ-STOCK SECTION.
       R-STOCK-000.
           MOVE STPR-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
       R-STOCK-005.
           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE 0 TO ST-PRICE
               GO TO R-STOCK-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-STOCK-005.
           IF F-EXIT-CH = X"1F" OR = X"9F"
               PERFORM READ-BRANCH-STOCK.
       R-STOCK-999.
           EXIT.
      *
       READ-BRANCH-STOCK SECTION.
       RBRST-100.
           IF WS-END = "Y"
               GO TO RBRST-900.
            IF WS-STOCK1 = " "
                GO TO RBRST-200.
            MOVE ST-STOCKNUMBER TO ST1-STOCKNUMBER
            START STOCK-MASTER1 KEY NOT < ST1-KEY
                INVALID KEY NEXT SENTENCE.
            MOVE "Reading 1st Branch Stock File....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 1 TO SUB-1.
       RBRST-110.
            READ STOCK-MASTER1
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK1-ST1 = 23 OR 35 OR 49
                MOVE 999999 TO WS-BRANCH-ONHAND (SUB-1)
                               WS-BRANCH-ONRES (SUB-1)
                               WS-BRANCH-ONORDER (SUB-1)
                               WS-BRANCH-ONBO (SUB-1)
                GO TO RBRST-200.
             IF WS-STOCK1-ST1 NOT = 0
                MOVE "STOCK RECORD1 BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK1-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK1-ST1
                GO TO RBRST-110.
             MOVE ST1-QTYONHAND    TO WS-BRANCH-ONHAND (SUB-1)
             MOVE ST1-QTYONRESERVE TO WS-BRANCH-ONRES (SUB-1)
             MOVE ST1-QTYONORDER   TO WS-BRANCH-ONORDER (SUB-1)
             MOVE ST1-QTYONBORDER  TO WS-BRANCH-ONBO (SUB-1).
       RBRST-200.
            IF WS-STOCK2 = " "
                GO TO RBRST-300.
            MOVE ST-STOCKNUMBER TO ST2-STOCKNUMBER
            START STOCK-MASTER2 KEY NOT < ST2-KEY
                INVALID KEY NEXT SENTENCE.
            MOVE "Reading 2nd Branch Stock File....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 2 TO SUB-1.
       RBRST-210.
            READ STOCK-MASTER2
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK2-ST1 = 23 OR 35 OR 49
                MOVE 999999 TO WS-BRANCH-ONHAND (SUB-1)
                               WS-BRANCH-ONRES (SUB-1)
                               WS-BRANCH-ONORDER (SUB-1)
                               WS-BRANCH-ONBO (SUB-1)
                GO TO RBRST-300.
             IF WS-STOCK2-ST1 NOT = 0
                MOVE "STOCK RECORD2 BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK2-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK2-ST1
                GO TO RBRST-210.
             MOVE ST2-QTYONHAND    TO WS-BRANCH-ONHAND (SUB-1)
             MOVE ST2-QTYONRESERVE TO WS-BRANCH-ONRES (SUB-1)
             MOVE ST2-QTYONORDER   TO WS-BRANCH-ONORDER (SUB-1)
             MOVE ST2-QTYONBORDER  TO WS-BRANCH-ONBO (SUB-1).
       RBRST-300.
            IF WS-STOCK3 = " "
                GO TO RBRST-400.
            MOVE ST-STOCKNUMBER TO ST3-STOCKNUMBER
            START STOCK-MASTER3 KEY NOT < ST3-KEY
                INVALID KEY NEXT SENTENCE.
            MOVE "Reading 3rd Branch Stock File....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 3 TO SUB-1.
       RBRST-310.
            READ STOCK-MASTER3
                INVALID KEY NEXT SENTENCE.
             IF WS-STOCK3-ST1 = 23 OR 35 OR 49
                MOVE 999999 TO WS-BRANCH-ONHAND (SUB-1)
                               WS-BRANCH-ONRES (SUB-1)
                               WS-BRANCH-ONORDER (SUB-1)
                               WS-BRANCH-ONBO (SUB-1)
                GO TO RBRST-400.
             IF WS-STOCK3-ST1 NOT = 0
                MOVE "STOCK RECORD3 BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK3-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK3-ST1
                GO TO RBRST-310.
             MOVE ST3-QTYONHAND    TO WS-BRANCH-ONHAND (SUB-1)
             MOVE ST3-QTYONRESERVE TO WS-BRANCH-ONRES (SUB-1)
             MOVE ST3-QTYONORDER   TO WS-BRANCH-ONORDER (SUB-1)
             MOVE ST3-QTYONBORDER  TO WS-BRANCH-ONBO (SUB-1).
       RBRST-400.
            IF WS-STOCK4 = " "
                GO TO RBRST-900.
            MOVE ST-STOCKNUMBER TO ST4-STOCKNUMBER
            START STOCK-MASTER4 KEY NOT < ST4-KEY
                INVALID KEY NEXT SENTENCE.
            MOVE "Reading 4th Branch Stock File....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 4 TO SUB-1.
       RBRST-410.
            READ STOCK-MASTER4
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK4-ST1 = 23 OR 35 OR 49
                MOVE 999999 TO WS-BRANCH-ONHAND (SUB-1)
                               WS-BRANCH-ONRES (SUB-1)
                               WS-BRANCH-ONORDER (SUB-1)
                               WS-BRANCH-ONBO (SUB-1)
                GO TO RBRST-900.
             IF WS-STOCK4-ST1 NOT = 0
                MOVE "STOCK RECORD4 BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK4-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK4-ST1
                GO TO RBRST-410.
             MOVE ST4-QTYONHAND    TO WS-BRANCH-ONHAND (SUB-1)
             MOVE ST4-QTYONRESERVE TO WS-BRANCH-ONRES (SUB-1)
             MOVE ST4-QTYONORDER   TO WS-BRANCH-ONORDER (SUB-1)
             MOVE ST4-QTYONBORDER  TO WS-BRANCH-ONBO (SUB-1).
       RBRST-900.
             PERFORM ERROR4-020.
       RBRST-999.
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
           MOVE 1 TO SUB-20.
       RNC-010.
           READ COMPANY-MENU NEXT
               AT END NEXT SENTENCE.
           IF WS-MENU-ST1 = 10
               GO TO RNC-900.
           IF WS-MENU-ST1 NOT = 0
               MOVE "COMPANY FILE BUSY ON READ-NEXT, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-MENU-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-MENU-ST1
               GO TO RNC-010.
           MOVE PTY-VOL-DIR TO LIST-VOL-DIR (SUB-20)
           MOVE PTY-NUMBER  TO LIST-NUMBER (SUB-20)
           MOVE PTY-CO-NAME TO LIST-NAME (SUB-20).
           
           IF SUB-20 < 20
              ADD 1 TO SUB-20
              GO TO RNC-010.
       RNC-900.
           CLOSE COMPANY-MENU.
       RNC-999.
           EXIT.
      *
       START-PARAM-RECORD SECTION.
       STR-000.
           MOVE 1         TO PA-RECORD.
           MOVE 7         TO PA-TYPE.
           START PARAMETER-FILE KEY NOT < PA-KEY
              INVALID KEY NEXT SENTENCE.
       STR-999.
             EXIT.
      *
       READ-PARAM-NEXT SECTION.
       RNX-001.
           MOVE 0   TO SUB-1.
           MOVE 0 TO WS-SLPARAMETER-ST1.
           PERFORM START-PARAM-RECORD.
       RNX-005.
           READ PARAMETER-FILE NEXT
             AT END
               GO TO RNX-999.
           IF PA-TYPE NOT = 7
               GO TO RNX-999.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "PARAM FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RNX-999.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "GOING TO RESTART PARAM FILE, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               PERFORM START-PARAM-RECORD
               GO TO RNX-005.
           ADD 1 TO SUB-1.
       RNX-010.
           MOVE PARAMETER-REC TO WS-BRANCH-INFO (SUB-1).
           
           GO TO RNX-005.
       RNX-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 0 TO PA-TYPE.
           MOVE 1 TO PA-RECORD.
           READ PARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO PARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-SLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RP-000.
       RP-999.
           EXIT.
      *
       READ-INVQUES-FILE SECTION.
       RINVQUES-000.
            MOVE 1 TO PA-RECORD.
            MOVE 6 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RINVQUES-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "N" TO WS-QUES-MU-GP-PERC
               GO TO RINVQUES-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER BUSY RINVQUES, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RINVQUES-010.
       RINVQUES-900.
            MOVE INVQUES-MU-GP-PERC TO WS-QUES-MU-GP-PERC.
       RINVQUES-999.
            EXIT.
      *     
       CHECK-BRANCH-DATA-NAMES SECTION.
       CBDN-005.
          MOVE 1 TO SUB-20.
          PERFORM STRIP-STOCKNAME.
       CBDN-006.
          MOVE " " TO ALPHA-RATE
                      DATA-RATE.
          MOVE 0   TO SUB-1.
          MOVE WS-BRANCH-NUMBER (SUB-20) TO SUB-25.
       CBDN-010.
          MOVE LIST-VOL-DIR (SUB-25) TO ALPHA-RATE.
       CBDN-015.
          ADD 1 TO SUB-1.
          IF SUB-1 NOT > 60
           IF AL-RATE (SUB-1) NOT = " "
            GO TO CBDN-015.
       CBDN-020.
          MOVE WS-BARE-STOCK TO DATA-RATE.
          MOVE 1             TO SUB-2.
       CBDN-025.
          MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
          ADD 1 TO SUB-1 SUB-2.
          IF DAT-RATE (SUB-2) NOT = " "
           IF SUB-1 NOT > 60
             GO TO CBDN-025.
       CBDN-030.
          MOVE ALPHA-RATE TO WS-BRANCH-STOCK-VOL-DIR (SUB-20).
          ADD 1 TO SUB-20.
          IF WS-BRANCH-NUMBER (SUB-20) > 0
              GO TO CBDN-006.
       CBDN-040.
          MOVE WS-BRANCH-STOCK-VOL-DIR (1) TO WS-STOCK1.
          MOVE WS-BRANCH-STOCK-VOL-DIR (2) TO WS-STOCK2.
          MOVE WS-BRANCH-STOCK-VOL-DIR (3) TO WS-STOCK3.
          MOVE WS-BRANCH-STOCK-VOL-DIR (4) TO WS-STOCK4.
       CBDN-999.
          EXIT.
      * 
       STRIP-STOCKNAME SECTION.
       STRIP-000.
      * SUB-10 IS TO COUNT HOW MANY "/" HAVE BEEN FOUND.  WE NEED 3
          MOVE 0 TO SUB-2 SUB-10.
          MOVE 1 TO SUB-1.
          MOVE " " TO ALPHA-RATE
                      DATA-RATE.
          MOVE WS-STOCK TO ALPHA-RATE.
       STRIP-015.
          ADD 1 TO SUB-2.
          IF SUB-2 NOT > 60
           IF AL-RATE (SUB-2) NOT = "/"
            GO TO STRIP-015.
           IF AL-RATE (SUB-2) = "/"
            IF SUB-10 < 2
              ADD 1 TO SUB-10
            GO TO STRIP-015.

           ADD 1 TO SUB-2.
       STRIP-020.
           MOVE AL-RATE (SUB-2) TO DAT-RATE (SUB-1).
           ADD 1 TO SUB-2 SUB-1.
           IF AL-RATE (SUB-2) NOT = " "
              GO TO STRIP-020.
       STRIP-030.
           MOVE DATA-RATE TO WS-BARE-STOCK.
       STRIP-999.
           EXIT.
      *
       CHECK-DATA-SIZE SECTION.         
       CDS-005.
           MOVE " " TO ALPHA-RATE.
           MOVE 0   TO SUB-2.
       CDS-015.
           ADD 1 TO SUB-2.
           IF AL-RATE (SUB-2) NOT = " "
            IF SUB-2 NOT > 60
            GO TO CDS-015.
          SUBTRACT 1 FROM SUB-2.
       CDS-999.
          EXIT.
      *
       OPEN-NODE-FILE SECTION.
       ONF-001.
           IF WS-STOCK1 = " "
               GO TO ONF-002.
           MOVE 1 TO SUB-1.

           PERFORM CDS-005.
           Move Ws-STOCK1 To Alpha-Rate.
           PERFORM CDS-015.

           MOVE WS-STOCK1        TO F-FILENAME
           MOVE SUB-2            TO F-CBFILENAME.
           CALL "OPENFILE" USING    F-ERROR5
                                    F-FH
                                    F-FILENAME
                                    F-CBFILENAME
                                    F-FILENAME
                                    F-INTEGERZERO
                                    F-OPENMODE.
                                    
           IF F-ERROR5 NOT = 0
            IF F-ERROR5 NOT = 220
               MOVE " " TO WS-STOCK1
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO ONF-002.

           IF F-ERROR5 NOT = 210
               CALL "CLOSEFILE" USING  F-ERROR5
                                       F-FH.
       ONF-002.
           IF WS-STOCK2 = " "
               GO TO ONF-003.
           MOVE 2 TO SUB-1.

           PERFORM CDS-005.
           Move Ws-STOCK2 To Alpha-Rate.
           PERFORM CDS-015.

           MOVE WS-STOCK2        TO F-FILENAME
           MOVE SUB-2            TO F-CBFILENAME.
           CALL "OPENFILE" USING    F-ERROR5
                                    F-FH
                                    F-FILENAME
                                    F-CBFILENAME
                                    F-FILENAME
                                    F-INTEGERZERO
                                    F-OPENMODE.
                                    
           IF F-ERROR5 NOT = 0
            IF F-ERROR5 NOT = 220
               MOVE " " TO WS-STOCK2
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO ONF-003.

           IF F-ERROR5 NOT = 210
               CALL "CLOSEFILE" USING  F-ERROR5
                                       F-FH.
       ONF-003.
           IF WS-STOCK3 = " "
               GO TO ONF-004.
           MOVE 3 TO SUB-1.
           
           PERFORM CDS-005.
           Move Ws-STOCK3 To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE WS-STOCK3        TO F-FILENAME
           MOVE SUB-2            TO F-CBFILENAME.
           CALL "OPENFILE" USING    F-ERROR5
                                    F-FH
                                    F-FILENAME
                                    F-CBFILENAME
                                    F-FILENAME
                                    F-INTEGERZERO
                                    F-OPENMODE.
                                    
           IF F-ERROR5 NOT = 0
            IF F-ERROR5 NOT = 220
               MOVE " " TO WS-STOCK3
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO ONF-004.

           IF F-ERROR5 NOT = 210
               CALL "CLOSEFILE" USING  F-ERROR5
                                       F-FH.
       ONF-004.
           IF WS-STOCK4 = " "
               GO TO ONF-999.
           MOVE 4 TO SUB-1.
           
           PERFORM CDS-005.
           Move Ws-STOCK4 To Alpha-Rate.
           PERFORM CDS-015.
           
           MOVE WS-STOCK4        TO F-FILENAME
           MOVE SUB-2            TO F-CBFILENAME.
           CALL "OPENFILE" USING    F-ERROR5
                                    F-FH
                                    F-FILENAME
                                    F-CBFILENAME
                                    F-FILENAME
                                    F-INTEGERZERO
                                    F-OPENMODE.
                                    
           IF F-ERROR5 NOT = 0
            IF F-ERROR5 NOT = 220
               MOVE " " TO WS-STOCK4
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO ONF-999.

           IF F-ERROR5 NOT = 210
               CALL "CLOSEFILE" USING  F-ERROR5
                                       F-FH.
       ONF-999.
            EXIT.
      *
       OPEN-BRANCH-STOCK SECTION.
       OBS-000.
            PERFORM OPEN-NODE-FILE.
       OBS-001.
            IF WS-STOCK1 = " "
                GO TO OBS-002.
            MOVE "Opening Branch1 Stock File ....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 1 TO SUB-1.
            MOVE WS-BRANCH-NAME (SUB-1) TO WS-LINE-BR1.

            OPEN I-O STOCK-MASTER1.
            IF WS-STOCK1-ST1 = 91
               MOVE "STOCK FILE1 CAN'T BE OPENED, 'ESC' TO IGNORE."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-BRANCH-NAME (SUB-1) TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE " " TO WS-STOCK1
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO OBS-002.
            IF WS-STOCK1-ST1 NOT = 0
               MOVE "STOCK FILE1 BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK1-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK1-ST1
               GO TO OBS-001.
       OBS-002.
            IF WS-STOCK2 = " "
                GO TO OBS-003.
            MOVE "Opening Branch2 Stock File ....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 2 TO SUB-1.
            MOVE WS-BRANCH-NAME (SUB-1) TO WS-LINE-BR2.

            OPEN I-O STOCK-MASTER2.
            IF WS-STOCK2-ST1 = 91
               MOVE "STOCK FILE2 CAN'T BE OPENED, 'ESC' TO IGNORE."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-BRANCH-NAME (SUB-1) TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE " " TO WS-STOCK2
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO OBS-003.
            IF WS-STOCK2-ST1 NOT = 0
               MOVE "STOCK FILE2 BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK2-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK2-ST1
               GO TO OBS-002.
       OBS-003.
            IF WS-STOCK3 = " "
                GO TO OBS-004.
            MOVE "Opening Branch3 Stock File ....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 3 TO SUB-1.
            MOVE WS-BRANCH-NAME (SUB-1) TO WS-LINE-BR3.

            OPEN I-O STOCK-MASTER3.
            IF WS-STOCK3-ST1 = 91
               MOVE "STOCK FILE3 CAN'T BE OPENED, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-BRANCH-NAME (SUB-1) TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE " " TO WS-STOCK3
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO OBS-004.
            IF WS-STOCK3-ST1 NOT = 0
               MOVE "STOCK FILE3 BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK3-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK3-ST1
               GO TO OBS-003.
       OBS-004.
            IF WS-STOCK4 = " "
                GO TO OBS-900.
            MOVE "Opening Branch4 Stock File ....." TO WS-MESSAGE
            PERFORM ERROR4-000.
            MOVE 4 TO SUB-1.
            MOVE WS-BRANCH-NAME (SUB-1) TO WS-LINE-BR4.

            OPEN I-O STOCK-MASTER4.
            IF WS-STOCK4-ST1 = 91
               MOVE "STOCK FILE4 CAN'T BE OPENED, 'ESC' TO IGNORE."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-BRANCH-NAME (SUB-1) TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE " " TO WS-STOCK4
               MOVE "XX" TO WS-BRANCH-NOT-THERE (SUB-1)
               GO TO OBS-999.
            IF WS-STOCK4-ST1 NOT = 0
               MOVE "STOCK FILE4 BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK4-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK4-ST1
               GO TO OBS-004.
       OBS-900.
             PERFORM ERROR1-020.
       OBS-999.
            EXIT.
      *
       CLEAR-MIDDLE SECTION.
       CM-010.
           MOVE " " TO WS-MIDDLE
           MOVE 801 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 901 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1001 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1101 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1201 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1301 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1401 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1501 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1601 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1701 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1801 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1901 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2001 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2101 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2201 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2301 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2401 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2501 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2601 TO POS
           DISPLAY WS-MIDDLE AT POS.
       CM-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK-MASTER BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE 0 TO WS-STOCK-STATUS
                GO TO OPEN-000.
       OPEN-005.
      *       GO TO OPEN-008.
      *
            OPEN I-O STPR-MASTER.
            IF WS-STPR-ST1 NOT = 0
               MOVE 0 TO WS-STPR-ST1
               MOVE "STOCK PRICE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-006.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-006.

           PERFORM READ-PARAM-NEXT.
           PERFORM READ-INVQUES-FILE.
           PERFORM READ-PARAMETER.
           CLOSE PARAMETER-FILE.
       OPEN-008.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StDescIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STOCK-MASTER
                 STPR-MASTER.
           PERFORM CLEAR-SCREEN.
      *     STOP RUN.
           EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
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
       Copy "Error4Message".
      *
      * END-OF-JOB
