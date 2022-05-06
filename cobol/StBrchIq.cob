        IDENTIFICATION DIVISION.
        PROGRAM-ID. StBrchIq.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES.
           CRT STATUS IS W-CRTSTATUS.
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
          Copy "SelectSlDaily".
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
           COPY ChlfdCompany.
           COPY ChlfdDaily.
           Copy ChlfdDataName.
           COPY ChlfdParam.
           COPY ChlfdStPrice.
      *
       WORKING-STORAGE SECTION.
       77  Ws-cbStock         PIC 9(3) VALUE 11.
       77  WS-RANGE1          PIC X(15) VALUE " ".      
       77  WS-RANGE2          PIC X(15) VALUE " ".      
       77  WS-RANGE3          PIC X VALUE " ".      
       77  WS-RANGE4          PIC X VALUE " ".      
       77  WS-RANGE5          PIC X VALUE " ".      
       77  WS-UPDATE          PIC X VALUE " ".      
       77  WS-CATEGORY        PIC X(3) VALUE " ".      
       77  WS-NUMBER          PIC 9(4) VALUE 0.
       77  WS-COMPANY-UPDATE  PIC 9(2) VALUE 0.
       77  WS-END             PIC X VALUE " ".
       77  WS-NO-OF-READS     PIC 9(2) VALUE 0.
       77  WS-STOCKNUMBER     PIC X(15) VALUE " ".
       77  WS-BARE-STOCK      PIC X(20) VALUE " ".
       77  WS-DIS-COSTS       PIC X VALUE "N".
       77  WS-INQUIRY-PROGRAM    PIC X(8) VALUE "StDescIq".
       77  WS-VATPRICE           PIC 9(6)V99 VALUE 0.
       77  WS-PERCENT            PIC S9(3)V99 VALUE 0.
       77  WS-QUES-MU-GP-PERC    PIC X VALUE " ".
       77  WS-PasswordSaved      Pic X(10).
       77  PSW-SUB1              PIC S9(5) VALUE 0.
       77  PSW-SUB2              PIC S9(5) VALUE 0.
       01  W-READ-KEY            PIC X.
       01  W-CRTSTATUS           PIC 9(4) VALUE 0.
       01  WS-PASSWORD-KEY.
           03  WS-PA-KEY         PIC X OCCURS 11.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1       PIC 99.
       01  WS-STOCK1-STATUS.
           03  WS-STOCK1-ST1      PIC 99.
       01  WS-STOCK2-STATUS.
           03  WS-STOCK2-ST1      PIC 99.
       01  WS-STOCK3-STATUS.
           03  WS-STOCK3-ST1      PIC 99.
       01  WS-STOCK4-STATUS.
           03  WS-STOCK4-ST1      PIC 99.
       01  WS-MENU-STATUS.
           03  WS-MENU-ST1        PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1       PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  WS-DATA-STATUS.
           03  WS-DATA-ST1        PIC 99.
       01  WS-STPR-STATUS.
           03  WS-STPR-ST1        PIC 99.
       01 COMPANIES-NAME-LIST.
         02  COMPANIES-LIST OCCURS 20.
           03  LIST-NAME        PIC X(40).
           03  LIST-NUMBER      PIC 99.
           03  LIST-VOL-DIR     PIC X(25).
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
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
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
           PERFORM CLEAR-SCREEN.
           MOVE 0317 TO POS
           DISPLAY "*** BRANCH STOCK FILE INQUIRY PROGRAM ***" AT POS
           MOVE 0417 TO POS
           DISPLAY "*****************************************" AT POS.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN.
       CONT-010.
           PERFORM DISPLAY-FORM
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
       CONT-020.
           PERFORM GET-DATA
           GO TO CONT-020.
       CONT-999.
           Exit.
      *
       GET-DATA SECTION.
       GET-001.
            MOVE 2810 TO POS
            DISPLAY 
            "PRESS <ALT-F12> TO SWITCH BETWEEN SEEING COSTS OR NOT." 
               AT POS.
       
            MOVE " " TO WS-END.
            MOVE 0   TO WS-NO-OF-READS.
            MOVE WS-LASTPASSWORD TO WS-PASSWORDSAVED.
            
            MOVE "STOCKNUMBER" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 25            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD   TO ST-STOCKNUMBER 
                                  WS-STOCKNUMBER.

      *  IF F-EXIT-CH = X"91" = <Code-Scroll-up>

           IF F-EXIT-CH = X"91"
              MOVE WS-LASTPASSWORD TO WS-PASSWORDSAVED
              PERFORM CHECK-PASSWORD.
           IF F-EXIT-CH = X"91"
            IF WS-PASSWORD-VALID = "Y"
             IF WS-DIS-COSTS = "N"
                 MOVE "Y" TO WS-DIS-COSTS
                 PERFORM GET-011
             ELSE
                 MOVE "N" TO WS-DIS-COSTS
                 PERFORM GET-011.
           IF F-EXIT-CH = X"91"
            IF WS-PASSWORD-VALID = "N"
             IF WS-DIS-COSTS = "Y"
                 MOVE "N" TO WS-DIS-COSTS.
            IF WS-LASTPASSWORD = " "  
               MOVE WS-PASSWORDSAVED TO WS-LASTPASSWORD.
               
            IF F-EXIT-CH = X"0C"
                 PERFORM READ-STOCK-NEXT
                 GO TO GET-003.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-STOCK-PREVIOUS
                 GO TO GET-003.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                  PERFORM CLEAR-SCREEN-FORM
                  PERFORM DISPLAY-FORM
                  GO TO GET-001.
            IF ST-STOCKNUMBER  = 0 OR = "   "
             IF F-EXIT-CH NOT = X"19"
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"91"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-001.
            PERFORM READ-STOCK.
            GO TO GET-005.
       GET-003.
            MOVE ST-STOCKNUMBER  TO F-NAMEFIELD
            MOVE 15                     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-005.
            MOVE "DESCRIPTION1"   TO F-FIELDNAME
            MOVE 12               TO F-CBFIELDNAME
            MOVE ST-DESCRIPTION1  TO F-NAMEFIELD
            MOVE 20               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESCRIPTION2"   TO F-FIELDNAME
            MOVE 12               TO F-CBFIELDNAME
            MOVE ST-DESCRIPTION2  TO F-NAMEFIELD
            MOVE 20               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CATEGORY"   TO F-FIELDNAME
            MOVE 8            TO F-CBFIELDNAME
            MOVE ST-CATEGORY  TO F-NAMEFIELD
            MOVE 3            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUPPLIER"   TO F-FIELDNAME
            MOVE 8            TO F-CBFIELDNAME
            MOVE ST-SUPPLIER  TO F-NAMEFIELD
            MOVE 7            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "FOREIGNCOST"   TO F-FIELDNAME
            MOVE 11              TO F-CBFIELDNAME
            MOVE ST-FOREIGNCOST  TO F-EDNAMEFIELDFOREIGN
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-FOREIGN.

            MOVE "SUPP-DISC"      TO F-FIELDNAME
            MOVE 9                TO F-CBFIELDNAME
            MOVE ST-SUPPLIERDISC  TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "CURRENCY"   TO F-FIELDNAME
            MOVE 8            TO F-CBFIELDNAME
            MOVE ST-CURRENCY  TO F-NAMEFIELD
            MOVE 5            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CURR-RATE"       TO F-FIELDNAME
            MOVE 9                 TO F-CBFIELDNAME
            MOVE ST-CURRENCY-RATE  TO F-EDNAMEFIELDVALUE
            MOVE 9                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-VALUE.

            MOVE "PRICE"   TO F-FIELDNAME
            MOVE 5         TO F-CBFIELDNAME
            MOVE ST-PRICE  TO F-EDNAMEFIELDAMOUNT
            MOVE 9         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "OLDPRICE"   TO F-FIELDNAME
            MOVE 8            TO F-CBFIELDNAME
            MOVE ST-OLDPRICE  TO F-EDNAMEFIELDAMOUNT
            MOVE 9            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.

            MOVE "SPRICE"   TO F-FIELDNAME
            MOVE 6          TO F-CBFIELDNAME
            MOVE STPR-PRICE TO F-EDNAMEFIELDAMOUNT
            MOVE 9          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.

            COMPUTE WS-VATPRICE ROUNDED =
                 ST-PRICE  + ((ST-PRICE  * PA-GST-PERCENT)
                      / 100).
            MOVE "VATPRICE"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE WS-VATPRICE TO F-EDNAMEFIELDAMOUNT
            MOVE 9           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNT.

            IF WS-DIS-COSTS = "N"
               GO TO GET-006.
            MOVE "PU-DESC"       TO F-FIELDNAME
            MOVE 7               TO F-CBFIELDNAME.
            IF WS-QUES-MU-GP-PERC = "N"
               MOVE "M/U% S/P :" TO F-NAMEFIELD
            ELSE
               MOVE "G/P% S/P :" TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            IF WS-QUES-MU-GP-PERC = "N"
               COMPUTE WS-PERCENT ROUNDED =
                ((ST-PRICE  - ST-AVERAGECOST) / ST-AVERAGECOST) * 100
            ELSE
               COMPUTE WS-PERCENT ROUNDED =
                ((ST-PRICE - ST-AVERAGECOST) / ST-PRICE) * 100.
            MOVE "PERC"     TO F-FIELDNAME
            MOVE 4          TO F-CBFIELDNAME
            MOVE WS-PERCENT TO F-EDNAMEFIELDPERCNEG
            MOVE 7          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PERC-NEG.

            MOVE "MIN-DESC"      TO F-FIELDNAME
            MOVE 8               TO F-CBFIELDNAME.
            IF WS-QUES-MU-GP-PERC = "N"
               MOVE "M/U% - D9:" TO F-NAMEFIELD
            ELSE
               MOVE "G/P% - D9:" TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            IF WS-QUES-MU-GP-PERC = "N"
               COMPUTE WS-PERCENT ROUNDED = (((ST-PRICE - (ST-PRICE  *
                       ST-DISCOUNT9 / 100)) - ST-AVERAGECOST) 
                       / ST-AVERAGECOST) * 100
            ELSE
               COMPUTE WS-PERCENT ROUNDED = (((ST-PRICE - (ST-PRICE  *
                       ST-DISCOUNT9 / 100)) - ST-AVERAGECOST)
                       / ST-PRICE) * 100.
            MOVE "PERC-DS"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE WS-PERCENT TO F-EDNAMEFIELDPERCNEG
            MOVE 7          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PERC-NEG.

            MOVE "MIN-MU"        TO F-FIELDNAME
            MOVE 6               TO F-CBFIELDNAME.
            IF WS-QUES-MU-GP-PERC = "N"
               MOVE "MIN M/U %:" TO F-NAMEFIELD
            ELSE
               MOVE "MIN G/P %:" TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PERC-MIN"   TO F-FIELDNAME
            MOVE 8            TO F-CBFIELDNAME
            MOVE ST-MIN-PERC  TO F-EDNAMEFIELDPERC
            MOVE 6            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PERC.
            
            GO TO GET-010.
       GET-006.
            MOVE "PU-DESC"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE "     "    TO F-NAMEFIELD
            MOVE 10         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA
            
            MOVE "PERC"     TO F-FIELDNAME
            MOVE 4          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 7          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "MIN-DESC" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE "     "    TO F-NAMEFIELD
            MOVE 10         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA
            
            MOVE "PERC-DS"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 7          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-010.
            MOVE "UNITOFMEASURE"   TO F-FIELDNAME
            MOVE 13                TO F-CBFIELDNAME
            MOVE ST-UNITOFMEASURE  TO F-NAMEFIELD
            MOVE 4                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DISCOUNT1"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME
            MOVE ST-DISCOUNT1  TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "DISCOUNT2"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME
            MOVE ST-DISCOUNT2  TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "DISCOUNT3"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME
            MOVE ST-DISCOUNT3  TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "DISCOUNT4"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME
            MOVE ST-DISCOUNT4  TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "DISCOUNT5"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME
            MOVE ST-DISCOUNT5  TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "DISCOUNT6"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME
            MOVE ST-DISCOUNT6  TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "DISCOUNT7"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME
            MOVE ST-DISCOUNT7  TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "DISCOUNT8"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME
            MOVE ST-DISCOUNT8  TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "DISCOUNT9"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME
            MOVE ST-DISCOUNT9  TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-AMOUNTDIS.
       GET-011.
            MOVE "AVERAGECOST" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME.
            IF WS-DIS-COSTS = "Y"
                MOVE ST-AVERAGECOST TO F-EDNAMEFIELDAMOUNT
                MOVE 9              TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNT
            ELSE
                MOVE " " TO F-NAMEFIELD
                MOVE 9   TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.

            MOVE "LASTCOST" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME.
            IF WS-DIS-COSTS = "Y"
                MOVE ST-LASTCOST TO F-EDNAMEFIELDAMOUNT
                MOVE 9           TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-AMOUNT
            ELSE
                MOVE " " TO F-NAMEFIELD
                MOVE 9   TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA.
       GET-012.
            MOVE "BINLOCATION"   TO F-FIELDNAME
            MOVE 11              TO F-CBFIELDNAME
            MOVE ST-BINLOCATION  TO F-NAMEFIELD
            MOVE 6               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "MINBUYQTY"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME
            MOVE ST-MINBUYQTY  TO F-EDNAMEFIELDINV
            MOVE 6             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-INV.

            MOVE "DEL-DELAY"   TO F-FIELDNAME
            MOVE 9             TO F-CBFIELDNAME
            MOVE ST-DEL-DELAY  TO F-EDNAMEFIELDANAL
            MOVE 2             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "MAXIMUMLEVEL"   TO F-FIELDNAME
            MOVE 12               TO F-CBFIELDNAME
            MOVE ST-MAXIMUMLEVEL  TO F-EDNAMEFIELDINV
            MOVE 6                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-INV.

            MOVE "MINIMUMLEVEL"   TO F-FIELDNAME
            MOVE 12               TO F-CBFIELDNAME
            MOVE ST-MINIMUMLEVEL  TO F-EDNAMEFIELDINV
            MOVE 6                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-INV.
            
            MOVE 1 TO SUB-1 F-INDEX.

            MOVE "BRANCH-ALIAS"  TO F-FIELDNAME
            MOVE 12              TO F-CBFIELDNAME
            MOVE "OWN"           TO F-NAMEFIELD
            MOVE 3               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QTYONHAND"             TO F-FIELDNAME
            MOVE 9                       TO F-CBFIELDNAME
            MOVE 6                       TO F-CBFIELDLENGTH.
            IF WS-END = "D"
               MOVE "NONSTK"             TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE ST-QTYONHAND         TO F-EDNAMEFIELDINV
               PERFORM WRITE-FIELD-INV.

            MOVE "QTYONRESERVE"          TO F-FIELDNAME
            MOVE 12                      TO F-CBFIELDNAME
            MOVE 6                       TO F-CBFIELDLENGTH.
            IF WS-END = "D"
               MOVE "      "             TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE ST-QTYONRESERVE      TO F-EDNAMEFIELDINV
               PERFORM WRITE-FIELD-INV.

            MOVE "QTYONORDER"            TO F-FIELDNAME
            MOVE 10                      TO F-CBFIELDNAME
            MOVE 6                       TO F-CBFIELDLENGTH.
            IF WS-END = "D"
               MOVE "      "             TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE ST-QTYONORDER        TO F-EDNAMEFIELDINV
               PERFORM WRITE-FIELD-INV.

            MOVE "QTYONB/ORDER"          TO F-FIELDNAME
            MOVE 12                      TO F-CBFIELDNAME
            MOVE 6                       TO F-CBFIELDLENGTH.
            IF WS-END = "D"
               MOVE "      "             TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE ST-QTYONBORDER       TO F-EDNAMEFIELDINV
               PERFORM WRITE-FIELD-INV.

            MOVE 1 TO SUB-1.
            ADD 1  TO F-INDEX.
       GET-020.
            MOVE "BRANCH-ALIAS"         TO F-FIELDNAME
            MOVE 12                     TO F-CBFIELDNAME
            MOVE WS-BRANCH-NAME (SUB-1) TO F-NAMEFIELD
            MOVE 3                      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

      * IF WS-BRANCH-NOT-THERE = "XX" THEN DISPLAY THE FOLLOWING
      * RATHER THAN THE USUAL STOCK QUANTITIES.
           IF WS-BRANCH-NOT-THERE (SUB-1) = "XX"
               MOVE "QTYONHAND"    TO F-FIELDNAME
               MOVE 9              TO F-CBFIELDNAME
               MOVE 6              TO F-CBFIELDLENGTH
               MOVE "BRANCH"       TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
               
               MOVE "QTYONRESERVE" TO F-FIELDNAME
               MOVE 12             TO F-CBFIELDNAME
               MOVE 6              TO F-CBFIELDLENGTH
               MOVE "NOT   "       TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA

               MOVE "QTYONORDER"   TO F-FIELDNAME
               MOVE 10             TO F-CBFIELDNAME
               MOVE 6              TO F-CBFIELDLENGTH
               MOVE "ONLINE"       TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA

               MOVE "QTYONB/ORDER" TO F-FIELDNAME
               MOVE 12             TO F-CBFIELDNAME
               MOVE 6              TO F-CBFIELDLENGTH
               MOVE "NOW.  "       TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
               GO TO GET-030.
           
            
            MOVE "QTYONHAND"              TO F-FIELDNAME
            MOVE 9                        TO F-CBFIELDNAME
            MOVE 6                        TO F-CBFIELDLENGTH.
            IF WS-BRANCH-ONHAND (SUB-1) = 999999
               MOVE "NONSTK"              TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE WS-BRANCH-ONHAND (SUB-1) TO F-EDNAMEFIELDINV
               PERFORM WRITE-FIELD-INV.

            MOVE "QTYONRESERVE"          TO F-FIELDNAME
            MOVE 12                      TO F-CBFIELDNAME
            MOVE 6                       TO F-CBFIELDLENGTH.
            IF WS-BRANCH-ONRES (SUB-1) = 999999
               MOVE "      "              TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE WS-BRANCH-ONRES (SUB-1) TO F-EDNAMEFIELDINV
               PERFORM WRITE-FIELD-INV.

            MOVE "QTYONORDER"              TO F-FIELDNAME
            MOVE 10                        TO F-CBFIELDNAME
            MOVE 6                         TO F-CBFIELDLENGTH.
            IF WS-BRANCH-ONORDER (SUB-1) = 999999
               MOVE "      "              TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE WS-BRANCH-ONORDER (SUB-1) TO F-EDNAMEFIELDINV
               PERFORM WRITE-FIELD-INV.

            MOVE "QTYONB/ORDER"         TO F-FIELDNAME
            MOVE 12                     TO F-CBFIELDNAME
            MOVE 6                      TO F-CBFIELDLENGTH.
            IF WS-BRANCH-ONORDER (SUB-1) = 999999
               MOVE "      "              TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE WS-BRANCH-ONBO (SUB-1) TO F-EDNAMEFIELDINV
               PERFORM WRITE-FIELD-INV.
       GET-030.
            IF SUB-1 < 10
               ADD 1 TO SUB-1
                        F-INDEX.
            IF WS-BRANCH-NUMBER (SUB-1) > 0
                GO TO GET-020.

            GO TO GET-001.
       GET-999.
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
       READ-STOCK SECTION.
       R-ST-000.
            MOVE ST-STOCKNUMBER  TO WS-STOCKNUMBER
            START STOCK-MASTER KEY NOT < ST-KEY
                INVALID KEY NEXT SENTENCE.
       R-ST-010.
            READ STOCK-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE "D" TO WS-END
                PERFORM CLEAR-SCREEN-FORM          
                MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER 
                MOVE "Enter An Existing St" TO ST-DESCRIPTION1 
                MOVE "ock Item, Try Again!" TO ST-DESCRIPTION2 
                GO TO R-ST-900.
             IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK-ST1
                GO TO R-ST-010.
       R-ST-900.
             PERFORM READ-SPECIAL-PRICES.
             PERFORM ERROR-020.
             
             PERFORM READ-BRANCH-STOCK.
       R-ST-999.
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
       START-STOCK SECTION.
       ST-ST-000.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT LESS ST-STOCKNUMBER 
                INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
                PERFORM CLEAR-SCREEN-FORM
                MOVE "Y" TO WS-END.
       ST-ST-999.
             EXIT.
      *
       READ-STOCK-NEXT SECTION.
       R-ST-NX-005. 
           IF WS-END = "Y"
               MOVE "End of NEXT-PAGE seq" TO ST-DESCRIPTION1 
               MOVE "uence, RE-ENTER.    " TO ST-DESCRIPTION2 
               GO TO R-ST-NX-999.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               MOVE "END OF NEXT-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM START-STOCK
               GO TO R-ST-NX-005.
     
           IF WS-STOCK-ST1 = 0
               GO TO R-ST-NX-900
           ELSE
               MOVE "STOCK FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               PERFORM START-STOCK
               GO TO R-ST-NX-005.
       R-ST-NX-900.
             PERFORM READ-SPECIAL-PRICES.
             PERFORM ERROR-020.
             PERFORM READ-BRANCH-STOCK.
       R-ST-NX-999.
             EXIT.
      *
       READ-STOCK-PREVIOUS SECTION.
       RPREV-005. 
           IF WS-END = "Y"
               MOVE "End of NEXT-PAGE seq" TO ST-DESCRIPTION1 
               MOVE "uence, RE-ENTER.    " TO ST-DESCRIPTION2 
               GO TO RPREV-999.
           READ STOCK-MASTER PREVIOUS
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
               MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM START-STOCK
               GO TO RPREV-999.
           IF WS-STOCK-ST1 = 0
               GO TO RPREV-900
           ELSE
               MOVE "STOCK FILE BUSY ON READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               PERFORM START-STOCK
               GO TO RPREV-005.
       RPREV-900.
             PERFORM READ-SPECIAL-PRICES.
             PERFORM ERROR-020.
             PERFORM READ-BRANCH-STOCK.
       RPREV-999.
             EXIT.
      *
       READ-SPECIAL-PRICES SECTION.
       SPR-000.
           MOVE ST-STOCKNUMBER  TO STPR-STOCKNUMBER.
           START STPR-MASTER KEY NOT < STPR-KEY
               INVALID KEY NEXT SENTENCE.
       SPR-005.
           READ STPR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STPR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO STPR-PRICE
               GO TO SPR-999.
           IF WS-STPR-ST1 NOT = 0
              MOVE "SPECIAL PRICES BUSY ON READ, 'ESC' TO RETRY."
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
            OPEN I-O STPR-MASTER.
            IF WS-STPR-ST1 NOT = 0
               MOVE 0 TO WS-STPR-ST1
               MOVE "STOCK PRICE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
       OPEN-005.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
               
           PERFORM READ-PARAM-NEXT.
           PERFORM READ-INVQUES-FILE.
           PERFORM READ-PARAMETER.
           CLOSE PARAMETER-FILE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StBrchIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-MASTER
                  PARAMETER-FILE.
       END-101.
            IF WS-STOCK1 = " "
                GO TO END-102.
            CLOSE STOCK-MASTER1.
       END-102.
            IF WS-STOCK2 = " "
                GO TO END-103.
            CLOSE STOCK-MASTER2.
       END-103.
            IF WS-STOCK3 = " "
                GO TO END-104.
            CLOSE STOCK-MASTER3.
       END-104.
            IF WS-STOCK4 = " "
                GO TO END-900.
            CLOSE STOCK-MASTER4.
       END-900.
            EXIT PROGRAM.
       END-999.
           EXIT.
      *      
       Copy "ClearFormStock".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldNumeric6".
       Copy "WriteFieldPercNeg".
       Copy "WriteFieldPerc".
       Copy "WriteFieldInv".
       Copy "WriteFieldNumDec".
       Copy "WriteFieldPrice".
       Copy "WriteFieldQty".
       Copy "WriteFieldDate".
       Copy "WriteFieldDuty".
       Copy "WriteFieldForeign".
       Copy "WriteFieldTariff".
       Copy "WriteFieldAmount".
       Copy "WriteFieldAmountDis".
       Copy "WriteFieldSale".
       Copy "WriteFieldNumber".
       Copy "WriteFieldValue".
       Copy "ReadKBD".
       Copy "StockSpecPassword".
       Copy "CTOSCobolAccept".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "ConvertDateFormat".
       Copy "DecimaliseRate".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "Error4Message".
      *
      * END-OF-JOB
