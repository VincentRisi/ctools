        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlInGpRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlMaster".
        Copy "SelectGlParameter".
        Copy "SelectCoCompany".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdGlMast.
           COPY ChlfdGlParam.
           COPY ChlfdCompany.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  LINE-CNT                  PIC 9(3) VALUE 66.
       77  WS-PRINTANSWER            PIC X(10) VALUE " ".
       77  WS-GROUP                  PIC X VALUE " ".
       77  WS-GLPARAMETER-SAVE       PIC X(40) VALUE " ".
       77  WS-GLMASTER-SAVE          PIC X(40) VALUE " ".
       77  WS-BARE-GLNUMBER          PIC X(12) VALUE " ".
       77  WS-BARE-GLPARAM           PIC X(12) VALUE " ".
       77  PAGE-CNT                  PIC 9(3) VALUE 0.
       77  WS-FOLDER-NUM             PIC 9 VALUE 0.
       77  WS-NEXT-NUMBER            PIC X(12) VALUE " ".
       77  WS-MTD-COST               PIC S9(8)V99 VALUE 0.
       77  WS-YTD-COST               PIC S9(8)V99 VALUE 0.
       77  WS-MTD-SALES              PIC S9(8)V99 VALUE 0.
       77  WS-YTD-SALES              PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-MV-PTD          PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-BU-PTD          PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-MV-YTD          PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-BU-YTD          PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-MV-LAST-YTD     PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-MV-LAST-TOTAL   PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-BU-TOTAL        PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-PERC            PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN1                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN2                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN3                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN4                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN5                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN6                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN7                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN1-SALES          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN2-SALES          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN3-SALES          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN4-SALES          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN5-SALES          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN6-SALES          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN7-SALES          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN1-COSTS          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN2-COSTS          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN3-COSTS          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN4-COSTS          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN5-COSTS          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN6-COSTS          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN7-COSTS          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN1-EXP            PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN2-EXP            PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN3-EXP            PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN4-EXP            PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN5-EXP            PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN6-EXP            PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN7-EXP            PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN1-NETT           PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN2-NETT           PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN3-NETT           PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN4-NETT           PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN5-NETT           PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN6-NETT           PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN7-NETT           PIC S9(8)V99 VALUE 0.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1      PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  WS-MENU-STATUS.
           03  WS-MENU-ST1        PIC 99.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-HEADER      PIC X(2).
               05  WS-SUB         PIC X(4).
           03  WS-REST            PIC X(6).
       01  WS-DATE-SPLIT.
           03  WS-DATE-CC         PIC 99.
           03  WS-DATE-YY         PIC 99.
       01  COMPANIES.
           03  COM-NUM            PIC Z9.
           03  FILLER             PIC X(2) VALUE ". ".
           03  COM-NAME           PIC X(42).
           03  COM-USED           PIC X(15).
       01  COMPANIES-LIST-NAMES.
         02  COMPANIES-LIST OCCURS 20.
           03  LIST-GROUP         PIC X.
           03  LIST-NAME          PIC X(40).
           03  LIST-NUMBER        PIC 99.
           03  LIST-VOL-DIR       PIC X(25).
           03  LIST-NOT-THERE     PIC X.
       01  WS-BRANCH-INFO-NAMES.
         02  WS-BRANCH-INFO OCCURS 20.
           03  WS-BRANCH-TYPE          PIC 9.
           03  WS-BRANCH-NUMBER        PIC 9.
           03  WS-BRANCH-NOT-THERE     PIC X(2).
           03  WS-BRANCH-NAME          PIC X(3).
           03  WS-BRANCH-GL-VOL-DIR    PIC X(40).
       01 GROUP-LINES-NAMES.
         02 GROUP-LINES OCCURS 50.
           03  WS-GL-ACCOUNT     PIC X(12).
           03  WS-GL-DESC        PIC X(40).
           03  WS-MV-PTD         PIC S9(8)V99.
           03  WS-BU-PTD         PIC S9(8)V99.
           03  WS-MV-YTD         PIC S9(8)V99.
           03  WS-BU-YTD         PIC S9(8)V99.
           03  WS-MV-LAST-YTD    PIC S9(8)V99.
           03  WS-MV-LAST        PIC S9(8)V99.
           03  WS-BU             PIC S9(8)V99.
           03  WS-PERC           PIC S9(8)V99.
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(18) VALUE " ".
           03  FILLER         PIC X(50) VALUE
           "GENERAL LEDGER SUMMARY INCOME STATEMENT - GROUP = ".
           03  H1-GROUP       PIC X.
           03  FILLER         PIC X(4) VALUE " ".
           03  FILLER         PIC X(7) VALUE "PERIOD:".
           03  H1-PERIOD      PIC X(3) VALUE " ".
           03  H1-PER-BEG     PIC X(10).
           03  FILLER         PIC X(4) VALUE " TO ".
           03  H1-PER-END     PIC X(10).
           03  FILLER         PIC X(2) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(1) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(41) VALUE " ".
           03  FILLER         PIC X(47) VALUE
           "Movement      Budget    Movement      Budget".
           03  FILLER         PIC X(44) VALUE
           "Last Year   Last Year      Budget      Move".
       01  HEAD3-1.
           03  FILLER         PIC X(46) VALUE " ".
           03  FILLER         PIC X(22) VALUE "Ptd         Ptd   Ytd".
           03  H3-1BEG        PIC X(2).
           03  FILLER         PIC X VALUE "/".
           03  H3-1END        PIC X(5).
           03  FILLER         PIC X(4) VALUE "Ytd".
           03  H3-2BEG        PIC X(2).
           03  FILLER         PIC X VALUE "/".
           03  H3-2END        PIC X(5).
           03  FILLER         PIC X(4) VALUE "Ytd ".
           03  H3-3BEG        PIC X(2).
           03  FILLER         PIC X VALUE "/".
           03  H3-3END        PIC X(9).
           03  FILLER         PIC X(12) VALUE "Total".
           03  H3-4BEG        PIC X(2).
           03  FILLER         PIC X VALUE "/".
           03  H3-4END        PIC X(9).
           03  FILLER         PIC X VALUE "%".
       01  DETAIL-LINE.
           03  D-NAME.
              05  D-NAME1         PIC X(22).
              05  D-NAMEPIC1      PIC X.
              05  D-NAMEPERC1     PIC Z(1)9.99.
              05  D-NAMEPIC2      PIC X(3).
              05  D-NAMEPIC3      PIC X.
              05  D-NAMEPERC2     PIC Z(1)9.99.
              05  D-NAMEPIC4      PIC X.
           03  D-MV-PTD       PIC Z(7)9.99-.
           03  D-BU-PTD       PIC Z(7)9.99-.
           03  D-MV-YTD       PIC Z(7)9.99-.
           03  D-BU-YTD       PIC Z(7)9.99-.
           03  D-MV-LAST-YTD  PIC Z(7)9.99-.
           03  D-MV-LAST      PIC Z(7)9.99-.
           03  D-BU           PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-PERC         PIC Z(3)9.99-.
       01  UNDER-LINE.
           03  FILLER            PIC X(38) VALUE " ".
           03  FILLER            PIC X(12) VALUE "-----------".
           03  FILLER            PIC X(12) VALUE "-----------".
           03  FILLER            PIC X(12) VALUE "-----------".
           03  FILLER            PIC X(12) VALUE "-----------".
           03  FILLER            PIC X(12) VALUE "-----------".
           03  FILLER            PIC X(12) VALUE "-----------".
           03  FILLER            PIC X(14) VALUE "-----------".
           03  FILLER            PIC X(8) VALUE  "-------".
       01  DOUBLE-UNDER-LINE.
           03  FILLER            PIC X(38) VALUE " ".
           03  FILLER            PIC X(12) VALUE "===========".
           03  FILLER            PIC X(12) VALUE "===========".
           03  FILLER            PIC X(12) VALUE "===========".
           03  FILLER            PIC X(12) VALUE "===========".
           03  FILLER            PIC X(12) VALUE "===========".
           03  FILLER            PIC X(12) VALUE "===========".
           03  FILLER            PIC X(14) VALUE "===========".
           03  FILLER            PIC X(8) VALUE  "=======".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 321 TO POS.
           DISPLAY "** SUMMARY INCOME STATEMENT BY GROUP **" AT POS.
           MOVE 421 TO POS.           
           DISPLAY "***************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM OPEN-FILES.
           CLOSE GL-MASTER.
           
           MOVE WS-GlMaster    to Ws-GlMaster-Save
           MOVE WS-GlParameter to Ws-GlParameter-Save.
           
           PERFORM ENTER-GROUP.
           
           Move 3010 to Pos
           Display "Reading Companies into Memory........" At Pos.
           Perform Read-Next-Company.

           Move 3010 to Pos
           Display "Getting Group Param Files........... " at Pos.
           Perform Check-Group-Parameter-Names
           Perform Error-020.
       CONTROL-020.
           PERFORM CHECK-COMPANIES-IN-GROUP.

           PERFORM PRINT-ROUTINE.
           PERFORM ERROR-020
           MOVE 2610 TO POS
           DISPLAY WS-MESSAGE AT POS.
           
           Move 2610 to Pos
           Display "Opening Home Office Files........" At Pos.

           MOVE WS-GlMaster-Save    to Ws-GlMaster
           MOVE WS-GlParameter-Save to Ws-GlParameter.
           
           Perform End-Off.
       CONTROL-999.
           EXIT.
      *
       CHECK-COMPANIES-IN-GROUP SECTION.
       CCIG-005.
           MOVE 1 TO SUB-20.
       CCIG-010.
           IF WS-BRANCH-GL-VOL-DIR (SUB-20) = " "
              GO TO CCIG-999.
              
           PERFORM CDS-005
           MOVE WS-BRANCH-GL-VOL-DIR (SUB-20) TO ALPHA-RATE              
           PERFORM OPEN-NODE-FILE.
              
           IF WS-GROUP = "A"
               GO TO CCIG-030.
              
           Move 3010 to Pos
           Display "Checking If branch Is In Group...... " at Pos.

           IF LIST-NOT-THERE (SUB-20) = "N"
               GO TO CCIG-025.
           MOVE WS-BRANCH-GL-VOL-DIR (SUB-20) TO WS-GLPARAMETER
           PERFORM OPEN-005
           PERFORM READ-PARAMETER
           CLOSE GLPARAMETER-FILE.
        
           MOVE GLPA-GROUP-NUM TO LIST-GROUP (SUB-20).
           IF GLPA-GROUP-NUM = WS-GROUP
              GO TO CCIG-030.
              
           MOVE "Z" TO LIST-NOT-THERE (SUB-20).
       CCIG-025.
           PERFORM DISPLAY-COMPANY-NAME.
           IF SUB-20 < 20
              ADD 1 TO SUB-20
              GO TO CCIG-010
           ELSE
              GO TO CCIG-999.
       CCIG-030.
           IF LIST-NOT-THERE (SUB-20) NOT = " "
               PERFORM DISPLAY-COMPANY-NAME
               GO TO CCIG-040.
           PERFORM ERROR1-020.
           PERFORM ERROR-020.
           Move 2910 to Pos
           Display "Getting Branch File Names........... " at Pos.
           Perform Check-Branch-Data-Names
       
           MOVE WS-BRANCH-GL-VOL-DIR (SUB-20) TO WS-GLMASTER.
           PERFORM OPEN-000.

           PERFORM DISPLAY-COMPANY-NAME.
      
           PERFORM ERROR1-020.

           PERFORM PRINT-TO-MEMORY.
           CLOSE GL-MASTER.
       CCIG-040.
           IF SUB-20 < 20
              ADD 1 TO SUB-20
              GO TO CCIG-010.

           Perform Error-020.
       CCIG-999.
           EXIT.
      *
       CHECK-DATA-SIZE SECTION.         
       CDS-005.
           Move " " To Alpha-Rate.
           Move 0   To SUB-2.
       CDS-015.
           Add 1 To SUB-2.
           If Al-Rate (SUB-2) Not = " "
            If SUB-2 Not > 60
            Go To CDS-015.
          Subtract 1 from SUB-2.
       CDS-999.
          EXIT.
      *
       OPEN-NODE-FILE SECTION.
       ONF-001.
           PERFORM CDS-015.
           MOVE WS-BRANCH-GL-VOL-DIR (SUB-20) TO F-FILENAME
           MOVE SUB-2           TO F-CBFILENAME.
           CALL "OPENFILE" USING   F-ERROR5
                                   F-FH
                                   F-FILENAME
                                   F-CBFILENAME
                                   F-FILENAME
                                   F-INTEGERZERO
                                   F-OPENMODE.
                                    
           IF F-ERROR5 NOT = 0
            IF F-ERROR5 NOT = 220
               MOVE "N" TO WS-BRANCH-NOT-THERE (SUB-20)
                                LIST-NOT-THERE (SUB-20)
               GO TO ONF-999.

           IF F-ERROR5 NOT = 210
               CALL "CLOSEFILE" USING  F-ERROR5
                                       F-FH.
           MOVE "  " TO WS-BRANCH-NOT-THERE (SUB-20)
                             LIST-NOT-THERE (SUB-20).
       ONF-999.
           EXIT.
      * 
       DISPLAY-COMPANY-NAME SECTION.
       DCNAD-005.
           MOVE 0714 TO POS
           DISPLAY "Company Name" AT POS
           ADD 42 TO POS
           DISPLAY "Used" AT POS.
       DCNAD-010.
           MOVE LIST-NUMBER (SUB-20)    TO COM-NUM
           MOVE LIST-NAME (SUB-20)      TO COM-NAME.
           IF LIST-NOT-THERE (SUB-20) = "N"
              MOVE "Link Down"          TO COM-USED.
           IF LIST-NOT-THERE (SUB-20) = " "
              MOVE "Yes"                TO COM-USED.
           IF LIST-NOT-THERE (SUB-20) = "Z"
              MOVE "Omitted"            TO COM-USED.
           
           IF SUB-20 = 1
               MOVE 0810 TO POS.
           IF SUB-20 = 2
               MOVE 0910 TO POS.
           IF SUB-20 = 3
               MOVE 1010 TO POS.
           IF SUB-20 = 4
               MOVE 1110 TO POS.
           IF SUB-20 = 5
               MOVE 1210 TO POS.
           IF SUB-20 = 6
               MOVE 1310 TO POS.
           IF SUB-20 = 7
               MOVE 1410 TO POS.
           IF SUB-20 = 8
               MOVE 1510 TO POS.
           IF SUB-20 = 9
               MOVE 1610 TO POS.
           IF SUB-20 = 10
               MOVE 1710 TO POS.
           IF SUB-20 = 11
               MOVE 1810 TO POS.
           IF SUB-20 = 12
               MOVE 1910 TO POS.
           IF SUB-20 = 13
               MOVE 2010 TO POS.
           IF SUB-20 = 14
               MOVE 2110 TO POS.
           IF SUB-20 = 15
               MOVE 2210 TO POS.
           IF SUB-20 = 16
               MOVE 2310 TO POS.
           IF SUB-20 = 17
               MOVE 2410 TO POS.
           IF SUB-20 = 18
               MOVE 2510 TO POS.
           IF SUB-20 = 19
               MOVE 2610 TO POS.
           IF SUB-20 = 20
               MOVE 2910 TO POS.
               
           DISPLAY COMPANIES AT POS.
       DCNAD-999.
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
               MOVE "COMPANY FILE BUSY ON READ-NEXT, GOING TO RE-TRY."
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
       Check-Group-Parameter-Names Section.
       CGDN-005.
          MOVE 1 TO SUB-20.
          PERFORM STRIP-GLPARAM.
       CGDN-006.
          Move " " To Alpha-Rate
                      Data-Rate.
          Move 0   To Sub-1.
       CGDN-010.
          Move List-Vol-Dir (SUB-20) To Alpha-Rate.
       CGDN-015.
          Add 1 To Sub-1.
          If Sub-1 Not > 60
           If Al-Rate (SUB-1) Not = " "
            Go To CGDN-015.
       CGDN-020.
          Move Ws-Bare-GLPARAM to Data-Rate.
          Move 1               To Sub-2.
       CGDN-025.
          Move Dat-Rate (Sub-2) To Al-Rate (SUB-1)
          Add 1 To Sub-1 Sub-2.
          If Dat-Rate (Sub-2) Not = " "
           If Sub-1 Not > 60
             Go To CGDN-025.
       CGDN-030.
          Move Alpha-Rate To WS-BRANCH-GL-VOL-DIR (SUB-20).

          IF SUB-20 < 20
             ADD 1 TO SUB-20.
          IF LIST-NUMBER (SUB-20) > 0
              GO TO CGDN-006.
       CGDN-999.
          Exit.
      * 
       STRIP-GLPARAM SECTION.
       STRIP-PAR-000.
      * WS-FOLDER-NUM IS USED TO WIORK OUT WHICH / HAS BEEN REACHED
      * IN ASCERTAINING THE FOLDER NAMES. E.G /MAIN/DATA01/
      * IN CTOS WE USED <DATA> SO WE ONLY HAD TO LOOK FOR THE >.
          MOVE 0 TO SUB-2 WS-FOLDER-NUM.
          MOVE 1 TO SUB-1.
          Move " " To Alpha-Rate
                      DATA-RATE.
          MOVE WS-GLPARAMETER TO ALPHA-RATE.
       STRIP-PAR-015.
          Add 1 To Sub-2.
          If SUB-2 Not > 60
           If Al-Rate (SUB-2) Not = "/"
            Go To STRIP-PAR-015.
          If SUB-2 Not > 60
           If Al-Rate (SUB-2) = "/"
            IF WS-FOLDER-NUM NOT = 2
               ADD 1 TO WS-FOLDER-NUM
            Go To STRIP-PAR-015.
           ADD 1 TO SUB-2.
       STRIP-PAR-020.
           MOVE AL-RATE (SUB-2) TO DAT-RATE (SUB-1).
           ADD 1 TO SUB-2 SUB-1.
           IF AL-RATE (SUB-2) NOT = " "
              GO TO STRIP-PAR-020.
       STRIP-PAR-030.
           MOVE DATA-RATE TO WS-BARE-GLPARAM.
       STRIP-PAR-999.
           EXIT.
      *     
       Check-Branch-Data-Names Section.
       CBDN-005.
          PERFORM STRIP-GLNAME.
       CBDN-006.
          Move " " To Alpha-Rate
                      Data-Rate.
          Move 0   To Sub-1.
       CBDN-010.
          Move List-Vol-Dir (SUB-20) To Alpha-Rate.
       CBDN-015.
          Add 1 To Sub-1.
          If Sub-1 Not > 60
           If Al-Rate (SUB-1) Not = " "
            Go To CBDN-015.
       CBDN-020.
          Move Ws-Bare-GLNUMBER to Data-Rate.
          Move 1                To Sub-2.
       CBDN-025.
          Move Dat-Rate (Sub-2) To Al-Rate (SUB-1)
          Add 1 To Sub-1 Sub-2.
          If Dat-Rate (Sub-2) Not = " "
           If Sub-1 Not > 60
             Go To CBDN-025.
       CBDN-030.
          Move Alpha-Rate To WS-BRANCH-GL-VOL-DIR (SUB-20).
       CBDN-999.
          Exit.
      * 
       STRIP-GLNAME SECTION.
       STRIP-000.
          MOVE 0 TO SUB-2 WS-FOLDER-NUM.
          MOVE 1 TO SUB-1.
          Move " " To Alpha-Rate
                      DATA-RATE.
          MOVE WS-GLMASTER TO ALPHA-RATE.
       STRIP-015.
          Add 1 To Sub-2.
          If SUB-2 Not > 60
           If Al-Rate (SUB-2) Not = "/"
            Go To STRIP-015.
          If SUB-2 Not > 60
           If Al-Rate (SUB-2) = "/"
            IF WS-FOLDER-NUM NOT = 2
               ADD 1 TO WS-FOLDER-NUM
            Go To STRIP-015.
           ADD 1 TO SUB-2.
       STRIP-020.
           MOVE AL-RATE (SUB-2) TO DAT-RATE (SUB-1).
           ADD 1 TO SUB-2 SUB-1.
           IF AL-RATE (SUB-2) NOT = " "
              GO TO STRIP-020.
       STRIP-030.
           MOVE DATA-RATE TO WS-BARE-GLNUMBER.
       STRIP-999.
           EXIT.
      *
       ENTER-GROUP SECTION.
       EG-010.
           MOVE " " TO WS-GROUP.
           MOVE 0715 TO POS.
           DISPLAY "ENTER 'A' FOR ALL GROUP COMPANIES." AT POS.
           MOVE 0615 TO POS.
           DISPLAY "ENTER THE GROUP NUMBER TO PRINT:[ ]" AT POS.
           MOVE 0648 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 3         TO CDA-ROW.
           MOVE 47        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-GROUP.

           IF WS-GROUP NOT > " " 
              MOVE "GROUP NUMBER MUST BE > SPACES" TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO EG-010.
           IF W-ESCAPE-KEY = 4
               PERFORM END-OFF.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO EG-900
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO EG-010.
       EG-900.
           MOVE 0715 TO POS.
           DISPLAY "                                      " AT POS.
       EG-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
            IF SUB-1 = 1
                MOVE "50-300-05-00" TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 2
                MOVE "50-200-05-00" TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 3
                MOVE "50-200-08-00" TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 4
                MOVE "50-200-10-00" TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 5
                MOVE "50-200-15-00" TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 6
                MOVE "TOTAL COST OF SALES" TO D-NAME
                PERFORM COMPUTE-PERC
                MOVE WS-COLUMN1            TO D-MV-PTD
                                             WS-MTD-COST
                MOVE WS-COLUMN2            TO D-BU-PTD
                MOVE WS-COLUMN3            TO D-MV-YTD
                                             WS-YTD-COST
                MOVE WS-COLUMN4            TO D-BU-YTD
                MOVE WS-COLUMN5            TO D-MV-LAST-YTD
                MOVE WS-COLUMN6            TO D-MV-LAST
                MOVE WS-COLUMN7            TO D-BU
                COMPUTE WS-MARGIN-PERC ROUNDED =
                 ((WS-COLUMN3 - WS-COLUMN5) / WS-COLUMN5) * 100
                MOVE WS-MARGIN-PERC        TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE WS-COLUMN1 TO WS-COLUMN1-COSTS
                MOVE WS-COLUMN2 TO WS-COLUMN2-COSTS
                MOVE WS-COLUMN3 TO WS-COLUMN3-COSTS
                MOVE WS-COLUMN4 TO WS-COLUMN4-COSTS
                MOVE WS-COLUMN5 TO WS-COLUMN5-COSTS
                MOVE WS-COLUMN6 TO WS-COLUMN6-COSTS
                MOVE WS-COLUMN7 TO WS-COLUMN7-COSTS
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                MOVE "60-200      "   TO WS-NEXT-NUMBER
                MOVE GL-PER (SUB-4)   TO WS-MTD-SALES
                MOVE WS-MARGIN-MV-YTD TO WS-YTD-SALES
                MOVE WS-MV-PTD (SUB-1)      TO WS-COLUMN1-SALES
                MOVE WS-BU-PTD (SUB-1)      TO WS-COLUMN2-SALES
                MOVE WS-MV-YTD (SUB-1)      TO WS-COLUMN3-SALES
                MOVE WS-BU-YTD (SUB-1)      TO WS-COLUMN4-SALES
                MOVE WS-MV-LAST-YTD (SUB-1) TO WS-COLUMN5-SALES
                MOVE WS-MV-LAST (SUB-1)     TO WS-COLUMN6-SALES
                MOVE WS-BU (SUB-1)          TO WS-COLUMN7-SALES
                GO TO GET-999.
            IF SUB-1 = 7
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                PERFORM COMPUTE-GP-PERC
                MOVE "GROSS PROFIT(-)/LOSS" TO D-NAME1
                MOVE "M"                    TO D-NAMEPIC1
                MOVE "Y"                    TO D-NAMEPIC3
                MOVE "%"                    TO D-NAMEPIC2
                                               D-NAMEPIC4
                COMPUTE WS-MARGIN-PERC ROUNDED = 
                   ((WS-COLUMN1 / WS-MTD-COST) * 100)
                MOVE WS-MARGIN-PERC         TO D-NAMEPERC1
                COMPUTE WS-MARGIN-PERC ROUNDED =
                   ((WS-COLUMN3 / WS-YTD-COST) * 100)
                MOVE WS-MARGIN-PERC         TO D-NAMEPERC2

                MOVE WS-COLUMN1             TO D-MV-PTD
                MOVE WS-COLUMN2             TO D-BU-PTD
                MOVE WS-COLUMN3             TO D-MV-YTD
                MOVE WS-COLUMN4             TO D-BU-YTD
                MOVE WS-COLUMN5             TO D-MV-LAST-YTD
                MOVE WS-COLUMN6             TO D-MV-LAST
                MOVE WS-COLUMN7             TO D-BU
                COMPUTE WS-MARGIN-PERC ROUNDED = 
                  ((WS-COLUMN3 - WS-COLUMN5) / WS-COLUMN5) * 100
                MOVE WS-MARGIN-PERC TO D-PERC

                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                MOVE 0          TO WS-COLUMN1
                                   WS-COLUMN2
                                   WS-COLUMN3
                                   WS-COLUMN4
                                   WS-COLUMN5
                                   WS-COLUMN6
                                   WS-COLUMN7
                MOVE "50-010      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 8
                MOVE "50-020      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 9
                MOVE "50-025      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 10
                MOVE "50-030      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 11
                MOVE "50-035      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 12
                MOVE "50-040      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 13
                MOVE "50-045      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 14
                MOVE "50-050      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 15
                MOVE "50-090      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 16
                MOVE "50-052      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 17
                MOVE "50-055      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 18
                MOVE "50-060      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 19
                MOVE "50-070      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 20
                MOVE "50-078      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 21
                MOVE "50-075      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 22
                MOVE "50-080      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 23
                MOVE "50-082      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 24
                MOVE "50-084      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 25
                MOVE "50-085      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 26
                MOVE "50-092      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 27
                MOVE "50-095      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 28
                MOVE "50-100      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 29
                MOVE "50-106      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 30
                MOVE "50-110      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 31
                MOVE "50-400      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 32
                MOVE "50-076      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 33
                MOVE "50-115      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 34
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                MOVE "TOTAL EXPENSES    " TO D-NAME
                PERFORM COMPUTE-COST-PERC
                MOVE WS-COLUMN1           TO D-MV-PTD
                MOVE WS-COLUMN2           TO D-BU-PTD
                MOVE WS-COLUMN3           TO D-MV-YTD
                MOVE WS-COLUMN4           TO D-BU-YTD
                MOVE WS-COLUMN5           TO D-MV-LAST-YTD
                MOVE WS-COLUMN6           TO D-MV-LAST
                MOVE WS-COLUMN7           TO D-BU
                COMPUTE WS-MARGIN-PERC ROUNDED =
                 ((WS-COLUMN3 - WS-COLUMN5) / WS-COLUMN5) * 100
                MOVE WS-MARGIN-PERC TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                MOVE "NETT OPERATING PROFIT(-)/LOSS" TO D-NAME
                MOVE WS-COLUMN1 TO WS-COLUMN1-EXP
                MOVE WS-COLUMN2 TO WS-COLUMN2-EXP
                MOVE WS-COLUMN3 TO WS-COLUMN3-EXP
                MOVE WS-COLUMN4 TO WS-COLUMN4-EXP
                MOVE WS-COLUMN5 TO WS-COLUMN5-EXP
                MOVE WS-COLUMN6 TO WS-COLUMN6-EXP
                MOVE WS-COLUMN7 TO WS-COLUMN7-EXP

                PERFORM COMPUTE-NETT-PERC
      
      *          MOVE WS-COLUMN1-SALES TO D-MV-PTD      WS-COLUMN1
      *          MOVE WS-COLUMN2-SALES TO D-BU-PTD      WS-COLUMN2
      *          MOVE WS-COLUMN3-SALES TO D-MV-YTD      WS-COLUMN3
      *          MOVE WS-COLUMN4-SALES TO D-BU-YTD      WS-COLUMN4
      *          MOVE WS-COLUMN5-SALES TO D-MV-LAST-YTD WS-COLUMN5
      *          MOVE WS-COLUMN6-SALES TO D-MV-LAST     WS-COLUMN6
      *          MOVE WS-COLUMN7-SALES TO D-BU          WS-COLUMN7
                MOVE WS-COLUMN1           TO D-MV-PTD
                MOVE WS-COLUMN2           TO D-BU-PTD
                MOVE WS-COLUMN3           TO D-MV-YTD
                MOVE WS-COLUMN4           TO D-BU-YTD
                MOVE WS-COLUMN5           TO D-MV-LAST-YTD
                MOVE WS-COLUMN6           TO D-MV-LAST
                MOVE WS-COLUMN7           TO D-BU
      *          COMPUTE WS-MARGIN-PERC ROUNDED =
      *            ((WS-COLUMN3-SALES - WS-COLUMN5-SALES)
      *               / WS-COLUMN5-SALES) * 100
                COMPUTE WS-MARGIN-PERC ROUNDED =
                  ((WS-COLUMN3 - WS-COLUMN5) / WS-COLUMN5) * 100
                MOVE WS-MARGIN-PERC                  TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                MOVE "60-015      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 35
                MOVE "60-300      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 36
                MOVE "60-400      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 37
                MOVE "55-010      " TO WS-NEXT-NUMBER
                GO TO GET-999.
            IF SUB-1 = 38
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC UNDER-LINE
                MOVE "PROFIT(-)/LOSS NOT POSTED" TO D-NAME

                PERFORM COMPUTE-PROFIT
      
                MOVE WS-COLUMN1                  TO D-MV-PTD
                MOVE WS-COLUMN2                  TO D-BU-PTD
                MOVE WS-COLUMN3                  TO D-MV-YTD
                MOVE WS-COLUMN4                  TO D-BU-YTD
                MOVE WS-COLUMN5                  TO D-MV-LAST-YTD
                MOVE WS-COLUMN6                  TO D-MV-LAST
                MOVE WS-COLUMN7                  TO D-BU
                COMPUTE WS-MARGIN-PERC ROUNDED =
                 ((WS-COLUMN3 - WS-COLUMN5) / WS-COLUMN5) * 100
                MOVE WS-MARGIN-PERC TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC FROM DOUBLE-UNDER-LINE
                MOVE " " TO PRINT-REC DOUBLE-UNDER-LINE.
             MOVE 99 TO SUB-1.
      *      PERFORM END-OFF.
       GET-999.
            EXIT.
      *
       GET-MEMORY-DATA SECTION.
       GET-MEM-000.
            IF SUB-1 = 1
                MOVE "50-300-05-00" TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 2
                MOVE "50-200-05-00" TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 3
                MOVE "50-200-08-00" TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 4
                MOVE "50-200-10-00" TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 5
                MOVE "50-200-15-00" TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 6
                MOVE "60-200      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 7
                MOVE "50-010      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 8
                MOVE "50-020      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 9
                MOVE "50-025      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 10
                MOVE "50-030      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 11
                MOVE "50-035      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 12
                MOVE "50-040      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 13
                MOVE "50-045      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 14
                MOVE "50-050      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 15
                MOVE "50-090      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 16
                MOVE "50-052      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 17
                MOVE "50-055      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 18
                MOVE "50-060      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 19
                MOVE "50-070      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 20
                MOVE "50-078      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 21
                MOVE "50-075      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 22
                MOVE "50-080      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 23
                MOVE "50-082      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 24
                MOVE "50-084      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 25
                MOVE "50-085      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 26
                MOVE "50-092      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 27
                MOVE "50-095      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 28
                MOVE "50-100      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 29
                MOVE "50-106      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 30
                MOVE "50-110      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 31
                MOVE "50-400      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 32
                MOVE "50-076      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 33
                MOVE "50-115      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 34
                MOVE "60-015      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 35
                MOVE "60-300      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 36
                MOVE "60-400      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 = 37
                MOVE "55-010      " TO WS-NEXT-NUMBER
                GO TO GET-MEM-999.
            IF SUB-1 > 37
               GO TO GET-MEM-999.
       GET-MEM-999.
            EXIT.
      *
       PRINT-TO-MEMORY SECTION.
       PRTM-000.
      * SUB-20 = COMPANY NUMBER BEING PROCESSED
            MOVE 1 TO SUB-1.
            MOVE 2910 TO POS.
            DISPLAY "The Report is being compiled to Memory....."
             AT POS.
       PRTM-001.
            PERFORM GET-MEMORY-DATA.
            IF SUB-1 > 37
                GO TO PRTM-999.
            
            MOVE WS-NEXT-NUMBER TO GL-NUMBER.
            START GL-MASTER KEY NOT < GL-KEY
              INVALID KEY NEXT SENTENCE.
       PRTM-002.
            READ GL-MASTER NEXT 
               AT END NEXT SENTENCE.
            IF WS-GLMAST-ST1 = 10
               MOVE 0 TO WS-GLMAST-ST1
               ADD 1 TO SUB-1
               GO TO PRTM-001.
            IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLMASTER FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-MENU-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-MENU-ST1
               GO TO PRTM-002.
            IF GL-NUMBER < WS-NEXT-NUMBER
               GO TO PRTM-002.
            IF GL-NUMBER > WS-NEXT-NUMBER
               GO TO PRTM-060.
       PRTM-020.
           PERFORM COMPUTE-TEMP-FIGURES.
           
      * 1ST COMPANY           
           IF SUB-20 = 1
               MOVE GL-NUMBER               TO WS-GL-ACCOUNT (SUB-1)
               MOVE GL-DESCRIPTION          TO WS-GL-DESC (SUB-1)
               MOVE GL-PER (SUB-4)          TO WS-MV-PTD (SUB-1)
               MOVE GL-PER-BU (SUB-4)       TO WS-BU-PTD (SUB-1)
               MOVE WS-MARGIN-MV-YTD        TO WS-MV-YTD (SUB-1)
               MOVE WS-MARGIN-BU-YTD        TO WS-BU-YTD (SUB-1)
               MOVE WS-MARGIN-MV-LAST-YTD   TO WS-MV-LAST-YTD (SUB-1)
               MOVE WS-MARGIN-MV-LAST-TOTAL TO WS-MV-LAST (SUB-1)
               MOVE WS-MARGIN-BU-TOTAL      TO WS-BU (SUB-1)
               MOVE WS-MARGIN-PERC          TO WS-PERC (SUB-1)
               GO TO PRTM-050.
               
      * 2ND AND SUBSEQUENT COMPANIES
           IF SUB-20 > 1
            IF GL-NUMBER = WS-GL-ACCOUNT (SUB-1)
               ADD GL-PER (SUB-4)           TO WS-MV-PTD (SUB-1)
               ADD GL-PER-BU (SUB-4)        TO WS-BU-PTD (SUB-1)
               ADD WS-MARGIN-MV-YTD         TO WS-MV-YTD (SUB-1)
               ADD WS-MARGIN-BU-YTD         TO WS-BU-YTD (SUB-1)
               ADD WS-MARGIN-MV-LAST-YTD    TO WS-MV-LAST-YTD (SUB-1)
               ADD WS-MARGIN-MV-LAST-TOTAL  TO WS-MV-LAST (SUB-1)
               ADD WS-MARGIN-BU-TOTAL       TO WS-BU (SUB-1)
               ADD WS-MARGIN-PERC           TO WS-PERC (SUB-1)
               GO TO PRTM-050.
      *****************************************************************
      * THIS NEXT SECTION ADDED INCASE ONE RUNS A REPORT FOR SAY      *
      * COMPANIES 9 THRU 12 OMITTING 1 THRU 8. SAY GROUP 2.           *
      * THE WS-GL INFO WOULD ALL BE BLANK AS NO ACCOUNT INFO HAS BEEN *
      * MOVED FOR SAY COMPANY 9 AS 1-8 WERE SKIPPED AS THEY WERE IN   *
      * A DIFFERENT GROUP.                                            *
      *****************************************************************
           IF SUB-20 > 1
            IF WS-GL-ACCOUNT (SUB-1) = " "
               MOVE GL-NUMBER               TO WS-GL-ACCOUNT (SUB-1)
               MOVE GL-DESCRIPTION          TO WS-GL-DESC (SUB-1)
               MOVE GL-PER (SUB-4)          TO WS-MV-PTD (SUB-1)
               MOVE GL-PER-BU (SUB-4)       TO WS-BU-PTD (SUB-1)
               MOVE WS-MARGIN-MV-YTD        TO WS-MV-YTD (SUB-1)
               MOVE WS-MARGIN-BU-YTD        TO WS-BU-YTD (SUB-1)
               MOVE WS-MARGIN-MV-LAST-YTD   TO WS-MV-LAST-YTD (SUB-1)
               MOVE WS-MARGIN-MV-LAST-TOTAL TO WS-MV-LAST (SUB-1)
               MOVE WS-MARGIN-BU-TOTAL      TO WS-BU (SUB-1)
               MOVE WS-MARGIN-PERC          TO WS-PERC (SUB-1)
               GO TO PRTM-050.
           IF SUB-20 > 1
            IF WS-GL-ACCOUNT (SUB-1) NOT = " "
             IF GL-NUMBER NOT = WS-GL-ACCOUNT (SUB-1)
               MOVE "WE HAVE A PROBLEM, SUB-1 NOT = ACCOUNT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE GL-NUMBER TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE SUB-1 TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GL-ACCOUNT (SUB-1) TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020.
               
           MOVE "DON'T KNOW WHAT HAPPENED, STUCK PRTM-050" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
       PRTM-050.
           MOVE 2610 TO POS
           DISPLAY "ACCOUNT PROCESSED:" AT POS
           ADD 20 TO POS
           DISPLAY GL-NUMBER AT POS.
           
           
      *     MOVE WS-MV-PTD (SUB-1) TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE WS-BU-PTD (SUB-1) TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       PRTM-060.
           ADD 1  TO SUB-1
           GO TO PRTM-001.
       PRTM-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE 1 TO SUB-1.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
            MOVE 2910 TO POS.
            DISPLAY "The Report is being compiled.........." AT POS.
       PRR-001.
           IF SUB-1 = 1
              PERFORM PRR-010.

           PERFORM GET-DATA.
           IF SUB-1 = 99
              GO TO PRR-999.
           GO TO PRR-020.
       PRR-010.
           IF LINE-CNT < 58
              GO TO PRR-020.
           ADD 1              TO PAGE-CNT
           MOVE PAGE-CNT      TO H1-PAGE
           MOVE WS-GROUP      TO H1-GROUP
           MOVE LIST-NAME (1) TO CO-NAME
           MOVE " "      TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
            WRITE PRINT-REC FROM HEAD1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD3.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD3-1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE 6 TO LINE-CNT.
       PRR-020.
           MOVE WS-GL-DESC (SUB-1)      TO D-NAME
           
      *     PERFORM COMPUTE-PERC.

           MOVE WS-MV-PTD (SUB-1)         TO D-MV-PTD
           MOVE WS-BU-PTD (SUB-1)         TO D-BU-PTD
           MOVE WS-MV-YTD (SUB-1)         TO D-MV-YTD
           MOVE WS-BU-YTD (SUB-1)         TO D-BU-YTD
           MOVE WS-MV-LAST-YTD (SUB-1)    TO D-MV-LAST-YTD
           MOVE WS-MV-LAST (SUB-1)        TO D-MV-LAST
           MOVE WS-BU (SUB-1)             TO D-BU.
           COMPUTE WS-MARGIN-PERC ROUNDED =
             ((WS-MV-YTD (SUB-1) - WS-MV-LAST-YTD (SUB-1)) /
               WS-MV-LAST-YTD (SUB-1)) * 100.
           MOVE WS-MARGIN-PERC            TO D-PERC.

           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC DETAIL-LINE.
           MOVE 0   TO WS-MARGIN-PERC.
       PRR-050.
           MOVE 2610 TO POS.
           DISPLAY "ACCOUNT PROCESSED:" AT POS.
           ADD 20 TO POS.
           DISPLAY WS-GL-ACCOUNT (SUB-1) AT POS.
       PRR-060.
           ADD 1  TO LINE-CNT SUB-1
           GO TO PRR-001.
       PRR-999.
           EXIT.
      *
       COMPUTE-FIGURES SECTION.
       CF-010.
           MOVE 0 TO SUB-2
                     WS-MARGIN-MV-YTD
                     WS-MARGIN-BU-YTD
                     WS-MARGIN-MV-LAST-YTD
                     WS-MARGIN-MV-LAST-TOTAL
                     WS-MARGIN-BU-TOTAL
                     WS-MARGIN-PERC.
       CF-020.
      * SUB-4 = GL-PARAMETER-CURRENT-PER
           ADD 1 TO SUB-2.
           IF SUB-2 > 12
              GO TO CF-900.
           IF SUB-2 NOT > SUB-4
               ADD GL-PER (SUB-2)         TO WS-MARGIN-MV-YTD
               ADD GL-PER-BU (SUB-2)      TO WS-MARGIN-BU-YTD
               ADD GL-LAST-PER (SUB-2)    TO WS-MARGIN-MV-LAST-YTD.
           ADD GL-LAST-PER (SUB-2)        TO WS-MARGIN-MV-LAST-TOTAL.
           ADD GL-PER-BU (SUB-2)          TO WS-MARGIN-BU-TOTAL.
           GO TO CF-020.
       CF-900.
           COMPUTE WS-MARGIN-PERC ROUNDED =
             ((WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST-YTD) /
               WS-MARGIN-MV-LAST-YTD) * 100.
           ADD GL-PER (SUB-4)              TO WS-COLUMN1.
           ADD GL-PER-BU (SUB-4)           TO WS-COLUMN2.
           ADD WS-MARGIN-MV-YTD            TO WS-COLUMN3.
           ADD WS-MARGIN-BU-YTD            TO WS-COLUMN4.
           ADD WS-MARGIN-MV-LAST-YTD       TO WS-COLUMN5.
           ADD WS-MARGIN-MV-LAST-TOTAL     TO WS-COLUMN6.
           ADD WS-MARGIN-BU-TOTAL          TO WS-COLUMN7.
       CF-999.
           EXIT.
      *
       COMPUTE-PERC SECTION.
       CP-010.
           MOVE 0 TO SUB-2
                     WS-MARGIN-MV-PTD
                     WS-MARGIN-BU-PTD
                     WS-MARGIN-MV-YTD
                     WS-MARGIN-BU-YTD
                     WS-MARGIN-MV-LAST-YTD
                     WS-MARGIN-MV-LAST-TOTAL
                     WS-MARGIN-BU-TOTAL
                     WS-MARGIN-PERC.
       CP-020.
           ADD 1 TO SUB-2.
           IF SUB-2 = SUB-1
               GO TO CP-900.
           ADD WS-MV-PTD (SUB-2)          TO WS-MARGIN-MV-PTD
           ADD WS-BU-PTD (SUB-2)          TO WS-MARGIN-BU-PTD
           ADD WS-MV-YTD (SUB-2)          TO WS-MARGIN-MV-YTD
           ADD WS-BU-YTD (SUB-2)          TO WS-MARGIN-BU-YTD
           ADD WS-MV-LAST-YTD (SUB-2)     TO WS-MARGIN-MV-LAST-YTD
           ADD WS-MV-LAST (SUB-2)         TO WS-MARGIN-MV-LAST-TOTAL
           ADD WS-BU (SUB-2)              TO WS-MARGIN-BU-TOTAL.
           
      *     MOVE WS-MARGIN-MV-PTD TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE WS-MARGIN-MV-YTD TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
           GO TO CP-020.
       CP-900.
           COMPUTE WS-MARGIN-PERC ROUNDED =
             ((WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST-YTD) /
               WS-MARGIN-MV-LAST-YTD) * 100.
           ADD WS-MARGIN-MV-PTD            TO WS-COLUMN1
           ADD WS-MARGIN-BU-PTD            TO WS-COLUMN2
           ADD WS-MARGIN-MV-YTD            TO WS-COLUMN3
           ADD WS-MARGIN-BU-YTD            TO WS-COLUMN4
           ADD WS-MARGIN-MV-LAST-YTD       TO WS-COLUMN5
           ADD WS-MARGIN-MV-LAST-TOTAL     TO WS-COLUMN6
           ADD WS-MARGIN-BU-TOTAL          TO WS-COLUMN7.
       CP-999.
           EXIT.
      *
       COMPUTE-GP-PERC SECTION.
       CGP-020.
           COMPUTE WS-COLUMN1 = WS-COLUMN1-SALES + WS-COLUMN1-COSTS
           COMPUTE WS-COLUMN2 = WS-COLUMN2-SALES + WS-COLUMN2-COSTS
           COMPUTE WS-COLUMN3 = WS-COLUMN3-SALES + WS-COLUMN3-COSTS
           COMPUTE WS-COLUMN4 = WS-COLUMN4-SALES + WS-COLUMN4-COSTS
           COMPUTE WS-COLUMN5 = WS-COLUMN5-SALES + WS-COLUMN5-COSTS
           COMPUTE WS-COLUMN6 = WS-COLUMN6-SALES + WS-COLUMN6-COSTS
           COMPUTE WS-COLUMN7 = WS-COLUMN7-SALES + WS-COLUMN7-COSTS.
       CGP-999.
           EXIT.
      *
       COMPUTE-NETT-PERC SECTION.
       CNP-020.
           COMPUTE WS-COLUMN1 =
            WS-COLUMN1-SALES + WS-COLUMN1-COSTS + WS-COLUMN1-EXP.
           COMPUTE WS-COLUMN2 =
            WS-COLUMN2-SALES + WS-COLUMN2-COSTS + WS-COLUMN2-EXP.
           COMPUTE WS-COLUMN3 =
            WS-COLUMN3-SALES + WS-COLUMN3-COSTS + WS-COLUMN3-EXP.
           COMPUTE WS-COLUMN4 =
            WS-COLUMN4-SALES + WS-COLUMN4-COSTS + WS-COLUMN4-EXP.
           COMPUTE WS-COLUMN5 =
            WS-COLUMN5-SALES + WS-COLUMN5-COSTS + WS-COLUMN5-EXP.
           COMPUTE WS-COLUMN6 =
            WS-COLUMN6-SALES + WS-COLUMN6-COSTS + WS-COLUMN6-EXP.
           COMPUTE WS-COLUMN7 =
            WS-COLUMN7-SALES + WS-COLUMN7-COSTS + WS-COLUMN7-EXP.

           MOVE WS-COLUMN1 TO WS-COLUMN1-NETT
           MOVE WS-COLUMN2 TO WS-COLUMN2-NETT
           MOVE WS-COLUMN3 TO WS-COLUMN3-NETT
           MOVE WS-COLUMN4 TO WS-COLUMN4-NETT
           MOVE WS-COLUMN5 TO WS-COLUMN5-NETT
           MOVE WS-COLUMN6 TO WS-COLUMN6-NETT
           MOVE WS-COLUMN7 TO WS-COLUMN7-NETT.
       CNP-999.
           EXIT.
      *
       COMPUTE-COST-PERC SECTION.
       CCP-010.
           MOVE 0 TO SUB-2
                     WS-MARGIN-MV-PTD
                     WS-MARGIN-BU-PTD
                     WS-MARGIN-MV-YTD
                     WS-MARGIN-BU-YTD
                     WS-MARGIN-MV-LAST-YTD
                     WS-MARGIN-MV-LAST-TOTAL
                     WS-MARGIN-BU-TOTAL
                     WS-MARGIN-PERC.
           MOVE 6 TO SUB-2.
       CCP-020.
           ADD 1 TO SUB-2.
           IF SUB-2 = 34
               GO TO CCP-900.
           ADD WS-MV-PTD (SUB-2)          TO WS-MARGIN-MV-PTD
           ADD WS-BU-PTD (SUB-2)          TO WS-MARGIN-BU-PTD
           ADD WS-MV-YTD (SUB-2)          TO WS-MARGIN-MV-YTD
           ADD WS-BU-YTD (SUB-2)          TO WS-MARGIN-BU-YTD
           ADD WS-MV-LAST-YTD (SUB-2)     TO WS-MARGIN-MV-LAST-YTD
           ADD WS-MV-LAST (SUB-2)         TO WS-MARGIN-MV-LAST-TOTAL
           ADD WS-BU (SUB-2)              TO WS-MARGIN-BU-TOTAL.
           
      *     MOVE WS-MARGIN-MV-PTD TO WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE WS-MARGIN-MV-YTD TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
           GO TO CCP-020.
       CCP-900.
           COMPUTE WS-MARGIN-PERC ROUNDED =
             ((WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST-YTD) /
               WS-MARGIN-MV-LAST-YTD) * 100.
           ADD WS-MARGIN-MV-PTD            TO WS-COLUMN1
           ADD WS-MARGIN-BU-PTD            TO WS-COLUMN2
           ADD WS-MARGIN-MV-YTD            TO WS-COLUMN3
           ADD WS-MARGIN-BU-YTD            TO WS-COLUMN4
           ADD WS-MARGIN-MV-LAST-YTD       TO WS-COLUMN5
           ADD WS-MARGIN-MV-LAST-TOTAL     TO WS-COLUMN6
           ADD WS-MARGIN-BU-TOTAL          TO WS-COLUMN7.
       CCP-999.
           EXIT.
      *
       COMPUTE-PROFIT SECTION.
       CPR-010.
           MOVE 0 TO SUB-2
                     WS-MARGIN-MV-PTD
                     WS-MARGIN-BU-PTD
                     WS-MARGIN-MV-YTD
                     WS-MARGIN-BU-YTD
                     WS-MARGIN-MV-LAST-YTD
                     WS-MARGIN-MV-LAST-TOTAL
                     WS-MARGIN-BU-TOTAL
                     WS-MARGIN-PERC.
           MOVE 33 TO SUB-2.
       CPR-020.
           ADD 1 TO SUB-2.
           IF SUB-2 = 39
               GO TO CPR-900.
           ADD WS-MV-PTD (SUB-2)          TO WS-MARGIN-MV-PTD
           ADD WS-BU-PTD (SUB-2)          TO WS-MARGIN-BU-PTD
           ADD WS-MV-YTD (SUB-2)          TO WS-MARGIN-MV-YTD
           ADD WS-BU-YTD (SUB-2)          TO WS-MARGIN-BU-YTD
           ADD WS-MV-LAST-YTD (SUB-2)     TO WS-MARGIN-MV-LAST-YTD
           ADD WS-MV-LAST (SUB-2)         TO WS-MARGIN-MV-LAST-TOTAL
           ADD WS-BU (SUB-2)              TO WS-MARGIN-BU-TOTAL.

           GO TO CPR-020.
       CPR-900.
           COMPUTE WS-MARGIN-PERC ROUNDED =
             ((WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST-YTD) /
               WS-MARGIN-MV-LAST-YTD) * 100.
           MOVE WS-MARGIN-MV-PTD            TO WS-COLUMN1
           MOVE WS-MARGIN-BU-PTD            TO WS-COLUMN2
           MOVE WS-MARGIN-MV-YTD            TO WS-COLUMN3
           MOVE WS-MARGIN-BU-YTD            TO WS-COLUMN4
           MOVE WS-MARGIN-MV-LAST-YTD       TO WS-COLUMN5
           MOVE WS-MARGIN-MV-LAST-TOTAL     TO WS-COLUMN6
           MOVE WS-MARGIN-BU-TOTAL          TO WS-COLUMN7.
           COMPUTE WS-COLUMN1 =
            WS-COLUMN1 + WS-COLUMN1-NETT.
           COMPUTE WS-COLUMN2 =
            WS-COLUMN2 + WS-COLUMN2-NETT.
           COMPUTE WS-COLUMN3 =
            WS-COLUMN3 + WS-COLUMN3-NETT.
           COMPUTE WS-COLUMN4 =
            WS-COLUMN4 + WS-COLUMN4-NETT.
           COMPUTE WS-COLUMN5 =
            WS-COLUMN5 + WS-COLUMN5-NETT.
           COMPUTE WS-COLUMN6 =
            WS-COLUMN6 + WS-COLUMN6-NETT.
           COMPUTE WS-COLUMN7 =
            WS-COLUMN7 + WS-COLUMN7-NETT.
       CPR-999.
           EXIT.
      *
       COMPUTE-TEMP-FIGURES SECTION.
       CFT-010.
           MOVE 0 TO SUB-2
                     WS-MARGIN-MV-PTD
                     WS-MARGIN-BU-PTD
                     WS-MARGIN-MV-YTD
                     WS-MARGIN-BU-YTD
                     WS-MARGIN-MV-LAST-YTD
                     WS-MARGIN-MV-LAST-TOTAL
                     WS-MARGIN-BU-TOTAL
                     WS-MARGIN-PERC.
            ADD GL-PER (SUB-4)         TO WS-MARGIN-MV-PTD
            ADD GL-PER-BU (SUB-4)      TO WS-MARGIN-BU-PTD.
       CFT-020.
      * SUB-4 = GL-PARAMETER-CURRENT-PER
           ADD 1 TO SUB-2.
           IF SUB-2 > 12
              GO TO CFT-900.
           IF SUB-2 NOT > SUB-4
               ADD GL-PER (SUB-2)         TO WS-MARGIN-MV-YTD
               ADD GL-PER-BU (SUB-2)      TO WS-MARGIN-BU-YTD
               ADD GL-LAST-PER (SUB-2)    TO WS-MARGIN-MV-LAST-YTD.
           ADD GL-LAST-PER (SUB-2)        TO WS-MARGIN-MV-LAST-TOTAL.
           ADD GL-PER-BU (SUB-2)          TO WS-MARGIN-BU-TOTAL.
           GO TO CFT-020.
       CFT-900.
           COMPUTE WS-MARGIN-PERC ROUNDED =
             ((WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST-YTD) /
               WS-MARGIN-MV-LAST-YTD) * 100.
      *     MOVE WS-MARGIN-MV-YTD        TO WS-MV-YTD (SUB-1)
      *     MOVE WS-MARGIN-BU-YTD        TO WS-BU-YTD (SUB-1)
      *     MOVE WS-MARGIN-MV-LAST-YTD   TO WS-MV-LAST-YTD (SUB-1)
      *     MOVE WS-MARGIN-MV-LAST-TOTAL TO WS-MV-LAST (SUB-1)
      *     MOVE WS-MARGIN-BU-TOTAL      TO WS-BU (SUB-1)
      *     MOVE WS-MARGIN-PERC          TO WS-PERC (SUB-1).
       CFT-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
              MOVE "NO GLPARAMETER RECORD READ, CALL THE SUPERVISOR."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "GLPARAMETER BUSY ON READ, RP-000, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO OPEN-000.
       OPEN-005.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-005.
       OPEN-0051.
           PERFORM READ-PARAMETER
           MOVE GLPA-NAME TO CO-NAME
           PERFORM ENTER-PERIOD-DATES.
       OPEN-006.
           MOVE 1                  TO SUB-1
           MOVE GL-BEGDATE (SUB-1) TO SPLIT-DATE
           MOVE SPLIT-YY           TO WS-DATE-SPLIT.
           MOVE WS-DATE-YY         TO H3-1BEG H3-2BEG H3-3END H3-4BEG.
           IF WS-DATE-YY > 0
              SUBTRACT 1         FROM WS-DATE-YY
           ELSE
              MOVE 99              TO WS-DATE-YY.
           MOVE WS-DATE-YY         TO H3-3BEG.

           MOVE 13               TO SUB-1
           MOVE GLPA-PER (SUB-1) TO SPLIT-DATE
           MOVE SPLIT-YY         TO WS-DATE-SPLIT.
           MOVE WS-DATE-YY       TO H3-1END H3-2END H3-4END
           PERFORM OPEN-010.
            
           CLOSE GLPARAMETER-FILE.
       OPEN-010.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-005.
           MOVE 1 TO SUB-20
           MOVE 
           "COMPANIES IN GROUP                            STATUS"
            TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
       END-006.
           MOVE LIST-NUMBER (SUB-20)    TO COM-NUM
           MOVE LIST-NAME (SUB-20)      TO COM-NAME.
           IF LIST-NOT-THERE (SUB-20) = "N"
              MOVE "Link Down"          TO COM-USED.
           IF LIST-NOT-THERE (SUB-20) = " "
              MOVE "Company Used"       TO COM-USED.
           IF LIST-NOT-THERE (SUB-20) = "Z"
              MOVE "Company Omitted"    TO COM-USED.
              
           MOVE COMPANIES TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
       END-007.
           IF SUB-20 < 20
              ADD 1 TO SUB-20.
           IF LIST-NAME (SUB-20) > " "
              GO TO END-006.
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
       END-010.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-500.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       Copy "EnterGLPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
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
      * END-OF-JOB.
