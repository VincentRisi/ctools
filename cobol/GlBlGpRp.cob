        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlBlGpRp.
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
       77  WS-GLPARAMETER-SAVE       PIC X(40) VALUE " ".
       77  WS-GLMASTER-SAVE          PIC X(40) VALUE " ".
       77  WS-PRINT                  PIC X VALUE " ".
       77  WS-1ST-CO-USED            PIC X VALUE " ".
       77  WS-GROUP                  PIC X VALUE " ".
       77  PAGE-CNT                  PIC 9(3) VALUE 0.
       77  WS-FOLDER-NUM             PIC 9 VALUE 0.
       77  WS-NEXT-NUMBER1           PIC X(12) VALUE " ".
       77  WS-NEXT-NUMBER2           PIC X(12) VALUE " ".
       77  WS-BARE-GLNUMBER          PIC X(12) VALUE " ".
       77  WS-BARE-GLPARAM           PIC X(12) VALUE " ".
       77  WS-MARGIN-PERC            PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN                 PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-MV-YTD          PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-MV-LAST         PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN1                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN2                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN3                PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN1-CAPIT          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN2-CAPIT          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN3-CAPIT          PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN1-ASSETS         PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN2-ASSETS         PIC S9(8)V99 VALUE 0.
       77  WS-COLUMN3-ASSETS         PIC S9(8)V99 VALUE 0.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1        PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1   PIC 99.
       01  WS-MENU-STATUS.
           03  WS-MENU-ST1          PIC 99.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-HEADER   PIC X(2).
               05  WS-SUB      PIC X(4).
           03  WS-REST         PIC X(6).
       01  COMPANIES.
           03  COM-NUM         PIC Z9.
           03  FILLER          PIC X(2) VALUE ". ".
           03  COM-NAME        PIC X(42).
           03  COM-USED        PIC X(15).
       01  COMPANIES-LIST-NAMES.
         02  COMPANIES-LIST OCCURS 20.
           03  LIST-GROUP       PIC X.
           03  LIST-NAME        PIC X(40).
           03  LIST-NUMBER      PIC 99.
           03  LIST-VOL-DIR     PIC X(25).
           03  LIST-NOT-THERE   PIC X.
       01  WS-BRANCH-INFO-NAMES.
         02  WS-BRANCH-INFO OCCURS 20.
           03  WS-BRANCH-TYPE          PIC 9.
           03  WS-BRANCH-NUMBER        PIC 9.
           03  WS-BRANCH-NOT-THERE     PIC X(2).
           03  WS-BRANCH-NAME          PIC X(3).
           03  WS-BRANCH-GL-VOL-DIR    PIC X(40).
       01 GROUP-LINES-NAMES.
         02 GROUP-LINES OCCURS 20.
           03  WS-SUB1 OCCURS 15.
              05  WS-GL-ACCOUNT       PIC X(12).
              05  WS-GL-DESC          PIC X(40).
              05  WS-GL-AMOUNT        PIC S9(8)V99.
              05  WS-GL-AMOUNT-LY     PIC S9(8)V99.
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(20) VALUE " ".
           03  FILLER         PIC X(45) VALUE
           "GROUP GENERAL LEDGER BALANCE SHEET - GROUP = ".
           03  H1-GROUP       PIC X.
           03  FILLER         PIC X(7) VALUE " ".
           03  FILLER         PIC X(7) VALUE "PERIOD:".
           03  H1-PERIOD      PIC X(3).
           03  H1-PER-BEG     PIC X(10).
           03  FILLER         PIC X(4) VALUE " TO ".
           03  H1-PER-END     PIC X(10).
           03  FILLER         PIC X(2) VALUE " ".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
       01  HEAD3.
           03  FILLER         PIC X(40) VALUE " ".
           03  FILLER         PIC X(92) VALUE
           "This Year      Previous      Movement        Move".
       01  HEAD3-1.
           03  FILLER         PIC X(40) VALUE " ".
           03  H3-1BEG        PIC X(4).
           03  FILLER         PIC X VALUE "/".
           03  H3-1END        PIC X(9).
           03  H3-2BEG        PIC X(4).
           03  FILLER         PIC X VALUE "/".
           03  H3-2END        PIC X(14).
           03  FILLER         PIC X(20) VALUE "Rand          %".
       01  DETAIL-LINE.
           03  D-NAME         PIC X(38).
           03  D-MV-YTD       PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-MV-LAST      PIC Z(7)9.99-.
           03  FILLER         PIC X(2) VALUE " ".
           03  D-MV-RAND      PIC Z(7)9.99-.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-PERC         PIC Z(3)9.99-.
           03  FILLER         PIC X(42) VALUE " ".
       01  UNDER-LINE.
           03  FILLER            PIC X(38) VALUE " ".
           03  FILLER            PIC X(14) VALUE "-----------".
           03  FILLER            PIC X(14) VALUE "-----------".
           03  FILLER            PIC X(16) VALUE "-----------".
           03  FILLER            PIC X(50) VALUE "-------".
       01  DOUBLE-UNDER-LINE.
           03  FILLER            PIC X(38) VALUE " ".
           03  FILLER            PIC X(14) VALUE "===========".
           03  FILLER            PIC X(14) VALUE "===========".
           03  FILLER            PIC X(16) VALUE "===========".
           03  FILLER            PIC X(50) VALUE "=======".
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
           DISPLAY "** GENERAL LEDGER GROUP BALANCE SHEET **" AT POS.
           MOVE 421 TO POS.           
           DISPLAY "****************************************" AT POS.
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
           PERFORM OPEN-012
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
      *     
      *     MOVE WS-BARE-GLPARAM TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
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
      *     MOVE "IN STRIP GLMASTER" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      *     MOVE WS-BARE-GLNUMBER TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       STRIP-999.
           EXIT.
      *
       ENTER-GROUP SECTION.
       EG-010.
           MOVE " " TO WS-GROUP.
           MOVE 0915 TO POS.
           DISPLAY "ENTER 'A' FOR ALL GROUP COMPANIES." AT POS.
           MOVE 0815 TO POS.
           DISPLAY "ENTER THE GROUP NUMBER TO PRINT:[ ]" AT POS.
           MOVE 0848 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
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
           MOVE 0915 TO POS.
           DISPLAY "                                          " AT POS.
       EG-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
            IF SUB-1 = 1
                MOVE " " TO PRINT-REC DETAIL-LINE
                MOVE "CAPITAL EMPLOYED" TO D-NAME
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                MOVE "70-015-10-00" TO WS-NEXT-NUMBER1
                MOVE "70-015-99-00" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 2
                MOVE "75-030-20-05" TO WS-NEXT-NUMBER1
                MOVE "75-030-20-99" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "N" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 3
                MOVE "Directors Loan A/cs   " TO D-NAME
                MOVE WS-MARGIN-MV-YTD         TO D-MV-YTD
                MOVE WS-MARGIN-MV-LAST        TO D-MV-LAST
                COMPUTE WS-MARGIN = WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST
                MOVE WS-MARGIN                TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = 
                   (WS-MARGIN / WS-MARGIN-MV-LAST) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                MOVE "DISTRIBUTABLE RESERVES" TO D-NAME
                MOVE WS-COLUMN1               TO D-MV-YTD
                MOVE WS-COLUMN2               TO D-MV-LAST
                MOVE WS-COLUMN3               TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = ((WS-COLUMN1 - WS-COLUMN2)
                    / WS-COLUMN2) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                MOVE "70-010      " TO WS-NEXT-NUMBER1
                MOVE "70-010      " TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 4
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                MOVE "TOTAL CAPITAL EMPLOYED" TO D-NAME
                MOVE WS-COLUMN1               TO D-MV-YTD
                MOVE WS-COLUMN2               TO D-MV-LAST
                MOVE WS-COLUMN3               TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = ((WS-COLUMN1 - WS-COLUMN2)
                     / WS-COLUMN2) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC FROM DOUBLE-UNDER-LINE
                MOVE " " TO PRINT-REC
                WRITE PRINT-REC
                MOVE "EMPLOYEMENT OF CAPITAL" TO D-NAME
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                MOVE 0 TO WS-COLUMN1 WS-COLUMN1-CAPIT
                          WS-COLUMN2 WS-COLUMN2-CAPIT
                          WS-COLUMN3 WS-COLUMN3-CAPIT
                MOVE "75-010      " TO WS-NEXT-NUMBER1
                MOVE "75-010      " TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 5
                MOVE "75-020-05-00" TO WS-NEXT-NUMBER1
                MOVE "75-020-05-00" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 6
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                MOVE "TOTAL FIXED ASSETS    " TO D-NAME
                MOVE WS-COLUMN1               TO D-MV-YTD
                MOVE WS-COLUMN2               TO D-MV-LAST
                MOVE WS-COLUMN3               TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = ((WS-COLUMN1 - WS-COLUMN2)
                     / WS-COLUMN2) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                MOVE WS-COLUMN1 TO WS-COLUMN1-CAPIT
                MOVE WS-COLUMN2 TO WS-COLUMN2-CAPIT
                MOVE WS-COLUMN3 TO WS-COLUMN3-CAPIT
                MOVE 0          TO WS-COLUMN1
                                   WS-COLUMN2
                                   WS-COLUMN3
                                   WS-MARGIN-MV-YTD
                                   WS-MARGIN-MV-LAST
                                   WS-MARGIN
                                   WS-MARGIN-PERC
                MOVE "75-020-10-05" TO WS-NEXT-NUMBER1
                MOVE "75-020-10-15" TO WS-NEXT-NUMBER2
                MOVE "N" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 7
                MOVE "Sundry Debtors        " TO D-NAME
                MOVE WS-MARGIN-MV-YTD         TO D-MV-YTD
                MOVE WS-MARGIN-MV-LAST        TO D-MV-LAST
                COMPUTE WS-MARGIN = WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST
                MOVE WS-MARGIN                TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = 
                   (WS-MARGIN / WS-MARGIN-MV-LAST) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                
                MOVE "75-020-05-05" TO WS-NEXT-NUMBER1
                MOVE "75-020-05-05" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 8
                MOVE "75-030-15-00" TO WS-NEXT-NUMBER1
                MOVE "75-030-15-00" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 9
                MOVE "75-020-15-00" TO WS-NEXT-NUMBER1
                MOVE "75-020-20-99" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 10
                MOVE "75-020-25-00" TO WS-NEXT-NUMBER1
                MOVE "75-020-25-99" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "N" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 11
                MOVE "Deposits              " TO D-NAME
                MOVE WS-MARGIN-MV-YTD         TO D-MV-YTD
                MOVE WS-MARGIN-MV-LAST        TO D-MV-LAST
                COMPUTE WS-MARGIN = WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST
                MOVE WS-MARGIN                TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = 
                   (WS-MARGIN / WS-MARGIN-MV-LAST) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                MOVE "75-020-30-10" TO WS-NEXT-NUMBER1
                MOVE "75-020-50-99" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 12
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                MOVE "CURRENT ASSETS        " TO D-NAME
                MOVE WS-COLUMN1               TO D-MV-YTD
                MOVE WS-COLUMN2               TO D-MV-LAST
                MOVE WS-COLUMN3               TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = ((WS-COLUMN1 - WS-COLUMN2)
                     / WS-COLUMN2) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                ADD  WS-COLUMN1 TO WS-COLUMN1-CAPIT WS-COLUMN1-ASSETS
                ADD  WS-COLUMN2 TO WS-COLUMN2-CAPIT WS-COLUMN2-ASSETS
                ADD  WS-COLUMN3 TO WS-COLUMN3-CAPIT WS-COLUMN3-ASSETS
                MOVE 0          TO WS-COLUMN1
                                   WS-COLUMN2
                                   WS-COLUMN3
                                   WS-MARGIN-MV-YTD
                                   WS-MARGIN-MV-LAST
                                   WS-MARGIN
                                   WS-MARGIN-PERC
                MOVE "75-030-05-00" TO WS-NEXT-NUMBER1
                MOVE "75-030-05-19" TO WS-NEXT-NUMBER2
                MOVE "N" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 13
                MOVE "Sundry Creditors      " TO D-NAME
                MOVE WS-MARGIN-MV-YTD         TO D-MV-YTD
                MOVE WS-MARGIN-MV-LAST        TO D-MV-LAST
                COMPUTE WS-MARGIN = WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST
                MOVE WS-MARGIN                TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = 
                   (WS-MARGIN / WS-MARGIN-MV-LAST) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                MOVE "75-030-05-20" TO WS-NEXT-NUMBER1
                MOVE "75-030-15-99" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 14
                MOVE "75-030-30-05" TO WS-NEXT-NUMBER1
                MOVE "75-030-30-99" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "N" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 15
                MOVE "Bank Asset Finance    " TO D-NAME
                MOVE WS-MARGIN-MV-YTD         TO D-MV-YTD
                MOVE WS-MARGIN-MV-LAST        TO D-MV-LAST
                COMPUTE WS-MARGIN = WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST
                MOVE WS-MARGIN                TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = 
                   (WS-MARGIN / WS-MARGIN-MV-LAST) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                MOVE "CURRENT LIABILITIES   " TO D-NAME
                MOVE WS-COLUMN1               TO D-MV-YTD
                MOVE WS-COLUMN2               TO D-MV-LAST
                MOVE WS-COLUMN3               TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = (WS-COLUMN3 / WS-COLUMN2) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                MOVE " " TO PRINT-REC
                ADD  WS-COLUMN1 TO WS-COLUMN1-CAPIT WS-COLUMN1-ASSETS
                ADD  WS-COLUMN2 TO WS-COLUMN2-CAPIT WS-COLUMN2-ASSETS
                ADD  WS-COLUMN3 TO WS-COLUMN3-CAPIT WS-COLUMN3-ASSETS
                MOVE 0          TO WS-COLUMN1
                                   WS-COLUMN2
                                   WS-COLUMN3
                                   WS-MARGIN-MV-YTD
                                   WS-MARGIN-MV-LAST
                                   WS-MARGIN
                                   WS-MARGIN-PERC
                MOVE "NETT CURRENT ASSETS/LIABILITIES(-)" TO D-NAME
                MOVE WS-COLUMN1-ASSETS         TO D-MV-YTD
                MOVE WS-COLUMN2-ASSETS         TO D-MV-LAST
                MOVE WS-COLUMN3-ASSETS         TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC =
                  ((WS-COLUMN1-ASSETS - WS-COLUMN2-ASSETS)
                     / WS-COLUMN2-ASSETS) * 100
                MOVE WS-MARGIN-PERC           TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                MOVE "ASSOCIATED COMPANIES  " TO D-NAME
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                MOVE 0 TO WS-COLUMN1
                          WS-COLUMN2
                          WS-COLUMN3
                          WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                          WS-MARGIN-PERC
                MOVE "75-040-05-00" TO WS-NEXT-NUMBER1
                MOVE "75-040-99-99" TO WS-NEXT-NUMBER2
                MOVE "Y" TO WS-PRINT
                GO TO GET-999.
            IF SUB-1 = 16
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                MOVE "LOANS FROM(-)/TO ASSOCIATE CO'S" TO D-NAME
                MOVE WS-COLUMN1                        TO D-MV-YTD
                MOVE WS-COLUMN2                        TO D-MV-LAST
                MOVE WS-COLUMN3                        TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC = ((WS-COLUMN1 - WS-COLUMN2)
                     / WS-COLUMN2) * 100
                MOVE WS-MARGIN-PERC                    TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC
                WRITE PRINT-REC FROM UNDER-LINE
                MOVE " " TO PRINT-REC
                ADD  WS-COLUMN1 TO WS-COLUMN1-CAPIT
                ADD  WS-COLUMN2 TO WS-COLUMN2-CAPIT
                ADD  WS-COLUMN3 TO WS-COLUMN3-CAPIT
                MOVE "TOTAL EMPLOYEMENT OF CAPITAL" TO D-NAME
                MOVE WS-COLUMN1-CAPIT               TO D-MV-YTD
                MOVE WS-COLUMN2-CAPIT               TO D-MV-LAST
                MOVE WS-COLUMN3-CAPIT               TO D-MV-RAND
                COMPUTE WS-MARGIN-PERC =
                  ((WS-COLUMN1-CAPIT - WS-COLUMN2-CAPIT)
                     / WS-COLUMN2-CAPIT) * 100
                MOVE WS-MARGIN-PERC    TO D-PERC
                WRITE PRINT-REC FROM DETAIL-LINE
                MOVE " " TO PRINT-REC DETAIL-LINE
                WRITE PRINT-REC FROM DOUBLE-UNDER-LINE
                MOVE " " TO PRINT-REC.
            MOVE 99 TO SUB-1.
      *      PERFORM END-OFF.
       GET-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE 1 TO SUB-1 SUB-40.
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
           MOVE 1 TO SUB-40
           GO TO PRR-020.
       PRR-010.
           IF LINE-CNT < 58
              GO TO PRR-020.
           ADD 1              TO PAGE-CNT
           MOVE PAGE-CNT      TO H1-PAGE
           MOVE WS-GROUP      TO H1-GROUP
           MOVE LIST-NAME (1) TO CO-NAME
           MOVE " "           TO PRINT-REC.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
           IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
           ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
           WRITE PRINT-REC FROM HEAD1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC FROM HEAD3
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC FROM HEAD3-1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE 6 TO LINE-CNT.
       PRR-020.
           IF SUB-40 > 15
               SUBTRACT 1 FROM LINE-CNT
               GO TO PRR-060.
           IF WS-GL-ACCOUNT (SUB-1 SUB-40) = " "
               ADD 1 TO SUB-40
               GO TO PRR-020.

           IF SUB-1 = 8
            IF WS-GL-ACCOUNT (SUB-1 SUB-40) = "75-030-15-00"
             IF WS-GL-AMOUNT (SUB-1 SUB-40) Not > 0
               GO TO PRR-050.
           IF SUB-1 = 13
            IF WS-GL-ACCOUNT (SUB-1 SUB-40) = "75-030-15-00"
             IF WS-GL-AMOUNT (SUB-1 SUB-40) Not < 0
               SUBTRACT 1 FROM LINE-CNT
               GO TO PRR-050.
       
           PERFORM COMPUTE-FIGURES.
           IF WS-PRINT = "N"
                PERFORM PRR-050
                ADD 1 TO SUB-40
                GO TO PRR-020.
           MOVE WS-GL-DESC (SUB-1 SUB-40)      TO D-NAME
           MOVE WS-GL-AMOUNT (SUB-1 SUB-40)    TO D-MV-YTD
           MOVE WS-GL-AMOUNT-LY (SUB-1 SUB-40) TO D-MV-LAST
           MOVE WS-MARGIN                      TO D-MV-RAND
           MOVE WS-MARGIN-PERC                 TO D-PERC.

           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE.
       PRR-050.
           MOVE 2610 TO POS
           DISPLAY "ACCOUNT PROCESSED:" AT POS
           ADD 20 TO POS
           DISPLAY WS-GL-ACCOUNT (SUB-1 SUB-40) AT POS.
       PRR-055.
           ADD 1 TO LINE-CNT SUB-40.
           IF SUB-40 < 16
               GO TO PRR-020
           ELSE
               GO TO PRR-060.
       PRR-060.
           ADD 1  TO LINE-CNT SUB-1
           MOVE 1 TO SUB-40
           GO TO PRR-001.
       PRR-999.
           EXIT.
      *
       COMPUTE-FIGURES SECTION.
       CF-010.
           IF WS-PRINT = "Y"
               MOVE 0 TO WS-MARGIN-MV-YTD
                         WS-MARGIN-MV-LAST
                         WS-MARGIN
                         WS-MARGIN-PERC.
       CF-015.
           ADD WS-GL-AMOUNT (SUB-1 SUB-40)       TO WS-MARGIN-MV-YTD
           ADD WS-GL-AMOUNT-LY (SUB-1 SUB-40)    TO WS-MARGIN-MV-LAST
           COMPUTE WS-MARGIN = WS-MARGIN-MV-YTD - WS-MARGIN-MV-LAST.
       CF-500.
           COMPUTE WS-MARGIN-PERC =
             ((WS-GL-AMOUNT (SUB-1 SUB-40) - 
               WS-GL-AMOUNT-LY (SUB-1 SUB-40)) /
               WS-GL-AMOUNT-LY (SUB-1 SUB-40)) * 100.
           COMPUTE WS-MARGIN = WS-GL-AMOUNT (SUB-1 SUB-40) - 
               WS-GL-AMOUNT-LY (SUB-1 SUB-40).
           ADD WS-GL-AMOUNT (SUB-1 SUB-40)    TO WS-COLUMN1
           ADD WS-GL-AMOUNT-LY (SUB-1 SUB-40) TO WS-COLUMN2
           ADD WS-MARGIN                      TO WS-COLUMN3.
       CF-999.
           EXIT.
      *
       GET-MEMORY-DATA SECTION.
       GET-MEM-000.
            IF SUB-1 = 1
                MOVE "70-015-10-00" TO WS-NEXT-NUMBER1
                MOVE "70-015-99-00" TO WS-NEXT-NUMBER2
                GO TO GET-MEM-999.
            IF SUB-1 = 2
                MOVE "75-030-20-05" TO WS-NEXT-NUMBER1
                MOVE "75-030-20-99" TO WS-NEXT-NUMBER2
                MOVE 0 TO WS-MARGIN-MV-YTD
                          WS-MARGIN-MV-LAST
                          WS-MARGIN
                GO TO GET-MEM-999.
            IF SUB-1 = 3
                MOVE "70-010      " TO WS-NEXT-NUMBER1
                MOVE "70-010      " TO WS-NEXT-NUMBER2
                GO TO GET-MEM-999.
            IF SUB-1 = 4
                MOVE "75-010      " TO WS-NEXT-NUMBER1
                MOVE "75-010      " TO WS-NEXT-NUMBER2
                GO TO GET-MEM-999.
            IF SUB-1 = 5
                MOVE "75-020-05-00" TO WS-NEXT-NUMBER1
                MOVE "75-020-05-00" TO WS-NEXT-NUMBER2
                GO TO GET-MEM-999.
            IF SUB-1 = 6
                MOVE "75-020-10-05" TO WS-NEXT-NUMBER1
                MOVE "75-020-10-15" TO WS-NEXT-NUMBER2
                GO TO GET-MEM-999.
            IF SUB-1 = 7
                MOVE "75-020-05-05" TO WS-NEXT-NUMBER1
                MOVE "75-020-05-05" TO WS-NEXT-NUMBER2
                GO TO GET-MEM-999.
            IF SUB-1 = 8
                MOVE "75-030-15-00" TO WS-NEXT-NUMBER1
                MOVE "75-030-15-00" TO WS-NEXT-NUMBER2
                GO TO GET-MEM-999.
            IF SUB-1 = 9
                MOVE "75-020-15-00" TO WS-NEXT-NUMBER1
                MOVE "75-020-20-99" TO WS-NEXT-NUMBER2
                GO TO GET-MEM-999.
            IF SUB-1 = 10
                MOVE "75-020-25-00" TO WS-NEXT-NUMBER1
                MOVE "75-020-25-99" TO WS-NEXT-NUMBER2
                GO TO GET-MEM-999.
            IF SUB-1 = 11
                MOVE "75-020-30-10" TO WS-NEXT-NUMBER1
                MOVE "75-020-50-00" TO WS-NEXT-NUMBER2
                GO TO GET-MEM-999.
            IF SUB-1 = 12
                MOVE "75-030-05-00" TO WS-NEXT-NUMBER1
                MOVE "75-030-05-19" TO WS-NEXT-NUMBER2
                GO TO GET-MEM-999.
            IF SUB-1 = 13
                MOVE "75-030-05-20" TO WS-NEXT-NUMBER1
                MOVE "75-030-15-99" TO WS-NEXT-NUMBER2
                GO TO GET-MEM-999.
            IF SUB-1 = 14
                MOVE "75-030-30-05" TO WS-NEXT-NUMBER1
                MOVE "75-030-30-99" TO WS-NEXT-NUMBER2
                GO TO GET-MEM-999.

            IF SUB-1 = 15
                MOVE "75-040-05-00" TO WS-NEXT-NUMBER1
                MOVE "75-040-99-99" TO WS-NEXT-NUMBER2
                GO TO GET-MEM-999.
            IF SUB-1 > 15
                GO TO GET-MEM-999.
       GET-MEM-999.
            EXIT.
      *
       PRINT-TO-MEMORY SECTION.
       PRTM-000.
        IF WS-1ST-CO-USED = " "
            MOVE "Y" TO WS-1ST-CO-USED
        ELSE
            MOVE "N" TO WS-1ST-CO-USED.
      * SUB-40 = WS-SUB1
      * SUB-20 = COMPANY NUMBER BEING PROCESSED
            MOVE 1 TO SUB-1 SUB-40.
            MOVE 2910 TO POS.
            DISPLAY "The Report is being compiled to Memory....."
             AT POS.
       PRTM-001.
            PERFORM GET-MEMORY-DATA.
            IF SUB-1 > 15
                GO TO PRTM-999.
            
            MOVE WS-NEXT-NUMBER1 TO GL-NUMBER.
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
               MOVE "GL-MASTER BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO PRTM-002.
            IF GL-NUMBER < WS-NEXT-NUMBER1
               GO TO PRTM-002.
            IF GL-NUMBER > WS-NEXT-NUMBER2
               GO TO PRTM-060.
               
            MOVE 1 TO SUB-40.
       PRTM-020.
           IF SUB-40 > 15
      *        MOVE "WE HAVE A PROBLEM, RUN OUT PRTM-020" TO WS-MESSAGE
      *        PERFORM ERROR-MESSAGE
               GO TO PRTM-060.
               
      * CHECKING FOR THE BANK ACCOUNT IN DEBIT OR CREDIT
           IF SUB-1 = 8
            IF GL-NUMBER = "75-030-15-00"
             IF WS-1ST-CO-USED = "Y"
              IF GL-BALANCE < 0
               IF WS-GL-ACCOUNT (SUB-1 SUB-40) = " "
           MOVE GL-NUMBER              TO WS-GL-ACCOUNT (SUB-1 SUB-40)
           MOVE "Bank Current Account" TO WS-GL-DESC (SUB-1 SUB-40)
           MOVE 0                      TO WS-GL-AMOUNT (SUB-1 SUB-40)
                                          WS-GL-AMOUNT-LY (SUB-1 SUB-40)
                   GO TO PRTM-060
               ELSE
                   ADD 1 TO SUB-40
                   GO TO PRTM-020.
           IF SUB-1 = 8
            IF GL-NUMBER = "75-030-15-00"
             IF WS-1ST-CO-USED = "Y"
              IF GL-BALANCE > 0
               IF WS-GL-ACCOUNT (SUB-1 SUB-40) = " "
           MOVE GL-NUMBER              TO WS-GL-ACCOUNT (SUB-1 SUB-40)
           MOVE "Bank Current Account" TO WS-GL-DESC (SUB-1 SUB-40)
           MOVE GL-BALANCE             TO WS-GL-AMOUNT (SUB-1 SUB-40)
           MOVE GL-OPEN-YEAR-BAL       TO WS-GL-AMOUNT-LY (SUB-1 SUB-40)
                   GO TO PRTM-060
               ELSE
                   ADD 1 TO SUB-40
                   GO TO PRTM-020.
           IF SUB-1 = 8
            IF GL-NUMBER = "75-030-15-00"
             IF WS-1ST-CO-USED = "N"
             IF GL-BALANCE < 0
               GO TO PRTM-060.
           
           IF SUB-1 = 13
            IF GL-NUMBER = "75-030-15-00"
             IF WS-1ST-CO-USED = "Y"
              IF GL-BALANCE > 0
               IF WS-GL-ACCOUNT (SUB-1 SUB-40) = " "
           MOVE GL-NUMBER              TO WS-GL-ACCOUNT (SUB-1 SUB-40)
           MOVE "Bank Current Account" TO WS-GL-DESC (SUB-1 SUB-40)
           MOVE 0                      TO WS-GL-AMOUNT (SUB-1 SUB-40)
                                          WS-GL-AMOUNT-LY (SUB-1 SUB-40)
                   GO TO PRTM-055
               ELSE
                   ADD 1 TO SUB-40
                   GO TO PRTM-020.
           IF SUB-1 = 13
            IF GL-NUMBER = "75-030-15-00"
             IF WS-1ST-CO-USED = "Y"
              IF GL-BALANCE < 0
               IF WS-GL-ACCOUNT (SUB-1 SUB-40) = " "
           MOVE GL-NUMBER              TO WS-GL-ACCOUNT (SUB-1 SUB-40)
           MOVE "Bank Current Account" TO WS-GL-DESC (SUB-1 SUB-40)
           MOVE GL-BALANCE             TO WS-GL-AMOUNT (SUB-1 SUB-40)
           MOVE GL-OPEN-YEAR-BAL       TO WS-GL-AMOUNT-LY (SUB-1 SUB-40)
               GO TO PRTM-055
               ELSE
                   ADD 1 TO SUB-40
                   GO TO PRTM-020.
           IF SUB-1 = 13
            IF GL-NUMBER = "75-030-15-00"
             IF WS-1ST-CO-USED = "N"
              IF GL-BALANCE > 0
                 GO TO PRTM-055.
           IF SUB-1 = 13
            IF GL-NUMBER = "75-030-15-00"
             IF WS-1ST-CO-USED = "N"
              IF GL-BALANCE < 0
               IF GL-NUMBER NOT = WS-GL-ACCOUNT (SUB-1 SUB-40)
                 ADD 1 TO SUB-40
                 GO TO PRTM-020
               ELSE
                 ADD GL-BALANCE        TO WS-GL-AMOUNT (SUB-1 SUB-40)
                 ADD GL-OPEN-YEAR-BAL  TO WS-GL-AMOUNT-LY (SUB-1 SUB-40)
                 GO TO PRTM-050.

      * 1ST COMPANY - ACCOUNTS OTHER THAN BANK ACCOUNT
           IF WS-1ST-CO-USED = "Y"
            IF WS-GL-ACCOUNT (SUB-1 SUB-40) = " "
               MOVE GL-NUMBER        TO WS-GL-ACCOUNT (SUB-1 SUB-40)
               MOVE GL-DESCRIPTION   TO WS-GL-DESC (SUB-1 SUB-40)
               MOVE GL-BALANCE       TO WS-GL-AMOUNT (SUB-1 SUB-40)
               MOVE GL-OPEN-YEAR-BAL TO WS-GL-AMOUNT-LY (SUB-1 SUB-40)
               GO TO PRTM-050
            ELSE
               ADD 1 TO SUB-40
               GO TO PRTM-020.
               
      * 2ND AND SUBSEQUENT COMPANIES
      * - ACCOUNTS OTHER THAN BANK ACCOUNT
           IF WS-1ST-CO-USED = "N"
            IF GL-NUMBER = WS-GL-ACCOUNT (SUB-1 SUB-40)
               ADD GL-BALANCE       TO WS-GL-AMOUNT (SUB-1 SUB-40)
               ADD GL-OPEN-YEAR-BAL TO WS-GL-AMOUNT-LY (SUB-1 SUB-40)
               GO TO PRTM-050.
               
           IF WS-1ST-CO-USED = "N"
            IF GL-NUMBER NOT = WS-GL-ACCOUNT (SUB-1 SUB-40)
             IF WS-GL-ACCOUNT (SUB-1 SUB-40) = " "
               MOVE GL-NUMBER        TO WS-GL-ACCOUNT (SUB-1 SUB-40)
               MOVE GL-DESCRIPTION   TO WS-GL-DESC (SUB-1 SUB-40)
               MOVE GL-BALANCE       TO WS-GL-AMOUNT (SUB-1 SUB-40)
               MOVE GL-OPEN-YEAR-BAL TO WS-GL-AMOUNT-LY (SUB-1 SUB-40)
               GO TO PRTM-050
             ELSE
               ADD 1 TO SUB-40
               GO TO PRTM-020.
       PRTM-050.
           MOVE 2610 TO POS
           DISPLAY "ACCOUNT PROCESSED:" AT POS
           ADD 20 TO POS
           DISPLAY GL-NUMBER AT POS.
       PRTM-055.
           IF SUB-40 < 15
             ADD 1 TO SUB-40
             GO TO PRTM-002.
       PRTM-060.
           ADD 1  TO SUB-1
           MOVE 1 TO SUB-40
           GO TO PRTM-001.
       PRTM-999.
           EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO GLPARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING W-ERC
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
             
      *     MOVE "GLMAST OPENED OK" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
      *     MOVE Ws-GlMaster to WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       OPEN-012.
           OPEN I-O GLPARAMETER-FILE.
           IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE "GLPARAMETER FILE BUSY, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-012.
       OPEN-014.
           PERFORM READ-PARAMETER.
           MOVE GLPA-NAME TO CO-NAME
           PERFORM ENTER-PERIOD-DATES.
       OPEN-015.
           MOVE 1 TO SUB-1
           MOVE GL-BEGDATE (SUB-1) TO SPLIT-DATE
           MOVE SPLIT-YY TO H3-1BEG
           SUBTRACT 1 FROM SPLIT-YY
           MOVE SPLIT-YY TO H3-2BEG.

           MOVE 13 TO SUB-1
           MOVE GLPA-PER (SUB-1) TO SPLIT-DATE
           MOVE SPLIT-YY TO H3-1END
           SUBTRACT 1 FROM SPLIT-YY
           MOVE SPLIT-YY TO H3-2END
           PERFORM OPEN-260.
           CLOSE GLPARAMETER-FILE.
       OPEN-260.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
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
      *
      * END-OF-JOB.
