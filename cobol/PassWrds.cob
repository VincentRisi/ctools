        IDENTIFICATION DIVISION.
        PROGRAM-ID. PassWrds.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. Linux.
        OBJECT-COMPUTER. Linux.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT MENU-PASSWORDS ASSIGN TO WS-MENU
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MU-KEY
               FILE STATUS IS WS-MENU-STATUS.
           SELECT PRINTER-MASTER ASSIGN TO Ws-CoPrinters
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-PRINT-STATUS
               RECORD KEY IS PRNT-KEY.
           SELECT PRINTER-REMOTE ASSIGN TO Ws-CoPrintersRemote
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-PRINT-STATUS
               RECORD KEY IS PRNREM-KEY.
           SELECT DATA-FILE ASSIGN TO WS-DATA
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-DATA-STATUS
               RECORD KEY IS DATA-KEY.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY "ChlfdMenu".
           COPY "ChlfdDataName".
           COPY "ChlfdCoPrinters".
           COPY "ChlfdCoPrintersRemote".

       WORKING-STORAGE SECTION.
       77  SUB-1                 PIC 9(4) VALUE 0.
       77  SUB-2                 PIC 9(4) VALUE 0.
       77  SUB-3                 PIC 9(4) VALUE 0.
       77  SUB-4                 PIC 9(4) VALUE 0.
       77  SUB-5                 PIC 9(4) VALUE 0.
       77  SUB-10                PIC 9(4) VALUE 0.
       77  SUB-30                PIC 9(4) VALUE 0.
       77  SUB-35                PIC 9(4) VALUE 0.
       77  SUB-40                PIC 9(4) VALUE 0.
       77  SUB-46                PIC 9(4) VALUE 0.
       77  POS                   PIC 9(4) VALUE 0.
       77  WS-DATA-NAME          PIC X(60) VALUE "CoDataName".
       77  WS-MESSAGE            PIC X(70) VALUE " ".
       77  WS-DIS-MSG            PIC Z9.
       77  W-ERC                 BINARY-SHORT VALUE 0.
       77  W-DELAY               BINARY-SHORT VALUE 50.
       77  W-COL                 BINARY-SHORT VALUE 0.
       77  W-FRAME               BINARY-SHORT VALUE 0.
       77  W-LINE                BINARY-SHORT VALUE 0.
       77  WS-READS              BINARY-SHORT VALUE 0.
       77  WS-ACCEPT             PIC X(11) VALUE " ".
       01  F-FIELDNAME           PIC X(20).
       01  W-MESSAGE.
           03 FILLER             PIC X(15) VALUE " ".
           03 W-MESS1            PIC X(23) VALUE " ".
           03 W-MESS2            PIC X(6) VALUE " ".
       01  ALPHA-RATE.
           03  AL-RATE           PIC X OCCURS 60.
       01  DATA-RATE.
           03  DAT-RATE          PIC X OCCURS 60.
       01  WS-MENU-STATUS        PIC 99.
       01  WS-PRINT-STATUS       PIC 99.
       01  WS-DATA-STATUS        PIC 99.
       01  MESSAGE-RATE.
           03  MES-AL-RATE      PIC X OCCURS 100.
       01  MES-DATA-RATE.
           03  MES-DAT-RATE     PIC X OCCURS 100.
       01  W-READ-KEY.
           03  WS-PA-KEY         PIC X OCCURS 11.
       01  CTOS_DISPLAY_ACCEPT.
          78  CDA-BLACK   VALUE 0.
          78  CDA-RED     VALUE 1.
          78  CDA-GREEN   VALUE 2.
          78  CDA-YELLOW  VALUE 3.
          78  CDA-BLUE    VALUE 4.
          78  CDA-MAGENTA VALUE 5.
          78  CDA-CYAN    VALUE 6.
          78  CDA-WHITE   VALUE 7.
          03  CDA-ERROR       BINARY-SHORT.
          03  CDA-ROW         BINARY-SHORT.
          03  CDA-COL         BINARY-SHORT.
          03  CDA-DATA        PIC X(80).
          03  CDA-DATALEN     BINARY-SHORT.
          03  CDA-COLOR       BINARY-SHORT.
          03  CDA-ATTR        PIC X.         
          03  CDA-KEY         BINARY-CHAR.
          03  CDA-FILTER      PIC X(10) VALUE
              X"0A1B04010B070C1D1F00".
      *          1 2 3 4 5 6 7 8 9  null terminated
      *  1=RETURN, 2=GO, 3=FINISH, 4=UP, 5=DOWN, 6=CANCEL,
      *  7=NEXT-PAGE, 8=F8, 9=F10
       
       01  W-DEFINE-ESCAPE.
           05  W-ESCAPE-KEY     PIC 99 COMP-X.
           05  W-ESCAPE-TABLE   PIC X(20) VALUE
              X"0A1B04010B070C1D1F00".
      *          1 2 3 4 5 6 7 8 9  null terminated
      *  1=RETURN, 2=GO, 3=FINISH, 4=UP, 5=DOWN, 6=CANCEL,
      *  7=NEXT-PAGE, 8=F8, 9=F10
       01  F-FORMS.
           03  F-ERROR1            BINARY-SHORT.
           03  F-ERROR5            BINARY-SHORT.
           03  F-FILENAME          PIC X(40) VALUE SPACES.
           03  F-CBFILENAME        BINARY-SHORT VALUE 0.
           03  F-FH                BINARY-SHORT.
           03  F-INTEGERZERO       BINARY-SHORT VALUE 0.
           03  F-OPENMODE          PIC X(2) VALUE "mr".
       01  F-EXITSTATE.
           03  F-EXIT-ICH          BINARY-SHORT.
           03  F-EXIT-CH           PIC X.
           03  FILLER              PIC X(13).
      *
       LINKAGE SECTION.
       Copy "ChlfdLinkage".
      *
       PROCEDURE DIVISION USING WS-LINKAGE.
       MAINLINE.
       SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
       SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.

       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM ERROR-020.
           
           IF WS-OPTION = 98
            IF WS-PASSWORD-VALID = "D"
              PERFORM CP-020 THRU CP-999
              GO TO CONTROL-999.
           PERFORM CHECK-DATA-NAMES.

          IF WS-OVER-RIDE = "N"
              GO TO CONTROL-002.
          IF WS-OVER-RIDE = "Y"
              PERFORM CHECK-PRINTER-SERVER-REMOTE.

      * SECTION WHERE WS-OVER-RIDE = Y.
          IF WS-CO-NUMBER = 1 OR = 2 OR =  3 OR =  4 OR =  5 OR =  6
                       OR = 7 OR = 8 OR =  9 OR = 10 OR = 11 OR = 12
           IF WS-SERVER-REMOTE = 1
               PERFORM READ-NEXT-PRINTER
               GO TO CONTROL-005
           ELSE
               PERFORM WORK-OUT-REMOTE-PRINTER-FILE
               PERFORM READ-NEXT-REMOTE-PRINTER
               GO TO CONTROL-005.
               
          IF WS-CO-NUMBER = 3 OR 7 OR 8
      *     IF WS-SERVER-REMOTE = WS-CO-NUMBER
               PERFORM WORK-OUT-REMOTE-PRINTER-FILE
               PERFORM READ-NEXT-REMOTE-PRINTER
               GO TO CONTROL-005.
      *     ELSE
      *         PERFORM READ-NEXT-PRINTER.
      *         GO TO CONTROL-005.

       CONTROL-002.
      * SECTION WHERE WS-OVER-RIDE = N.
          IF WS-CO-NUMBER = 1 OR 2 OR 4 OR 5 OR 6 OR 9 OR 10 OR 11 OR 12
               MOVE 1 TO WS-SERVER-REMOTE
               PERFORM CPSR-010
               PERFORM READ-NEXT-PRINTER
               GO TO CONTROL-005
          ELSE
               MOVE WS-CO-NUMBER TO WS-SERVER-REMOTE
               PERFORM CPSR-010
               PERFORM WORK-OUT-REMOTE-PRINTER-FILE
               PERFORM READ-NEXT-REMOTE-PRINTER.
               GO TO CONTROL-005.
       CONTROL-005.
          IF WS-OVER-RIDE = "Y"
              MOVE 0 TO   WS-PRIORITYNEEDED
                          WS-LASTPRIORITY
                          WS-LASTOPTION
              MOVE " " TO WS-LASTPASSWORD.
          
           IF WS-OPTION = 98
              PERFORM CLEAR-MEM-OF-PREV-CO.
          PERFORM CHECK-PASSWORD.
       CONTROL-999.
           EXIT PROGRAM.
      *
       CLEAR-MEM-OF-PREV-CO SECTION.
       CMOPC-001.
      *     MOVE SPACES TO WS-LINKAGE.
           MOVE 0 TO SUB-1.
       CMOPC-005.
           ADD 1 TO SUB-1.
           IF SUB-1 > 30
              GO TO CMOPC-900.
           MOVE SPACES TO WS-PASSWORDDATA (SUB-1).
           GO TO CMOPC-005.
       CMOPC-900.
           MOVE 1 TO SUB-1.
       CMOPC-999.
           EXIT.
      *
       WORK-OUT-REMOTE-PRINTER-FILE SECTION.
       WORPF-000.
      * "/ctools/data01/CoPrintersRemote"
            MOVE WS-COPRINTERSREMOTE TO ALPHA-RATE.

      *      MOVE WS-COPRINTERSREMOTE TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
       WORPF-010.
            MOVE WS-SERVER-REMOTE TO DATA-RATE.
            MOVE DAT-RATE (1) TO AL-RATE (32)
            MOVE DAT-RATE (2) TO AL-RATE (33).
            
            MOVE ALPHA-RATE TO WS-COPRINTERSREMOTE.

      *      MOVE WS-COPRINTERSREMOTE TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
            MOVE SPACES TO ALPHA-RATE DATA-RATE.
       WORPF-999.
            EXIT.
      *
       CHECK-PASSWORD-INFO SECTION.
       CPI-005.
           MOVE 1 TO SUB-1.
       CPI-010.
           IF WS-MENUNUMBER (SUB-1) NOT = WS-OPTION
            IF SUB-1 < 30
              ADD 1 TO SUB-1
              GO TO CPI-010.
       CPI-020.
           MOVE WS-SELECTION                 TO SUB-3.
           MOVE WS-PA-NUMBER (SUB-1 SUB-3)   TO WS-PASSWORDNEEDED
           MOVE WS-PA-PRIORITY (SUB-1 SUB-3) TO WS-PRIORITYNEEDED.
       CPI-999.
           EXIT.
      *
       CHECK-PASSWORD SECTION.
       CP-001.
           IF WS-1STREAD = "N"
               GO TO CP-020.
           MOVE 2710 TO POS
           DISPLAY "READING ALL PASSWORDS INTO MEMORY...." AT POS.
           MOVE 1 TO SUB-1 SUB-2 SUB-3.
       CP-002.
           MOVE " " TO WS-PA-KEY (SUB-1).
           IF SUB-1 NOT > 10
               ADD 1 TO SUB-1
               GO TO CP-002.
           MOVE 1 TO SUB-1 SUB-2 SUB-3.
       CP-005.
           OPEN INPUT MENU-PASSWORDS.
           IF WS-MENU-STATUS NOT = 0
               MOVE "ERROR ON PASSWORD OPEN" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-MENU-STATUS TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "D" TO WS-PASSWORD-VALID
               CLOSE MENU-PASSWORDS
               MOVE WS-MENU TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               EXIT PROGRAM.
           MOVE 0 TO MU-KEY.
           START MENU-PASSWORDS KEY NOT < MU-KEY
             INVALID KEY NEXT SENTENCE.
       CP-010.
           READ MENU-PASSWORDS NEXT
               AT END NEXT SENTENCE.
           IF WS-MENU-STATUS = 10
               MOVE 0 TO WS-MENU-STATUS
               GO TO CP-015.
           IF WS-MENU-STATUS NOT = 0
               MOVE "ERROR ON PASSWORD READ-NEXT." TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-MENU-STATUS TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "D" TO WS-PASSWORD-VALID
               EXIT PROGRAM.
           MOVE MU-OPTION TO WS-MENUNUMBER (SUB-2)
           MOVE 1 TO SUB-3.
       CP-011.
           MOVE MU-NUMBER (SUB-3)   TO WS-PA-NUMBER (SUB-2 SUB-3)
           MOVE MU-PRIORITY (SUB-3) TO WS-PA-PRIORITY (SUB-2 SUB-3)
           IF SUB-3 < 35
              ADD 1 TO SUB-3
              GO TO CP-011.
           ADD 1 TO SUB-2
           IF SUB-2 > 35
              MOVE "THERE ARE MORE THAN THE 35 ALLOWED MENU'S"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO CP-015.
           GO TO CP-010.
       CP-015.
           CLOSE MENU-PASSWORDS.
       CP-020.
           MOVE 1 TO SUB-1 SUB-2 SUB-3.
           MOVE 2710 TO POS
           DISPLAY "                                        " AT POS.
           PERFORM CHECK-PASSWORD-INFO.
           IF WS-PASSWORDNEEDED = "    "
               MOVE "Y" TO WS-PASSWORD-VALID
               GO TO CP-900.
       CP-450.
           IF WS-OPTION = 98
            IF WS-LASTPASSWORD NOT = " "
              PERFORM CP-910 THRU CP-960
           IF WS-PASSWORD-VALID = "N"
              GO TO CP-500
           ELSE
              GO TO CP-900.
           IF WS-LASTOPTION = 99
            IF WS-LASTPRIORITY NOT < WS-PRIORITYNEEDED
              MOVE "Y" TO WS-PASSWORD-VALID
              GO TO CP-900.
           IF WS-LASTPASSWORD = WS-PASSWORDNEEDED
              MOVE "Y" TO WS-PASSWORD-VALID
              GO TO CP-900.
       CP-500.
           MOVE " " TO W-READ-KEY.
           MOVE 2925 TO POS
           DISPLAY "ENTER A PASSWORD :" AT POS
           MOVE 2945 TO POS
           MOVE 1 TO SUB-2.
       CP-550.
           MOVE ' '       TO CDA-DATA.
           MOVE 11        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 44        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT-PWD.
           MOVE CDA-DATA TO W-READ-KEY.

           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CP-800
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CP-550.
       CP-800.
           IF W-READ-KEY = WS-PASSWORDNEEDED
              MOVE "Y" TO WS-PASSWORD-VALID
              GO TO CP-860.
           MOVE WS-OPTION TO WS-OPTIONSAVE
           MOVE 99        TO WS-OPTION.
       CP-810.
           PERFORM CPI-005 THRU CPI-010.
           MOVE "N" TO WS-PASSWORD-VALID
           MOVE 1 TO SUB-2.
       CP-850.
           IF W-READ-KEY = WS-PA-NUMBER (SUB-1 SUB-2)
            IF WS-PA-PRIORITY (SUB-1 SUB-2) NOT < WS-PRIORITYNEEDED
              MOVE "Y" TO WS-PASSWORD-VALID
              MOVE WS-PA-PRIORITY (SUB-1 SUB-2) TO WS-LASTPRIORITY
              GO TO CP-860.
           IF SUB-2 NOT > 34
              ADD 1 TO SUB-2
              GO TO CP-850.
       CP-860.
           IF WS-PASSWORD-VALID = "N"
               MOVE 0   TO WS-LASTOPTION
                           WS-LASTPRIORITY
               MOVE " " TO WS-LASTPASSWORD
           ELSE
               MOVE WS-OPTION     TO WS-LASTOPTION
               MOVE WS-OPTIONSAVE TO WS-OPTION
               MOVE W-READ-KEY    TO WS-LASTPASSWORD.
       CP-900.
           PERFORM ERROR-020.
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS.
           GO TO CP-999.
       CP-910.
      * THIS SECTION IS USED ONLY TO CHECK THAT THE PASSWORD
      * ALREADY IN MEMORY IS FOUND IN THE 99 SECTION OF THE NEW
      * COMPANY BEING SELECTED
      
           MOVE WS-OPTION TO WS-OPTIONSAVE
           MOVE 99  TO WS-OPTION.
           MOVE "N" TO WS-PASSWORD-VALID
           MOVE 1   TO SUB-2.
           PERFORM CPI-005 THRU CPI-010.
       CP-950.
           IF WS-LASTPASSWORD = WS-PA-NUMBER (SUB-1 SUB-2)
              MOVE "Y" TO WS-PASSWORD-VALID
              MOVE WS-PA-PRIORITY (SUB-1 SUB-2) TO WS-LASTPRIORITY
              GO TO CP-960.
           IF SUB-2 NOT > 34
              ADD 1 TO SUB-2
              GO TO CP-950.
       CP-960.
           IF WS-PASSWORD-VALID = "N"
               MOVE 0   TO WS-LASTOPTION
                           WS-LASTPRIORITY
               MOVE " " TO WS-LASTPASSWORD
           ELSE
               MOVE WS-OPTION                  TO WS-LASTOPTION
               MOVE WS-OPTIONSAVE              TO WS-OPTION
               MOVE WS-PA-NUMBER (SUB-1 SUB-2) TO WS-LASTPASSWORD.
       CP-999.
           EXIT.
      *
       READ-NEXT-PRINTER SECTION.
       RNX-001.
      *     PERFORM CHECK-PRINTER-NODE.
      *     MOVE "PROCESSING SERVER PRINTERS - RNX-001." TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       RNX-002.
           OPEN INPUT PRINTER-MASTER.
           IF WS-PRINT-STATUS NOT = 0
               MOVE "PRINTER SERVER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-COPRINTERS TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-PRINT-STATUS TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNX-002.
       RNX-003.
           MOVE 1 TO SUB-1
                     PRNT-NUMBER.
           START PRINTER-MASTER KEY NOT < PRNT-KEY
             INVALID KEY NEXT SENTENCE.
       RNX-005.
           READ PRINTER-MASTER NEXT
             AT END NEXT SENTENCE.
           IF WS-PRINT-STATUS = 10
               GO TO RNX-900.
           IF WS-PRINT-STATUS NOT = 0
               MOVE "PRINTER SERVER FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNX-005.
           MOVE PRNT-NUMBER       TO WS-PRINTERNUMBER (SUB-1)
           MOVE PRNT-NAME         TO WS-PRINTERNAME (SUB-1)
           MOVE PRNT-TYPE         TO WS-PRINTERTYPE (SUB-1)
           MOVE PRNT-DESC         TO WS-PRINTERLOCATION (SUB-1)
           MOVE PRNT-PROMPT-PAPER TO WS-PRINTERPROMPT (SUB-1)
           MOVE PRNT-CHARS        TO WS-PRINTERCHARS (SUB-1).
           IF SUB-1 < 21
              ADD 1 TO SUB-1
              GO TO RNX-005.
       RNX-900.
           IF SUB-1 < 21
              MOVE " " TO WS-PRINTERDATA (SUB-1)
              ADD 1 TO SUB-1
              GO TO RNX-900.
           CLOSE PRINTER-MASTER.
           PERFORM ERROR1-020.
       RNX-999.
            EXIT.
      *
       READ-NEXT-REMOTE-PRINTER SECTION.
       RNXR-001.
      *     PERFORM CHECK-PRINTER-NODE.
      *     MOVE "PROCESSING REMOTE PRINTERS - RNXR-001." TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       RNXR-002.
           OPEN INPUT PRINTER-REMOTE.
           IF WS-PRINT-STATUS NOT = 0
               MOVE "PRINTER REMOTE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-COPRINTERSREMOTE TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-PRINT-STATUS TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNXR-002.
       RNXR-003.
           MOVE 1 TO SUB-1
                  PRNREM-NUMBER.
           START PRINTER-REMOTE KEY NOT < PRNREM-KEY
             INVALID KEY NEXT SENTENCE.
       RNXR-005.
           READ PRINTER-REMOTE NEXT
             AT END NEXT SENTENCE.
           IF WS-PRINT-STATUS = 10
               GO TO RNXR-900.
           IF WS-PRINT-STATUS NOT = 0
               MOVE "PRINTER REMOTE FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNXR-005.
           MOVE PRNREM-NUMBER       TO WS-PRINTERNUMBER (SUB-1)
           MOVE PRNREM-NAME         TO WS-PRINTERNAME (SUB-1)
           MOVE PRNREM-TYPE         TO WS-PRINTERTYPE (SUB-1)
           MOVE PRNREM-DESC         TO WS-PRINTERLOCATION (SUB-1)
           MOVE PRNREM-PROMPT-PAPER TO WS-PRINTERPROMPT (SUB-1)
           MOVE PRNREM-CHARS        TO WS-PRINTERCHARS (SUB-1).
           IF SUB-1 < 21
              ADD 1 TO SUB-1
              GO TO RNXR-005.
       RNXR-900.
           IF SUB-1 < 21
              MOVE SPACES TO WS-PRINTERDATA (SUB-1)
              ADD 1 TO SUB-1
              GO TO RNXR-900.
           CLOSE PRINTER-REMOTE.
           PERFORM ERROR1-020.
       RNXR-999.
            EXIT.
      *
       CHECK-PRINTER-SERVER-REMOTE SECTION.
       CPSR-001.
           MOVE 2910 TO POS
           DISPLAY "READING ALL PRINTER INFO INTO MEMORY...   " AT POS.
       CPSR-005.
           MOVE 2610 TO POS.
           DISPLAY
           "PLEASE CHOOSE WHERE YOU WISH ALL YOUR REPORTS TO PRINT,"
             AT POS
           MOVE 2710 TO POS
           DISPLAY "FOR THIS SESSION.  TO CHANGE THIS OPTION YOU MUST"
            AT POS
           MOVE 2810 TO POS
           DISPLAY "RETURN TO THIS SCREEN TO RELOAD THE COMPANY AGAIN."
            AT POS
            MOVE 2910 TO POS
           DISPLAY 
           "THE OPTIONS ARE AS FOLLOWS: 1=SERVER PRINTING @ CTJ,"
             AT POS
           MOVE 3010 TO POS
           DISPLAY
           "3=CTN PRINTING, 7=SFJ PRINTING & 8=CSC PRINTING. [ ]"
            AT POS           
            MOVE 3060 TO POS
           
           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 27        TO CDA-ROW.
           MOVE 59        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SERVER-REMOTE.
           
           IF WS-SERVER-REMOTE NOT = 1 AND NOT = 3
                           AND NOT = 7 AND NOT = 8
            GO TO CPSR-005.
       CPSR-010.
            IF WS-SERVER-REMOTE = 1
                MOVE "(CTJ PRN)" TO WS-SERVER-REMOTE-NAME.
            IF WS-SERVER-REMOTE = 3
                MOVE "(CTN PRN)" TO WS-SERVER-REMOTE-NAME.
            IF WS-SERVER-REMOTE = 7
                MOVE "(SFJ PRN)" TO WS-SERVER-REMOTE-NAME.
            IF WS-SERVER-REMOTE = 8
                MOVE "(CSC PRN)" TO WS-SERVER-REMOTE-NAME.
       CPSR-900.
            PERFORM ERROR1-020.
            PERFORM ERROR-020.
            MOVE 2601 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE 2701 TO POS
            DISPLAY WS-MESSAGE AT POS
            MOVE 2801 TO POS
            DISPLAY WS-MESSAGE AT POS.
       CPSR-999.
           EXIT.
      *
       CHECK-PRINTER-NODE SECTION.
       CPN-005.
           MOVE " " TO ALPHA-RATE DATA-RATE.
           MOVE 0   TO SUB-1.
           MOVE WS-COPRINTERS TO ALPHA-RATE.
       CPN-015.
           IF AL-RATE (1) NOT = "{"
              GO TO CPN-999.
       CPN-020.
           ADD 1 TO SUB-1.
           IF SUB-1 NOT > 60
            IF AL-RATE (SUB-1) NOT = "}"
              GO TO CPN-020.
           ADD  1 TO SUB-1.
           MOVE 1 TO SUB-2.
       CPN-025.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2)
           ADD 1 TO SUB-1 SUB-2.
           IF AL-RATE (SUB-1) NOT = " "
            IF SUB-1 NOT > 60
              GO TO CPN-025.
           MOVE DATA-RATE TO WS-COPRINTERS.
       CPN-999.
           EXIT.
      *
       READ-DATAFILE SECTION.
       RC-005.
           MOVE 0 TO SUB-4 DATA-KEY.
           START DATA-FILE KEY NOT < DATA-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-DATA-STATUS NOT = 0
               MOVE "BAD START ON DATA-FILE, GOING TO RC-999."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RC-900.
       RC-010.
           READ DATA-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-DATA-STATUS NOT = 0
            IF WS-DATA-STATUS NOT = 10
               MOVE "DATAFILE BUSY ON READ, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RC-010.
           ADD 1 TO SUB-4.
       RC-900.
           CLOSE DATA-FILE.
       RC-999.
           EXIT.
      *
       CHECK-DATA-NAMES SECTION.
       CDN-005.
           MOVE 2710 TO POS
           DISPLAY "READING ALL DATANAMES INTO MEMORY.....    " AT POS.
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
           MOVE WS-DATA-NAME TO DATA-RATE WS-DATA-FILE.
           MOVE 1            TO SUB-2.
       CDN-025.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF DAT-RATE (SUB-2) NOT = " "
            IF SUB-1 NOT > 60
              GO TO CDN-025.
       CDN-030.
           MOVE ALPHA-RATE TO WS-DATA.
       CDN-031.
           MOVE " " TO ALPHA-RATE DATA-RATE
           MOVE WS-VOL-DIR TO ALPHA-RATE.
       CDN-035.
           PERFORM OPEN-001 THRU OPEN-005.
           IF F-ERROR5 NOT = 0 AND NOT = 220
              EXIT PROGRAM.
           IF WS-DATA-STATUS NOT = 0
              PERFORM RC-900
              EXIT PROGRAM.
           PERFORM RC-005.
       CDN-040.
           PERFORM RC-010.
           
      *     IF SUB-4 > 65
      *        MOVE DATA-NAME TO WS-MESSAGE
      *        PERFORM ERROR-MESSAGE.

           IF WS-DATA-STATUS = 10
               PERFORM RC-900
               GO TO CDN-900.
           
           SUBTRACT SUB-2 FROM SUB-1
           ADD 1 TO SUB-1
           MOVE 1 TO SUB-2
           MOVE DATA-NAME TO DATA-RATE
           PERFORM CDN-025
           PERFORM CHECK-DATA-NAME-POSITION           
           
      *     IF SUB-4 > 70
      *        MOVE ALPHA-RATE TO WS-MESSAGE
      *        PERFORM ERROR-MESSAGE.

           PERFORM CDN-031.
           
           GO TO CDN-040.
       CDN-900.
           PERFORM ERROR-020.
       CDN-999.
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
      * THIS BELOW FOR CTOS - NOT LINUX
       OPEN-NODE-FILE SECTION.
       ONF-001.
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
      * THIS BELOW FOR CTOS - NOT LINUX
       OPEN-FILES SECTION.
       OPEN-001.
            PERFORM OPEN-NODE-FILE.
            IF F-ERROR5 NOT = 0 AND NOT = 220
                PERFORM ERROR1-020
                PERFORM ERROR-020
                MOVE
          "THE DIGINET LINK IS NOT CURRENTLY RUNNING, 'CANCEL' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE "ERROR MESSAGE STATUS = " TO W-MESS1
                MOVE F-ERROR5                  TO W-MESS2
                MOVE W-MESSAGE                 TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                PERFORM ERROR-020
                MOVE "B" TO WS-PASSWORD-VALID.
       OPEN-005.
           IF F-ERROR5 = 0 OR = 220
                 OPEN INPUT DATA-FILE.
            IF WS-DATA-STATUS NOT = 00
               MOVE "DATAFILE BUSY ON OPEN, 'ESC ' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE "D" TO WS-PASSWORD-VALID
               GO TO OPEN-005.
       OPEN-999.
       EXIT.
      ******************************************************************
      * CHANGE THE ENTRY BELOW WHEN ADDING EXTRA FILE NAMES IN         *
      * THE CHLFDLINKAGE SECTION.  THE SUB-4 ENTRY MUST BE ADDED HERE  *
      * LIKE:SUB4=6 EQUALS WS-STOCK.  ADD THE ENTRY INTO LINKAGE       *
      * LIKE:03 WS-STOCK   PIC X(40).  NB ADD THE NEW ENTRY INTO       *
      * DATA NAME PROGRAM IN THE SYSTEM MANAGERS MENU TO DEFINE WHERE  *
      * ON THE DISK THE FILE IS FOUND LIKE - StMaster.  /ctools/data01 *
      * GETS ADDED AUTOMATICALLY BY THE "COMPANY VOL / DIR INFO" PRGRAM*
      *                                                                *
      *  THEN RE-COMPILE THIS PROGRAM WITH ENTRIES IN FOLLOWING FILES  *
      *  <COPY>CHECKDATANAMEPOSITION & <COPY>CHLFDLINKAGE CHANGED.     *
      * ALSO RECOMPILE MainCont.cob                                    *
      *                                                                *
      * IN LINUX DO "make clean" AND THEN "make -k" then               *
      * "make install" TO RECOMPILE                                    *
      * ALL PROGRAMS TO TAKE CARE OF THE CHANGES                       *
      ******************************************************************
       Copy "CheckDataNamePosition".
       Copy "ReadMenuKBD".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".
      * END-OF-JOB
