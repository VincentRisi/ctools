        IDENTIFICATION DIVISION.
        PROGRAM-ID. BaUpMenu.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        REPOSITORY. 
           FUNCTION ALL INTRINSIC.
        SPECIAL-NAMES.
          CRT STATUS IS W-CRTSTATUS.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT PARAMETER-FILE ASSIGN TO Ws-Parameter
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-PARAMETER-STATUS
               RECORD KEY IS PA-KEY.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdParam.

       WORKING-STORAGE SECTION.
       77  WS-QUES-PAUSE-BACKUP   PIC X VALUE " ".
       77  WS-COMPYYMM            PIC X(4) VALUE " ".
       77  WS-DRYYMM              PIC X(4) VALUE " ".
       77  W-BACKUP-DELAY         PIC 9(4) COMP-X.
       01  W-CRTSTATUS            PIC 9(4) value 0.
       Copy "WsMenuDateInfo".
       01  WS-PARAMETER-STATUS.
           03  WS-PARAMETER-ST1     PIC X.
           03  WS-PARAMETER-ST2     PIC X.
       01  WS-COMMAND-LINE          PIC X(256).                                    
       01  W-PRINTCOMMAND.
           03  W-PRINTCOM1A        PIC X(6) VALUE SPACES.
           03  W-PRINTCOM1         PIC X(90) VALUE SPACES.
           03  W-PRINTCOM2         PIC X(100) VALUE SPACES.
       01  W-STATUS             PIC 9(4) BINARY COMP.
       LINKAGE SECTION.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
        CONTROL-PARAGRAPH SECTION.
        CONTROL-000.
            PERFORM OPEN-FILES
            PERFORM CLEAR-SCREEN.
        CONT-010.
            PERFORM DISPLAY-FORM
            PERFORM GET-DATA
            GO TO CONT-010.
      *
       GET-DATA SECTION.
       GET-005.
            MOVE 2           TO WS-MODE.
            MOVE "         " TO F-NAMEFIELD
            MOVE "SELECTION" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            PERFORM WRITE-FIELD-ALPHA.
       GET-010.
            MOVE "                      " TO F-NAMEFIELD
            MOVE "SELECTION" TO F-FIELDNAME
            MOVE 9 TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"1D"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = "  "
                GO TO GET-010.
            MOVE F-NAMEFIELD TO WS-NUMBER-CHECK.
            IF WS-NU2 = " "
               MOVE WS-NU1 TO WS-NU2
               MOVE " "    TO WS-NU1
               MOVE WS-NUMBER-CHECK TO WS-ANSWER
            ELSE
               MOVE F-NAMEFIELD TO WS-ANSWER.
            IF WS-ANSWER NOT = " 1" AND NOT = " 2" AND NOT = " 3"
                     AND NOT = " 4" AND NOT = " 5" AND NOT = " 6"
                     AND NOT = " 7" AND NOT = " 8" AND NOT = " 9"
                     AND NOT = "10"
                     AND NOT = "XX"
                MOVE "Selection Must Be Between 1 & 10, Re-Enter."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE "  " TO F-NAMEFIELD WS-ANSWER
                GO TO GET-010.

            IF WS-ANSWER = "XX"
               MOVE "35"   TO WS-ANSWER.
            MOVE WS-ANSWER TO WS-SELECTION
            MOVE 90        TO WS-OPTION
            MOVE "N"       TO WS-PASSWORD-VALID.
            IF F-EXIT-CH = X"1D"
                MOVE "Y"   TO WS-OVER-RIDE
            ELSE
                MOVE "N"   TO WS-OVER-RIDE.
         Perform Check-Password.
      *    Move "N" to Ws-1stRead.
      *    MOVE "PassWrds" TO WS-PROGRAM
      *    CALL WS-PROGRAM USING Ws-Linkage.

            IF WS-PASSWORD-VALID = "N"
                MOVE "INVALID PASSWORD ENTERED, PLEASE RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-010.
      *      CANCEL WS-PROGRAM.
            IF WS-ANSWER = "35"
                MOVE "CoStffIq" TO WS-PROGRAM.
            IF WS-ANSWER = " " OR = "0"
                GO TO GET-005.
            IF WS-ANSWER = " 1"
                Move "Backup.Sub" TO Ws-Data-Name
                PERFORM GET-100.
            IF WS-ANSWER = " 2"
                Move "Restore.Sub" TO Ws-Data-Name
                PERFORM GET-100.
            IF WS-ANSWER = " 3"
                MOVE 
              "MAKE SURE THE USB DRIVE IS PLUGGED IN, 'ESC' TO CONFIRM."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                MOVE 
            "THE COMMAND PROMPT WILL BE DISPLAYED WHILE BACKUP HAPPENS."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE "Backup03.sh " TO WS-DATA-NAME 
                PERFORM SETUP-BACKUP-FILES
                MOVE 
              "BACKUP HAS BEEN RUN, CONTINUE WITH OTHER PROCESSES."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                GO TO GET-005.
            IF WS-ANSWER = " 5"
                MOVE 
              "MAKE SURE THE USB DRIVE IS PLUGGED IN, 'ESC' TO CONFIRM."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                MOVE 
            "THE COMMAND PROMPT WILL BE DISPLAYED WHILE BACKUP HAPPENS."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE "Backup05.sh " TO WS-DATA-NAME 
                PERFORM SETUP-BACKUP-FILES
                MOVE 
              "BACKUP HAS BEEN RUN, CONTINUE WITH OTHER PROCESSES."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                GO TO GET-005.
            IF WS-ANSWER = " 6"
                MOVE 
              "MAKE SURE THE USB DRIVE IS PLUGGED IN, 'ESC' TO CONFIRM."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                MOVE 
            "THE COMMAND PROMPT WILL BE DISPLAYED WHILE BACKUP HAPPENS."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE "Backup06.sh " TO WS-DATA-NAME 
                PERFORM SETUP-BACKUP-FILES
                MOVE 
              "BACKUP HAS BEEN RUN, CONTINUE WITH OTHER PROCESSES."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                GO TO GET-005.
            IF WS-ANSWER = " 7"
                MOVE 
              "MAKE SURE THE USB DRIVE IS PLUGGED IN, 'ESC' TO CONFIRM."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                MOVE 
            "THE COMMAND PROMPT WILL BE DISPLAYED WHILE BACKUP HAPPENS."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE "Backup07.sh " TO WS-DATA-NAME 
                PERFORM SETUP-BACKUP-FILES
                MOVE 
              "BACKUP HAS BEEN RUN, CONTINUE WITH OTHER PROCESSES."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                GO TO GET-005.
      *      IF WS-ANSWER = " 3"
      *          Move "TapeBackup.Sub" TO Ws-Data-Name
      *          PERFORM CHECK-FOR-BACKUP-PAUSE
      *          PERFORM GET-100.
      *      IF WS-ANSWER = " 4"
      *          Move "TapeRestore.Sub" TO Ws-Data-Name
      *          PERFORM GET-100.
      *      IF WS-ANSWER = " 5"
      *          Move "TapeBackupVol.Sub" TO Ws-Data-Name
      *          PERFORM GET-100.
      *      IF WS-ANSWER = " 6"
      *          Move "TapeBackupFax.Sub" TO Ws-Data-Name
      *          PERFORM GET-100.
      *      IF WS-ANSWER = " 7"
      *          Move "TapeBackupMonth.Sub" TO Ws-Data-Name
      *          PERFORM GET-100.
                
            IF WS-ANSWER = " 8"
                Move "FormatFloppy.Sub" TO Ws-Data-Name
                PERFORM GET-100.
            IF WS-ANSWER = " 9"
                Move "TapeErase.Sub" TO Ws-Data-Name
                PERFORM GET-100.
            IF WS-ANSWER = "10"
                Move "TapeRetension.Sub" TO Ws-Data-Name
                PERFORM GET-100.
       GET-100.
            Perform Check-Data-Names.
            STOP RUN.
       GET-999.
            EXIT.
      *
       CHECK-FOR-BACKUP-PAUSE SECTION.
       CFBP-005.
           IF WS-QUES-PAUSE-BACKUP NOT = "Y"
                GO TO CFBP-999.
       CFBP-010.
           MOVE 2710 TO POS
           DISPLAY 
           "The Backup Has Been Set On a 30 Min DELAY, You May leave."
            AT POS.

           CALL "C$SLEEP" USING 3600.
       
      *     MOVE 18000 TO W-BACKUP-DELAY.
      *     CALL "&DELAY" USING W-ERC
      *                         W-BACKUP-DELAY.
       CFBP-999.
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
            IF WS-PARAMETER-ST1 = 35 or 47 or 51
               MOVE "N" TO WS-QUES-PAUSE-BACKUP
               GO TO RINVQUES-999.
            IF WS-PARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-PARAMETER-ST1
               MOVE "Parameter Busy RINVQUES, Press 'Esc' To Retry."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RINVQUES-010.
       RINVQUES-900.
            MOVE INVQUES-PAUSE-BACKUP TO WS-QUES-PAUSE-BACKUP.
       RINVQUES-999.
            EXIT.
      *
       SETUP-BACKUP-FILES SECTION.
       SUQFD-001.
          IF WS-ANSWER NOT = " 7"
             MOVE WS-DATA-NAME  TO WS-COMMAND-LINE
             GO TO SUQFD-020.
       SUQFD-002.
          MOVE 
          "Enter the Year & Month numbers To Rename LAST MONTHS Company"
              TO WS-MESSAGE
              PERFORM ERROR1-000
          MOVE
          "files for e.g. 1506 for 2015 June." TO WS-MESSAGE
             PERFORM ERROR-000.
       SUQFD-003.
          MOVE 2610 TO POS
          DISPLAY "ENTER YEAR MONTH AS YYMM. : [    ]" AT POS

           MOVE ' '       TO CDA-DATA.
           MOVE 4         TO CDA-DATALEN.
           MOVE 23        TO CDA-ROW.
           MOVE 38        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COMPYYMM.

            IF WS-COMPYYMM = " "
               GO TO SUQFD-003.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO SUQFD-005
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO SUQFD-003.
       SUQFD-005.
          MOVE 
          "Enter the Year & Month numbers To Rename THIS MONTHS Debtor"
              TO WS-MESSAGE
              PERFORM ERROR1-000
          MOVE
          "files for e.g. 1507 for 2015 July." TO WS-MESSAGE
             PERFORM ERROR-000.
       SUQFD-006.
          MOVE 2610 TO POS
          DISPLAY "ENTER YEAR MONTH AS YYMM. : [    ]" AT POS

           MOVE ' '       TO CDA-DATA.
           MOVE 4         TO CDA-DATALEN.
           MOVE 23        TO CDA-ROW.
           MOVE 38        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-DRYYMM.

            IF WS-DRYYMM = " "
               GO TO SUQFD-005.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO SUQFD-010
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO SUQFD-005.
       SUQFD-010.
          PERFORM ERROR-020
          PERFORM ERROR1-020
          MOVE 2610 TO POS
          DISPLAY WS-MESSAGE AT POS.
          MOVE 
          CONCATENATE('./Backup07.sh ', TRIM(WS-COMPYYMM), ' '
              TRIM(WS-DRYYMM))  TO WS-COMMAND-LINE.
      *    DISPLAY WS-COMMAND-LINE.  
      *    ACCEPT W-ENTER.
      *The variables which will be specified are:
      *$1 = The Date Of Last Month               e.g. 1612 
      *$2 = The Date Of This Month               e.g. 1701
       SUQFD-020.
          IF WS-ANSWER = " 3"
              PERFORM CHECK-FOR-BACKUP-PAUSE.
          CALL "SYSTEM" USING   WS-COMMAND-LINE
                       RETURNING W-STATUS.
       SUQFD-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O PARAMETER-FILE.
           IF WS-PARAMETER-ST1 NOT = 0 
              MOVE 0 TO WS-PARAMETER-ST1
              MOVE "PARAMETER FILE BUSY, 'Esc' TO RETRY." TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
           PERFORM READ-INVQUES-FILE.
           CLOSE PARAMETER-FILE.
        OPEN-010.
            Move Ws-Forms-Name   TO F-FileName
            Move Ws-cbForms-name TO F-CbFileName.
            MOVE "BaUpMenu"      TO F-FORMNAME
            MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
      *      STOP RUN.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "PassChck.cob".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "MenuClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "Error5Message".
       Copy "DisplayProgNum".
       Copy "CheckMenuDataNames".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
