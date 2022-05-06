        IDENTIFICATION DIVISION.
        PROGRAM-ID. Sl6Menu.
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
      *
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  WS-COMPYYMM              PIC X(4) VALUE " ".
       77  WS-DRYYMM                PIC X(4) VALUE " ".
       01  W-CRTSTATUS              PIC 9(4) value 0.
       01  WS-COMMAND-LINE          PIC X(256).                                    
       01  W-STATUS                 PIC 9(4) BINARY COMP.
       Copy "WsMenuDateInfo".

       LINKAGE SECTION.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
          PERFORM OPEN-FILES
          PERFORM CLEAR-SCREEN.
       CONTROL-010.
          PERFORM DISPLAY-FORM
          PERFORM GET-DATA
          GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-010.
      *      MOVE 2 TO WS-MODE
            MOVE "                      " TO F-NAMEFIELD
            MOVE "SELECTION" TO F-FIELDNAME
            MOVE 9 TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"1D"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
            MOVE 2 TO F-CBFIELDLENGTH
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
                     AND NOT = "10" AND NOT = "11" AND NOT = "12"
                     AND NOT = "13" AND NOT = "14" AND NOT = "15"
                     AND NOT = "16" AND NOT = "17" AND NOT = "18"
                     AND NOT = "19" AND NOT = "20" AND NOT = "21"
                     AND NOT = "22" AND NOT = "23" AND NOT = "24"
                     AND NOT = "XX"
                MOVE "Selection Must Be Between 1 & 24, Re-Enter."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE "  " TO F-NAMEFIELD WS-ANSWER
                GO TO GET-010.
     
            IF WS-ANSWER = "XX"
               MOVE "31"   TO WS-ANSWER.
            MOVE WS-ANSWER TO WS-SELECTION
            MOVE 35        TO WS-OPTION
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
      *     CANCEL WS-PROGRAM.
           PERFORM DISPLAY-PR-NO.
           IF WS-ANSWER = "31"
               MOVE "CoStffIq" TO WS-PROGRAM.
           IF WS-ANSWER = " 1"
               MOVE "SlIRegMt" TO WS-PROGRAM.
           IF WS-ANSWER = " 2"
               MOVE "SlRgChMt" TO WS-PROGRAM.
           IF WS-ANSWER = " 3"
               MOVE "SlSBChMt" TO WS-PROGRAM.
           IF WS-ANSWER = " 4"
                MOVE "SlInCrLy" TO WS-PROGRAM.
           IF WS-ANSWER = " 5"
                MOVE "SlInAcLy" TO WS-PROGRAM.
           IF WS-ANSWER = " 6"
                MOVE "SlInStLy" TO WS-PROGRAM.
           IF WS-ANSWER = " 7"
                MOVE "SlPoShLy" TO WS-PROGRAM.
           IF WS-ANSWER = " 8"
                MOVE "SlInOrLy" TO WS-PROGRAM.
           IF WS-ANSWER = " 9"
                MOVE "SlInDlLy" TO WS-PROGRAM.
           IF WS-ANSWER = "10"
                MOVE "SlIRegLy" TO WS-PROGRAM.
           IF WS-ANSWER = "11"
                MOVE "SlRgNaMy" TO WS-PROGRAM.
           IF WS-ANSWER = "12"
                MOVE "SlRgNaLy" TO WS-PROGRAM.
           IF WS-ANSWER = "13"
                MOVE "SlRgLyMt" TO WS-PROGRAM.
           IF WS-ANSWER = "14"
                MOVE "SlRgChLy" TO WS-PROGRAM.
           IF WS-ANSWER = "15"
                MOVE "SlRgAcLy" TO WS-PROGRAM.
           IF WS-ANSWER = "16"
                MOVE "CoPuByMt" TO WS-PROGRAM.
           IF WS-ANSWER = "17"
                MOVE "SlRegisterMv" TO WS-PROGRAM.
           IF WS-ANSWER = "18"
                Move "Auto.sh" TO Ws-Data-Name
                PERFORM SETUP-AUTO-MONTH-FILES
                MOVE "The Auto Allocation Has Been Run....."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE " Press 'Esc' To Continue With Other Processes."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020 
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD WS-ANSWER
                Go To GET-010.
           IF WS-ANSWER = "19"
                Move "Branch.sh" TO Ws-Data-Name
                PERFORM SETUP-BRANCH-MONTH-FILES
                MOVE "The Branch Update Has Been Run....."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE " Press 'Esc' To Continue With Other Processes."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020 
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD WS-ANSWER
                Go To GET-010.
           IF WS-ANSWER = "20"
                Move "DrFaxSend.sh" TO Ws-Data-Name
                PERFORM SETUP-FAX-MONTH-FILES
                MOVE "The Debtor Overdue Faxes Have Been Sent ....."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE " Press 'Esc' To Continue With Other Processes."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020 
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD WS-ANSWER
                Go To GET-010.
           IF WS-ANSWER = "21"
                Move "Doculogix.sh" TO Ws-Data-Name
                PERFORM SETUP-DOCU-MONTH-FILES
                MOVE "DocuTrieve Files Converted for Month-End....."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE " Press 'Esc' To Continue With Other Processes."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020 
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD WS-ANSWER
                Go To GET-010.
           IF WS-ANSWER = "22"
                Move "DDCreateImage.sh" TO Ws-Data-Name
                MOVE WS-DATA-NAME       TO WS-COMMAND-LINE
                CALL "SYSTEM" USING        WS-COMMAND-LINE
                             RETURNING      W-STATUS
                MOVE "DD Has Been Run to Create a Mirror Disk....."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE " Press 'Esc' To Continue With Other Processes."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020 
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD WS-ANSWER
                Go To GET-010.
           IF WS-ANSWER = "23"
                Move "StMinMax.sh" TO Ws-Data-Name
                PERFORM SETUP-MINMAX-FILES
                MOVE "The Stock Min/Max YTD Update has Run....."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE " Press 'Esc' To Continue With Other Processes."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020 
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD WS-ANSWER
                Go To GET-010.
           IF WS-ANSWER = "24"
                Move "StMinMaxLy.sh" TO Ws-Data-Name
                PERFORM SETUP-MINMAX-LY-FILES
                MOVE "The Stock Min/Max L/Yr Update has Run....."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE " Press 'Esc' To Continue With Other Processes."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020 
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD WS-ANSWER
                Go To GET-010.

           Call Ws-Program Using Ws-Linkage.
           PERFORM CLEAR-SCREEN
           CANCEL WS-PROGRAM
           PERFORM DISPLAY-FORM
           MOVE " " TO F-NAMEFIELD WS-ANSWER
           GO TO GET-010.
       GET-999.
            EXIT.
      *
       SETUP-AUTO-MONTH-FILES SECTION.
       SUQFD-002.
           MOVE "./Auto" TO ALPHA-RATE
           MOVE WS-CO-NUMBER TO DATA-RATE
           MOVE DAT-RATE (1) TO AL-RATE (7)
           MOVE DAT-RATE (2) TO AL-RATE (8)
           MOVE "."          TO AL-RATE (9)
           MOVE "s"          TO AL-RATE (10)
           MOVE "h"          TO AL-RATE (11)
           MOVE ALPHA-RATE   TO WS-DATA-NAME.

           MOVE CONCATENATE(WS-DATA-NAME) TO WS-COMMAND-LINE.
      
      *    MOVE WS-COMMAND-LINE TO WS-MESSAGE
      *    PERFORM ERROR-MESSAGE.
      *    ACCEPT WS-ACCEPT.
       SUQFD-020.
          CALL "SYSTEM" USING   WS-COMMAND-LINE
                       RETURNING W-STATUS.
       SUQFD-999.
           EXIT.
      *
       SETUP-BRANCH-MONTH-FILES SECTION.
       SUBMF-002.
           MOVE "./Branch" TO ALPHA-RATE
           MOVE WS-CO-NUMBER TO DATA-RATE
           MOVE DAT-RATE (1) TO AL-RATE (9)
           MOVE DAT-RATE (2) TO AL-RATE (10)
           MOVE "."          TO AL-RATE (11)
           MOVE "s"          TO AL-RATE (12)
           MOVE "h"          TO AL-RATE (13)
           MOVE ALPHA-RATE   TO WS-DATA-NAME.

           MOVE CONCATENATE(WS-DATA-NAME) TO WS-COMMAND-LINE.
      
      *    MOVE WS-COMMAND-LINE TO WS-MESSAGE
      *    PERFORM ERROR-MESSAGE.
      *    ACCEPT WS-ACCEPT.
       SUBMF-020.
          CALL "SYSTEM" USING   WS-COMMAND-LINE
                       RETURNING W-STATUS.
       SUBMF-999.
           EXIT.
      *
       SETUP-FAX-MONTH-FILES SECTION.
       SUFMF-002.
           MOVE "./DrFaxSend" TO ALPHA-RATE
           MOVE WS-CO-NUMBER TO DATA-RATE
           MOVE DAT-RATE (1) TO AL-RATE (12)
           MOVE DAT-RATE (2) TO AL-RATE (13)
           MOVE "."          TO AL-RATE (14)
           MOVE "s"          TO AL-RATE (15)
           MOVE "h"          TO AL-RATE (16)
           MOVE ALPHA-RATE   TO WS-DATA-NAME.

           MOVE CONCATENATE(WS-DATA-NAME) TO WS-COMMAND-LINE.
      
      *    MOVE WS-COMMAND-LINE TO WS-MESSAGE
      *    PERFORM ERROR-MESSAGE.
      *    ACCEPT WS-ACCEPT.
       SUFMF-020.
          CALL "SYSTEM" USING   WS-COMMAND-LINE
                       RETURNING W-STATUS.
       SUFMF-999.
           EXIT.
      *
       SETUP-DOCU-MONTH-FILES SECTION.
       SUDFD-002.
          MOVE 
          "Enter the Year & Month numbers To Rename Statements to Email"
              TO WS-MESSAGE
              PERFORM ERROR1-000
          MOVE
          "files, for e.g. 1506 for 2015 June." TO WS-MESSAGE
             PERFORM ERROR-000.
       SUDFD-003.
          MOVE 2710 TO POS
          DISPLAY "ENTER YEAR MONTH AS YYMM. : [    ]" AT POS

           MOVE ' '       TO CDA-DATA.
           MOVE 4         TO CDA-DATALEN.
           MOVE 24        TO CDA-ROW.
           MOVE 38        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-COMPYYMM.

            IF WS-COMPYYMM = " "
               GO TO SUDFD-003.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO SUDFD-010
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO SUDFD-003.
       SUDFD-010.
          PERFORM ERROR-020
          PERFORM ERROR1-020
          MOVE 2710 TO POS
          DISPLAY WS-MESSAGE AT POS.
          MOVE 
          CONCATENATE('./Doculogix.sh ', TRIM(WS-COMPYYMM), ' '
              TRIM(WS-DRYYMM))  TO WS-COMMAND-LINE.
      *    DISPLAY WS-COMMAND-LINE.  
      *    ACCEPT W-ENTER.
      *The variables which will be specified are:
      *$1 = The Year / Month numbers             e.g. 1702
       SUDFD-020.
          CALL "SYSTEM" USING   WS-COMMAND-LINE
                       RETURNING W-STATUS.
       SUDFD-999.
           EXIT.
      *
       SETUP-MINMAX-FILES SECTION.
       SUMXFD-002.
           MOVE "./StMinMax" TO ALPHA-RATE
           MOVE WS-CO-NUMBER TO DATA-RATE
           MOVE DAT-RATE (1) TO AL-RATE (11)
           MOVE DAT-RATE (2) TO AL-RATE (12)
           MOVE "."          TO AL-RATE (13)
           MOVE "s"          TO AL-RATE (14)
           MOVE "h"          TO AL-RATE (15)
           MOVE ALPHA-RATE   TO WS-DATA-NAME.

           MOVE CONCATENATE(WS-DATA-NAME) TO WS-COMMAND-LINE.
      
      *    MOVE WS-COMMAND-LINE TO WS-MESSAGE
      *    PERFORM ERROR-MESSAGE.
      *    ACCEPT WS-ACCEPT.
       SUMXFD-020.
          CALL "SYSTEM" USING   WS-COMMAND-LINE
                       RETURNING W-STATUS.
       SUMXFD-999.
           EXIT.
      *
       SETUP-MINMAX-LY-FILES SECTION.
       SUMXLYR-002.
           MOVE "./StMinMaxLy" TO ALPHA-RATE
           MOVE WS-CO-NUMBER   TO DATA-RATE
           MOVE DAT-RATE (1)   TO AL-RATE (13)
           MOVE DAT-RATE (2)   TO AL-RATE (14)
           MOVE "."            TO AL-RATE (15)
           MOVE "s"            TO AL-RATE (16)
           MOVE "h"            TO AL-RATE (17)
           MOVE ALPHA-RATE     TO WS-DATA-NAME.

           MOVE CONCATENATE(WS-DATA-NAME) TO WS-COMMAND-LINE.
      
      *    MOVE WS-COMMAND-LINE TO WS-MESSAGE
      *    PERFORM ERROR-MESSAGE.
      *    ACCEPT WS-ACCEPT.
       SUMXLYR-020.
          CALL "SYSTEM" USING   WS-COMMAND-LINE
                       RETURNING W-STATUS.
       SUMXLYR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-010.
            Move Ws-Forms-Name   TO F-FileName
            Move Ws-cbForms-name TO F-CbFileName.
            MOVE "Sl6Menu"       TO F-FORMNAME
            MOVE 7               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
      *     STOP RUN.
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
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
