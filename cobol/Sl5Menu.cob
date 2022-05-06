        IDENTIFICATION DIVISION.
        PROGRAM-ID. Sl5Menu.
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
            MOVE 2 TO WS-MODE
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
               MOVE "35"   TO WS-ANSWER.
            MOVE WS-ANSWER TO WS-SELECTION
            MOVE 34        TO WS-OPTION
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
           IF WS-ANSWER = "35"
                MOVE "CoStffIq" TO WS-PROGRAM.
            IF WS-ANSWER = " 1"
                MOVE "SlInRgRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 2"
                MOVE "SlDistRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 3"
                MOVE "SlDyExRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 4"
                MOVE "SlGstxRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 5"
                MOVE "SlMastRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 6"
                MOVE "SlSoByRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 7"
                MOVE "StReLoRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 8"
                MOVE "SlAnCoRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 9"
                MOVE "SlSManRp" TO WS-PROGRAM.
            IF WS-ANSWER = "10"
                MOVE "SlPOrdRp" TO WS-PROGRAM.
            IF WS-ANSWER = "11"
                MOVE "DrAgeARp" TO WS-PROGRAM.
            IF WS-ANSWER = "12"
                MOVE "StSGmRp" TO WS-PROGRAM.
            IF WS-ANSWER = "13"
                MOVE "StValuRp" TO WS-PROGRAM.
            IF WS-ANSWER = "14"
                MOVE "StMvPeRp" TO WS-PROGRAM.
            IF WS-ANSWER = "15"
                MOVE "DrStatRp" TO WS-PROGRAM.
            IF WS-ANSWER = "16"
                MOVE "StAnalRp" TO WS-PROGRAM.
            IF WS-ANSWER = "17"
                MOVE "StQyAdRp" TO WS-PROGRAM.
            IF WS-ANSWER = "18"
                MOVE "DrDiscRp" TO WS-PROGRAM.
            IF WS-ANSWER = "19"
                MOVE "SlPerEnd" TO WS-PROGRAM.

            IF WS-ANSWER = "20"
                Move "SortDr.Sub" TO Ws-Data-Name
                MOVE
           "THIS PROCESS HAS BEEN BLOCKED IN THIS APP VERSION, 'Esc'" &
           " TO EXIT" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE " " TO F-NAMEFIELD WS-ANSWER
                GO TO GET-010.
            IF WS-ANSWER = "21"
                Move "SortData.Sub" TO Ws-Data-Name
                MOVE
           "THIS PROCESS HAS BEEN BLOCKED IN THIS APP VERSION, 'Esc'" &
           " TO EXIT" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE " " TO F-NAMEFIELD WS-ANSWER
                GO TO GET-010.
            IF WS-ANSWER = "22"
                Move "IReoData.Sub" TO Ws-Data-Name
                MOVE
           "THIS PROCESS HAS BEEN BLOCKED IN THIS APP VERSION, 'Esc'" &
           " TO EXIT" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE " " TO F-NAMEFIELD WS-ANSWER
                GO TO GET-010.
            IF WS-ANSWER = "23"
                Move "MonthEnd.sh" TO Ws-Data-Name
                PERFORM SETUP-MONTH-FILES
                MOVE "The MonthEnd Has Been Run....." TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE " Press 'Esc' To Continue With Other Processes."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020 
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD WS-ANSWER
                Go To GET-010.
      *          PERFORM GET-100.
            IF WS-ANSWER = "24"
                Move "YearEnd.sh" TO Ws-Data-Name
                PERFORM SETUP-MONTH-FILES
                MOVE "The Yearend Has Been Run ....." TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE " Press 'Esc' To Continue With Other Processes."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020 
                PERFORM DISPLAY-FORM
                MOVE " " TO F-NAMEFIELD WS-ANSWER
                Go To GET-010.
      *          PERFORM GET-100.
       GET-050.
           Call Ws-Program Using Ws-Linkage.
            PERFORM CLEAR-SCREEN
            CANCEL WS-PROGRAM
            PERFORM DISPLAY-FORM
            MOVE " " TO F-NAMEFIELD WS-ANSWER
            GO TO GET-010.
       GET-100.
            Perform Check-Data-Names.
            CALL "OPENFILELL" USING F-ERROR1
                                    F-FH
                                    F-FILENAME
                                    F-CBFILENAME
                                    F-FILENAME
                                    F-INTEGERZERO
                                    F-OPENMODE.
            IF F-ERROR1 NOT = 0
                DISPLAY "OPENFILE ERROR IN SUBMIT FILE."
                DISPLAY F-ERROR1
                STOP RUN.
            CALL "SETSYSINMODE" USING WS-CTOS-ERROR
                                      WS-MODE
                                      F-FH.
            STOP RUN.
       GET-999.
            EXIT.
      *
       SETUP-MONTH-FILES SECTION.
       SUQFD-002.
            IF WS-ANSWER = "23"
              MOVE "./MonthEnd" TO ALPHA-RATE
              MOVE WS-CO-NUMBER TO DATA-RATE
              MOVE DAT-RATE (1) TO AL-RATE (11)
              MOVE DAT-RATE (2) TO AL-RATE (12)
              MOVE "."          TO AL-RATE (13)
              MOVE "s"          TO AL-RATE (14)
              MOVE "h"          TO AL-RATE (15)
              MOVE ALPHA-RATE   TO WS-DATA-NAME
            ELSE 
              MOVE "./YearEnd" TO ALPHA-RATE
              MOVE WS-CO-NUMBER TO DATA-RATE
              MOVE DAT-RATE (1) TO AL-RATE (10)
              MOVE DAT-RATE (2) TO AL-RATE (11)
              MOVE "."          TO AL-RATE (12)
              MOVE "s"          TO AL-RATE (13)
              MOVE "h"          TO AL-RATE (14)
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
       OPEN-FILES SECTION.
       OPEN-010.
            Move Ws-Forms-Name   TO F-FileName
            Move Ws-cbForms-name TO F-CbFileName.
            MOVE "Sl5Menu"       TO F-FORMNAME
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
       Copy "CheckMenuDataNames".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
