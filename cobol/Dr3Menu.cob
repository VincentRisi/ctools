       IDENTIFICATION DIVISION.
       PROGRAM-ID. Dr3Menu.
       AUTHOR. CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. B20.
       OBJECT-COMPUTER. B20.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       Copy "WsMenuDateInfo".

       LINKAGE SECTION.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
          PERFORM OPEN-FILES.
          PERFORM CLEAR-SCREEN.
       CONTROL-010.
          PERFORM DISPLAY-FORM
          PERFORM GET-DATA.
          GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-010.
            MOVE "                      " TO F-NAMEFIELD
            MOVE " " TO F-EXIT-CH
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
                     AND NOT = "22"
                     AND NOT = "XX"
                MOVE "Selection Must Be Between 1 & 22, Re-Enter."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE "  " TO F-NAMEFIELD WS-ANSWER
                GO TO GET-010.
     
            IF WS-ANSWER = "XX"
               MOVE "35"   TO WS-ANSWER.
            MOVE WS-ANSWER TO WS-SELECTION.
            MOVE 12        TO WS-OPTION.
            MOVE "N"       TO WS-PASSWORD-VALID.
            IF F-EXIT-CH = X"1D"
                MOVE "Y"   TO WS-OVER-RIDE
            ELSE
                MOVE "N"   TO WS-OVER-RIDE.
         Perform Check-Password.
      *    Move "N" to Ws-1stRead.
      *     MOVE "PassWrds" TO WS-PROGRAM.
      *     Call Ws-Program Using Ws-Linkage.
            IF WS-PASSWORD-VALID = "N"
                MOVE "INVALID PASSWORD ENTERED, PLEASE RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-010
           ELSE
                PERFORM ERROR-020.

      *     CANCEL WS-PROGRAM.
           PERFORM DISPLAY-PR-NO.
           IF WS-ANSWER = "35"
                MOVE "CoStffIq" TO WS-PROGRAM.
            IF WS-ANSWER = " 1"
                MOVE "DrMastRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 2"
                MOVE "DrLablRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 3"
                MOVE "DrDiscRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 4"
                MOVE "DrAlDiRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 5"
                MOVE "DrAgeARp" TO WS-PROGRAM.
            IF WS-ANSWER = " 6"
                MOVE "DrAgeTRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 7"
                MOVE "DrDuFxRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 8"
                MOVE "DrAcStRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 9"
                MOVE "DrStatRp" TO WS-PROGRAM.
            IF WS-ANSWER = "10"
                MOVE "DrArSaRp" TO WS-PROGRAM.
            IF WS-ANSWER = "11"
                MOVE "DrArToRp" TO WS-PROGRAM.
            IF WS-ANSWER = "12"
                MOVE "DrArJhRp" TO WS-PROGRAM.
            IF WS-ANSWER = "13"
                MOVE "DrLaSoRp" TO WS-PROGRAM.
            IF WS-ANSWER = "14"
                MOVE "DrSaMxRp" TO WS-PROGRAM.
            IF WS-ANSWER = "15"
                MOVE "DrSaCoRp" TO WS-PROGRAM.
            IF WS-ANSWER = "16"
                MOVE "DrMailRp" TO WS-PROGRAM.
            IF WS-ANSWER = "17"
                MOVE "DrDiAcRp" TO WS-PROGRAM.
            IF WS-ANSWER = "18"
                MOVE "DrDiStRp" TO WS-PROGRAM.
            IF WS-ANSWER = "19"
                MOVE "DrPrAcRp" TO WS-PROGRAM.
            IF WS-ANSWER = "20"
                MOVE "DrCorpRp" TO WS-PROGRAM.
            IF WS-ANSWER = "21"
                MOVE "DrNmFxRp" TO WS-PROGRAM.
            IF WS-ANSWER = "22"
                MOVE "DrCredRp" TO WS-PROGRAM.
       GET-050.
           Call Ws-Program Using Ws-Linkage.
           PERFORM CLEAR-SCREEN
           CANCEL WS-PROGRAM
           PERFORM DISPLAY-FORM
           MOVE " " TO F-NAMEFIELD WS-ANSWER
           GO TO GET-010.
       GET-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-010.
            Move Ws-Forms-Name   TO F-FileName
            Move Ws-cbForms-name TO F-CbFileName.
            MOVE "Dr3Menu"       TO F-FORMNAME
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
