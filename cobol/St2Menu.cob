        IDENTIFICATION DIVISION.
        PROGRAM-ID. St2Menu.
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
       01  W-CRTSTATUS           PIC 9(4) value 0.
       LINKAGE SECTION.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
          PERFORM OPEN-FILES
          PERFORM CLEAR-SCREEN.
       CONTROL-010.
          PERFORM DISPLAY-FORM.
       CONTROL-011.
          PERFORM GET-DATA.
          GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-010.
            MOVE "                      " TO F-NAMEFIELD.
            MOVE "SELECTION" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"1D"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = "E "
                 PERFORM END-OFF.
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
                     AND NOT = "25" AND NOT = "26" AND NOT = "27"
                     AND NOT = "28" AND NOT = "29" AND NOT = "30"
                     AND NOT = "31" AND NOT = "32"
                     AND NOT = "XX"
            MOVE "Selection Must Be Between 1 & 32, Press 'CANCEL'"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE "  " TO F-NAMEFIELD WS-ANSWER
                GO TO GET-010.

            IF WS-ANSWER = "XX"
               MOVE "35"   TO WS-ANSWER.
            MOVE WS-ANSWER TO WS-SELECTION.
            MOVE 21        TO WS-OPTION.
            MOVE "N"       TO WS-PASSWORD-VALID.
            IF F-EXIT-CH = X"1D"
                MOVE "Y"   TO WS-OVER-RIDE
            ELSE
                MOVE "N"   TO WS-OVER-RIDE.
         PERFORM CHECK-PASSWORD.
      *    Move "N" to Ws-1stRead.
      *    MOVE "PassWrds" TO WS-PROGRAM
      *    CALL WS-PROGRAM USING Ws-Linkage.
           IF WS-PASSWORD-VALID = "N"
                MOVE "INVALID PASSWORD ENTERED, PLEASE RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-010
           ELSE
                PERFORM ERROR-020.

      *      CANCEL WS-PROGRAM.
       GET-020.
            PERFORM DISPLAY-PR-NO.
           IF WS-ANSWER = "35"
                MOVE "CoStffIq" TO WS-PROGRAM.
            IF WS-ANSWER = " 1"
                MOVE "StMastMt" TO WS-PROGRAM.
            IF WS-ANSWER = " 2"
                MOVE "StMastIq" TO WS-PROGRAM.
            IF WS-ANSWER = " 3"
                MOVE "StMasBMt" TO WS-PROGRAM.
            IF WS-ANSWER = " 4"
                MOVE "StMasLMt" TO WS-PROGRAM.
            IF WS-ANSWER = " 5"
                MOVE "StMiMxMt" TO WS-PROGRAM.
            IF WS-ANSWER = " 6"
                MOVE "StMasPMt" TO WS-PROGRAM.
            IF WS-ANSWER = " 7"
                MOVE "StPrChMt" TO WS-PROGRAM.
            IF WS-ANSWER = " 8"
                MOVE "StPrSpMt" TO WS-PROGRAM.
            IF WS-ANSWER = " 9"
                MOVE "StNoChMt" TO WS-PROGRAM.
            IF WS-ANSWER = "10"
                MOVE "StNoCopy" TO WS-PROGRAM.
            IF WS-ANSWER = "11"
                MOVE "StDescIq" TO WS-PROGRAM.
            IF WS-ANSWER = "12"
                MOVE "StBoReMt" TO WS-PROGRAM.
            IF WS-ANSWER = "13"
                MOVE "StReLoMt" TO WS-PROGRAM.
            IF WS-ANSWER = "14"
                MOVE "StReLoIq" TO WS-PROGRAM.
            IF WS-ANSWER = "15"
                MOVE "StReImMt" TO WS-PROGRAM.
            IF WS-ANSWER = "16"
                MOVE "StBoStIq" TO WS-PROGRAM.
            IF WS-ANSWER = "17"
                MOVE "StBoAcIq" TO WS-PROGRAM.
            IF WS-ANSWER = "18"
                MOVE "StOrdrMt" TO WS-PROGRAM.
            IF WS-ANSWER = "19"
                MOVE "StOrStIq" TO WS-PROGRAM.
            IF WS-ANSWER = "20"
                MOVE "StOrOrIq" TO WS-PROGRAM.
            IF WS-ANSWER = "21"
                MOVE "StOrSuIq" TO WS-PROGRAM.
            IF WS-ANSWER = "22"
                MOVE "StOrDtMt" TO WS-PROGRAM.
            IF WS-ANSWER = "23"
                MOVE "StOrGnMt" TO WS-PROGRAM.
            IF WS-ANSWER = "24"
                MOVE "StOrPsMt" TO WS-PROGRAM.
            IF WS-ANSWER = "25"
                MOVE "StStQyMt" TO WS-PROGRAM.
            IF WS-ANSWER = "26"
                MOVE "StPrNwMt" TO WS-PROGRAM.
            IF WS-ANSWER = "27"
                MOVE "StCataMt" TO WS-PROGRAM.
            IF WS-ANSWER = "28"
                MOVE "StCatPIq" TO WS-PROGRAM.
            IF WS-ANSWER = "29"
                MOVE "StAlteMt" TO WS-PROGRAM.
            IF WS-ANSWER = "30"
                MOVE "StAlteIq" TO WS-PROGRAM.
            IF WS-ANSWER = "31"
                MOVE "StNumber" TO WS-PROGRAM.
            IF WS-ANSWER = "32"
                MOVE "StBrchIq" TO WS-PROGRAM.
            CALL WS-PROGRAM USING WS-LINKAGE.
            
      *      MOVE "PROGRAM CALLED AND EXITED" TO WS-MESSAGE
      *      PERFORM ERROR-MESSAGE.
            
            PERFORM CLEAR-SCREEN
            CANCEL WS-PROGRAM
            PERFORM DISPLAY-FORM
            MOVE " " TO F-NAMEFIELD WS-ANSWER
            PERFORM CLEAR-SCREEN.
            PERFORM CONTROL-010.
            GO TO GET-010.
       GET-999.
            EXIT.
      *
       DISPLAY-TEXT-FORM SECTION.
       DTF-005.
            MOVE 0430 TO POS
            DISPLAY "*** STOCK LEDGER MENU ***" AT POS
            MOVE 0535 TO POS
            DISPLAY "*** STOCK MAINTENANCE  MENU ***" AT POS
            
            MOVE 0710 TO POS
            DISPLAY " 1. STOCK ENTRY / EDIT PROGRAM            " AT POS
            MOVE 0910 TO POS
            DISPLAY " 2. STOCK ENQUIRY PROGRAM                 " AT POS
            MOVE 1110 TO POS
            DISPLAY " 3. STOCK DESCRIPTION ENQUIRY PROGRAM     " AT POS.
       DTF-999.
            EXIT.

       OPEN-FILES SECTION.
       OPEN-010.
            MOVE WS-FORMS-NAME   TO F-FILENAME
            MOVE WS-CBFORMS-NAME TO F-CBFILENAME.
            MOVE "St2Menu"       TO F-FORMNAME
            MOVE 7               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
      *     STOP RUN.
           PERFORM CLEAR-SCREEN.
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
