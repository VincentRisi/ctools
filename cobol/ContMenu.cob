        IDENTIFICATION DIVISION.
        PROGRAM-ID. ContMenu.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. LINUX.
        OBJECT-COMPUTER. LINUX.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       WORKING-STORAGE SECTION.
       COPY "WsMenuDateInfo".

       LINKAGE SECTION.
       COPY "ChlfdLinkage".
      *
       PROCEDURE DIVISION USING WS-LINKAGE.
       MAINLINE.

       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM CLEAR-SCREEN.
           PERFORM DISPLAY-FORM.
           CALL "DATEMAILCO" USING WS-CO-NAME.
           CALL "SETSYSID"   USING WS-SERVER-REMOTE-NAME.
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
            PERFORM ERROR-020.
            MOVE 2 TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = " "
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
                     AND NOT = "XX"
                MOVE SPACES TO WS-MESSAGE
                MOVE "SELECTION MUST BE BETWEEN 1 & 9, PRESS 'CANCEL'"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE "  " TO F-NAMEFIELD WS-ANSWER
                GO TO GET-010.

            IF WS-ANSWER = "XX"
               MOVE "35"   TO WS-ANSWER.
            MOVE WS-ANSWER TO WS-SELECTION.
            MOVE 95        TO WS-OPTION.
            MOVE "N"       TO WS-PASSWORD-VALID.
            IF F-EXIT-CH = X"1D"
                MOVE "Y"   TO WS-OVER-RIDE
            ELSE
                MOVE "N"   TO WS-OVER-RIDE.
          PERFORM CHECK-PASSWORD.
      *    MOVE "N" TO WS-1STREAD.
      *    MOVE "PassWrds" TO WS-PROGRAM
      *    CALL WS-PROGRAM USING WS-LINKAGE.

           IF WS-PASSWORD-VALID = "N"
                MOVE "INVALID PASSWORD ENTERED, PLEASE RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-010
           ELSE
                PERFORM CLEAR-SCREEN
                PERFORM ERROR-020.
           CANCEL WS-PROGRAM.
           IF WS-ANSWER = "35"
                MOVE "CoStffIq" TO WS-PROGRAM.
           IF WS-ANSWER = " 1"
                MOVE "Dr1Menu" TO WS-PROGRAM.
           IF WS-ANSWER = " 2"
                MOVE "St1Menu" TO WS-PROGRAM.
           IF WS-ANSWER = " 3"
                MOVE "Sl1Menu" TO WS-PROGRAM.
           IF WS-ANSWER = " 4"
                MOVE "Cr1Menu" TO WS-PROGRAM.
           IF WS-ANSWER = " 5"
                MOVE "Gl1Menu" TO WS-PROGRAM.
           IF WS-ANSWER = " 6"
                MOVE "Bm1Menu" TO WS-PROGRAM.
           IF WS-ANSWER = " 7"
                MOVE "Cb1Menu" TO WS-PROGRAM.
           IF WS-ANSWER = " 8"
                MOVE "MastMenu" TO WS-PROGRAM.
           IF WS-ANSWER = " 9"
                MOVE "BaUpMenu" TO WS-PROGRAM.

           CALL WS-PROGRAM USING WS-LINKAGE.
           
           PERFORM CLEAR-SCREEN
           CANCEL WS-PROGRAM
           PERFORM DISPLAY-FORM
           MOVE " " TO F-NAMEFIELD WS-ANSWER

           PERFORM CONTROL-010.
           GO TO GET-010.
       GET-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-010.
            MOVE WS-FORMS-NAME   TO F-FILENAME
            MOVE WS-CBFORMS-NAME TO F-CBFILENAME.
            MOVE "ContMenu"      TO F-FORMNAME
            MOVE 8               TO F-CBFORMNAME.
       COPY "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           EXIT PROGRAM.
           PERFORM CLEAR-SCREEN.
      *     STOP RUN.
       END-999.
           EXIT.
      *      
       COPY "PassChck.cob".
       COPY "ReadFieldAlpha".
       COPY "WriteFieldAlpha".
       COPY "DisplayForm".
       COPY "UserFillField".
       COPY "MenuClearScreen".
       COPY "ErrorMessage".
       Copy "Error1Message".
       Copy "Error5Message".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
