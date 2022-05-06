        IDENTIFICATION DIVISION.
        PROGRAM-ID. MainCont.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. Linux.
        OBJECT-COMPUTER. Linux.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT COMPANY-MENU ASSIGN TO "CoCompany"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-MENU-STATUS
               RECORD KEY IS PTY-KEY.
      *
       DATA DIVISION.
       FILE SECTION.
       COPY "ChlfdCompany".
       WORKING-STORAGE SECTION.
       Copy "WsMenuDateInfo".
       01  POS-DIS.
           03  POS-DIS1         PIC 99.
           03  POS-DIS2         PIC 99.
       01  COMPANYLISTS.   
         02  COMPANIES-LIST OCCURS 20.
           03  LIST-NAME        PIC X(40).
           03  LIST-NUMBER      PIC 99.
           03  LIST-VOL-DIR     PIC X(25).
       01  PASSWORD-PEAK.
           03  PP-PSWD          PIC X(12).
           03  PP-PRIORITY      PIC 99.
       01  WS-MENU-STATUS       PIC 99.

       Copy "ChlfdLinkage".
      *
       PROCEDURE DIVISION.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
          PERFORM OPEN-FILES.
          PERFORM CLEAR-SCREEN.
       CONTROL-005.
          MOVE "*** COMPANY SELECTION ***" TO WS-CO-NAME.
      * SETSYSID IS A 20 CHAR FIELD CALL THAT WE CAN USE TO ADD ANY
      * THING ELSE INTO AT A LATER STAGE.  WE ARE CURRENTLY ONLY
      * USING 7 CHAR OF THE 20 WITH WS-SERVER-REMOTE-NAME
          MOVE "(PRN SEL)"                 TO WS-SERVER-REMOTE-NAME.
          CALL "DATEMAILCO" USING WS-CO-NAME.
          CALL "SETSYSID"   USING WS-SERVER-REMOTE-NAME.
          PERFORM DISPLAY-FORM.
          MOVE "GOING TO READ-NEXT COMPANY FILE ....." TO WS-MESSAGE
          PERFORM ERROR-000.
          PERFORM READ-NEXT-COMPANY.
          PERFORM ERROR-020.
          PERFORM GET-DATA.
          GO TO CONTROL-005.
      *
       GET-DATA SECTION.
       GET-010.
           MOVE SPACES  TO F-NAMEFIELD
                           F-EXIT-CH
                          WS-NUMBER-CHECK.
           MOVE 1       TO F-INDEX.
           
           MOVE "SELECTION" TO F-FIELDNAME
           MOVE 9           TO F-CBFIELDNAME
           MOVE 2           TO F-CBFIELDLENGTH
           PERFORM USER-FILL-FIELD.
           PERFORM READ-FIELD-ALPHA.

      *THIS SECTION USED TO DISPLAY PASSWORD ALREADY ENTERED
      * <CODE-SHIFT-v>
           IF F-EXIT-CH = X"D6"
            IF F-NAMEFIELD = "xx"
              MOVE WS-LASTPASSWORD TO PP-PSWD
              MOVE WS-LASTPRIORITY TO PP-PRIORITY
              MOVE PASSWORD-PEAK   TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO GET-010.

           IF F-EXIT-CH = X"04"
            IF F-NAMEFIELD NOT = "E" AND NOT = "e"
              MOVE "Enter 'E' and Press <RETURN>/<END> to EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO GET-010
           ELSE
              PERFORM END-OFF.
              
           IF F-NAMEFIELD = "  " OR = "0 " OR = "00" OR = " 0"
              GO TO GET-010.
           MOVE SPACES      TO ALPHA-RATE
           MOVE F-NAMEFIELD TO WS-NUMBER-CHECK.
            
            IF WS-NU2 = " "
               MOVE WS-NU1 TO WS-NU2
               MOVE " "    TO WS-NU1
               MOVE WS-NUMBER-CHECK TO WS-ANSWER
            ELSE
               MOVE F-NAMEFIELD     TO WS-ANSWER.
                
           IF F-NAMEFIELD = "E" Or = "e"
              PERFORM END-OFF.
           IF F-NAMEFIELD = "  "
              GO TO GET-010.
           IF F-EXIT-CH = X"04"
            IF F-NAMEFIELD NOT = "E" AND NOT = "e"
              MOVE "Enter 'E' and Press <RETURN>/<END> to EXIT."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO GET-010
           ELSE
              PERFORM END-OFF.

            IF WS-ANSWER NOT = " 1" AND NOT = " 2" AND NOT = " 3"
                     AND NOT = " 4" AND NOT = " 5" AND NOT = " 6"
                     AND NOT = " 7" AND NOT = " 8" AND NOT = " 9"
                     AND NOT = "10" AND NOT = "11" AND NOT = "12"
                     AND NOT = "XX"
                MOVE SPACES TO WS-MESSAGE
                MOVE "Selection Must Be Between 1 & 12, Press 'ESC'"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE "  " TO F-NAMEFIELD WS-ANSWER
                GO TO GET-010.

           IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B"
                    AND NOT = X"1D" AND NOT = X"04"
               DISPLAY " " AT 3079 WITH BELL
              GO TO GET-010.
              
           MOVE WS-ANSWER            TO SUB-1
           MOVE LIST-NAME (SUB-1)    TO WS-CO-NAME
           MOVE LIST-NUMBER (SUB-1)  TO WS-CO-NUMBER
           MOVE LIST-VOL-DIR (SUB-1) TO WS-VOL-DIR.

          CALL "DATEMAILCO" USING WS-CO-NAME.

           IF LIST-NAME (SUB-1) = " "
           MOVE "Invalid Company Selected, Please Re-Enter."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE "  " TO F-NAMEFIELD WS-ANSWER
                GO TO GET-010.
      **************************************************
      *THIS NEW IF STATEMENT CHECKS IF CO-NUM DIFFERENT*
      * IF PASSWORD ENTERED IS WRONG.                  *
      *IF WS-PASSWORD-VALID = B THEN THE LINK TO THE   *
      * BRANCH IS DOWN AND THE PROGRAM MUST RECHECK ALL*
      * THE DATA FILE INFO AGAIN.                      *
      **************************************************
           IF WS-PASSWORD-VALID = "B"
              MOVE "Y" TO WS-OVER-RIDE
              MOVE "N" TO WS-PASSWORD-VALID.
           IF WS-SELECTION NOT = SUB-1
              MOVE "N"    TO WS-PASSWORD-VALID.
              
      * NEW LINE ADDED TO GET PASSWRDS TO RELOAD EVERYTHING FROM SCRATCH
      * AS THE SERVER / REMOTE PRINTER CHANGES ONLY WORKS IF YOU RELOAD
      * AGAIN.
           MOVE "N" TO WS-PASSWORD-VALID.
           MOVE "Y" TO WS-OVER-RIDE.

           MOVE SUB-1     TO WS-SELECTION.
           MOVE 98        TO WS-OPTION.
           IF WS-PASSWORD-VALID NOT = "D"
              MOVE "N"    TO WS-PASSWORD-VALID.
           IF F-EXIT-CH = X"1D"
                MOVE "Y"   TO WS-OVER-RIDE
           ELSE
                MOVE "N"   TO WS-OVER-RIDE.
       GET-020.
           Perform Error-020.
           Move "Y"        to Ws-1stRead.
           MOVE "PassWrds" TO WS-PROGRAM.
           CALL WS-PROGRAM USING WS-LINKAGE.
           IF WS-OVER-RIDE = "B"
                MOVE " "   TO WS-OVER-RIDE.
       GET-025.
           IF WS-PASSWORD-VALID = "B"
                MOVE 0 TO WS-SELECTION
                GO TO GET-010.
           IF WS-PASSWORD-VALID = "N"
                MOVE "D" TO WS-PASSWORD-VALID
                MOVE "INVALID PASSWORD ENTERED, PLEASE RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-010
            ELSE
                PERFORM ERROR-020.

           IF WS-PASSWORD-VALID = "D" OR = "B"
                CANCEL WS-PROGRAM
                GO TO GET-010.
           CANCEL WS-PROGRAM.
       GET-030.
           MOVE "ContMenu" TO WS-PROGRAM.
           CALL WS-PROGRAM USING WS-LINKAGE.
      
           PERFORM CLEAR-SCREEN

           CANCEL WS-PROGRAM
           
           MOVE " " TO F-NAMEFIELD WS-ANSWER.
       GET-999.
           EXIT.
      *
       READ-NEXT-COMPANY SECTION.
       RNC-005.
           OPEN I-O COMPANY-MENU.
           IF WS-MENU-STATUS NOT = 0
               MOVE "COMPANY FILE BUSY ON OPEN, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-MENU-STATUS TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO WS-MENU-STATUS
               GO TO RNC-005.
               
           MOVE 2710 TO POS
           DISPLAY "READING COMPANY-FILE DATA......." AT POS.
           MOVE 1 TO PTY-NUMBER.
           START COMPANY-MENU KEY NOT < PTY-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-MENU-STATUS NOT = 0
               MOVE "STATUS ON START IS NOT = 00" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNC-995.
           MOVE 1 TO SUB-1.
       RNC-010.
           READ COMPANY-MENU NEXT
               AT END
               GO TO RNC-900.
           IF WS-MENU-STATUS NOT = 0
               MOVE "COMPANY FILE BUSY ON READ-NEXT, GOING TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-MENU-STATUS TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNC-010.
           MOVE PTY-VOL-DIR TO LIST-VOL-DIR (SUB-1)
           MOVE PTY-NUMBER  TO LIST-NUMBER (SUB-1)
           MOVE PTY-CO-NAME TO LIST-NAME (SUB-1).
         
           IF SUB-1 < 20
              ADD 1 TO SUB-1
              GO TO RNC-010.
       RNC-900.
           MOVE 2710 TO POS
           DISPLAY "                                      " AT POS.
           MOVE  1 TO SUB-1 F-INDEX.
       RNC-910.
           MOVE "NUM"              TO F-FIELDNAME
           MOVE 3                  TO F-CBFIELDNAME
           MOVE LIST-NUMBER (SUB-1)TO F-EDNAMEFIELDPTY
           MOVE 2                  TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-PTY.

           MOVE "NAME"            TO F-FIELDNAME
           MOVE 4                 TO F-CBFIELDNAME
           MOVE LIST-NAME (SUB-1) TO F-NAMEFIELD
           MOVE 40                TO F-CBFIELDLENGTH
           PERFORM WRITE-FIELD-ALPHA.
           
           ADD  1  TO SUB-1 F-INDEX 
           IF LIST-NAME (SUB-1) NOT = " "
               GO TO RNC-910.
       RNC-995.
           CLOSE COMPANY-MENU.
       RNC-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-010.
           MOVE "/ctools/lib/CoForms.Lib" TO F-FILENAME
                                             WS-FORMS-NAME.
           MOVE 23                        TO F-CBFILENAME
                                             WS-CBFORMS-NAME.
           MOVE "MainCont"                TO F-FORMNAME
           MOVE 8                         TO F-CBFORMNAME.

      * ORIGINAL FORMAT FOR CTOS
      *     MOVE "[Win]<Program>CoForms.Lib" TO Ws-Forms-Name
      *     MOVE Ws-Forms-Name               TO F-FILENAME.
      *     MOVE 25                          TO WS-cbForms-Name
      *     MOVE WS-cbForms-Name             TO F-CBFILENAME.
      *     MOVE "MainCont"                  TO F-FORMNAME
      *     MOVE 8                           TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           STOP RUN.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldPty".
       Copy "DecimaliseRate".
       Copy "DisplayForm".
      * Copy "ReadMenuKBD".
       Copy "UserFillField".
       Copy "MenuClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "Error5Message".
       Copy "DisplayProgNum".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
