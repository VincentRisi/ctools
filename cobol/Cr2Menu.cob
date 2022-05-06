        IDENTIFICATION DIVISION.
        PROGRAM-ID. Cr2Menu.
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
       77  WS-YYNAME                PIC X(5) VALUE " ".
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
       CONT-010.
          PERFORM DISPLAY-FORM
          PERFORM GET-DATA
          GO TO CONT-010.
      *
       GET-DATA SECTION.
       GET-005.
            MOVE 2 TO WS-MODE.
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
                     AND NOT = "10" AND NOT = "11" AND NOT = "12"
                     AND NOT = "13" AND NOT = "14" AND NOT = "15"
                     AND NOT = "16" AND NOT = "17" AND NOT = "18"
                     AND NOT = "19" AND NOT = "20" AND NOT = "21"
                     AND NOT = "XX"
                MOVE "Selection Must Be Between 1 & 21, Re-Enter."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE "  " TO F-NAMEFIELD WS-ANSWER
                GO TO GET-010.
     
            IF WS-ANSWER = "XX"
               MOVE "35"   TO WS-ANSWER.
            MOVE WS-ANSWER TO WS-SELECTION
            MOVE 41        TO WS-OPTION
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
                MOVE "CrMastMt" TO WS-PROGRAM.
            IF WS-ANSWER = " 2"
                MOVE "CrMastIq" TO WS-PROGRAM.
            IF WS-ANSWER = " 3"
                MOVE "CrNameIq" TO WS-PROGRAM.
            IF WS-ANSWER = " 4"
                MOVE "CrCurrMt" TO WS-PROGRAM.
            IF WS-ANSWER = " 5"
                MOVE "CrInCrMt" TO WS-PROGRAM.
            IF WS-ANSWER = " 6"
                MOVE "CrPayDMt" TO WS-PROGRAM.
            IF WS-ANSWER = " 7"
                MOVE "CrJrnIq" TO WS-PROGRAM.
            IF WS-ANSWER = " 8"
                MOVE "CrTranIq" TO WS-PROGRAM.
            IF WS-ANSWER = " 9"
                MOVE "CrPaymnt" TO WS-PROGRAM.
            IF WS-ANSWER = "10"
                MOVE "CrFBCMt" TO WS-PROGRAM.
            IF WS-ANSWER = "11"
                MOVE "CrFBCTrs" TO WS-PROGRAM.
            IF WS-ANSWER = "12"
                MOVE "CrAlias" TO WS-PROGRAM.
            IF WS-ANSWER = "13"
                MOVE "CrCAMSCt" TO WS-PROGRAM.
            IF WS-ANSWER = "14"
                MOVE "CrCAMSIq" TO WS-PROGRAM.
            IF WS-ANSWER = "15"
                MOVE "CrCAMSDl" TO WS-PROGRAM.
            IF WS-ANSWER = "16"
                MOVE "CrRemiMt" TO WS-PROGRAM.
            IF WS-ANSWER = "17"
                MOVE "CrRemiIq" TO WS-PROGRAM.
            IF WS-ANSWER = "18"
                MOVE "CrReFrAl" TO WS-PROGRAM.
            IF WS-ANSWER = "19"
                MOVE "CrAdjuIq" TO WS-PROGRAM.
            IF WS-ANSWER = "20"
                MOVE "CrPerEnd" TO WS-PROGRAM.
                
            IF WS-ANSWER = "21"
             IF WS-CO-NUMBER NOT = 1
                MOVE
           "THIS PROCESS CAN ONLY BE RUN FROM COMPANY NUMBER 1, 'Esc'" &
           " TO EXIT" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE " " TO F-NAMEFIELD WS-ANSWER
                GO TO GET-010
             ELSE
                Move "CrMaint.sh" TO Ws-Data-Name
                PERFORM SETUP-ZIP-FILES
                MOVE 
            "The Zip File Has Been Created & last Months Files Deleted."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE " Press 'Esc' To Continue With Other Processes."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
               Go To GET-011.
           Call Ws-Program Using Ws-Linkage.
       GET-011.
            PERFORM CLEAR-SCREEN
            CANCEL WS-PROGRAM
            PERFORM DISPLAY-FORM
            MOVE " " TO F-NAMEFIELD WS-ANSWER
            GO TO GET-010.
       GET-015.
            Perform Check-Data-Names.
            CALL "&OPENFILELL" USING F-ERROR1
                                     F-FH
                                     F-FILENAME
                                     F-CBFILENAME
                                     F-FILENAME
                                     F-INTEGERZERO
                                     F-OPENMODE.
            IF F-ERROR1 NOT = 0
                DISPLAY "OPENFILE CRMAINT.SUB ERROR"
                DISPLAY F-ERROR1
                display f-filename
                display f-cbfilename
                STOP RUN.
            CALL "&SETSYSINMODE" USING WS-CTOS-ERROR
                                       WS-MODE
                                       F-FH.
            STOP RUN.
       GET-999.
            EXIT.
      *
       SETUP-ZIP-FILES SECTION.
       SUQFD-002.
          MOVE 
         "Enter the Year Number & Month Name for the Creditor Zip File."
              TO WS-MESSAGE
              PERFORM ERROR1-000
          MOVE "For e.g. Enter 15Jun for 2015 June." TO WS-MESSAGE
             PERFORM ERROR-000.
       SUQFD-003.
          MOVE 2721 TO POS
          DISPLAY "ENTER YEAR MONTH AS YYName: [     ]" AT POS

           MOVE ' '       TO CDA-DATA.
           MOVE 5         TO CDA-DATALEN.
           MOVE 24        TO CDA-ROW.
           MOVE 49        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-YYNAME.

            IF WS-YYNAME = " "
               GO TO SUQFD-003.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO SUQFD-010
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO SUQFD-003.
       SUQFD-010.
          PERFORM ERROR-020
          PERFORM ERROR1-020
          MOVE 2710 TO POS
          DISPLAY WS-MESSAGE AT POS.
          MOVE CONCATENATE('./CrMaint.sh ', TRIM(WS-YYNAME))
                TO WS-COMMAND-LINE.
      *    DISPLAY WS-COMMAND-LINE.  
      *    ACCEPT WS-ACCEPT.
      *The variable which will be specified are:
      *$1 = The YYName          e.g. 15Jun
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
            MOVE "Cr2Menu"       TO F-FORMNAME.
            MOVE 7               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            EXIT PROGRAM.
      *      STOP RUN.
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
