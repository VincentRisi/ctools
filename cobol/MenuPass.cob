        IDENTIFICATION DIVISION.
        PROGRAM-ID. MenuPass.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT MENU-PASSWORDS ASSIGN TO WS-MENU
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-MENU-STATUS
               RECORD KEY IS MU-KEY.

           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdMenu.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-1STPRINT        PIC 9 VALUE 0.
       77  NEW-NO             PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-NUMBER          PIC X(10) VALUE " ".
       77  WS-SAVE            PIC 99 VALUE 0.
       01  WS-MENU-STATUS.
           03  WS-MENU-ST1   PIC 99.
       01  HEAD1-LINE.
           03  FILLER         PIC X(5) VALUE "DATE:".
           03  H1-DATE        PIC X(10) VALUE " ".
           03  FILLER         PIC X(5) VALUE " ".
           03  FILLER         PIC X(114) VALUE
              "** PASSWORD MENU PROGRAM REPORT **".
       01  HEAD2-LINE.
           03  FILLER         PIC X(20) VALUE " ".
           03  FILLER         PIC X(34) VALUE ALL "*".
           03  FILLER         PIC X(80) VALUE " ".
       01  HEADER-LINE.
           03  H-MENU        PIC X(7) VALUE "MENU".
           03  H-SUB         PIC X(10) VALUE "PROGRAM".
           03  H-PASSWORD    PIC X(15) VALUE "PASSWORD".
           03  H-PRIORITY    PIC X(110) VALUE "PRIORITY".
       01  DETAIL-LINE.
           03  D-MENU        PIC Z9 BLANK WHEN ZERO.
           03  FILLER        PIC X(5) VALUE " ".
           03  D-PROG-NO     PIC Z9.
           03  FILLER        PIC X(8) VALUE " ".
           03  D-PASSWORD    PIC X(15).
           03  D-PRIORITY    PIC Z9.
           03  FILLER        PIC X(106).
       Copy "WsDateInfo".
       Copy "FormsInfo".
       LINKAGE SECTION.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
        CONTROL-PARAGRAPH SECTION.
        CONTROL-000.
            PERFORM OPEN-FILES
            PERFORM CLEAR-SCREEN.
            MOVE 0310 TO POS
            DISPLAY "** PASSWORD MENU CHANGE PROGRAM **" AT POS
            MOVE 0410 TO POS
            DISPLAY "**********************************" AT POS.
       CONT-003.
            Copy "PrinterAccept".
            PERFORM CLEAR-SCREEN.
       CONT-010.
            PERFORM DISPLAY-FORM
            PERFORM GET-DATA
            PERFORM FILL-DATA
            GO TO CONT-010.
      *
        GET-DATA SECTION.
        GET-000.
           MOVE "N" TO NEW-NO
                       WS-END.
           MOVE
           "ENTER # & <RETURN>, ENTER # & <F10> TO PRINT THAT #,"
            TO WS-MESSAGE
            PERFORM ERROR1-000
            MOVE "OR XX TO PRINT ALL INFO." TO WS-MESSAGE
            PERFORM ERROR-000.
        GET-001.
            MOVE "                " TO F-NAMEFIELD
            MOVE "OPTION"           TO F-FIELDNAME
            MOVE 6                  TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 2                  TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA.

            IF F-NAMEFIELD = "YY"
                PERFORM GET-USER-MAIL-NAME

                MOVE WS-USERNAME TO WS-MESSAGE
                PERFORM ERROR-MESSAGE

                GO TO GET-001.
                
            IF F-NAMEFIELD = "XX"
                 MOVE 0 TO MU-OPTION
                 PERFORM PRINT-PASSWORDS
                 MOVE "Y" TO WS-END
                 GO TO GET-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDANAL
                                 MU-OPTION.
            PERFORM WRITE-FIELD-ANALYSIS.
      *PRINT KEY
            IF F-EXIT-CH = X"1F"
                 PERFORM PRINT-PASSWORDS
                 MOVE "Y" TO WS-END
                 GO TO GET-999.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-SAVE TO MU-OPTION
                 PERFORM START-RECORD
                 PERFORM READ-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-003 THRU GET-005
                 GO TO GET-999.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 CALL X"E5"
                 GO TO GET-000.
            PERFORM READ-RECORD.
            IF NEW-NO = "Y"
               GO TO GET-999.
            MOVE 1 TO SUB-1.
            
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            GO TO GET-004.
        GET-003.
            MOVE "OPTION"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            MOVE MU-OPTION TO F-EDNAMEFIELDANAL
            MOVE 2         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS.
        GET-004.
            IF SUB-1 < 1
               MOVE 1 TO SUB-1.
            MOVE "MENUNO" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            MOVE SUB-1    TO F-EDNAMEFIELDANAL
            MOVE 2        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS.
        GET-005.
            MOVE "NUMBER"          TO F-FIELDNAME
            MOVE 6                 TO F-CBFIELDNAME
            MOVE MU-NUMBER (SUB-1) TO F-NAMEFIELD
            MOVE 10                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PRIORITY"          TO F-FIELDNAME
            MOVE 8                   TO F-CBFIELDNAME
            MOVE MU-PRIORITY (SUB-1) TO F-EDNAMEFIELDANAL
            MOVE 2                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ANALYSIS.
       GET-999.
            EXIT.
      *
       PRINT-PASSWORDS SECTION.
       PP-010.
           PERFORM ERROR1-020.
           PERFORM ERROR-020.
           MOVE 2701 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 2730 TO POS
           DISPLAY WS-MESSAGE AT POS.
           
           MOVE 2720 TO POS
           DISPLAY "Printing of Passwords in Progress......" AT POS
           OPEN OUTPUT PRINT-FILE.
           MOVE WS-CO-NAME TO CO-NAME.
           MOVE 1 TO SUB-1 WS-1STPRINT.
           IF MU-OPTION = 0
              MOVE 1 TO MU-OPTION.
           START MENU-PASSWORDS KEY NOT < MU-KEY
              INVALID KEY NEXT SENTENCE.
           IF WS-MENU-ST1 NOT = 0
              MOVE "INVALID START, 'CANCEL' TO EXIT." TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO PP-999.
       PP-020.
           READ MENU-PASSWORDS NEXT
               AT END NEXT SENTENCE.
           IF WS-MENU-ST1 = 10
               MOVE 0 TO WS-MENU-ST1
               GO TO PP-900.
           IF WS-MENU-ST1 NOT = 0
               MOVE 0 TO WS-MENU-ST1
               GO TO PP-020.
           IF WS-1STPRINT = "1"
               WRITE PRINT-REC FROM COMPANY-LINE
               WRITE PRINT-REC FROM HEAD1-LINE AFTER 1
               WRITE PRINT-REC FROM HEAD2-LINE AFTER 1
               MOVE " " TO PRINT-REC
               WRITE PRINT-REC
               MOVE 0 TO WS-1STPRINT.
           WRITE PRINT-REC FROM HEADER-LINE AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
       PP-030.
           IF MU-NUMBER (SUB-1) = "     "
              GO TO PP-045.
           MOVE MU-OPTION           TO D-MENU
           MOVE SUB-1               TO D-PROG-NO
           MOVE MU-NUMBER (SUB-1)   TO D-PASSWORD
           MOVE MU-PRIORITY (SUB-1) TO D-PRIORITY
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC.
       PP-040.
           IF SUB-1 NOT > 34
               ADD 1 TO SUB-1
               GO TO PP-030.
       PP-045.
           MOVE 1 TO SUB-1.
           IF F-NAMEFIELD = "XX"
              GO TO PP-020.
       PP-900.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
           
           CLOSE PRINT-FILE.
           PERFORM ERROR1-020.
       PP-999.
           EXIT.
      *
       DELETE-RECORD SECTION.
       DDR-000.
            IF NEW-NO = "Y"
                GO TO DDR-999.
       DDR-010.
            MOVE "DELETING MENU-PASSWORDS, <ACTION-FINISH> TO ABORT."
            TO WS-MESSAGE
            PERFORM ERROR-MESSAGE.
            
            DELETE MENU-PASSWORDS
               INVALID KEY NEXT SENTENCE.
            IF WS-MENU-ST1 NOT = 0
               MOVE 0 TO WS-MENU-ST1
               GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       FILL-DATA SECTION.
       FILL-000.
            IF WS-END = "Y"
                GO TO FILL-999.
       FILL-001.
            MOVE "                " TO F-NAMEFIELD
            MOVE "MENUNO"     TO F-FIELDNAME
            MOVE 6            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 2            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDANAL
                                 SUB-1
            PERFORM WRITE-FIELD-ANALYSIS.
            IF SUB-1 > 35
               MOVE "ONLY 35 PROGRAMS PER MENU ARE ALLOWED, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO FILL-999.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 CALL X"E5"
                 GO TO FILL-001.
            PERFORM GET-005.
       FILL-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NUMBER"    TO F-FIELDNAME
            MOVE 6           TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 10          TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD TO MU-NUMBER (SUB-1).
            IF F-EXIT-CH = X"01"
               GO TO  FILL-001.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
              IF WS-END NOT = "Y"
                 GO TO FILL-999
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-003 THRU GET-005
                 GO TO FILL-001.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'CANCEL' TO CLEAR THE CURRENT INPUT BEFORE 'FINISH'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 CALL X"E5"
                 GO TO FILL-010.
       FILL-020.
            MOVE "                " TO F-NAMEFIELD
            MOVE "PRIORITY"   TO F-FIELDNAME
            MOVE 8            TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD
            MOVE 2            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDANAL
                                 MU-PRIORITY (SUB-1)
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"01"
                 GO TO FILL-010.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-RECORD
               PERFORM READ-NEXT
              IF WS-END NOT = "Y"
                 GO TO FILL-999
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM GET-003 THRU GET-005
                 GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'CANCEL' TO CLEAR THE CURRENT INPUT BEFORE 'FINISH'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 CALL X"E5"
                 GO TO FILL-020.

            IF SUB-1 < 35
                ADD 1 TO SUB-1
                PERFORM GET-004 THRU GET-005
                GO TO FILL-010.
            GO TO FILL-020.
       FILL-999.
            EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             UNLOCK MENU-PASSWORDS.
             MOVE SUB-1 TO SUB-6
             MOVE 1 TO SUB-1.
       CLSC-010.
             MOVE " " TO MU-NUMBER (SUB-1)
             MOVE 0   TO MU-PRIORITY (SUB-1)
             ADD 1 TO SUB-1.
             IF SUB-1 < 35
                 GO TO CLSC-010.
             MOVE WS-SAVE TO MU-OPTION.
             IF SUB-6 > 0
                 MOVE SUB-6 TO SUB-1
             ELSE
                 MOVE 1 TO SUB-1.
       CLSC-999.
             EXIT.      
      *
       REWRITE-RECORD SECTION.
       RDR-010.
            IF NEW-NO = "Y"
               GO TO RDR-020.
       RDR-015.
            REWRITE MENU-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-MENU-ST1 NOT = 0
      *          MOVE 0 TO WS-MENU-ST1
                MOVE "MENU RECORD BUSY ON REWRITE, 'CANCEL' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE WS-MENU-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-020.
            GO TO RDR-999.
       RDR-020.
            WRITE MENU-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-MENU-ST1 NOT = 0
      *          MOVE 0 TO WS-MENU-ST1
                MOVE "MENU RECORD BUSY ON WRITE, 'CANCEL' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE WS-MENU-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDR-015.
       RDR-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE MU-OPTION TO WS-SAVE.
           START MENU-PASSWORDS KEY NOT < MU-KEY.
        RD-010.
           READ MENU-PASSWORDS WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-MENU-ST1 = 35 OR 47 OR 23
                MOVE 0 TO WS-MENU-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-NO
                MOVE WS-SAVE TO MU-OPTION
                GO TO RD-999.
           IF WS-MENU-ST1 NOT = 0
                MOVE 0 TO WS-MENU-ST1
                MOVE "MENU Busy ON READ, Press 'CANCEL' To Retry"
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RD-010.
           PERFORM ERROR-020
           MOVE "N" TO NEW-NO
           MOVE MU-OPTION TO WS-SAVE.
       RD-999.
           EXIT.
      *
       START-RECORD SECTION.
       STR-000.
           MOVE WS-SAVE TO MU-OPTION.
           START MENU-PASSWORDS KEY NOT < MU-KEY.
       STR-999.
           EXIT.
      *
       READ-NEXT SECTION.
       RNX-001.
           MOVE 0 TO WS-MENU-ST1.
       RNX-005.
           READ MENU-PASSWORDS NEXT WITH LOCK
             AT END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE " " TO WS-NUMBER
               MOVE 0   TO WS-SAVE
               MOVE "Y" TO WS-END
               GO TO RNX-999.
           IF WS-MENU-ST1 = 35
               MOVE 0 TO WS-MENU-ST1
               MOVE "MENU BUSY ON READ-NEXT, 'CANCEL' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RNX-005.
           IF WS-MENU-ST1 NOT = 0
               MOVE 0 TO WS-MENU-ST1
               PERFORM START-RECORD
               GO TO RNX-005.
           IF MU-OPTION = 0
               GO TO RNX-005.
           MOVE MU-OPTION TO WS-SAVE
           MOVE "N" TO NEW-NO.
       RNX-999.
           EXIT.
      *
        OPEN-FILES SECTION.
        OPEN-000.
            OPEN I-O MENU-PASSWORDS.
            IF WS-MENU-ST1 NOT = 0
               MOVE 0 TO WS-MENU-ST1
               MOVE "MENU FILE BUSY ON OPEN, 'CANCEL' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *      ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
            Go To Open-010.
       OPEN-010.
            Move Ws-Forms-Name   TO F-FileName
            Move Ws-cbForms-name TO F-CbFileName.
            MOVE "MenuPass"      TO F-FORMNAME
            MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE MENU-PASSWORDS.
       END-900.
      *      STOP RUN.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAnalysis".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
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
      *
      * END-OF-JOB
