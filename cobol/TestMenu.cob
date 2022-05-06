       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestMenu.
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
       01  SUBA PIC 99.
       01  WS-COLOR PIC 9.
       01 DATA-MAINCONT.
         03 D-CBNUM BINARY-CHAR VALUE 2.
         03 D-NUM PIC X(2) OCCURS 15.
         03 D-CBNAME BINARY-CHAR VALUE 40.
         03 D-NAME PIC X(40) OCCURS 15.
         03 D-CBSELECTION BINARY-CHAR VALUE 2.
         03 D-SELECTION PIC X(2).
       01 DEFS-MAINCONT.
         03 N-CBFORMNAME BINARY-CHAR VALUE 8.
         03 N-FORMNAME PIC X(8) VALUE 'MAINCONT'.
         03 N-FIELDS.
           05 N-CBNUM        BINARY-CHAR VALUE 3.
           05 N-NUM          PIC X(40) VALUE 'NUM'.
           05 N-OCCNUM       BINARY-CHAR VALUE 15.
           05 N-CBNAME       BINARY-CHAR VALUE 4.
           05 N-NAME         PIC X(40) VALUE 'NAME'.
           05 N-OCCNAME      BINARY-CHAR VALUE 15.
           05 N-CBSELECTION  BINARY-CHAR VALUE 9.
           05 N-SELECTION    PIC X(40) VALUE 'SELECTION'.
           05 N-OCCSELECTION BINARY-CHAR VALUE 0.
         03 N-FIELD-ARRAY REDEFINES N-FIELDS OCCURS 3.
           05 N-CBFIELDNAME BINARY-CHAR.
           05 N-FIELDNAME    PIC X(40).
           05 N-OCCFIELDNAME BINARY-CHAR.
      * 01  CTOS_DISPLAY_ACCEPT.
      *    78  CDA-BLACK   VALUE 0.
      *    78  CDA-RED     VALUE 1.
      *    78  CDA-GREEN   VALUE 2.
      *    78  CDA-YELLOW  VALUE 3.
      *    78  CDA-BLUE    VALUE 4.
      *    78  CDA-MAGENTA VALUE 5.
      *    78  CDA-CYAN    VALUE 6.
      *    78  CDA-WHITE   VALUE 7.
      *    03  CDA-ERROR   BINARY-SHORT.
      *    03  CDA-ROW     BINARY-SHORT.
      *    03  CDA-COL     BINARY-SHORT.
      *    03  CDA-DATA    PIC X(80).
      *    03  CDA-DATALEN BINARY-SHORT.
      *    03  CDA-COLOR   BINARY-SHORT.
      *    03  CDA-ATTR    PIC X.         
      *    03  CDA-KEY     BINARY-SHORT.
      *    03  CDA-FILTER  PIC X(10) VALUE
      *        X"0A1B04010B070C1D1F00".
      *          1 2 3 4 5 6 7 8 9  null terminated
      *  1=RETURN, 2=GO, 3=FINISH, 4=UP, 5=DOWN, 6=CANCEL,
      *  7=NEXT-PAGE, 8=F8, 9=F10
       COPY "WsMenuDateInfo".
          
       COPY "ChlfdLinkage".
       PROCEDURE DIVISION.
           MOVE 'FRED'    TO CDA-DATA.
           MOVE 4         TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 10        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'A'       TO CDA-ATTR.
           PERFORM CTOS-DISPLAY.
           
           MOVE '16'      TO CDA-COL.
           MOVE ' '       TO CDA-DATA.
           MOVE 2         TO CDA-DATALEN.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           
           PERFORM OPEN-FILES.
           PERFORM DISPLAY-FORM.
           MOVE "0123456789012345678901234567890123456789" TO WS-CO-NAME.
           CALL "DATEMAILCO" USING WS-CO-NAME.
           move x'0A' to F-EXIT-CH.
           move 0     to F-INIT-ICH.
           move 0     to F-INDEX.
           move 'AB'  to D-SELECTION.
        
           CALL 'WRITEFIELD' 
                USING F-ERROR1 F-FORM 
                      N-SELECTION N-CBSELECTION N-OCCSELECTION
                      D-SELECTION D-CBSELECTION F-TYPE 
           END-CALL.

           DISPLAY F-ERROR1 LINE 26 COLUMN 10 
                 WITH REVERSE-VIDEO FOREGROUND-COLOR IS 4. 
           call 'USERFILLFIELD' 
             using F-ERROR1 F-FORM
                   D-SELECTION
                   N-CBSELECTION 
                   F-INDEX
                   F-INITSTATE F-EXITSTATE
           end-call.

           DISPLAY F-ERROR1 LINE 26 COLUMN 15 
                 WITH REVERSE-VIDEO FOREGROUND-COLOR IS 4. 
           PERFORM VARYING SUBA FROM 1 BY 1 UNTIL SUBA > 26
             DIVIDE SUBA BY 7 GIVING PP-PRIORITY
                           REMAINDER WS-COLOR
             DISPLAY SUBA LINE SUBA COLUMN 1 
                 WITH REVERSE-VIDEO FOREGROUND-COLOR IS WS-COLOR 
           END-PERFORM.
           CALL "LOCKKBD" USING F-ERROR1.
           MOVE "0123456789012345678901234567890" TO WS-CO-NAME.
           CALL "DATEMAILCO" USING WS-CO-NAME.
           CALL "LOCKKBD" USING F-ERROR1.
           MOVE "012345678901234567890" TO WS-CO-NAME.
           CALL "DATEMAILCO" USING WS-CO-NAME.
           PERFORM VARYING SUBA FROM 1 BY 1 UNTIL SUBA > 26
             DIVIDE SUBA BY 7 GIVING PP-PRIORITY
                           REMAINDER WS-COLOR
             DISPLAY SUBA LINE SUBA COLUMN 2
                 WITH REVERSE-VIDEO FOREGROUND-COLOR IS WS-COLOR 
           END-PERFORM.
           CALL "LOCKKBD" USING F-ERROR1.
           CALL "XE4".
           PERFORM VARYING SUBA FROM 1 BY 1 UNTIL SUBA > 26
             DIVIDE SUBA BY 7 GIVING PP-PRIORITY
                           REMAINDER WS-COLOR
             DISPLAY SUBA LINE SUBA COLUMN 3
                 WITH REVERSE-VIDEO FOREGROUND-COLOR IS WS-COLOR 
           END-PERFORM.
           DISPLAY " XE4 " LINE 2 COLUMN 3.
           CALL "LOCKKBD" USING F-ERROR1.
           CALL "CLOSEFORM" USING F-ERROR1 F-FORM.
           PERFORM VARYING SUBA FROM 1 BY 1 UNTIL SUBA > 26
             DIVIDE SUBA BY 7 GIVING PP-PRIORITY
                           REMAINDER WS-COLOR
             DISPLAY SUBA LINE SUBA COLUMN 4
                 WITH REVERSE-VIDEO FOREGROUND-COLOR IS WS-COLOR 
           END-PERFORM.
           CALL "LOCKKBD" USING F-ERROR1.
           DISPLAY " CLOSEFORM " LINE 2 COLUMN 3.
           CALL "LOCKKBD" USING F-ERROR1.
           STOP RUN.
          
       OPEN-FILES SECTION.  
           MOVE "/ctools/lib/CoForms.Lib" TO F-FILENAME
                                             WS-FORMS-NAME.
           MOVE 33                        TO F-CBFILENAME
                                             WS-CBFORMS-NAME.
           MOVE "MainCont"                TO F-FORMNAME
           MOVE 8                         TO F-CBFORMNAME.
           COPY "OpenForms".
           EXIT.
           
       CTOS-DISPLAY SECTION.
           CALL 'DISPLAY' USING CDA-ERROR CDA-ROW CDA-COL
                                CDA-DATA CDA-DATALEN
                                CDA-COLOR CDA-ATTR.
           EXIT.                       
           
       CTOS-ACCEPT SECTION.
           CALL 'ACCEPT' USING CDA-ERROR CDA-ROW CDA-COL
                               CDA-DATA  CDA-DATALEN
                               CDA-COLOR CDA-ATTR
                               CDA-KEY   CDA-FILTER.
           MOVE CDA-KEY TO W-ESCAPE-KEY.                    
           EXIT.                                
           
       COPY "ReadFieldAlpha".
       COPY "WriteFieldAlpha".
       COPY "WriteFieldPty".
       COPY "DisplayForm".
       COPY "ReadMenuKBD".
       COPY "UserFillField".
       COPY "MenuClearScreen".
       COPY "ErrorMessage".
       COPY "DisplayProgNum".
