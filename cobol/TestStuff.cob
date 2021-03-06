       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestStuff.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY. 
           FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-USERNAME               PIC X(30) VALUE SPACES.
       01  W-ENTER                  PIC X.
       01  W-COMP                   PIC 99.
       01  W-PRINTCOMMAND.
           03  W-PRINTCOM1A         PIC X(6) VALUE SPACES.
           03  W-PRINTCOM1          PIC X(95) VALUE SPACES.
           03  W-PRINTCOM2          PIC X(50) VALUE SPACES.
       01  W-PDF-COMMAND.
           03  W-PDF-PRINTCOM1A     PIC X(6) VALUE SPACES.
           03  W-PDF-PRINTCOM1      PIC X(95) VALUE SPACES.
           03  W-PDF-PRINTCOM2      PIC X(50) VALUE SPACES.
       01  W-PYTHONCOMMAND.
           03  W-PYTHONCOM1A        PIC X(7) VALUE SPACES.
           03  W-PYTHONCOM1B        PIC X(28) VALUE SPACES.
           03  W-PYTHONCOM1         PIC X(38) VALUE SPACES.
           03  W-PYTHONCOM2         PIC X(36) VALUE SPACES.
       01  W-TEXT2PDFCOMMAND.
           03  W-TEXT2PDFCOM1A      PIC X(12) VALUE SPACES.
           03  W-TEXT2PDFCOM1       PIC X(39) VALUE SPACES.
           03  W-TEXT2PDFCOM2       PIC X(59) VALUE SPACES.
       01  W-PDFTKCOMMAND.
           03  W-PDFTKCOM1A         PIC X(15) VALUE SPACES.
           03  W-PDFTKCOM1          PIC X(33) VALUE SPACES.
           03  W-PDFTKCOM2          PIC X(57) VALUE SPACES.
       01  W-PDFTK2COMMAND.
           03  W-PDFTK2COM1A        PIC X(15) VALUE SPACES.
           03  W-PDFTK2COM1         PIC X(36) VALUE SPACES.
           03  W-PDFTK2COM2         PIC X(55) VALUE SPACES.
       01  W-STATUS                 PIC 9(4) BINARY COMP.
       01  WS-PRINTER               PIC X(5) VALUE "MP140".
       01  WS-PRINT-FILE            PIC X(50) VALUE 
                                           "/ctools/spl/steve.ttt".
       01  WS-COMMAND-LINE          PIC X(256).                                    
      *
       PROCEDURE DIVISION.
       000-Main.
      * printing routine only for test purposes......
       
          ACCEPT W-USERNAME FROM ENVIRONMENT "USERNAME".
          DISPLAY "USERNAME: " W-USERNAME.

      *    MOVE CONCATENATE('invoice01 ', TRIM(W-USERNAME)) 
      *      TO WS-COMMAND-LINE.
      *    DISPLAY WS-COMMAND-LINE.  

          MOVE CONCATENATE('text2pdf ', TRIM(W-USERNAME), ' > fred.log') 
            TO WS-COMMAND-LINE.
          CALL "SYSTEM" USING WS-COMMAND-LINE.  
          DISPLAY WS-COMMAND-LINE.  
          
          ACCEPT W-ENTER.
          GO TO 050-MAIN.
      *****************************************************************
      * printing routine - sends disk file to printer....
      *    MOVE "lp -d" WS-PRINTER &
      *      "/ctools/dev/source/cobol/TestStuff.cob"    TO W-PRINTCOM1
      *    MOVE "/ctools/dev/source/cobol/TestStuff.cob" TO W-PRINTCOM2
      *****************************************************************

          MOVE "lp -d "       TO W-PRINTCOM1A
          MOVE WS-PRINTER     TO W-PRINTCOM1
          MOVE WS-PRINT-FILE  TO W-PRINTCOM2.
          
          DISPLAY "PRINT COMMAND: " W-PRINTCOMMAND.
          ACCEPT W-ENTER.
           CALL "SYSTEM" USING W-PRINTCOMMAND 
               RETURNING W-STATUS
               END-CALL.
               
          DISPLAY "STATUS of CALL: " W-STATUS.
          ACCEPT W-ENTER.
          STOP RUN.
       010-Main.
      *****************************************************************
      * invoice01 routine.  Section to convert text file into .pdf then
      * merge with overlay .pdf file, rotate and send to printer.
      *****************************************************************
          DISPLAY "PYTHON COMMAND: "
          ACCEPT W-ENTER.

          MOVE "python "                                TO W-PYTHONCOM1A
          MOVE "./fohtotext.py W-USERNAME "            TO W-PYTHONCOM1B
          MOVE "-r invoice -T /ctools/spl/$1.temp0.txt" 
                                                         TO W-PYTHONCOM1
          MOVE " /ctools/spl/$1InPrintCo01"              TO W-PYTHONCOM2
                              
          DISPLAY W-PYTHONCOMMAND
           CALL "SYSTEM" USING W-PYTHONCOMMAND
               RETURNING W-STATUS
               END-CALL.
          DISPLAY "STATUS of PYTHON CALL: " W-STATUS.
          ACCEPT W-ENTER.

          DISPLAY "TEXT2PDF COMMAND: "
          ACCEPT W-ENTER.
          MOVE "./text2pdf W-USERNAME "              TO W-TEXT2PDFCOM1A
          MOVE "/ctools/spl/$1.temp0.txt -fCourier-Bold" 
                                                     TO W-TEXT2PDFCOM1
          MOVE 
          " -t8 -s10 -x842 -y595 -c135 -l48 > /ctools/spl/$1.temp1.pdf"
                                                     TO W-TEXT2PDFCOM2
                              
          DISPLAY W-TEXT2PDFCOMMAND
           CALL "SYSTEM" USING W-TEXT2PDFCOMMAND
               RETURNING W-STATUS
               END-CALL.
          DISPLAY "STATUS of TEXT2PDF CALL: " W-STATUS.
          ACCEPT W-ENTER.

       020-Main.
          DISPLAY "PDFTK COMMAND: "
          ACCEPT W-ENTER.
          MOVE "./pdftk "                   TO W-PDFTKCOM1A
          MOVE "/ctools/spl/.temp1.pdf background" TO W-PDFTKCOM1
          MOVE 
            " /ctools/spl/invoice01.pdf output /ctools/spl/.temp2.pdf"
                                                   TO W-PDFTKCOM2
                              
          DISPLAY W-PDFTKCOMMAND
           CALL "SYSTEM" USING W-PDFTKCOMMAND
               RETURNING W-STATUS
               END-CALL.
          DISPLAY "STATUS of PDFTK CALL: " W-STATUS.
          ACCEPT W-ENTER.
          STOP RUN.
       030-Main.
          DISPLAY "PDFTK 2nd COMMAND: "
          ACCEPT W-ENTER.
          MOVE "./pdftk W-USERNAME"                     TO W-PDFTK2COM1A
          MOVE "/ctools/spl/.temp2.pdf cat 1-endwest" TO W-PDFTK2COM1
          MOVE " output /ctools/spl/InPrintCo01.pdf"  TO W-PDFTK2COM2
                              
          DISPLAY W-PDFTK2COMMAND
           CALL "SYSTEM" USING W-PDFTK2COMMAND
               RETURNING W-STATUS
               END-CALL.
          DISPLAY "STATUS of PDFTK 2nd CALL: " W-STATUS.
          ACCEPT W-ENTER.

          ACCEPT W-USERNAME FROM ENVIRONMENT "USERNAME".
          DISPLAY "USERNAME: " W-USERNAME.
          
          ACCEPT W-ENTER.

       040-Main.
          MOVE "lp -d "                       TO W-PRINTCOM1A
          MOVE "MP140"                        TO W-PRINTCOM1
          MOVE "/ctools/spl/InPrintCo01.pdf"  TO W-PRINTCOM2.
          
          DISPLAY "PRINT COMMAND: " W-PRINTCOMMAND.
          ACCEPT W-ENTER.
           CALL "SYSTEM" USING W-PRINTCOMMAND 
               RETURNING W-STATUS
               END-CALL.
               
       045-Main.
          DISPLAY "STATUS of CALL: " W-STATUS.
          ACCEPT W-ENTER.
          STOP RUN.

      ****************************************************************
      * invoice01 routine
      *    text2pdf /ctools/spl/.temp0.txt -fCourier-Bold 
      *         -t8 -s10 -x842 -y595 -c135 -l48 > /ctools/spl/.temp1.pdf
      *
      *    pdftk /ctools/spl/.temp1.pdf background 
      *        /ctools/spl/invoice01.pdf output /cttools/spl/.temp2.pdf
      *
      *    pdftk /ctools/spl/.temp2.pdf cat 1-endwest 
      *                         output /ctools/spl/InPrintCo01.pdf
      *#
      *#--- add any extra commands here - perhapS cups printing
      *#--- or sendfax via hylaFAX
      *****************************************************************
       050-MAIN.
      *vinces version as per email - but can't get it to work.....
          MOVE 99 TO W-COMP.
          MOVE 
          CONCATENATE('./PrintInvoice ', TRIM(W-USERNAME), ' '(W-COMP)) 
            TO WS-COMMAND-LINE.
          DISPLAY WS-COMMAND-LINE.  
           
          ACCEPT W-ENTER.
      *     MOVE CONCATENATE('./invoice01 ', TRIM(W-USERNAME)) 
      *                     TO W-PDF-COMMAND.
      *    DISPLAY W-PDF-COMMAND. 
          CALL "SYSTEM" USING WS-COMMAND-LINE
                    RETURNING W-STATUS.
        999-MAIN.
           STOP RUN.
