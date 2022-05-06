       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestStuff.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-USERNAME             PIC X(30) VALUE SPACES.
       01  W-ENTER                PIC X.
       01  W-PRINTCOMMAND.
           03  W-PRINTCOM1A        PIC X(6) VALUE SPACES.
           03  W-PRINTCOM1         PIC X(95) VALUE SPACES.
           03  W-PRINTCOM2         PIC X(50) VALUE SPACES.
       01  W-PYTHONCOMMAND.
           03  W-PYTHONCOM1A        PIC X(7) VALUE SPACES.
           03  W-PYTHONCOM1B        PIC X(13) VALUE SPACES.
           03  W-PYTHONCOM1         PIC X(50) VALUE SPACES.
           03  W-PYTHONCOM2         PIC X(50) VALUE SPACES.
       01  W-TEXT2PDFCOMMAND.
           03  W-TEXT2PDFCOM1A        PIC X(12) VALUE SPACES.
           03  W-TEXT2PDFCOM1         PIC X(37) VALUE SPACES.
           03  W-TEXT2PDFCOM2         PIC X(57) VALUE SPACES.
       01  W-PDFTKCOMMAND.
           03  W-PDFTKCOM1A        PIC X(15) VALUE SPACES.
           03  W-PDFTKCOM1         PIC X(33) VALUE SPACES.
           03  W-PDFTKCOM2         PIC X(57) VALUE SPACES.
       01  W-STATUS               PIC 9(4) BINARY COMP.
       01  WS-PRINTER             PIC X(5) VALUE "MP140".
       01  WS-PRINT-FILE          PIC X(50) VALUE 
                                           "/ctools/spl/steve.ttt".
      *
       PROCEDURE DIVISION.
       000-Main.
          ACCEPT W-USERNAME FROM ENVIRONMENT "USERNAME".
          DISPLAY "USERNAME: " W-USERNAME.
          
          ACCEPT W-ENTER.
          GO TO 010-MAIN.
      *    MOVE "lp -d" WS-PRINTER &
      *      "/ctools/dev/source/cobol/TestStuff.cob" TO W-PRINTCOMMAND.
      *    MOVE "/ctools/dev/source/cobol/TestStuff.cob" TO

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
          DISPLAY "PYTHON COMMAND: "
          ACCEPT W-ENTER.

          MOVE "python "       TO W-PYTHONCOM1A
          MOVE "fohtotext.py"  TO W-PYTHONCOM1B
          MOVE "-r invoice -T /ctools/spl/s.temp0.txt"  TO W-PYTHONCOM1
          MOVE " /ctools/spl/InPrintCo01"               TO W-PYTHONCOM2
                              
           CALL "SYSTEM" USING W-PYTHONCOMMAND
               RETURNING W-STATUS
               END-CALL.
          DISPLAY "STATUS of PYTHON CALL: " W-STATUS.
          ACCEPT W-ENTER.

          DISPLAY "TEXT2PDF COMMAND: "
          ACCEPT W-ENTER.
          MOVE "./text2pdf "                          TO W-TEXT2PDFCOM1A
          MOVE "/ctools/spl/.temp0.txt -fCourier-Bold" TO W-TEXT2PDFCOM1
          MOVE 
            " -t8 -s10 -x842 -y595 -c135 -l48 > /ctools/spl/.temp1.pdf"
                                                       TO W-TEXT2PDFCOM2
                              
          DISPLAY W-TEXT2PDFCOMMAND
           CALL "SYSTEM" USING W-TEXT2PDFCOMMAND
               RETURNING W-STATUS
               END-CALL.
          DISPLAY "STATUS of TEXT2PDF CALL: " W-STATUS.
          ACCEPT W-ENTER.
      *    STOP RUN.

          DISPLAY "PDFTK COMMAND: "
          ACCEPT W-ENTER.
          MOVE "/usr/bin/pdftk "                          TO W-PDFTKCOM1A
          MOVE "/ctools/spl/.temp1.pdf background" TO W-PDFTKCOM1
          MOVE 
            " /ctools/spl/invoice01.pdf output /cttools/spl/.temp2.pdf"
                                                   TO W-PDFTKCOM2
                              
          DISPLAY W-PDFTKCOMMAND
           CALL "SYSTEM" USING W-PDFTKCOMMAND
               RETURNING W-STATUS
               END-CALL.
          DISPLAY "STATUS of PDFTK CALL: " W-STATUS.
          ACCEPT W-ENTER.
          STOP RUN.

      *    text2pdf /ctools/spl/.temp0.txt -fCourier-Bold 
      *         -t8 -s10 -x842 -y595 -c135 -l48 > /ctools/spl/.temp1.pdf
          
      *    pdftk /ctools/spl/.temp1.pdf background 
      *        /ctools/spl/invoice01.pdf output /cttools/spl/.temp2.pdf
          
      *    pdftk /ctools/spl/.temp2.pdf cat 1-endwest 
      *                         output /ctools/spl/InPrintCo01.pdf
      *#
      *#--- add any extra commands here - perhap cups printing
      *#--- or sendfax via hylaFAX
