        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrDuFxRp.
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
         Copy "SelectDrMaster".
         Copy "SelectDrTrans".
         Copy "SelectSlParameter".
         Copy "SelectCoFaxParam".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SPL-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  PRINT-FILE.
       01  PRINT-REC                PIC X(80).
      *
       COPY ChlfdDebtor.
       COPY ChlfdDrTrans.
       COPY ChlfdParam.
       COPY ChlfdFaxParam.
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTER-TYPE       PIC X.
       77  WS-VALID-LINE-PRINTED PIC X.
       77  WS-FOUND             PIC X VALUE " ".
       77  WS-DOTPRINTER        PIC X(100) VALUE " ".
       77  WS-NOTIFY            PIC X VALUE " ".
       77  WS-ENTERD            PIC X VALUE " ".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-RANGE1            PIC 9(7) VALUE 0.
       77  WS-RANGE2            PIC 9(7) VALUE 0.
       77  WS-PERIOD            PIC 9 VALUE 0.
       77  WS-AMT-OWED-PERIOD   PIC S9(8)V99 VALUE 0.
       77  WS-AMT-OF-INVOICE    PIC S9(8)V99 VALUE 0.
       77  WS-TOT-BALANCE       PIC S9(8)V99 VALUE 0.
       77  WS-TOT-CURRENT       PIC S9(8)V99 VALUE 0.
       77  WS-TOT-30DAY         PIC S9(8)V99 VALUE 0.
       77  WS-TOT-60DAY         PIC S9(8)V99 VALUE 0.
       77  WS-TOT-90DAY         PIC S9(8)V99 VALUE 0.
       77  WS-TOT-120DAY        PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-BALANCE     PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-CURRENT     PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-30DAY       PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-60DAY       PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-90DAY       PIC S9(8)V99 VALUE 0.
       77  WS-GRTOT-120DAY      PIC S9(8)V99 VALUE 0.
       77  WS-LETTER-TOT        PIC 9(6) VALUE 0.
       77  WS-LETTER-TOT-DIS    PIC Z(5)9.
       77  WS-FAXED-TOT         PIC 9(4) VALUE 0.
       77  WS-FAXED-TOT-DIS     PIC Z(3)9.
       77  WS-FAX-NUMBER        PIC X(20) VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-PRINT-Y-N         PIC X VALUE " ".
       77  WS-BAD-FAX           PIC X VALUE " ".
       77  WS-CASH-ACC          PIC X VALUE " ".
       77  WS-BY-DATE           PIC X VALUE " ".
       77  PAGE-CNT             PIC 9(2) VALUE 0.
       77  LINE-CNT             PIC 9(3) VALUE 999.
       77  WS-REFNO             PIC 9(4) VALUE 0.
       77  WS-CONTACT           PIC X(20) VALUE " ".
       77  WS-SPEC-MESSAGE      PIC X(48) VALUE " ".
       77  WS-QUES-ACC-CONTACT  PIC X(15) VALUE " ".
       77  WS-QUES-ACC-PHONE    PIC X(15) VALUE " ".
       77  WS-PRINTER-PAGE1     PIC X(100) VALUE " ".
       77  WS-PRINTER-PAGE2     PIC X(100) VALUE " ".
       77  WS-PRINTER-PAGE3     PIC X(100) VALUE " ".
       77  WS-PRINTER-PAGE4     PIC X(100) VALUE " ".
       77  WS-PRINTER-PAGE5     PIC X(100) VALUE " ".
       77  WS-PRINTER-PDF       PIC X(100) VALUE " ".
       77  WS-SUBJECT-FIXED     PIC X(100) VALUE " ".
       01  W-CRTSTATUS          PIC 9(4) value 0.
       01  WS-EMAIL             PIC X(50).
       01  WS-TEMP-EMAIL-FILE   PIC X(50).
       01  WS-ACCOUNT-COMMENT.
           03  WS-ACC-MESSAGE    PIC X(53).
           03  WS-ACC-NUMBER     PIC X(7).
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1       PIC 99.
       01  WS-DRTRANS-STATUS.
           03  WS-DRTRANS-ST1      PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1  PIC 99.
       01  WS-SPL-STATUS.
           03  WS-SPL-ST1          PIC 99.
       01  WS-Fax-STATUS.
           03  WS-Fax-ST1          PIC 99.
       01  WS-SUBJECT.
           03  WS-SUBJECT-LINE1        PIC X(15) VALUE " ".
           03  WS-SUBJECT-LINE2        PIC X(11).
           03  WS-SUBJECT-LINE3        PIC X(7) VALUE " ".
           03  WS-SUBJECT-LINE4        PIC X(40) VALUE " ".
       01  WS-XQS-FAX.
           03  WS-XQS-BEG       PIC X(9) VALUE "(:(FaxTo=".
           03  WS-XQS-END       PIC X(3) VALUE "):)".
           03  WS-XQS-COVERS.
               05  WS-XQS1      PIC X(6) VALUE "Cover=".
               05  WS-XQS2      PIC X(2) VALUE "  ".
               05  WS-XQS3      PIC X(12) VALUE "Quote\Other=".
               05  WS-XQS4      PIC X(2) VALUE "  ".
               05  WS-XQS5      PIC X(6) VALUE "Quote2".
           03  WS-XQS-FROM.
               05  WS-XQS-FROMID    PIC X(6) VALUE "\From=".
               05  WS-XQS-FROM-NAME PIC X(25) VALUE " ".
           03  WS-XQS-COMMENT-LINE.
               05 WS-XQS-COMM-DESC PIC X(34) VALUE
                "(:(Comment=OUR OVERDUE FAX REF #: ".
               05 WS-XQS-COMMENT   PIC X(17) VALUE " ".
           03  WS-XQS-PRIORITY     PIC X(26) VALUE
               "Pri=N\OurRef=OVERDUE A/C\".
           03  WS-XQS-USERNAME.
               05  WS-XQS-SENT   PIC X(6) VALUE "NSent=".
               05  WS-XQS-SNAME  PIC X(25) VALUE " ".
               05  WS-XQS-ERROR  PIC X(8) VALUE "\NError=".
               05  WS-XQS-ENAME  PIC X(25) VALUE " ".
       01  WS-HYLA-TO-LINE.
           03  FILLER             PIC X(16) VALUE " ".
           03  WS-HYLA-TO-NAME    PIC X(25) VALUE " ".
       01 WS-HYLA-FROM-LINE.
           03  FILLER             PIC X(16) VALUE " ".
           03  WS-HYLA-FROM-NAME  PIC X(25) VALUE " ".
           03  FILLER             PIC X(28) VALUE " ".
           03  WS-HYLA-PAGE       PIC Z9 VALUE " ".
       01  WS-HYLA-TYPE-LINE.
           03  FILLER             PIC X(16) VALUE " ".
           03  WS-HYLA-TYPE       PIC X(30) VALUE "*OVERDUE A/C*".
           03  WS-HYLA-DATE       PIC X(30) VALUE " ".
       01  WS-HYLA-COMMENT-LINE.
           03  FILLER             PIC X(16) VALUE " ".
           03 WS-HYLA-COMM-DESC   PIC X(23) VALUE
              "DEBTOR OVER DUE REF #: ".
           03 WS-HYLA-COMMENT     PIC X(17) VALUE " ".
       01  WS-HYLA-TYPE-LINE2.
           03  FILLER             PIC X(16) VALUE " ".
           03  WS-HYLA-TYPE2      PIC X(30) VALUE "*OVERDUE A/C*".
       01 WS-HYLA-FROM-LINE2.
           03  FILLER             PIC X(7) VALUE " ".
           03  WS-HYLA-PAGE2      PIC Z9 VALUE " ".
       01  WS-FILE-NAME-FOR-FAX.
           03  WS-FOLDER-NAME         PIC X(12) VALUE "/ctools/fax/".
           03  WS-QUOTE-REFERENCE     PIC X(15) VALUE " ".
       01  WS-XQS-FAX-LINE-NAMES.
         02  WS-XQS-FAX-LINES OCCURS 5.
           03  WS-XQS-LINE     PIC X(100) VALUE " ".
       01  WS-SAVE-DATE.
           03  WS-SAVE-YY           PIC 9999.
           03  WS-SAVE-MM           PIC 99.
           03  WS-SAVE-DD           PIC 99.
       01  WS-COM-PRINT.
           03  WS-COM1         PIC X(39) VALUE " ".
           03  WS-COM2         PIC X(38) VALUE " ".
       01  WS-REFERENCE.
           03  WS-REF           PIC 9(4).
           03  WS-REFDOT1       PIC X.
           03  WS-REFMM         PIC 99.
           03  WS-REFDOT2       PIC X.
           03  WS-REFYY         PIC 9999.
       01  WS-REFNO-CHECK.
           03  WS-RF-C          PIC X OCCURS 25.
       01  WS-SP-PRINT.
           03  WS-1ST-15CHAR    PIC X(15).
           03  WS-REST          PIC X(23).
       01  WS-TYPES.
           03  FILLER           PIC X(7) VALUE "Invoice".
           03  FILLER           PIC X(7) VALUE "Payment".
           03  FILLER           PIC X(7) VALUE "R/D Chq".
           03  FILLER           PIC X(7) VALUE "Jnl-Dr".
           03  FILLER           PIC X(7) VALUE "Jnl-Cr".
           03  FILLER           PIC X(7) VALUE "C/Note".
           03  FILLER           PIC X(7) VALUE "Interst".
           03  FILLER           PIC X(7) VALUE "Discnt".
           03  FILLER           PIC X(7) VALUE "B-Debt".
           03  FILLER           PIC X(7) VALUE "Chq Ref".
       01  WS-TYPES-RED REDEFINES WS-TYPES.
           03  WS-TYPE-DESC     PIC X(7) OCCURS 10.
       01  HEAD1.
           03  FILLER           PIC X(7) VALUE " DATE:".
           03  H-DATE           PIC X(10) VALUE " ".
           03  FILLER           PIC X(4) VALUE " ".
           03  FILLER           PIC X(50) VALUE
           "**** ATTENTION CREDITORS DEPARTMENT ****".
           03  FILLER           PIC X(5) VALUE "PAGE:".
           03  H-PAGE           PIC Z9.
       01  HEAD2.
           03  FILLER           PIC X(20) VALUE " ".
           03  H-ADD            PIC X(28).
           03  H2-DESC          PIC X(10).
           03  H-NUM            PIC X(20).
       01  HEAD3.
           03  FILLER           PIC X(12) VALUE "Account No:".
           03  DEBT-ACCNO       PIC 9(7).
           03  FILLER           PIC X(29) VALUE " ".
           03  FILLER           PIC X(10) VALUE "Your Fax#:".
           03  DEBT-FAX         PIC X(20) VALUE " ".
       01  HEAD3-1.
           03  DEBT-NAME        PIC X(48).
           03  H-DESC           PIC X(10) VALUE "Fax Ref #:".
           03  H-REFNO          PIC Z(3)9.
           03  FILLER           PIC X VALUE ".".
           03  H-REF-MM         PIC 99.
           03  FILLER           PIC X VALUE ".".
           03  H-REF-YY         PIC 9999.
       01  HEAD3-2.
           03  FILLER           PIC X(12) VALUE "   Balance:".
           03  DEBT-BALANCE     PIC Z(5)9.99-.
           03  FILLER           PIC X(26) VALUE " ".
           03  FILLER           PIC X(12) VALUE "Last Paid:".
           03  DEBT-PAID-DATE   PIC X(10).
       01  HEAD5.
           03  FILLER           PIC X(24) VALUE "  TYPE  REF.NO   DATE".
           03  FILLER           PIC X(11) VALUE "   CURRENT".
           03  FILLER           PIC X(11) VALUE " 31-60 DAY".
           03  FILLER           PIC X(11) VALUE " 61-90 DAY".
           03  FILLER           PIC X(11) VALUE "91-120 DAY".
           03  FILLER           PIC X(12) VALUE " 121+ DAY".
       01  TRANS-LINE.
           03  TRANS-TYPE       PIC X(8) VALUE " ".
           03  TRANS-REFNO      PIC Z(5)9.
           03  FILLER           PIC X(1) VALUE " ".
           03  TRANS-DATE       PIC X(10).
           03  TRANS-CURRENT    PIC Z(5)9.99-.
           03  FILLER           PIC X(1) VALUE " ".
           03  TRANS-30DAY      PIC Z(5)9.99-.
           03  FILLER           PIC X(1) VALUE " ".
           03  TRANS-60DAY      PIC Z(5)9.99-.
           03  FILLER           PIC X(1) VALUE " ".
           03  TRANS-90DAY      PIC Z(5)9.99-.
           03  TRANS-120DAY     PIC Z(5)9.99-.
       01  PORDER-LINE.
           03  FILLER           PIC X(8) VALUE " ".
           03  FILLER           PIC X(8) VALUE "Order #".
           03  TRANS-PO         PIC X(61) VALUE " ".
       01  P-UNDERLINE.
           03  FILLER           PIC X(24) VALUE " ".
           03  FILLER           PIC X(11) VALUE " ---------".
           03  FILLER           PIC X(11) VALUE " ---------".
           03  FILLER           PIC X(11) VALUE " ---------".
           03  FILLER           PIC X(10) VALUE " ---------".
           03  FILLER           PIC X(10) VALUE " ---------".
       01  TOTAL-LINE.
           03  TOT-DESC         PIC X(14) VALUE " ".
           03  TOT-BALANCE      PIC Z(6)9.99-.
           03  TOT-CURRENT      PIC Z(5)9.99-.
           03  FILLER           PIC X(1) VALUE " ".
           03  TOT-30DAY        PIC Z(5)9.99-.
           03  FILLER           PIC X(1) VALUE " ".
           03  TOT-60DAY        PIC Z(5)9.99-.
           03  FILLER           PIC X(1) VALUE " ".
           03  TOT-90DAY        PIC Z(5)9.99-.
           03  TOT-120DAY       PIC Z(5)9.99-.
       01  DR-CLERK-LINE.
           03  FILLER           PIC X(16) VALUE "PLEASE CONTACT:".
           03  DR-CLERK         PIC X(16) VALUE " ".
           03  FILLER           PIC X(2) VALUE "@ ".
           03  DR-CLERK-PHONE   PIC X(17) VALUE " ".
       01  CONTINUE-LINE.
           03  CONT-DESC        PIC X(25).
           03  CONT-PAGE        PIC Z9.
           03  FILLER           PIC X(49). 
       Copy "WsDateInfo".
       Copy "WStore".
       Copy "FaxInfo".
      *
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN
           MOVE 0315 TO POS
           DISPLAY "** DEBTOR OVERDUE ACCOUNT FAX REPORT **" AT POS
           MOVE 0415 TO POS
           DISPLAY "***************************************" AT POS.
       CONT-003.
           Copy "PrinterAcceptDr".
           MOVE 3010 TO POS.
           DISPLAY "The Debtor Due Fax Program is being loaded." AT POS.
           MOVE Ws-Printer TO WS-DOTPRINTER WS-PRINTER-SAVE.
           PERFORM OPEN-DATA-FILES.
           PERFORM CLEAR-SCREEN.
       CONT-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
       CONT-030.
           MOVE 2715 TO POS.
           DISPLAY "The Report Is Being compiled On Account:       ...." 
           AT POS.
           MOVE " " TO WS-MESSAGE.
           PERFORM ERROR-020.
           PERFORM READ-DEBTOR-MASTER.
           IF WS-FOUND = " "
               MOVE "NOTHING TO PRINT IN THAT RANGE!!!" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE.
       CONT-040.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-030.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "RANGE1" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-900.
            MOVE 7            TO F-CBFIELDLENGTH
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-RANGE1.
            IF WS-RANGE1 = 0
               MOVE "THIS FIELD SHOULD BE > ZERO, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-030.
            PERFORM ERROR-020.
       GET-040.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "RANGE2"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-030.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-RANGE2.
            IF WS-RANGE2 < WS-RANGE1
               MOVE
               "THIS FIELD SHOULD BE = OR > THE FIRST FIELD, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-040.
            PERFORM ERROR-020.
       GET-050.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "PERIOD"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
               GO TO GET-040.
            MOVE 1            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-PERIOD.
            IF WS-PERIOD = 1 OR = 2 OR = 3 OR = 4 OR = 5
                GO TO GET-060.
            MOVE "YOUR OPTIONS ARE ONLY BETWEEN 1 & 5." TO WS-MESSAGE
            PERFORM ERROR-000
            GO TO GET-050.
            PERFORM ERROR-020.
       GET-060.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "DRCLERK"           TO F-FIELDNAME
            MOVE 7                   TO F-CBFIELDNAME
            MOVE WS-QUES-ACC-CONTACT TO F-NAMEFIELD
            MOVE 15                  TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-050.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-QUES-ACC-CONTACT.
            IF WS-QUES-ACC-CONTACT = "    "
               MOVE "THIS ENTRY MAY NOT BE BLANK, PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-060.
            PERFORM ERROR-020.
       GET-065.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "REFNO"   TO F-FIELDNAME
            MOVE 5         TO F-CBFIELDNAME
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-060.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA
            MOVE F-NAMEFIELD  TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO WS-REFNO.
            PERFORM ERROR-020.
       GET-080.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "FAX"     TO F-FIELDNAME
            MOVE 3         TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-065.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-AUTO-FAX.
            IF WS-AUTO-FAX NOT = "Y" AND NOT = "N" AND NOT = "E"
               MOVE "YOU MUST ANSWER 'Y', 'N' OR 'E', PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-080.
            PERFORM ERROR-020.
       GET-090.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "DELAY"   TO F-FIELDNAME
            MOVE 5         TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-080.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DELAY-FAX.
            IF WS-DELAY-FAX NOT = "Y" AND NOT = "N"
               MOVE "YOU MUST ANSWER 'Y' OR 'N', PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-090.
            PERFORM ERROR-020.
       GET-100.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "NOTIFY"  TO F-FIELDNAME
            MOVE 6         TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-090.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-NOTIFY.
      *      IF WS-NOTIFY NOT = "Y" AND NOT = "N"
      *         MOVE "YOU MUST ANSWER 'Y' OR 'N', PLEASE RE-ENTER"
            IF WS-NOTIFY NOT = "N"
               MOVE "YOU MUST ANSWER 'N', PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-100.
            PERFORM ERROR-020.
       GET-105.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "PRINT"   TO F-FIELDNAME
            MOVE 5         TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-100.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-PRINT-Y-N.
            IF WS-AUTO-FAX = "Y" OR = "E"
              IF WS-PRINT-Y-N NOT = "P"
               MOVE 
           "IF FAX/EMAIL = Y OR = E, YOU MUST ANSWER 'P' NOW."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-105.
               
            IF WS-PRINT-Y-N NOT = "Y" AND NOT = "N" AND NOT = "P"
               MOVE "YOU MUST ANSWER 'Y', 'N' OR 'P', PLEASE RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-105.
            MOVE WS-PRINT-Y-N TO WS-ANSWER.
            PERFORM ERROR-020.
       GET-110.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "BAD-FAX" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-105.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-BAD-FAX.
      *********************************
      *Y=BOTH GOOD AND BAD FAX #'S    *
      *B=BAD ONLY                     *
      *N=NO BAD FAXES                 *
      *********************************
            IF WS-BAD-FAX NOT = "Y" AND NOT = "N" AND NOT = "B"
               MOVE "YOU MUST ANSWER 'B', 'N' OR 'Y', PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-110.
            PERFORM ERROR-020.
       GET-120.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "CASHACC" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-110.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-CASH-ACC.
      *********************************
      *Y=BOTH CASH AND CREDIT A/C'S   *
      *C=CASH ONLY                    *
      *N=NO CASH A/C'S (INTERNAL)     *
      *********************************
            IF WS-CASH-ACC NOT = "Y" AND NOT = "N" AND NOT = "C"
               MOVE "YOU MUST ANSWER 'C', 'N' OR 'Y', PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-120.
            PERFORM ERROR-020.
       GET-130.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "BY-DATE" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                 GO TO GET-120.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-BY-DATE.
      *********************************
      *I=BY INVOICE DATE              *
      *D=BY DELIVERED DATE            *
      *********************************
            IF WS-BY-DATE NOT = "I" AND NOT = "D"
               MOVE "YOU MUST ANSWER 'D' OR 'I', PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO GET-130.
            PERFORM ERROR-020.

            IF WS-BY-DATE = "D"
               GO TO GET-999.
       GET-140.
            PERFORM CLEAR-010.
            IF F-EXIT-CH = X"01"
                 GO TO GET-130.
            MOVE 2910 TO POS
            DISPLAY
           "COMMENT:[                                                ]"
                AT POS
            MOVE 3010 TO POS
              DISPLAY "48 CHARS" AT POS
            MOVE 2919 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 48        TO CDA-DATALEN.
           MOVE 26        TO CDA-ROW.
           MOVE 18        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-SPEC-MESSAGE.

      *     IF W-ESCAPE-KEY = 4
      *         GO TO GET-130.
       GET-999.
            EXIT.
      *
       READ-DEBTOR-MASTER SECTION.
       RDM-005.
           MOVE "."   TO WS-REFDOT1 WS-REFDOT2
           MOVE WS-MM TO WS-REFMM
           MOVE WS-YY TO WS-REFYY
           MOVE WS-QUES-ACC-CONTACT TO DR-CLERK
           MOVE WS-QUES-ACC-PHONE   TO DR-CLERK-PHONE
           MOVE WS-RANGE1           TO DR-KEY.
           START DEBTOR-MASTER KEY NOT < DR-KEY
               INVALID KEY NEXT SENTENCE.
       RDM-010.
           READ DEBTOR-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 10
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RDM-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR RECORD BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RDM-010.
           IF DR-KEY < WS-RANGE1
               GO TO RDM-010.
           IF DR-KEY > WS-RANGE2
               GO TO RDM-999.
               
           MOVE 2756 TO POS 
           DISPLAY DR-ACCOUNT-NUMBER AT POS.
               
           IF WS-CASH-ACC = "N"
            IF DR-ACCOUNT-NUMBER = 0300087 OR = 0300090 OR = 0300100
                 OR = 0300150 OR = 0300200 OR = 9999999
               GO TO RDM-010.
           IF WS-CASH-ACC = "C"
            IF DR-ACCOUNT-NUMBER   NOT = 0300087 AND NOT = 0300090
             AND NOT = 0300100 AND NOT = 0300150 AND NOT = 0300200
              AND NOT = 9999999
               GO TO RDM-010.
               
           IF DR-BALANCE NOT > 0
               GO TO RDM-010.
           IF DR-BALANCE < 0
               GO TO RDM-010.
           IF DR-BALANCE = 0
               GO TO RDM-010.
      ******************************************************************
      * THIS NEW SECTION IS TO MAKE SURE THAT UNALLOCATED              *
      * CREDITS IN THE PERIOD UNDER ASSESMENT IS NOT > THAN THE INVOICE*
      * AS THIS WOULD MEAN A FAX WOULD HAVE BEEN SENT PURELY BECAUSE   *
      * A PERIOD 60DAY EG. HAD AN INVOICE BUT A CREDIT IN 90DAY EG.    *
      * WOULD COVER THIS.                                              *
      ******************************************************************
               
           IF WS-PERIOD = 1
               COMPUTE WS-AMT-OWED-PERIOD = DR-CURRENT + DR-30DAY
                    + DR-60DAY + DR-90DAY + DR-120DAY.
           IF WS-PERIOD = 2
               COMPUTE WS-AMT-OWED-PERIOD = DR-30DAY + DR-60DAY +
                    DR-90DAY + DR-120DAY.
           IF WS-PERIOD = 3
               COMPUTE WS-AMT-OWED-PERIOD = DR-60DAY +
                    DR-90DAY + DR-120DAY.
           IF WS-PERIOD = 4
               COMPUTE WS-AMT-OWED-PERIOD = DR-90DAY + DR-120DAY.
           IF WS-PERIOD = 5
               COMPUTE WS-AMT-OWED-PERIOD = DR-120DAY.

           IF WS-PERIOD = 1 OR = 2 OR = 3 OR = 4 OR = 5
            IF WS-AMT-OWED-PERIOD > 0
               GO TO RDM-020.
           GO TO RDM-010.
       RDM-020.
           IF WS-FOUND = " "
              MOVE "Y" TO WS-FOUND.
           ADD 1 TO WS-LETTER-TOT
                    WS-REFNO.
           MOVE WS-REFNO      TO WS-REF.
           MOVE WS-REFNO      TO H-REFNO.
           MOVE WS-LETTER-TOT TO WS-LETTER-TOT-DIS.
           MOVE 2610 TO POS.
           DISPLAY "LETTERS PRINTED =  " AT POS.
           ADD 18 TO POS.
           DISPLAY WS-LETTER-TOT-DIS AT POS.
      *************************************
      * TO PRINT HARD COPY FIRST.         *
      *************************************
           IF WS-PRINT-Y-N = "N"
      *         MOVE "P" TO WS-AUTO-FAX
               GO TO RDM-021.
           MOVE Ws-DOTPrinter TO WS-PRINTER.
      *     IF WS-AUTO-FAX = "Y" OR = "E"
      *         MOVE "P" TO WS-AUTO-FAX.
           IF WS-AUTO-FAX = "N"
               MOVE "O" TO WS-AUTO-FAX.
           IF WS-PRINT-Y-N = "Y"
              PERFORM PRINT-ROUTINE.
      ******************************************************************
      * THIS NEW SECTION WAS ADDED DUE TO THE DRTR-DEL-DATE BEING USED *
      * SO THAT IF IT IS FOUND THAT THE HEADING IS PRINTED AND THE TAIL*
      * IS PRINTED BUT NO VALID LINE ITEMS PRINTED THEN SKIP THAT      *
      * DEBTOR AND GO READ THE NEXT ONE.  THIS IS DONE TO STOP FAXING  *
      * A LETTER WITH NO TRANSACTIONS ON IT.                           *
      ******************************************************************
           IF WS-VALID-LINE-PRINTED = "N"
               GO TO RDM-010.
       RDM-021.
      *************************************
      * TO PRINT COPY TO FAX UNIT.        *
      *************************************
           IF WS-AUTO-FAX = "O"
               MOVE "N" TO WS-AUTO-FAX
               GO TO RDM-010.
           IF WS-AUTO-FAX = "P"
               MOVE "Y" TO WS-AUTO-FAX.
      *************************************
      * NEW SECTION FOR XQS FAX UNIT.     *
      *************************************
           IF Fax-PaNumber = 3 OR = 4
               PERFORM CHECK-FAX-NUMBER.
      ********************************************************
      * REMOVED SO THAT ALL FAXES WILL BE SENT TO THE QUEUE  *
      * EVEN THE ONES WITH A BAD PHONE #.  SEE XQS-004.      *
      ********************************************************
      *      IF SIGN-FOUND = 1
      *         GO TO RDM-010
      *      ELSE
      * FAX-PANUMBER = 3 MEANS XQS CTOS FAX
      * FAX-PANUMBER = 4 MEANS HYLAFAX IN LINUX
           IF Fax-PaNumber = 3 OR = 4
            IF WS-BAD-FAX = "Y"
               PERFORM REMOVE-ZERO-IN-REFERENCE
               PERFORM ENTER-XQS-DETAILS
               PERFORM PRINT-XQS-ROUTINE
               GO TO RDM-030.
           IF Fax-PaNumber = 3 OR = 4
            IF WS-BAD-FAX = "B"
             IF SIGN-FOUND = 1
               PERFORM REMOVE-ZERO-IN-REFERENCE
               PERFORM ENTER-XQS-DETAILS
               PERFORM PRINT-XQS-ROUTINE
               GO TO RDM-030
             ELSE
               GO TO RDM-010.
           IF Fax-PaNumber = 3 OR = 4
            IF WS-BAD-FAX = "N"
             IF SIGN-FOUND NOT = 1
               PERFORM REMOVE-ZERO-IN-REFERENCE
               PERFORM ENTER-XQS-DETAILS
               PERFORM PRINT-XQS-ROUTINE
               GO TO RDM-030
             ELSE
               GO TO RDM-010.
      ********************************************
      *OLD SECTION FOR FAXING OF MURATA FAXES.   *
      ********************************************
           MOVE WS-REFNO          TO WS-REF.
           MOVE "/ctools/debtor/" TO WS-1ST-15CHAR.
           MOVE WS-REFERENCE      TO WS-REST.
           MOVE WS-SP-PRINT       TO WS-PRINTER.
           PERFORM PRINT-ROUTINE.
           MOVE "/ctools/fax/"    TO WS-FAX-12CHAR.
           PERFORM CHECK-FAX-NUMBER.
           IF SIGN-FOUND = 1
               GO TO RDM-010.
           PERFORM CHECK-REFERENCE.
           PERFORM PREPARE-FAX-SENDING.
       RDM-030.
           ADD 1 TO WS-FAXED-TOT.
           MOVE WS-FAXED-TOT TO WS-FAXED-TOT-DIS.
           MOVE 2650 TO POS.
           DISPLAY "LETTERS FAXED =  " AT POS.
           ADD 16 TO POS.
           DISPLAY WS-FAXED-TOT-DIS AT POS.
           
           IF Fax-PaNumber NOT = 3
            IF WS-DELAY-FAX = "N"
             IF WS-FAXED-TOT = 100
              MOVE
           "100 LETTERS HAVE BEEN FAXED, THE LAST ACCOUNT SENT IS:"
               TO WS-ACC-MESSAGE
              MOVE DR-ACCOUNT-NUMBER  TO WS-ACC-NUMBER
              MOVE WS-ACCOUNT-COMMENT TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RDM-999.
      *******************************************
      * W-DELAY =1000  IS 1MIN 40 SEC (100 SEC) *
      *******************************************
      *        MOVE 600    TO W-DELAY
      *        CALL "&DELAY" USING
      *                       W-ERROR
      *                       W-DELAY.

           IF WS-DELAY-FAX = "Y"
               CALL "C$SLEEP" USING 2.
           MOVE " " TO WS-PRINTER.
           MOVE WS-DOTPRINTER TO WS-PRINTER.
           GO TO RDM-010.
       RDM-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PR-000.
           MOVE "N" TO WS-VALID-LINE-PRINTED.
           PERFORM GET-USER-PRINT-NAME.
           
      *     MOVE " IN PRINT-ROUTINE" TO WS-MESSAGE
      *     PERFORM ERROR1-000 
      *     MOVE WS-PRINTER TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           MOVE WS-DOTPRINTER TO WS-PRINTER.
           OPEN OUTPUT PRINT-FILE.
           IF WS-SPL-ST1 NOT = 0
               CLOSE PRINT-FILE
               MOVE "SPOOLER STATUS NOT = 0 ON OPEN, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SPL-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PR-000.
           MOVE DR-ACCOUNT-NUMBER TO DRTR-ACC-KEY.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACC-KEY
               INVALID KEY NEXT SENTENCE.
       PR-002.
           READ DEBTOR-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 10
               PERFORM SUBTOTALS
               GO TO PR-900.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE
                "DR-TRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO PR-002.
           IF DRTR-AMT-OUTSTANDING = 0
               GO TO PR-002.
           IF DRTR-ACCOUNT-NUMBER = DR-ACCOUNT-NUMBER
               GO TO PR-010.
           PERFORM SUBTOTALS.
           GO TO PR-900.
       PR-010.
           IF LINE-CNT > 60
            IF LINE-CNT < 100
              MOVE "****   CONTINUED TO PAGE:" TO CONT-DESC
              ADD 1 TO PAGE-CNT
              MOVE PAGE-CNT                TO CONT-PAGE
              WRITE PRINT-REC FROM CONTINUE-LINE AFTER 2
              SUBTRACT 1 FROM PAGE-CNT
              MOVE " " TO PRINT-REC
              MOVE "**** CONTINUED FROM PAGE:" TO CONT-DESC
              MOVE PAGE-CNT                TO CONT-PAGE
              WRITE PRINT-REC FROM CONTINUE-LINE AFTER PAGE
              ADD 1 TO PAGE-CNT
              MOVE " " TO PRINT-REC
              PERFORM PH-010
              GO TO PR-020.
           IF LINE-CNT = 999
              PERFORM PRINT-HEADINGS.
       PR-020.
      ******************************************************************
      * CHANGED ON 18/12/2002.                                         *
      * USING DEL-DATE INSTEAD OF INVOICE DATE AS CUSTOMERS HAVE GOTTEN*
      * PISSED OFF BY FAXES FOR GOODS THEY NEVER RECEIVED IN THAT MONTH*
      * DR-DEL-DATE = 0 MEANS GOODS NOT YET FLAGGED AS DELIVERED       *
      * THEREFORE OMIT FROM THE PRINT.                                 *
      *                                                                *
      * CHANGED 31/12/2008                                             *
      * ADDED IN I-INVOICE DATE, D-DEL DATE                            *
      * DONE SO THAT ACCOUNTS LADIES CAN USE THIS METHOD OF SENDING    *
      * FAXES ON THE FLY INSTEAD OF JUST IN THE MAIN BATCH RUN.        *
      ******************************************************************
           IF WS-BY-DATE = "D"
            IF DRTR-DEL-DATE = 0
               GO TO PR-002.
               
           MOVE WS-TYPE-DESC (DRTR-TYPE) TO TRANS-TYPE.
           MOVE DRTR-REFERENCE1          TO TRANS-PO.
           MOVE DRTR-REFERENCE2          TO TRANS-REFNO.
           IF WS-BY-DATE = "D"
               MOVE DRTR-DEL-DATE        TO WS-AGE-DATE
                                         SPLIT-DATE
           ELSE
               MOVE DRTR-DATE            TO WS-AGE-DATE
                                         SPLIT-DATE.
                                         
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE             TO TRANS-DATE.
           MOVE WS-SAVE-DATE             TO WS-DATE.
           IF WS-AGE-YY NOT = WS-YY
               COMPUTE WS-MM = (((WS-YY - WS-AGE-YY) * 12)
                                   + WS-MM).
           SUBTRACT WS-AGE-MM FROM WS-MM.
           
           MOVE DRTR-AMT-OUTSTANDING TO WS-AMT-OF-INVOICE.
           IF DRTR-TYPE = 2 OR = 5 OR = 6 OR = 8 OR = 9
               COMPUTE WS-AMT-OF-INVOICE = WS-AMT-OF-INVOICE * -1. 


           IF WS-PERIOD = 1
            IF WS-MM = 0
               ADD WS-AMT-OF-INVOICE TO WS-TOT-CURRENT
                                        WS-GRTOT-CURRENT
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-CURRENT
               GO TO PR-030.
           IF WS-PERIOD = 1 OR = 2
            IF WS-MM = 1
               ADD WS-AMT-OF-INVOICE TO WS-TOT-30DAY
                                        WS-GRTOT-30DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-30DAY
               GO TO PR-030.
           IF WS-PERIOD = 1 OR = 2 OR = 3
            IF WS-MM = 2
               ADD WS-AMT-OF-INVOICE TO WS-TOT-60DAY
                                        WS-GRTOT-60DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-60DAY
               GO TO PR-030.
           IF WS-PERIOD = 1 OR = 2 OR = 3 OR = 4
            IF WS-MM = 3
               ADD WS-AMT-OF-INVOICE TO WS-TOT-90DAY
                                        WS-GRTOT-90DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-90DAY
               GO TO PR-030.
           IF WS-PERIOD = 1 OR = 2 OR = 3 OR = 4 OR = 5
            IF WS-MM > 3
               ADD WS-AMT-OF-INVOICE TO WS-TOT-120DAY
                                        WS-GRTOT-120DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-120DAY
               GO TO PR-030.
           GO TO PR-002.
       PR-030.
           WRITE PRINT-REC FROM TRANS-LINE AFTER 1
           MOVE " " TO PRINT-REC TRANS-LINE
           WRITE PRINT-REC FROM PORDER-LINE AFTER 1
           MOVE " " TO PRINT-REC
           ADD 2 TO LINE-CNT
           MOVE "Y" TO WS-VALID-LINE-PRINTED
           GO TO PR-002.
       PR-900.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       PR-999.
           EXIT.
      *
       REMOVE-SPACES-IN-FAX-NAME SECTION.
       RSIFN-005.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
           MOVE WS-FILE-NAME-FOR-FAX TO ALPHA-RATE
           MOVE 1 TO SUB-45.
       RSIFN-010.
           IF AL-RATE (SUB-45) NOT = " "
              ADD 1 TO SUB-45 
              GO TO RSIFN-010.
           MOVE "-" TO AL-RATE (SUB-45).
           ADD 1 TO SUB-45.
           IF PAGE-CNT = 1
              MOVE 1 TO AL-RATE (SUB-45)
           ELSE
              MOVE 2 TO AL-RATE (SUB-45).
           MOVE ALPHA-RATE TO WS-PRINTER.
       RSIFN-999.
           EXIT.
      *
       PRINT-XQS-ROUTINE SECTION.
       PRXQS-000.
           MOVE 1 TO PAGE-CNT.
           IF Fax-PaNumber = 3
            IF WS-AUTO-FAX = "Y"
               MOVE "[QFax]" TO WS-PRINTER.
               
           IF Fax-PaNumber = 4
               MOVE WS-HYLA-COMMENT TO WS-QUOTE-REFERENCE
               PERFORM REMOVE-SPACES-IN-FAX-NAME
               MOVE WS-PRINTER TO WS-PRINTER-PAGE1.
           
      *     MOVE " IN XQS PRINT-ROUTINE" TO WS-MESSAGE
      *     PERFORM ERROR1-000 
      *     MOVE WS-PRINTER TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
               
           OPEN OUTPUT PRINT-FILE.
           IF WS-SPL-ST1 NOT = 0
               CLOSE PRINT-FILE
               MOVE "SPOOLER STATUS NOT = 0, 'ESC' TO RE-TRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SPL-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PRXQS-000.
           IF Fax-PaNumber = 3
            IF WS-AUTO-FAX = "Y"
               WRITE PRINT-REC FROM WS-XQS-LINE (1)
               WRITE PRINT-REC FROM WS-XQS-LINE (2)
               WRITE PRINT-REC FROM WS-XQS-LINE (3)
               WRITE PRINT-REC FROM WS-XQS-LINE (4)
             IF WS-NOTIFY = "Y"
               WRITE PRINT-REC FROM WS-XQS-LINE (5).
               
           IF Fax-PaNumber = 4
               PERFORM GET-REPORT-Y2K-DATE
               MOVE PBRET          TO WS-REPORT-DATE
               MOVE WS-REPORT-DATE TO WS-HYLA-DATE
               MOVE PAGE-CNT       TO WS-HYLA-PAGE
               
               MOVE SPACES TO PRINT-REC
               WRITE PRINT-REC AFTER 11
               WRITE PRINT-REC FROM WS-HYLA-TO-LINE
               MOVE SPACES TO PRINT-REC
               WRITE PRINT-REC
               WRITE PRINT-REC FROM WS-HYLA-FROM-LINE
               MOVE SPACES TO PRINT-REC
               WRITE PRINT-REC
               WRITE PRINT-REC FROM WS-HYLA-TYPE-LINE
               MOVE SPACES TO PRINT-REC
               WRITE PRINT-REC
               WRITE PRINT-REC FROM WS-HYLA-COMMENT-LINE
               MOVE SPACES TO PRINT-REC
               WRITE PRINT-REC.
              
           MOVE DR-ACCOUNT-NUMBER TO DRTR-ACC-KEY.
           START DEBTOR-TRANS-FILE KEY NOT < DRTR-ACC-KEY
               INVALID KEY NEXT SENTENCE.
       PRXQS-002.
           READ DEBTOR-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-DRTRANS-ST1 = 10
               PERFORM SUBTOTALS
               GO TO PRXQS-900.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE 
              "DRTRANS BUSY ON READ-NEXTPRXQS, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DRTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DRTRANS-ST1
               GO TO PRXQS-002.
           IF DRTR-AMT-OUTSTANDING = 0
               GO TO PRXQS-002.
           IF DRTR-ACCOUNT-NUMBER = DR-ACCOUNT-NUMBER
               GO TO PRXQS-010.
           PERFORM SUBTOTALS.
           GO TO PRXQS-900.
       PRXQS-010.
           IF LINE-CNT = 999
              PERFORM PRINT-HEADINGS.

           IF Fax-PaNumber = 3
            IF PAGE-CNT = 1
             IF LINE-CNT > 55
              ADD 1 TO PAGE-CNT
              WRITE PRINT-REC FROM HEAD5 AFTER PAGE
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              MOVE 2 TO LINE-CNT
              GO TO PRXQS-020.
           IF Fax-PaNumber = 3
            IF LINE-CNT > 57
             IF LINE-CNT < 100
              ADD 1 TO PAGE-CNT
              WRITE PRINT-REC FROM HEAD5 AFTER PAGE
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC AFTER 1
              MOVE 2 TO LINE-CNT
              GO TO PRXQS-020.
              
           IF Fax-PaNumber = 4
            IF PAGE-CNT = 1
      *       IF LINE-CNT > 42
             IF LINE-CNT > 55
              ADD 1 TO PAGE-CNT
                 CLOSE PRINT-FILE
                 PERFORM REMOVE-SPACES-IN-FAX-NAME
                 MOVE WS-PRINTER TO WS-PRINTER-PAGE2
                 OPEN OUTPUT PRINT-FILE
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC FROM WS-HYLA-TYPE-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 MOVE PAGE-CNT       TO WS-HYLA-PAGE2
                 WRITE PRINT-REC FROM WS-HYLA-FROM-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC FROM HEAD5
                 MOVE " " TO PRINT-REC
      *           WRITE PRINT-REC
                 MOVE 8 TO LINE-CNT.
           IF Fax-PaNumber = 4
            IF PAGE-CNT > 1
             IF LINE-CNT > 55
              ADD 1 TO PAGE-CNT
                 MOVE " " TO PRINT-REC
                 WRITE PRINT-REC BEFORE PAGE
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC FROM WS-HYLA-TYPE-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 MOVE PAGE-CNT TO WS-HYLA-PAGE2
                 WRITE PRINT-REC FROM WS-HYLA-FROM-LINE2
                 MOVE SPACES TO PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC
                 WRITE PRINT-REC FROM HEAD5
                 MOVE " " TO PRINT-REC
      *           WRITE PRINT-REC
                 MOVE 8 TO LINE-CNT.
       PRXQS-020.
      ******************************************************************
      * CHANGED ON 18/12/2002.                                         *
      * USING DEL-DATE INSTEAD OF INVOICE DATE AS CUSTOMERS HAVE GOTTEN*
      * PISSED OFF BY FAXES FOR GOODS THEY NEVER RECEIVED IN THAT MONTH*
      * DR-DEL-DATE = 0 MEANS GOODS NOT YET FLAGGED AS DELIVERED       *
      * THEREFORE OMIT FROM THE PRINT.                                 *
      *                                                                *
      * CHANGED 31/12/2008                                             *
      * ADDED IN I-INVOICE DATE, D-DEL DATE                            *
      * DONE SO THAT ACCOUNTS LADIES CAN USE THIS METHOD OF SENDING    *
      * FAXES ON THE FLY INSTEAD OF JUST IN THE MAIN BATCH RUN.        *
      ******************************************************************
           IF WS-BY-DATE = "D"
            IF DRTR-DEL-DATE = 0
               GO TO PRXQS-002.
           MOVE WS-TYPE-DESC (DRTR-TYPE) TO TRANS-TYPE.
           MOVE DRTR-REFERENCE1          TO TRANS-PO.
           MOVE DRTR-REFERENCE2          TO TRANS-REFNO.
           MOVE DRTR-REFERENCE2          TO TRANS-REFNO.
           IF WS-BY-DATE = "D"
               MOVE DRTR-DEL-DATE        TO WS-AGE-DATE
                                         SPLIT-DATE
           ELSE
               MOVE DRTR-DATE            TO WS-AGE-DATE
                                         SPLIT-DATE.
                                         
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE             TO TRANS-DATE.
           MOVE WS-SAVE-DATE TO WS-DATE.
           IF WS-AGE-YY NOT = WS-YY
               COMPUTE WS-MM = (((WS-YY - WS-AGE-YY) * 12)
                                   + WS-MM).
           SUBTRACT WS-AGE-MM FROM WS-MM.
           MOVE DRTR-AMT-OUTSTANDING TO WS-AMT-OF-INVOICE.
           IF DRTR-TYPE = 2 OR = 5 OR = 6 OR = 8 OR = 9
               COMPUTE WS-AMT-OF-INVOICE = WS-AMT-OF-INVOICE * -1. 

           IF WS-PERIOD = 1
            IF WS-MM = 0
               ADD WS-AMT-OF-INVOICE TO WS-TOT-CURRENT
                                        WS-GRTOT-CURRENT
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-CURRENT
               GO TO PRXQS-030.
           IF WS-PERIOD = 1 OR = 2
            IF WS-MM = 1
               ADD WS-AMT-OF-INVOICE TO WS-TOT-30DAY
                                        WS-GRTOT-30DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-30DAY
               GO TO PRXQS-030.
           IF WS-PERIOD = 1 OR = 2 OR = 3
            IF WS-MM = 2
               ADD WS-AMT-OF-INVOICE TO WS-TOT-60DAY
                                        WS-GRTOT-60DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-60DAY
               GO TO PRXQS-030.
           IF WS-PERIOD = 1 OR = 2 OR = 3 OR = 4
            IF WS-MM = 3
               ADD WS-AMT-OF-INVOICE TO WS-TOT-90DAY
                                        WS-GRTOT-90DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-90DAY
               GO TO PRXQS-030.
           IF WS-PERIOD = 1 OR = 2 OR = 3 OR = 4 OR = 5
            IF WS-MM > 3
               ADD WS-AMT-OF-INVOICE TO WS-TOT-120DAY
                                        WS-GRTOT-120DAY
                                        WS-TOT-BALANCE
                                        WS-GRTOT-BALANCE
               MOVE WS-AMT-OF-INVOICE TO TRANS-120DAY
               GO TO PRXQS-030.
           GO TO PRXQS-002.
       PRXQS-030.
           WRITE PRINT-REC FROM TRANS-LINE AFTER 1
           MOVE SPACES TO PRINT-REC TRANS-LINE
           WRITE PRINT-REC FROM PORDER-LINE AFTER 1
           MOVE SPACES TO PRINT-REC
           ADD 2 TO LINE-CNT
           GO TO PRXQS-002.
       PRXQS-900.
           CLOSE PRINT-FILE.

      * IF PAGE-CNT > 2 WE MOVE 2 TO PAGE-CNT AS THERE ARE ONLY 
      * TWO FILES CREATED - 1 AND 2.  2 HAS ALL THE SUBSEQUENT PAGES
      * INSIDE IT.
           IF PAGE-CNT > 2 
              MOVE 2 TO PAGE-CNT.

           IF Fax-PaNumber = 4
            IF WS-ANSWER = "P" OR = "Y" OR = "E"
             IF PAGE-CNT = 1
                 PERFORM WORK-OUT-PDF-FILE-NAMES
                 MOVE WS-PRINTER-PAGE1   TO WS-PRINTER
                 PERFORM FIND-PDF-TYPE-PRINTER
                 PERFORM SETUP-OVERDUE-FOR-PDF
             ELSE
                 PERFORM WORK-OUT-PDF-FILE-NAMES
                 MOVE WS-PRINTER-PAGE1   TO WS-PRINTER
                 PERFORM FIND-PDF-TYPE-PRINTER
                 PERFORM SETUP-OVERDUE-FOR-PDF
                 MOVE WS-PRINTER-PAGE2   TO WS-PRINTER
                 PERFORM SETUP-OVERDUE2-FOR-PDF
                 PERFORM SETUP-MERGE-OVERDUE-FOR-PDF.
           
      *     MOVE WS-AUTO-FAX TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           
            IF WS-AUTO-FAX = "E"
                MOVE "DEBTOR OVERDUE"  TO WS-SUBJECT-LINE1
                MOVE WS-HYLA-COMMENT   TO WS-SUBJECT-LINE2
                MOVE " FROM:"          TO WS-SUBJECT-LINE3 
                MOVE WS-CO-NAME        TO WS-SUBJECT-LINE4
                PERFORM TAKE-OUT-BLANKS-IN-CO-NAME
                PERFORM MAKE-PDF-FINAL-FOR-EMAIL
                PERFORM SETUP-DRDUE-FOR-PDF-MGEMAIL. 
       PRXQS-999.
           EXIT.
      *
       MAKE-PDF-FINAL-FOR-EMAIL SECTION.
       MFPFE-001.
           MOVE SPACES           TO ALPHA-RATE DATA-RATE.
       MFPFE-002.
           MOVE "/ctools/fax/"   TO ALPHA-RATE
           
           MOVE WS-CO-NUMBER     TO DATA-RATE
           MOVE DAT-RATE(1) TO AL-RATE(13)
           MOVE DAT-RATE(2) TO AL-RATE(14)

           MOVE 1 TO SUB-45
           MOVE 1 TO SUB-50.
       MFPFE-005.
           MOVE AL-RATE (SUB-45) TO DAT-RATE (SUB-50)
           ADD 1 TO SUB-45 SUB-50.
           IF AL-RATE (SUB-45) NOT = " "
               GO TO MFPFE-005.
           MOVE "D"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "r"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "O"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "v"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "e"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "r"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "d"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "u"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "e"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           IF PAGE-CNT = 1
              GO TO MFPFE-007.
           MOVE "F"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "i"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "n"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "a"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "l"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
       MFPFE-007.
           MOVE SPACES TO ALPHA-RATE.
           IF PAGE-CNT = 1
              MOVE WS-PRINTER-PAGE1 TO ALPHA-RATE
           ELSE 
              MOVE WS-PRINTER-PAGE2 TO ALPHA-RATE.
           MOVE 1 TO SUB-45.
       MFPFE-010.
           MOVE AL-RATE (SUB-45) TO DAT-RATE (SUB-50)
           ADD 1 TO SUB-45 SUB-50.
           IF AL-RATE (SUB-45) NOT = " "
               GO TO MFPFE-010.
           MOVE "."  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "p"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "d"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE "f"  TO DAT-RATE (SUB-50)
                ADD 1 TO SUB-50.
           MOVE DATA-RATE TO WS-PRINTER-PDF.
           MOVE SPACES    TO ALPHA-RATE DATA-RATE.
           
      *     MOVE WS-PRINTER-PDF TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       MFPFE-999.
           EXIT.
      *
       TAKE-OUT-BLANKS-IN-CO-NAME SECTION.
       TOBICN-005.
           MOVE SPACES TO ALPHA-RATE DATA-RATE.
       TOBICN-005.
           MOVE WS-SUBJECT TO DATA-RATE.
           MOVE 1 TO SUB-1
           MOVE 1 TO SUB-2.
           MOVE 0 TO SUB-3.
           MOVE "'" TO AL-RATE (SUB-2).
           MOVE 2 TO SUB-1.
       TOBICN-010.
           MOVE DAT-RATE (SUB-2) TO AL-RATE (SUB-1)
           ADD 1 TO SUB-1 SUB-2.
           IF SUB-1 < 100
            IF DAT-RATE (SUB-2) NOT = " "
                MOVE 0 TO SUB-3
               GO TO TOBICN-010
            ELSE 
               ADD 1 TO SUB-3.
           IF SUB-3 = 1 OR = 2 OR = 3 OR = 4
              GO TO TOBICN-010.
           MOVE "'" TO AL-RATE (SUB-1).
       TOBICN-030.
           MOVE SPACES       TO WS-SUBJECT-FIXED
           MOVE ALPHA-RATE   TO WS-SUBJECT-FIXED.
       TOBICN-999.
           EXIT.
      *
       FIND-PDF-TYPE-PRINTER SECTION.
       FPTP-040.
           MOVE 1 TO SUB-45.
       FPTP-045.
           IF WS-PRINTERNAME (SUB-45) = " "
              MOVE 
           "NO PDF PRINTERNUMBER, PRN PARAMETER NOT SET UP."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO FPTP-999.
           IF WS-PRINTERNUMBER (SUB-45) = 15
               MOVE WS-PRINTERNAME (SUB-45)  TO WS-PRINTER-SAVE
               GO TO FPTP-999.
           IF SUB-45 < 25
             ADD 1 TO SUB-45
             GO TO FPTP-045.
           MOVE 
           "CAN'T FIND A PDF PRINTERNUMBER, PRN PARAMETER NOT SET UP."
             TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
       FPTP-999.
            EXIT.
      *
       WORK-OUT-PDF-FILE-NAMES SECTION.
       WOPFN-001.
           MOVE SPACES           TO ALPHA-RATE DATA-RATE.
           MOVE WS-PRINTER-PAGE1 TO ALPHA-RATE.
           MOVE 13 TO SUB-45
           MOVE 1  TO SUB-46.
       WOPFN-010.
           MOVE AL-RATE (SUB-45) TO DAT-RATE (SUB-46)
           ADD 1 TO SUB-45 SUB-46.
           IF AL-RATE (SUB-45) NOT = " "
               GO TO WOPFN-010.
           MOVE DATA-RATE TO WS-PRINTER-PAGE1.
           
      *     WS-MESSAGE
      *     PERFORM ERROR1-000
      *     MOVE "PRN-PAGE1 FILE NAME IS ABOVE" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     PERFORM ERROR1-020
           
           MOVE SPACES           TO ALPHA-RATE DATA-RATE
           MOVE WS-PRINTER-PAGE2 TO ALPHA-RATE.
           MOVE 13 TO SUB-45
           MOVE 1  TO SUB-46.
       WOPFN-015.
           MOVE AL-RATE (SUB-45) TO DAT-RATE (SUB-46)
           ADD 1 TO SUB-45 SUB-46.
           IF AL-RATE (SUB-45) NOT = " "
               GO TO WOPFN-015.
           MOVE DATA-RATE        TO WS-PRINTER-PAGE2.
         
      *     WS-MESSAGE  
      *     PERFORM ERROR1-000
      *     MOVE "PRN-PAGE2 FILE NAME IS ABOVE" TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     PERFORM ERROR1-020.
           
           MOVE SPACES           TO ALPHA-RATE DATA-RATE.
       WOPFN-999.
           EXIT.
      *
       SUBTOTALS SECTION.
       SUB-000.
           WRITE PRINT-REC FROM P-UNDERLINE AFTER 1
           ADD 1 TO LINE-CNT
           MOVE "TOTAL DUE:"     TO TOT-DESC
           MOVE WS-TOT-BALANCE   TO TOT-BALANCE
           MOVE WS-TOT-CURRENT   TO TOT-CURRENT
           MOVE WS-TOT-30DAY     TO TOT-30DAY
           MOVE WS-TOT-60DAY     TO TOT-60DAY
           MOVE WS-TOT-90DAY     TO TOT-90DAY
           MOVE WS-TOT-120DAY    TO TOT-120DAY
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.

           IF WS-VALID-LINE-PRINTED = "N"
               MOVE 
           "NO VALID TRANSACTIONS FOR THIS ACCOUNT, LETTER NOT FAXED."
               TO PRINT-REC
               WRITE PRINT-REC AFTER 2
               GO TO SUB-500.

           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           WRITE PRINT-REC
           MOVE "AMOUNTS OLDER THAN 30 DAYS ARE OVER DUE" TO WS-COM1
           MOVE " FOR PAYMENT. PLEASE CONTACT OUR       " TO WS-COM2
           WRITE PRINT-REC FROM WS-COM-PRINT
           MOVE " " TO PRINT-REC WS-COM-PRINT.

           MOVE "DEBTORS DEPARTMENT IF YOU ARE NOT IN AG" TO WS-COM1
           MOVE "REEMENT WITH THE ABOVE AMOUNTS.        " TO WS-COM2
           WRITE PRINT-REC FROM WS-COM-PRINT
           MOVE " " TO PRINT-REC WS-COM-PRINT.

           MOVE "IF YOUR PAYMENT HAS CROSSED THIS LETTER" TO WS-COM1
           MOVE " IN THE POST, PLEASE IGNORE REMINDER.  " TO WS-COM2
           WRITE PRINT-REC FROM WS-COM-PRINT
           MOVE " " TO PRINT-REC WS-COM-PRINT.

           MOVE "ACCORDING TO OUR CREDIT TERMS, INTEREST" TO WS-COM1
           MOVE " MAY BE ADDED TO ALL OVERDUE AMOUNTS.  " TO WS-COM2
           WRITE PRINT-REC FROM WS-COM-PRINT
           MOVE " " TO PRINT-REC WS-COM-PRINT.
           IF WS-TOT-60DAY > 0
            AND WS-TOT-90DAY > 0
             OR WS-TOT-120DAY > 0
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC
              MOVE
              "**** PLEASE REFER TO OUR FAX / LETTER OF LAST MONTH ****"
               TO PRINT-REC
              WRITE PRINT-REC.

           IF DR-SUPPLY-Y-N = "N"
              MOVE " " TO PRINT-REC
              WRITE PRINT-REC
              MOVE "**** FOR THE ATTENTION OF THE ACCOUNTANT. ****"
               TO PRINT-REC
              WRITE PRINT-REC
              MOVE "     YOUR ACCOUNT HAS BEEN PLACED ON HOLD."
              TO PRINT-REC
              WRITE PRINT-REC.

           MOVE " " TO TOTAL-LINE PRINT-REC
           WRITE PRINT-REC FROM DR-CLERK-LINE AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC AFTER 1.
           
           IF WS-BY-DATE = "I"
             MOVE WS-SPEC-MESSAGE TO PRINT-REC
             WRITE PRINT-REC AFTER 1.
           
           IF WS-BY-DATE = "D"
              MOVE "TRANSACTIONS BASED ON DELIVERY DATE." TO PRINT-REC
           ELSE
              MOVE "TRANSACTIONS BASED ON INVOICE DATE. " TO PRINT-REC.
              
           WRITE PRINT-REC AFTER 1.
       SUB-500.
           IF WS-PRINTER NOT = "[QFax]"
                PERFORM GET-USER-MAIL-NAME
                PERFORM GET-REPORT-Y2K-DATE
                PERFORM PRINT-REPORT-INFO.
           
           MOVE 999 TO LINE-CNT
           MOVE 0 TO WS-TOT-BALANCE
                     WS-TOT-CURRENT
                     WS-TOT-30DAY
                     WS-TOT-60DAY
                     WS-TOT-90DAY
                     WS-TOT-120DAY.
       SUB-999.
           EXIT.
      *
       PRINT-HEADINGS SECTION.
       PH-000.
           MOVE 1 TO PAGE-CNT
                     LINE-CNT.
       PH-010.
           IF Fax-PaNumber = 3 OR = 4
            IF WS-AUTO-FAX = "P" OR = "Y" OR = "E"
               GO TO PH-020.
           Move Ws-Print-Bold        To Comp-Dig1
           Move Ws-Print-Unbold      To Comp-Dig2.
           MOVE PAGE-CNT      TO H-PAGE.
           WRITE PRINT-REC FROM HEAD1 AFTER 1
           MOVE " " TO PRINT-REC
           MOVE PA-NAME      TO CO-NAME
           WRITE PRINT-REC FROM COMPANY-LINE AFTER 2
           MOVE " " TO PRINT-REC COMPANY-LINE
           MOVE PA-ADD1      TO CO-NAME
           WRITE PRINT-REC FROM COMPANY-LINE AFTER 1
           MOVE " " TO PRINT-REC COMPANY-LINE
           MOVE PA-ADD2      TO CO-NAME
           WRITE PRINT-REC FROM COMPANY-LINE AFTER 1
           MOVE " " TO PRINT-REC COMPANY-LINE.
           MOVE "Fax     #:" TO H2-DESC
           MOVE PA-FAX       TO H-NUM
           MOVE PA-ADD3      TO H-ADD
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC HEAD2
           MOVE "Phone   #:" TO H2-DESC
           MOVE PA-PHONE     TO H-NUM
           MOVE PA-CODE      TO H-ADD
           WRITE PRINT-REC FROM HEAD2 AFTER 1.
           MOVE 13 TO LINE-CNT.
       PH-020.
           IF DR-ACC-EMAIL > " "
              MOVE DR-ACC-EMAIL                         TO WS-EMAIL
           ELSE
              MOVE "Statements@AccountingOptions.co.za" TO WS-EMAIL.

           MOVE " " TO PRINT-REC HEAD2.
           MOVE DR-ACC-EMAIL      TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE " "               TO PRINT-REC
           
           MOVE DR-ACCOUNT-NUMBER TO DEBT-ACCNO
           MOVE DR-NAME           TO DEBT-NAME
           MOVE DR-ACC-FAX        TO DEBT-FAX
           MOVE DR-DATE-LAST-PAY  TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE      TO DEBT-PAID-DATE
           MOVE DR-BALANCE        TO DEBT-BALANCE
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           WRITE PRINT-REC FROM HEAD3-1 AFTER 1
           WRITE PRINT-REC FROM HEAD3-2 AFTER 1
           WRITE PRINT-REC FROM HEAD5 AFTER 2
           MOVE " "               TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           MOVE 19 TO LINE-CNT.
       PH-999.
           EXIT.
      *
       ENTER-XQS-DETAILS SECTION.
       XQS-000.
           PERFORM GET-USER-MAIL-NAME.
           MOVE WS-pbValue TO WS-XQS-SNAME
                              WS-XQS-ENAME
                              WS-XQS-FROM-NAME WS-HYLA-FROM-NAME.
       XQS-001.
           MOVE " "              TO ALPHA-RATE WS-FAX-CHECK
           MOVE WS-XQS-BEG       TO ALPHA-RATE
           MOVE 10               TO SUB-1
           MOVE "THE ACCOUNTANT" TO WS-FAX-CHECK WS-HYLA-TO-NAME
           MOVE 40               TO SUB-2.
       XQS-002.
           IF WS-F-C (SUB-2) = " "
              SUBTRACT 1 FROM SUB-2
              GO TO XQS-002.
              MOVE SUB-2 TO SUB-4.
      *SUB-4 = LENGTH OF THE CONTACT-NAME.
           MOVE 1 TO SUB-2.
       XQS-003.
           IF SUB-2 > SUB-4
              GO TO XQS-004.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 41
              ADD 1 TO SUB-1 SUB-2
              GO TO XQS-003.
       XQS-004.
           MOVE "(" TO AL-RATE (SUB-1).
           ADD 1    TO SUB-1.
           MOVE " "           TO WS-FAX-CHECK.
      *****************************************************************
      * NEW SECTION FOR PUTTING FAXES IN THE QUEUE EVEN IF THE FAX    *
      * NUMBER HAS AN ERROR.  THIS WAY ALL FAXES WILL EITHER BE SENT  *
      * BY XFAX OR MANUALLY                                           *
      *****************************************************************
           IF SIGN-FOUND = 1
              MOVE SPACES        TO WS-FAX-CHECK
           ELSE
              MOVE WS-FAX-NUMBER TO WS-FAX-CHECK.
           MOVE 1 TO SUB-2.
       XQS-015.
           IF WS-F-C (SUB-2) = " "
              MOVE ")" TO AL-RATE (SUB-1)
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-016.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 41
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-015.
       XQS-016.
           MOVE " "          TO WS-FAX-CHECK
           MOVE WS-XQS-FROM  TO WS-FAX-CHECK
           MOVE 1            TO SUB-2.
       XQS-017.
           IF WS-F-C (SUB-2) = " "
              ADD 1 TO SUB-2
            IF WS-F-C (SUB-2) = " "
              SUBTRACT 1 FROM SUB-2
              GO TO XQS-018
            ELSE
              SUBTRACT 1 FROM SUB-2.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 31
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-017.
       XQS-018.
           MOVE " "        TO WS-FAX-CHECK
           MOVE WS-XQS-END TO WS-FAX-CHECK
           MOVE 1          TO SUB-2.
       XQS-019.
           IF WS-F-C (SUB-2) = " "
              GO TO XQS-021.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 25
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-019.
       XQS-021.
           MOVE ALPHA-RATE    TO WS-XQS-LINE (1).
           MOVE " "   TO WS-FAX-CHECK ALPHA-RATE
           MOVE "(:(" TO ALPHA-RATE
           MOVE 4     TO SUB-1
           MOVE 1     TO SUB-2.
       XQS-022.
           MOVE " "           TO WS-FAX-CHECK
           MOVE WS-CO-NUMBER  TO WS-XQS2 WS-XQS4
           MOVE WS-XQS-COVERS TO WS-FAX-CHECK
           MOVE 1             TO SUB-2.
       XQS-023.
           IF WS-F-C (SUB-2) = " "
              GO TO XQS-025.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 30
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-023.
       XQS-025.
           MOVE " "        TO WS-FAX-CHECK
           MOVE WS-XQS-END TO WS-FAX-CHECK
           MOVE 1          TO SUB-2.
       XQS-026.
           IF WS-F-C (SUB-2) = " "
              GO TO XQS-027.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 25
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-026.
       XQS-027.
           MOVE ALPHA-RATE          TO WS-XQS-LINE (2).
           MOVE " "                 TO WS-FAX-CHECK ALPHA-RATE
           MOVE WS-REFNO-CHECK      TO WS-XQS-COMMENT
           MOVE " "                 TO WS-FAX-CHECK
           MOVE WS-XQS-COMMENT      TO WS-FAX-CHECK WS-HYLA-COMMENT
           MOVE 1                   TO SUB-2.
           MOVE 1                   TO SUB-1.
       XQS-028.
           IF WS-F-C (SUB-2) NOT = "0"
              GO TO XQS-029.
           MOVE " " TO WS-F-C (SUB-2).
           IF SUB-2 < 60
              ADD 1 TO SUB-2
              GO TO XQS-028.
       XQS-029.
           IF WS-F-C (SUB-2) = " "
              MOVE ALPHA-RATE          TO WS-XQS-COMMENT
              MOVE " "                 TO WS-FAX-CHECK ALPHA-RATE
              MOVE WS-XQS-COMMENT-LINE TO WS-FAX-CHECK ALPHA-RATE
              MOVE 36                  TO SUB-1
              GO TO XQS-032.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 40
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-029.
       XQS-032.
           IF AL-RATE (SUB-1) = " "
              GO TO XQS-033.
           IF SUB-1 < 60
              ADD 1 TO SUB-1
              GO TO XQS-032.
       XQS-033.
           MOVE " "        TO WS-FAX-CHECK
           MOVE WS-XQS-END TO WS-FAX-CHECK
           MOVE 1          TO SUB-2.
       XQS-034.
           IF WS-F-C (SUB-2) = " "
              GO TO XQS-0341.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 20
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-034.
       XQS-0341.
           MOVE ALPHA-RATE          TO WS-XQS-LINE (3).
           MOVE " "   TO WS-FAX-CHECK ALPHA-RATE
           MOVE "(:(" TO ALPHA-RATE
           MOVE 4     TO SUB-1
           MOVE 1     TO SUB-2.
           MOVE WS-XQS-USERNAME TO WS-FAX-CHECK
           MOVE 1               TO SUB-2.
       XQS-035.
           MOVE " "             TO WS-FAX-CHECK
           MOVE WS-XQS-PRIORITY TO WS-FAX-CHECK
           MOVE 1               TO SUB-2.
       XQS-037.
           IF WS-F-C (SUB-2) = " "
              ADD 1 TO SUB-2
            IF WS-F-C (SUB-2) = " "
              SUBTRACT 1 FROM SUB-2
              GO TO XQS-038
            ELSE
              SUBTRACT 1 FROM SUB-2.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 40
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-037.
       XQS-038.
           MOVE " "        TO WS-FAX-CHECK
           MOVE WS-XQS-END TO WS-FAX-CHECK
           MOVE 1          TO SUB-2.
       XQS-039.
           IF WS-F-C (SUB-2) = " "
              GO TO XQS-041.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 25
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-039.
       XQS-041.
           MOVE ALPHA-RATE          TO WS-XQS-LINE (4).
           MOVE " "   TO WS-FAX-CHECK ALPHA-RATE
           MOVE "(:(" TO ALPHA-RATE
           MOVE 4     TO SUB-1
           MOVE 1     TO SUB-2.
           MOVE WS-XQS-USERNAME TO WS-FAX-CHECK
           MOVE 1               TO SUB-2.
       XQS-042.
           IF WS-F-C (SUB-2) = " "
              ADD 1 TO SUB-2
            IF WS-F-C (SUB-2) = " "
              SUBTRACT 1 FROM SUB-2
              GO TO XQS-043
            ELSE
              SUBTRACT 1 FROM SUB-2.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 60
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-042.
       XQS-043.
           IF WS-F-C (SUB-2) = " "
              ADD 1 TO SUB-2
              GO TO XQS-043.
       XQS-044.
           IF WS-F-C (SUB-2) = " "
              ADD 1 TO SUB-2
            IF WS-F-C (SUB-2) = " "
              SUBTRACT 1 FROM SUB-2
              GO TO XQS-116
            ELSE
              SUBTRACT 1 FROM SUB-2.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 60
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-044.
       XQS-116.
           MOVE " "        TO WS-FAX-CHECK
           MOVE WS-XQS-END TO WS-FAX-CHECK
           MOVE 1          TO SUB-2.
       XQS-117.
           IF WS-F-C (SUB-2) = " "
              GO TO XQS-999.
           MOVE WS-F-C (SUB-2) TO AL-RATE (SUB-1).
           IF SUB-2 < 25
              ADD 1 TO SUB-1
                       SUB-2
              GO TO XQS-117.
       XQS-999.
           MOVE ALPHA-RATE TO WS-XQS-LINE (5).
       XQS-9999.
           EXIT.
      *
       REMOVE-ZERO-IN-REFERENCE SECTION.
       RZIR-010.
           MOVE SPACES       TO WS-REFNO-CHECK
                                ALPHA-RATE
           MOVE WS-REFERENCE TO WS-REFNO-CHECK
           MOVE 1            TO SUB-1 SUB-2.
       RZIR-015.
           IF WS-RF-C (SUB-1) = "0"
            IF SUB-1 < 25
              ADD 1 TO SUB-1
              GO TO RZIR-015.
       RZIR-016.
           MOVE WS-RF-C (SUB-1) TO AL-RATE (SUB-2).
            IF SUB-1 < 25
              ADD 1 TO SUB-1 SUB-2
              GO TO RZIR-016.
           MOVE " "        TO WS-REFNO-CHECK
           MOVE ALPHA-RATE TO WS-REFNO-CHECK.
       RZIR-999.
           EXIT.
      *
       CHECK-REFERENCE SECTION.
       CREF-010.
           MOVE SPACES       TO WS-REFNO-CHECK
           MOVE WS-REFERENCE TO WS-REFNO-CHECK
           MOVE 1            TO SUB-1.
       CREF-015.
           IF WS-RF-C (SUB-1) = "."
              MOVE "-" TO WS-RF-C (SUB-1).
           IF WS-RF-C (SUB-1) = " "
              GO TO CREF-016.
           IF SUB-1 < 25
              ADD 1 TO SUB-1
              GO TO CREF-015.
       CREF-016.
           MOVE SUB-1   TO SUB-5
           SUBTRACT 1 FROM SUB-1
           ADD 13       TO SUB-1
           MOVE WS-SP-PRINT TO FAX-ASCIIFILENAME
           MOVE SUB-1       TO FAX-CBASCIIFILENAME
           ADD 1      TO SUB-1
           MOVE SUB-1 TO FAX-CBFAXFILENAME
           MOVE SUB-5 TO SUB-1.
       CREF-017.
           MOVE "." TO WS-RF-C (SUB-1)
           ADD 1 TO SUB-1
           MOVE "F" TO WS-RF-C (SUB-1)
           ADD 1 TO SUB-1
           MOVE "a" TO WS-RF-C (SUB-1)
           ADD 1 TO SUB-1
           MOVE "x" TO WS-RF-C (SUB-1)
           MOVE WS-REFNO-CHECK TO WS-FAX-REST
           MOVE WS-FAX-SEND    TO FAX-FAXFILENAME
           MOVE 1 TO SUB-1.
       CREF-999.
           EXIT.
      *
       CHECK-FAX-NUMBER SECTION.
       FNC-010.
           MOVE 0          TO SIGN-FOUND.
           MOVE DR-ACC-FAX TO WS-FAX-CHECK.
           MOVE 1 TO SUB-1.
           IF DR-ACC-FAX = " "
               MOVE 1 TO SIGN-FOUND
               MOVE " " TO FAX-PHONENUMBER WS-FAX-NUMBER
               GO TO FNC-999.
       FNC-015.
           IF WS-F-C (SUB-1) = "." OR = "-"
              MOVE "," TO WS-F-C (SUB-1).
           COMPUTE SUB-2 = SUB-1 + 1.
           IF WS-F-C (SUB-1) = " "
            IF WS-F-C (SUB-2) = " "
              GO TO FNC-016
            ELSE
              MOVE "," TO WS-F-C (SUB-1).
           IF WS-F-C (SUB-1) = "/"
               GO TO FNC-020.
           IF WS-F-C (SUB-1) NOT = " " AND NOT = "0" AND NOT = "1"
           AND NOT = "2" AND NOT = "3" AND NOT = "4" AND NOT = "5"
           AND NOT = "6" AND NOT = "7" AND NOT = "8" AND NOT = "9"
                         AND NOT = ","
                 MOVE 1 TO SIGN-FOUND.
      *           GO TO FNC-999.
           IF WS-F-C (SUB-1) = " "
              GO TO FNC-016.
           IF SUB-1 < 25
              ADD 1 TO SUB-1
              GO TO FNC-015.
       FNC-016.
           MOVE WS-FAX-CHECK TO FAX-PHONENUMBER WS-FAX-NUMBER.
           SUBTRACT 1 FROM SUB-1.
           MOVE SUB-1        TO FAX-CBPHONENUMBER.
           GO TO FNC-999.
       FNC-020.
           MOVE SUB-1 TO SUB-5.
       FNC-021.
           MOVE " " TO WS-F-C (SUB-1).
           IF SUB-1 < 25
              ADD 1 TO SUB-1
              GO TO FNC-021
           ELSE
              MOVE SUB-5 TO SUB-1
              GO TO FNC-016.
       FNC-999.
           EXIT.
      *
       READ-INVQUES-FILE SECTION.
       RINVQUES-000.
            MOVE 1 TO PA-RECORD.
            MOVE 6 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RINVQUES-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE ALL "X" TO WS-QUES-ACC-CONTACT
                               WS-QUES-ACC-PHONE
               GO TO RINVQUES-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "Parameter Busy RINVQUES, Press 'ESC' To Retry."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RINVQUES-010.
       RINVQUES-900.
            MOVE INVQUES-ACC-CONTACT TO WS-QUES-ACC-CONTACT
            MOVE INVQUES-ACC-PHONE   TO WS-QUES-ACC-PHONE.
       RINVQUES-999.
            EXIT.
      *
       END-OFF SECTION.
       END-800.
           CLOSE DEBTOR-TRANS-FILE
                 DEBTOR-MASTER
                 Fax-Parameter.
       END-900.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       OPEN-DATA-FILES SECTION.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE WS-SAVE-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H-DATE
           MOVE WS-MM TO H-REF-MM
           MOVE WS-YY TO H-REF-YY.
       OPEN-020.
           OPEN I-O DEBTOR-MASTER.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DR-MASTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-020.
       OPEN-030.
           OPEN I-O DEBTOR-TRANS-FILE.
           IF WS-DRTRANS-ST1 NOT = 0
               MOVE 0 TO WS-DRTRANS-ST1
               MOVE "DR-TRANS FILE BUSY ON OPEN, 'ESC' TO RETRY."
                   TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-030.
       OPEN-040.
           OPEN I-O PARAMETER-FILE.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-040.
           PERFORM READ-INVQUES-FILE.
       OPEN-050.
           MOVE 0 TO PA-TYPE
                     PA-RECORD.
           START PARAMETER-FILE KEY NOT < PA-KEY.
           READ PARAMETER-FILE NEXT
              AT END NEXT SENTENCE.
           IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO OPEN-050.
           CLOSE PARAMETER-FILE.
       OPEN-055.
           OPEN I-O FAX-PARAMETER.
           IF WS-FAX-ST1 NOT = 0
               MOVE "FAXPARAMETER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-055.
       OPEN-056.
           MOVE 1 TO FAX-PAKEY.
           START FAX-PARAMETER KEY NOT < FAX-PAKEY
              INVALID KEY NEXT SENTENCE.
           IF WS-FAX-ST1 NOT = 0
              MOVE
              "THERE IS NO VALID FAXPARAMETER ON START, 'ESC' TO EXIT."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-FAX-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              EXIT PROGRAM.
       OPEN-058.
           READ FAX-PARAMETER
              INVALID KEY NEXT SENTENCE.
           IF WS-FAX-ST1 NOT = 0
              MOVE "FAXPARAMETER-058 BUSY ON READ, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-FAX-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              GO TO OPEN-058.
           CLOSE FAX-PARAMETER.
       OPEN-060.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "DrDuFxRp"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       Copy "ReadFieldNumeric".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "PrepareFaxSending".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
       Copy "SetupDrOverdueForPDF".
       Copy "SetupDrOverdue2ForPDF".
       Copy "SetupMergeDrOverdueForPDF".
       Copy "SetupDrOverdueForPDFMgEMail".
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
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
