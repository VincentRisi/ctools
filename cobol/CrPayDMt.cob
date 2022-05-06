       IDENTIFICATION DIVISION.
       PROGRAM-ID. CrPayDMt.
       AUTHOR.  CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. B20.
       OBJECT-COMPUTER. B20.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           Copy "SelectCrMaster".
           Copy "SelectCrJrn".
           Copy "SelectCrTrans".
           Copy "SelectSlDaily".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdCrJrn.
           COPY ChlfdCrTrans.
           COPY ChlfdCreditor.
           COPY ChlfdDaily.
      *
       WORKING-STORAGE SECTION.
       77  WS-NEWBATCH          PIC X VALUE " ".
       77  WS-TYPE-OF-READ      PIC X VALUE "I".
       77  WS-NEWINVOICE        PIC X VALUE " ".
       77  WS-LINE-CHANGED      PIC X VALUE " ".
       77  WS-CURRENTPER        PIC 99 VALUE 0.
       77  WS-CREDITOR          PIC 9(7).
       77  WS-CURRENTGLPER      PIC 99 VALUE 0.
       77  WS-TYPE-OF-END       PIC X VALUE " ".
       77  WS-ABOVE-BODY        PIC X VALUE " ".
       77  WS-PAGE              PIC 99 VALUE 0.
       01  WS-CREDITOR-STATUS.
           03  WS-CREDITOR-ST1 PIC 99.
       01  WS-CRJRN-STATUS.
           03  WS-CRJRN-ST1    PIC 99.
       01  WS-CRTRANS-STATUS.
           03  WS-CRTRANS-ST1  PIC 99.
       01  WS-DAILY-STATUS.
           03  WS-DAILY-ST1    PIC 99.
       01  JOURNAL-DATA.
           03  WS-INV-AMT           PIC S9(7)V99.
           03  WS-UNAPPLIED         PIC S9(7)V99.
           03  WS-SETT-DISC         PIC S9(7)V99.
           03  WS-INV-NO            PIC X(10).
           03  WS-DNOTE-NO          PIC X(10).
       01  WS-DAILY-MESSAGE.
           03  WS-DAILY-1ST        PIC X(20) VALUE " ".
           03  WS-DAILY-2ND        PIC X(20) VALUE " ".
           03  WS-DAILY-3RD        PIC X(20) VALUE " ".
           03  WS-DAILY-4TH        PIC X(20) VALUE " ".
       01  WS-GLNO-CHECK.
           03  WS-CH               PIC X OCCURS 15.
       Copy "WsDateInfo".
      *
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-001.
           PERFORM CLEAR-FIELDS.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-001.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-010.
            GO TO GET-070.
       GET-065.
            MOVE "DNOTE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE CRJRN-DNOTE-NO TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CRJRN-INV-NO TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUPPLIER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 7 TO F-CBFIELDLENGTH.
            MOVE CRJRN-CRACC-NUMBER TO F-NAMEFIELD WS-CREDITOR. 
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM READ-CREDITOR.
            MOVE "SUPPLIERNAME" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE CR-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "LOCALAMT" TO F-FIELDNAME
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CRJRN-LOC-AMT TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "SETTLEDISC" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE CRJRN-SETT-DISC TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "INVDATE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CRJRN-INV-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DUEDATE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE CRJRN-DUE-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-070.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-FIELDS
                PERFORM CLEAR-BODY
                PERFORM DISPLAY-FORM
                GO TO GET-010.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-INV-NO.
            IF WS-MESSAGE NOT = " "
               PERFORM ERROR-020.
       GET-080.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "SUPPLIER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-070.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-CREDITOR.
            PERFORM READ-CREDITOR.

            MOVE "SUPPLIERNAME" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE CR-NAME TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            IF CR-NAME = "** UNKNOWN **"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-080.
       GET-085.
            PERFORM READ-CRJRN-INVOICES.
            IF WS-NEWINVOICE = "N"
                PERFORM GET-065
                PERFORM SCROLL-NEXT
            ELSE
                MOVE "INVOICE CAN'T BE FOUND IN THE SYSTEM, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-070.
       GET-100.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "SETTLEDISC" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-080.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRJRN-SETT-DISC F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
       GET-160.
            MOVE "                                    " TO F-NAMEFIELD.
            MOVE "DUEDATE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-100.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO GET-160.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO CRJRN-DUE-DATE
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO GET-160.
       GET-190.
            MOVE 1 TO SUB-1 F-INDEX.
            PERFORM SCROLL-NEXT.
            PERFORM FILL-BODY.
            IF WS-ABOVE-BODY = "1"
                GO TO GET-160.
       GET-195.
           PERFORM REWRITE-CRJRN.
           PERFORM REWRITE-CRTRANS.
       GET-999.
            EXIT.
      *
       FILL-BODY SECTION.
       FILL-000.
           MOVE " " TO WS-ABOVE-BODY.
           MOVE 1 TO SUB-1 SUB-2 SUB-3.
           MOVE "N" TO WS-LINE-CHANGED.
       FILL-005.
           PERFORM ERROR-020.
       FILL-050.
           MOVE "                                " TO F-NAMEFIELD.
           MOVE "GLSETDISC" TO F-FIELDNAME.
           MOVE 9 TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           MOVE 11 TO F-CBFIELDLENGTH.
           IF F-EXIT-CH = X"0B" OR = X"0A"
            IF CRJRN-GLACC (SUB-1) = " "
              GO TO FILL-050.

           IF F-EXIT-CH = X"01" AND F-INDEX = 1
               MOVE "1" TO WS-ABOVE-BODY
               GO TO FILL-999.

           IF F-EXIT-CH = X"01" AND F-INDEX > 1
               SUBTRACT 1 FROM F-INDEX SUB-1
               GO TO FILL-050.

           IF F-EXIT-CH = X"0B" AND F-INDEX < 10
               ADD 1 TO F-INDEX SUB-1
             IF CRJRN-GLDISC (SUB-1) = 0
               SUBTRACT 1 FROM SUB-1 F-INDEX
               GO TO FILL-050
             ELSE
               GO TO FILL-050.
           IF F-EXIT-CH = X"0B" AND F-INDEX = 10
               GO TO FILL-050.
      * TAB CHARACTER
           IF F-EXIT-CH = X"09"
                PERFORM READ-FIELD-ALPHA
                MOVE F-NAMEFIELD TO ALPHA-RATE
                PERFORM DECIMALISE-RATE
                MOVE NUMERIC-RATE TO CRJRN-GLDISC (SUB-1)
                MOVE CRJRN-GLDISC (SUB-1) TO F-EDNAMEFIELDNUM6
                MOVE 11 TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-NUMERIC6
                PERFORM FILL-900 THRU FILL-910
                MOVE " " TO WS-ABOVE-BODY
                PERFORM ERROR-020
                GO TO FILL-990.
           IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B"
                GO TO FILL-050.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO CRJRN-GLDISC (SUB-1).
            MOVE CRJRN-GLDISC (SUB-1) TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.
       FILL-090.
           IF F-INDEX < 10
               ADD 1 TO SUB-1 F-INDEX.
           MOVE "N" TO WS-LINE-CHANGED.
           IF CRJRN-GLACC (SUB-1) = " "
               SUBTRACT 1 FROM SUB-1 F-INDEX.
           GO TO FILL-050.
       FILL-900.
           MOVE 1 TO SUB-1.
           MOVE 0 TO WS-SETT-DISC.
       FILL-910.
           IF SUB-1 < 11
            IF CRJRN-GLACC (SUB-1) NOT = " "
                ADD CRJRN-GLDISC (SUB-1) TO WS-SETT-DISC
                ADD 1 TO SUB-1
                GO TO FILL-910.
           MOVE F-INDEX TO SUB-1.
       FILL-990.
            IF WS-SETT-DISC NOT = CRJRN-SETT-DISC
                MOVE
                "THIS ALLOCATED DISC DOES NOT = THE ORIGINAL DISCOUNT."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-050.
       FILL-999.
           EXIT.
      *
       CLEAR-HEAD-DETAILS SECTION.
       CHD-005.
            MOVE "DNOTE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUPPLIER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 7 TO F-CBFIELDLENGTH.
            MOVE " " TO F-NAMEFIELD. 
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUPPLIERNAME" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "LOCALAMT" TO F-FIELDNAME
            MOVE 8 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SETTLEDISC" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVDATE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DUEDATE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       CHD-010.
            MOVE " " TO WS-INV-NO
                        WS-DNOTE-NO.
            MOVE 0 TO WS-SETT-DISC.
       CHD-999.
           EXIT.
      *
       READ-CREDITOR SECTION.
       RCR-000.
           MOVE WS-CREDITOR TO CR-ACCOUNT-NUMBER.
           START CREDITOR-MASTER KEY NOT < CR-KEY.
       RCR-010.
           READ CREDITOR-MASTER
                INVALID KEY NEXT SENTENCE.
           IF WS-CREDITOR-ST1 = 23 OR 35 OR 49
                MOVE "** UNKNOWN **" TO CR-NAME
                MOVE 0 TO WS-CREDITOR-ST1
                GO TO RCR-999.
           IF WS-CREDITOR-ST1 NOT = 0
                MOVE 0 TO WS-CREDITOR-ST1
                MOVE "CREDITOR RECORD BUSY, 'ESC' TO RETRY."
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RCR-010.
       RCR-999.
             EXIT.
      *
       READ-CRJRN-INVOICES SECTION.
       RCRI-000.
           MOVE "Y" TO WS-NEWINVOICE.
       RCRI-005.
           MOVE WS-INV-NO TO CRJRN-INV-NO.
           START CRJRN-FILE KEY NOT < CRJRN-INV-NO
              INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-CRJRN-ST1
              GO TO RCRI-299.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CRJRN BUSY ON START, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRJRN-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CRJRN-ST1
              CLOSE CRJRN-FILE
              PERFORM OPEN-015
              GO TO RCRI-005.
       RCRI-010.
           READ CRJRN-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-CRJRN-ST1 = 10
              GO TO RCRI-299.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CR-JRN BUSY ON READ-NEXT-LOCK, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RCRI-010.
           IF CRJRN-INV-NO = WS-INV-NO
            IF CRJRN-CRACC-NUMBER NOT = WS-CREDITOR
              GO TO RCRI-010.
           IF CRJRN-INV-NO NOT = WS-INV-NO
              GO TO RCRI-299.
           IF CRJRN-COMPLETE = "Y"
              MOVE 88 TO WS-CRJRN-ST1
              MOVE "THIS BATCH HAS BEEN POSTED, 'ESC' TO RE-ENTER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RCRI-299.
       RCRI-050.
           IF WS-NEWINVOICE = "Y"
              MOVE "N" TO WS-NEWINVOICE.
           PERFORM READ-CRTRANS.
           GO TO RCRI-999.
       RCRI-299.
           PERFORM CF-000 THRU CF-010.
       RCRI-999.
           EXIT.
      *
       READ-CRTRANS SECTION.
       RTRANS-000.
           MOVE "Y" TO WS-NEWINVOICE.
       RTRANS-005.
           MOVE WS-INV-NO TO CRTR-INV-NO.
           START CRTR-FILE KEY NOT < CRTR-INV-NO
              INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 23 OR 35 OR 49
              MOVE 0 TO WS-CRTRANS-ST1
              GO TO RTRANS-999.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE "CR-TRANS BUSY ON START, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CRTRANS-ST1
              CLOSE CRTR-FILE
              PERFORM OPEN-013
              GO TO RTRANS-005.
       RTRANS-010.
           READ CRTR-FILE NEXT WITH LOCK
              AT END NEXT SENTENCE.
           IF WS-CRTRANS-ST1 = 10
              GO TO RTRANS-999.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE "CR-TRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-CRTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-000
              CALL "C$SLEEP" USING 1
              PERFORM ERROR1-020
              PERFORM ERROR-020
              MOVE 0 TO WS-CRTRANS-ST1
              GO TO RTRANS-010.
           IF CRTR-INV-NO = WS-INV-NO
            IF CRTR-ACC-NUMBER NOT = WS-CREDITOR
              GO TO RTRANS-010.
           IF CRTR-INV-NO NOT = WS-INV-NO
              MOVE "Y" TO WS-NEWINVOICE
              GO TO RTRANS-999.
       RTRANS-050.
           MOVE "N" TO WS-NEWINVOICE.
       RTRANS-999.
           EXIT.
      *
       REWRITE-CRJRN SECTION.
       RWCR-018.
           REWRITE CRJRN-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-CRJRN-ST1 NOT = 0
              MOVE "CRJRN REWRITE ERR, RWCR-018. TRANS NOT RE-WRITTEN"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-CRJRN-ST1
              GO TO RWCR-018.
       RWCR-999.
           EXIT.
      *
       REWRITE-CRTRANS SECTION.
       WTRANS-010.
           MOVE CRJRN-SETT-DISC      TO CRTR-SETT-DISC.
           MOVE CRJRN-DUE-DATE       TO CRTR-DUE-DATE.
       WTRANS-018.
           REWRITE CRTR-REC
              INVALID KEY NEXT SENTENCE.
           IF WS-CRTRANS-ST1 NOT = 0
              MOVE "CRTR REWRITE ERR, WTRANS-018. TRANS NOT RE-WRITTEN"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              MOVE 0 TO WS-CRTRANS-ST1
              GO TO WTRANS-018.
       WTRANS-999.
           EXIT.
      *
       SCROLL-NEXT SECTION.
       NEXT-000.
            MOVE 1 TO SUB-1 F-INDEX. 
            PERFORM CLEAR-BODY.
            MOVE 1 TO F-INDEX. 
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF SUB-1 > 10
                GO TO NEXT-030.
            IF F-INDEX < 10
                GO TO NEXT-010.
       NEXT-030.
            MOVE 1 TO SUB-1 F-INDEX. 
       NEXT-999.
             EXIT.
      *
       SCROLLING SECTION.
       SCROLL-010.
            MOVE "GLNUMBER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE CRJRN-GLACC (SUB-1) TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-015.
            MOVE "GLAMOUNT" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            IF CRJRN-GLACC (SUB-1) = " "
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-020.
            MOVE CRJRN-GLAMT (SUB-1) TO F-EDNAMEFIELDNUM6.
            PERFORM WRITE-FIELD-NUMERIC6.
       SCROLL-016.
            MOVE "GLSETDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            IF CRJRN-GLACC (SUB-1) = " "
                MOVE " " TO F-NAMEFIELD
                PERFORM WRITE-FIELD-ALPHA
                GO TO SCROLL-020.
            MOVE CRJRN-GLDISC (SUB-1) TO F-EDNAMEFIELDNUM6.
            PERFORM WRITE-FIELD-NUMERIC6.
       SCROLL-020.
            MOVE "GLNAME" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE CRJRN-GLDESC (SUB-1) TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-999.
             EXIT.
      *
       CLEAR-BODY SECTION.
       CLEAR-000.
            MOVE 0 TO F-INDEX.
       CLEAR-002.
            ADD 1 TO F-INDEX.
            IF F-INDEX > 10
               GO TO CLEAR-BODY-999.

            MOVE "GLNUMBER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLAMOUNT" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            MOVE " " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLSETDISC" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE 11 TO F-CBFIELDLENGTH.
            MOVE " " TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GLNAME" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            GO TO CLEAR-002.
       CLEAR-BODY-999.
             EXIT.
      *
       CLEAR-FIELDS SECTION.
       CF-000.
             MOVE 1 TO SUB-1.
       CF-010.
             MOVE " " TO CRJRN-GLACC (SUB-1)
                         CRJRN-GLDESC (SUB-1).
             MOVE 0   TO CRJRN-GLAMT (SUB-1)
                         CRJRN-GLDISC (SUB-1).
             ADD 1 TO SUB-1.
             IF SUB-1 < 10
                 GO TO CF-010.
       CF-020.
             MOVE " " TO CRJRN-REFERENCE
                         CRJRN-PERIOD
                         CRJRN-INV-NO
                         CRJRN-DNOTE-NO
                         CRJRN-COMPLETE.
             MOVE 0 TO   CRJRN-TRANS
                         CRJRN-NO
                         CRJRN-INV-DATE
                         CRJRN-DUE-DATE
                         CRJRN-CRACC-NUMBER
                         CRJRN-LOC-AMT
                         CRJRN-FOR-AMT
                         CRJRN-SETT-DISC.
       CF-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O CREDITOR-MASTER.
            IF WS-CREDITOR-ST1 NOT = 0
               MOVE 0 TO WS-CREDITOR-ST1
               MOVE "CREDITOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-013.
           OPEN I-O CRTR-FILE.
           IF WS-CRTRANS-ST1 NOT = 0 
              MOVE 0 TO WS-CRTRANS-ST1
              MOVE "CRTRANS FILE BUSYON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-013.
       OPEN-015.
           OPEN I-O CRJRN-FILE.
           IF WS-CRJRN-ST1 NOT = 0 
              MOVE 0 TO WS-CRJRN-ST1
              MOVE "CRJRN FILE BUSYON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-015.
       OPEN-020.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "CrPayDMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE CREDITOR-MASTER
                 CRJRN-FILE
                 CRTR-FILE.
           EXIT PROGRAM.
      *     STOP RUN.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAccount".
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAccount".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldLine".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldNumeric6".
      *
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
      * END-OF-JOB
