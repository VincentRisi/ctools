        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlDistIq.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectSlDistributions".
      *
       DATA DIVISION.
       FILE SECTION.
          COPY ChlfdDisTot.

       WORKING-STORAGE SECTION.
       77  NEW-NO             PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-NUMBER          PIC 9 VALUE 0.
       77  WS-SAVE            PIC 9 VALUE 0.
       01  WS-DISTRIBUTION-STATUS.
           03  WS-DISTRIBUTION-ST1   PIC 99.
       Copy "WsDateInfo".
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
       CONTROL-010.
           PERFORM DISPLAY-FORM
           PERFORM GET-DATA
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE 0 TO DIST-REC.
            MOVE "N" TO NEW-NO
                        WS-END.
            MOVE 1 TO DIST-KEY.
            MOVE "KEY" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE DIST-KEY TO F-EDNAMEFIELDIND1.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INDEX1.
       GET-001.
            MOVE "KEY" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-NUMERIC.
            MOVE F-NAMEFIELDNUM TO DIST-KEY.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 DISPLAY " " AT 3079 WITH BELL
                 GO TO GET-000.
            PERFORM READ-RECORD.
            IF NEW-NO = "Y"
               GO TO GET-999.
            GO TO GET-005.
        GET-003.
            MOVE "KEY" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE DIST-KEY TO F-EDNAMEFIELDIND1.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INDEX1.
        GET-005.
            MOVE "INVOICEWEEK" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE DIST-INVOICEWEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "INVOICEPTD" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DIST-INVOICEPTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "INVOICEYTD" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DIST-INVOICEYTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "PAYMENTWEEK" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE DIST-PAYMENTWEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "PAYMENTPTD" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DIST-PAYMENTPTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "PAYMENTYTD" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DIST-PAYMENTYTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "RDCHEQUEWEEK" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE DIST-RDCHEQUEWEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "RDCHEQUEPTD" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE DIST-RDCHEQUEPTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "RDCHEQUEYTD" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE DIST-RDCHEQUEYTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "JRNDRWEEK" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DIST-JOURNALDRWEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "JRNDRPTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DIST-JOURNALDRPTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "JRNDRYTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DIST-JOURNALDRYTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "JRNCRWEEK" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DIST-JOURNALCRWEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "JRNCRPTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DIST-JOURNALCRPTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "JRNCRYTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DIST-JOURNALCRYTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "CNOTEWEEK" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DIST-CNOTEWEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "CNOTEPTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DIST-CNOTEPTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "CNOTEYTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DIST-CNOTEYTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "INTERESTWEEK" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE DIST-INTERESTWEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "INTERESTPTD" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE DIST-INTERESTPTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "INTERESTYTD" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE DIST-INTERESTYTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "DISCOUNTWEEK" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE DIST-DISCOUNTWEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "DISCOUNTPTD" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE DIST-DISCOUNTPTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "DISCOUNTYTD" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE DIST-DISCOUNTYTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "ADDONWEEK" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DIST-ADDONWEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "ADDONPTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DIST-ADDONPTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "ADDONYTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DIST-ADDONYTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "BDEBTWEEK" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DIST-BDEBTWEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "BDEBTPTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DIST-BDEBTPTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "BDEBTYTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DIST-BDEBTYTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "ACCRECWEEK" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DIST-ACCRECWEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "ACCRECPTD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DIST-ACCRECPTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "ACCRECYTD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DIST-ACCRECYTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "GSTWEEK" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE GST-AMT-TAXED-WEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "GSTPTD" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE GST-AMT-TAXED-PTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "GSTYTD" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE GST-AMT-TAXED-YTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "TAXABLEWEEK" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE GST-AMT-TAXABLE-WEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "TAXABLEPTD" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE GST-AMT-TAXABLE-PTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "TAXABLEYTD" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE GST-AMT-TAXABLE-YTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "NONTAXABLEWEEK" TO F-FIELDNAME.
            MOVE 14 TO F-CBFIELDNAME.
            MOVE GST-AMT-NONTAXABLE-WEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "NONTAXABLEPTD" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE GST-AMT-NONTAXABLE-PTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "NONTAXABLEYTD" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE GST-AMT-NONTAXABLE-YTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "EXPORTWEEK" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE GST-AMT-EXPORT-WEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "EXPORTPTD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE GST-AMT-EXPORT-PTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "EXPORTYTD" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE GST-AMT-EXPORT-YTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.
            
            GO TO GET-001.
       GET-999.
            EXIT.
      *
       READ-RECORD SECTION.
       RD-000.
           MOVE 0 TO WS-DISTRIBUTION-ST1.
           MOVE DIST-KEY TO WS-NUMBER.
           START DISTRIBUTIONS KEY NOT < DIST-KEY.
       RD-010.
           READ DISTRIBUTIONS
                 INVALID KEY NEXT SENTENCE.
           IF WS-DISTRIBUTION-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-DISTRIBUTION-ST1
                MOVE "Y" TO NEW-NO
                MOVE WS-NUMBER TO DIST-KEY
                GO TO RD-999.
           IF WS-DISTRIBUTION-ST1 NOT = 0
                MOVE "DISTRIBUTIONS BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DISTRIBUTION-ST1
                GO TO RD-010.
           MOVE "N" TO NEW-NO.
           MOVE DIST-KEY TO WS-SAVE.
       RD-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DISTRIBUTIONS.
            IF WS-DISTRIBUTION-ST1 NOT = 0
               MOVE 0 TO WS-DISTRIBUTION-ST1
               MOVE "DISTRIBUTION BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME
           MOVE "SlDistMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE DISTRIBUTIONS.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldNumeric".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldIndex1".
       Copy "WriteFieldRec".
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
