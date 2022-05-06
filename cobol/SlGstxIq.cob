        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlGstxIq.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectSlParameter".
         Copy "SelectSlDistributions".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDisTot.
           COPY ChlfdParam.

       WORKING-STORAGE SECTION.
       77  WS-WEEK              PIC S9(8)V99 VALUE 0.
       77  WS-PTD               PIC S9(8)V99 VALUE 0.
       77  WS-YTD               PIC S9(8)V99 VALUE 0.
       01  WS-DISTRIBUTION-STATUS.
           03  WS-DISTRIBUTION-ST1 PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1  PIC 99.
       Copy "WsDateInfo".
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
       CONTROL-010.
           PERFORM CLEAR-SCREEN
           PERFORM DISPLAY-FORM
           PERFORM READ-MASTER-DISPLAY
           PERFORM END-OFF.
      *
       READ-MASTER-DISPLAY SECTION.
       READ-000.
            MOVE 0 TO PA-TYPE.
            MOVE 1 TO PA-RECORD.
            READ PARAMETER-FILE 
               INVALID KEY NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 23 OR 35 OR 49
               MOVE "NO PARAMETER RECORD, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO READ-999.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO READ-000.
       READ-005.
            MOVE "1" TO DIST-KEY.
            READ DISTRIBUTIONS
               INVALID KEY NEXT SENTENCE.
            IF WS-DISTRIBUTION-ST1 = 23 OR 35 OR 49
               MOVE "NO TAX RECORD, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO READ-999.
            IF WS-DISTRIBUTION-ST1 NOT = 0
               MOVE "VAT RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DISTRIBUTION-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DISTRIBUTION-ST1
               GO TO READ-005.
        READ-010.
            MOVE "TAXPERCENT"   TO F-FIELDNAME
            MOVE 10             TO F-CBFIELDNAME
            MOVE PA-GST-PERCENT TO F-EDNAMEFIELDAMOUNTDIS
            MOVE 5              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-AMOUNTDIS.

            MOVE "TAXWEEK" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE GST-AMT-TAXED-WEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "TAXPTD" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE GST-AMT-TAXED-PTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "TAXYTD" TO F-FIELDNAME.
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

            ADD GST-AMT-TAXABLE-WEEK
                GST-AMT-NONTAXABLE-WEEK
                GST-AMT-EXPORT-WEEK
                 GIVING WS-WEEK.

            MOVE "TOTALWEEK" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE WS-WEEK TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD GST-AMT-TAXABLE-PTD
                GST-AMT-NONTAXABLE-PTD
                GST-AMT-EXPORT-PTD
                 GIVING WS-PTD.

            MOVE "TOTALPTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-PTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD GST-AMT-TAXABLE-YTD
                GST-AMT-NONTAXABLE-YTD
                GST-AMT-EXPORT-YTD
                 GIVING WS-YTD.

            MOVE "TOTALYTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-YTD TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.
        READ-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DISTRIBUTIONS.
            IF WS-DISTRIBUTION-ST1 NOT = 0
               MOVE "DISTRIBUTION BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-010.
            OPEN I-O PARAMETER-FILE.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER BUSYON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-010.
       OPEN-015.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlGstxIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE DISTRIBUTIONS
                  PARAMETER-FILE.
            MOVE "Press 'ESC' To Clear The Screen & EXIT THE PROGRAM."
                TO WS-MESSAGE
                  PERFORM ERROR-MESSAGE.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "ReadFieldNumeric".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldRec".
       Copy "WriteFieldAmountDis".
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
