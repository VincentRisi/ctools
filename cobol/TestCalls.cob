        IDENTIFICATION DIVISION.
        PROGRAM-ID. TestCalls.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY "STRELOMT.cob".
        01  F-FORMS.
           03  F-ATTR              BINARY-SHORT.
           03  F-NPOS              BINARY-SHORT.
           03  F-STATUS            PIC X(6) VALUE "DISPLY".
           03  F-FLDCOMP           BINARY-SHORT.
           03  F-ERROR             BINARY-SHORT.
           03  F-ERROR1            BINARY-SHORT.
           03  F-ERROR2            BINARY-SHORT.
           03  F-ERROR3            BINARY-SHORT.
           03  F-ERROR4            BINARY-SHORT.
           03  F-ERROR5            BINARY-SHORT.
           03  F-FH                BINARY-SHORT.
           03  F-FILENO OCCURS 10  BINARY-SHORT.
           03  F-FILENAME          PIC X(40) VALUE SPACES.
           03  F-CBFILENAME        BINARY-SHORT VALUE 0.
           03  F-INTEGERZERO       BINARY-SHORT VALUE 0.
           03  F-OPENMODE          PIC X(2) VALUE "mr".
           03  F-OPENMODE-MM       PIC X(2) VALUE "mm".
           03  F-FORMNAME          PIC X(20) VALUE SPACES.
           03  F-CBFORMNAME        BINARY-SHORT VALUE 0.
           03  F-FORM              PIC X(5000) VALUE " ".
           03  F-CBMAX             BINARY-SHORT VALUE 5000.
           03  F-IFRAME            BINARY-SHORT VALUE 0.
           03  F-ICOL              BINARY-SHORT VALUE 0.
           03  F-ILINE             BINARY-SHORT VALUE 0.
           03  F-FIELDNAME         PIC X(20).
           03  F-FIELDNAMEACC      PIC X(7).
           03  F-CBFIELDNAME       BINARY-SHORT.
           03  F-CBFIRSTLINE       BINARY-SHORT.
           03  F-CBFIELDLENGTH     BINARY-SHORT.
           03  F-TYPE              PIC X(10) VALUE "CHARACTER.".
           03  F-NUMBERFIELD       PIC X(3).
           03  F-NAMEFIELD         PIC X(40).
           03  F-NAMEFIELD60       PIC X(60).
           03  F-NAMEFIELDJOB      PIC Z(3)9.
           03  F-NAMEFIELDNUMNEW   PIC 9(8).
           03  F-NAMEFIELDNUM      PIC 9(11).
           03  F-NAMEFIELDNUMDEC   PIC 9(8)V9(5).
           03  F-NAMEFIELDACC      PIC 9(7).
           03  F-EDRATE            PIC Z(4)9.9(5).
           
           03  F-EDNAMEFIELDPTY       PIC Z9.
           03  F-EDNAMEFIELDDBL       PIC 99.
           03  F-EDNAMEFIELDACC       PIC 9(7).
           03  F-EDNAMEFIELDADDON     PIC Z(3)9.99.
           03  F-EDNAMEFIELDAMOUNT    PIC Z(5)9.99.
           03  F-EDNAMEFIELDAMTBLANK  PIC Z(5)9.99 BLANK WHEN ZERO.
           03  F-EDNAMEFIELDAMOUNT1   PIC Z(4)9.99.
           03  F-EDNAMEFIELDAMOUNTDIS PIC Z9.99.
           03  F-EDNAMEFIELDANAL      PIC Z9.
           03  F-EDNAMEFIELDCHANGE    PIC Z(3)9.
           03  F-EDNAMEFIELDCODE      PIC X.
           03  F-EDNAMEFIELDCRED      PIC Z(4)9.
           03  F-EDNAMEFIELDCREDIT    PIC Z(6)9.
           03  F-EDNAMEFIELDDATE      PIC 9(6).
           03  F-EDNAMEFIELDDUTY      PIC Z9.9.
           03  F-EDNAMEFIELDFACTOR    PIC Z9.9999.
           03  F-EDNAMEFIELDFAX       PIC 9.
           03  F-EDNAMEFIELDFOREIGN   PIC Z(5)9.999.
           03  F-EDNAMEFIELDFOREIGN99 PIC Z(7)9.999.
           03  F-EDNAMEFIELD9MIL      PIC Z(6)9.99.
           03  F-EDNAMEFIELD99MIL     PIC Z(7)9.99.
           03  F-EDNAMEFIELDFOREX     PIC Z(5)9.99- BLANK WHEN ZERO.
           03  F-EDNAMEFIELDFORTOTAL  PIC Z(7)9.99.
           03  F-EDNAMEFIELDGROUP     PIC Z(2)9.
           03  F-EDNAMEFIELDIND       PIC ZZ.
           03  F-EDNAMEFIELDIND1      PIC 9.
           03  F-EDNAMEFIELDINV       PIC Z(5)9.
           03  F-EDNAMEFIELDKITQTY    PIC Z(2)9.
           03  F-EDNAMEFIELDLINE      PIC ZZ9.
           03  F-EDNAMEFIELDNUM       PIC Z(5)9 BLANK WHEN ZERO.
           03  F-EDNAMEFIELDNUM5      PIC Z(5)9.99.
           03  F-EDNAMEFIELDNUM6      PIC Z(6)9.99-.
           03  F-EDNAMEFIELDNUMBER    PIC Z(5)9-.
           03  F-EDNAMEFIELDNUMDEC    PIC Z(3)9.9(5).
           03  F-EDNAMEFIELDNUMNEG    PIC Z(4)9-.
           03  F-EDNAMEFIELDPERC      PIC Z(2)9.99.
           03  F-EDNAMEFIELDPERCNEG   PIC Z(2)9.99-.
           03  F-EDNAMEFIELDPOST      PIC 9(4).
           03  F-EDNAMEFIELDPRICE     PIC Z(5)9.99.
           03  F-EDNAMEFIELDQTY       PIC Z(4)9.
           03  F-EDNAMEFIELDQTYCRED   PIC Z(3)9-.
           03  F-EDNAMEFIELDREC       PIC Z(7)9.99-.
           03  F-EDNAMEFIELDSALE      PIC Z(6)9.99-.
           03  F-EDNAMEFIELDTARIFF    PIC Z(8) BLANK WHEN ZERO.
           03  F-EDNAMEFIELDTRANS     PIC Z(3)9.99999.
           03  F-EDNAMEFIELDTYPE      PIC Z9.9.
           03  F-EDNAMEFIELDVALUE     PIC Z(2)9.9(5).
           03  F-EDRUNNINGAMOUNT      PIC Z(5)9.99-.

           03  F-INDEX                BINARY-SHORT VALUE 1.
           03  F-FXNO                 BINARY-SHORT VALUE 1.
           03  F-NO                   BINARY-SHORT VALUE 1.
           03  F-ILED                 BINARY-SHORT VALUE 7.
           03  F-FON                  PIC X(2) VALUE X"FFFF".
       01  F-HEX.
           03  FILLER PIC X OCCURS 80.
       01  F-HEXOVER1 REDEFINES F-HEX.
           03  F-HEX1ST            PIC X(20).
           03  F-HEX2ND            PIC X(20).
           03  F-HEX3RD            PIC X(20).
           03  F-HEX4TH            PIC X(20).
       01  F-INITSTATE.
           03  F-INIT-ICH          BINARY-SHORT VALUE 0.
           03  FILLER              PIC X(6).
       01  F-EXITSTATE.
           03  F-EXIT-ICH          BINARY-SHORT.
           03  F-EXIT-CH           PIC X.
           03  FILLER              PIC X(13).
       01  F-DISPLAY.
           03  F-POSITION.
               05  F-LINEPOS       PIC 99.
           03  F-FUNCCODE          PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       000-Main.
           DISPLAY 'Starting cobmain'.
           MOVE '/ctools/lib/CoForms.Lib' TO F-FILENAME.
           MOVE 33 TO F-CBFILENAME.
           MOVE N-FORMNAME   OF DEFS-STRELOMT  TO F-FORMNAME.
           MOVE N-CBFORMNAME OF DEFS-STRELOMT  TO F-CBFORMNAME.
           PERFORM OPEN-900.
           CALL "DISPLAYFORM" USING F-ERROR1
                                    F-FORM
                                    F-IFRAME
                                    F-ICOL
                                    F-ILINE.
           IF F-ERROR1 NOT = 0
               DISPLAY "DISPLAY FORM ERROR"
               DISPLAY F-ERROR1
               STOP RUN.
           PERFORM FILL-050.    
           PERFORM WRITE-101.    
           PERFORM INPUT-200.    
           PERFORM READ-301.
           CALL "CLOSEFORM"  USING F-ERROR2
                                   F-FORM.
           IF F-ERROR2 NOT = 0
               DISPLAY "CLOSE FORM ERROR"
               DISPLAY F-ERROR2
               STOP RUN.
           DISPLAY "LOCK KBD"
           CALL "LOCKKBD" USING F-ERROR1.
           DISPLAY F-ERROR1
           PERFORM DISPLAY-400.
           STOP RUN.
       FILL-050.    
           MOVE '1' TO D-TRANSCODE.
           MOVE 'ONE IS ONE IS ONE' TO D-TRANSDESC.
           PERFORM VARYING F-INDEX FROM 1 BY 1 UNTIL F-INDEX > 10
               MOVE 'STOCK ' TO D-STOCKNUMBER(F-INDEX)
               MOVE F-INDEX TO F-EDNAMEFIELDINV
               MOVE F-EDNAMEFIELDINV TO D-QUANTITY(F-INDEX)
               COMPUTE F-EDNAMEFIELD99MIL = F-INDEX + 1.01
               END-COMPUTE
               MOVE F-EDNAMEFIELD99MIL TO D-UNITPRICE(F-INDEX)               
               COMPUTE F-EDNAMEFIELD99MIL = ( F-INDEX + 1.01 ) 
                                          * F-INDEX
               END-COMPUTE
               MOVE F-EDNAMEFIELD99MIL TO D-TOTALPRICE(F-INDEX)               
               MOVE 'REFERENCENO ' TO D-REFERENCENO(F-INDEX)
               MOVE '2014-01-01' TO D-REFERENCEDATE(F-INDEX)
           END-PERFORM.  
           MOVE 'DESCRIPTION1' TO D-DESCRIPTION1.
           MOVE 'DESCRIPTION2' TO D-DESCRIPTION2.
       WRITE-100.
           CALL 'WRITEFIELD' 
                USING F-ERROR1 F-FORM 
                      N-TRANSCODE N-CBTRANSCODE N-OCCTRANSCODE 
                      D-TRANSCODE D-CBTRANSCODE F-TYPE 
           END-CALL.
           CALL 'WRITEFIELD' 
                USING F-ERROR1 F-FORM 
                      N-TRANSDESC N-CBTRANSDESC N-OCCTRANSDESC 
                      D-TRANSDESC D-CBTRANSDESC F-TYPE 
           END-CALL.
           PERFORM VARYING F-INDEX FROM 1 BY 1 UNTIL F-INDEX > 10
               CALL 'WRITEFIELD' 
                    USING F-ERROR1 F-FORM 
                          N-STOCKNUMBER N-CBSTOCKNUMBER F-INDEX
                          D-STOCKNUMBER(F-INDEX) D-CBSTOCKNUMBER F-TYPE 
               END-CALL       
               CALL 'WRITEFIELD' 
                    USING F-ERROR1 F-FORM 
                          N-QUANTITY N-CBQUANTITY F-INDEX
                          D-QUANTITY(F-INDEX) D-CBQUANTITY F-TYPE 
               END-CALL         
               CALL 'WRITEFIELD' 
                    USING F-ERROR1 F-FORM 
                          N-UNITPRICE N-CBUNITPRICE F-INDEX
                          D-UNITPRICE(F-INDEX) D-CBUNITPRICE F-TYPE 
               END-CALL         
               CALL 'WRITEFIELD' 
                    USING F-ERROR1 F-FORM 
                          N-TOTALPRICE N-CBTOTALPRICE F-INDEX
                          D-TOTALPRICE(F-INDEX) D-CBTOTALPRICE F-TYPE 
               END-CALL         
               CALL 'WRITEFIELD' 
                    USING F-ERROR1 F-FORM 
                          N-REFERENCENO N-CBREFERENCENO F-INDEX
                          D-REFERENCENO(F-INDEX) D-CBREFERENCENO F-TYPE 
               END-CALL       
               CALL 'WRITEFIELD' 
                    USING F-ERROR1 F-FORM 
                          N-REFERENCEDATE N-CBREFERENCEDATE F-INDEX
                          D-REFERENCEDATE(F-INDEX) D-CBREFERENCEDATE 
                          F-TYPE 
               END-CALL       
           END-PERFORM.  
           CALL 'WRITEFIELD' 
                USING F-ERROR1 F-FORM 
                      N-DESCRIPTION1 N-CBDESCRIPTION1 N-OCCDESCRIPTION1 
                      D-DESCRIPTION1 D-CBDESCRIPTION1 F-TYPE 
           END-CALL.
           CALL 'WRITEFIELD' 
                USING F-ERROR1 F-FORM 
                      N-DESCRIPTION2 N-CBDESCRIPTION2 N-OCCDESCRIPTION2 
                      D-DESCRIPTION2 D-CBDESCRIPTION2 F-TYPE 
           END-CALL.
       WRITE-101. 
           call 'WRITEALL' using F-ERROR1 F-FORM DATA-STRELOMT
           end-call.
       INPUT-200.   
           move 1 to F-NO.
           move x'0A' to F-EXIT-CH.
           perform forever
             if F-NO < 1
               move 1 to F-NO
             end-if               
             if F-NO > 64
               move 64 to F-NO
             end-if               
             move F-NO to F-FXNO
             move 0 to F-INDEX
             if F-NO > 2 and < 63
               subtract 3 from F-FXNO                 
               divide 6 into F-FXNO giving F-INDEX remainder F-FXNO
               add 3 to F-FXNO
               add 1 to F-INDEX
             end-if               
             if F-NO > 62
               subtract 54 from F-FXNO
             end-if               
             move 0 to F-INIT-ICH
             call 'USERFILLFIELD' 
               using F-ERROR1 F-FORM
                     N-FIELDNAME   OF N-FIELD-ARRAY(F-FXNO) 
                     N-CBFIELDNAME OF N-FIELD-ARRAY(F-FXNO) 
                     F-INDEX
                     F-INITSTATE F-EXITSTATE
             end-call        
             if F-EXIT-CH = X'01'
                subtract 1 from F-NO
             else if F-EXIT-CH = X'0B' or X'09' or X'12'
                add 1 to F-NO
             else  
                exit perform             
             end-if
           end-perform.    
       READ-300.    
           CALL 'READFIELD'
                USING F-ERROR1 F-FORM 
                      N-TRANSCODE N-CBTRANSCODE N-OCCTRANSCODE 
                      D-TRANSCODE D-CBTRANSCODE F-CBFIELDLENGTH F-TYPE 
           END-CALL. 
           CALL 'READFIELD' 
                USING F-ERROR1 F-FORM 
                      N-TRANSDESC N-CBTRANSDESC N-OCCTRANSDESC 
                      D-TRANSDESC D-CBTRANSDESC F-CBFIELDLENGTH F-TYPE 
           END-CALL.
           PERFORM VARYING F-INDEX FROM 1 BY 1 UNTIL F-INDEX > 10
               CALL 'READFIELD' 
                    USING F-ERROR1 F-FORM 
                          N-STOCKNUMBER N-CBSTOCKNUMBER F-INDEX
                          D-STOCKNUMBER(F-INDEX) D-CBSTOCKNUMBER 
                          F-CBFIELDLENGTH F-TYPE 
               END-CALL       
               CALL 'READFIELD' 
                    USING F-ERROR1 F-FORM 
                          N-QUANTITY N-CBQUANTITY F-INDEX
                          D-QUANTITY(F-INDEX) D-CBQUANTITY 
                          F-CBFIELDLENGTH F-TYPE 
               END-CALL         
               CALL 'READFIELD' 
                    USING F-ERROR1 F-FORM 
                          N-UNITPRICE N-CBUNITPRICE F-INDEX
                          D-UNITPRICE(F-INDEX) D-CBUNITPRICE 
                          F-CBFIELDLENGTH F-TYPE 
               END-CALL         
               CALL 'READFIELD' 
                    USING F-ERROR1 F-FORM 
                          N-TOTALPRICE N-CBTOTALPRICE F-INDEX
                          D-TOTALPRICE(F-INDEX) D-CBTOTALPRICE 
                          F-CBFIELDLENGTH F-TYPE 
               END-CALL         
               CALL 'READFIELD' 
                    USING F-ERROR1 F-FORM 
                          N-REFERENCENO N-CBREFERENCENO F-INDEX
                          D-REFERENCENO(F-INDEX) D-CBREFERENCENO 
                          F-CBFIELDLENGTH F-TYPE 
               END-CALL       
               CALL 'READFIELD' 
                    USING F-ERROR1 F-FORM 
                          N-REFERENCEDATE N-CBREFERENCEDATE F-INDEX
                          D-REFERENCEDATE(F-INDEX) D-CBREFERENCEDATE 
                          F-CBFIELDLENGTH F-TYPE 
               END-CALL       
           END-PERFORM.  
           CALL 'READFIELD' 
                USING F-ERROR1 F-FORM 
                      N-DESCRIPTION1 N-CBDESCRIPTION1 N-OCCDESCRIPTION1 
                      D-DESCRIPTION1 D-CBDESCRIPTION1 
                      F-CBFIELDLENGTH F-TYPE 
           END-CALL.
           CALL 'READFIELD' 
                USING F-ERROR1 F-FORM 
                      N-DESCRIPTION2 N-CBDESCRIPTION2 N-OCCDESCRIPTION2 
                      D-DESCRIPTION2 D-CBDESCRIPTION2 
                      F-CBFIELDLENGTH F-TYPE 
           END-CALL.
       READ-301.    
           call 'READALL' using F-ERROR1 F-FORM DATA-STRELOMT
           end-call.
       DISPLAY-400.    
         DISPLAY 'TRANSCODE: ' D-TRANSCODE.
         DISPLAY 'TRANSDESC: ' D-TRANSDESC.
         PERFORM VARYING F-NO FROM 1 BY 1 UNTIL F-NO > 10
           DISPLAY 'STOCKNUMBER  (' F-NO '):' D-STOCKNUMBER(F-NO)
           DISPLAY 'QUANTITY     (' F-NO '):' D-QUANTITY(F-NO)
           DISPLAY 'UNITPRICE    (' F-NO '):' D-UNITPRICE(F-NO)               
           DISPLAY 'TOTALPRICE   (' F-NO '):' D-TOTALPRICE(F-NO)               
           DISPLAY 'REFERENCENO  (' F-NO '):' D-REFERENCENO(F-NO)
           DISPLAY 'REFERENCEDATE(' F-NO '):' D-REFERENCEDATE(F-NO)
         END-PERFORM.  
         DISPLAY 'D-DESCRIPTION1: ' D-DESCRIPTION1.
         DISPLAY 'D-DESCRIPTION2: ' D-DESCRIPTION2.
       OPEN-900.
           CALL "OPENFILE" USING F-ERROR1
                                 F-FH
                                 F-FILENAME
                                 F-CBFILENAME
                                 F-FILENAME
                                 F-INTEGERZERO
                                 F-OPENMODE.
           IF F-ERROR1 NOT = 0
               DISPLAY "OPEN FILE FORM ERROR"
               DISPLAY F-ERROR1
               STOP RUN.
           CALL "OPENFORM" USING F-ERROR2
                                 F-FH
                                 F-FORMNAME
                                 F-CBFORMNAME
                                 F-FORM
                                 F-CBMAX.
           IF F-ERROR2 NOT = 0
               DISPLAY "OPEN FORM IN LIB ERROR"
               DISPLAY F-ERROR2
               STOP RUN.
           CALL "CLOSEFILE" USING F-ERROR1
                                  F-FH.
           IF F-ERROR1 NOT = 0
               DISPLAY "CLOSEFILE ERROR"
               DISPLAY F-ERROR1
               STOP RUN.
           
