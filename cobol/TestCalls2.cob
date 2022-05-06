identification division.
program-id. TestCalls2.
data division.
working-storage section.
copy "STRELOMT.cob".
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
  
  03  F-TITLE             PIC X(40).
   
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

  03  F-INDEX  BINARY-SHORT VALUE 1.
  03  F-FXNO   BINARY-SHORT VALUE 1.
  03  F-NO     BINARY-SHORT VALUE 1.
  03  F-ILED   BINARY-SHORT VALUE 7.
  03  F-FON    PIC X(2) VALUE X"FFFF".
01  F-HEX.
  03  FILLER PIC X OCCURS 80.
01  F-HEXOVER1 REDEFINES F-HEX.
  03  F-HEX1ST    PIC X(20).
  03  F-HEX2ND    PIC X(20).
  03  F-HEX3RD    PIC X(20).
  03  F-HEX4TH    PIC X(20).
01  F-INITSTATE.
  03  F-INIT-ICH  BINARY-SHORT VALUE 0.
  03  FILLER      PIC X(6).
01  F-EXITSTATE.
  03  F-EXIT-ICH  BINARY-SHORT.
  03  F-EXIT-CH   PIC X.
  03  FILLER      PIC X(13).
01  F-DISPLAY.
  03  F-POSITION.
    05  F-LINEPOS PIC 99.
  03  F-FUNCCODE  PIC 99 VALUE 0.

01 F-OTHERS.
  03 F-OCOLOR  BINARY-SHORT VALUE 7.
  03 F-OATTR   PIC X VALUE 'A'.
  03 F-OLINE   BINARY-SHORT.
  03 F-OCOLUMN BINARY-SHORT. 
  03 F-ODATA   PIC X(60). 
  03 F-OCBDATA BINARY-SHORT. 

procedure division.
000-Main.
  display 'Starting cobmain'.
  move '/ctools/lib/CoForms.Lib' to F-FILENAME.
  move 33 to F-CBFILENAME.
  move N-FORMNAME   of DEFS-STRELOMT  to F-FORMNAME.
  move N-CBFORMNAME of DEFS-STRELOMT  to F-CBFORMNAME.
  move '** FRED MICHAEL AND CAPLAN **' to F-TITLE.
  call "DATEMAILCO" using F-TITLE.
  perform OPEN-900.
  call "DISPLAYFORM" using F-ERROR1
                           F-FORM
                           F-IFRAME
                           F-ICOL
                           F-ILINE.
  IF F-ERROR1 not = 0
      display "display FORM ERROR"
      display F-ERROR1
      stop run.
  perform FILL-050.    
  perform WRITE-101.    
  perform INPUT-200.    
  perform READ-301.
  *>call "CLOSEFORM"  using F-ERROR2
  *>                        F-FORM.
  if F-ERROR2 not = 0
      display "CLOSE FORM ERROR"
      display F-ERROR2
      stop run.
  display "LOCK KBD"
  call "LOCKKBD" using F-ERROR1.
  display F-ERROR1
  perform display-400.
  stop run.
FILL-050.    
  move '1' to D-TRANSCODE.
  move 'ONE IS ONE IS ONE' to D-TRANSDESC.
  perform varying F-INDEX from 1 by 1 until F-INDEX > 10
      move 'STOCK ' to D-STOCKNUMBER(F-INDEX)
      move F-INDEX to F-EDNAMEFIELDINV
      move F-EDNAMEFIELDINV to D-QUANTITY(F-INDEX)
      compute F-EDNAMEFIELD99MIL = F-INDEX + 1.01
      end-compute
      move F-EDNAMEFIELD99MIL to D-UNITPRICE(F-INDEX)               
      compute F-EDNAMEFIELD99MIL = ( F-INDEX + 1.01 ) 
                                 * F-INDEX
      end-compute
      move F-EDNAMEFIELD99MIL to D-TOTALPRICE(F-INDEX)               
      move 'REFERENCENO ' to D-REFERENCENO(F-INDEX)
      move '2014-01-01' to D-REFERENCEDATE(F-INDEX)
  end-perform.  
  move 'DESCRIPTION1' to D-DESCRIPTION1.
  move 'DESCRIPTION2' to D-DESCRIPTION2.
WRITE-101.
  move 'C' to F-OATTR.
  move 23 to F-OLINE.
  move 50 to F-OCOLUMN.
  move 'Please note the loss.' to F-ODATA.
  move 21 to F-OCBDATA.
  call 'WRITETO' using F-ERROR1 F-FORM F-OCOLUMN F-OLINE 
                       F-ODATA F-OCBDATA F-OCOLOR F-OATTR 
  end-call.                       
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
            N-FIELDNAME   of N-FIELD-ARRAY(F-FXNO) 
            N-CBFIELDNAME of N-FIELD-ARRAY(F-FXNO) 
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
READ-301.    
  call 'READALL' using F-ERROR1 F-FORM DATA-STRELOMT
  end-call.
display-400.    
  display 'TRANSCODE: ' D-TRANSCODE.
  display 'TRANSDESC: ' D-TRANSDESC.
  perform varying F-NO from 1 by 1 until F-NO > 10
    display 'STOCKNUMBER  (' F-NO '):' D-STOCKNUMBER(F-NO)
    display 'QUANTITY     (' F-NO '):' D-QUANTITY(F-NO)
    display 'UNITPRICE    (' F-NO '):' D-UNITPRICE(F-NO)               
    display 'TOTALPRICE   (' F-NO '):' D-TOTALPRICE(F-NO)               
    display 'REFERENCENO  (' F-NO '):' D-REFERENCENO(F-NO)
    display 'REFERENCEDATE(' F-NO '):' D-REFERENCEDATE(F-NO)
  end-perform.  
  display 'D-DESCRIPTION1: ' D-DESCRIPTION1.
  display 'D-DESCRIPTION2: ' D-DESCRIPTION2.
OPEN-900.
  call "OPENFILE" using F-ERROR1
                        F-FH
                        F-FILENAME
                        F-CBFILENAME
                        F-FILENAME
                        F-INTEGERZERO
                        F-OPENMODE.
  if F-ERROR1 not = 0
    display "OPEN FILE FORM ERROR"
    display F-ERROR1
    stop run.
  call "OPENFORM" using F-ERROR2
                        F-FH
                        F-FORMNAME
                        F-CBFORMNAME
                        F-FORM
                        F-CBMAX.
  if F-ERROR2 not = 0
    display "OPEN FORM IN LIB ERROR"
    display F-ERROR2
    stop run.
  call "CLOSEFILE" using F-ERROR1
                         F-FH.
  if F-ERROR1 not = 0
    display "CLOSEFILE ERROR"
    display F-ERROR1
    stop run.
   

