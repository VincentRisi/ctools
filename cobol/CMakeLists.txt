project (ctools_cobol)

set (COBOL_SOURCE ${CMAKE_SOURCE_DIR}/cobol)
set (COBOL_BINARY ${CMAKE_BINARY_DIR}/cobol)

function (cobol_executable name)
  set (source ${COBOL_SOURCE}/${name}.cob)
  set (output ${COBOL_BINARY}/${name})
  add_custom_command (
    OUTPUT ${output}
    DEPENDS ${source} ${COBOL_BINARY}/cobcalls.o
    COMMAND cobc -g -I ${COBOL_BINARY} -I ${COBOL_SOURCE} -I ${COBOL_SOURCE}/copy 
                 -I ${COBOL_SOURCE}/fd -x ${source} -l rt ${COBOL_BINARY}/cobcalls.o 
                 -L ${CMAKE_BINARY_DIR}/lib -l b20forms
  )
  add_custom_target (${name} ALL
    DEPENDS ${output} b20forms
    SOURCES ${source}
  )
  install (PROGRAMS ${output} DESTINATION ${CMAKE_INSTALL_PREFIX}/bin COMPONENT cobol_executable)
endfunction()

function (cobol_module name)
  set (source ${COBOL_SOURCE}/${name}.cob)
  set (output ${COBOL_BINARY}/${name}.so)
  add_custom_command (
    OUTPUT ${output}
    DEPENDS ${source} ${COBOL_BINARY}/cobcalls.o
    COMMAND cobc -g -I ${COBOL_BINARY} -I ${COBOL_SOURCE} -I ${COBOL_SOURCE}/copy 
                 -I ${COBOL_SOURCE}/fd -m ${source} -l rt ${COBOL_BINARY}/cobcalls.o 
                 -L ${CMAKE_BINARY_DIR}/lib -l b20forms
  )
  add_custom_target (${name} ALL
    DEPENDS ${output} b20forms
    SOURCES ${source}
  )
  install (FILES ${output} DESTINATION ${CMAKE_INSTALL_PREFIX}/bin COMPONENT cobol_module)
endfunction()

set (executable_list 
  MainCont TestMenu TestStuff
)

foreach (name ${executable_list})
  cobol_executable(${name})
endforeach()  

set (company_list 
  CoCsParm  CoFxParm  Company   CoPrinter  CoPrntMt  CoPuByMt CoQuParm
  CoRpParm  CoStffIq  CoStffMt  CrPrinter  DrPrinter DataName PassWrds 
  OfNameIq
)

set (menu_list 
  Dr1Menu   Dr2Menu  Dr3Menu  Dr4Menu
  St1Menu   St2Menu  St3Menu  St4Menu
  Sl1Menu   Sl2Menu  Sl3Menu  Sl4Menu Sl5Menu Sl6Menu
  Cr1Menu   Cr2Menu  Cr3Menu  Cr4Menu
  Gl1Menu   Gl2Menu  Gl3Menu  Gl4Menu
  BaUpMenu  Bm1Menu  Cb1Menu  MastMenu
  ContMenu  MenuPass   
)

set (billofmaterial_list 
  BmAnalCh  BmKitCop  BmKitsIq  BmKitsMt  BmKitsRp  BmKtAnRp  BmKtChMt  BmKtItIq
  BmKtLiRp  BmKtQyMt  BmNewItm  BmKitMfg  BmKtMfIq  BmKtMfLy  BmKtRgRp
)

set (cashbook_list 
  CbBankTr CbCamsAl  CbCamsMt  CbGlPost  CbMaLyIq  CbMastIq  CbMastMt
  CbPerEnd CbStAlMt  CbStatRp  CbTranIq  CbTrans   CbTrFuIq  CbTrLyIq   CbTrOsRp
)

set (creditors_list 
  CrAdjuIq  CrAgeARp  CrAgeTmp  CrAgeTRp  CrAliaRp  CrAlias   CrCAMSCt  CrCAMSDl
  CrCAMSIq  CrCAMSMt  CrCheqMt  CrCheqRp  CrCurrMt  CrFBCMt   CrFBCPRp  CrFBCRp
  CrFBCTrs  CrInCrMt  CrInCrRp  CrJnAnRp  CrJrnIq   CrJrnMt   CrLablRp  CrLaPuRp  CrMasOld
  CrMaster  CrMastIq  CrMastMt  CrMastRp  CrNameIq  CrNamOld  CrNmFxRp
  CrNoChMt  CrPayDMt  CrPaymnt  CrPaymRp  CrPerEnd  CrReFrAl  CrRemiEM  CrRemiIq
  CrRemiMt  CrRemiRp  CrReTrMt  CrTrans   CrTranIq  CrTransMv
)

set (debtors_list 
  DrAcStIq  DrAcStRp  DrAdjuIq  DrAdSlAc  DrAgeARp  DrAgeTmp  DrAgeTRp
  DrAlDiRp  DrAnChck  DrArJhRp  DrArSaRp  DrArToRp  DrCorpRp  DrCredRp
  DrCrLiMt  DrDateMt  DrDelvMt  DrDiAcRp  DrDiscRp  DrDiStRp  DrDuFxRp
  DrInteMt  DrLablRp  DrLaSoRp  DrMailRp  DrMasOld  DrMasSMt  DrMaster
  DrMastIq  DrMastMt  DrMastRp  DrNameIq  DrNamOld  DrNmFxRp  DrNoChMt
  DrNumber  DrPaymnt  DrPrAcRp  DrRepRst  DrSaCoMt  DrSaCoRp  DrSaMxRp 
  DrStatRp  DrTrans   DrTranMv  DrMastMv
)

set (general_list 
  GlAsInRp  GlAsRgRp  GlBalSRp  GlBlGpRp  GlBudgMt  GlInGpRp  GlInStRp
  GlJrnARp  GlJrnBat  GlJrnlMt  GlJrnLRp  GlMaLyIq  GlMaster  GlMastIq
  GlMastLy  GlMastMt  GlMastRp  GlNameIq  GlNameLy  GlNoChMt  GlParmMt
  GlPerEnd  GlRandom  GlTbLyRp  GlTranIq  GlTranLy  GlTrans   GlTrBaRp  
  GlTrbSRp  GlTrFuIq  GlTrLyIq  GlTrSrch  GlTranMv
)

set (sales_list 
  SlAnCoRp SlAreaMt SlBrOrdr SlBrchMt SlCashRp SlChckRp SlCredit SlCrdRev
  SlDelvMt SlDelvRp SlDistMt SlDistIq SlDistRp SlDyExRp SlGstxRp SlGstxIq
  SlInAcIq SlInAcLy SlInCoMt SlInCrLy SlInCrRp SlInDlLy SlInDlMt SlInOrIq
  SlInOrLy SlInRgRp SlInStIq SlInStLy SlIRegLy SlInvoic SlInvRev SlIRegIq
  SlIRegMt SlMastMt SlMastIq SlMastRp SlOrChMt SlOrChRp SlOrFiRp SlOrRgRp
  SlOrSuRp SlOrders SlPackRp SlParmMt SlPerEnd SlPOrdRp SlPoShLy SlPoShIq
  SlPSlpIq SlPsPuMt SlPuByMt SlPuByRp SlQuAcIq SlQuRgRp SlQuote  SlQuStIq
  SlRgAcRp SlRgName SlRgNaIq SlRgNaMy SlRgNaLy SlRgLyMt SlRgChLy SlRgAcLy
  SlRgChMt SlSBChMt SlSaleRp SlSManRp SlSoByMt SlSoByRp SlSpecRp SlSRepMt
  SlTermMt SlRegisterMv      SlQuBhRp
)

set (stock_list 
  StAlteIq  StAlteMt  StAlteRp  StAnalRp  StAnOhRp  StAveLst  StBinNRp  StBoAcIq
  StBoAcRp  StBoLeRp  StBoReMt  StBoStIq  StBoStRp  StBranch  StBrchIq  StBrCtMt
  StByGdRp  StCataMt  StCataRp  StCatPIq  StChngMt  StCounRp  StDescIq
  StDiAcBt  StDiAcIq  StDiAcMt  StHiSlRp  StImport  StInStRp  StLablRp  StLookMt
  StLookRp  StMasBMt  StMasFMt  StMasLMt  StMasOMt  StMasPMt  StMaster
  StMastIq  StMastMt  StMastRp  StMiMxMt  StMiMxRp  StMvPeRp  StNoChMt
  StNoCopy  StNumber  StOrdrMt  StOrDtMt  StOrGnMt  StOrGnRp  StOrOrIq  StOrOrRp
  StOrPrRp  StOrPsMt  StOrStIq  StOrStRp  StOrSuIq  StOrSuRp  StPrChMt
  StPrCoRp  StPrFoRp  StPricMt  StPrLoRp  StPrNwMt  StPrSpMt  StPrSpRp  StQyAdRp
  StQyChRp  StReDtRp  StReduRp  StReImMt  StReImRp  StReLoIq  StReLoMt
  StReLoRp  StRepair  StRpAcIq  StRpRgRp  StRpStIq  StSGmRp   StSplit   StStQyMt
  StTariRp  StTranLy  StTrans   StTrChMt  StTrDuMt  StTrDuRp  StValuRp
  StTranMv StTrLyMv   StCataMv  StOrdrMv
)
foreach (name ${company_list})
 cobol_module(${name})
endforeach()
foreach (name ${menu_list})
  cobol_module(${name})
endforeach()
foreach (name ${billofmaterial_list})
 cobol_module(${name})
endforeach()
foreach (name ${cashbook_list})
  cobol_module(${name})
endforeach()
foreach (name ${creditors_list})
  cobol_module(${name})
endforeach()
foreach (name ${debtors_list})
  cobol_module(${name})
endforeach()
foreach (name ${general_list})
  cobol_module(${name})
endforeach()
foreach (name ${sales_list})
 cobol_module(${name})
endforeach()
foreach (name ${stock_list})
 cobol_module(${name})
endforeach()
