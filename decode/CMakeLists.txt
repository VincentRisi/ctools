project (ctools_decode)

set(generated
  ${CMAKE_CURRENT_BINARY_DIR}/Parser.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/Parser.h
  ${CMAKE_CURRENT_BINARY_DIR}/Scanner.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/Scanner.h
)

set (COCO ${CMAKE_BINARY_DIR}/bin/coco)
set (DECODE ${CMAKE_BINARY_DIR}/bin/ctos_decode)
set (ARCHIVE ${CMAKE_BINARY_DIR}/bin/ctos_archive)

include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR})

set (atg_source
  ${CMAKE_CURRENT_SOURCE_DIR}/ctosio.c
  ${CMAKE_CURRENT_SOURCE_DIR}/ctosio.h
  ${CMAKE_CURRENT_SOURCE_DIR}/ctosio.atg
  ${CMAKE_CURRENT_SOURCE_DIR}/Parser.frame
  ${CMAKE_CURRENT_SOURCE_DIR}/Scanner.frame
)

source_group("CocoR Files"    REGULAR_EXPRESSION ".*[.](atg|frame)$" )

##message (STATUS "${COCO} ${CMAKE_CURRENT_SOURCE_DIR}/ctosio.atg -o ${CMAKE_CURRENT_BINARY_DIR}")
  
add_custom_command (
  OUTPUT  ${generated}
  COMMAND ${COCO} ${CMAKE_CURRENT_SOURCE_DIR}/ctosio.atg -o ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS ${atg_source} coco
)

add_custom_target (ctos_decode_coco ALL
  DEPENDS ${generated}
  SOURCES ${atg_source} ${generated}
)

set (source 
  ${generated}
  ${atg_source}
  ${CMAKE_CURRENT_SOURCE_DIR}/binio.c 
  ${CMAKE_CURRENT_SOURCE_DIR}/binio.h
  ${CMAKE_CURRENT_SOURCE_DIR}/decode.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/decode.h
  ${CMAKE_CURRENT_SOURCE_DIR}/getargs.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/getargs.h
  ${CMAKE_CURRENT_SOURCE_DIR}/xdir.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/xdir.h
)

add_executable (ctos_decode ${source})
install (TARGETS ctos_decode RUNTIME DESTINATION bin)

set (source 
  ${CMAKE_CURRENT_SOURCE_DIR}/archive.c
  ${CMAKE_CURRENT_SOURCE_DIR}/archive.h
  ${CMAKE_CURRENT_SOURCE_DIR}/binio.c 
  ${CMAKE_CURRENT_SOURCE_DIR}/binio.h
)

add_executable (ctos_archive ${source})
install (TARGETS ctos_archive RUNTIME DESTINATION bin)

set (frp_list
  BAUPMENU  BM1MENU   CB1MENU   CONTMENU  CR1MENU   CR2MENU   CR3MENU
  CR4MENU   DR1MENU   DR2MENU   DR3MENU   DR4MENU   GL1MENU   GL2MENU
  GL3MENU   GL4MENU   MAINCONT  MASTMENU  SL1MENU   SL2MENU   SL3MENU
  SL4MENU   SL5MENU   SL6MENU   ST1MENU   ST2MENU   ST3MENU   ST4MENU

  DRACSTIQ  DRADJUIQ  DRCONTACT DRCRLIMT  DRDELVMT  DRDUFXRP  DRINTEMT
  DRMASTER  DRNAMEIQ  DRNOCHMT  DRPAYMNT  DRSTATRP  DRTRANS   DRLABLRP
  
  STALTEMT  STBOACIQ  STBOSTIQ  STBRCHIQ  STBRCTMT  STCATAMT  STCATPIQ
  STCRJNMT  STCRJNIM  STDESCIQ  STDIACBT  STDIACIQ  STDIACMT  STIMPORT
  STLABLRP  STLOOKMT  STMASTIQ  STMASTMT  STMIMXMT  STNOCHMT  STNOCOPY
  STORDRMT  STORORIQ  STORGNMT  STORSTIQ  STORSUIQ  STPRCHMT  STPRLORP
  STPRNWMT  STPRSPMT  STREIMMT  STRELOIQ  STRELOMT  STREPAIR  STRPACIQ
  STRPSTIQ  STTRANS

  SLAREAMT  SLBRCHMT  SLCRDREV  SLCREDIT  SLDELVMT  SLDISTMT  SLGSTXIQ
  SLINACIQ  SLINACLY  SLINCOMT  SLINCRRP  SLINDLLY  SLINDLMT  SLINORIQ
  SLINORLY  SLINSTIQ  SLINSTLY  SLINVOIC  SLINVREV  SLIREGIQ  SLIREGLY
  SLMASTIQ  SLORDERS  SLPACKRP  SLPARMMT  SLPOSHIQ  SLPOSHLY  SLPSLPIQ
  SLPSPUMT  SLPUBYMT  SLQUACIQ  SLQUOTE   SLQUSTIQ  SLRGACLY  SLRGBMIQ
  SLRGCHLY  SLRGNAIQ  SLSBREP   SLSOBYMT  SLTERMMT

  CRADJUIQ  CRALIAS   CRCAMSIQ  CRCAMSMT  CRCURRMT  CRFBCMT   CRFBCTRS
  CRINCRMT  CRJRNIQ   CRJRNMT   CRMASTIQ  CRNAMEIQ  CRNOCHMT  CRPAYDMT
  CRPAYMNT  CRREFRAL  CRREMIMT  CRRETRMT  CRTRANIQ  CRTRANS   CRLABLRP

  GLJRNLMT  GLMALYIQ  GLMASTIQ  GLNAMEIQ  GLNAMELY  GLNOCHMT  GLPARMCB
  GLPARMCR  GLPARMSL  GLPARMGL  GLTRANIQ  GLTRANLY  GLTRANS   GLTRFUIQ
  GLTRLYIQ

  BMKITCOP  BMKITMFG  BMKITSIQ  BMKITSMT  BMKTITIQ  
  
  CBBANKTR  CBCAMSAL  CBCAMSMT  CBMALYIQ  CBMASTER  CBSTALMT  CBTRANIQ
  CBTRANS   CBTRFUIQ  CBTRLYIQ

  COCSPARM  COFXPARM  COMPANY   COPRNTMT  COPUBYMT  COQUPARM  CORPPARM
  COSTFFIQ  COSTFFMT  MENUPASS  DATANAME

  OFNAMEIQ  OFNAMEMT
)

set (frp_source)
set (bin_files)
set (cob_files)
set (h_files)

foreach(name ${frp_list})
  list(APPEND frp_source ${CTOS_SOURCE_DIR}/data/${name}.FRP)
  list(APPEND bin_files  ${CTOS_BINARY_DIR}/data/${name}.bin)
  list(APPEND cob_files  ${CTOS_BINARY_DIR}/cob/${name}.cob)
  string(TOLOWER ${name} lowname)
  list(APPEND h_files    ${CTOS_BINARY_DIR}/c/${lowname}.h)
endforeach()  

set (libfile
  ${CTOS_BINARY_DIR}/data/CoForms.Lib
)

file (MAKE_DIRECTORY 
  ${CTOS_BINARY_DIR}/data
  ${CTOS_BINARY_DIR}/cob 
  ${CTOS_BINARY_DIR}/c
)

add_custom_command (
  OUTPUT  ${bin_files} ${cob_files} ${h_files} 
  COMMAND ${DECODE} ${frp_source} 
          -B ${CTOS_BINARY_DIR}/data
          -c -C ${CTOS_BINARY_DIR}/cob 
          -h -H ${CTOS_BINARY_DIR}/c
  DEPENDS ${frp_source} ctos_decode
)

add_custom_command (
  OUTPUT  ${libfile}
  COMMAND ${ARCHIVE} ${libfile} ${bin_files}
  DEPENDS ${bin_files}
)

add_custom_target (ctos_compile ALL
  DEPENDS ${libfile}
  SOURCES ${frp_source}
)

install (FILES ${libfile} DESTINATION ${CMAKE_INSTALL_PREFIX}/lib )

set (source 
  ${CMAKE_CURRENT_SOURCE_DIR}/b20forms.c
  ${CMAKE_CURRENT_SOURCE_DIR}/b20forms.h
  ${CMAKE_CURRENT_SOURCE_DIR}/binio.c 
  ${CMAKE_CURRENT_SOURCE_DIR}/binio.h
  ${CMAKE_CURRENT_SOURCE_DIR}/ctosio.c
  ${CMAKE_CURRENT_SOURCE_DIR}/ctosio.h
)
add_library (b20forms SHARED ${source})
target_link_libraries (b20forms ncurses)
install(TARGETS b20forms LIBRARY DESTINATION lib)

set (source
  ${CMAKE_CURRENT_SOURCE_DIR}/b20codes.c
)
add_executable (ctos_b20codes ${source})
target_link_libraries (ctos_b20codes ncurses)
install (TARGETS ctos_b20codes RUNTIME DESTINATION bin)

include_directories(${CTOS_BINARY_DIR}/c)
set (source
  ${CMAKE_CURRENT_SOURCE_DIR}/tester.c
  ${CMAKE_CURRENT_SOURCE_DIR}/tester.h
)
add_executable (ctos_tester ${source})
target_link_libraries (ctos_tester b20forms)

set (source
  ${CMAKE_CURRENT_SOURCE_DIR}/tester2.c
)
add_executable (ctos_tester2 ${source})
target_link_libraries (ctos_tester2 ncurses)

set (source
  ${CMAKE_CURRENT_SOURCE_DIR}/record.c
)
add_executable (Record ${source})
target_link_libraries (Record)
install (TARGETS Record RUNTIME DESTINATION bin)

set (source
  ${CMAKE_CURRENT_SOURCE_DIR}/submit.c
)
add_executable (Submit ${source})
target_link_libraries (Submit)
install (TARGETS Submit RUNTIME DESTINATION bin)

set (source
  ${CMAKE_CURRENT_SOURCE_DIR}/submit_popen.c
)
add_executable (submitp ${source})
target_link_libraries (submitp ncurses)
install (TARGETS submitp RUNTIME DESTINATION bin)

set (source
  ${CMAKE_CURRENT_SOURCE_DIR}/text2pdf.c
)
add_executable (text2pdf ${source})
target_link_libraries (text2pdf)
install (TARGETS text2pdf RUNTIME DESTINATION bin)

set (source
  ${CMAKE_CURRENT_SOURCE_DIR}/sox.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/sox.h
  ${CMAKE_CURRENT_SOURCE_DIR}/showpic.cpp
)
add_executable (showpic ${source})
target_link_libraries (showpic)
if (WIN32)
  target_link_libraries (showpic ws2_32 winmm)
endif ()  
install (TARGETS showpic RUNTIME DESTINATION bin)

set (source
  ${CMAKE_CURRENT_SOURCE_DIR}/header.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/header.h
  ${CMAKE_CURRENT_SOURCE_DIR}/getargs.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/getargs.h
)
add_executable (ctos_header ${source})
target_link_libraries (ctos_header ncurses)

set (cobcalls      ${CMAKE_CURRENT_SOURCE_DIR}/cobcalls.c)

add_custom_command (
  OUTPUT  ${CMAKE_BINARY_DIR}/cobol/cobcalls.o
  DEPENDS ${cobcalls}
  COMMAND gcc -g -fPIC -c ${cobcalls} -o ${CMAKE_BINARY_DIR}/cobol/cobcalls.o
)      

add_custom_target (ctos_objects ALL
  DEPENDS ${CMAKE_BINARY_DIR}/cobol/cobcalls.o b20forms 
  SOURCES ${cobcalls}
)

install (FILES ${CMAKE_BINARY_DIR}/cobol/cobcalls.o DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
