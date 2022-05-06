project (tools_coco)

set (source
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Action.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/ArrayList.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/BitArray.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/CharClass.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/CharSet.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Coco.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Comment.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/DFA.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Generator.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/HashTable.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Melted.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Node.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Parser.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/ParserGen.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Position.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Scanner.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/SortedList.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/State.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/StringBuilder.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Symbol.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Tab.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Target.cpp
)

add_executable (coco ${source})
