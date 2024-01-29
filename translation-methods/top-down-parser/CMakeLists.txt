cmake_minimum_required(VERSION 3.26)
project(top_down_parser)

set(CMAKE_CXX_STANDARD 20)

set(COMMON_INCLUDES top_down_parser/include)
include_directories(${COMMON_INCLUDES})


add_executable(top_down_parser
        src/main.cpp
        src/grammar.cpp
        src/lexAn.cpp
        src/tree.cpp
        src/parser.cpp
)
