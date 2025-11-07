# CMake configuration examples

# Minimum required version
cmake_minimum_required(VERSION 3.15)

# Project definition
project(MyProject
    VERSION 1.0.0
    DESCRIPTION "Example CMake project"
    LANGUAGES C CXX
)

# C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

# Output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Options
option(BUILD_SHARED_LIBS "Build shared libraries" ON)
option(BUILD_TESTS "Build test programs" ON)
option(BUILD_DOCS "Build documentation" OFF)
option(ENABLE_WARNINGS "Enable compiler warnings" ON)

# Variables
set(PROJECT_SOURCES
    src/main.cpp
    src/utils.cpp
    src/config.cpp
)

set(PROJECT_HEADERS
    include/utils.h
    include/config.h
)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_BINARY_DIR}/include
)

# Compiler flags
if(ENABLE_WARNINGS)
    if(MSVC)
        add_compile_options(/W4 /WX)
    else()
        add_compile_options(-Wall -Wextra -Wpedantic -Werror)
    endif()
endif()

# Debug flags
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -O0 -DDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -DNDEBUG")

# Find packages
find_package(Threads REQUIRED)
find_package(OpenSSL REQUIRED)
find_package(Boost 1.70 REQUIRED COMPONENTS system filesystem)

# Check for system features
include(CheckIncludeFile)
include(CheckFunctionExists)
include(CheckCXXSymbolExists)

check_include_file(unistd.h HAVE_UNISTD_H)
check_function_exists(strdup HAVE_STRDUP)
check_cxx_symbol_exists(std::filesystem::path "filesystem" HAVE_STD_FILESYSTEM)

# Configure header file
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/config.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/include/config.h
)

# Add library
add_library(mylib
    src/lib/libutils.cpp
    src/lib/libconfig.cpp
)

target_include_directories(mylib PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

target_link_libraries(mylib PUBLIC
    Threads::Threads
    OpenSSL::SSL
    OpenSSL::Crypto
    Boost::system
    Boost::filesystem
)

# Add executable
add_executable(myapp ${PROJECT_SOURCES})

target_link_libraries(myapp PRIVATE mylib)

# Set target properties
set_target_properties(myapp PROPERTIES
    OUTPUT_NAME "my-application"
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# Add compile definitions
target_compile_definitions(myapp PRIVATE
    APP_VERSION="${PROJECT_VERSION}"
    $<$<CONFIG:Debug>:DEBUG_MODE>
)

# Conditional compilation
if(UNIX)
    target_compile_definitions(myapp PRIVATE UNIX_SYSTEM)
elseif(WIN32)
    target_compile_definitions(myapp PRIVATE WINDOWS_SYSTEM)
endif()

# Custom targets
add_custom_target(format
    COMMAND clang-format -i ${PROJECT_SOURCES} ${PROJECT_HEADERS}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Formatting source code"
)

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/version.txt
    COMMAND ${CMAKE_COMMAND} -E echo ${PROJECT_VERSION} > version.txt
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating version file"
)

# Testing
if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)

    # Add test
    add_executable(test_mylib tests/test_mylib.cpp)
    target_link_libraries(test_mylib PRIVATE mylib)
    add_test(NAME LibraryTest COMMAND test_mylib)

    # CTest configuration
    include(CTest)
endif()

# Documentation
if(BUILD_DOCS)
    find_package(Doxygen)
    if(DOXYGEN_FOUND)
        set(DOXYGEN_IN ${CMAKE_CURRENT_SOURCE_DIR}/docs/Doxyfile.in)
        set(DOXYGEN_OUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)

        configure_file(${DOXYGEN_IN} ${DOXYGEN_OUT} @ONLY)

        add_custom_target(docs
            COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYGEN_OUT}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT "Generating API documentation with Doxygen"
            VERBATIM
        )
    endif()
endif()

# Subdirectories
add_subdirectory(src)
add_subdirectory(external)

# Install rules
install(TARGETS myapp mylib
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

install(FILES ${PROJECT_HEADERS}
    DESTINATION include/myproject
)

install(FILES README.md LICENSE
    DESTINATION share/doc/myproject
)

# Package configuration
include(CMakePackageConfigHelpers)

configure_package_config_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/cmake/MyProjectConfig.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/MyProjectConfig.cmake
    INSTALL_DESTINATION lib/cmake/MyProject
)

write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/MyProjectConfigVersion.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
)

install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/MyProjectConfig.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/MyProjectConfigVersion.cmake
    DESTINATION lib/cmake/MyProject
)

# CPack configuration
set(CPACK_PACKAGE_NAME ${PROJECT_NAME})
set(CPACK_PACKAGE_VERSION ${PROJECT_VERSION})
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY ${PROJECT_DESCRIPTION})
set(CPACK_PACKAGE_VENDOR "Example Vendor")
set(CPACK_PACKAGE_CONTACT "admin@example.com")
set(CPACK_GENERATOR "TGZ;ZIP")

include(CPack)

# Print configuration summary
message(STATUS "")
message(STATUS "Configuration Summary:")
message(STATUS "  Project: ${PROJECT_NAME} ${PROJECT_VERSION}")
message(STATUS "  Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  C++ standard: ${CMAKE_CXX_STANDARD}")
message(STATUS "  Install prefix: ${CMAKE_INSTALL_PREFIX}")
message(STATUS "  Build shared libs: ${BUILD_SHARED_LIBS}")
message(STATUS "  Build tests: ${BUILD_TESTS}")
message(STATUS "  Build docs: ${BUILD_DOCS}")
message(STATUS "")

# Functions and macros
function(add_example name)
    add_executable(example_${name} examples/${name}.cpp)
    target_link_libraries(example_${name} PRIVATE mylib)
    install(TARGETS example_${name} DESTINATION bin/examples)
endfunction()

macro(set_default_warnings target)
    if(MSVC)
        target_compile_options(${target} PRIVATE /W4)
    else()
        target_compile_options(${target} PRIVATE -Wall -Wextra)
    endif()
endmacro()

# Usage
add_example(basic)
add_example(advanced)
set_default_warnings(myapp)

# Generator expressions
target_compile_options(myapp PRIVATE
    $<$<CXX_COMPILER_ID:GNU>:-fno-rtti>
    $<$<CXX_COMPILER_ID:Clang>:-fno-rtti>
    $<$<CXX_COMPILER_ID:MSVC>:/GR->
)

# FetchContent for external dependencies
include(FetchContent)

FetchContent_Declare(
    fmt
    GIT_REPOSITORY https://github.com/fmtlib/fmt.git
    GIT_TAG 9.1.0
)

FetchContent_MakeAvailable(fmt)

target_link_libraries(myapp PRIVATE fmt::fmt)

# ExternalProject for complex dependencies
include(ExternalProject)

ExternalProject_Add(
    json
    URL https://github.com/nlohmann/json/releases/download/v3.11.2/json.tar.xz
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ${CMAKE_COMMAND} -E copy_directory
        <SOURCE_DIR>/include
        ${CMAKE_BINARY_DIR}/include
)

# Platform-specific settings
if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    target_link_libraries(myapp PRIVATE dl)
elseif(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    target_link_libraries(myapp PRIVATE "-framework CoreFoundation")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Windows")
    target_link_libraries(myapp PRIVATE ws2_32)
endif()

# Features
include(CheckCXXCompilerFlag)

check_cxx_compiler_flag(-std=c++20 HAVE_CXX20)
if(HAVE_CXX20)
    message(STATUS "C++20 support detected")
endif()
