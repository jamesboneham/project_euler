cmake_minimum_required(VERSION 3.14)

# Set the source binary and library dirs
set(maindir main)
set(probsdir probs)
set(utilsdir utils)
set(mpfundir mpmodule)
# set(CMAKE_Fortran_MODULE_DIRECTORY "${probsdir}")



# Set the project name
set(PROJECT_NAME ProjectEulerFortran)
project(${PROJECT_NAME} LANGUAGES Fortran VERSION 0.0)

# add problem library directory
 add_subdirectory("${probsdir}")

# add multiple precision library directory
 add_subdirectory("${mpfundir}")

# add utilities library directory
 add_subdirectory("${utilsdir}")

# add the executable
add_executable(${PROJECT_NAME} "${maindir}/main.f90")

find_package(fortran_stdlib REQUIRED)

target_link_libraries(${PROJECT_NAME}
                      PUBLIC probs
                      PUBLIC utils
                      )

target_include_directories(${PROJECT_NAME} PUBLIC
                           "${ProjectEulerFortran_BINARY_DIR}"
                           "${ProjectEulerFortran_BINARY_DIR}/${probsdir}"
                           "${ProjectEulerFortran_BINARY_DIR}/${utilsdir}"
                           )
