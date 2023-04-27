# Utilities library
set(libname utils)
add_library("${libname}" utilsmodule.f90 timeitmodule.f90 bigintmodule.f90 primesmodule.f90)
set_target_properties("${libname}"
    PROPERTIES Fortran_MODULE_DIRECTORY "${ProjectEulerFortran_BINARY_DIR}/${libname}")

target_link_libraries("${libname}"
                      PUBLIC mpmodule
                      )
target_include_directories("${libname}"
                           PUBLIC "${ProjectEulerFortran_BINARY_DIR}/mpmodule"
                           )

# # Add Multiple Precision Library
# find_library(mplib
#              NAMES "libmpmodule.a"
#              PATHS "${ProjectEulerFortran_BINARY_DIR}/mpmodule")
# find_path(mpdir
#           NAMES "libmpmodule.a"
#           PATHS "${ProjectEulerFortran_BINARY_DIR}/mpmodule")

# target_link_libraries("${libname}"
#                       PRIVATE "${mplib}"
#                       )
# target_include_directories("${libname}"
#                            PRIVATE "${mpdir}"
#                            )
