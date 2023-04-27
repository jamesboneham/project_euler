# Problem library

set(libname probs)
add_library("${libname}" probs.f90 probs01to10.f90 probs11to20.f90)
set_target_properties("${libname}"
    PROPERTIES Fortran_MODULE_DIRECTORY "${ProjectEulerFortran_BINARY_DIR}/probs")

target_link_libraries("${libname}"
                      PRIVATE mpmodule
                      PUBLIC utils
                      )

target_include_directories("${libname}"
                           PRIVATE "${ProjectEulerFortran_BINARY_DIR}/mpmodule"
                           PRIVATE "${ProjectEulerFortran_BINARY_DIR}/utils"
                           )
