# Multiple precision library

set(libname mpmodule)

# Set compile flags for source files
set_source_files_properties(mpfuna.f90 mpfunb.f90 mpfunc.f90 mpfund.f90 mpfune.f90
    mpfunf.f90 mpfung1.f90 mpfunh1.f90 mpmodule.f90
    PROPERTIES COMPILE_OPTIONS "-Ofast")
set_source_files_properties(mpmask13.f90 second.f90
    PROPERTIES COMPILE_OPTIONS "-O3")

add_library("${libname}" mpfuna.f90 mpfunb.f90 mpfunc.f90 mpfund.f90
    mpfune.f90 mpfunf.f90 mpfung1.f90 mpfunh1.f90 mpmodule.f90
    mpmask13.f90 second.f90)
set_target_properties("${libname}"
    PROPERTIES Fortran_MODULE_DIRECTORY "${ProjectEulerFortran_BINARY_DIR}/${libname}")
