function(findmkllib retlib libname)
    find_library(${retlib} ${libname}
        PATHS
            ENV MKLROOT
            ${MKL_ROOT}
            ${MKLROOT}
        PATH_SUFFIXES
            lib/intel64
    )

    # set(${retlib} ${libbuf} PARENT_SCOPE)
endfunction()


file(TO_CMAKE_PATH "$ENV{MKLROOT}" MKL_ROOT)

find_path(MKL_INCLUDE_DIR
    NAMES
        lapack.f90
    PATHS
        ENV MKLROOT
        ${MKL_ROOT}
        ${MKLROOT}
    PATH_SUFFIXES
        include
)

findmkllib(MKL_LP64 mkl_intel_lp64.lib)
findmkllib(MKL_CORE mkl_core.lib)
findmkllib(MKL_THREAD mkl_intel_thread.lib)
findmkllib(MKL_LAPACK mkl_lapack95_lp64.lib)
findmkllib(MKL_DFT mkl_cdft_core.lib)

set(MKL_LIBRARIES
    ${MKL_LAPACK}
    ${MKL_DFT}
    ${MKL_CORE}
    ${MKL_LP64}
    ${MKL_THREAD}
)

mark_as_advanced(
    MKL_INCLUDE_DIR
    MKL_LIBRARIES
)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(MKL
    REQUIRED_VARS
        MKL_INCLUDE_DIR
        MKL_LIBRARIES
)

if (MKL_FOUND AND NOT TARGET MKL)
    add_library(MKL STATIC)

    target_link_libraries(MKL
        ${MKL_LIBRARIES}
        libiomp5md.lib
    )

    target_sources(MKL
        PRIVATE
        ${MKL_INCLUDE_DIR}/lapack.f90
        ${MKL_INCLUDE_DIR}/mkl_dfti.f90
    )
endif()