cmake_minimum_required(VERSION 3.13.2)
set(CMAKE_VERBOSE_MAKEFILE ON)
project(2dim_sic_simulator Fortran)

set(LIBRARY_DIR )

set(EXECUTABLE main)

list(APPEND CMAKE_MODULE_PATH ";${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# convert file paths including backslash to cmake-like paths
file(TO_CMAKE_PATH "$ENV{MKLROOT}" MKL_ROOT)

add_subdirectory(utils)
add_subdirectory(vectors)
add_subdirectory(halfedge)
add_subdirectory(field)
add_subdirectory(fft)
add_subdirectory(io)

add_executable(${EXECUTABLE}
    main.f90
    parameters.f90
    commons.f90
    efield.f90
    status.f90
    sic_types.f90
    simplex_methods.f90
    tracer_methods.f90
    sic.f90
    volume_integral.f90
    poisson.f90
    meshfile.f90
)

target_compile_options(${EXECUTABLE}
    PUBLIC -Ofast
)

find_package(OpenMP REQUIRED)

find_package(MKL REQUIRED)

include(FetchContent)
# Build json-fortran library
FetchContent_Declare(jsonfortran
    GIT_REPOSITORY https://github.com/jacobwilliams/json-fortran.git
)
FetchContent_MakeAvailable(jsonfortran)

target_link_libraries(${EXECUTABLE}
    PUBLIC
    OpenMP::OpenMP_Fortran
    MKL
    utils
    vectors
    halfedge
    field
    fft
    io
    jsonfortran-static
)