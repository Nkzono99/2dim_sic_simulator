set(Modules fft)

add_library(${Modules}
    STATIC
    fft.f90
    intel_fft.f90
)

find_package(MKL REQUIRED)

target_link_libraries(${Modules}
    MKL
)