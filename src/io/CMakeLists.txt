set(Module io)

add_library(${Module}
    STATIC
    csv.f90
    progressbar.f90
)

target_link_libraries(${Module}
    utils
)