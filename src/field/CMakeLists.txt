set(Modules field)

add_library(${Modules}
    STATIC
    field.f90
    scalar_field.f90
    vector_field.f90
)

target_link_libraries(${Modules}
    vectors
    utils
)