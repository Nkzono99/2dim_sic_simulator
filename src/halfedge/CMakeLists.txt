set(Modules halfedge)

add_library(${Modules}
    STATIC
    halfedge.f90
    hfiter.f90
    edgedict.f90
)

target_link_libraries(${Modules}
    vectors
    utils
)