module commons
    use parameters
    use m_vectors
    use sic_types, only: t_Tracer, t_Simplex
    use m_scalar_field
    use m_vector_field
    use m_halfedge, only: t_Halfedge

    implicit none

    type(t_ScalarField), target :: phi, rho
    type(t_VectorField), target :: ef, bf

    type(t_Tracer), allocatable, target :: tracers(:)
    type(t_Simplex), allocatable, target :: simplicies(:)

contains

    subroutine commons_init
        ef = t_VectorField(nx, ny, dx)
        bf = t_VectorField(nx, ny, dx)
        rho = t_ScalarField(nx, ny, dx)
        phi = t_ScalarField(nx, ny, dx)
        allocate(tracers(max_npcl))
        allocate(simplicies(max_npcl))
    end subroutine

end module