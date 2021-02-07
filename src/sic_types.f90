module sic_types
    use m_halfedge
    use m_vector2d
    use m_vector3d
    use m_scalar_field
    use m_vector_field
    implicit none

    private
    public t_Simplex
    public t_Tracer
    public sic_types_init

    type, extends(t_Vertex) :: t_Tracer
        integer :: ispec
        type(Vector3d) :: velocity

    contains

        procedure :: update => tracer_update

    end type

    interface t_Tracer
        module procedure init_tracer
    end interface

    type, extends(t_Face) :: t_Simplex
        integer :: ispec
        double precision :: npcl
        double precision :: q
        double precision :: m
        double precision :: q_m

        type(Vector3d) :: ef
        type(Vector3d) :: bf

    contains

        procedure :: scatter => simplex_scatter
        procedure :: gather_scalar => simplex_gather_scalar
        procedure :: gather_vector => simplex_gather_vector
        procedure :: area => simplex_area
        procedure :: position => simplex_position
        procedure :: velocity => simplex_velocity
        procedure :: energy => simplex_energy

    end type

    interface t_Simplex
        module procedure init_simplex
    end interface

    ! Interface for tracer
    interface
        module function init_tracer(ispec, position, velocity) result(obj)
            integer, intent(in) :: ispec
            type(Vector2d), intent(in), optional :: position
            type(Vector3d), intent(in), optional :: velocity
            type(t_Tracer), target :: obj
        end function

        module subroutine tracer_update(self, dt)
            class(t_Tracer) :: self
            double precision, intent(in) :: dt
        end subroutine
    end interface

    ! Interface for simplex
    interface
        module function init_simplex(ispec, npcl, q, m) result(obj)
            type(t_Simplex), target :: obj
            integer, intent(in) :: ispec
            double precision, intent(in) :: npcl
            double precision, intent(in) :: q
            double precision, intent(in) :: m
        end function
        module subroutine simplex_scatter(self, moment, field, offsets)
            class(t_Simplex) :: self
            double precision, intent(in) :: moment
            type(t_ScalarField), intent(inout) :: field
            double precision, intent(in) :: offsets(2)
        end subroutine
        
        module function simplex_gather_scalar(self, field, offsets)
            class(t_Simplex) :: self
            type(t_ScalarField), intent(inout) :: field
            double precision, intent(in) :: offsets(2)
            double precision :: simplex_gather_scalar
        end function

        module function simplex_gather_vector(self, field, offsets)
            class(t_Simplex) :: self
            type(t_VectorField), intent(inout) :: field
            double precision, intent(in) :: offsets(2)
            type(Vector3d) :: simplex_gather_vector
        end function

        module function simplex_area(self)
            class(t_Simplex) :: self
            double precision :: simplex_area
        end function

        module function simplex_position(self)
            class(t_Simplex) :: self
            type(Vector2d) :: simplex_position
        end function

        module function simplex_velocity(self)
            class(t_Simplex) :: self
            type(Vector3d) :: simplex_velocity
        end function

        module function simplex_energy(self)
            class(t_Simplex) :: self
            double precision :: simplex_energy
        end function
    end interface

    type(t_Tracer), pointer :: tracers(:)
    type(t_Simplex), pointer :: simplicies(:)

contains

    subroutine sic_types_init(ptracers, psimplices)
        class(t_Tracer), target :: ptracers(:)
        class(t_Simplex), target :: psimplices(:)

        tracers => ptracers
        simplicies => psimplices
    end subroutine

end module
