module sic_types
    use vectors
    implicit none

    type Tracer

        type(Vector3d) :: position
        type(Vector3d) :: velocity

        type(Simplex), pointer :: simplex(:)

    contains

        procedure :: update => tracer_update

    end type

    interface Tracer
        module procedure init_tracer
    end interface

    type Simplex

        type(Tracer), pointer :: tracer(:)
        real(8) :: rate

        type(Vector3d) :: E
        type(Vector3d) :: B

    contains

        procedure :: scatter => simplex_scatter
        procedure :: gather => simplex_gather

    end type

    interface Simplex
        module procedure init_simplex
    end interface

    ! Interface for tracer
    interface
        module function init_tracer(position, velocity) result(obj)
            type(Vector3d), intent(in), optional :: position, velocity
            type(Tracer), target :: obj
        end function

        module subroutine tracer_update(self, dt)
            class(Tracer) :: self
            real(8), intent(in) :: dt
        end subroutine
    end interface

    ! Interface for simplex
    interface
        module function init_simplex(rate) result(obj)
            type(Simplex), target :: obj
            real(8), intent(in) :: rate
        end function

        module subroutine simplex_scatter(self)
            class(Simplex) :: self
        end subroutine

        module subroutine simplex_gather(self)
            class(Simplex) :: self
        end subroutine
    end interface

end module
