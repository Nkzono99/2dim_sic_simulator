submodule(sic_types) tracer_methods
    use m_hfiter
    implicit none

contains
    !> Particle Constructor.
    module function init_tracer(ispec, position, velocity) result(obj)
        integer, intent(in) :: ispec
        type(Vector2d), intent(in), optional :: position
        type(Vector3d), intent(in), optional :: velocity
        type(t_Tracer), target :: obj

        obj%ispec = ispec

        if (present(position)) then
            obj%position = position
        else
            obj%position = 0.0d0
        end if

        if (present(velocity)) then
            obj%velocity = velocity
        else
            obj%velocity = 0.0d0
        end if
    end function

    module subroutine tracer_update(self, dt)
        class(t_Tracer) :: self
        double precision, intent(in) :: dt

        type(t_Simplex), pointer :: simp
        integer :: isimp
        integer :: is
        type(Vector3d) :: e_sum, b_sum
        double precision :: q_m

        type(t_FaceAroundVertexIter) :: it

        logical :: hasnext

        it = t_FaceAroundVertexIter(self%iedge)
        e_sum = 0.0d0
        b_sum = 0.0d0
        do while (.true.)
            isimp = it%next()
            simp => simplicies(isimp)
            e_sum = e_sum + simp%ef * simp%q_m
            b_sum = b_sum + simp%bf

            if (.not. it%hasnext()) then
                exit
            end if
        end do

        ! print *, e_sum, q_m, q_m * e_sum

        call update_velocity(self, e_sum, b_sum, dt)

        call update_position(self, dt)
    end subroutine

    !> Update velocity by Beuneman-Boris method.
    subroutine update_velocity(trc, e, b, dt)
        type(t_Tracer), intent(inout) :: trc
        type(Vector3d), intent(in) :: e, b
        double precision, intent(in) :: dt

        double precision :: dt_half
        type(Vector3d) :: t, s, vm, va, vp

        dt_half = 0.5d0*dt

        t = b*dt_half
        s = 2.0d0*t/(1.0d0 + t%norm2())

        vm = trc%velocity + e*dt_half
        va = vm + vm%cross(t)
        vp = vm + va%cross(s)
        trc%velocity = vp + e*dt_half
    end subroutine

    subroutine update_position(trc, dt)
        type(t_Tracer), intent(inout) :: trc
        double precision, intent(in) :: dt

        trc%position%x = trc%position%x + trc%velocity%x*dt
        trc%position%y = trc%position%y + trc%velocity%y*dt
    end subroutine
end submodule
