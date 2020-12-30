submodule(sic_types) tracer_methods
    implicit none

contains
    !> Particle Constructor.
    function init_tracer(position, velocity) result(obj)
        type(Vector3d), intent(in), optional :: position, velocity
        type(Tracer), target :: obj

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

    subroutine tracer_update(self, dt)
        class(Tracer) :: self
        real(8), intent(in) :: dt

        integer :: isimp
        type(Vector3d) :: e, b

        do isimp = 1, 3
            e = e + self%simplex(isimp)%E/3.0d0
            b = b + self%simplex(isimp)%B/3.0d0
        end do

        call update_velocity(self, e, b, dt)

        call update_position(self, dt)
    end subroutine

    subroutine update_position(trc, dt)
        type(Tracer), intent(out) :: trc
        real(8), intent(in) :: dt

        trc%position = trc%position + trc%velocity*dt
    end subroutine

    !> Update velocity by Beuneman-Boris method.
    subroutine update_velocity(trc, e, b, dt)
        type(Tracer), intent(inout) :: trc
        type(Vector3d), intent(in) :: e, b
        real(8), intent(in) :: dt

        real(8) :: dt_half
        type(Vector3d) :: t, s, vm, va, vp

        dt_half = 0.5d0*dt

        t = b*dt_half
        s = 2.0d0*t/(1.0d0 + t%norm2())

        vm = trc%velocity + e*dt_half
        va = vm + vm%cross(t)
        vp = vm + va%cross(s)
        trc%velocity = vp + e*dt_half
    end subroutine
end submodule
