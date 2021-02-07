submodule(sic_types) simplex_methods
    use volume_integral
    use m_hfiter
    implicit none

contains

    module function init_simplex(ispec, npcl, q, m) result(obj)
        type(t_Simplex), target :: obj
        integer, intent(in) :: ispec
        double precision, intent(in) :: npcl
        double precision, intent(in) :: q
        double precision, intent(in) :: m

        obj%ispec = ispec
        obj%npcl = npcl
        obj%q = q
        obj%m = m
        obj%q_m = q/m
    end function

    module subroutine simplex_scatter(self, moment, field, offsets)
        class(t_Simplex) :: self
        double precision, intent(in) :: moment
        type(t_ScalarField), intent(inout) :: field
        double precision, intent(in) :: offsets(2)

        integer i, j
        type(t_Plane) :: planes(3)
        type(t_Poly) :: poly

        type(Vector2d) :: v1, v2, v3
        type(Vector2d) :: v12, v23
        double precision :: v123

        double precision :: moment_per_area
        integer :: imin, imax, jmin, jmax
        double precision :: dx

        type(t_VertexAroundFaceIter) :: viter

        viter = t_VertexAroundFaceIter(self%iedge)

        v1 = tracers(viter%next())%position
        v2 = tracers(viter%next())%position
        v3 = tracers(viter%next())%position

        v12 = v2 - v1
        v23 = v3 - v2
        v123 = v12%cross(v23)

        ! 反時計回りに三角形を形成する.
        if (v123 >= 0) then
            planes = triangle(Vector2d(v1%x, v1%y), Vector2d(v2%x, v2%y), Vector2d(v3%x, v3%y))
        else
            planes = triangle(Vector2d(v1%x, v1%y), Vector2d(v3%x, v3%y), Vector2d(v2%x, v2%y))
        end if

        dx = field%gridwidth
        imin = int(min(v1%x, min(v2%x, v3%x))/dx + offsets(1)/dx)
        imax = int(max(v1%x, max(v2%x, v3%x))/dx + offsets(1)/dx) + 1
        jmin = int(min(v1%y, min(v2%y, v3%y))/dx + offsets(2)/dx)
        jmax = int(max(v1%y, max(v2%y, v3%y))/dx + offsets(2)/dx) + 1

        moment_per_area = moment/self%area()
        do i = imin, imax
            do j = jmin, jmax
                poly = gridbox(Vector2d(i*dx + offsets(1), j*dx + offsets(2)), dx)

                call poly%clip(planes)
                call field%add(i, j, moment_per_area*poly%area())
            end do
        end do
    end subroutine

    module function simplex_gather_vector(self, field, offsets)
        class(t_Simplex) :: self
        type(t_VectorField), intent(inout) :: field
        double precision, intent(in) :: offsets(2)
        type(Vector3d) :: simplex_gather_vector

        integer i, j
        type(t_Plane) :: planes(3)
        type(t_Poly) :: poly

        type(Vector2d) :: v1, v2, v3
        type(Vector2d) :: v12, v23
        double precision :: v123
        double precision :: dx

        integer :: imin, imax, jmin, jmax

        type(t_VertexAroundFaceIter) :: viter

        viter = t_VertexAroundFaceIter(self%iedge)

        v1 = tracers(viter%next())%position
        v2 = tracers(viter%next())%position
        v3 = tracers(viter%next())%position

        if (self%area() == 0) then
            i = int((v1%x + offsets(1))/dx)
            j = int((v1%y + offsets(2))/dx)
            simplex_gather_vector = field%get(i, j)
            return
        end if

        ! 反時計回りに三角形を形成する.
        v12 = v2 - v1
        v23 = v3 - v2
        v123 = v12%cross(v23)
        if (v123 >= 0) then
            planes = triangle(Vector2d(v1%x, v1%y), Vector2d(v2%x, v2%y), Vector2d(v3%x, v3%y))
        else
            planes = triangle(Vector2d(v1%x, v1%y), Vector2d(v2%x, v2%y), Vector2d(v3%x, v3%y))
        end if

        dx = field%gridwidth
        imin = int(min(v1%x, min(v2%x, v3%x))/dx + offsets(0)/dx)
        imax = int(max(v1%x, max(v2%x, v3%x))/dx + offsets(0)/dx) + 1
        jmin = int(min(v1%y, min(v2%y, v3%y))/dx + offsets(1)/dx)
        jmax = int(max(v1%y, max(v2%y, v3%y))/dx + offsets(1)/dx) + 1

        simplex_gather_vector = 0.0d0
        do i = imin, imax
            do j = jmin, jmax
                poly = gridbox(Vector2d(i*dx + offsets(1), j*dx + offsets(2)), dx)
                call poly%clip(planes)
                simplex_gather_vector = simplex_gather_vector + field%get(i, j)*poly%area()
            end do
        end do
        simplex_gather_vector = simplex_gather_vector/self%area()
    end function

    module function simplex_gather_scalar(self, field, offsets)
        class(t_Simplex) :: self
        type(t_ScalarField), intent(inout) :: field
        double precision, intent(in) :: offsets(2)
        double precision :: simplex_gather_scalar

        integer i, j
        type(t_Plane) :: planes(3)
        type(t_Poly) :: poly

        type(Vector2d) :: v1, v2, v3
        type(Vector2d) :: v12, v23
        double precision :: v123
        double precision :: dx

        integer :: imin, imax, jmin, jmax

        type(t_VertexAroundFaceIter) :: viter

        viter = t_VertexAroundFaceIter(self%iedge)

        v1 = tracers(viter%next())%position
        v2 = tracers(viter%next())%position
        v3 = tracers(viter%next())%position

        v12 = v2 - v1
        v23 = v3 - v2
        v123 = v12%cross(v23)

        ! 反時計回りに三角形を形成する.
        if (v123 >= 0) then
            planes = triangle(Vector2d(v1%x, v1%y), Vector2d(v2%x, v2%y), Vector2d(v3%x, v3%y))
        else
            planes = triangle(Vector2d(v1%x, v1%y), Vector2d(v2%x, v2%y), Vector2d(v3%x, v3%y))
        end if

        dx = field%gridwidth
        imin = int(min(v1%x, min(v2%x, v3%x))/dx + 0.5d0)
        imax = int(max(v1%x, max(v2%x, v3%x))/dx + 0.5d0) + 1
        jmin = int(min(v1%y, min(v2%y, v3%y))/dx + 0.5d0)
        jmax = int(max(v1%y, max(v2%y, v3%y))/dx + 0.5d0) + 1

        simplex_gather_scalar = 0
        do i = imin, imax
            do j = jmin, jmax
                poly = gridbox(Vector2d(i*dx + offsets(1), j*dx + offsets(2)), dx)
                call poly%clip(planes)
                simplex_gather_scalar = simplex_gather_scalar + field%get(i, j)*poly%area()
            end do
        end do
        simplex_gather_scalar = simplex_gather_scalar/self%area()
    end function

    module function simplex_area(self)
        class(t_Simplex) :: self
        double precision :: simplex_area
        type(Vector2d) :: v1, v2, v3

        type(t_VertexAroundFaceIter) :: viter

        viter = t_VertexAroundFaceIter(self%iedge)

        v1 = tracers(viter%next())%position
        v2 = tracers(viter%next())%position
        v3 = tracers(viter%next())%position
        simplex_area = 0.5*((v1%x - v3%x)*(v2%y - v3%y) - (v2%x - v3%x)*(v1%y - v3%y))
        simplex_area = abs(simplex_area)
    end function

    module function simplex_position(self)
        class(t_Simplex) :: self
        type(Vector2d) :: simplex_position
        type(Vector2d) :: pos1, pos2, pos3

        type(t_VertexAroundFaceIter) :: viter

        viter = t_VertexAroundFaceIter(self%iedge)

        pos1 = tracers(viter%next())%position
        pos2 = tracers(viter%next())%position
        pos3 = tracers(viter%next())%position

        simplex_position = (pos1 + pos2 + pos3)/3.0d0
    end function

    module function simplex_velocity(self)
        class(t_Simplex) :: self
        type(Vector3d) :: simplex_velocity
        type(Vector3d) :: vel1, vel2, vel3

        type(t_VertexAroundFaceIter) :: viter

        viter = t_VertexAroundFaceIter(self%iedge)

        vel1 = tracers(viter%next())%velocity
        vel2 = tracers(viter%next())%velocity
        vel3 = tracers(viter%next())%velocity

        simplex_velocity = (vel1 + vel2 + vel3)/3.0d0
    end function

    module function simplex_energy(self)
        class(t_Simplex) :: self
        double precision :: simplex_energy

        type(Vector3d) :: vel1, vel2, vel3
        type(Vector3d) :: mean_vel

        type(t_VertexAroundFaceIter) :: viter

        double precision :: v1x, v2x, v3x
        double precision :: v1y, v2y, v3y

        viter = t_VertexAroundFaceIter(self%iedge)

        vel1 = tracers(viter%next())%velocity
        vel2 = tracers(viter%next())%velocity
        vel3 = tracers(viter%next())%velocity

        ! Simplexの速度が線形補間により定義されている場合
        ! simplex_energy = vel1%x*vel1%x &
        !                  + vel2%x*vel2%x &
        !                  + vel3%x*vel3%x &
        !                  + vel1%x*vel2%x &
        !                  + vel2%x*vel3%x &
        !                  + vel3%x*vel1%x &
        !                  + vel1%y*vel1%y &
        !                  + vel2%y*vel2%y &
        !                  + vel3%y*vel3%y &
        !                  + vel1%y*vel2%y &
        !                  + vel2%y*vel3%y &
        !                  + vel3%y*vel1%y

        ! Simplexの速度が一様である場合
        mean_vel = (vel1 + vel2 + vel3) / 3.0d0
        simplex_energy = mean_vel%norm2()

        simplex_energy = 0.5d0*self%m*simplex_energy
    end function

end submodule
