module m_field
    implicit none

    private
    public t_Field

    ! Number of grid points : (nx + 1) * (ny + 1)
    ! Number of cells : nx * ny
    ! field%values(0:nx, 0:ny)
    ! ( 0,  0) -------- (nx,  0)
    ! |                        |
    ! |                        |
    ! |                        |
    ! |                        |
    ! |                        |
    ! ( 0, ny) -------- (nx, ny)
    type t_Field
        integer :: nx
        integer :: ny

        double precision :: gridwidth
    end type

end module
