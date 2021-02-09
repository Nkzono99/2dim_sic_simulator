module m_vector_field
    use m_vector3d
    use m_field
    use mymath
    implicit none

    private

    public t_VectorField

    type, extends(t_Field) :: t_VectorField

        type(Vector3d), allocatable :: values(:, :)

    contains

        procedure :: set => vectorfield_set
        procedure :: get_i => vectorField_iget
        procedure :: get_r => vectorField_rget
        generic :: get => get_i, get_r
        procedure :: add => vectorField_add
        procedure :: mul => vectorField_mul

    end type

    interface t_VectorField
        module procedure init_VectorField
    end interface

contains

    function init_VectorField(nx, ny, gridwidth) result(obj)
        type(t_VectorField) :: obj
        integer, intent(in) :: nx
        integer, intent(in) :: ny
        double precision, intent(in) :: gridwidth

        obj%nx = nx
        obj%ny = ny
        obj%gridwidth = gridwidth

        allocate (obj%values(-1:nx + 1, -1:ny + 1))
    end function

    subroutine vectorField_set(self, x, y, val)
        class(t_VectorField), intent(out) :: self
        integer, intent(in) :: x
        integer, intent(in) :: y
        type(Vector3d), intent(in) :: val

        integer :: i, j

        i = mymath_pmod(x, self%nx)
        j = mymath_pmod(y, self%ny)

        self%values(i, j) = val
    end subroutine

    function vectorField_iget(self, x, y) result(val)
        class(t_VectorField), intent(in) :: self
        integer, intent(in) :: x
        integer, intent(in) :: y
        type(Vector3d) :: val

        integer :: i, j

        i = mymath_pmod(x, self%nx)
        j = mymath_pmod(y, self%ny)

        val = self%values(i, j)
    end function

    function vectorField_rget(self, x, y) result(val)
        class(t_VectorField), intent(in) :: self
        double precision, intent(in) :: x
        double precision, intent(in) :: y
        type(Vector3d) :: val

        integer :: i, j
        double precision :: lx, ly
        type(Vector3d) :: v11, v12, v21, v22

        i = int(x/self%gridwidth)
        j = int(y/self%gridwidth)
        lx = x - i
        ly = y - j

        v11 = vectorField_iget(self, i, j)
        v21 = vectorField_iget(self, i + 1, j)
        v12 = vectorField_iget(self, i, j + 1)
        v22 = vectorField_iget(self, i + 1, j + 1)

        val = v11*(1.0d0 - lx)*(1.0d0 - ly) &
              + v21*lx*(1.0d0 - ly) &
              + v12*(1.0d0 - lx)*ly &
              + v22*lx*ly
    end function

    subroutine vectorField_add(self, x, y, val)
        class(t_VectorField), intent(inout) :: self
        integer, intent(in) :: x
        integer, intent(in) :: y
        type(Vector3d), intent(in) :: val

        integer :: i, j

        i = mymath_pmod(x, self%nx)
        j = mymath_pmod(y, self%ny)

        self%values(i, j) = self%values(i, j) + val
    end subroutine

    subroutine vectorField_mul(self, x, y, val)
        class(t_VectorField), intent(inout) :: self
        integer, intent(in) :: x
        integer, intent(in) :: y
        type(Vector3d), intent(in) :: val

        integer :: i, j

        i = mymath_pmod(x, self%nx)
        j = mymath_pmod(y, self%ny)

        self%values(i, j) = self%values(i, j)*val
    end subroutine

end module
