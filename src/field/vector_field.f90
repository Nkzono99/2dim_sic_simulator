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
        procedure :: get => vectorfield_get
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

        allocate (obj%values(-1:nx+1, -1:ny+1))
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

    function vectorField_get(self, x, y) result(val)
        class(t_VectorField), intent(in) :: self
        integer, intent(in) :: x
        integer, intent(in) :: y
        type(Vector3d) :: val

        integer :: i, j

        i = mymath_pmod(x, self%nx)
        j = mymath_pmod(y, self%ny)

        val = self%values(i, j)
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
