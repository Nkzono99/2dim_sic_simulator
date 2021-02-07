module m_scalar_field
    use m_field
    use mymath

    implicit none

    private

    public t_ScalarField

    type, extends(t_Field) :: t_ScalarField

        double precision, allocatable :: values(:, :)

    contains

        procedure :: set => scalarField_set
        procedure :: get => scalarField_get
        procedure :: add => scalarField_add
        procedure :: mul => scalarField_mul

    end type

    interface t_ScalarField
        module procedure init_ScalarField
    end interface

contains

    function init_ScalarField(nx, ny, gridwidth) result(obj)
        type(t_ScalarField) :: obj
        integer, intent(in) :: nx
        integer, intent(in) :: ny
        double precision, intent(in) :: gridwidth

        obj%nx = nx
        obj%ny = ny
        obj%gridwidth = gridwidth

        allocate (obj%values(-1:nx+1, -1:ny+1))
    end function

    subroutine scalarField_set(self, x, y, val)
        class(t_ScalarField), intent(out) :: self
        integer, intent(in) :: x
        integer, intent(in) :: y
        double precision, intent(in) :: val

        integer :: i, j

        i = mymath_pmod(x, self%nx)
        j = mymath_pmod(y, self%ny)

        self%values(i, j) = val
    end subroutine

    function scalarField_get(self, x, y) result(val)
        class(t_ScalarField), intent(in) :: self
        integer, intent(in) :: x
        integer, intent(in) :: y
        double precision :: val

        integer :: i, j

        i = mymath_pmod(x, self%nx)
        j = mymath_pmod(y, self%ny)

        val = self%values(i, j)
    end function

    subroutine scalarField_add(self, x, y, val)
        class(t_ScalarField), intent(inout) :: self
        integer, intent(in) :: x
        integer, intent(in) :: y
        double precision, intent(in) :: val

        integer :: i, j

        i = mymath_pmod(x, self%nx)
        j = mymath_pmod(y, self%ny)

        self%values(i, j) = self%values(i, j) + val
    end subroutine

    subroutine scalarField_mul(self, x, y, val)
        class(t_ScalarField), intent(inout) :: self
        integer, intent(in) :: x
        integer, intent(in) :: y
        double precision, intent(in) :: val

        integer :: i, j

        i = mymath_pmod(x, self%nx)
        j = mymath_pmod(y, self%ny)

        self%values(i, j) = self%values(i, j)*val
    end subroutine

    ! subroutine scalarField_xdiff(self, values)
    !     class(t_ScalarField), intent(inout) :: self
    !     double precision, intent(out) :: values(:, :)

    !     integer i, j

    !     do i = 1, self%nx
    !         do j = 1, self%ny
    !             values(i, j) = self%values(i, j) - self%values(i-1, j)
    !         end do
    !     end do
    ! end subroutine

end module
