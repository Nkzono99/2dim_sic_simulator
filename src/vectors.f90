module vectors
    implicit none

    type Vector3d
        real(8) :: x = 0.0d0
        real(8) :: y = 0.0d0
        real(8) :: z = 0.0d0

    contains
        procedure :: cross => c_cross
        procedure :: dot => c_dot

        procedure :: norm => c_norm
        procedure :: norm2 => c_norm2
        procedure :: normalize => c_normalize
        procedure :: normalized => c_normalized

        procedure :: copy => c_copy
        procedure :: str => c_str
    end type

    interface Vector3d
        module procedure init_vector3d
    end interface

    interface operator(+)
        module procedure add2, add2_scalar1, add2_scalar2
    end interface

    interface operator(-)
        module procedure sub2, sub2_scalar1, sub2_scalar2
    end interface

    interface operator(*)
        module procedure mul2, mul2_scalar1, mul2_scalar2
    end interface

    interface operator(/)
        module procedure div2, div2_scalar1, div2_scalar2
    end interface

    interface assignment(=)
        module procedure assign2
    end interface

    private

    public Vector3d

    public cross
    public dot
    public operator(+), operator(-), operator(*), operator(/)
    public assignment(=)

contains

    !> Vector3d Constructor
    function init_vector3d(x, y, z) result(self)
        real(8), intent(in) :: x
        real(8), intent(in) :: y
        real(8), intent(in) :: z
        type(Vector3d) :: self

        self%x = x
        self%y = y
        self%z = z
    end function

    ! Class methods

    function c_cross(self, v) result(ret)
        class(Vector3d) :: self
        type(Vector3d), intent(in) :: v
        type(Vector3d) :: ret

        ret = cross(self, v)
    end function

    function c_dot(self, v) result(ret)
        class(Vector3d) :: self
        type(Vector3d), intent(in) :: v
        real(8) :: ret

        ret = dot(self, v)
    end function

    function c_norm(self) result(ret)
        class(Vector3d) :: self
        real(8) :: ret

        ret = self%x*self%x + self%y*self%y + self%z*self%z
        ret = sqrt(ret)
    end function

    function c_norm2(self) result(ret)
        class(Vector3d) :: self
        real(8) :: ret

        ret = self%x*self%x + self%y*self%y + self%z*self%z
    end function

    subroutine c_normalize(self)
        class(Vector3d) :: self
        real(8) :: norm

        norm = self%norm()

        self%x = self%x/norm
        self%y = self%y/norm
        self%z = self%z/norm
    end subroutine

    function c_copy(self) result(ret)
        class(Vector3d) :: self
        type(Vector3d) :: ret

        ret%x = self%x
        ret%y = self%y
        ret%z = self%z
    end function

    function c_str(self) result(ret)
        class(Vector3d) :: self
        character(30) :: ret

        write(ret, '(f8.4, f8.4, f8.4)') self%x, self%y, self%z
    end function

    ! Operator overrides

    function c_normalized(self) result(ret)
        class(Vector3d) :: self
        type(Vector3d) :: ret
        real(8) :: norm

        norm = self%norm()

        ret%x = self%x/norm
        ret%y = self%y/norm
        ret%z = self%z/norm
    end function

    function add2(v1, v2) result(ret)
        type(Vector3d), intent(in) :: v1, v2
        type(Vector3d) :: ret

        ret%x = v1%x + v2%x
        ret%y = v1%y + v2%y
        ret%z = v1%z + v2%z
    end function

    function add2_scalar1(s, v) result(ret)
        real(8), intent(in) :: s
        type(Vector3d), intent(in) :: v
        type(Vector3d) :: ret

        ret%x = s + v%x
        ret%y = s + v%y
        ret%z = s + v%z
    end function

    function add2_scalar2(v, s) result(ret)
        type(Vector3d), intent(in) :: v
        real(8), intent(in) :: s
        type(Vector3d) :: ret

        ret%x = v%x + s
        ret%y = v%y + s
        ret%z = v%z + s
    end function

    function sub2(v1, v2) result(ret)
        type(Vector3d), intent(in) :: v1, v2
        type(Vector3d) :: ret

        ret%x = v1%x - v2%x
        ret%y = v1%y - v2%y
        ret%z = v1%z - v2%z
    end function

    function sub2_scalar1(s, v) result(ret)
        real(8), intent(in) :: s
        type(Vector3d), intent(in) :: v
        type(Vector3d) :: ret

        ret%x = s - v%x
        ret%y = s - v%y
        ret%z = s - v%z
    end function

    function sub2_scalar2(v, s) result(ret)
        type(Vector3d), intent(in) :: v
        real(8), intent(in) :: s
        type(Vector3d) :: ret

        ret%x = v%x - s
        ret%y = v%y - s
        ret%z = v%z - s
    end function

    function mul2(v1, v2) result(ret)
        type(Vector3d), intent(in) :: v1, v2
        type(Vector3d) :: ret

        ret%x = v1%x*v2%x
        ret%y = v1%y*v2%y
        ret%z = v1%z*v2%z
    end function

    function mul2_scalar1(s, v) result(ret)
        real(8), intent(in) :: s
        type(Vector3d), intent(in) :: v
        type(Vector3d) :: ret

        ret%x = s*v%x
        ret%y = s*v%y
        ret%z = s*v%z
    end function

    function mul2_scalar2(v, s) result(ret)
        type(Vector3d), intent(in) :: v
        real(8), intent(in) :: s
        type(Vector3d) :: ret

        ret%x = v%x*s
        ret%y = v%y*s
        ret%z = v%z*s
    end function

    function div2(v1, v2) result(ret)
        type(Vector3d), intent(in) :: v1, v2
        type(Vector3d) :: ret

        ret%x = v1%x/v2%x
        ret%y = v1%y/v2%y
        ret%z = v1%z/v2%z
    end function

    function div2_scalar1(s, v) result(ret)
        real(8), intent(in) :: s
        type(Vector3d), intent(in) :: v
        type(Vector3d) :: ret

        ret%x = s/v%x
        ret%y = s/v%y
        ret%z = s/v%z
    end function

    function div2_scalar2(v, s) result(ret)
        type(Vector3d), intent(in) :: v
        real(8), intent(in) :: s
        type(Vector3d) :: ret

        ret%x = v%x/s
        ret%y = v%y/s
        ret%z = v%z/s
    end function

    subroutine assign2(v, s)
        type(Vector3d), intent(out) :: v
        real(8), intent(in) :: s

        v%x = s
        v%y = s
        v%z = s
    end subroutine

    ! Global methods

    !> Calculate cross products.
    function cross(v1, v2)
        type(Vector3d), intent(in) :: v1, v2
        type(Vector3d) :: cross

        cross%x = v1%y*v2%z - v1%z*v2%y
        cross%y = v1%z*v2%x - v1%x*v2%z
        cross%z = v1%x*v2%y - v1%y*v2%x
    end function

    !> Calculate dot products.
    function dot(v1, v2)
        type(Vector3d), intent(in) :: v1, v2
        real(8) :: dot

        dot = v1%x*v2%x + v1%y*v2%y + v1%z*v2%z
    end function

end module
