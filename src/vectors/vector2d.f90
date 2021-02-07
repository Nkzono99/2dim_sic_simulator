module m_vector2d
    implicit none

    private

    public Vector2d
    public dot2d
    public cross2d
    public rotate2d
    public rand2dvec

    public operator(+), operator(-), operator(*), operator(/)
    public assignment(=)

    !> 円周率
    double precision, parameter :: pi = 4.0d0*atan(1.0d0)

    type Vector2d
        double precision :: x = 0.0d0
        double precision :: y = 0.0d0

    contains
        procedure :: cross => c_cross
        procedure :: dot => c_dot

        procedure :: norm => c_norm
        procedure :: norm2 => c_norm2
        procedure :: normalize => c_normalize
        procedure :: normalized => c_normalized

        procedure :: rotate => rotate2d

        procedure :: copy => c_copy
        procedure :: str => c_str
    end type

    interface Vector2d
        module procedure init_Vector2d
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
        module procedure assign2, assign2_scalar
    end interface

contains

    !> Vector2d Constructor
    function init_Vector2d(x, y) result(self)
        double precision, intent(in) :: x
        double precision, intent(in) :: y
        type(Vector2d) :: self

        self%x = x
        self%y = y
    end function

    ! Class methods

    function c_cross(self, v) result(ret)
        class(Vector2d) :: self
        type(Vector2d), intent(in) :: v
        double precision :: ret

        ret = cross2d(self, v)
    end function

    function c_dot(self, v) result(ret)
        class(Vector2d) :: self
        type(Vector2d), intent(in) :: v
        double precision :: ret

        ret = dot2d(self, v)
    end function

    function c_norm(self) result(ret)
        class(Vector2d) :: self
        double precision :: ret

        ret = self%x*self%x + self%y*self%y
        ret = sqrt(ret)
    end function

    function c_norm2(self) result(ret)
        class(Vector2d) :: self
        double precision :: ret

        ret = self%x*self%x + self%y*self%y
    end function

    subroutine c_normalize(self)
        class(Vector2d) :: self
        double precision :: norm

        norm = self%norm()

        self%x = self%x/norm
        self%y = self%y/norm
    end subroutine

    function c_copy(self) result(ret)
        class(Vector2d) :: self
        type(Vector2d) :: ret

        ret%x = self%x
        ret%y = self%y
    end function

    function c_str(self) result(ret)
        class(Vector2d) :: self
        character(30) :: ret

        write (ret, '(f8.4, f8.4)') self%x, self%y
    end function

    ! Operator overrides

    function c_normalized(self) result(ret)
        class(Vector2d) :: self
        type(Vector2d) :: ret
        double precision :: norm

        norm = self%norm()

        ret%x = self%x/norm
        ret%y = self%y/norm
    end function

    function add2(v1, v2) result(ret)
        type(Vector2d), intent(in) :: v1, v2
        type(Vector2d) :: ret

        ret%x = v1%x + v2%x
        ret%y = v1%y + v2%y
    end function

    function add2_scalar1(s, v) result(ret)
        double precision, intent(in) :: s
        type(Vector2d), intent(in) :: v
        type(Vector2d) :: ret

        ret%x = s + v%x
        ret%y = s + v%y
    end function

    function add2_scalar2(v, s) result(ret)
        type(Vector2d), intent(in) :: v
        double precision, intent(in) :: s
        type(Vector2d) :: ret

        ret%x = v%x + s
        ret%y = v%y + s
    end function

    function sub2(v1, v2) result(ret)
        type(Vector2d), intent(in) :: v1, v2
        type(Vector2d) :: ret

        ret%x = v1%x - v2%x
        ret%y = v1%y - v2%y
    end function

    function sub2_scalar1(s, v) result(ret)
        double precision, intent(in) :: s
        type(Vector2d), intent(in) :: v
        type(Vector2d) :: ret

        ret%x = s - v%x
        ret%y = s - v%y
    end function

    function sub2_scalar2(v, s) result(ret)
        type(Vector2d), intent(in) :: v
        double precision, intent(in) :: s
        type(Vector2d) :: ret

        ret%x = v%x - s
        ret%y = v%y - s
    end function

    function mul2(v1, v2) result(ret)
        type(Vector2d), intent(in) :: v1, v2
        type(Vector2d) :: ret

        ret%x = v1%x*v2%x
        ret%y = v1%y*v2%y
    end function

    function mul2_scalar1(s, v) result(ret)
        double precision, intent(in) :: s
        type(Vector2d), intent(in) :: v
        type(Vector2d) :: ret

        ret%x = s*v%x
        ret%y = s*v%y
    end function

    function mul2_scalar2(v, s) result(ret)
        type(Vector2d), intent(in) :: v
        double precision, intent(in) :: s
        type(Vector2d) :: ret

        ret%x = v%x*s
        ret%y = v%y*s
    end function

    function div2(v1, v2) result(ret)
        type(Vector2d), intent(in) :: v1, v2
        type(Vector2d) :: ret

        ret%x = v1%x/v2%x
        ret%y = v1%y/v2%y
    end function

    function div2_scalar1(s, v) result(ret)
        double precision, intent(in) :: s
        type(Vector2d), intent(in) :: v
        type(Vector2d) :: ret

        ret%x = s/v%x
        ret%y = s/v%y
    end function

    function div2_scalar2(v, s) result(ret)
        type(Vector2d), intent(in) :: v
        double precision, intent(in) :: s
        type(Vector2d) :: ret

        ret%x = v%x/s
        ret%y = v%y/s
    end function

    subroutine assign2_scalar(v, s)
        type(Vector2d), intent(out) :: v
        double precision, intent(in) :: s

        v%x = s
        v%y = s
    end subroutine

    subroutine assign2(v1, v2)
        type(Vector2d), intent(out) :: v1
        type(Vector2d), intent(in) :: v2

        v1%x = v2%x
        v1%y = v2%y
    end subroutine

    ! Global methods

    !> Calculate cross products.
    function cross2d(v1, v2)
        type(Vector2d), intent(in) :: v1, v2
        double precision :: cross2d

        cross2d = v1%x*v2%y - v2%x*v1%y
    end function

    !> Calculate dot products.
    function dot2d(v1, v2)
        type(Vector2d), intent(in) :: v1, v2
        double precision :: dot2d

        dot2d = v1%x*v2%x + v1%y*v2%y
    end function

    function rotate2d(v, rad_angle)
        class(Vector2d), intent(in) :: v
        double precision :: rad_angle

        type(Vector2d) :: rotate2d

        double precision :: x, y

        x = v%x * cos(rad_angle) - v%y * sin(rad_angle)
        y = v%x * sin(rad_angle) + v%y * cos(rad_angle)

        rotate2d = Vector2d(x, y)
    end function

    function rand2dvec
        type(Vector2d) :: rand2dvec
        double precision :: rand

        call random_number(rand)

        rand2dvec = Vector2d(1.0d0, 0.0d0)
        rand2dvec = rotate2d(rand2dvec, rand * 2 * pi)
    end function

end module
