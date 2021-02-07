module m_vector3d
    implicit none

    private

    public Vector3d
    public dot3d
    public cross3d
    public rotate3dx, rotate3dy, rotate3dz
    public rand3dxy, rand3dyz, rand3dzx

    public operator(+), operator(-), operator(*), operator(/)
    public assignment(=)

    !> 円周率
    double precision, parameter :: pi = 4.0d0*atan(1.0d0)

    type Vector3d
        double precision :: x = 0.0d0
        double precision :: y = 0.0d0
        double precision :: z = 0.0d0

    contains
        procedure :: cross => c_cross
        procedure :: dot => c_dot

        procedure :: norm => c_norm
        procedure :: norm2 => c_norm2
        procedure :: normalize => c_normalize
        procedure :: normalized => c_normalized

        procedure :: rotatex => rotate3dx
        procedure :: rotatey => rotate3dy
        procedure :: rotatez => rotate3dz

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
        module procedure assign2, assign2_scalar
    end interface

contains

    !> Vector3d Constructor
    function init_vector3d(x, y, z) result(self)
        double precision, intent(in) :: x
        double precision, intent(in) :: y
        double precision, intent(in) :: z
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

        ret = cross3d(self, v)
    end function

    function c_dot(self, v) result(ret)
        class(Vector3d) :: self
        type(Vector3d), intent(in) :: v
        double precision :: ret

        ret = dot3d(self, v)
    end function

    function c_norm(self) result(ret)
        class(Vector3d) :: self
        double precision :: ret

        ret = self%x*self%x + self%y*self%y + self%z*self%z
        ret = sqrt(ret)
    end function

    function c_norm2(self) result(ret)
        class(Vector3d) :: self
        double precision :: ret

        ret = self%x*self%x + self%y*self%y + self%z*self%z
    end function

    subroutine c_normalize(self)
        class(Vector3d) :: self
        double precision :: norm

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

        write (ret, '(f8.4, f8.4, f8.4)') self%x, self%y, self%z
    end function

    ! Operator overrides

    function c_normalized(self) result(ret)
        class(Vector3d) :: self
        type(Vector3d) :: ret
        double precision :: norm

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
        double precision, intent(in) :: s
        type(Vector3d), intent(in) :: v
        type(Vector3d) :: ret

        ret%x = s + v%x
        ret%y = s + v%y
        ret%z = s + v%z
    end function

    function add2_scalar2(v, s) result(ret)
        type(Vector3d), intent(in) :: v
        double precision, intent(in) :: s
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
        double precision, intent(in) :: s
        type(Vector3d), intent(in) :: v
        type(Vector3d) :: ret

        ret%x = s - v%x
        ret%y = s - v%y
        ret%z = s - v%z
    end function

    function sub2_scalar2(v, s) result(ret)
        type(Vector3d), intent(in) :: v
        double precision, intent(in) :: s
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
        double precision, intent(in) :: s
        type(Vector3d), intent(in) :: v
        type(Vector3d) :: ret

        ret%x = s*v%x
        ret%y = s*v%y
        ret%z = s*v%z
    end function

    function mul2_scalar2(v, s) result(ret)
        type(Vector3d), intent(in) :: v
        double precision, intent(in) :: s
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
        double precision, intent(in) :: s
        type(Vector3d), intent(in) :: v
        type(Vector3d) :: ret

        ret%x = s/v%x
        ret%y = s/v%y
        ret%z = s/v%z
    end function

    function div2_scalar2(v, s) result(ret)
        type(Vector3d), intent(in) :: v
        double precision, intent(in) :: s
        type(Vector3d) :: ret

        ret%x = v%x/s
        ret%y = v%y/s
        ret%z = v%z/s
    end function

    subroutine assign2_scalar(v, s)
        type(Vector3d), intent(out) :: v
        double precision, intent(in) :: s

        v%x = s
        v%y = s
        v%z = s
    end subroutine

    subroutine assign2(v1, v2)
        type(Vector3d), intent(out) :: v1
        type(Vector3d), intent(in) :: v2

        v1%x = v2%x
        v1%y = v2%y
        v1%z = v2%z
    end subroutine

    ! Global methods

    !> Calculate cross products.
    function cross3d(v1, v2)
        type(Vector3d), intent(in) :: v1, v2
        type(Vector3d) :: cross3d

        cross3d%x = v1%y*v2%z - v1%z*v2%y
        cross3d%y = v1%z*v2%x - v1%x*v2%z
        cross3d%z = v1%x*v2%y - v1%y*v2%x
    end function

    !> Calculate dot products.
    function dot3d(v1, v2)
        type(Vector3d), intent(in) :: v1, v2
        double precision :: dot3d

        dot3d = v1%x*v2%x + v1%y*v2%y + v1%z*v2%z
    end function

    function rotate3dx(v, rad_angle)
        class(Vector3d), intent(in) :: v
        double precision, intent(in) :: rad_angle

        type(Vector3d) :: rotate3dx

        double precision :: y, z

        y = v%y * cos(rad_angle) - v%z * sin(rad_angle)
        z = v%y * sin(rad_angle) + v%z * cos(rad_angle)
        rotate3dx = Vector3d(v%x, y, z)
    end function

    function rotate3dy(v, rad_angle)
        class(Vector3d), intent(in) :: v
        double precision, intent(in) :: rad_angle

        type(Vector3d) :: rotate3dy

        double precision :: z, x

        z = v%z * cos(rad_angle) - v%x * sin(rad_angle)
        x = v%z * sin(rad_angle) + v%x * cos(rad_angle)

        rotate3dy = Vector3d(x, v%y, z)
    end function

    function rotate3dz(v, rad_angle)
        class(Vector3d), intent(in) :: v
        double precision, intent(in) :: rad_angle

        type(Vector3d) :: rotate3dz

        double precision :: x, y

        x = v%x * cos(rad_angle) - v%y * sin(rad_angle)
        y = v%x * sin(rad_angle) + v%y * cos(rad_angle)

        rotate3dz = Vector3d(x, y, v%z)
    end function

    function rand3dxy() result(v)
        type(Vector3d) :: v
        double precision :: rand

        call random_number(rand)

        v = Vector3d(1.0d0, 0.0d0, 0.0d0)
        v = rotate3dz(v, rand * 2 * pi)
    end function

    function rand3dyz() result(v)
        type(Vector3d) :: v
        double precision :: rand

        call random_number(rand)

        v = Vector3d(0.0d0, 1.0d0, 0.0d0)
        v = rotate3dx(v, rand * 2 * pi)
    end function

    function rand3dzx() result(v)
        type(Vector3d) :: v
        double precision :: rand

        call random_number(rand)

        v = Vector3d(0.0d0, 0.0d0, 1.0d0)
        v = rotate3dy(v, rand * 2 * pi)
    end function

end module
