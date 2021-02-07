module mymath
    implicit none

    public mymath_pmod

    interface mymath_pmod
        module procedure ipmod
        module procedure rpmod
    end interface

contains

    !> @brief 正剰余を返す.
    !>
    !> @param[in] a 割られる数
    !> @param[in] b 割る数
    !> @retval rpmod 正剰余
    function rpmod(a, b)
        double precision, intent(in) :: a
        double precision, intent(in) :: b
        double precision :: rpmod
        rpmod = a - floor(a/b)*b
    end function

    !> @brief 正剰余を返す.
    !>
    !> @param[in] a 割られる数
    !> @param[in] b 割る数
    !> @retval ipmod 正剰余
    function ipmod(a, b)
        integer, intent(in) :: a
        integer, intent(in) :: b
        integer :: ipmod

        ipmod = mod(b + mod(a, b), b)
    end function
end module
