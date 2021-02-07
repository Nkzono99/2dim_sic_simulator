!> @brief Utilityモジュール
module myrandom
    implicit none

    private

    public myrandom_set_random_seed
    public myrandom_rand_bm
    public myrandom_rand_bm2

    !> 円周率
    real, parameter :: pi = 4.0*atan(1.0)

contains

    !> @brief 乱数のシード値を設定する.
    !>
    !> @param[in] seed シード値
    subroutine myrandom_set_random_seed(seed)
        integer, intent(in), optional :: seed
        integer :: i
        integer :: seedsize
        integer, allocatable :: seeds(:)
        double precision :: value

        call random_seed(size=seedsize)
        allocate (seeds(seedsize))
        call random_seed(get=seeds)

        if (present(seed) .and. seed /= -1) then
            seeds = seed
            call random_seed(put=seeds)

            do i = 1, seedsize
                call random_number(value)
                seeds(i) = int((2*(value - 0.5))*2147483647)
            end do
        else
            call system_clock(count=seeds(1))
        end if

        call random_seed(put=seeds)
    end subroutine

    !> @brief Box-Muller法により正規分布に従う乱数を生成する.
    !>
    !> @param[out] z 生成した乱数を代入する変数
    subroutine myrandom_rand_bm(z)
        double precision, intent(out) :: z
        double precision :: x, y

        call random_number(x)
        call random_number(y)

        z = sqrt(-2*log(x))*cos(2*pi*y)
    end subroutine

    !> @brief Box-Muller法により正規分布に従う2つの独立な乱数を生成する.
    !>
    !> @param[out] z1 生成した乱数を代入する変数
    !> @param[out] z2 生成した乱数を代入する変数
    subroutine myrandom_rand_bm2(z1, z2)
        double precision, intent(out) :: z1, z2
        double precision :: x, y

        call random_number(x)
        call random_number(y)

        z1 = sqrt(-2*log(x))*cos(2*pi*y)
        z2 = sqrt(-2*log(x))*sin(2*pi*y)
    end subroutine

end module
