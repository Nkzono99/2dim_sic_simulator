!> @brief 電場・電位に対する処理モジュール.
module efield
    use parameters
    use commons
    use poisson
    implicit none

    private

    public efield_update

contains

    !> @brief 電場と電位を更新する.
    subroutine efield_update
        call calc_phi
        call calc_ex
    end subroutine

    !> 電位を計算する.
    subroutine calc_phi
        call poisson_solve_periodic(rho%values(0:nx - 1, 0:ny - 1), phi%values(0:nx - 1, 0:ny - 1))
        phi%values(:, :) = -phi%values(:, :) / eps0

        ! 周期境界条件を適用
        phi%values(nx, :) = phi%values(0, :)
        phi%values(:, ny) = phi%values(:, 0)
    end subroutine

    !> 電場を計算する.
    subroutine calc_ex
        integer i, j

        ! 電位分布から電場分布を計算する.
        ! ex(i+0.5, j) = (phi(i, j) - phi(i+1, j)) / dx
        ! ey(i, j+0.5) = (phi(i, j) - phi(i, j+1)) / dx
        do i = 0, nx - 1
            do j = 0, ny - 1
                ef%values(i, j)%x = (phi%values(i, j) - phi%values(i + 1, j))/dx
                ef%values(i, j)%y = (phi%values(i, j) - phi%values(i, j + 1))/dx
            end do
        end do

        do j = 0, ny - 1
            ef%values(-1, j) = ef%values(nx - 1, j)
        end do
        do i = 0, nx - 1
            ef%values(i, -1) = ef%values(i, ny - 1)
        end do

        ! self-forceを回避するため電荷密度と同じグリッドに再分配する.
        ! ex(i, j) = 0.5(ex(i-0.5, j) + ex(i+0.5, j))
        ! ey(i, j) = 0.5(ey(i, j-0.5) + ey(i, j+0.5))
        do i = nx - 1, 0, -1
            do j = ny - 1, 0, -1
                ef%values(i, j)%x = 0.5*(ef%values(i - 1, j)%x + ef%values(i, j)%x)
                ef%values(i, j)%y = 0.5*(ef%values(i, j - 1)%y + ef%values(i, j)%y)
            end do
        end do

        ! 周期境界条件を適用
        ef%values(nx, :) = ef%values(0, :)
        ef%values(:, ny) = ef%values(:, 0)
    end subroutine

end module
