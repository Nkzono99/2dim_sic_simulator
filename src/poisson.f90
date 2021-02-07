! FFTを用いた2次元ポアソン方程式ソルバーモジュール
! 想定するポアソン方程式:
!   ∂^2/∂x^2 u(x, y) = f(x, y)
! 入力: f(x, y)
! 出力: u(x, y)
module poisson
    use parameters
    use fft
    implicit none

contains

    !> 2次元ポアソン方程式(Periodic)を解く
    subroutine poisson_solve_periodic(f, u)
        double precision, intent(in) :: f(:, :)
        double precision, intent(out) :: u(:, :)
        double complex, allocatable :: buf(:, :)

        integer :: kx, ky
        double precision :: lx, ly

        ! rho(0:nx-1, 0:ny-1) => buf(1:nx, 1:ny)
        buf = f(:, :)

        call fft_init(nx, ny)

        ! rho(ix, iy) => rho(kx, ky)
        call fft_forward(buf(1:nx, 1:ny))

        ! rho(kx, ky) => phi(kx, ky)
        do kx = 1, nx
            lx = 2 * (cos(2.0d0 * (kx-1) / nx * pi) - 1)
            do ky = 1, ny
                ly = 2 * (cos(2.0d0 * (ky-1) / ny * pi) - 1)

                buf(kx, ky) = buf(kx, ky) / (lx + ly) * dx * dx
            end do
        end do

        buf(1, 1) = 0

        ! phi(kx, ky) => phi(ix, iy)
        call fft_backward(buf(1:nx, 1:ny))

        u(:, :) = dble(buf(1:nx, 1:ny))
    end subroutine


    !> 2次元ポアソン方程式(Direcret)を解く
    subroutine poisson_solve_direcret(f, u)
        double precision, intent(in) :: f(:, :)
        double precision, intent(out) :: u(:, :)
        double complex, allocatable :: buf(:, :)

        integer :: kx, ky
        double precision :: lx, ly

        ! rhoの値をバッファにコピーする
        ! rho(0:nx-1, 0:ny-1) => buf(1:nx, 1:ny)
        buf = f(:, :)

        call fft_init(nx, ny)

        ! rhoを周波数領域に変換する
        ! rho(ix, iy) => rho(kx, ky)
        call fft_forward(buf(1:nx, 1:ny))

        ! Direcret境界条件のため、cos成分をゼロにする
        buf(:, :) = cmplx(buf(:, :), kind=8)

        ! phiのスペクトルを計算する
        ! rho(kx, ky) => phi(kx, ky)
        do kx = 1, nx
            lx = 2 * (cos(dx * (kx-1) / nx * pi) - 1)
            do ky = 1, ny
                ly = 2 * (cos(dx * (ky-1) / ny * pi) - 1)

                buf(kx, ky) = -buf(kx, ky) / (lx + ly) * dx * dx
            end do
        end do

        buf(1, 1) = 0

        ! phiを位置領域に変換する
        ! phi(kx, ky) => phi(ix, iy)
        call fft_backward(buf(1:nx, 1:ny))

        u(:, :) = dble(buf(1:nx, 1:ny))
    end subroutine

    !> 2次元ポアソン方程式(Neumann)を解く
    subroutine poisson_solve_neumann(f, u)
        double precision, intent(in) :: f(:, :)
        double precision, intent(out) :: u(:, :)
        double complex, allocatable :: buf(:, :)

        integer :: kx, ky
        double precision :: lx, ly

        ! rhoの値をバッファにコピーする
        ! rho(0:nx-1, 0:ny-1) => buf(1:nx, 1:ny)
        buf = f(:, :)

        call fft_init(nx, ny)

        ! rhoを周波数領域に変換する
        ! rho(ix, iy) => rho(kx, ky)
        call fft_forward(buf(1:nx, 1:ny))

        ! Neumann境界条件のため、sin成分をゼロにする
        buf(:, :) = dble(buf(:, :))

        ! phiのスペクトルを計算する
        ! rho(kx, ky) => phi(kx, ky)
        do kx = 1, nx
            lx = 2 * (cos(dx * (kx-1) / nx * pi) - 1)
            do ky = 1, ny
                ly = 2 * (cos(dx * (ky-1) / ny * pi) - 1)

                buf(kx, ky) = -buf(kx, ky) / eps0 / (lx + ly) * dx * dx
            end do
        end do

        buf(1, 1) = 0

        ! phiを位置領域に変換する
        ! phi(kx, ky) => phi(ix, iy)
        call fft_backward(buf(1:nx, 1:ny))

        u(:, :) = dble(buf(1:nx, 1:ny))
    end subroutine
end module