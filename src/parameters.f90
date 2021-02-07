module parameters
    use omp_lib
    implicit none

    !> 真空の誘電率[F/m]
    double precision, parameter :: eps0 = 8.8541878128e-12
    !> ボルツマン定数[J/K]
    double precision, parameter :: kB = 1.380649e-23
    !> 電気素量 [C]
    double precision, parameter :: e = 1.6021766343e-19
    !> 電子質量 [kg]
    double precision, parameter :: me = 9.10938356e-31
    !> 円周率
    double precision, parameter :: pi = 4.0d0*atan(1.0d0)

    !> シミュレーションできる粒子の最大種類数
    integer, parameter :: max_nspec = 5

    !> 乱数のシード値(-1の場合実行ごとに異なるシード値を与える)
    integer :: random_seed = -1

    !> シミュレーションステップ数
    integer :: nsteps

    !> 出力ステップ数
    integer :: output_steps
    !> 出力ステップ間隔
    integer :: output_skips

    !> 出力フラグ

    !> 境界条件(0: 周期境界条件, 1: 反射境界条件)
    integer :: boundary_type = 0

    !> 初期のトレーサー数
    integer :: npcl_init(max_nspec)
    !> 現在のトレーサー数
    integer :: npcl = 0
    !> 最大トレーサー数
    integer :: max_npcl
    !> 現在のシンプレックス数
    integer :: nsimp = 0

    !> 空間グリッド数
    integer :: nx, ny
    !> グリッド幅 [m]
    double precision :: dx
    !> 時間幅 [s]
    double precision :: dt

    !> デバイ長 [m]
    double precision :: lambda

    !> 粒子の種類数
    integer :: nspec
    !> 各粒子の質量と電子の質量の比
    double precision :: q_ratio(max_nspec)
    !> 各粒子の電荷と電気素量の比
    double precision :: m_ratio(max_nspec)
    !> 各粒子の温度 [K]
    double precision :: Ts(max_nspec)

    !> 一つのsimplexに割り当てる粒子数
    double precision :: npcl_per_super
    !> 一つのsimplexの電荷
    double precision :: qs(max_nspec)
    !> 一つのsimplexの質量
    double precision :: ms(max_nspec)

    ! Refinement Parameter
    !> refinementを実行する間隔(0ならrefinementを行わない)
    integer :: refinement_interval = 0
    !> refinementを実行するしきい値 [grid]
    double precision :: refinement_threshold = 1

    !> スレッド数 (プログラム実行時に格納される)
    integer :: nthreads = 1

contains
    !> パラメータをファイルから読み込み初期化する.
    !>
    !> @param[in] inputfilename パラメータファイル名
    !> @param[in] output_number 出力番号(default: 11)
    subroutine parameters_init(inputfilename, output_number)
        character(*), intent(in) :: inputfilename
        integer, intent(in), optional :: output_number
        integer :: output_number_

        namelist /random/ random_seed
        namelist /simulation/ nsteps, npcl_init, max_npcl, &
            nx, ny, dx, dt, boundary_type
        namelist /solver/ refinement_interval, refinement_threshold
        namelist /output/ output_steps
        namelist /plasma/ nspec, lambda, q_ratio, m_ratio, Ts

        if (present(output_number)) then
            output_number_ = output_number
        else
            output_number_ = 11
        end if

        open (output_number_, file=inputfilename)
        read (output_number_, nml=random)
        read (output_number_, nml=simulation)
        read (output_number_, nml=solver)
        read (output_number_, nml=output)
        read (output_number_, nml=plasma)
        close (output_number_)

        output_skips = nsteps/output_steps

        if (lambda == -1.0) then
            npcl_per_super = 1
        else
            ! npcl_per_super = eps0*kB*Ts(1)/(lambda*lambda*e*e)*dx*nx*dx*ny/npcl_init(1)
            npcl_per_super = eps0*kB*Ts(1)/(lambda*lambda*e*e)*dx*nx*dx*ny/(nx*ny)
        end if

        qs = e*q_ratio*npcl_per_super
        ms = me*m_ratio*npcl_per_super

        !$omp parallel
        !$omp single
        nthreads = omp_get_num_threads()
        !$omp end single
        !$omp end parallel
    end subroutine
end module
