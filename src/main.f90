!> 1次元静電SICシミュレータ.
program main
    use parameters
    use argparse
    use commons
    use efield
    use status
    use progressbar
    use sic
    use myrandom
    use m_vectors
    use m_halfedge, only: halfedge_init
    use sic_types, only: sic_types_init
    use m_meshfile

    implicit none

    integer istep

    ! コマンドライン引数の処理
    call argparse_init
    call argparse_add('--datadir', subflagname='-d', default='.', description='data output directory')
    call argparse_add('--inputfilename', subflagname='-i', description='parameter namelist filename (*.in)', default='plasma.in')
    call argparse_parse

    write (*, fmt='(a)', advance='no') 'set random seed...'
    call myrandom_set_random_seed(random_seed)
    print *, 'done'

    write (*, fmt='(a)', advance='no') 'initialize parameter...'
    call parameters_init(argparse_get('datadir')//'/'//argparse_get('inputfilename'))
    print *, 'done'

    write (*, fmt='(a)', advance='no') 'initialize and allocate common variables...'
    call commons_init
    print *, 'done'

    write (*, fmt='(a)', advance='no') 'initialize modules...'
    call sic_types_init(tracers, simplicies)
    call halfedge_init(tracers, simplicies)
    print *, 'done'

    write (*, fmt='(a)', advance='no') 'initialize simiplex and tracers...'
    call sic_init
    print *, 'done'

    ! call test()

    call show_simulation_settings

    call status_start(argparse_get('datadir'))

    call status_write(0)

    print *, 'start main loop'
    do istep = 1, nsteps
        if (mod(istep-1, 10) == 0) then
            call progressbar_show(istep, nsteps, n=50,&
                 message='(npcl:'//str(npcl)//', nsimp:'//str(nsimp)//')')
            print *
        end if

        ! print *, 'scatter'
        call sic_scatter_on_grid

        ! print *, 'efield update'
        call efield_update

        ! print *, 'sic udpate'
        call sic_update

        if (refinement_interval /= 0 .and. mod(istep, refinement_interval) == 0) then
            call sic_refinement(refinement_threshold*dx*dx)
        end if

        call status_write(istep)
    end do

    call status_close

contains

    subroutine show
        use parameters
        print *, 'show'
    end subroutine

    !> @brief シミュレーション設定を出力する.
    subroutine show_simulation_settings
        print *, '---- Simulation Parameter --------'
        print *, 'total steps =', nsteps
        print *, 'initial number of super particles', npcl_init(1:nspec)
        print *, 'max number of super particles', max_npcl
        print *, 'number of grids =', nx, ny
        print *, ''
        print *, 'dt =', dt
        print *, 'dx =', dx
        print *, 'npcl_per_super =', npcl_per_super
        print *, 'q_per_super =', qs(1:nspec)
        print *, 'm_per_super =', ms(1:nspec)
        print *, '----------------------------------'
        print *, '---- Plasma Parameter ------------'
        print *, 'debye length =', lambda
        print *, 'wpe =', 1/lambda*sqrt(kB*Ts(1)/me)
        print *, '----------------------------------'
        print *, '---- Parallel Parameter ----------'
        print *, 'number of thread =', nthreads
        print *, '----------------------------------'
        print *, '---- Refinement Parameter --------'
        print *, 'refinement interval =', refinement_interval
        print *, 'refinement threthold =', refinement_threshold
        print *, '----------------------------------'
    end subroutine

    subroutine test()
        ! call scatter_test()

        call poisson_test()

        ! call fft_test()
    end subroutine

    subroutine scatter_test()
        use m_halfedge, only: halfedge_register_face, halfedge_collect_edges
        use m_csv
        use sic_types, only: t_Simplex

        implicit none

        double precision :: moment
        double precision :: offsets(2)

        tracers(1) = t_Tracer(1, Vector2d(1, 1), Vector3d(0, 0, 0))
        tracers(2) = t_Tracer(1, Vector2d(20, 7), Vector3d(0, 0, 0))
        tracers(3) = t_Tracer(1, Vector2d(4, 15), Vector3d(0, 0, 0))

        simplicies(1) = t_Simplex(1, 1.0d0, qs(1), ms(1))
        npcl = 3
        nsimp = 1

        call halfedge_register_face(nsimp, [1, 2, 3])
        call halfedge_collect_edges

        moment = simplicies(1)%q

        offsets = [-0.5d0*dx, -0.5d0*dx]
        call simplicies(1)%scatter(moment, rho, offsets)

        call create_csv_r2d(100, rho%values, 'rho.csv')
    end subroutine

    subroutine poisson_test()
        use poisson
        use m_csv

        rho%values(:, :) = 0
        rho%values(int(nx/10), int(ny/3)) = 1e-19
        call create_csv_r2d(101, rho%values(0:nx - 1, 0:ny - 1), 'rho_poisson.csv')

        call poisson_solve_periodic(rho%values(0:nx - 1, 0:ny - 1), phi%values(0:nx - 1, 0:ny - 1))
        phi%values(:, :) = -phi%values(:, :) / eps0

        print *, 'create'
        call create_csv_r2d(102, phi%values(0:nx - 1, 0:ny - 1), 'phi_poisson.csv')
        print *, 'created'

    end subroutine

    subroutine fft_test()
        use fft

        integer, parameter :: n = 10
        complex(8), allocatable :: zfunc(:, :)
        double precision :: r1, r2

        integer :: i, j

        print *, 111

        allocate (zfunc(n, n))
        do i = 1, n
            do j = 1, n
                call random_number(r1)
                call random_number(r2)
                zfunc(i, j) = dcmplx(r1, r2)
            end do
        end do

        call fft_init(n, n)

        print *, '----------- ORIGINAL -------------'
        print *, zfunc

        call fft_forward(zfunc)

        print *, '----------- FFT TRANSFORMED -------------'
        print *, zfunc

        call fft_backward(zfunc)
        print *, '----------- FFT BACKWARDED -------------'
        print *, zfunc

    end subroutine
end program
