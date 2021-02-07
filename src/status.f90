!> @brief 現在の状態の出力処理モジュール.
module status
    use parameters
    use commons
    use m_csv
    implicit none

    private

    public status_start, status_write, status_close

    character(:), allocatable :: output_directory

contains

    !> @brief 出力ファイルを作成する.
    !>
    !> @param[in] output_dir 出力ディレクトリ
    subroutine status_start(output_dir)
        character(:), allocatable, intent(in) :: output_dir
        integer ispec

        output_directory = output_dir

        call system('mkdir '//trim(output_dir)//'\phi')
        call system('mkdir '//trim(output_dir)//'\rho')
        call system('mkdir '//trim(output_dir)//'\simplex')

        open (103, file=output_dir//'/energy.csv', status='replace')
    end subroutine

    !> @brief 現在の状況をファイルに書き込む.
    !> @param[in] istep 現在のステップ数
    subroutine status_write(istep)
        integer, intent(in) :: istep
        integer ispec

        if (mod(istep, output_skips) == 0) then
            call create_csv_r2d( &
                100, &
                phi%values(0:nx - 1, 0:ny - 1), &
                filename=output_directory//'/'//'phi/phi_'//str(istep)//'.csv')

            call create_csv_r2d( &
                101, &
                rho%values(0:nx - 1, 0:ny - 1), &
                filename=output_directory//'/'//'rho/rho_'//str(istep)//'.csv')

            call create_simplex( &
                102, &
                filename=output_directory//'/'//'simplex/simplex_'//str(istep)//'.csv')

            call write_energy(103)
        end if
    end subroutine

    !> @brief 出力を終了する.
    subroutine status_close
        close (103)
    end subroutine

    subroutine create_simplex(output_number, filename)
        use m_hfiter, only: t_VertexAroundFaceIter
        integer, intent(in) :: output_number
        character(len=*), intent(in) :: filename

        integer :: isimp, ispec

        type(t_VertexAroundFaceIter) :: it
        integer :: it1, it2, it3

        double precision :: values(6)

        open (output_number, file=filename, status='replace')

        do isimp = 1, nsimp
            ispec = simplicies(isimp)%ispec
            it = t_VertexAroundFaceIter(simplicies(isimp)%iedge)
            it1 = it%next()
            it2 = it%next()
            it3 = it%next()

            values(1) = tracers(it1)%position%x
            values(2) = tracers(it1)%position%y
            values(3) = tracers(it2)%position%x
            values(4) = tracers(it2)%position%y
            values(5) = tracers(it3)%position%x
            values(6) = tracers(it3)%position%y

            write (output_number, '(I2, ",", 6(E20.10, :, ","))') ispec, values(1:6)
        end do
    end subroutine

    subroutine write_energy(output_number)
        integer, intent(in) :: output_number

        double precision :: values(3)

        integer :: i, j
        double precision :: ex, ey

        values(1) = 0.0d0
        do i = 1, nsimp
            values(1) = values(1) + simplicies(i)%energy()
        end do

        values(2) = 0.0d0
        do i = 1, nx
            do j = 1, ny
                values(2) = values(2) + 0.5d0*eps0*ef%values(i, j)%norm2()*dx*dx
            end do
        end do

        values(3) = values(1) + values(2)

        write (output_number, '(3(E20.10, :, ","))') values(1:3)
    end subroutine

end module
