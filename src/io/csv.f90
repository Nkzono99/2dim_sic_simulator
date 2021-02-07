module m_csv
    use m_str

    implicit none

contains
    subroutine create_csv_r2d(output_number, values, filename)
        integer, intent(in) :: output_number
        double precision, intent(in) :: values(:, :)
        character(len=*), intent(in) :: filename

        open (output_number, file=filename, status='replace')
        call write_csv_r2d(output_number, values)
        close (output_number)
    end subroutine

    subroutine write_csv_r2d(output_number, values)
        integer, intent(in) :: output_number
        double precision, intent(in) :: values(:, :)

        character(:), allocatable :: line_format
        integer :: j, j_start

        line_format = '('//str(size(values, dim=1))//'(E20.6, :, ","))'
        j_start = lbound(values, dim=2) - 1
        do j = 1, size(values, dim=2)
            write (output_number, line_format) values(:, j_start + j)
        end do
    end subroutine

end module
