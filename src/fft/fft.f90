module fft
    use intel_fft

    implicit none

    private
    public fft_init
    public fft_forward
    public fft_backward

    integer nxy, inx, iny

    interface
        subroutine init(nx, ny)
            integer, intent(in) :: nx
            integer, intent(in) :: ny
        end subroutine

        subroutine forward(zfunc)
            complex(8), intent(inout) :: zfunc(*)
        end subroutine

        subroutine backward(zfunc)
            complex(8), intent(inout) :: zfunc(*)
        end subroutine
    end interface

    ! Use Intel-MKL-DFT library
    procedure(init), pointer :: fft_init => intel_fft_init
    procedure(forward), pointer :: fft_forward => intel_fft_forward
    procedure(backward), pointer :: fft_backward => intel_fft_backward

end module
