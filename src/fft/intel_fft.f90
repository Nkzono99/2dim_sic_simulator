module intel_fft
    use mkl_dfti

    implicit none

    private
    public intel_fft_init
    public intel_fft_forward
    public intel_fft_backward

    type(dfti_descriptor), pointer, save, private :: plandft
    integer, save, private :: statdft

contains

    subroutine intel_fft_init(nx, ny)
        integer, intent(in) :: nx, ny
        integer :: nxy

        nxy = nx*ny
        statdft = dftiCreateDescriptor(plandft, dfti_double, dfti_complex, 2, [nx, ny])
        statdft = dftiSetValue(plandft, dfti_backward_scale, 1.d0/dble(nxy))
        statdft = dftiCommitDescriptor(plandft)
        return
    end subroutine

    subroutine intel_fft_forward(zfunc)
        complex(8), intent(inout) :: zfunc(*)
        statdft = dftiComputeForward(plandft, zfunc)
    end subroutine

    subroutine intel_fft_backward(zfunc)
        complex(8), intent(inout) :: zfunc(*)
        statdft = dfticomputebackward(plandft, zfunc)
    end subroutine

end module
