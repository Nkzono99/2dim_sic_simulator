module m_str
    implicit none

    private

    public str

    interface str
        module procedure int_to_str
    end interface

contains

    function int_to_str(i) result(s)
        integer, intent(in) :: i
        character(:), allocatable :: s
        character(100) :: buf
        
        write (buf, '(i0)') i
        s = trim(buf)
    end function

end module