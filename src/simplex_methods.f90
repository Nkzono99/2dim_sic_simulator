submodule(sic_types) simplex_methods
    implicit none

contains

    function init_simplex(rate) result(obj)
        type(Simplex), target :: obj
        real(8), intent(in) :: rate

        obj%rate = rate
    end function

    subroutine simplex_scatter(self)
        class(Simplex) :: self
    end subroutine

    subroutine simplex_gather(self)
        class(Simplex) :: self
    end subroutine

end submodule
