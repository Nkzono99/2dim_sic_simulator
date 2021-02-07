module m_iter
    implicit none

    private
    public t_Iter

    type, abstract :: t_Iter
    contains
        procedure(abstract_next), deferred :: next
        procedure(abstract_hasnext), deferred :: hasnext
    end type

    interface
        function abstract_next(self) result(inext)
            import t_Iter
            class(t_Iter), intent(inout) :: self
            integer :: inext
        end function

        function abstract_hasnext(self) result(hasnext)
            import t_Iter
            class(t_Iter), intent(in) :: self
            logical :: hasnext
        end function
    end Interface
end module