module m_edgedict
    implicit none

    private

    public t_Key
    public t_Dictionary

    integer, parameter :: DEFAULT_SIZE = 524287

    type t_Key
        integer :: i1, i2
    contains
        procedure gethash => key_gethash
        procedure equals => key_equals
    end type

    interface t_Key
        procedure init_key
    end interface

    type :: t_Entity
        type(t_Key) :: key
        integer :: value
        type(t_Entity), pointer :: next => null()
    end type

    type t_ptr_Entity
        type(t_Entity), pointer :: ref => null()
    end type

    type t_Dictionary
        integer :: size
        type(t_ptr_Entity), allocatable :: entities(:)
    contains
        procedure get => dictionary_get
        procedure set => dictionary_set
    end type

    interface t_Dictionary
        procedure init_Dictionary
    end interface

contains

    function init_key(i1, i2) result(obj)
        integer, intent(in) :: i1
        integer, intent(in) :: i2
        type(t_Key) :: obj

        obj%i1 = i1
        obj%i2 = i2
    end function

    function key_gethash(self) result(hash)
        class(t_Key), intent(in) :: self
        integer :: hash

        hash = abs(self%i1*1446659 + self%i2*230309)
    end function

    function key_equals(self, key) result(result)
        class(t_Key), intent(in) :: self
        class(t_Key), intent(in) :: key
        logical :: result

        result = (self%i1 == key%i1) .and. (self%i2 == key%i2)
    end function

    function init_Dictionary() result(obj)
        type(t_Dictionary) :: obj

        obj%size = DEFAULT_SIZE
        allocate (obj%entities(obj%size))
    end function

    function dictionary_get(self, key) result(val)
        class(t_Dictionary), intent(inout), target :: self
        class(t_Key), intent(in), target :: key
        integer :: val

        integer :: index
        type(t_Entity), pointer :: curentity

        index = mod(key%gethash(), self%size) + 1

        if (.not. associated(self%entities(index)%ref)) then
            val = -1
            return
        end if

        curentity => self%entities(index)%ref
        if (curentity%key%equals(key)) then
            val = curentity%value
            return
        end if

        do while (associated(curentity%next))
            curentity => curentity%next
            if (curentity%key%equals(key)) then
                val = curentity%value
                return
            end if
        end do

        val = -1
    end function

    subroutine dictionary_set(self, key, value)
        class(t_Dictionary), intent(inout), target :: self
        class(t_Key), intent(in) :: key
        integer, intent(in) :: value

        integer :: index
        type(t_Entity), pointer :: curentity
        type(t_Entity), pointer :: newentity

        allocate(newentity)
        newentity%key = key
        newentity%value = value
        newentity%next => null()

        index = mod(key%gethash(), self%size) + 1
        if (associated(self%entities(index)%ref)) then
            curentity => self%entities(index)%ref
            do while (associated(curentity%next))
                curentity => curentity%next
            end do
            curentity%next => newentity
        else
            self%entities(index)%ref => newentity
        end if
    end subroutine

end module
