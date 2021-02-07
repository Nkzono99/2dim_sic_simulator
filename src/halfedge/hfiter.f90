module m_hfiter
    use m_halfedge
    use m_iter

    implicit none

    private
    public t_FaceAroundVertexIter
    public t_VertexAroundFaceIter

    type, extends(t_Iter) :: t_FaceAroundVertexIter
        integer :: iedge_origin
        integer :: iedge_current
        logical :: reversed

    contains
        procedure next => faceAroundVertexIter_next
        procedure hasnext => faceAroundVertexIter_hasnext
    end type

    interface t_FaceAroundVertexIter
        module procedure init_FaceAroundVertexIter
    end interface

    type, extends(t_Iter) :: t_VertexAroundFaceIter
        integer :: iedge_origin
        integer :: iedge_current

    contains
        procedure next => vertexAroundFaceIter_next
        procedure hasnext => vertexAroundFaceIter_hasnext
    end type

    interface t_VertexAroundFaceIter
        module procedure init_VertexAroundFaceIter
    end interface

contains

    function init_FaceAroundVertexIter(iedge) result(obj)
        type(t_FaceAroundVertexIter) :: obj
        integer, intent(in) :: iedge

        obj%iedge_origin = iedge
        obj%iedge_current = iedge
        obj%reversed = .false.
    end function

    function faceAroundVertexIter_next(self) result(iface)
        class(t_FaceAroundVertexIter), intent(inout) :: self

        integer :: iface
        integer :: iedge

        iedge = self%iedge_current

        iface = halfedges(iedge)%iface

        ! 次のエッジを見つける
        if (.not. self%reversed) then
            iedge = halfedges(iedge)%ipair

            ! ペアが見つからない場合は反転する
            if (iedge == -1) then
                self%reversed = .true.
                iedge = self%iedge_origin
                iedge = halfedges(iedge)%iprev
                iedge = halfedges(iedge)%ipair
            else
                iedge = halfedges(iedge)%inext
            end if
        else
            iedge = halfedges(iedge)%iprev
            iedge = halfedges(iedge)%ipair
        end if

        self%iedge_current = iedge
    end function

    function faceAroundVertexIter_hasnext(self) result(hasnext)
        class(t_FaceAroundVertexIter), intent(in) :: self
        logical :: hasnext

        if (self%iedge_current == -1) then
            hasnext = .false.
            return
        end if

        ! 頂点周りの辺を一周した場合.false.を返す
        if (self%iedge_current == self%iedge_origin) then
            hasnext = .false.
            return
        end if

        hasnext = .true.
    end function

    function init_VertexAroundFaceIter(iedge) result(obj)
        type(t_VertexAroundFaceIter) :: obj
        integer, intent(in) :: iedge

        obj%iedge_origin = iedge
        obj%iedge_current = iedge
    end function

    function vertexAroundFaceIter_next(self) result(ivert)
        class(t_VertexAroundFaceIter), intent(inout) :: self
        integer :: ivert

        ivert = halfedges(self%iedge_current)%istart

        self%iedge_current = halfedges(self%iedge_current)%inext
    end function

    function vertexAroundFaceIter_hasnext(self) result(hasnext)
        class(t_VertexAroundFaceIter), intent(in) :: self
        logical :: hasnext
        integer :: inext

        inext = halfedges(self%iedge_current)%inext

        hasnext = (inext /= self%iedge_origin)
    end function

end module
