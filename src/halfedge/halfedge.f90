module m_halfedge
    use m_vectors
    use m_edgedict

    implicit none

    private

    public t_Vertex
    public t_Halfedge
    public t_Face

    public vertices
    public nedge
    public halfedges
    public faces

    public halfedge_init
    public halfedge_register_face
    public halfedge_register_face_periodic
    public halfedge_register_face_split
    public halfedge_collect_edges

    type t_Vertex
        type(Vector2d) :: position
        integer :: iedge
        integer :: id = -1
    end type

    type t_Halfedge
        integer :: istart
        integer :: ipair
        integer :: inext
        integer :: iprev
        integer :: iface
    end type

    type t_Face
        integer :: iedge
    end type

    interface t_Vertex
        module procedure init_vertex
    end interface

    interface t_Halfedge
        module procedure init_halfedge
    end interface

    interface t_Face
        module procedure init_face
    end interface

    class(t_Vertex), pointer :: vertices(:)
    class(t_Face), pointer :: faces(:)

    integer :: nedge = 0
    type(t_Halfedge), allocatable :: halfedges(:)

    type(t_Dictionary) :: edgedict

    integer :: uid_ = 0

contains

    function get_uid()
        integer :: get_uid

        get_uid = uid_
        uid_ = uid_ + 1
    end function

    subroutine halfedge_init(pvertices, pfaces)
        class(t_Vertex), target, intent(in) :: pvertices(:)
        class(t_Face), target, intent(in) :: pfaces(:)

        vertices => pvertices
        faces => pfaces

        allocate (halfedges(size(vertices)*3))

        edgedict = t_Dictionary()
    end subroutine

    function init_vertex(position, iedge) result(obj)
        type(t_Vertex) :: obj
        type(Vector2d), intent(in), optional :: position
        integer, intent(in), optional :: iedge

        if (present(position)) obj%position = position
        if (present(iedge)) obj%iedge = iedge
    end function

    function init_halfedge(istart, ipair, inext, iprev, iface) result(obj)
        type(t_Halfedge) :: obj
        integer, intent(in), optional :: istart
        integer, intent(in), optional :: ipair
        integer, intent(in), optional :: inext
        integer, intent(in), optional :: iprev
        integer, intent(in), optional :: iface

        if (present(istart)) obj%istart = istart
        if (present(ipair)) obj%ipair = ipair
        if (present(inext)) obj%inext = inext
        if (present(iprev)) obj%iprev = iprev
        if (present(iface)) obj%iface = iface
    end function

    function init_face(iedge) result(obj)
        type(t_Face) :: obj
        integer, intent(in), optional :: iedge

        if (present(iedge)) obj%iedge = iedge
    end function

    subroutine halfedge_register_face(iface, iverts)
        integer, intent(in) :: iface
        integer, intent(in) :: iverts(:)

        integer :: ivert1, ivert2, ivert3

        integer :: iedge1, iedge2, iedge3
        integer :: uid1, uid2, uid3

        ivert1 = iverts(1)
        ivert2 = iverts(2)
        ivert3 = iverts(3)

        iedge1 = nedge + 1
        iedge2 = nedge + 2
        iedge3 = nedge + 3
        nedge = nedge + 3

        halfedges(iedge1) = t_Halfedge(ivert1, -1, iedge2, iedge3, iface)
        halfedges(iedge2) = t_Halfedge(ivert2, -1, iedge3, iedge1, iface)
        halfedges(iedge3) = t_Halfedge(ivert3, -1, iedge1, iedge2, iface)

        faces(iface)%iedge = iedge1
        vertices(ivert1)%iedge = iedge1
        vertices(ivert2)%iedge = iedge2
        vertices(ivert3)%iedge = iedge3

        uid1 = get_uid()
        uid2 = get_uid()
        uid3 = get_uid()

        vertices(ivert1)%id = uid1
        vertices(ivert2)%id = uid2
        vertices(ivert3)%id = uid3

        call edgedict%set(t_Key(uid1, uid2), iedge1)
        call edgedict%set(t_Key(uid2, uid3), iedge2)
        call edgedict%set(t_Key(uid3, uid1), iedge3)
    end subroutine

    !> Register a face across a periodic boundary.
    subroutine halfedge_register_face_periodic(iface, iverts_list)
        integer, intent(in) :: iface
        integer, intent(in) :: iverts_list(:)

        integer :: iedge1, iedge2, iedge3
        integer :: ivert1, ivert2, ivert3
        integer :: i
        integer :: uid1, uid2, uid3

        iedge1 = nedge + 1
        iedge2 = nedge + 2
        iedge3 = nedge + 3
        nedge = nedge + 3

        ivert1 = iverts_list(1)
        ivert2 = iverts_list(2)
        ivert3 = iverts_list(3)

        halfedges(iedge1) = t_Halfedge(ivert1, -1, iedge2, iedge3, iface)
        halfedges(iedge2) = t_Halfedge(ivert2, -1, iedge3, iedge1, iface)
        halfedges(iedge3) = t_Halfedge(ivert3, -1, iedge1, iedge2, iface)

        faces(iface)%iedge = iedge1

        uid1 = get_uid()
        uid2 = get_uid()
        uid3 = get_uid()

        call edgedict%set(t_Key(uid1, uid2), iedge1)
        call edgedict%set(t_Key(uid2, uid3), iedge2)
        call edgedict%set(t_Key(uid3, uid1), iedge3)

        do i = 1, size(iverts_list)/3
            ivert1 = iverts_list((i - 1)*3 + 1)
            ivert2 = iverts_list((i - 1)*3 + 2)
            ivert3 = iverts_list((i - 1)*3 + 3)

            vertices(ivert1)%iedge = iedge1
            vertices(ivert2)%iedge = iedge2
            vertices(ivert3)%iedge = iedge3

            vertices(ivert1)%id = uid1
            vertices(ivert2)%id = uid2
            vertices(ivert3)%id = uid3
        end do
    end subroutine

    subroutine halfedge_register_face_split(iface, ivert, iface2, iface3)
        integer, intent(in) :: iface
        integer, intent(in) :: ivert
        integer, intent(in) :: iface2
        integer, intent(in) :: iface3

        integer :: ivert1, ivert2, ivert3
        integer :: iedge1, iedge2, iedge3

        iedge1 = faces(iface)%iedge
        iedge2 = halfedges(iedge1)%inext
        iedge3 = halfedges(iedge2)%inext

        ivert1 = halfedges(iedge1)%istart
        ivert2 = halfedges(iedge2)%istart
        ivert3 = halfedges(iedge3)%istart

        faces(iface2)%iedge = iedge2
        faces(iface3)%iedge = iedge3

        halfedges(iedge1)%inext = nedge+2
        halfedges(iedge1)%iprev = nedge+1

        halfedges(iedge2)%inext = nedge+4
        halfedges(iedge2)%iprev = nedge+3
        halfedges(iedge2)%iface = iface2

        halfedges(iedge3)%inext = nedge+6
        halfedges(iedge3)%iprev = nedge+5
        halfedges(iedge3)%iface = iface3

        halfedges(nedge + 1) = t_Halfedge(istart=ivert, ipair=nedge + 6, inext=iedge1, iprev=nedge + 2, iface=iface)
        halfedges(nedge + 2) = t_Halfedge(istart=ivert2, ipair=nedge + 3, inext=nedge + 1, iprev=iedge1, iface=iface)
        halfedges(nedge + 3) = t_Halfedge(istart=ivert, ipair=nedge + 2, inext=iedge2, iprev=nedge + 4, iface=iface2)
        halfedges(nedge + 4) = t_Halfedge(istart=ivert3, ipair=nedge + 5, inext=nedge + 3, iprev=iedge2, iface=iface2)
        halfedges(nedge + 5) = t_Halfedge(istart=ivert, ipair=nedge + 4, inext=iedge3, iprev=nedge + 6, iface=iface3)
        halfedges(nedge + 6) = t_Halfedge(istart=ivert1, ipair=nedge + 1, inext=nedge + 5, iprev=iedge3, iface=iface3)

        nedge = nedge + 6
    end subroutine

    ! ハーフエッジのペアを登録する
    subroutine halfedge_collect_edges
        integer :: iedge, jedge

        integer :: ivert1, ivert2
        integer :: uid1, uid2

        do iedge = 1, nedge
            ivert1 = halfedges(iedge)%istart
            ivert2 = halfedges(halfedges(iedge)%inext)%istart
            uid1 = vertices(ivert1)%id
            uid2 = vertices(ivert2)%id
            halfedges(iedge)%ipair = edgedict%get(t_Key(uid2, uid1))
        end do
    end subroutine

end module
