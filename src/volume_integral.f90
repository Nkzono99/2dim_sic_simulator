module volume_integral
    use m_vector2d
    use mymath
    use m_hfiter
    implicit none

    integer, parameter :: MAX_VERTS = 10

    type t_Vertex
        type(Vector2d) :: pos
        integer :: neighbors(2)
    end type
    interface t_Vertex
        module procedure init_vertex
    end interface

    type t_Poly
        type(t_Vertex) :: verts(MAX_VERTS)
        integer :: nverts

    contains
        procedure :: clip => c_clip
        procedure :: clip_with_plane => c_clip_with_plane
        procedure :: area => c_area
    end type
    interface t_Poly
        module procedure init_poly
    end interface

    type t_Plane
        double precision :: d
        type(Vector2d) :: n
    end type
    interface t_Plane
        module procedure init_plane
    end interface

contains

    function init_vertex(pos) result(obj)
        type(Vector2d), intent(in) :: pos
        type(t_Vertex) :: obj

        obj%pos = pos
        obj%neighbors = (/-1, -1/)
    end function

    function init_poly(verts) result(obj)
        type(t_Vertex), intent(in) :: verts(:)
        type(t_Poly) :: obj
        integer :: i

        obj%nverts = size(verts)

        do i = 1, obj%nverts
            obj%verts(i)%pos = verts(i)%pos
            obj%verts(i)%neighbors(1) = mymath_pmod((i-1) - 1, obj%nverts) + 1
            obj%verts(i)%neighbors(2) = mymath_pmod((i-1) + 1, obj%nverts) + 1
        end do
    end function

    function init_plane(v1, v2) result(obj)
        type(Vector2d), intent(in) :: v1, v2
        type(t_Plane) :: obj

        obj%n%x = v2%y - v1%y
        obj%n%y = v1%x - v2%x
        obj%d = -obj%n%dot(v1)
    end function

    subroutine c_clip(self, planes)
        class(t_Poly), intent(inout) :: self
        type(t_Plane), intent(in) :: planes(:)

        integer :: i

        do i = 1, size(planes)
            call self%clip_with_plane(planes(i))
        end do

    end subroutine

    subroutine c_clip_with_plane(self, plane)
        class(t_Poly), intent(inout), target :: self
        type(t_Plane), intent(in) :: plane

        integer :: nverts_org
        double precision :: smin, smax
        double precision :: sdist(MAX_VERTS)
        logical :: clipped(MAX_VERTS)
        integer :: indices(MAX_VERTS)

        integer :: i, j, k
        double precision :: d, r1, r2
        type(t_Vertex), pointer :: vert, vcur, vnxt
        integer :: i1, i2

        type(Vector2d) :: pnew
        type(t_Vertex), pointer :: vnew

        integer :: nunclipped

        ! 元の頂点数を保存する.
        nverts_org = self%nverts

        ! 頂点数が0のとき終了する.
        if (nverts_org == 0) then
            return
        end if

        ! 頂点が面の裏表どちらに位置するか判定する.(clipped=.true.なら表)
        smin = 1.0d30
        smax = -1.0d30
        sdist(:) = 0.0d0
        clipped(:) = .false.

        do i = 1, self%nverts
            vert => self%verts(i)
            d = plane%d + vert%pos%dot(plane%n)
            sdist(i) = d
            if (d < smin) smin = d
            if (d > smax) smax = d
            if (d > 0.0) clipped(i) = .true.
        end do

        ! すべての頂点が表側に位置するなら頂点をすべて削除して終了する.
        if (smin >= 0) then
            self%nverts = 0
            return
        end if
        ! すべての頂点が裏側に位置するなら終了する.
        if (smax <= 0) then
            return
        end if

        ! 頂点間に面が存在する場合に新しい頂点を挿入する.
        do i = 1, nverts_org
            ! 表側の頂点の場合は何もしない.
            if (clipped(i)) cycle

            vcur => self%verts(i)

            ! 裏側の頂点と表側の頂点がつながっている場合、新しい頂点を挿入する.
            do k = 1, 2
                j = vcur%neighbors(k)
                vnxt => self%verts(j)

                ! 裏-裏の繋がりの場合なにもしない.
                if (.not. clipped(j)) cycle

                ! 面と辺の交点を計算する.
                r1 = sdist(i)
                r2 = -sdist(j)
                pnew = (r2*vcur%pos + r1*vnxt%pos)/(r1 + r2)

                ! 新しい頂点を追加し、リンクを貼る.
                self%nverts = self%nverts + 1

                vnew => self%verts(self%nverts)
                vnew = t_Vertex(pnew)
                vnew%neighbors(3-k) = i
                vnew%neighbors(k) = -1

                vcur%neighbors(k) = self%nverts
            end do
        end do

        ! 新しく挿入した頂点同士をつなげる.
        if (self%nverts > nverts_org) then
            i1 = nverts_org+1
            i2 = nverts_org+2

            if (self%verts(i1)%neighbors(1) == -1) then
                self%verts(i1)%neighbors(1) = i2
                self%verts(i2)%neighbors(2) = i1
            else
                self%verts(i1)%neighbors(2) = i2
                self%verts(i2)%neighbors(1) = i1
            end if
        end if

        ! 表側に位置する頂点(clipped==.true.)を配列から削除する.
        ! 裏側に位置する頂点を配列の左詰めにする.
        nunclipped = 0
        do i = 1, self%nverts
            if (.not. clipped(i)) then
                nunclipped = nunclipped + 1
                self%verts(nunclipped) = self%verts(i)
                indices(i) = nunclipped
            end if
        end do

        ! 配列のインデックスが変化した頂点のリンクを張り直す.
        self%nverts = nunclipped
        do i = 1, self%nverts
            vcur => self%verts(i)
            vcur%neighbors(1) = indices(vcur%neighbors(1))
            vcur%neighbors(2) = indices(vcur%neighbors(2))
        end do
    end subroutine

    function c_area(self)
        class(t_Poly), target :: self
        double precision :: c_area

        type(t_Vertex), pointer :: vcur, vnxt

        integer i, j

        c_area = 0
        ! 頂点を一つも持たないなら面積は0
        if (self%nverts == 0) return

        do i = 1, self%nverts
            ! 頂点を取得する
            vcur => self%verts(i)

            ! 隣接する頂点を取得する
            j = vcur%neighbors(2)
            vnxt => self%verts(j)

            ! 外積の総和を取る
            c_area = c_area + vcur%pos%cross(vnxt%pos)
        end do

        c_area = 0.5*c_area

        ! 頂点が時計回りでも反時計回りでも面積は正の値を取るため絶対値を取る
        c_area = abs(c_area)
    end function

    function triangle(v1, v2, v3)
        type(Vector2d), intent(in) :: v1, v2, v3
        type(t_Plane) :: triangle(3)

        triangle(1) = t_Plane(v1, v2)
        triangle(2) = t_Plane(v2, v3)
        triangle(3) = t_Plane(v3, v1)
    end function

    function gridbox(leftdown, width)
        type(Vector2d), intent(in) :: leftdown
        double precision :: width
        type(t_Poly) :: gridbox
        type(t_Vertex) :: verts(4)

        verts(1)%pos = leftdown + Vector2d(0, 0)
        verts(2)%pos = leftdown + Vector2d(width, 0)
        verts(3)%pos = leftdown + Vector2d(width, width)
        verts(4)%pos = leftdown + Vector2d(0, width)

        gridbox = t_Poly(verts)
    end function
end module
