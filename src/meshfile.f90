module m_meshfile
    use commons
    use m_halfedge
    use m_str
    use myrandom
    implicit none

    type t_VertexData
        integer :: index
        double precision, allocatable :: position(:)
        double precision, allocatable :: velocity(:)
    end type

    type t_FaceData
        integer :: index
        logical :: periodic
        integer, allocatable :: iverts(:)
        integer, allocatable :: iverts_pr(:, :)
        double precision :: rate
    end type

contains

    subroutine create_mesh(unit, filename, ispec)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: filename
        integer, intent(in) :: ispec

        integer :: nverts, nfaces

        integer :: index

        double precision :: x, y, vx, vy

        integer :: periodic, n
        double precision :: rate
        integer :: iverts(20)

        integer :: itracer, isimp
        integer :: nverts_org, nfaces_org
        integer :: i, j

        print *, 'load mesh file (', filename, ')...'

        nverts_org = npcl
        nfaces_org = nsimp

        open (unit, file=filename, status='old')
        read (unit, *) nverts, nfaces
        npcl = npcl + nverts
        nsimp = nsimp + nfaces

        write (*, fmt='(a)', advance='no'), 'load vertices('//str(nverts)//')...'
        do i = 1, nverts
            read (unit, *) index, x, y, vx, vy

            ! call myrandom_rand_bm2(vx, vy)
            ! x = x + vx
            ! y = y + vy
            ! call myrandom_rand_bm2(vx, vy)

            itracer = nverts_org + index
            tracers(itracer)%ispec = ispec
            tracers(itracer)%position%x = x * dx * nx
            tracers(itracer)%position%y = y * dx * ny
            tracers(itracer)%velocity%x = vx
            tracers(itracer)%velocity%y = vy
        end do
        print *, 'done'

        write (*, fmt='(a)', advance='no'), 'load faces('//str(nfaces)//')...'
        do i = 1, nfaces
            read (unit, *) index, periodic, rate, n, (iverts(j), j=1, n)

            do j = 1, n
                iverts(j) = iverts(j) + nverts_org
            end do

            isimp = nfaces_org + index
            simplicies(isimp) = t_Simplex( &
                                ispec, &
                                npcl_per_super*rate, &
                                qs(ispec)*rate, &
                                ms(ispec)*rate &
                                )
            if (periodic == 1) then
                call halfedge_register_face_periodic(isimp, iverts(1:n))
            else
                call halfedge_register_face(isimp, iverts(1:3))
            end if
        end do
        print *, 'done'

        close(unit)
    end subroutine
end module
