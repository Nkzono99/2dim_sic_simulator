module m_mesh_jsonfile
    use commons
    use m_halfedge
    use json_module
    use m_str

    implicit none

    private
    public create_mesh

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

    subroutine create_mesh(filename, ispec)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: ispec

        type(json_file) :: meshfile
        integer :: nverts, nfaces

        type(t_VertexData) :: vert_data

        logical :: periodic
        type(t_FaceData) :: face_data

        integer :: itracer, isimp
        integer :: nverts_org, nfaces_org
        integer :: i

        print *, 'load mesh file (', filename, ')...'

        nverts_org = npcl
        nfaces_org = nsimp

        call meshfile%initialize()

        call meshfile%load_file(filename)
        ! if (meshfile%failed()) then
        !     stop
        ! end if

        call meshfile%get('nverts', nverts)
        call meshfile%get('nfaces', nfaces)
        npcl = npcl + nverts
        nsimp = nsimp + nfaces

        write (*, fmt='(a)', advance='no'), 'load vertices('//str(nverts)//')...'
        do i = 1, nverts
            call get_vertex(meshfile, i, vert_data)

            itracer = nverts_org + vert_data%index
            tracers(itracer)%ispec = ispec
            tracers(itracer)%position%x = vert_data%position(1)
            tracers(itracer)%position%y = vert_data%position(2)
            tracers(itracer)%velocity%x = vert_data%velocity(1)
            tracers(itracer)%velocity%y = vert_data%velocity(2)
        end do
        print *, 'done'

        write (*, fmt='(a)', advance='no'), 'load faces('//str(nfaces)//')...'
        do i = 1, nfaces
            call get_face(meshfile, i, face_data)

            isimp = nfaces_org + face_data%index
            simplicies(isimp) = t_Simplex( &
                                ispec, &
                                npcl_per_super*face_data%rate, &
                                qs(ispec)*face_data%rate, &
                                ms(ispec)*face_data%rate &
                                )
            if (face_data%periodic) then
                call halfedge_register_face_periodic(isimp, reshape(face_data%iverts, [int(size(face_data%iverts)/3), 3]))
            else
                call halfedge_register_face(isimp, face_data%iverts)
            end if
        end do
        print *, 'done'
    end subroutine

    subroutine get_vertex(meshfile, i, vert_data)
        type(json_file), intent(inout) :: meshfile
        integer, intent(in) :: i
        type(t_VertexData), intent(inout) :: vert_data
        character(:), allocatable :: name

        name = 'vertex_list('//str(i)//').'

        call meshfile%get(name//'index', vert_data%index)
        call meshfile%get(name//'position', vert_data%position)
        call meshfile%get(name//'velocity', vert_data%velocity)
    end subroutine

    subroutine get_face(meshfile, i, face_data)
        type(json_file), intent(inout) :: meshfile
        integer, intent(in) :: i
        type(t_FaceData), intent(inout) :: face_data

        character(:), allocatable :: name

        name = 'face_list('//str(i)//').'

        call meshfile%get(name//'index', face_data%index)
        call meshfile%get(name//'periodic', face_data%periodic)
        call meshfile%get(name//'iverts', face_data%iverts)
        call meshfile%get(name//'rate', face_data%rate)
    end subroutine

end module
