!> @brief SIC法のメイン処理モジュール
module sic
    use parameters
    use commons
    use m_hfiter
    use m_vector3d
    use myrandom
    use m_halfedge, only: halfedge_register_face, halfedge_collect_edges
    use m_meshfile

    implicit none

    private

    public sic_init
    public sic_correct_temprature
    public sic_refinement
    public sic_scatter_on_grid
    public sic_update

contains

    !> @brief simplexを初期化する.
    subroutine sic_init
        integer ispec

        integer it1, it2, it3
        integer i, j
        type(Vector2d) :: pos
        type(Vector3d) :: vel
        double precision :: vx, vy
        integer :: npcl_prev

        npcl = 0
        nsimp = 0

        call create_mesh(17, 'meshe.msh', 1)
        call create_mesh(17, 'meshi.msh', 2)

        call halfedge_collect_edges

        call sic_correct_temprature
    end subroutine

    !> @brief 粒子電荷をグリッドに分配する.
    subroutine sic_scatter_on_grid
        integer :: ispec, isimp
        double precision :: offsets(2)

        double precision :: moment

        rho%values(:, :) = 0.0d0

        offsets = [-0.5d0*dx, -0.5d0*dx]
        do isimp = 1, nsimp
            moment = simplicies(isimp)%q
            call simplicies(isimp)%scatter(moment, rho, offsets)
        end do

        rho%values(nx, :) = rho%values(0, :)
        rho%values(:, ny) = rho%values(:, 0)
    end subroutine

    !> @brief トレーサー位置・速度を更新する.
    subroutine sic_update
        integer :: ispec, itracer, isimp

        type(Vector3d) :: ef_simp
        double precision :: offsets(2)

        offsets = [-0.5d0*dx, -0.5d0*dx]
        !$omp do private(ef_simp)
        do isimp = 1, nsimp
            ! Simplexに働く平均電場を求める
            ef_simp = simplicies(isimp)%gather_vector(ef, offsets)
            simplicies(isimp)%ef = ef_simp
        end do
        !$omp end do

        !$omp do
        do itracer = 1, npcl
            call tracers(itracer)%update(dt)
        end do
        !$omp end do
    end subroutine

    !> 設定した粒子温度になるように速度を補正する.
    subroutine sic_correct_temprature
        double precision :: npcl_spec(nspec)
        double precision :: Ts_current(nspec)
        double precision :: energies(nspec)
        double precision :: xs(nspec)

        integer :: isimp, itracer
        integer :: ispec

        type(t_Tracer), pointer :: trc
        type(t_VertexAroundFaceIter) :: iter

        type(Vector3d) :: randvec
        type(Vector3d), allocatable :: vbuf(:)

        allocate (vbuf(npcl))

        ! トレーサーの速度に摂動を与える
        do itracer = 1, npcl
            trc => tracers(itracer)
            vbuf(itracer) = trc%velocity

            ! randvec = rand3dxy()
            ! trc%velocity = randvec

            trc%velocity = Vector3d(sin(2*pi*trc%position%x/(dx*nx)), &
                                    sin(2*pi*trc%position%y/(dx*nx)), &
                                    0.0d0)
        end do

        ! 各粒子種の運動エネルギーの和を計算する.
        energies(:) = 0
        npcl_spec(:) = 0
        do isimp = 1, nsimp
            ispec = simplicies(isimp)%ispec

            npcl_spec(ispec) = npcl_spec(ispec) + simplicies(isimp)%npcl
            energies(ispec) = energies(ispec) + simplicies(isimp)%energy()
        end do

        ! 温度補正係数を計算する.
        do ispec = 1, nspec
            if (Ts(ispec) == 0.0) then
                xs(ispec) = 0
                cycle
            end if

            ! kBT = 1/2mv^2より、温度を求める.
            Ts_current(ispec) = energies(ispec)/npcl_spec(ispec)/kB

            xs(ispec) = sqrt(Ts(ispec)/Ts_current(ispec))
        end do

        ! トレーサーの速度を補正する
        do itracer = 1, npcl
            ispec = tracers(itracer)%ispec

            tracers(itracer)%velocity = tracers(itracer)%velocity*xs(ispec)
        end do

        do itracer = 1, npcl
            tracers(itracer)%velocity = tracers(itracer)%velocity + vbuf(itracer)
        end do
    end subroutine

    subroutine sic_split_simplex(isimp)
        integer, intent(in) :: isimp

        integer :: ispec
        double precision :: q_new, m_new, npcl_new

        type(t_VertexAroundFaceIter) :: viter
        integer :: it1, it2, it3
        type(Vector2d) :: pos1, pos2, pos3, newpos
        type(Vector2d) :: dpos1, dpos2, dpos3
        type(Vector3d) :: vel1, vel2, vel3, newvel
        double precision :: r1, r2, r3

        viter = t_VertexAroundFaceIter(simplicies(isimp)%iedge)
        it1 = viter%next()
        it2 = viter%next()
        it3 = viter%next()

        ! 新しいトレーサーの位置は重心とする.
        pos1 = tracers(it1)%position
        pos2 = tracers(it2)%position
        pos3 = tracers(it3)%position
        newpos = (pos1 + pos2 + pos3)/3.0d0

        ! 新しいトレーサーの速度は線形補間で求める.
        vel1 = tracers(it1)%velocity
        vel2 = tracers(it2)%velocity
        vel3 = tracers(it3)%velocity
        newvel = (vel1 + vel2 + vel3)/3.0d0

        tracers(npcl + 1)%position = newpos
        tracers(npcl + 1)%velocity = newvel
        tracers(npcl + 1)%ispec = simplicies(isimp)%ispec

        ! 分割後のSimplexを設定する
        ispec = simplicies(isimp)%ispec
        q_new = simplicies(isimp)%q/3
        m_new = simplicies(isimp)%m/3
        npcl_new = simplicies(isimp)%npcl/3

        simplicies(isimp)%ispec = ispec
        simplicies(isimp)%q = q_new
        simplicies(isimp)%m = m_new
        simplicies(isimp)%q_m = q_new/m_new
        simplicies(isimp)%npcl = npcl_new

        simplicies(nsimp + 1)%ispec = ispec
        simplicies(nsimp + 1)%q = q_new
        simplicies(nsimp + 1)%m = m_new
        simplicies(nsimp + 1)%q_m = q_new/m_new
        simplicies(nsimp + 1)%npcl = npcl_new

        simplicies(nsimp + 2)%ispec = ispec
        simplicies(nsimp + 2)%q = q_new
        simplicies(nsimp + 2)%m = m_new
        simplicies(nsimp + 2)%q_m = q_new/m_new
        simplicies(nsimp + 2)%npcl = npcl_new

        ! ハーフエッジを更新する
        call halfedge_register_face_split(isimp, npcl + 1, nsimp + 1, nsimp + 2)

        npcl = npcl + 1
        nsimp = nsimp + 2
    end subroutine

    !> @brief Refinementを適用する.
    !>
    !> @details トレーサー間がしきい値より離れた場合そのsimplexをしきい値未満になるまで分割する.
    !>
    !> @param[in] threshold しきい値
    subroutine sic_refinement(threshold)
        double precision, intent(in) :: threshold
        integer :: isimp

        isimp = 1
        do while (isimp <= nsimp)
            if (nsimp + 2 > max_npcl) then
                exit
            end if

            if (simplicies(isimp)%area() > threshold) then
                call sic_split_simplex(isimp)

                ! 分割後のsimplexをチェックするためcontinue
                continue
            end if

            isimp = isimp + 1
        end do
    end subroutine

end module
