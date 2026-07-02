module dead_weight
    use Lagrange, only: LagPol
    use GQInt, only: intGQ
    implicit none
    private
    public :: get_dead_weight

    real(8), allocatable :: g_areas(:)
    real(8), allocatable :: g_px(:)
    real(8) :: g_rho
    real(8) :: g_gravity  ! Acceleration of gravity
contains
    subroutine get_dead_weight(rho, length, px, areas, gravity)
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        real(8), intent(in) :: rho  ! Mass density
        real(8), intent(in) :: length  ! The length of element
        real(8), intent(in) :: areas(:)  ! Area
        real(8), intent(in) :: px(:)  ! x points where the area is calculated
        real(8), optional, intent(in) :: gravity  ! Acceleration of gravity

        ! Calc
        real(8) :: weight ! Total weight of element

        ! =========================================================================================
        ! Vars initialize
        ! =========================================================================================
        if (present(gravity)) then
            g_gravity = gravity
        else
            g_gravity = 9.80665d+00
        end if

        g_areas = areas
        g_px = px
        g_rho = rho

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        weight = intGQ(0d0, length, q, 4)
        print *, weight



    end subroutine get_dead_weight

    pure function q(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = LagPol(g_px, g_areas, x) * g_rho
    end function q
end module dead_weight
