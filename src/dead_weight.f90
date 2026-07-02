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
    subroutine get_dead_weight(rho, dx, dy, px, areas, gravity)
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        real(8), intent(in) :: rho  ! Mass density
        real(8), intent(in) :: dx, dy  ! Delta x and y to get a element vector
        real(8), intent(in) :: areas(:)  ! Area
        real(8), intent(in) :: px(:)  ! x points where the area is calculated
        real(8), optional, intent(in) :: gravity  ! Acceleration of gravity

        ! Calc
        real(8) :: weight  ! Total weight of element
        real(8) :: length  ! Length of element
        real(8) :: element_dir(2)  ! Unit vector to gravity direction
        real(8) :: gravity_dir(2)  ! Direction of gravity
        real(8) :: force(2)
        real(8) :: force_nor(2)
        real(8) :: force_per(2)

        ! Auxiliary
        ! =========================================================================================
        ! Vars initialization
        ! =========================================================================================
        if (present(gravity)) then
            g_gravity = gravity
        else
            g_gravity = 9.80665d+00
        end if

        g_areas = areas
        g_px = px
        g_rho = rho

        gravity_dir = [0d0, -1d0]

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        length = sqrt(dx**2 + dy**2)
        element_dir(1) = dx / length
        element_dir(2) = dy / length

        weight = intGQ(0d0, length, q, 4)
        force =  intGQ(0d0, length, q, 4) * gravity_dir

        force_nor = dot_product(force, element_dir) * element_dir
        force_per = force - force_nor
    end subroutine get_dead_weight

    pure function q(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = LagPol(g_px, g_areas, x) * g_rho * g_gravity
    end function q
end module dead_weight
