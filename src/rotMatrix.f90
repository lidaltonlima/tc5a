module rotMatrix
    ! Rotation Matrix
    implicit none
    private

    public :: getRotMat
contains
    pure function getRotMat(xl, yl, xg, yg) result(rotMat)
        real(8), intent(in) :: xl(2), yl(2) ! Local system vectors
        real(8), intent(in) :: xg(2), yg(2) ! Global system vectors
        real(8) :: cos_xx ! cos(x', x)
        real(8) :: cos_xy ! cos(x', y)
        real(8) :: cos_yx ! cos(y', x)
        real(8) :: cos_yy ! cos(y', y)
        real(8) :: rotMat(2, 2) ! Rotation Matrix

        cos_xx =  dot_product(xl, xg)
        cos_xy =  dot_product(xl, yg)
        cos_yx =  dot_product(yl, xg)
        cos_yy =  dot_product(yl, yg)

        rotMat(1, :) = [cos_xx, cos_xy]
        rotMat(2, :) = [cos_yx, cos_yy]
    end function getRotMat
end module rotMatrix
