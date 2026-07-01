program main
    use file_to_structure, only: get_structure_data
    use stiffness, only: get_kl
    implicit none

    ! Structure data
    integer :: nno  ! Number of nodes
    integer :: nel  ! Number of elements
    integer :: ndofn  ! Number of degrees of freedom per node
    integer :: nmat  ! Number of materials
    integer :: nsec  ! Number of sections

    real(8), allocatable :: materials(:, :)
    real(8), allocatable :: sections(:, :)
    real(8), allocatable :: nodes(:, :)
    integer, allocatable :: bars(:, :)

    ! Calculate data
    real(8), allocatable :: kl(:, :, :)  ! Stiffness matrix kl(element_id, i, j)

    ! Controls
    integer :: i, j  ! Indexes
    integer :: id   ! Index id

    ! =============================================================================================
    ! Calc
    ! =============================================================================================
    call get_structure_data(nno, nel, ndofn, nmat, nsec, &
        materials, sections, nodes, bars)

    call get_kl(kl, nel, ndofn, &
        materials, sections, nodes, bars)

    ! =============================================================================================
    ! Debug
    ! =============================================================================================
    do id = 1, nel
        write(*, *) 'Element ID: ', id
        do i = 1, 2 * ndofn
            do j = 1, 2 * ndofn
                write(*, '(ES20.4)', advance='no') kl(id, i, j)
            end do
            print *
        end do
    end do
end program main
