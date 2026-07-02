program main
    use StructureData, only: get_structure_data
    use Stiffness, only: get_kl
    use DeadWeight, only: get_dead_weight
    implicit none

    ! =============================================================================================
    ! Vars statement
    ! =============================================================================================
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
    logical, parameter :: debug = .false.

    ! =============================================================================================
    ! Calculation
    ! =============================================================================================
    call get_structure_data(nno, nel, ndofn, nmat, nsec, &
        materials, sections, nodes, bars,debug=.true.)

    kl = get_kl(nel, ndofn, materials, sections, nodes, bars)

    call get_dead_weight(rho=7850d0, dx=5d0, dy=0d0, px=[0d0, 2.5d0, 5d0], areas=[0.05d0, 0.0825d0, 0.12d0])

    ! =============================================================================================
    ! Debug
    ! =============================================================================================
    if ( debug ) then
        do id = 1, nel
            write(*, *) 'Element ID: ', id
            do i = 1, 2 * ndofn
                do j = 1, 2 * ndofn
                    write(*, '(ES15.4)', advance='no') kl(id, i, j)
                end do
                print *
            end do
        end do
    end if
end program main
