module file_to_structure
    use table, only: show_table
    implicit none

    private

    public :: get_structure_data

contains
    subroutine open_data_file(file_name,  file_unit)
        ! File vars
        character(*), intent(in) :: file_name  ! File name
        integer, intent(out) :: file_unit  ! Unit to file
        character(7), parameter :: data_folder = './data/'  ! Data file location
        character(4), parameter :: file_extension = '.dat'  ! Data file extension
        integer :: file_stat  ! State of file
        character(30) :: file_error  ! Message to file error
        character(30) :: file_path  ! Complete path to file


        ! Open ************************************************************************************
        file_path = data_folder // trim(file_name) // file_extension
        open(newUnit=file_unit, &
            file=file_path, &
            status='old', &
            action='read', &
            ioStat=file_stat, &
            ioMsg=file_error)


        ! Error ***********************************************************************************
        if ( file_stat /= 0) then
            print *, 'State: ', file_stat
            print *, 'MSG: ', file_error
            error stop 'File open'
        end if
    end subroutine open_data_file

    subroutine get_structure_data()
        ! PURPOSE: Get da data structure from file structure.dat

        ! I/O vars
        logical :: debug = .true.  ! If the debug output will be show

        ! File vars
        character(7), parameter :: data_folder = './data/'  ! Data file location
        character(4), parameter :: file_extension = '.dat'  ! Data file extension
        integer :: file_unit  ! Unit to file

        ! Read vars
        integer :: read_stat  ! State of current read

        ! Data vars
        integer :: nno  ! Number of nodes
        integer :: nel  ! Number of elements
        integer :: ndofn  ! Number of degrees of freedom per node
        integer :: nmat  ! Number of materials
        integer :: nsec  ! Number of sections

        real(8), allocatable :: materials(:, :)
        real(8), allocatable :: sections(:, :)
        real(8), allocatable :: nodes(:, :)
        integer, allocatable :: bars(:, :)

        ! Control vars
        integer :: i
        integer :: id  ! Object ID
        character(20) :: line_label

        ! Temp vars
        integer :: temp_int

        ! =========================================================================================
        ! CONTROLS
        ! =========================================================================================
        ! Open ************************************************************************************
        call open_data_file('controls', file_unit)

        ! Read ************************************************************************************
        CONTROLS: do
            read(file_unit, *, ioStat=read_stat) line_label, temp_int

            if (read_stat == 0) then
                select case (line_label)
                    case ('nno')
                        nno = temp_int
                    case ('nel')
                        nel = temp_int
                    case ('ndofn')
                        ndofn = temp_int
                    case ('nmat')
                        nmat = temp_int
                    case ('nsec')
                        nsec = temp_int
                end select
            else if (read_stat == -1) then
                exit CONTROLS
            else
                write(*, *) 'Read stat:', read_stat
                error stop 'Error in CONTROLS read'
            end if
        end do CONTROLS

        ! Close ***********************************************************************************
        close(file_unit)

        ! =========================================================================================
        ! MATERIALS
        ! =========================================================================================
        ! Allocation ******************************************************************************
        allocate(materials(nmat, 2))

        ! Open ************************************************************************************
        call open_data_file('materials', file_unit)

        ! Read ************************************************************************************
        do id = 1, nmat
            read(file_unit, *) materials(id, 1), materials(id, 2)
        end do

        ! Close ***********************************************************************************
        close(file_unit)

        ! =========================================================================================
        ! SECTIONS
        ! =========================================================================================
        ! Allocation ******************************************************************************
        allocate(sections(nsec, 2))

        ! Open ************************************************************************************
        call open_data_file('sections', file_unit)

        ! Read ************************************************************************************
        do id = 1, nsec
            read(file_unit, *) sections(id, 1), sections(id, 2)
        end do

        ! Close ***********************************************************************************
        close(file_unit)

        ! =========================================================================================
        ! NODES
        ! =========================================================================================
        ! Allocation ******************************************************************************
        allocate(nodes(nno, 2))

        ! Open ************************************************************************************
        call open_data_file('nodes', file_unit)

        ! Read ************************************************************************************
        do id = 1, nno
            read(file_unit, *) nodes(id, 1), nodes(id, 2)
        end do

        ! Close ***********************************************************************************
        close(file_unit)

        ! =========================================================================================
        ! BARS
        ! =========================================================================================
        ! Allocation ******************************************************************************
        allocate(bars(nel, 4))

        ! Open ************************************************************************************
        call open_data_file('bars', file_unit)

        ! Read ************************************************************************************
        do id = 1, nel
            read(file_unit, *) bars(id, 1), bars(id, 2), bars(id, 3), bars(id, 4)
        end do

        ! Close ***********************************************************************************
        close(file_unit)

        ! =========================================================================================
        ! Debug
        ! =========================================================================================
100     format(1A5, ':', 1I20)
        if ( debug ) then
            ! Title *******************************************************************************
            do i = 1, 100
                write(*, '(A)', advance='no') '='
            end do

            write(*, '(/, A)') 'Debug'

            do i = 1, 100
                write(*, '(A)', advance='no') '='
            end do
            write(*, *)

            ! Controls ****************************************************************************
            write(*, '(A9)', advance='no') 'CONTROLS '

            do i = 1, 91
                write(*, '(A1)', advance='no') '*'
            end do
            print *


            write(*, 100) 'nno', nno
            write(*, 100) 'nel', nel
            write(*, 100) 'ndofn', ndofn
            write(*, 100) 'nmat', nmat
            write(*, 100) 'nsec', nsec
            write(*, *)

            ! Materials ***************************************************************************
            write(*, '(A9)', advance='no') 'MATERIALS '

            do i = 1, 91
                write(*, '(A1)', advance='no') '*'
            end do
            print *
            call show_table(materials, 100, precision=5, titles=[character(len=3) :: 'E', 'rho'])
            print *

            ! SECTIONS ****************************************************************************
            write(*, '(A9)', advance='no') 'SECTIONS '

            do i = 1, 91
                write(*, '(A1)', advance='no') '*'
            end do
            print *
            call show_table(sections, &
                100, &
                precision=5, &
                titles=[character(len=7) :: 'Area', 'Inertia'])
            print *

            ! NODES *******************************************************************************
            write(*, '(A9)', advance='no') 'NODES '

            do i = 1, 91
                write(*, '(A1)', advance='no') '*'
            end do
            print *
            call show_table(nodes, &
                100, &
                precision=2, &
                titles=[character(len=7) :: 'x', 'y'])
            print *

            ! Bars ********************************************************************************
            write(*, '(A9)', advance='no') 'BARS '

            do i = 1, 91
                write(*, '(A1)', advance='no') '*'
            end do
            print *
            call show_table(bars, &
                100, &
                titles=[character(len=10) :: 'Material', 'Section', 'Start Node', 'End Node'])
            print *
        end if
    end subroutine get_structure_data
end module file_to_structure
