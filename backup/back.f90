module file_to_structure
    implicit none
    private

    public :: get_structure_data

contains
    subroutine get_structure_data()
        ! PURPOSE: Get da data structure from file structure.dat

        ! I/O vars
        logical :: debug  ! If the debug output will be show

        ! File vars
        character(7), parameter :: data_folder = './data/'  ! Data file location
        character(4), parameter :: file_extension = '.dat'  ! Data file extension
        integer :: file_unit  ! Unit to file
        integer :: file_stat  ! State of file
        character(30) :: file_error  ! Message to file error
        character(10) :: file_name  ! File name
        character(30) :: file_path  ! Complete path to file

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
        real(8), allocatable :: bars(:, :)

        ! Control vars
        integer :: i, j
        character(9) :: line_label  ! Label of line value
        integer :: line_value  ! Line value
        character(9) :: title  ! Indicates the type of next values

        ! Temp vars
        integer :: temp_int

        if ( debug ) then
            do i = 1, 100
                write(*, '(A)', advance='no') '='
            end do

            write(*, '(/, A)') 'Debug'

            do i = 1, 100
                write(*, '(A)', advance='no') '='
            end do
            write(*, *)
        end if

        ! =========================================================================================
        ! Values Statements
        ! =========================================================================================
        file_name = 'structure'
        file_path = data_folder // trim(file_name) // file_extension
        debug = .true.

        ! =========================================================================================
        ! Read File
        ! =========================================================================================
        ! Open ************************************************************************************
        open(newUnit=file_unit, &
            file=file_path, &
            status='old', &
            action='read', &
            ioStat=file_stat, &
            ioMsg=file_error)


        ! Error ***********************************************************************************
        if ( file_stat /= 0) then
            print *, file_stat
            print *, file_error
            error stop 'File open'
        end if

        ! Read ************************************************************************************
        ! The first value need be CONTROLS values
        read(file_unit, *) title
        if ( title /= 'CONTROLS' ) error stop 'The first values int the file need be CONTROLS'

        ! CONTROLS --------------------------------------------------------------------------------
        if (debug) then
            write(*, '(A9)', advance='no') 'CONTROLS '

            do i = 1, 91
                write(*, '(A1)', advance='no') '*'
            end do
            print *
        end if

        CONTROLS: do
            read(file_unit, *, ioStat=read_stat) line_label, line_value

            if (read_stat == 0) then
                select case (line_label)
                    case ('nno')
                        nno = line_value
                    case ('nel')
                        nel = line_value
                    case ('ndofn')
                        ndofn = line_value
                    case ('nmat')
                        nmat = line_value
                    case ('nsec')
                        nsec = line_value
                end select
            else if (read_stat == 5010) then
                title = line_label
                exit CONTROLS
            else
                write(*, *) 'Read stat:', read_stat
                error stop 'Error in CONTROLS read'
            end if

            if (debug) print *, line_label, line_value
        end do CONTROLS

        if (debug) print *


        ! Allocation ------------------------------------------------------------------------------
        allocate(materials(nmat, 2))
        allocate(sections(nsec, 2))
        allocate(nodes(nsec, 2))
        allocate(bars(nel, 4))

        do j = 1, 4
            select case (title)
                case ('MATERIALS')
                    do i = 1, nmat
                        read(file_unit, *) temp_int, materials(i, 1), materials(i, 2)
                    end do
                case ('SECTIONS')
                    read(file_unit, *)
                    do i = 1, nmat
                        read(file_unit, *) temp_int, sections(i, 1), sections(i, 2)
                    end do
                case ('NODES')
                    read(file_unit, *)
                    do i = 1, nno
                        read(file_unit, *) temp_int, nodes(i, 1), nodes(i, 2)
                    end do
                case ('BARS')
                    read(file_unit, *) line_label
                    print *, line_label
                    do i = 1, nno
                        read(file_unit, *) temp_int, bars(i, 1), bars(i, 2), bars(i, 3), bars(i, 4)
                    end do
            end select

            read(file_unit, *) title
        end do

        print *, bars(1, :)

        ! Close ***********************************************************************************
        close(file_unit)
    end subroutine get_structure_data

end module file_to_structure
