module OutputTerminal
    implicit none

    interface print_table
        module procedure print_table_int
        module procedure print_table_real8
    end interface print_table

contains
    subroutine print_table_int(array, table_width, precision, titles, line_number)
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        integer, intent(in) :: array(:,:)
        integer, intent(in) :: table_width
        integer, optional, intent(in) :: precision
        character(*), optional, intent(in) :: titles(:)
        logical, optional, intent(in) :: line_number

        ! Control
        integer :: width
        integer :: i, j, k  ! indexes
        integer :: nLin, nCol  ! Number of lines and cols
        integer :: column_size
        logical :: show_line_number
        character(10) :: fmt_int  ! Format string


        ! =========================================================================================
        ! Vars initialization
        ! =========================================================================================
        nLin = size(array, 1)
        nCol = size(array, 2)

        show_line_number = .false.
        if (present(line_number)) then
            show_line_number = line_number
        end if

        if (show_line_number) then
            width = table_width - 4
        else
            width = table_width
        end if

        column_size = width / nCol


        if (nCol /= size(titles)) error stop 'Size not equals (titles /= nCol)'

        ! =========================================================================================
        ! Output
        ! =========================================================================================

        if (present(precision)) then
            error stop 'Do not have precision in integer values'
        else
            write(fmt_int, "('(I', I0, ')')") (column_size - 1)
        end if

        if (present(titles)) then
            call print_titles(titles, width, line_number=show_line_number)
        end if

        do i = 1, nLin
            ! Draw line
            if (show_line_number) write(*, '(A5)', advance='no') '+----'
            do k = 0, width
                if ( mod(k, column_size) == 0 ) then
                    write(*, '(A)', advance='no') '+'
                else
                    write(*, '(A)', advance='no') '-'
                end if
            end do
            print *

            ! Print columns
            if (show_line_number) then
                write(*, '(A)', advance='no') '|'
                write(*, '(I4)', advance='no') i
            end if
            do j = 1, nCol
                write(*, '(A)', advance='no') '|'
                write(*, fmt_int, advance='no') array(i, j)
            end do
            write(*, '(A)') '|'
        end do

        ! Draw line
        if (show_line_number) write(*, '(A5)', advance='no') '+----'
        do k = 0, width
            if ( mod(k, column_size) == 0 ) then
                write(*, '(A)', advance='no') '+'
            else
                write(*, '(A)', advance='no') '-'
            end if
        end do
        print *
    end subroutine print_table_int

    subroutine print_table_real8(array, table_width, precision, titles, line_number)
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        real(8), intent(in) :: array(:,:)
        integer, intent(in) :: table_width
        integer, optional, intent(in) :: precision
        character(*), optional, intent(in) :: titles(:)
        logical, optional, intent(in) :: line_number

        ! Control
        integer :: width
        integer :: i, j, k  ! indexes
        integer :: nLin, nCol  ! Number of lines and cols
        integer :: column_size
        logical :: show_line_number
        character(10) :: fmt_real, fmt_char  ! Format string

        ! =========================================================================================
        ! Vars initialization
        ! =========================================================================================
        nLin = size(array, 1)
        nCol = size(array, 2)

        show_line_number = .false.
        if (present(line_number)) then
            show_line_number = line_number
        end if

        if (show_line_number) then
            width = table_width - 4
        else
            width = table_width
        end if

        column_size = width / nCol

        if (nCol /= size(titles)) error stop 'Size not equals (titles /= nCol)'

        ! =========================================================================================
        ! Output
        ! =========================================================================================
        if (present(precision)) then
            write(fmt_real, "('(ES', I0, '.', I0, ')')") (column_size - 1), precision
        else
            write(fmt_real, "('(ES', I0, '.4', ')')") (column_size - 1)
        end if
        write(fmt_char, "('(A', I0, ')')") (column_size - 1)

        if (present(titles)) then
            call print_titles(titles, width, line_number=show_line_number)
        end if

        do i = 1, nLin
            ! Draw line
            if (show_line_number) write(*, '(A5)', advance='no') '+----'
            do k = 0, width
                if ( mod(k, column_size) == 0 ) then
                    write(*, '(A)', advance='no') '+'
                else
                    write(*, '(A)', advance='no') '-'
                end if
            end do
            print *

            ! Print columns
            if (show_line_number) then
                write(*, '(A)', advance='no') '|'
                write(*, '(I4)', advance='no') i
            end if
            do j = 1, nCol
                write(*, '(A)', advance='no') '|'
                write(*, fmt_real, advance='no') array(i, j)
            end do
            write(*, '(A)') '|'
        end do

        ! Draw line
        if (show_line_number) write(*, '(A5)', advance='no') '+----'
        do k = 0, width
            if ( mod(k, column_size) == 0 ) then
                write(*, '(A)', advance='no') '+'
            else
                write(*, '(A)', advance='no') '-'
            end if
        end do
        print *
    end subroutine print_table_real8

    subroutine print_titles(titles, width, line_number)
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        character(*), intent(in) :: titles(:)
        integer, intent(in) :: width
        integer :: column_size
        logical, optional, intent(in) :: line_number

        integer :: nCol
        integer :: j, k
        logical :: show_line_number
        character(10) :: fmt_char

        ! =========================================================================================
        ! Vars initialization
        ! =========================================================================================
        nCol = size(titles)

        show_line_number = .false.
        if (present(line_number)) show_line_number = line_number

        column_size = width / nCol

        ! =========================================================================================
        ! Output
        ! =========================================================================================
        write(fmt_char, "('(A', I0, ')')") (column_size - 1)
        ! Draw line
        if (show_line_number) write(*, '(A5)', advance='no') '+----'
        do k = 0, width
            if (mod(k, column_size) == 0) then
                write(*, '(A)', advance='no') '+'
            else
                write(*, '(A)', advance='no') '-'
            end if
        end do
        print *

        ! Print columns
        if (show_line_number) then
            write(*, '(A)', advance='no') '|'
            write(*, '(A4)', advance='no') 'i'
        end if
        do j = 1, nCol
            write(*, '(A)', advance='no') '|'
            write(*, fmt_char, advance='no') trim(titles(j))
        end do
        write(*, '(A)') '|'
    end subroutine print_titles
end module OutputTerminal
