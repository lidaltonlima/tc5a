module table
    implicit none

    interface show_table
        module procedure show_table_int
        module procedure show_table_real8
    end interface show_table

contains
    subroutine show_table_int(array, width, precision, titles)
        ! I/O vars
        integer, intent(in) :: array(:,:)
        integer, intent(in) :: width
        integer, optional, intent(in) :: precision
        character(*), optional, intent(in) :: titles(:)

        ! Control vars
        integer :: i, j, k  ! indexes
        integer :: nLin, nCol  ! Number of lines and cols
        integer :: column_size
        character(10) :: fmt_int, fmt_char  ! Format string


        nLin = size(array, 1)
        nCol = size(array, 2)
        column_size = width / nCol

        if (nCol /= size(titles)) error stop 'Size not equals (titles /= nCol)'

        if (present(precision)) then
            error stop 'Do not have precision in integer values'
        else
            write(fmt_int, "('(I', I0, ')')") (column_size - 1)
        end if
        write(fmt_char, "('(A', I0, ')')") (column_size - 1)

        if (present(titles)) then
            ! Draw line
            do k = 0, width
                if ( mod(k, column_size) == 0 ) then
                    write(*, '(A)', advance='no') '+'
                else
                    write(*, '(A)', advance='no') '-'
                end if
            end do
            print *

            ! Print columns
            do j = 1, nCol
                write(*, '(A)', advance='no') '|'
                write(*, fmt_char, advance='no') trim(titles(j))
            end do
            write(*, '(A)') '|'
        end if

        do i = 1, nLin
            ! Draw line
            do k = 0, width
                if ( mod(k, column_size) == 0 ) then
                    write(*, '(A)', advance='no') '+'
                else
                    write(*, '(A)', advance='no') '-'
                end if
            end do
            print *

            ! Print columns
            do j = 1, nCol
                write(*, '(A)', advance='no') '|'
                write(*, fmt_int, advance='no') array(i, j)
            end do
            write(*, '(A)') '|'
        end do

        ! Draw line
        do k = 0, width
            if ( mod(k, column_size) == 0 ) then
                write(*, '(A)', advance='no') '+'
            else
                write(*, '(A)', advance='no') '-'
            end if
        end do
        print *
    end subroutine show_table_int

    subroutine show_table_real8(array, width, precision, titles)
        ! I/O vars
        real(8), intent(in) :: array(:,:)
        integer, intent(in) :: width
        integer, optional, intent(in) :: precision
        character(*), optional, intent(in) :: titles(:)

        ! Control vars
        integer :: i, j, k  ! indexes
        integer :: nLin, nCol  ! Number of lines and cols
        integer :: column_size
        character(10) :: fmt_real, fmt_char  ! Format string


        nLin = size(array, 1)
        nCol = size(array, 2)
        column_size = width / nCol

        if (nCol /= size(titles)) error stop 'Size not equals (titles /= nCol)'

        if (present(precision)) then
            write(fmt_real, "('(ES', I0, '.', I0, ')')") (column_size - 1), precision
        else
            write(fmt_real, "('(ES', I0, '.4', ')')") (column_size - 1)
        end if
        write(fmt_char, "('(A', I0, ')')") (column_size - 1)

        if (present(titles)) then
            ! Draw line
            do k = 0, width
                if ( mod(k, column_size) == 0 ) then
                    write(*, '(A)', advance='no') '+'
                else
                    write(*, '(A)', advance='no') '-'
                end if
            end do
            print *

            ! Print columns
            do j = 1, nCol
                write(*, '(A)', advance='no') '|'
                write(*, fmt_char, advance='no') trim(titles(j))
            end do
            write(*, '(A)') '|'
        end if

        do i = 1, nLin
            ! Draw line
            do k = 0, width
                if ( mod(k, column_size) == 0 ) then
                    write(*, '(A)', advance='no') '+'
                else
                    write(*, '(A)', advance='no') '-'
                end if
            end do
            print *

            ! Print columns
            do j = 1, nCol
                write(*, '(A)', advance='no') '|'
                write(*, fmt_real, advance='no') array(i, j)
            end do
            write(*, '(A)') '|'
        end do

        ! Draw line
        do k = 0, width
            if ( mod(k, column_size) == 0 ) then
                write(*, '(A)', advance='no') '+'
            else
                write(*, '(A)', advance='no') '-'
            end if
        end do
        print *
    end subroutine show_table_real8

end module table
