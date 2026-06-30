module table
    implicit none

    interface show_table
        ! module procedure show_table_int
        ! module procedure show_table_real
        module procedure show_table_real8
    end interface show_table

contains
    ! subroutine show_table_int(array, width, precision)
    !     integer, intent(in) :: array(:,:)
    !     integer, intent(in) :: width
    !     integer, optional, intent(in) :: precision

    !     print *, 'Not Implemented'
    !     print *, array, width, precision
    ! end subroutine show_table_int

    ! subroutine show_table_real(array, width, precision)
    !     real, intent(in) :: array(:,:)
    !     integer, intent(in) :: width
    !     integer, optional, intent(in) :: precision

    !     print *, 'Not Implemented'
    !     print *, array, width, precision
    ! end subroutine show_table_real

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
100         format('(F', I0, '.', I0, ')')
            write(fmt_real, 100) (column_size - 1), precision
        else
110         format('(F', I0, '.4', ')')
            write(fmt_real, 110) (column_size - 1)
        end if
120     format('(A', I0, ')')
        write(fmt_char, 120) (column_size - 1)

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
