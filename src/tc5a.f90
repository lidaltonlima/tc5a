module tc5a
    implicit none
    private

    public :: say_hello
contains
    subroutine say_hello
        print *, "Hello, tc5a!"
    end subroutine say_hello
end module tc5a
