! Solution for SPOJ TEST - Life, the Universe, and Everything
! https://www.spoj.com/problems/TEST
program test
    implicit none
    integer :: n, ios
    do
        read(*,*,iostat=ios) n
        if (ios /= 0) exit
        if (n == 42) exit
        write(*,'(I0)') n
    end do
end program test
