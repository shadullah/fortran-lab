!=====================================================================
! PROGRAM mat_mult
! Purpose: Multiply two square matrices A and B read from files
!=====================================================================

program mat_mult
    implicit none

    ! Parameters
    integer, parameter :: MAXSIZE = 20   ! Max matrix size

    ! Variables
    character(len=30) :: file_a, file_b
    integer           :: n, i, j, k, status
    real, dimension(MAXSIZE, MAXSIZE) :: a, b, c

    ! ------------------- Get filenames -------------------
    print '(A)', 'Enter the file name containing array A: '
    read '(A)', file_a

    print '(A)', 'Enter the file name containing array B: '
    read '(A)', file_b

    ! ------------------- Open and read Matrix A -------------------
    open(unit=11, file=file_a, status='OLD', iostat=status)
    if (status /= 0) then
        print '(A,I0)', 'ERROR: Cannot open file A, IOSTAT = ', status
        stop
    end if

    read(11, *) n
    if (n <= 0 .or. n > MAXSIZE) then
        print *, 'ERROR: Invalid size in file A.'
        stop
    end if

    do i = 1, n
        read(11, *) (a(i,j), j = 1, n)
    end do
    close(11)

    ! ------------------- Open and read Matrix B -------------------
    open(unit=12, file=file_b, status='OLD', iostat=status)
    if (status /= 0) then
        print '(A,I0)', 'ERROR: Cannot open file B, IOSTAT = ', status
        stop
    end if

    read(12, *) k
    if (k /= n) then
        print *, 'ERROR: Matrices must be same size!'
        stop
    end if

    do i = 1, n
        read(12, *) (b(i,j), j = 1, n)
    end do
    close(12)

    ! ------------------- Multiply: C = A × B -------------------
    do i = 1, n
        do j = 1, n
            c(i,j) = 0.0
            do k = 1, n
                c(i,j) = c(i,j) + a(i,k) * b(k,j)
            end do
        end do
    end do

    ! ------------------- Print Result -------------------
    print *
    print '(A)', ' The resulting matrix C is:'
    do i = 1, n
        write(*, '(100F8.2)') (c(i,j), j = 1, n)
    end do
    print *

end program mat_mult