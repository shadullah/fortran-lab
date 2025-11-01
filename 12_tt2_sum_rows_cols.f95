!=====================================================================
! PROGRAM sum_rows_and_cols
! Purpose: Read array from file and compute sum of rows and columns
!=====================================================================

program sum_rows_and_cols
    implicit none

    ! Parameters
    integer, parameter :: MAXSIZE = 20   ! Max rows & columns

    ! Variables
    character(len=30)    :: filename
    integer              :: n, i, j, status
    real, dimension(MAXSIZE, MAXSIZE) :: array
    real, dimension(MAXSIZE) :: row_sum, col_sum
    real                 :: total_sum

    ! ------------------- Get filename -------------------
    print '(A)', 'Enter the file name containing the array: '
    read '(A)', filename

    ! ------------------- Open file -------------------
    open(unit=10, file=filename, status='OLD', iostat=status)
    if (status /= 0) then
        print '(A,I0)', 'ERROR: Open error on file ', status
        stop
    end if

    ! ------------------- Read size n -------------------
    read(10, *) n
    if (n <= 0 .or. n > MAXSIZE) then
        print *, 'ERROR: Invalid size or too large.'
        stop
    end if

    ! ------------------- Read array -------------------
    do i = 1, n
        read(10, *) (array(i,j), j = 1, n)
    end do
    close(10)

    ! ------------------- Compute sums -------------------
    total_sum = 0.0
    do i = 1, n
        row_sum(i) = 0.0
        col_sum(i) = 0.0
    end do

    do i = 1, n
        do j = 1, n
            row_sum(i) = row_sum(i) + array(i,j)
            col_sum(j) = col_sum(j) + array(i,j)
            total_sum  = total_sum  + array(i,j)
        end do
    end do

    ! ------------------- Print results -------------------
    print *
    print '(A,F12.2)', ' Sum of all values in array: ', total_sum

    print '(A)', ' Sum of each row:'
    do i = 1, n
        print '(I4, F12.2)', i, row_sum(i)
    end do

    print '(A)', ' Sum of each column:'
    do j = 1, n
        print '(I4, F12.2)', j, col_sum(j)
    end do
    print *

end program sum_rows_and_cols