!=====================================================================
! PROGRAM all_means
! Purpose: Read numbers from a file and compute:
!          - Arithmetic mean
!          - Geometric mean
!          - Harmonic mean
!          - RMS (Root Mean Square)
!=====================================================================

program all_means
    implicit none

    ! Variables
    character(len=50) :: filename   ! Change this if needed
    integer           :: iostat, unit = 10
    real              :: x
    integer           :: n = 0
    real              :: sum_x = 0.0, prod_x = 1.0
    real              :: sum_x2 = 0.0, sum_1x = 0.0
    real              :: ave, g_mean, h_mean, rms

    ! ------------------- Open the file -------------------
    print *, 'enter filename'
    read(*,*) filename
    open(unit=unit, file=filename, status='OLD', iostat=iostat)

    if (iostat /= 0) then
        print '(A,I0)', 'ERROR: Open error on file ', iostat
        stop
    end if

    ! ------------------- Read and process data -------------------
    do
        read(unit, *, iostat=iostat) x
        if (iostat /= 0) exit   ! End of file or error

        n = n + 1
        sum_x  = sum_x  + x
        prod_x = prod_x * x
        sum_x2 = sum_x2 + x*x
        sum_1x = sum_1x + 1.0 / x
    end do

    close(unit)

    ! ------------------- Check if data was read -------------------
    if (n == 0) then
        print *, 'ERROR: No data found in the file.'
        stop
    end if

    ! ------------------- Calculate the means -------------------
    ave    = sum_x / real(n)
    g_mean = prod_x ** (1.0 / real(n))
    h_mean = real(n) / sum_1x
    rms    = sqrt(sum_x2 / real(n))

    ! ------------------- Print results -------------------
    print *
    print '(A,F10.4)', ' The average of this data set is: ', ave
    print '(A,F10.4)', ' The geometric mean of this data set is: ', g_mean
    print '(A,F10.4)', ' The harmonic mean of this data set is: ', h_mean
    print '(A,F10.4)', ' The rms average of this data set is: ', rms
    print '(A,I0)',    ' The number of data points is: ', n
    print *

end program all_means