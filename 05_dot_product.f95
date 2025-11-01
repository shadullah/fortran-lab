!=====================================================================
! PROGRAM calc_dot_product
! Purpose: Calculate the dot product of two 3D vectors
!=====================================================================

program calc_dot_product
    implicit none

    ! Constants
    integer, parameter :: SIZE = 3

    ! Variables
    real, dimension(SIZE) :: v1, v2
    real                  :: dot_product
    integer               :: i

    ! ------------------- Get first vector -------------------
    print '(A)', 'Enter first vector (three numbers):'
    read (*, *) v1

    ! ------------------- Get second vector -------------------
    print '(A)', 'Enter second vector (three numbers):'
    read (*, *) v2

    ! ------------------- Calculate dot product -------------------
    dot_product = 0.0
    do i = 1, SIZE
        dot_product = dot_product + v1(i) * v2(i)
    end do

    ! ------------------- Print result -------------------
    print '(A, F12.4)', ' The dot product of the two vectors is ', dot_product

end program calc_dot_product