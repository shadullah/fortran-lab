!=====================================================================
! PROGRAM calc_cross_product
! Purpose: Calculate the cross product of two 3D vectors
!=====================================================================

program calc_cross_product
    implicit none

    ! Constants
    integer, parameter :: SIZE = 3

    ! Variables
    real, dimension(SIZE) :: v1, v2, vcross

    ! ------------------- Get first vector -------------------
    print '(A)', 'Enter first vector (three terms):'
    read (*, *) v1

    ! ------------------- Get second vector -------------------
    print '(A)', 'Enter second vector (three terms):'
    read (*, *) v2

    ! ------------------- Calculate cross product -------------------
    vcross(1) = v1(2)*v2(3) - v1(3)*v2(2)
    vcross(2) = v1(3)*v2(1) - v1(1)*v2(3)
    vcross(3) = v1(1)*v2(2) - v1(2)*v2(1)

    ! ------------------- Print result -------------------
    print '(A, F10.1, A, F10.1, A, F10.1, A)', &
          ' The cross product of the two vectors is ', &
          vcross(1), ' i + ', vcross(2), ' j + ', vcross(3), ' k'

end program calc_cross_product