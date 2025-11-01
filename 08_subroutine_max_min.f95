!=====================================================================
! PROGRAM test_minmax
! Purpose: Test the subroutine minmax with the function
!          f(x) = x³ - 5x² + 5x + 2
!          over [-1, 3] using 200 steps
!=====================================================================

program test_minmax
    implicit none

    ! Declare external function
    real, external :: func

    ! Variables for the search
    real    :: first_value, last_value
    integer :: num_steps
    real    :: xmin, xmax, min_value, max_value

    ! ------------------- Set inputs -------------------
    first_value = -1.0
    last_value  =  3.0
    num_steps   = 200

    ! ------------------- Call subroutine -------------------
    call minmax(func, first_value, last_value, num_steps, &
                xmin, min_value, xmax, max_value)

    ! ------------------- Print results -------------------
    print *
    print '(A)',       ' Results from minmax subroutine:'
    print '(A,F8.6,A,F12.6)', ' Minimum value: f(', xmin, ') = ', min_value
    print '(A,F8.6,A,F12.6)', ' Maximum value: f(', xmax, ') = ', max_value
    print *

end program test_minmax

!=====================================================================
! REAL FUNCTION func(x)
! Purpose: User-defined function f(x) = x³ - 5x² + 5x + 2
!=====================================================================

real function func(x)
    implicit none
    real, intent(in) :: x
    func = x**3 - 5.0*x**2 + 5.0*x + 2.0
end function func

!=====================================================================
! SUBROUTINE minmax
! Purpose: Find min and max of a function over a uniform grid
!=====================================================================

subroutine minmax(func, first_value, last_value, num_steps, &
                  xmin, min_value, xmax, max_value)
    implicit none

    ! List of calling arguments
    real, external :: func
    real, intent(in)  :: first_value, last_value
    integer, intent(in) :: num_steps
    real, intent(out) :: xmin, min_value, xmax, max_value

    ! Local variables
    integer :: i
    real    :: step_size, x, value

    ! ------------------- Calculate step size -------------------
    step_size = (last_value - first_value) / real(num_steps - 1)

    ! ------------------- Initialize with first point -------------------
    x = first_value
    value = func(x)

    xmin = x
    min_value = value
    xmax = x
    max_value = value

    ! ------------------- Loop over all other steps -------------------
    do i = 1, num_steps - 1
        x = first_value + i * step_size
        value = func(x)   ! <-- Function call happens here

        if (value < min_value) then
            min_value = value
            xmin = x
        end if

        if (value > max_value) then
            max_value = value
            xmax = x
        end if
    end do

end subroutine minmax