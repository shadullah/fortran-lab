!=====================================================================
! PROGRAM retrieve_value
! Purpose: Store non-negative numbers in a scratch file and retrieve any
!=====================================================================

program retrieve_value
    implicit none

    ! Variables
    real              :: value
    integer           :: index, status, i,j
    character(len=50) :: response

    ! ------------------- Step 1: Input & Save to Scratch File -------------------
    print *, 'Enter non-negative real numbers (type "done" to finish):'

    ! Open a scratch file (temporary, auto-deleted when closed)
    open(unit=20, status='scratch', iostat=status)

    i = 0
    do
        print '(A,I0,A)', ' Enter value #', i+1, ': '
        read(*, '(A)') response

        ! Check if user typed "done"
        if (trim(response) == 'done' .or. trim(response) == 'DONE') exit

        ! Convert string to real
        read(response, *, iostat=status) value

        if (status /= 0) then
            print *, '  ERROR: Invalid number. Try again.'
            cycle
        end if

        if (value < 0.0) then
            print *, '  ERROR: Only non-negative values allowed.'
            cycle
        end if

        ! Save to scratch file
        i = i + 1
        write(20) value
        print '(A,F10.4,A,I0)', '  Stored: ', value, ' at position ', i
    end do

    if (i == 0) then
        print *, 'No values entered. Exiting.'
        close(20)
        stop
    end if

    ! Rewind to read from beginning
    rewind(20)

    ! ------------------- Step 2: Retrieve a value -------------------
    print *
    print '(A,I0,A)', ' You entered ', i, ' values.'
    print '(A)', ' Which one do you want to see? (1 to ', i, ')'

    do
        print '(A)', ' Enter index: '
        read(*, *, iostat=status) index
        if (status /= 0 .or. index < 1 .or. index > i) then
            print *, '  Invalid index. Try again.'
        else
            exit
        end if
    end do

    ! Read the requested value from file
    do j = 1, index
        read(20) value
    end do

    ! ------------------- Step 3: Display the value -------------------
    print *
    print '(A,I0,A,F10.4)', ' Value at position ', index, ' is: ', value

    close(20)  ! Scratch file is deleted automatically

end program retrieve_value