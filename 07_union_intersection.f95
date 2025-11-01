!=====================================================================
! PROGRAM sets
! Purpose: Read two sets of integers from a file and compute
!          Union and Intersection
!=====================================================================

program sets
    implicit none

    integer, parameter :: SIZE = 100
    integer :: a1(SIZE), a2(SIZE), union_set(SIZE), inter_set(SIZE)
    integer :: n1, n2, n_union, n_inter
    character(len=20) :: filename
    integer :: i, j, status
    logical :: in_set

    print *, "Enter filename: "
    read (*, '(A)') filename

    open(unit=10, file=filename, status='OLD', iostat=status)
    if (status /= 0) then
        print *, "Error opening file."
        stop
    end if

    ! ---- Read first set ----
    read(10, *) n1
    read(10, *) (a1(i), i = 1, n1)

    ! ---- Read second set ----
    read(10, *) n2
    read(10, *) (a2(i), i = 1, n2)

    close(10)

    n_union = 0
    n_inter = 0

    ! ---- Intersection ----
    do i = 1, n1
        in_set = .false.
        do j = 1, n2
            if (a1(i) == a2(j)) then
                in_set = .true.
                exit
            end if
        end do
        if (in_set) then
            n_inter = n_inter + 1
            inter_set(n_inter) = a1(i)
        end if
    end do

    ! ---- Union ----
    do i = 1, n1
        n_union = n_union + 1
        union_set(n_union) = a1(i)
    end do

    do i = 1, n2
        in_set = .false.
        do j = 1, n1
            if (a2(i) == a1(j)) then
                in_set = .true.
                exit
            end if
        end do
        if (.not. in_set) then
            n_union = n_union + 1
            union_set(n_union) = a2(i)
        end if
    end do

    print *, "Intersection:"
    write(*, '(10I6)') (inter_set(i), i = 1, n_inter)

    print *, "Union:"
    write(*, '(10I6)') (union_set(i), i = 1, n_union)

end program sets
