program minmax
    implicit none
    character(len=36):: filename
    real:: maxval
    real:: minval
    real:: x 
    integer:: error
    integer:: in1

    write(*, 1000)
    1000 format(1x, 'this program finds the minimum and max val ', /, &
                1x, 'in an input data set. Enter the name of the file ', /, &
                1x, 'containing the input data: ')
    read(*, '(A)') filename
    
    open(unit=in1, file=filename, status='old', iostat=error)

    openok: if (error>0) then 
        write(*, 1020) filename, error
        1020 format(1x, 'error: open error on file ',A,': iostat= ', i6)
    else
        read(in1, *, iostat=error) x 

        if(error==0) then 
            minval=x 
            maxval=x 
        endif

        loop: do 
        read(in1, *, iostat=error) x 
        if(error/=0)exit 

        minval = min(minval, x)
        maxval = max(maxval, x)
        end do loop

        write(*, 1030) minval 
        1030 format(1X, 'The minimum value in the file is ', es13.6, '.')
        write(*, 1030) maxval 
        1040 format(1X, 'The maximum value in the file is ', es13.6, '.')

        close(unit=in1)
    end if openok

end program minmax