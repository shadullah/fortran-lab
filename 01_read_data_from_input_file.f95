program round
    implicit none
    character(len=36):: filename1
    character(len=36):: filename2
    integer :: istat                ! i/0 status of reads
    integer :: istat1               ! i/0 status of input file open    
    integer :: istat2               ! i/0 status of output file open
    real :: val

    write(*,*) 'round -- Round values to Nearest integer'
    write(*, '(1x, A)') 'Enter the input file name: '
    read(*, '(A36)') filename1

    write(*,'(1x, A)') 'Enter the output file name: '
    read(*, '(A36)') filename2

    open(UNIT=8, FILE=filename1, STATUS='OLD', IOSTAT=istat1)

    ! 1_input.txt
    ! 1_output.txt

    in_ok: IF(istat1/=0) then
        write(*, 1010) istat1
        1010 FORMAT (1X, 'open failed on input file: iostat= ', I6)
    else
        open(UNIT=9, FILE=filename2, STATUS='REPLACE', IOSTAT=istat2)

        out_ok: IF(istat2 /= 0 ) then 
            write(*, 1020) istat2
            1020 FORMAT (1X, 'open failed on output file: iostat= ', I6)
        else 
            loop: Do 
            read(8, *, IOSTAT=istat) val
            print *, 'Opened input file successfully: ', trim(filename1)
            print *, 'Read value:', val
            IF(istat /= 0) exit
            write(9,*, IOSTAT=istat) NINT(val)
            end DO loop 
            close (UNIT=9)
        end IF out_ok

        close (UNIT=8)
    end IF in_ok

end program round