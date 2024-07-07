! Sarah Toll
! ID# 1159798
! this file is to implement reading from and writing to a file.
module readAndWrite
    implicit none
    contains
    subroutine readUnsorted(a)
        implicit none
        character (len=20) :: fname
        logical :: doesFileExist
        integer, dimension (:), intent(inout), allocatable :: a
        integer :: readStatus = 0
        integer :: count=0
        integer :: index=0
        ! ask for the file name
        write(*, *) 'enter file name to read from'
        read(*, '(A)') fname
        ! check that the file exists
        inquire(file=fname, exist=doesFileExist)
        ! if it exists, then open it
        if(doesFileExist) then
            ! open the file
            open(unit = 10, file=fname, action='read')
            do while(readStatus==0)
                read(10,*, iostat=readStatus)
                if(readStatus /=0)  exit
                count = count + 1
            end do
            allocate(a(count))
            rewind(10)
            ! write(*,*) count
            do while(index < count)
                read(10, *) a(index + 1)
                index = index + 1
                ! write(*,*) a(index)
            end do
            close(11)
        ! if doesnt exist, give error message and exit
        else
            write(*,*) 'file does not exist'
            call exit(0)
        end if
    end subroutine readUnsorted

    ! write to a file
    subroutine writeSorted(a)
        implicit none
        ! declare parameters and variables
        integer, intent(inout),dimension (:), allocatable :: a
        integer :: count
        integer :: i
        logical :: doesFileExist
        character(len=1) :: overwrite = 'a'
        character(len=1), parameter :: yes = 'y'
        character(len=1), parameter :: no = 'n'
        character(len=20) :: fname = 'sortedNUM.txt'
        ! check if file exists
        inquire(file=fname, exist=doesFileExist)
        !if it doesnt exist, create a new file
        if(.not. doesFileExist) then
            open(unit=9,file=fname,status='new',action='write')
            count = size(a)
            do i = 1, count
                write(9,*) a(i)
            end do
          ! if it exists, prompt the user if they want to overwrite it
          else
            overwrite = 'a'
            do while(overwrite /= yes .and. overwrite/= no)
                write(*,*) 'file exists, do you want to overwrite? type y for yes, n for no'
                read(*,*) overwrite
            end do
            if(overwrite == yes) then
                open(unit=9,file=fname,status='replace',action='write')
                count = size(a)
                do i = 1, count
                    write(9,*) a(i)
                end do
            end if
        end if
        deallocate(a)
    end subroutine writeSorted
end module readAndWrite
