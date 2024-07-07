! Sarah Toll
! ID# 1159798
! this file is to implement sorting a list of integers using quicksort
! implemented recursively. 
module recursion
    implicit none
    contains
    ! partition function used in quicksort
    integer function partition(a, lowNumber, highNumber)
        ! declaring parameters
        integer, dimension(:), intent(inout) :: a
        integer, intent(inout) :: lowNumber, highNumber
        integer :: pivot, i, j, temp
        pivot = a(highNumber)
        i = lowNumber -1
        j = lowNumber
        ! swap while numbers are less then the pivot
        do while(j < highNumber)
            if(a(j) < pivot) then
                i = i + 1
                temp = a(i)
                a(i) = a(j)
                a(j) = temp
            end if
            j = j + 1
        end do
        ! swap at the end
        temp = a(i+1)
        a(i+1) = a(highNumber)
        a(highNumber) = temp
        partition = i+1
    end
    ! quick sort recursive function
    recursive subroutine quickSort(a, lowNumber, highNumber)
        implicit none
        integer, dimension(:), intent(inout) :: a
        integer, intent(inout) :: lowNumber, highNumber
        integer :: pivoting, oneDown, oneUp
        if(lowNumber < highNumber) then
            pivoting = partition(a, lowNumber, highNumber)
            oneDown = pivoting - 1
            oneUp = pivoting + 1
            !call quick sort on the upper half and lower half of arrays
            call quickSort(a, lowNumber, oneDown)
            call quickSort(a, oneUp, highNumber)
        end if
    end subroutine quickSort

end module recursion

! main program
program sort 
    !  This program recursively quicksorts an array a
    use readAndWrite
    use recursion
    implicit none

    ! declaring variables
    integer, dimension (:), allocatable :: a
    integer :: sizing, starting
    real:: start, end, time
    
    ! call functions to get array, time it, sort it recursively, and write it to a file
    call readUnsorted(a)
    call cpu_time(start)
    sizing = size(a)
    starting = 1
    call quickSort(a, starting, sizing)
    call cpu_time(end)
    call writeSorted(a)
    time = end - start
    write(*,*) 'time running recursive was: ', time, 'seconds'
end