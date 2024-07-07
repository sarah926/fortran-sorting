! Sarah Toll
! ID# 1159798
! this file is to implement sorting a list of integers using an 
! iterative quicksort algorithm adapted from the pascal program given in assignment 1
module sorting
    implicit none
    contains
    ! iterative quick sort adapted from NonRecursiveQuicksort procedure
    subroutine iterativeQsort(a)   
        use stacking
        ! declare parameters and variables
        integer, dimension(:), intent(inout) :: a
        integer :: m
        integer :: i, j, l, r
        integer :: x, w
        integer :: s = 0
        
        type(stackIndex) :: temp1, temp2
        type(newStack) :: currStack
        call init(currStack, size(a))
        ! s is the stack pointer, starts at 1
        s = 1
        ! set first partition to entire array
        temp1%l = 1
        temp1%r = size(a)
        call push(currStack, temp1)
        ! m is max for stack pointer s
        m = size(a)/2

        ! run until stack is empty
        do while(s/=0 .and. s <= m)
            ! pop the top item from the stack
            temp2 = pop(currStack)
            l = temp2%l
            r = temp2%r
            s = s - 1
            ! while left is less then right, keep looping
            do while(l < r)
                i = l
                j = r
                ! set x as the pivot
                x = a((l+r) / 2)
                
                do while (i <= j)
                  ! find the elements to swap
                    do while(a(i) < x)
                        i = i + 1
                    end do
                    do while (x < a(j))
                        j = j - 1
                    end do
                    if (i <= j) then
                      ! swapping elements
                        w = a(i)
                        a(i) = a(j)
                        a(j) = w
                        i = i + 1
                        j = j - 1
                    end if
                end do
                ! pick partition to sort next

                    if((j-l) < (r-i)) then
                     ! chooses right partition
                        if(i < r) then
                            s = s + 1
                            temp1%l = i
                            temp1%r = r
                            ! push onto the stack
                            call push(currStack, temp1) 
                        end if
                        r = j
                    else
                     ! chooses left partition
                        if(l < j) then 
                            s = s + 1
                            ! push onto the stack
                            temp1%l = l
                            temp1%r = j
                            call push(currStack, temp1)
                        end if
                        l = i
                    end if
            end do
        end do
    end subroutine iterativeQsort
end module sorting

program sort 
    !  This program iteratively quicksorts an array a
    use readAndWrite
    use sorting
    use stacking
    implicit none
    integer, dimension (:), allocatable :: a
    real:: start,end, time
    
    ! declaring variables
    call readUnsorted(a)
    call cpu_time(start)
    call iterativeQsort(a)
    call cpu_time(end)
    call writeSorted(a)
    
    time = end - start
    write(*,*) 'time running iterative was: ', time, 'seconds'
end