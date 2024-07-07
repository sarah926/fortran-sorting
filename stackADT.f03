! Sarah Toll
! ID# 1159798
! this file is to implement a stack with each element of type stackindex, which is two integers, l and r
module stacking
   ! declaring the types
    type :: stackIndex
        integer :: l, r
    end type stackIndex
    type :: newStack
        integer :: maxSize
        type(stackIndex), dimension(:), allocatable :: stack
        integer :: size = 0
    end type newStack
    
    contains
        ! init current stack to set the max size and allocate
        subroutine init(currStack, maxSize)
            integer, intent(in) :: maxSize
            type(newStack), intent(inout) :: currStack
            currStack%maxSize = maxSize
            allocate(currStack%stack(maxSize))
        end subroutine init
        ! push element onto the stack
        subroutine push(currStack, s)
            type(stackIndex), intent(in) :: s
            type(newStack), intent(inout) :: currStack
            ! if there is room, add to the stack
            if(currStack%size < currStack%maxSize) then
                currStack%stack(currStack%size + 1)%l = s%l
                currStack%stack(currStack%size + 1)%r = s%r
                currStack%size = currStack%size + 1
            end if
        end subroutine push

        ! pop element from the stack
        type(stackIndex) function pop(currStack)
            type(newStack), intent(inout) :: currStack
            if(currStack%size > 0) then
                pop%l = currStack%stack(currStack%size)%l
                pop%r = currStack%stack(currStack%size)%r
                currStack%size = currStack%size - 1
              ! if stack is empty, put negative 1
              else
                pop%l = -1
                pop%r = -1
            end if
        end
        ! take everything off the stack
        subroutine clear(currStack)
            type(newStack), intent(inout) :: currStack
            ! until size is 0 set all elements to -1 for empty
            do while(currStack%size > 0)
                currStack%stack(currStack%size)%l = -1
                currStack%stack(currStack%size)%r = -1
                currStack%size = currStack%size - 1
            end do
        end subroutine clear

        ! returns true if the stack is empty based on variable size
        logical function isEmpty(currStack)
            type(newStack), intent(inout) :: currStack
            if(currStack%size == 0) then
                isEmpty = .true.
            else
                isEmpty = .false.
            end if
        end

        ! print the stack for testing purposes
        subroutine printStack(currStack)
            integer :: i
            type(newStack), intent(inout) :: currStack
            write(*,*) 'size is ', currStack%size, ' maxsize is ', currStack%maxSize
            i = 0
            ! loops through stack and prints the element at index i
            do while(i < currStack%size)
                write(*,*)  i, ':' ,currStack%stack(i+1)%l, currStack%stack(i+1)%r
                i = i+1
            end do
        end subroutine printStack
end module stacking

