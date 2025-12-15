!----------------------------------------------------------------------------------------------------------------
! This is a WAY over complication of this problem. It still gets the right answer but the logic does way too much
!----------------------------------------------------------------------------------------------------------------

program multiples
        use iso_fortran_env, only: int64
        implicit none
        
        integer(int64) :: endValue
        integer :: ios
        character(len=128) :: arg1

        call get_command_argument(1, arg1)

        if (len_trim(arg1) == 0 .OR. iargc() /= 1) then
                print *, "Usage: ./a.exe <num>"
                stop
        end if

        read(arg1, *, iostat=ios) endValue
        
        if(ios /= 0) then
                print *, "Error: Argument must be an integer."
                stop
        end if

        if(endValue <= 0) then
                print *, "Error: Argument must be a positive integer"
                stop
        end if
        
        call calculate_sum(endValue)
contains 

        subroutine calculate_sum(endValue)
                integer(int64), intent(in) :: endValue
                integer(int64) :: x
                integer(int64), allocatable :: arr(:)
                
                arr = get_array(endValue)
                x = sum(arr)
                print *, x !End value
        end subroutine calculate_sum

        function get_array(endValue) result(arr)
                integer(int64), intent(in) :: endValue
                integer(int64), allocatable :: arr(:), temp(:)
                integer(int64) :: i, count

                allocate(temp(endValue))
                count = 0

                do i = 1, endValue
                        if (MOD(i,3) == 0 .OR. MOD(i,5) == 0) then
                                count = count + 1
                                temp(count) = i        
                        end if
                end do

                if (count > 0) then
                        allocate(arr(count))
                        arr = temp(1:count)
                else
                                allocate(arr(0))
                end if
                deallocate(temp)
        end function get_array

end program multiples

! I realize that this is not needed as the array can't ever include duplicate values anyway
        !function is_valid(x, arr, count) result(res)
        !       logical :: res
        !        integer(int64), intent(in) :: x, count
        !        integer(int64), intent(in) :: arr(:)
        !        
        !        res = (.NOT. ANY(arr(1:count) == x)) .AND. (MOD(x,3) == 0 .OR. MOD(x,5) == 0)
        !
        !end function is_valid
        
        ! Original solution for is_valid which I then simplified to do in one line
        !check = ANY(arr == x)
        !if (.NOT. check .AND. (mod(x,3) == 0 .OR. mod(x,5) == 0)) then
        !        res = .TRUE.  
        !else
        !        res = .FALSE.
        !end if
