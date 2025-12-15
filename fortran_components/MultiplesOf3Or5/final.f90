program multiples
        use iso_fortran_env, only: int64
        implicit none

        ! Set up some variables
        integer(int64) :: endValue
        integer :: ios
        character(len=128) :: arg1

        ! Code to deal with command line arguments        
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
        
        ! The main function call
        call calculate_sum(endValue)
contains 

        subroutine calculate_sum(endValue)
                integer(int64), intent(in) :: endValue
                integer(int64) :: x, i
                
                x = 0
                do i = 1, endValue
                        if (MOD(i,3) == 0 .OR. MOD(i,5) == 0) then
                                x = x + i
                        end if
                end do
                print *, x !End value
        end subroutine calculate_sum

end program multiples
