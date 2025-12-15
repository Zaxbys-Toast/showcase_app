program even_fib
        use iso_fortran_env, only: int64
        implicit none

        ! Set up some variables
        integer(int64) :: targetValue
        integer :: ios
        character(len=128) :: arg1

        ! Code to deal with command line arguments
        call get_command_argument(1, arg1)

        if (len_trim(arg1) == 0 .OR. iargc() /= 1) then
                print *, "Usage: ./<program_name> <num>"
                stop
        end if

        read(arg1, *, iostat=ios) targetValue

        if(ios /= 0) then
                print *, "Error: Argument must be an integer."
                stop
        end if

        if(targetValue <= 0) then
                print *, "Error: Argument must be a positive integer"
                stop
        end if
        
        ! Main function call
        call sum_even_fibs(targetValue)
contains
        subroutine sum_even_fibs(targetValue)
                integer(int64), intent(in) :: targetValue
                integer(int64) :: a, b, temp, res, i
                
                a = 0
                b = 1
                temp = 0

                do while(b <= targetValue)
                        if (MOD(b, 2) == 0) then
                                res = res + b
                        end if
                        temp = a + b
                        a = b
                        b = temp        
                end do
                print *, res
        end subroutine sum_even_fibs

end program even_fib
