program largest_prime
        use iso_fortran_env, only: int64
        implicit none

        integer(int64) :: targetValue
        integer :: ios
        character(len=128) :: arg1

        call get_command_argument(1, arg1)

        if (len_trim(arg1) == 0 .or. iargc() /= 1) then
                print *, "Usage: ./<program_name> <num>"
                stop
        end if

        read(arg1, *, iostat=ios) targetValue

        if (ios /= 0 .or. targetValue <= 1) then
                print *, "Error: Argument must be an integer greater than 1."
                stop
        end if

        call compute_largest_prime(targetValue)
contains
        subroutine compute_largest_prime(n)
                integer(int64), intent(in) :: n
                integer(int64) :: i, res, target

                target = n
                res = 0

                ! Remove factor 2 first
                i = 2_int64
                do while (mod(target, i) == 0)
                res = i
                target = target / i
                end do

                ! Check odd numbers starting from 3
                i = 3_int64
                do while (i*i <= target)
                if (mod(target, i) == 0) then
                        res = i
                        target = target / i
                else
                        i = i + 2_int64
                end if
                end do

                ! If remaining number is greater than 1, it is prime
                if (target > 1_int64) then
                        res = target
                end if

                print *, res
        end subroutine compute_largest_prime

end program largest_prime
