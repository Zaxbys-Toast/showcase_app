module largest_prime
        use iso_fortran_env, only: int64
        implicit none

contains
        function compute_largest_prime(n) result(res)
                integer(int64), intent(in) :: n
                integer(int64) :: i, res, target

                target = n

                ! End early if less than 2
                if (n <= 1_int64) then
                        res = 0
                        return
                end if

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
        end function compute_largest_prime

end module largest_prime
