module largest_prime
        implicit none
contains
        function compute_largest_prime(n) result(res)
                integer(kind=8), intent(in) :: n
                integer(kind=8) :: i, res, target

                target = n

                ! End early if less than 2
                if (n <= 1) then
                        res = 0
                        return
                end if

                res = 0

                ! Remove factor 2 first
                i = 2_8
                do while (mod(target, i) == 0)
                        res = i
                        target = target / i
                end do

                ! Check odd numbers starting from 3
                i = 3_8
                do while (i*i <= target)
                        if (mod(target, i) == 0) then
                                res = i
                                target = target / i
                        else
                                i = i + 2_8
                        end if
                end do

                ! If remaining number is greater than 1, it is prime
                if (target > 1_8) then
                        res = target
                end if
        end function compute_largest_prime
end module largest_prime

