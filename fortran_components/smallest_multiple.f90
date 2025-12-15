module smallest_multiple_mod
        implicit none
contains
        function gcd_func(a, b) result(r)
                integer, intent(in) :: a, b
                integer :: r, x, y
                x = a
                y = b
                do while (y /= 0)
                        r = mod(x, y)
                        x = y
                        y = r
                end do
                r = x
        end function gcd_func

        function lcm_func(a, b) result(r)
                integer, intent(in) :: a, b
                integer :: r
                r = a * b / gcd_func(a, b)
        end function lcm_func

        function smallest_multiple_func(maxDiv) result(n)
                integer, intent(in) :: maxDiv
                integer :: n, i

                n = 1
                do i = 1, maxDiv
                        n = lcm_func(n, i)
                end do
        end function smallest_multiple_func
end module smallest_multiple_mod

