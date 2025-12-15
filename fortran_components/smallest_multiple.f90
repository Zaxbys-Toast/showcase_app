module smallest_multiple
        implicit none
contains
        function gcd(a, b) result(r)
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
        end function gcd

        function lcm(a, b) result(r)
                integer, intent(in) :: a, b
                integer :: r
                r = a * b / gcd(a, b)
        end function lcm

        function smallest_multiple(maxDiv) result(n)
                integer, intent(in) :: maxDiv
                integer :: i, n
                n = 1
                do i = 1, maxDiv
                       n = lcm(n, i)
                end do
        end function smallest_multiple
end module smallest_multiple
