module even_fib_mod
        implicit none
contains
        function sum_even_fibs(targetValue) result(res)
                integer(kind=8), intent(in) :: targetValue
                integer(kind=8) :: a, b, temp, res

                a = 0
                b = 1
                res = 0

                do while (b <= targetValue)
                        if (mod(b, 2) == 0) then
                                res = res + b
                        end if
                        temp = a + b
                        a = b
                        b = temp
                end do
        end function sum_even_fibs

end module even_fib_mod

