module multiples_sum_mod
        implicit none
contains
        function multiples_sum_func(endValue) result(x)
                integer(kind=8), intent(in) :: endValue
                integer(kind=8) :: x, i

                x = 0
                if (endValue < 1_8) return

                do i = 1, endValue
                        if (mod(i,3) == 0 .or. mod(i,5) == 0) then
                                x = x + i
                        end if
                end do
        end function multiples_sum_func
end module multiples_sum_mod

