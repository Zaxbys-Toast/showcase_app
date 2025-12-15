module multiples_sum
        use iso_fortran_env, only: int64
        implicit none
contains 

        function multiples_sum(endValue) result(x)
                integer(int64), intent(in) :: endValue
                integer(int64) :: x, i
                
                if (endValue < 1_int64) then
                        x = 0
                        return
                end if

                x = 0
                do i = 1, endValue
                        if (MOD(i,3) == 0 .OR. MOD(i,5) == 0) then
                                x = x + i
                        end if
                end do
        end function multiples_sum

end module multiples_sum
