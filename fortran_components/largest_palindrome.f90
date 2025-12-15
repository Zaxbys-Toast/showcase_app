module largest_palindrome
        implicit none
contains

        subroutine find_largest_pal(best_i, best_j, largest_pal)
                integer, intent(out) :: best_i, best_j, largest_pal
                integer :: i, j, product

                largest_pal = 0

                do i = 999, 100, -1
                        do j = 999, 100, -1
                                product = i * j

                                if (has_pal(product)) then
                                        if (product > largest_pal) then
                                                largest_pal = product
                                                best_i = i
                                                best_j = j
                                        end if
                                end if
                        end do
                end do
        end subroutine find_largest_pal

        logical function has_pal(num)
                integer, intent(in) :: num
                integer :: temp, reverse

                temp = abs(num)
                reverse = 0

                do while (temp /= 0)
                reverse = reverse * 10 + mod(temp, 10)
                temp = temp / 10
                end do

                has_pal = (reverse == num)
        end function has_pal

end module largest_palindrome

