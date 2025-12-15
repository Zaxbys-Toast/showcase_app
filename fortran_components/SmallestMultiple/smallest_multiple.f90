program smallest_multiple
        implicit none
        integer :: i, n
        logical :: is_divisible

        n = 1

        do
               is_divisible = .true.      ! Assume true until proven false

                do i = 1, 20
                        if (mod(n, i) /= 0) then
                                is_divisible = .false.
                                exit
                        end if
                end do

                if (is_divisible) exit
                n = n + 1
        end do

        print *, n
end program smallest_multiple

