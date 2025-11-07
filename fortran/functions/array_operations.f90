! Array operations in Fortran

program array_operations
    implicit none

    integer, parameter :: n = 5
    integer :: numbers(n), i, sum_val, product_val
    real :: avg

    write(*,*) ''
    write(*,*) '=== Array Operations in Fortran ==='
    write(*,*) ''

    ! Initialize array
    numbers = [1, 2, 3, 4, 5]

    ! Print array
    write(*,'(A,5I3)') 'Numbers:', numbers

    ! Array length
    write(*,'(A,I3)') 'Length:', size(numbers)

    ! Access elements
    write(*,'(A,I3)') 'First element:', numbers(1)
    write(*,'(A,I3)') 'Last element:', numbers(n)

    ! Array operations
    write(*,*) ''
    write(*,'(A,5I3)') 'Add 10 to each:', numbers + 10
    write(*,'(A,5I3)') 'Multiply by 2:', numbers * 2
    write(*,'(A,5I3)') 'Square:', numbers ** 2

    ! Sum
    sum_val = sum(numbers)
    write(*,*) ''
    write(*,'(A,I3)') 'Sum:', sum_val

    ! Product
    product_val = product(numbers)
    write(*,'(A,I6)') 'Product:', product_val

    ! Average
    avg = real(sum_val) / real(n)
    write(*,'(A,F6.2)') 'Average:', avg

    ! Min and max
    write(*,'(A,I3)') 'Minimum:', minval(numbers)
    write(*,'(A,I3)') 'Maximum:', maxval(numbers)

    ! Element-wise operations
    write(*,*) ''
    write(*,*) 'Doubled:'
    do i = 1, n
        write(*,'(I3)') numbers(i) * 2
    end do

end program array_operations
