include '../../src/sort_mod.f90'
    
program test_sort
use sort_mod
implicit none
    integer                 :: k1                   ! iterator
    double precision        :: tda1(10), tda2(10)   ! double arrays
    integer                 :: tia1(8), tia2(8)     ! integer arrays
    integer, allocatable    :: sid(:), sii(:)       ! sort_inds
    
    double precision, allocatable   :: unique_arr(:)
    double precision                :: unique_tol = 1.d-1
    
    logical             :: test_failed = .false.
    
    tda1 = [0.45933988, 0.85925698, 0.19866538, 0.19590951, 0.26951959, &
            0.21166847, 0.58193321, 0.54999474, 0.74012868, 0.58838597]
    tda2 = tda1
    
    tia1 = [14, -56, 86, 3, 94, -44, -55, -68]
    tia2 = tia1
    
    call sortinds(tda1, sid)
    ! Check that sortinds does not have any side effects
    if (.not.all(tda1==tda2)) then
        write(*,*) 'sortinds affected original double array'
        test_failed = .true.
    endif
    call sort(tda1)
    ! Check that sorted array is (weakly) ascending
    do k1=2,size(tda1)
        if (tda1(k1-1) > tda1(k1)) then
            write(*,*) 'sorted dbl array not at least weakly ascending'
            test_failed = .true.
        endif
    enddo
    ! Check that sortinds and sort produces the same results
    if (.not.all(tda1==tda2(sid))) then
        write(*,*) 'sortinds does not give the same order of elements as sort (dbl)'
        test_failed = .true.
    endif
    
    call sortinds(tia1, sii)
    ! Check that sortinds does not have any side effects
    if (.not.all(tia1==tia2)) then
        write(*,*) 'sortinds affected original int array'
        test_failed = .true.
    endif
    call sort(tia1)
    ! Check that sorted array is (weakly) ascending
    do k1=2,size(tia1)
        if (tia1(k1-1) > tia1(k1)) then
            write(*,*) 'sorted int array not at least weakly ascending'
            test_failed = .true.
        endif
    enddo
    
    ! Check that sortinds and sort produces the same results
    if (.not.all(tia1==tia2(sii))) then
        write(*,*) 'sortinds does not give the same order of elements as sort (dbl)'
        test_failed = .true.
    endif
    
    ! Test unique
    call unique(tda2, unique_arr, unique_tol)
    do k1=2,size(unique_arr)
        if (unique_arr(k1)<=unique_arr(k1-1)) then
            write(*,*) 'unique array not strictly ascending'
            test_failed = .true.
        endif
    enddo
    
    if (test_failed) then
        error stop 1
    else
        stop 0
    endif
    
end program