include '../../src/find_mod.f90'
    
program test_sort
use find_mod
implicit none
    integer                 :: k1                   ! iterator
    double precision        :: tda1(10), tda2(10)   ! double arrays
    integer                 :: tia1(8), tia2(8)     ! integer arrays
    integer, allocatable    :: sid(:)               ! sort_inds
    
    integer                 :: find_int = 86        ! Value to find
    integer                 :: find_int_ind = 3     ! Position to find (int)
    double precision        :: find_dbl = 0.8592    ! Value to find
    integer                 :: find_dbl_ind = 2     ! Position to find (dbl)
    double precision        :: find_tol = 1.e-3     ! Find tolerance
    
    logical             :: test_failed = .false.
    
    ! Functions to test
    ! - find_int_1d - OK
    ! - find_int_2d - OK
    ! - find_dbl_1d_tol - TBA
    ! - find_dbl_2d_tol - TBA
    ! - find_dbl_pt_3d_array - TBA
    
    tia1 = [14, -56, 86, 3, 94, -44, -55, -68]
    tia1(find_int_ind) = find_int
    tia2 = tia1
    
    tda1 = [0.45933988, 0.85925698, 0.19866538, 0.19590951, 0.26951959, &
            0.21166847, 0.58193321, 0.54999474, 0.74012868, 0.58838597]
    tda1(find_dbl_ind) = find_dbl
    tda2 = tda1
    
    if (find(tia1, find_int) /= find_int_ind) then
        write(*,"(A,F0.5,A)") 'Could not find ', find_int, ' in:'
        write(*,*) tia1
        test_failed = .true.
    endif
    if (.not.all(tia1==tia2)) then
        write(*,*) 'call to find affected the searched integer array'
        test_failed = .true.
    endif
    
    
    if (find(tda1, find_dbl-find_tol/2, find_tol) /= find_dbl_ind) then
        write(*,"(A,F0.5,A,F0.5,A)") 'Could not find ', find_dbl-find_tol/2, ' with tol = ', find_tol, ' in:'
        write(*,*) tda1
        test_failed = .true.
    endif
    if (.not.all(tda1==tda2)) then
        write(*,*) 'call to find affected the searched double array'
        test_failed = .true.
    endif
    
    
    if (test_failed) then
        error stop 1
    else
        stop 0
    endif
    
end program