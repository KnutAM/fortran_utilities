include '../../src/linalg_mod.f90'    
program test_linalg
use linalg_mod
implicit none
    logical             :: test_failed = .false.
    
    if (test_failed) then
        error stop 1
    else
        stop 0
    endif
    
end program