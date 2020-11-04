module linalg_mod
implicit none

private

public  :: norm                 ! Get the norm of a vector or matrix

interface norm
    procedure norm_dbl_1d, norm_dbl_2d
end interface norm


contains

! Norm function
function norm_dbl_1d(array) result(the_norm)
implicit none
    double precision, intent(in)    :: array(:)
    double precision                :: the_norm
    
    the_norm = sqrt(sum(array**2))
    
end function norm_dbl_1d
    
function norm_dbl_2d(array) result(the_norm)
implicit none
    double precision, intent(in)    :: array(:,:)
    double precision                :: the_norm
    
    the_norm = sqrt(sum(array**2))
    
end function norm_dbl_2d

end module linalg_mod