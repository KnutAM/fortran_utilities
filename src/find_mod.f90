module find_mod
implicit none

private

public  :: find                 ! Find position in an array being equal to a given value

interface find
    procedure find_int_1d, find_int_2d, find_dbl_1d_tol, find_dbl_2d_tol, &
              find_dbl_pt_3d_array
end interface find


contains

function find_int_1d(array, val_to_find) result(ind)
implicit none
    integer, intent(in) :: array(:)
    integer, intent(in) :: val_to_find
    integer             :: ind, tmp_ind(1)
    
    tmp_ind = minloc(abs(array - val_to_find))
    ind = tmp_ind(1)
    
    if (abs(array(ind)-val_to_find) /= 0) ind = -1
    
    
end function find_int_1d
    
function find_int_2d(array, val_to_find) result(ind)
implicit none
    integer, intent(in) :: array(:,:)
    integer, intent(in) :: val_to_find
    integer             :: ind(2)
    
    ind = minloc(abs(array - val_to_find))
    
    if (abs(array(ind(1), ind(2))-val_to_find) /= 0) ind = -1
    
end function find_int_2d

function find_dbl_1d_tol(array, val_to_find, tol) result(ind)
implicit none
    double precision, intent(in)    :: array(:,:)
    double precision, intent(in)    :: val_to_find
    double precision, intent(in)    :: tol
    integer                         :: ind, tmp_ind(1)
    
    tmp_ind = minloc(abs(array - val_to_find))
    ind = tmp_ind(1)
    
    if (abs(array(ind)-val_to_find) > tol) ind = -1
    
end function find_dbl_1d_tol

function find_dbl_2d_tol(array, val_to_find, tol) result(ind)
implicit none
    double precision, intent(in)    :: array(:,:)
    double precision, intent(in)    :: val_to_find
    double precision, intent(in)    :: tol
    integer                         :: ind(2)
    
    ind = minloc(abs(array - val_to_find))
    
    if (abs(array(ind(1), ind(2))-val_to_find) > tol) ind = -1
    
end function find_dbl_2d_tol

function find_dbl_pt_3d_array(array, point_to_find, point_dim, tol) result(ind)
implicit none
    double precision, intent(in)    :: array(:,:,:)
    double precision, intent(in)    :: point_to_find(:)
    integer, intent(in)             :: point_dim    ! Which dimension of array is the point along
    double precision, intent(in)    :: tol
    integer                         :: ind(2)
    
    double precision, allocatable   :: error_array(:,:)
    integer                         :: k1
    
    if (size(array, point_dim) /= size(point_to_find)) then
        write(*,*) 'ERROR: size(array, point_dim) must be equal to size(point_to_find)'
        write(*,*) '       Returning not found'
        ind = -1
        return 
    endif
    
    if (point_dim == 1) then
        allocate(error_array(size(array, 2), size(array, 3)))
    elseif (point_dim==2) then
        allocate(error_array(size(array, 1), size(array, 3)))
    else
        allocate(error_array(size(array, 1), size(array, 2)))
    endif
    error_array = 0.d0
    
    do k1=1,size(point_to_find)
        if (point_dim == 1) then
            error_array = error_array + (array(k1,:,:) - point_to_find(k1))**2
        elseif (point_dim==2) then
            error_array = error_array + (array(:, k1,:) - point_to_find(k1))**2
        else
            error_array = error_array + (array(:, :, k1) - point_to_find(k1))**2
        endif
    enddo
    
    ind = find_dbl_2d_tol(error_array, 0.d0, tol**2)
    
end function



end module