	program idMatrix
	implicit none
	integer, dimension(:,:), allocatable :: A
	integer :: r,c,ierror

	print *, "Rows, cols"
	read *, r, c
	allocate (A(r,c), stat=ierror) !catch error
	if (ierror /= 0) stop 'error allocating A'


	A = identity(r,c)
	print *, A

	contains 

	function identity(rows, cols)
	implicit none

	integer, intent(in) :: rows, cols
	integer :: i, j
	integer, dimension(rows, cols) :: identity
	identity = 0
	do i=1, rows
		identity(i,i) = 1
	end do

	end function
	end program
