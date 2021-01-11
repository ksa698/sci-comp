	program test_random
	implicit none

	real, dimension(100) :: r
	integer :: i
	integer :: maxLocation
	real :: maxValue

	call random_number(r)

	do i=1, 101
		r(i) = r(i)*100
	end do

	print *
	print *
	print *, r
	
	maxValue= findmaxval(maxLocation, 100, r)
	
	print *, "Max value :: ", maxValue
	print *, "Location is :: ", maxLocation
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	contains

	real function findmaxval(maxlocation, arraysize, r1)

	implicit none
	integer, intent(out) :: maxlocation
	integer :: i
	integer :: arraysize
	real, dimension(arraysize) :: r1
	real :: curmax

	curmax =r1(1)
	do i=1,arraysize
		if (r1(i) > curmax) then !to find min just switch to <
			curmax = r1(i)
			maxlocation = i
		end if
	end do

	findmaxval = curmax 


	end function 
	end program
