	program test_rand_num
	implicit none
	real, dimension(100) :: r
	integer, dimension(100) :: r2


	call random_number(r)

	r2=100*r
	print *, r2
	print *
	print *, r2(10:50)
	print *
	print *, r2(10:50:5)
	print *
	print *, r2(:)

	end program
