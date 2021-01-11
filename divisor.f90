	program divisor
	implicit none
	
	integer :: a,b
	print *, "Give me a and b values:"
	read *, a
	read *, b

	if (mod(a,b) ==0) then
		print *, b, "is a divisor of", a
	else
		print *, b, "is not a divisor of", a
	end if

	end program

