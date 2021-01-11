!Kiara Sabrina Alvarez ksa698
!Program: prime numbers
!Function: find all positive prime numbers <100 
!Created 11/14/20

	program prime
	implicit none
	integer :: i,j,count
	count=100

	!print *, "	   0 is neither prime nor composite"
	!print *, "   	   1 is neither prime nor composite"
	
	do i =0,count
		if (i == count) then
			exit
		end if

		do j = 2,count
			if ( i == 0 .or. i == 1) then
				print *,i, "is neither prime nor composite"
				exit
			else if (i == 2) then
				print *,i, "is prime!!! WOOOO"
				exit	
			else if	(i == j) then
				cycle
			else if (mod(i,j) == 0) then
				print *, i, ":is NOT prime. Divisible by:",j
				exit
			else if (j==count) then
				print *, i, "is prime!!! WOOOO"
				exit
			end if
		end do
	end do
	end program
