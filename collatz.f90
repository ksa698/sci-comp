	program collatz
	implicit none
	
	integer :: u1, u0, chain_length, max_length, umax
!	read *, u1
	umax=0
	max_length=0

	do u0=1,1000 !automatically steps 1 value
		u1=u0
		chain_length=1

	do
		!print *, u1
	
		if (u1 ==1) then
			exit
		end if 

		if (mod(u1, 2) ==0) then
			u1=u1/2
		else
			u1=3*u1+1
		end if
		chain_length=chain_length+1
	end do
		if (chain_length > max_length) then
			max_length = chain_length
			umax = u0
		end if
	end do
	
	print *,umax,max_length
	end program
