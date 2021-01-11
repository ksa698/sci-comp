!Kiara Sabrina Alvarez ksa698
!Program: module prime numbers 
!Function: use a module to find all prime numbers <100 
!Created 11/15/20
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Instructions (LOOK AT LECTURE 3, SLIDE 17)
	!write module, PrimeNum
	!functions
		!IsPrime(n) n=int returns true/false 
		!NextPrime(n) returns next prime num after n
		!PreviousPrime(n) returns previous prime num before n

	module PrimeNum
	implicit none
	integer :: i,j,n,o

	contains
		logical function IsPrime(n)
			integer :: n

			do j = 2,n
				if (n == 0 .or. n == 1) then
					IsPrime = .false.
					exit
				else if (n == 2) then	!2 is prime
					IsPrime = .true.
					exit
				else if (mod(n,j) == 0 .and. n == j) then	!1 case of not prime
					IsPrime = .true.
					exit
				else if (mod(n,j) == 0) then
					IsPrime = .false.
					exit
				end if
			end do
		end function 

		integer function NextPrime(n) result(i)
		integer :: n
		
		do i =n+1,(n+2)*4	!look for next number, n+2*4 in case n=0
			if (IsPrime(i) == .false.) then
				cycle
			else if (IsPrime(i) == .true.) then	!check that it is prime
				!print *, i, ": is next prime number"
				exit
			end if
		end do
		end function

		integer function PreviousPrime(n) result(o)
		integer :: n
	
		do o =n-1,n-((n+2)*4),-1
			if (IsPrime(o) == .false.) then
				cycle
			else if (IsPrime(o) == .true.) then
				exit
			end if
		end do
		end function

	end module PrimeNum

	program primeboi
	use PrimeNum
	implicit none
	n=1001
		print *, "n =", n
		print *, "T/F  is the number prime?",IsPrime(n)
		print *, "Next prime number is:",NextPrime(n)
		print *, "Previous prime number from n is:",PreviousPrime(n)
	end program
