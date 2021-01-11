!Kiara Sabrina Alvarez ksa698
!Program: module prime numbers 
!Function: Goldbach Conjecture 
!Created 12/08/20
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!write module, PrimeNum
	!functions
		!IsPrime(n) n=int returns true/false 
		!NextPrime(n) returns next prime num after n
		!PreviousPrime(n) returns previous prime num before n

	module PrimeNum
	implicit none
	integer :: i,j,n,o,r,q,p
!	integer, dimension(1,1) :: array

	contains
	!!This function says if number is prime T/F
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

!!Create new function to find the next prime
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Make new function 
		integer function PrimeSum(n) result(r)
		integer :: n

		do n=1,10		!Gets multiple prime values p
			do r=1,10	!Loops until r is prime
			if (IsPrime(n)== .true.) then
				p=n
				q=NextPrime(n)
				r=p+(p-q)
			else
				p=NextPrime(n)
				q=NextPrime(NextPrime(n))
				r=p+(p-q)	
			end if

			if (IsPrime(r)== .true.) then
				print *, p, q, r
				print *, q-p
				exit
			else
				cycle
			end if

			end if
		end function




	end module PrimeNum

	program primeboi
	use PrimeNum
	implicit none
	n=100
		print *, "n =", n
!		print *, "T/F  is the number prime?",IsPrime(n)
!		print *, "Next prime number is:",NextPrime(n)
!		print *, "Previous prime number from n is:",PreviousPrime(n)
		print *, "r = ", PrimeSum(n)
	end program
