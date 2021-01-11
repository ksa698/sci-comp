!Kiara Sabrina Alvarez ksa698
!Program: module prime numbers 
!Function: Goldbach Conjecture 
!Created 12/07/20
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!write module, PrimeNum
	!functions
		!IsPrime(n) n=int returns true/false 
		!NextPrime(n) returns next prime num after n
		!PreviousPrime(n) returns previous prime num before n

	module PrimeNum
	implicit none
	real :: sum
	integer :: i,j,n,o,r,q,p
	integer, dimension(10000) :: dist

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
	end module PrimeNum
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	program primeboi
	use PrimeNum
	implicit none
	p=3

                do n=1,10000               !Gets multiple prime values p
	                p=NextPrime(p)
			q=NextPrime(p)
			do while (.true.)
				r=p+(p-q)
				if (IsPrime(r)== .true.) then
					!print *,p,q,r, q-p
					!print *, p-r, q-p
					dist(n)=q-p
					exit
						
				else 
					q=NextPrime(q)
				end if
			end do
		end do
!STATS	
	sum=0
		do o=1,10000
			sum=sum+dist(o)
		end do
		
!mean, mode, median=sort
		print *, "Too save you from eye strain the distance array was not made an output :) "
                print *, "Maximum value:", maxval(dist)
                print *, "Minimum value:", minval(dist)
		print *, "Average:", sum/10000
		!print *, dist
	end program

