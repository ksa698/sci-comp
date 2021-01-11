!read in integer, use read command 
! if mult of 3 print fizz 
!use mod(A,P)
!if mult of 5 print buzz
!if mult of both is fizzbuzz
!otherwise print nothing
	program fizzbuzz
	implicit none

	integer :: a
	print *, "Enter a number: "
	read *, a

        if (mod(a,3)==0 .and. mod(a,5)==0) then
                print *, "fizzbuzz"
	else if (mod(a,3) ==0) then
		print *, "fizz"
	else if (mod(a,5) ==0) then
		print *, "buzz"
	end if

	end program
