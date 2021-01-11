!u^2+v^2 = w^2
!Finds perfect triangles woooo
!Lord Charlie has granted permission to submit this code
	program challenge10
	implicit none
	integer :: u,v,w
	real :: r,w1


	do u=1,100
		do v=u,100   !v=u removes duplicates
			r=u**2+v**2 !by having r as real we can input for sqrt
			w1=sqrt(r) !sqrt only takes real values not int
			w=w1
	
			if (w==w1 .and. w<=100) then
				print *, "A triple!", u,v,w
			end if
		end do
	end do

	end program
