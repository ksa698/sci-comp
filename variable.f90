       program vars
       implicit none
       integer :: i, j
       real :: x,y
!end of declaration so now we go to execute

       i=1;
       j=5
       x=real(i)/j  !when it was i/j the output was just 0
       y=i*real(j)/2

       print *, i, j
       print *, x, y
       end program
