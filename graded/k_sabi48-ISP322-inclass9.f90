       program challenge
       implicit none
       integer :: a, b, c, d_
       real :: p, Vs, Vr, d

       a=3 !we will use this as the sphere radius as well :)
       b=4
       c=5
       p=3.14159
!For a sphere Volume
       Vs=(4.0/3)*p*a**3
!For a rectangular prism
       Vr=a*b*c
       d=(a*b*c*1.0)/7
       !d_=((a*b*c)/7)
       d_=int(d)

       print *, "Hello my name is Sabrina!"
       print *, "My a, b, c values in meters are: ",a, b, c
       print *, "Volume of a sphere is [m^3]: ",Vs
       print *, "Volume of a rectangular prism is [m^3]: " ,Vr
       print *, "d is: ",d
       print *, "d as an integer is: ",d_
       end program
