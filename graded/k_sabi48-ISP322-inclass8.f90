!COMMENTS
!ifort -o name.bin name.F90

       program hello

       implicit none

       print *, & "Hello World", " Hello ME!"
       print *, "One divided by three (incorrectly)= ", 1/3
       print *, "The reason this is wrong is that the math operation is done on integer type numbers so we must change the type of the numerator"
       print *, "One divided by three= ", 1.0/3
       end program
