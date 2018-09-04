program simple
implicit none
   
   interface
      pure function f(x)
         real(8), intent(in) :: x
         real(8) :: f
      end function
      pure function exact(x)
         real(8), intent(in) :: x
         real(8) :: exact
      end function
   end interface

  integer :: expn, i

   print *, "Exponent:"
   read(*,*) expn

   print *, f(expn*1d0), exact(expn*1d0)  
end program

pure function f(x)
   real(8), intent(in) :: x
   real(8) :: f

   f = 100 * exp(-10 * x)
end function

pure function exact(x)
   real(8), intent(in) :: x
   real(8) :: exact

   exact = 1 - (1 - exp(-10d0)) * x - exp(-10 * x)
end function
