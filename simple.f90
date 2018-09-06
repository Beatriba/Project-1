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

   integer :: expn, i, n
   real(8) :: h, h_sq        
   real(8), dimension(:), allocatable :: d, solution, b, analytic

   print *, "Exponent:"
   read(*,*) expn

   n = 10d0 ** expn
   h = 1d0 / n
   h_sq = h * h
 
   allocate(d(1:n-1), solution(0:n), b(1:n-1), analytic(0:n))

   solution(0) = 0; solution(n) = 0
   do i = 1, n-1
      d(i) = (1d0 + i) / i
      b(i) = h_sq * f(i*h)
   end do
   
   do i = 2, n-1
      b(i) = b(i) + b(i-1) / d(i-1)
   end do
      solution(n-1) = b(n-1) / d(n-1)
   do i = n-1, 2, -1
      solution(i-1) = (solution(i) + b(i-1)) / d(i-1)
   end do
   do i = 0, n
      analytic(i) = exact(i*h)
   end do

do i = 10, 20
   print *, solution(i), analytic(i)
end do
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
