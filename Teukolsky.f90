program Teukolsky
implicit none

interface
   subroutine back_for_sub(a,b,c,f,solution)
      real(8), dimension(:), intent(in) :: a, b, c, f
      real(8), dimension(:), intent(out) :: solution
   end subroutine
   pure function f(x)
      real(8), intent(in) :: x
      real(8) :: f
   end function
   pure function analytic(x)
      real(8), intent(in) :: x
      real(8) :: analytic
   end function
end interface

   real(8), dimension(:), allocatable :: a, b, c, d, solution
   real(8) :: h, h_sq, temp, expn
   integer :: n, i

   print *, "Exponent:"
   read(*,*) expn

   n = 10d0 ** expn

   allocate(a(2:n), b(1:n), c(1:n-1), d(1:n), solution(0:n+1))

   h = 1d0 / n
   h_sq = h * h
   do i = 1, n
      d(i) = h_sq * f(i*h)
   end do
   a = -1; b = 2; c = -1
  
   call back_for_sub(a,b,c,d,solution)

   do i = 10, 20
      print *, solution(i), analytic(i*h)
   end do

end program

pure function f(x)
   real(8), intent(in) :: x
   real(8) :: f
   
   f =100 * exp(-10 * x)
end function

pure function analytic(x)
   real(8), intent(in) :: x
   real(8) :: analytic

   analytic = 1. - (1. - exp(-10.)) * x - exp(-10. * x)
end function

subroutine back_for_sub(a,b,c,f,solution)
implicit none

   real(8), dimension(:), intent(in) :: a, b, c, f
   real(8), dimension(:), intent(out) :: solution

   real(8), dimension(:), allocatable :: temp
   real(8) :: btemp
   integer :: i, n

   n=size(b)

   allocate(temp(n))

   btemp = b(1)
   solution(1) = f(1) / btemp

   do i = 2, n
      temp(i) = c(i-1) / btemp 
      btemp = b(i) - a(i) * temp(i)
      solution(i) = (f(i) - a(i) * solution(i-1)) / btemp
   end do

   do i = n-1, 1, -1
      solution(i) = solution(i) - temp(i) * solution(i+1)
   end do

end subroutine
