program test
    implicit none
    real x,y
    integer q
    x=5
    y=100
    call subr(x,y,10)
    print *,x,y
end program test

subroutine subr(x,y,n)
    implicit none
    real x,y
    integer n
    x=n
    y=y*x
end subroutine
