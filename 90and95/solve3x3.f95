program solve3x3
    implicit none
    real a(3,3),b(3),det,num,x1,x2,x3
    a(1,1)=6;a(1,2)=4;a(1,3)=-5;b(1)=9;
    a(2,1)=1;a(2,2)=-8;a(2,3)=2;b(2)=-3;
    a(3,1)=4;a(3,2)=1;a(3,3)=-10;b(3)=12;

    call determinant33(a(1,1),a(1,2),a(1,3),det)
    call determinant33(b,a(1,2),a(1,3),num)
    x1=num/det
    call determinant33(a(1,1),b,a(1,3),num)
    x2=num/det
    call determinant33(a(1,1),a(1,2),b,num)
    x3=num/det
    print *,'X1,X2,X3=',x1,x2,x3
end program solve3x3

subroutine determinant33(u,v,w,det)
    implicit none
    real :: u(3),v(3),w(3),det
    det=u(1)*v(2)*w(3)+u(2)*v(3)*w(1)+u(3)*v(1)*w(2) &
       -w(1)*v(2)*u(3)-w(2)*v(3)*u(1)-w(3)*v(1)*u(2)
end subroutine determinant33