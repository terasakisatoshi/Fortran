program gauss4by4
    implicit none
    real a(4,4),b(4),x(4),det
    integer i,j

    a(1,1)=2;a(1,2)= 4;a(1,3) = 5;a(1,4)= 2;
    a(2,1)=1;a(2,2)=-8;a(2,3) = 2;a(2,4)=-6;
    a(3,1)=4;a(3,2)= 1;a(3,3)=-10;a(3,4)=-2;
    a(4,1)=1;a(4,2)= 7;a(4,3) = 1;a(4,4)=-2;
    b(1)  =9;b(2)  =-3;b(3)   = 1;b(4)  =-3;
    call gaussian(a,b,4,x,det)
    print *,'X=',x(1),x(2),x(3),x(4),det
end program gauss4by4

subroutine gaussian(a,b,n,x,det)
    implicit none
    real a(n,n),b(n),x(n),det,dd
    integer n,i,j,k,row,col
    row=n;col=n
    do k=1,row
        do i=k+1,row
            dd=a(i,k)/a(k,k)
            do j = k+1,col
                a(i,j)=a(i,j)-dd*a(k,j)
            enddo
            b(i)=b(i)-dd*b(k)
        enddo
    enddo
    x(n)=b(n)/a(n,n)
    do i = n-1,1,-1
        dd=b(i)
        do j = i+1,n
            dd=dd-a(i,j)*x(j)
        enddo
        x(i)=dd/a(i,i)
    enddo
    det = a(1,1)
    do i=2,n
        det = det*a(i,i)
    enddo

end subroutine gaussian