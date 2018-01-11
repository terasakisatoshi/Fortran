program gauss_pivot
    implicit none
    real a(4,4),b(4),x(4),det 
    a(1,1)=0;a(1,2)=4;a(1,3)=5;a(1,4)=2
    a(2,1)=1;a(2,2)=0;a(2,3)=2;a(2,4)=-6
    a(3,1)=4;a(3,2)=1;a(3,3)=0;a(3,4)=-2
    a(4,1)=1;a(4,2)=7;a(4,3)=1;a(4,4)=0
    b(1)=9  ;b(2) =-3;b(3)  =1;b(4)  =-3
    call gaussian_pivot(a,b,4,x,det)
    print *,'X=',x(1),x(2),x(3),x(4),det
end program gauss_pivot

subroutine gaussian_pivot(a,b,n,x,det)
    implicit none
    real a(n,n),b(n),x(n),det,dd,amax
    integer n,ipv(n),i,j,k,imax,ip

    do i =1,n
        ipv(i)=i
    enddo

    ip=1
    do k=1,n-1
        amax=a(ipv(k),k)
        imax=k
        do i = k+1,n
            if (amax<abs(a(ipv(i),k))) then
                amax = abs(a(ipv(i),k))
                imax=i
            endif
        enddo
        if(imax/=k) then
            j=ipv(k)
            ipv(k)=ipv(imax)
            ipv(imax)=j
            ip=-ip
        endif
        do i=k+1,n
            dd=a(ipv(i),k)/a(ipv(k),k)
            do j=k+1,n
                a(ipv(i),j)=a(ipv(i),j)-dd*a(ipv(k),j)
            enddo
            b(ipv(i))=b(ipv(i))-dd*b(ipv(k))
        enddo
    enddo
    x(n)=b(ipv(n))/a(ipv(n),n)
    do i=n-1,1,-1
        dd = b(ipv(i))
        do j=i+1,n
            dd=dd-a(ipv(i),j)*x(j)
        enddo
        x(i)=dd/a(ipv(i),i)
    enddo
    det = a(ipv(1),1)
    do i =2,n 
        det=det*a(ipv(i),i)
    enddo
    if(ip<0) det=-det     
end subroutine gaussian_pivot