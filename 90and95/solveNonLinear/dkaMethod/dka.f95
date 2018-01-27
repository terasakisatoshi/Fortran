!Reference: 田口俊弘 著 Fortran ハンドブック技術評論社１４４ｐ
program dka
    implicit none
    real pa(0:10),eps
    complex zroot(10),zz 
    integer i,n,ind,j 
    pa(0) = 580 
    pa(1) = 1398
    pa(2) = 232
    pa(3) = 593 
    pa(4) = -708
    pa(5) = 180 
    n = 5 
    eps = 1e-7
    call dkaroot(pa,n,zroot,eps,ind)
    do i=1,n 
        zz=0 
        do j=n,0,-1 
            zz=zz*zroot(i)+pa(j) 
        enddo 
        print *,i,zroot(i),abs(zz),ind 
    enddo 
end program dka

subroutine dkaroot(pa,n,zroot,eps,ind)
    implicit none
    real pa(0:n),p1(0:n-1),p2(0:n),eps,beta,rad,ang,del 
    complex zroot(*),z(n),dz,zp,zd
    integer n,ind,i,j,it 
    integer,parameter :: itmax=1000000
    real,parameter :: pi2=6.28319
    
    if(pa(n)==0) then 
        ind = -1 
        return 
    endif 
    !convert p1 into monic 
    p2(n) = 1
    do i=0,n-1
        p1(i)=pa(i)/pa(n)
        p2(i)=p1(i)
    enddo 

    beta = p1(n-1)/n
    do i=0,n-2 
        do j = n-1, i, -1 
            p2(j) = p2(j+1)*beta + p2(j) 
        enddo 
        p2(i) = -abs(p2(i))
    enddo 
    p2(n-1)=0

    call proot1_newton(p2,n,rad)
    do i=1,n
        ang=(pi2*(i-1)+1.5)/n
        z(i)=-beta+rad*cmplx(cos(ang),sin(ang))
    enddo

    do it = 1, itmax 
        del = 0 
        do i=1,n 
            zp=1
            zd=1
            do j = n-1,0,-1 
                zp=zp*z(i)+p1(j)
                if (j+1/= i ) zd=zd*(z(i)-z(j+1))
            enddo   
            if (zd==0) then 
                ind = -2 
                return 
            endif 
            dz=-zp/zd 
            del = max(del,abs(dz))
            z(i)=z(i)+dz 
        enddo 
        if(del<eps) exit 
    enddo
    if(it>itmax)then 
        ind = -itmax 
    else 
        ind = it 
    endif 
    do i=1,n
        zroot(i)=z(i)
    enddo 
end subroutine dkaroot

subroutine proot1_newton(p1,n,x)
    implicit none
    real p1(0:n-1),x,eps,pp,dp,x0,x1
    integer n,it,j,m0
    integer, parameter :: Itmax =100 
    eps = 1e-7 
    x0=0
    m0=0
    do j = 0,n-1 
        if(p1(j)/= 0) m0=m0+1 
    enddo   
    do j=0,n-1 
        if(p1(j)/= 0) then 
            x0=max(x0,abs(m0*p1(j))**(1.0/(n-j)))    
        endif 
    enddo

    do it=1,itmax 
        pp=1
        dp=0
        do j = n-1,0,-1
            dp=dp*x0+pp
            pp=pp*x0+p1(j)
        enddo 
        if(pp==0)exit 
        x1=x0 - pp/dp
        if(abs(x1-x0)<eps) exit 
        x0=x1 
    enddo 
    x=x1 
end subroutine proot1_newton