program Fresnel_test
    implicit none
    real x,cx,sx,h
    integer i,n 
    n=1000
    h=0.01
    do i =-n,n 
        x=h*i 
        call fresnel(x,cx,sx)
        print *,x,",",cx,",",sx
    enddo
end program Fresnel_test

subroutine fresnel(x,cx,sx)
    implicit none
    real, parameter :: pi=3.14159265358979,pi2=(pi/2)**2
    real, parameter :: eps = 1e-15 
    real x,cx,sx,xa,x2,x4,f,g,xp 
    integer k,kmax,k2,k4 
    complex z,zcs,zcs1,zc0,zd0,zc1,zd1,zc2,zd2,zbi 
    if(x==0) then 
        cx=0; sx=0
        return 
    endif 
    xa=abs(x)
    if(xa<2) then 
        kmax=100 
        x2=xa*xa 
        f=xa 
        g=pi*xa*x2/2
        x4=x2*x2
        cx=f
        sx=g/3
        do k=1,kmax
            k2=k+k
            k4=k2+k2
            f=-f*pi2*x4/(k2*(k2-1))
            cx=cx+f/(k4+1)
            g=-g*pi2*x4/((k2+1)*k2) 
            sx=sx+g/(k4+3)
            if(abs(f)<eps .and. abs(g)<eps) exit 
        enddo
    else
        x2=xa*xa
        xp=pi*x2
        z=cmplx(0.0,1/xp)
        zcs=1
        zc0=1; zd0=0
        zc1=1; zd1=1
        zcs1=zcs 
        kmax=100
        do k=1,kmax 
            zbi=k*z
            zc2=zc1+zbi*zc0
            zd2=zd1+zbi*zd0
            zcs=zc2/zd2
            if(abs(zcs-zcs1)<eps) exit 
            zc0=zc1;zd0=zd1
            zc1=zc2;zd1=zd2
            zcs1=zcs 
        enddo 
        k2=xa
        k2=k2/2
        f=xa-2*k2 
        g=f*k2 
        g=g-int(g)
        f=f*f+4*g 
        zcs=zd2*cmplx(-sin(pi/2*f),cos(pi/2*f))/(zc2*pi*xa)
        cx=0.5-real(zcs)
        sx=0.5-imag(zcs)
    endif
    if(x<0) then
        cx=-cx; sx=-sx
    endif
end subroutine fresnel