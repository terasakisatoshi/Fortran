program main_bisection
    implicit none
    real x1,x2,x,eps,xcosx 
    external xcosx
    integer ind 
    x1=0
    x2=3.14
    eps=1e-7
    call bisection(xcosx,x1,x2,x,eps,ind)
    if (ind>=0) then 
        print *,'X= ',x,xcosx(x),ind
    else if (ind==-1) then 
        print *,'f(x1)*f(x2) must be less than 0'
    else 
        print *,'No Convergence'
    endif
end program main_bisection

function xcosx(x)
    real xcosx,x
    xcosx = x-cos(x)
end function xcosx

subroutine bisection(func,x10,x20,x,eps,ind)
    implicit none
    real func,x10,x20,x,eps,x1,x2,y1,y2,xm,ym
    external func 
    integer ind,it 
    integer, parameter :: itmax=100000000

    x1=x10
    x2=x20 
    y1=func(x1)
    y2=func(x2)
    ind=0
    if(y1==0) then 
        x=x1
        return 
    else if (y2==0) then 
        x=x2
        return 
    else if(y1*y2>0) then 
        ind=-1
        return 
    endif 

    do it = 1, itmax 
        xm=(x1+x2)/2
        ym=func(xm)
        if(ym==0) then 
            x=xm
            exit 
        endif
        if(ym*y1<0) then
            x2=xm 
            y2=ym 
        else 
            x1=xm
            y1=ym
        endif
        if(abs(x2-x1)<eps) then 
            x=xm
            exit 
        endif 
    enddo

    if(it>itmax) then 
        ind=-itmax 
    else 
        ind=it 
    endif 
    
end subroutine bisection