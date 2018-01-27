!solve eq x=cos(x)
program xcosx
    implicit none
    real x1,x2,eps
    integer it,itmax 
    x1=0
    eps=1e-7
    itmax=10000000
    do it =1,itmax
        x2=cos(x1)
        if(abs(x2-x1)<eps) exit
        x1=x2
    enddo

    if (it > itmax) then 
        print *,'No Convergence!'
    else 
        print *,"X=",x2,cos(x2)-x2,it
    endif

end program xcosx