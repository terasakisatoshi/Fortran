program solve_quadratic
    implicit none
    real a,b,c,x1,x2
    integer ib,num_roots
    a=3
    c=1
    do ib =0,10
        b=ib
        call quadratic(a,b,c,x1,x2,num_roots)
        if (num_roots==0) then 
            print *,"No roots"
        else if (num_roots==1) then 
            print *,"Single Root: x=",x1
        else if (num_roots==2) then
            print *,"Two Real Roots:X1,X2=",x1,x2
        else 
            print *,'Two Imaginary Roots:X1,X2=',x1,'+-i(',x2,')'
        endif
    enddo

end program solve_quadratic

subroutine quadratic(a,b,c,x1,x2,num_roots)
    implicit none
    real a,b,c,x1,x2,D,d1,s
    integer num_roots

    if(a==0) then 
        if(b==0) then 
            num_roots=0
        else
            x1=-c/b
            num_roots=1
        endif
        return 
    endif

    D=b*b-4*a*c
    num_roots=2
    if (D==0) then
        x1=-b/(2*a)
        x2=x1
    else if (D>0) then
        d1=sqrt(D)
        if(b>=0) d1=-d1
        s=(-b+d1)/2
        x1=s/a
        x2=c/s
    else 
        num_roots=-2
        x1=-b/(2*a)
        x2=sqrt(-D)/(2*a)
    endif

end subroutine quadratic