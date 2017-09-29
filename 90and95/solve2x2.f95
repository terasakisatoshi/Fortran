program solve2by2eq
    implicit none
    real a11,a12,a21,a22,b1,b2,det,x1,x2
    a11=7;a12=4 ;b1=2
    a21=1;a22=-8;b2=-1
    det = a11*a22-a21*a12
    x1=(b1*a22-b2*a12)/det
    x2=(a11*b2-a21*b1)/det
    print*,x1,x2
end program solve2by2eq