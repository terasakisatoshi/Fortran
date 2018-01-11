program quadratic
    implicit none 
    real a,b,c,D,x1,x2
    a=3
    b=5
    c=1
    D=b*b-4*a*c
    x1=(-b+sqrt(D))/(2*a)
    x2=(-b-sqrt(D))/(2*a)
    print *,'X1 X2 = ',x1,x2
end