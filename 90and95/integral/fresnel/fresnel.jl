using PyCall

function series_expansion(x)
    N=100 
    cx=x
    sx=(pi/2)*(1/3)*x^3
    for k in 1:N
        cx*=-(4k+1)/((2k+1)*(4k+5))*(pi/2)^2*x^4
        sx*=-(4k+3)/(2k+3)*(pi/2)*x^4
    end 
    return cx,sx
end

function continued_fraction(x)
    N=100 
    cx=x
    sx=(pi/2)*(1/3)*x^3
    for k in 1:N
        cx*=-(4k+1)/((2k+1)*(4k+5))*(pi/2)^2*x^4
        sx*=-(4k+3)/(2k+3)*(pi/2)*x^4
    end 
    return cx,sx
end

function fresnel(x)
    cx,sx=0.0,0.0

    if x==0 
        return 0,0
    end 
    
    xa=abs(x)
    if xa<2
        cx,sx=series_expansion(xa)
    elseif xa>=2
        cx,sx=continued_fraction(xa)
    end 

    if x<0
        cx,sx=-cx,-sx 
    end 
    return cx,sx 
end 

function main_()
    n=1000
    h=0.001
    for i in -n:n 
        x=i*h 
        cx,sx=fresnel(x)
        print(x,cx,sx)
    end 
end 

function main()
    xs=linspace(-pi,pi,100)
    ys=sin.(xs)
    @pyimport matplotlib.pyplot as plt 
    plt.plot(xs,ys)
    plt.show()
end 
main()
