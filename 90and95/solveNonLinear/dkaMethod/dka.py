# Reference: 田口俊弘 著 Fortran ハンドブック技術評論社１４４ｐ
# Converted from Fortran to Python

from functools import partial
import numpy as np
from copy import copy


def calc_poly(x, cs):
    px = 0
    for i, c in enumerate(cs):
        px += c*x**i
    return px


def newton(coefficients, eps=1e-7):
    iteration = 100000
    degree = len(coefficients)-1
    
    #define x0 as init value for newton method
    x0 = 0
    m0 = sum(coefficients != 0)
    for j in range(degree):
        if coefficients[j] != 0:
            x0 = max(x0, abs(m0*coefficients[j]))**(1.0/(degree-j))

    for i in range(iteration):
        pp = 1
        dp = 0
        for j in reversed(range(degree)):
            dp = dp*x0+pp
            pp = pp*x0+coefficients[j]
        if(pp == 0):
            return x1, i
        x1 = x0-pp/dp
        if(abs(x1-x0) < eps):
            return x1, i
        x0 = x1
    return x1, iteration


def dkaroot(coefficients, eps=1e-7):
    iteration = 100000
    degree = len(coefficients)-1
    # convert poly into monic
    coefficients = coefficients/coefficients[-1]
    beta = coefficients[-2]/degree

    #set polynomial r_poly to define r
    r_poly = copy(coefficients)
    for i in range(degree-1):
        for j in reversed(range(i, degree)):
            r_poly[j] = r_poly[j+1]*beta+r_poly[j]
        r_poly[i] = -abs(r_poly[i])
    r_poly[degree-1] = 0

    #define initial value of Aberth 
    r, _ = newton(r_poly)
    z = np.zeros(degree).astype(np.complex)
    for k in range(degree):
        angle = (2*np.pi*k+3/2)/degree
        z[k] = -beta+r*np.exp(angle*1j)

    #update z using Durand Kerner method 
    poly = partial(calc_poly, cs=coefficients)
    for i in range(iteration):
        znext = np.zeros(degree).astype(complex)
        for k in range(len(z)):
            pr = [z[k]-z[j] for j in range(degree) if j != k]
            zp = np.prod(pr)
            znext[k] = z[k]-poly(z[k])/zp
            #print("z znext",z[k], znext[k])
        if np.sum(np.abs(znext-z)) < eps:
            return z, i
        z = znext
    return None, -iteration


def main():
    eps = 1e-7
    # p(z)=580+1398z+232z^2,593z^3-708z^4+180z^5
    poly = np.array([580, 1398, 232, 593, -708, 180])
    p = partial(calc_poly, cs=poly)

    solutions, iteration = dkaroot(poly, eps)
    if solutions is not None:
        for s in solutions:
            print(s, p(s), iteration)
    else:
        print("failed to solve")

if __name__ == '__main__':
    main()
