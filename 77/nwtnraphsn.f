      implicit double precision (a-h,o-z)
      external userf

      write(6,*) 'initial value'
      read(5,*) x0
      write(6,*) 'critelia epsilon'
      read(5,*) e 
      write(6,*) 'max iteration'
      read(5,*) nmax
c     Newton-Raphson method
      call s_newt(n,x0,e,nmax,userf)
      write(6,10) 'num iteration N:=',n,'solution x(n)=',x0
   10 format(1h,a11,i4,1h,a8,f10.7)
      stop
      end

      subroutine s_newt(n,x0,e,nmax,func)
      implicit double precision (a-h,o-z)
      n=0
      xn=x0
  100 if(n.lt.nmax) then 
        call func(fun,dfun,xn)
        x0=xn-fun/dfun
        n=n+1
        if(abs(xn-x0).gt.e) then
          xn=x0
          go to 100
        end if
      end if
      return
      end

      subroutine userf(fun,dfun,x)
      implicit double precision (a-h,o-z)
      x2 = x*x
      fun=x2*x-1.0
      dfun = 3.0*x2
      return 
      end