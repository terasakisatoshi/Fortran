C << MAIN PROGRAM>>
      IMPLICIT double precision (A-H,O-Z)
C *** Step1 input data ***
      write(6,*) 'input init value x1 and x2='
      read(5,*) x1, x2
      write(6,*) 'criteria of convergence epsilon='
      read(5,*) e
C *** Step 2 calculate bisection method ***
 100  x=(x1+x2)/2.0
      if(f(x1)*f(x).lt.0) then
          x2=x
      else
          x1=x
      end if

      if(abs(x1-x2).ge.e) then
          goto 100
      end if
C *** Step 3 output result
      write(6,200) 'answer = ',x
 200  format(1h,a6,f10.6)
      stop
      end

c << define nonlinear equation >>
      double precision function f(x)
c      x : value
      implicit double precision (A-H,O-Z)
      f=x**3-1
      return 
      end 