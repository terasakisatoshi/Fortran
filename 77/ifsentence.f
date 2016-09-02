      program main
      double precision x,y,z, y_int
      write(6,*) 'input x:'
      read(5,*) x
      write(6,*) 'input y:'
      read(5,*) y

      y_int=int(y)

      if (x.gt.0) then
        z=x**y
        write(6,*) '(',x,')^(',y,')=',z
      else if (x.eq.0) then
        z=0.0d0
        write(6,*) '(',x,')^(',y,')=',z
      else if (y.eq.y_int) then
        z=x**y
        write(6,*) '(',x,')^(',y,')=',z
      else 
        write(6,*) 'Range error in x and/or y'
      end if
      stop
      end