      program main
      double precision x1,y1,z1, x2,y2,z2,length
      write(6,*) 'input 1st point:'
      read(5,*) x1,y1,z1
      write(6,*) 'input 2nd point:'
      read(5,*) x2,y2,z2

      length=sqrt((x2-x1)**2+(y2-y1)**2+(z2-z1)**2)

      write(6,*) 'length between Point 1 and 2 = ',length
      stop 
      end
