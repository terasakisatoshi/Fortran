c----+----1----+----2
      program arithmetic

      integer number1,number2
      integer sum,diff,mul,div,pow

      write(6,*) 'Input the number 1'
      read(5,*) number1
      write(6,*) 'Input the number 2'
      read(5,*) number2

      sum=number1+number2
      diff=number1-number2
      mul=number1*number2
      div=number1/number2
      pow=number1**number2

      write(6,*) number1,' + ',number2,' = ',sum
      write(6,*) number1,' - ',number2,' = ',diff
      write(6,*) number1,' * ',number2,' = ',mul
      write(6,*) number1,' / ',number2,' = ',div
      write(6,*) number1,' ** ',number2,' = ',pow
      stop
      end
