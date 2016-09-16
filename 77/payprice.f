c---+----1----+----2----+----3----+----4
      program main
      double precision price_pay,price_org,discout,tax
      discount=0.1d0
      tax=0.05d0
      price_org=980.0d0
      price_pay=price_org*(1.0d0-discount)*(1.0d0+tax)
      write(6,*) 'You should pay',price_pay,'Yen'
      stop
      end