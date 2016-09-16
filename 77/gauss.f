c     main program
      implicit double precision (a-h,o-z)
      integer ierr
      parameter (max=8)
      dimension x(max),a(max,max),b(max)
c     input data
      write(6,*) 'gauss 消去法による Ax=bの計算(A:n*n x,b:n*1)'
      write(6,*) 'n='
      read(5,*) n
      call s_inmg(a,n,n,'a')
      call s_inmg(b,n,1,'b')

      call s_gauss(x,a,b,n,ierr)
      if(ierr.eq.1) then
        write(6,*) '不定または不能'
        stop
      end if
      call s_outm(x,n,1,'x')
      stop
      end

      subroutine s_gauss(x,a,b,n,ierr)
      implicit double precision (a-h,o-z)
      dimension x(n),a(n,n),b(n)
      parameter (e_eps=1.0e-14)
c     前進消去
      do 30 k=1,n-1
        call s_pivo(a,lc,n,k,ierr)
        if(ierr.eq.1) then
          return
        else if (le.ne.k) then
          tmp=b(k)
          b(k)=b(lc)
          b(lc)=tmp
        end if
        do 20 i=k+1,n
          a(i,j)=a(i,j)-p*a(k,j)
   10     continue
          b(i)=b(i)-p*b(k)
   20   continue
   30 continue
c   　 後退代入
      do 50 k=n,1,-1
        if(abs(a(k,k)).le.e_eps) then
          ierr=1
          return
        end if
          s=0.0
          do 40 j=k+1,n
            s=s+a(k,j)*x(j)
   40     continue
          x(k)=(b(k)-s)/a(k,k)
   50   continue
      ierr=0
      return 
      end

      subroutine s_pivo(a,m,n,k,ierr)
      implicit double precision (a-h,o-z)
      dimension a(n,n)
      parameter (e_eps=1.0e-14)

      m=k
      d=abs(a(k,k))

      do 10 i=k+1,n
        if(abs(a(i,k)).gt.d) then
          m=i
          d=abs(a(i,k))
        end if
   10 continue

      if (abs(d).lt.e_eps) then 
        ierr=1
        return
      else if(m.eq.k) then
        ierr=0
        return
      end if

      do 20 i=k,n
        tmp = a(k,i)
        a(k,i)=a(m,i)
        a(m,i)=tmp
   20 continue
      ierr=0
      return
      end
c     行列のキー入力サブルーチン
      subroutine s_inmg(a,n,m,name)
      implicit double precision (a-h,o-z)
      character name*1
      dimension a(n,m)
      write(6,*)
      do 20 i=1,n
        if(m.eq.1) then
            write(6,30) name ,i
   30       format(1h+,a,'(',i2,')の入力')
        else
            write(6,10) name,i,name,i,m
   10       format(1h+,a,'(',i2,', 1)~',a,'('i2,',',i2,')の入力')
        end if
            write(6,*)
            read(5,*)(a(i,j),j=1,m)
   20   continue
      return
      end

c     行列の画面表示サブルーチン
      subroutine s_outm(a,n,m,name)
      implicit double precision (a-h,o-z)
      character name*1
      dimension a(n,m)
      write(6,10) name
   10 format(1h,a,'=')
      do 30 i=1,n
        write(6,20)(a(i,j),j=1,m)
   20   format(8f15.6)
   30 continue
      return 
      end


