       program ghist
c This program formats the results from the bootstrapping into frequency.
       real sr(2000),r(2000,60),ri(60,60),tm(60),ti(60),rm(60),x1(60,6),
     +      x2(60,6)
       integer rh(60,60)/3600*0/
       character*80 infile(6)/'marb.out','ma11b.out','ma20b.out',
     +              'ma35b.out','ferb.out','fe90b.out'/,outfile
c      write(*,'(a\)') ' Enter input file name: '
c      read(*,'(a)') infile
       write(*,'(a\)') ' Enter output file name: '
       read(*,'(a)') outfile
       open(2,file=outfile,status='new')
       write(*,'(a\)') ' Enter number of columns: '
       read(*,*) nn
       do kk = 1, 6
         open(1,file=infile(kk),status='old')
         i = 1
         if (kk.eq.1 .or. kk.eq.5) then
            n = nn - 1
         else
            n = nn
         endif
1        read(1,*,END=99) (r(i,j),j=1,n)
         i = i + 1
         goto 1
99       m = i - 1
         close(1)
         do 30 j = 1, n
           do 10 i = 1, m
10         sr(i) = r(i,j)
           call sort(m,sr)
           do 20 i = 1, m
20         r(i,j) = sr(i)
30       continue
         n1 = 2
         n2 = m - 1
         n3 = 0.025*m
         n4 = 0.05*m
         nm = int(m/2.0)+1
         n5 = 0.95*m
         n6 = 0.975*m
         do 40 j = 1, n
           rm(j) = r(nm,j)
           if ((nm-float(m)/2.0).gt.0.6) rm(j) = (rm(j)+r(nm-1,j))/2.0
40       continue
         do 70 j = 1, n
           tm(j) = r(n2,j) - r(n1,j)
           ti(j) = tm(j)/49.0
           do 50 k = 1, 50
50         ri(k,j) = r(n1,j) + (k-1)*ti(j)
           do 60 i = 1, m
             if (r(i,j).le.r(n1,j)+0.5*ti(j)) then
               rh(1,j) = rh(1,j) + 1
             else if (r(i,j).gt.r(n2,j)-0.5*ti(j)) then
               rh(50,j) = rh(50,j) + 1
             else
               ii = int((r(i,j)-r(n1,j)-0.5*ti(j))/ti(j)) + 1
               rh(ii,j) = rh(ii,j) + 1
             endif
60         continue
70       continue
         do 80 j = 1, n
           x1(j,kk) = r(n3,j)
           x2(j,kk) = r(n6,j)
80       continue
       end do
c       do 80 k = 1, 50
c80     write(2,100) (ri(k,j), j=1,n)
       do j = 1, n
        write(2,100) (x1(j,kk)/1000.0, kk=1,6),(x2(j,kk)/1000.0, kk=1,6)
       end do
c       do 90 k = 1, 50
c90     write(2,110) (rh(k,j), j=1,n)
c       write(2,105) (ti(j),j=1,n)
c       write(2,100) (r(1,j),j=1,n)
c       write(2,100) (r(n3,j),j=1,n)
c       write(2,100) (r(n4,j),j=1,n)
c       write(2,100) (rm(j),j=1,n)
c       write(2,100) (r(n5,j),j=1,n)
c       write(2,100) (r(n6,j),j=1,n)
c       write(2,100) (r(m,j),j=1,n)
100    format(1x,12(f9.3,1x))
c100    format(1x,35(f9.2,1x))
c105    format(1x,35(f9.3,1x))
c110    format(1x,35(i9,1x))
       end
c-----------------------------------------------------------
       subroutine sort(n,ra)
       dimension ra(n)
       L = n/2 + 1
       ir = n
10     continue
         if (L .gt. 1) then
           L = L - 1
           rra = ra(L)
         else
           rra = ra(ir)
           ra(ir) = ra(1)
           ir = ir - 1
           if (ir .eq. 1) then
             ra(1) = rra
             return
           endif
         endif
         i = L
         j = L + L
20       if (j .le. ir) then
           if (j.lt.ir) then
             if (ra(j).lt.ra(j+1)) j = j + 1
           endif
           if (rra.lt.ra(j)) then
             ra(i) = ra(j)
             i = j
             j = j + j
           else
             j = ir + 1
           endif
           goto 20
         endif
         ra(i) = rra
       goto 10
       return
       end
c---------------------------------------------------------------
