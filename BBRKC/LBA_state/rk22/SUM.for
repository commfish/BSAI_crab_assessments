       program sum
c This program summarizes the results from the LBA for BBay RK crabs.
       real ttnm(14,60)/840*0.0/,tto(14,60)/840*0.0/,
     +  t95(60)/60*0.0/,t11(60)/60*0.0/,t20(60)/60*0.0/,t35(60)/60*0.0/
       real ttnf(11,60)/660*0.0/,t90(60)/60*0.0/,rm(60),rf(60),
     +  ep(60)/60*0.0/,epb(60)/60*0.0/,tm(60)/60*0.0/,r(60),
     +  wm(14)/0.69624,0.81544,0.94789,1.09433,1.25553,1.43224,1.62525,
     +   1.83531,2.06321,2.30974,2.57567,2.86180,3.16894,3.49787/,
     +  wf(11)/0.56382,0.63418,0.70913,0.78874,0.87305,0.96211,
     +   1.05597,1.15469,1.25829,1.36684,1.48036/,                                                       /,
     +  ra(9)/1.0,1.2,1.4,1.6,1.8,2.1,2.4,2.7,3.0/
       character*80 infim,infif,outfile,inparm,inparf,tx1,tx2,tx3
       write(*,'(a\)') ' Enter male pop result file name: '
       read(*,'(a)') infim
       write(*,'(a\)') ' Enter female pop result file name: '
       read(*,'(a)') infif
       write(*,'(a\)') ' Enter male para result file name: '
       read(*,'(a)') inparm
       write(*,'(a\)') ' Enter female para result file name: '
       read(*,'(a)') inparf
       write(*,'(a\)') ' Enter output file name: '
       read(*,'(a)') outfile
       open(1,file=infim,status='old')
       open(2,file=infif,status='old')
       open(3,file=inparm,status='old')
       open(4,file=inparf,status='old')
       open(7,file=outfile,status='new')
       write(*,'(a\)') ' Enter number of Years: '
       read(*,*) n
       do 10 i = 1, 14
10     read(1,*) (ttnm(i,j),j=1,n)
       do 15 i = 1, 14
15     read(1,*) (tto(i,j),j=1,n)
       do 40 j = 1, n
         do 20 i = 1, 3
20       t95(j) = t95(j)+ttnm(i,j)+tto(i,j)
         do 25 i = 4, 8
25       t11(j) = t11(j)+ttnm(i,j)+tto(i,j)
         do 30 i = 6, 14
30       t20(j) = t20(j)+ttnm(i,j)+tto(i,j)
         do 35 i = 9, 14
35       t35(j) = t35(j)+ttnm(i,j)+tto(i,j)
40     continue
       do 45 i = 1, 11
45     read(2,*) (ttnf(i,j),j=1,n)
       do 55 j = 1, n
         do 50 i = 1, 11
50       t90(j) = t90(j)+ttnf(i,j)
55     continue
c male reproductive potential
       do 65 j = 1, n
         do 60 i = 6, 14
60       tm(j) = tm(j) + (ttnm(i,j)+tto(i,j))*ra(i-5)
         r(j) = tm(j)/t90(j)
         if (r(j).gt.1.0) r(j) = 1.0
65     continue
c effective spawning biomass
       do 75 j = 1, n
         do 70 i = 1, 11
70       ep(j) = ep(j) + ttnf(i,j)*wf(i)*r(j)
         epb(j) = ep(j)*2.0/0.90718474
75     continue
       read(3,100) tx1,tx2,tx3
       read(3,*) (rm(j),j=1,n)
       read(4,110) tx1,tx2
       read(4,*) (rf(j),j=1,n)
100    format(1x,a80/a80/a80)
110    format(1x,a80/a80)
       write(7,120)
       do 80 j = 1, n
80     write(7,130) 1971+j,rm(j)/1000.0,t95(j)/1000.0,t11(j)/1000.0,
     +  t20(j)/1000.0,t35(j)/1000.0,rf(j)/1000.0,t90(j)/1000.0,
     +  epb(j)/1000.0,ep(j)/1000.0,tm(j)/1000.0,r(j)
120    format(1x,'                     Males          ',
     +  '              Females'/1x,
     +  '     --------------------------------------  ',
     +  ' ---------------'/1x,
     +  'Year Recruits  Small  Pre-recr Mature  Legal',
     +  '  Recruits Mature Eff.Spa.Biomass'/1x,
     +  '             95-109mm 110-134mm >119mm >134mm',
     +  '          >89mm  mill.lbs 1000 mt M.rep.p r'/)
130    format(1x,i4,1x,10(f7.3,1x),f4.2)
       end
