      program rkf
*
*--   Version 5.0  Females           @ J. Zheng      June 2020
*
*  This version adds flexibity for missing wurvey years
*
*----------------------------------------------------------------------------
      use dfimsl
      external popmodel
      include 'rkf.cmn'
      integer   maxparms,maxobs
      integer   ldfjac,iwklen,wklen,ic
      parameter (maxparms=2*maxyear+5)
      parameter (maxobs=maxyear*maxlen)
      parameter (ldfjac=maxobs,iwklen=2*maxparms)
      parameter (wklen=11*maxparms+3*maxobs-1)
*
      integer iparam(6),iwk(iwklen),nobs,nparms,ibtype /0/,ncalls
      real   theta(maxparms),gtheta(maxparms),rssq
      real   rparam(7),wk(wklen)
      real   xlb(maxparms),xub(maxparms)
      real   xscale(maxparms) /maxparms*1.0  /
      real   fscale(maxobs)   /  maxobs*1.0  /
      real   fvec(maxobs), fjac(ldfjac,maxparms),popmodel
      common /fitstats/ncalls,rssq
*
*--Read input parameters
      call readparm(ihr,imin,nobs)
      ib = 0
      ibm = 1
*--Enter control commands
    1 call control(ic,nparms,nobs,fvec)
      if (ic.eq.0) then
        goto 9999
      else if (ic.eq.1) then
        goto 999
      else if (ic.eq.2) then
        goto 99
      endif
*--Create a vectorized parameter list, from the separate parameter vectors
   11 call parmsvec(theta,xlb,xub,nparms)
*--If parameters are not estimated, compute their function values
      if (id(1).eq.0) call gammal
      if ((id(2)+id(3)).lt.1) call gammar
      if ((id(4)+id(5)).lt.1) call slength
*--Project population abundance in year ny.
      if (ic.eq.4) then
        call project(nparms,theta)
        goto 1
      endif
*--starting bootstrapping
      if (ic.eq.5.and.ib.eq.0) then
        call boot0(nobs,nparms,theta,fvec)
        call boot
        ib = 1
        ib0 = 1
      endif
      do 20 i = 1, nparms
   20 gtheta(i) = theta(i)
*--Echo input parameters
      if (ib.eq.0) call lstheta(nparms,theta)
*--Set and list the convergence parameters
      call setconvg(iparam,rparam,ib)
*--Estimate model parameters using nonlinear least squares
      ncalls = 0
      call ERSET(4,1,0)
      call  B2LSF(popmodel,nobs,nparms,gtheta,ibtype,xlb,xub,xscale,
     +            fscale,iparam,rparam,theta,fvec,fjac,ldfjac,wk,iwk)
*--List output
      if (ib.eq.0) then
         call lstheta(nparms,theta)
      else
         write(*,100) ib,ncalls, rssq
  100 format(' Replicate:',i4,2x,i6,' calls to popmodel, SSQ=',e14.7/)
      endif
      if ((ib.ge.1).and.(ib0.eq.1)) then
        ib0 = 2
        goto 11
      endif
      if (ib.ge.1) goto 99
      goto 1
*--Update bootstrapping and output
   99 if (ib.gt.0.and.ib.le.ibm) then
        call update
        call boot
        if (ib.eq.ibm) then
          call bootout
          stop
        else
          ib = ib + 1
          ib0 = 1
          goto 11
        endif
      endif
*--Normal output
      call writout(fvec,ihr,imin,iparam,rparam,ncalls,nparms,nobs,
     +             maxparms,theta,wk,wklen,xscale,fscale)
      goto 1
*--Sensitivity analyses
  999 CALL PARMRESP(XLB,XUB,NPARMS,THETA,FVEC,NOBS)
 9999 stop
      end
*-----------------------------------------------------------------
      subroutine control(ic,nparms,nobs,fvec)
*
*--Enter control commands
*
      integer ic,ic0,nparms,nobs
      real fvec(nobs)
      include 'rkf.cmn'
*
   11 write(*,100)
  100 format(/' Continue(6),Boot(5),Project(4),View Res(3),Output(2),',
     +        'Sen.Anal(1) or Quit(0): '\)
      read(*,*) ic
      if (ic.eq.0 .or. ic.eq.1 .or. ic.eq.2) then
        return
      else if (ic.gt.6 .or. ic.lt.0) then
        goto 11
      else if (ic.eq.3) then
        do a = 1, nl
           write(*,105) (fvec((a-1)*nyd+y),y=1,nyd)
        end do
 105  format(60(f4.1,1x))
        goto 11
      else if (ic.eq.4) then
        goto 71
      endif
   13 write(*,110) betal
  110 format(' BETAL= ',f7.4,', Enter 0 for fixing, 1 for estimating:'\)
      read(*,*) id(1)
      if (id(1).lt.0 .or. id(1).gt.1) goto 13
   15 write(*,120) mr
  120 format(' Mean R= ',f7.2,',Enter 0 for fixing, 1 for estimating:'\)
      read(*,*) id(2)
      if (id(2).lt.0 .or. id(2).gt.1) goto 15
   17 write(*,130) betar
  130 format(' BETAR= ',f7.4,', Enter 0 for fixing, 1 for estimating:'\)
      read(*,*) id(3)
      if (id(3).lt.0 .or. id(3).gt.1) goto 17
c  19 write(*,140) alphas
c 140 format(' ALPHAS= ',f7.1,',Enter 0 for fixing, 1 for estimating:'\)
c     read(*,*) id(4)
c     if (id(4).lt.0 .or. id(4).gt.1) goto 19
c  21 write(*,150) betas
c 150 format(' BETAS= ',f7.4,', Enter 0 for fixing, 1 for estimating:'\)
c     read(*,*) id(5)
c     if (id(5).lt.0 .or. id(5).gt.1) goto 21
      id(4) = 0
      id(5) = 0
   31 write(*,160)
  160 format(' RECRUIT by YEAR,Enter 0 for fixing, 1 for estimating:'\)
      read(*,*) id(6)
      if (id(6).lt.0 .or. id(6).gt.1) then
         goto 31
      else if (id(6).eq.1) then
   41   write(*,'(a\)') ' Modify recruitment estimate (1=yes, 0=no)? '
        read(*,*) ic0
        if (ic0.eq.0) then
          goto 61
        else if (ic0.lt.0 .or. ic0.gt.1) then
          goto 41
        endif
   51   write(*,'(a\)') ' Enter ID # (1=1st yr, etc..., -1 for quit): '
        read(*,*) ic0
        if (ic0.lt.0) then
          goto 61
        else if (ic0.lt.2 .or.ic0.gt.ny) then
          goto 51
        endif
        write(*,165) ic0,rr(ic0)
  165 format(1x,'RR(',i2,')= ',f10.2,', Enter new guess of recruit: '\)
        read(*,*) rr(ic0)
        goto 51
      endif
   61 write(*,170)
  170 format(' ABUND-1ST YEAR, Enter 0 for fixing, 1 for estimating:'\)
      read(*,*) id(7)
      if (id(7).lt.0 .or. id(7).gt.1) goto 61
   63 write(*,175)
  175 format(' Re-est. bycatch in 72-89, Enter 1 for Yes & 0 for No:'\)
      read(*,*) iby
      if (iby.lt.0 .or. iby.gt.1) goto 63
   71 write(*,180)
  180 format(1x,'Select natural mortalities (NM) for estimation: '/,
     +       5x,'0 --- Do not estimate NM'/,
     +       5x,'1 --- Estimate NM by year'/,
     +       5x,'2 --- Estimate NM by group of years'/,
     +       5x,'3 --- Estimate NM by group of lengths'/,
     +       5x,'4 --- Estimate NM by a length function'/,
     +       5x,'5 --- Combination of 2 and 3'/,
     +       5x,'6 --- Combination of 2 and 4 :=>>>>:'\)
      read(*,*) inm
      if (inm.lt.0 .or.inm .gt. 6) goto 71
      nparms = ny+id(1)+id(2)+id(3)+id(4)+id(5)
      if (inm.eq.1) then
        nparms = nparms + ny
      else if (inm.eq.2) then
        nparms = nparms + iy
      else if (inm.eq.3) then
        nparms= nparms + il
      else if (inm.eq.4) then
        nparms = nparms + 4
      else if (inm.eq.5) then
        nparms = nparms + iy + il
      else if (inm.eq.6) then
        nparms = nparms + iy + 3
      endif
*
*--Initialize............
      do 60 a = 1, nl
        v(a) = 1.0
        r(a) = 0.0
   60 continue
      do 70 a = 1, nl
      do 70 i = 1, nl+10
   70 pg(a,i) = 0.0
      return
      end
*-----------------------------------------------------------------
      subroutine popmodel(nobs,nparms,theta,fvec)
*
*--Upon call from IMSL routine B2LSF, compute the residuals of the
*--   observed and estimated abundances from the pop. models.
*
      integer nparms,nobs
      real   theta(nparms),fvec(nobs),rssq,t1
      common /fitstats/ ncalls, rssq
      external vecparms,gammal,gammar,lstheta,slength
      include 'rkf.cmn'
*
* Unvectorize the parameters into catch/age parameter form
      call vecparms(theta,nparms)
* Compute length-specific vulnerabilities from logistic function
      if ((id(4)+id(5)).gt.0) call slength
* Compute recruitment proportion by length from gamma function
      if ((id(2)+id(3)).gt.0) call gammar
* Compute probability of growth increment from gamma function
      if (id(1).eq.1) call gammal
c Compute bycatch
      call bycatch
* Update the population sizes for the current parameter list:
* First length group
      do 10 y = 2, ny
   10   pn(1,y) = rr(y)*r(1)
* First year abundance
      do 15 a = 1, nl
   15 pn(a,1) = pni*pl(a)
* Forwardly update population
      do 30 a = 1, nl-1
      do 30 y = 1, ny-1
        tp = 0.0
        do 20 j = 1, a+1
          tm = sl(j)*sy(y)
          tp = tp + pg(j,a+1)*(pn(j,y)*exp(-tm) -
     +              ebc(j,y)*exp((T(y)-1)*tm))
   20   continue
* Force the population to be non-negative.
        if (tp.lt.0.0) tp = 0.0
        pn(a+1,y+1) = tp + rr(y+1)*r(a+1)
   30 continue
*
      ii = 0
* Compute the residuals
      do 40 a = 1, nl
      do 40 y = 1, nyd
        ii = ii + 1
        fvec(ii) = (log(pn(a,iys(y))*v(a)+sc)
     +             - opnl(a,y))/(1.4142*wei(iys(y)))
   40 continue
      ncalls = ncalls + 1
* Output the results on screen every 50 iterations
      if (mod(ncalls,50) .lt. 0.000001) then
        rssq = 0.0
        do 80 i = 1, nobs
   80   rssq = rssq + fvec(i)**2
        if (ib.eq.0) call lstheta(nparms,theta)
      endif
      return
      end
*-----------------------------------------------------------------
      subroutine readparm(ihr,imin,nobs)
*
* Read and echo input parameters from input files
*
      real t1, t2,qq
      include 'rkf.cmn'
*
      call gettim(ihr,imin,isec,i100)
      call getdat(iyr,imon,iday)
      open(unit=1,file='paramf.dat',status='old')
      open(unit=3,file='surveyf.dat',status='old')
      open(4,file='nmf.dat',status='old')
*
* Read the dimensioning and parameter input file
      read(1,*) ny,nl,ns,iyear1,ilen1,lenint,nr,mr,sc
      do 10 a = 1, nl
   10 read(1,*) g(a),ngl(a),ngu(a)
      read(1,*) betal,betalx(1),betalx(2)
      read(1,*) alphar,alrx(1),alrx(2)
      read(1,*) betar,betarx(1),betarx(2)
      read(1,*) alphas,alsx(1),alsx(2)
      read(1,*) betas,betasx(1),betasx(2)
      do 16 y = 2, ny
   16 read(1,*) rr(y),rrx(1,y),rrx(2,y)
      read(1,*) pni,pnix(1),pnix(2)
      close(1)
c      write(*,*) ' Enter catchability for 1994-1997 (<=1): '
c      read(*,*) qq
       qq = 1.0
      write(*,100) imon,iday,iyr-1900,ihr,imin,nl,ny,iyear1,ilen1
  100 format(//2x,
     +  'CRABLEB: CRAB LEngth-Based pop. analysis Program. Run on:',
     +                   i2,'/',i2,'/',i2,' at ',i2,':',i2,'.',//,
     +  22X,'@ J. Zheng  February 1993.'//,
     +   8x,'The analysis will be stratified for',i3,' lengths,',
     +                                i3,' years.'//,
     +   8x,'The first year is',i5,', and the first length is',i3,'.'/)
      if (nl .gt. maxlen .or. ny .gt. maxyear) then
         print *,'****Error - nl or ny too big...'
         stop
      endif
*
* Read survey data
      read(3,*) nyd
	  read(3,*) (iys(y),y=1,nyd)
      do 20 a = 1, nl
   20 read(3,*) (opn(a,y),y=1,nyd)
      do 21 a = 1, nl
   21 read(3,*) (bc(a,y),y=1,nyd)
      read(3,*) (T(y),y=1,ny)
      read(3,*) (hr(y),y=1,ny)
      do a = 1, 4
         do y = 23, 26
            opn(a,y) = opn(a,y)/qq
         end do
      end do
      do 24 a = 1, nl
      do 24 y = 1, nyd
   24 opnl(a,y) = log(opn(a,y)+sc)
      t1 = 0.0
      do 26 a = 1, nl
   26 t1 = t1 + opn(a,1)
      do 28 a = 1, nl
   28 plo(a) = opn(a,1)/t1
      nobs = nyd*nl
* read natural mortality specification
      read(4,*) iy,il
      if (iy.lt.1 .or. iy.gt.8 .or. il.lt.1 .or. il.gt.13) then
        print *,'iy<1, or iy>8, or il<1, or il>13. Change file NM.DAT'
        stop
      endif
      read(4,*) byr
      do 40 i = 1, iy
   40 read(4,*) gy(i),gyx(1,i),gyx(2,i)
      do 50 y = 1, ny
   50 read(4,*) i,idy(i-iyear1+1)
      do 60 i = 1, il
   60 read(4,*) gl(i),glx(1,i),glx(2,i)
      do 70 a = 1, nl
   70 read(4,*) i,idl(i)
      do 80 i = 1, 4
   80 read(4,*) slf(i),slfx(1,i),slfx(2,i)
      do 85 a = 1, nl
   85 read(4,*) sl(a),slx(1,a),slx(2,a)
      do 90 y = 1, ny
   90 read(4,*) sy(y),syx(1,y),syx(2,y),wei(y)
      close(4)
c initialize id(1-5) = 0
      do 95 i = 1, 5
   95 id(i) = 0
      print *,'All input data has been read... now conduct analysis...'
      return
      end
*-----------------------------------------------------------------
      subroutine vecparms(theta,nparms)
*
* Convert vectorized parameter list to separate parameter arrays.
*
      include 'rkf.cmn'
      real   theta(nparms),t1
* growth increment variation
      ii = 1
      if (id(1).eq.1) then
        betal = theta(ii)
        ii = ii + 1
      endif
* recruitment by length
      if (id(3).eq.1) then
        betar = theta(ii)
        ii = ii + 1
      endif
      if (id(2).eq.1) then
        alphar = theta(ii)
        ii = ii + 1
      else
        alphar = mr/betar
      endif
* selectivity
      if (id(4).eq.1) then
        alphas = theta(ii)
        ii = ii + 1
      endif
      if (id(5).eq.1) then
        betas = theta(ii)
        ii = ii + 1
      endif
* recruitment by year
      if (id(6).eq.1) then
        do 10 y = 2, ny
   10   rr(y) = theta(ii+y-2)
        ii = ii+ny-1
      endif
* abundances in initial year
      if (id(7).eq.1) then
        pni = theta(ii)
      else
        ii = ii - 1
      endif
      if (inm.eq.1) then
* natural mortalities by year
        do 40 y = 1, ny
   40   sy(y) = theta(ii+y)
      else if (inm.eq.2) then
* natural mortalities by year group
        do 45 i = 1, iy
   45   gy(i) = theta(ii+i)
        do 50 y = 1, ny
   50   sy(y) = gy(idy(y))
      else if (inm.eq.3) then
* natural mortalities by length group
        do 55 i = 1, il
   55   gl(i) = theta(ii+i)
        do 60 a = 1, nl
   60   sl(a) = gl(idl(a))
      else if (inm.eq.4) then
* natural mortalities by a length function
        do 65 i = 1, 4
   65   slf(i) = theta(ii+i)
        do 70 a = 1, nl
          t1 = (a-1.0)*lenint+ilen1+lenint/2.0 - slf(2)
          if (t1.lt.0.00000001) then
            sl(a) = slf(1)*exp(-t1*slf(3))
          else
            sl(a) = slf(1)*exp(t1*slf(4))
          endif
   70   continue
      else if (inm.eq.5) then
* natural mortalities by year group and length group
        do 72 i = 1, iy
   72   gy(i) = theta(ii+i)
        do 74 y = 1, ny
   74   sy(y) = gy(idy(y))
        ii = ii + iy
        do 76 i = 1, il
   76   gl(i) = theta(ii+i)
        do 78 a = 1, nl
   78   sl(a) = gl(idl(a))
      else if (inm.eq.6) then
* natural mortalities by year group and a length function
        do 80 i = 1, iy
   80   gy(i) = theta(ii+i)
        do 82 y = 1, ny
   82   sy(y) = gy(idy(y))
        ii = ii + iy
        do 84 i = 2, 4
   84   slf(i) = theta(ii+i-1)
        do 86 a = 1, nl
          t1 = (a-1.0)*lenint+ilen1+lenint/2.0 - slf(2)
          if (t1.lt.0.00000001) then
            sl(a) = exp(-t1*slf(3))
          else
            sl(a) = exp(t1*slf(4))
          endif
   86   continue
      endif
      return
      end
*-----------------------------------------------------------------
      subroutine parmsvec(theta,xlb,xub,nparms)
*
* Convert separate parameter arrays to vectorized parameter list.
*
      include 'rkf.cmn'
      real   theta(nparms),xlb(nparms),xub(nparms),t1
* growth increment variation
      ii = 1
      if (id(1).eq.1) then
        theta(ii) = betal
        xlb(ii) = betalx(1)
        xub(ii) = betalx(2)
        ii = ii + 1
      endif
* recruitment by length
      if (id(3).eq.1) then
        theta(ii) = betar
        xlb(ii) = betarx(1)
        xub(ii) = betarx(2)
        ii = ii + 1
      endif
      if (id(2).eq.1) then
        theta(ii) = alphar
        xlb(ii) = alrx(1)
        xub(ii) = alrx(2)
        ii = ii + 1
      endif
* selectivity
      if (id(4).eq.1) then
        theta(ii) = alphas
        xlb(ii) = alsx(1)
        xub(ii) = alsx(2)
        ii = ii + 1
      endif
      if (id(5).eq.1) then
        theta(ii) = betas
        xlb(ii) = betasx(1)
        xub(ii) = betasx(2)
        ii = ii + 1
      endif
* recruitment by year
      if (id(6).eq.1) then
        do 10 y = 2, ny
          theta(ii+y-2) = rr(y)
          xlb(ii+y-2) = rrx(1,y)
          xub(ii+y-2) = rrx(2,y)
   10   continue
        ii = ii + ny - 1
      endif
* abundances in initial year
      if (id(7).eq.1) then
        theta(ii) = pni
        xlb(ii) = pnix(1)
        xub(ii) = pnix(2)
      else
        ii = ii - 1
      endif
      if (inm.eq.1) then
* natural mortalities by year
        do 40 y = 1, ny
          theta(ii+y) = sy(y)
          xlb(ii+y) = syx(1,y)
          xub(ii+y) = syx(2,y)
   40   continue
      else if (inm.eq.2) then
* natural mortalities by year group
        do 45 i = 1, iy
          theta(ii+i) = gy(i)
          xlb(ii+i) = gyx(1,i)
          xub(ii+i) = gyx(2,i)
   45   continue
      else if (inm.eq.3) then
* natural mortalities by length group
        do 50 i = 1, il
          theta(ii+i) = gl(i)
          xlb(ii+i) = glx(1,i)
          xub(ii+i) = glx(2,i)
   50   continue
      else if (inm.eq.4) then
* natural mortalities by a length function
        do 55 i = 1, 4
          theta(ii+i) = slf(i)
          xlb(ii+i) = slfx(1,i)
          xub(ii+i) = slfx(2,i)
   55   continue
      else if (inm.eq.5) then
* natural mortalities by year group and length group
        do 60 i = 1, iy
          theta(ii+i) = gy(i)
          xlb(ii+i) = gyx(1,i)
          xub(ii+i) = gyx(2,i)
   60   continue
        ii = ii + iy
        do 65 i = 1, il
          theta(ii+i) = gl(i)
          xlb(ii+i) = glx(1,i)
          xub(ii+i) = glx(2,i)
   65   continue
      else if (inm.eq.6) then
* natural mortalities by year group and a length function
        do 70 i = 1, iy
          theta(ii+i) = gy(i)*slf(1)
          xlb(ii+i) = gyx(1,i)
          xub(ii+i) = gyx(2,i)
   70   continue
        ii = ii + iy
        slf(1) = 1.0
        do 75 i = 2, 4
          theta(ii+i-1) = slf(i)
          xlb(ii+i-1) = slfx(1,i)
          xub(ii+i-1) = slfx(2,i)
   75   continue
      endif
      return
      end
*-----------------------------------------------------------------
      subroutine lstheta(nparms,theta)
*
*--List the parameter vector theta on unit iunit
*
      real   theta(nparms), rssq
      include 'rkf.cmn'
      common /fitstats/ ncalls, rssq
*
      call vecparms(theta,nparms)
      write(*,100) ncalls, rssq
  100 format(1x,i6,' calls to popmodel, SSQ=',e14.7/)
      write(*,110) betal,alphar,betar,alphas,betas
  110 format(1x,'  Betal   Alphar   Betar   Alphas   Betas:'/,
     +       1x,f7.4,1x,f8.3,1x,f7.4,1x,f8.2,1x,f7.4)
      if (id(6).eq.1) then
        print *, 'recruits by year: '
        write(*,120) (rr(y), y=1,ny)
      endif
  120 format(4(7(f10.2,1x)/),7(f10.2,1x))
      if (id(7).eq.1) then
        print *, 'abundances in initial year by length: '
        write(*,130) (pn(a,1), a=1,nl),pni
      endif
  130 format(2(8(f9.1,1x)/),5(f9.1,1x),f11.1)
      if (inm.eq.1) then
        print *, 'natural mortalities by year:'
        write(*,140) (sy(y), y=1,ny)
  140   format(1x,3(11(f6.3,1x)/))
      else if (inm.eq.2) then
        print *, 'natural mortalities by year group: '
        write(*,150) (gy(i), i=1,iy)
  150   format(1x,10(f6.3,1x))
      else if (inm.eq.3) then
        print *, 'natural mortalities by length group: '
        write(*,160) (gl(i), i=1,il)
  160   format(1x,13(f5.3,1x))
      else if (inm.eq.4) then
        print *, 'natural mortality function: '
        write(*,170) (slf(i), i=1,4)
  170   format(1x,f7.4,1x,f8.3,1x,2(f12.9,1x))
      else if (inm.eq.5) then
        print *, 'natural mortalities by year group: '
        write(*,150) (gy(i), i=1,iy)
        print *, 'natural mortalities by length group: '
        write(*,160) (gl(i), i=1,il)
      else if (inm.eq.6) then
        print *, 'natural mortalities by year group: '
        write(*,150) (gy(i), i=1,iy)
        print *, 'natural mortality function: '
        write(*,170) (slf(i), i=1,4)
      endif
      if (ib.ge.1) print *, ' Replicate: ',ib
      if (ib.ge.ibm) print *, ' ib >= ibm!!!'
*
      return
      end
*-----------------------------------------------------------------
      subroutine setconvg(iparam,rparam,ib)
*
*--Set up the convergence criteria for IMSL routines
*
      integer iparam(6),idparam(6)
      real   rparam(7),rdparam(7)
*
*--First setup the default convergence parameters
       call  U4LSF(idparam,rdparam)
*--Now read the user input convergence parameters
      open(unit=9,file='param0.dat')
      read(9,*) (iparam(i),i=1,6)
      read(9,*) (rparam(i),i=1,5)
*
      if (ib.eq.0) write(*,101) (idparam(i),iparam(i),i=1,6)
  101 format(' Convergence Parameter Settings for IPARAM',//,
     +  ' IMSL defaults (from DU4LSF)            User Input Values',/,
     +  ' ---------------------------            -----------------',/,
     +   6(4x,i15,18x,i15,/) )
      do 10 i = 6,7
        rparam(i) = rdparam(i)
   10 continue
      if (ib.eq.0) write(*,102) (rdparam(i),rparam(i),i=1,7)
  102 format(1x,'Convergence Parameter Settings for RPARAM',//,
     +  ' IMSL defaults (from DU4LSF)            User Input Values',/,
     +  ' ---------------------------            -----------------',/,
     +   7(4x,E15.4,18x,E15.4,/) )
*
      close(9)
      return
      end
*-----------------------------------------------------------------
      subroutine writout(fvec,ihr,imin,iparam,rparam,ncalls,nparms,nobs,
     +             maxparms,theta,wk,wklen,xscale,fscale)
*
*--Write completion information to output file
*
      integer iparam(6),minutes,iehr,iemin,iesec,wklen,nobs,nparms
      real   sse,fvec(nobs),wk(wklen),xscale(nparms),rparam(7),
     *       fscale(nobs)
      real   gradnorm /0.0  /,stepdist /0.0  /,
     +       theta(maxparms),fvecnorm /0.0  /
      include 'rkf.cmn'
*
      open(unit=11,file='paramf.out',recl=721)
      open(unit=12,file='popf.out',recl=721)
*
      call popmodel(nobs,nparms,theta,fvec)
*
*--output the estimated parameters
      write(11,100) inm,ny,nl,ns,iyear1,ilen1,lenint,nr,mr,sc
  100 format(1x,i1,1x,5(i4,1x),f4.1,1x,i2,1x,f5.1,1x,f6.1)
      write(11,110) betal,alphar,betar,alphas,betas,pni
  110 format(1x,f8.5,1x,f8.3,1x,f8.5,1x,f9.1,1x,f9.6,1x,f12.3)
*--output the recruitment by year
      write(11,120) (rr(y),y=1,ny)
  120 format(1x,60(f10.2,1x))
*--output the partial recruitment coefficients
      write(11,130) (r(a),a=1,nl)
  130 format(1x,60(f7.4,1x))
*--output bycatch selectivity
      write(11,130) (bys(a),a=1,nl)
*--output natural mortalities
      write(11,130) (sy(y),y=1,ny)
      write(11,130) (sl(a),a=1,nl)
      write(11,140) (slf(i),i=1,4)
  140 format(1x,f7.4,1x,f8.3,1x,2(f12.9,1x))
*--output the selectivity coefficients
      write(11,130) (v(a),a=1,nl)
*--output the proportion of growth increments
      do 10 a = 1, nl
   10 write(11,130) (pg(a,i),i=1,nl)
*--output the estimated population sizes
      do 20 a = 1, nl
   20 write(12,120) (pn(a,y),y=1,ny)
*--output death bycatch
      do a = 1, nl
        write(12,120) (ebc(a,y),y=1,ny)
      end do
*--output measurable abundance
c      do 43 a = 1, nl
c   43 write(12,120) (pn(a,y)*v(a),y=1,nyd)
*--output residuals
      do a = 1, nl
         write(12,120) (fvec((a-1)*nyd+y),y=1,nyd)
      end do
c      write(12,120) (fvec(ii),ii=1,nobs)
*
*  Compute function norm
      sse =   0.0
      do 50 i = 1, nobs
         sse = sse + fvec(i)**2
         fvecnorm = MAX1(fvecnorm,ABS(fvec(i)))
   50 continue
*
* Compute gradient norm and last step distance
      do 60 i = 1,nparms
         gradnorm = MAX1(gradnorm,ABS((xscale(i)*wk(3*nparms+i))))
         stepdist = MAX1(stepdist,ABS((xscale(i)*wk(1*nparms+i))))
   60 continue
*
      call gettim(iehr,iemin,iesec,ie100)
      call getdat(iyr,imon,iday)
      minutes = iehr*60+iemin-(ihr*60+imin)
      write(11,*) (iparam(i),i=1,6)
      write(11,*) (rparam(i),i=1,7)
      WRITE(11,*) GRADNORM, STEPDIST, FVECNORM,
     +            SSE,NCALLS,MINUTES,IMON,IDAY,IYR,IEHR,IEMIN
*
      close(11)
      close(12)
      open(unit=11,file='scale.dat')
      write(11,*) xscale
      write(11,*) ' fscale:'
      write(11,*) (fscale(i),i=1,nobs)
      close(11)
*
      return
      end
*-----------------------------------------------------------------
      subroutine bycatch
*
*--Return bycatch and bycatch selectivity
      real tt0(11,60)/660*0.0/,ttt
      include 'rkf.cmn'
      if (iby.eq.0) then
         do a = 1, nl
         do y = 1, ny
            ebc(a,y) = bc(a,y)
         end do
         end do
      else
         do a = 1, nl
           bys(a) = 0.0
           do y = 19, ny
             ebc(a,y) = bc(a,y)
             ttt = pn(a,y)
             if (ttt.lt. 0.1) ttt = 0.1
             tt0(a,y) = (bc(a,y)/ttt)/hr(y)
             if (tt0(a,y) .gt. 1.0) tt0(a,y) = 1.0
            if (y.lt.23.or.(y.gt.24.and.y.lt.34)) bys(a)=bys(a)+tt0(a,y)
           end do
           bys(a) = bys(a)/13.0
         end do
         do a = 1, nl
           do y = 1, 18
             ebc(a,y) = bys(a)*hr(y)*pn(a,y)
           end do
         end do
      endif
      do a = 1, nl
      do y = 1, ny
         ebc(a,y) = ebc(a,y)*byr
      end do
      end do
*
      return
      end
*-----------------------------------------------------------------
      subroutine gammal
*
*--Return probabilities of length increment, given gamma function parameters
*  The gamma function is truncated and numerically simplified.
*
      real   al(30),t1(30),t2(26,50)/1300*0.0  /,t3,t4
      include 'rkf.cmn'
      do 20 a = 1, nl
        al(a) = g(a)/betal
        t1(a) = 0.0
        t3 = (al(a)-1.0)*log(g(a))-g(a)/betal-al(a)*log(betal)
        do 10 i = ngl(a),ngu(a)
          t4 = float(i)
          t2(a,i)=(al(a)-1.0)*log(t4)-t4/betal-al(a)*log(betal)-t3
          if (t2(a,i).lt.-1.0e30) t2(a,i)=-1.0e30
          t2(a,i) = exp(t2(a,i))
          t1(a) = t1(a) + t2(a,i)
   10   continue
   20 continue
      do 30 a = 1, nl
      do 30 i = ngl(a),ngu(a)
   30 t2(a,i)=t2(a,i)/t1(a)
      ilen = int(lenint + 0.001)
      do 60 a = 1, nl
      ii = int(ngu(a)/lenint)+2
        do 40 i = 1, ii
          j = (i-1)*ilen
          if (i.eq.1) then
           pg(a,a)=0.8*t2(a,i)+0.6*t2(a,i+1)+0.4*t2(a,i+2)+0.2*t2(a,i+3)
          else
            pg(a,a+i-1) = 0.2*t2(a,j-4)+0.4*t2(a,j-3)+0.6*t2(a,j-2)+
     +                    0.8*t2(a,j-1)+t2(a,j)+0.8*t2(a,j+1)+
     +                    0.6*t2(a,j+2)+0.4*t2(a,j+3)+0.2*t2(a,j+4)
          endif
   40   continue
        if ((ii+a-1).gt.nl) then
          do 50 i = nl+1, ii+a-1
   50     pg(a,nl) = pg(a,nl) + pg(a,i)
        endif
   60 continue
      return
      end
*-----------------------------------------------------------------
      subroutine gammar
*
*--Return proporttions of recruits belonging to a length group.
*  The gamma function is truncated and numerically simplified.
*
      real   t1(70)/70*0.0  /,t2,t3,t4
      include 'rkf.cmn'
      ii = nr*int(lenint+0.001)
      ilen = int(lenint+0.001)
      t2 = 0.0
      t4 = alphar*betar
      t3 = (alphar-1.0)*log(t4)-t4/betar-alphar*log(betar)
      do 10 a = 1, ii
        t4 = (a-1)+ilen1
        t1(a) = (alphar-1.0)*log(t4)-t4/betar-alphar*log(betar)-t3
        if (t1(a).lt.-1.0e30) t1(a)=-1.0e30
        t1(a) = exp(t1(a))
        t2 = t2 + t1(a)
   10 continue
      do 30 a = 1, nr
        r(a) = 0.0
        do 20 i = 1, ilen
   20   r(a) = r(a) + t1((a-1)*ilen+i)/t2
   30 continue
      return
      end
*-----------------------------------------------------------------
      subroutine slength
*
*--Return selectivity coefficients, given parameters of a logistic func.
*
      real t1
      include  'rkf.cmn'
      do 10 a = 1, ns
        t1 = ilen1+lenint/2.0+(a-1)*lenint
        v(a) = 1.0/(1.0+alphas*exp(-betas*t1))
   10 continue
      do 20 a = 1,ns
   20 v(a) = v(a)/v(ns)
      t1 = 0.0
      do 30 a = 1, nl
        pl(a) = plo(a)/v(a)
        t1 = t1 + pl(a)
   30 continue
      do 40 a = 1, nl
   40 pl(a) = pl(a)/t1
      return
      end
*-----------------------------------------------------------------
      SUBROUTINE PARMRESP(XLB,XUB,NPARMS,THETA,FVEC,NOBS)
*
* Investigate response surface in neighborhood of solution for each parameter
*   Vary parameter over range from solution +/- rangepct*solution
*   Print residual sum of squares at each step over range
*
      include 'rkf.cmn'
      integer nparms,ncalls
      real   xlb(nparms),xub(nparms),fvec(nobs),boundval,
     +      theta(nparms),rangepct,fuzz,theta1,rssq
      common /fitstats/ ncalls, rssq
      parameter (nstep=9)
      real   frssq(nstep)
      data rangepct /0.4/
*
      boundval = int(rssq) + 1.0
      open(unit=11,file='parmresp.csv')
*
* First write out the range of percents used
      fuzz = (1-rangepct)
      do 10 i = 1, nstep
         frssq(i) = fuzz
         fuzz = fuzz + 2*rangepct/(nstep-1)
   10 continue
      write(11,101) (frssq(i),i=1,nstep)
*
      do 60 nparm = 1, nparms
         fuzz = (1-rangepct)
         theta1 = theta(nparm)
         do 50 i = 1, nstep
* Fuzz this particular parameter keeping other parameters at solution
            theta(nparm) = fuzz*theta1
* Compute RSSQ if fuzzed parameter is within bounds
            if (theta(nparm) .le. xub(nparm) .and.
     +          theta(nparm) .ge. xlb(nparm)) then
               call popmodel(nobs,nparms,theta,fvec)
               rssq = 0.0
               do 40 ii = 1, nobs
   40          rssq = rssq + fvec(ii)**2
               FRSSQ(I) = RSSQ
* Otherwise SSQ to missing value of 0.0
            else
               FRSSQ(I) = boundval
            endif
            fuzz = fuzz + 2*rangepct/(nstep-1)
   50    continue
*
* Reset this parameter to its original value, pop.values and output result
         theta(nparm) = theta1
         write(11,101) (frssq(i),i=1,nstep)
  101    format(1x,20(f11.5,1h,))
   60 continue
      close(11)
      return
      end
*-----------------------------------------------------------------
       subroutine project(nparms,theta)
c project the abundance in the last year without the survey data.
       character*80 filen
       real theta(nparms)
       include 'rkf.cmn'
       write(*,*) ' Enter recruits in the projected year (1000 crabs):'
       read(*,*) rr(ny)
       write(*,'(a\)') ' File name for abundance in ny-1: '
       read(*,'(a)') filen
       open(13,file=filen,status='old')
       do 10 a = 1, nl
10     read(13,*) pn(a,ny-1)
c project abundances by length and shell condition.
      call vecparms(theta,nparms)
      y = ny-1
      pn(1,ny) = rr(ny)*r(1)
      do 30 a = 1, nl-1
        tp = 0.0
        do 20 j = 1, a+1
          tm = sl(j)*sy(y)
          tp = tp + pg(j,a+1)*pn(j,y)*exp(-tm)
   20   continue
* Force the population to be non-negative.
        if (tp.lt.0.0) tp = 0.0
        pn(a+1,y+1) = tp + rr(y+1)*r(a+1)
   30 continue
c Output results
       write(*,*) ' Mid-CL (mm)   Abundance'
       do 40 a = 1, nl
40     write(*,100) ilen1+(a-1.0)*lenint+lenint/2.0,pn(a,ny)
100    format(4x,f5.1,4x,f11.3)
       write(*,'(a\)') ' Want to save the result (1=Yes, 0=No)? '
       read(*,*) iii
       if (iii.eq.1) then
         write(*,'(a\)') ' Please enter file name to save the result: '
         read(*,'(a)') filen
         open(14,file=filen,status='new')
         do 50 a = 1, nl
50       write(14,100) ilen1+(a-1.0)*lenint+lenint/2.0,pn(a,ny)
       endif
       return
       end
*-----------------------------------------------------------------
       real*8 function rann(s)
c This function generates random numbers by congruential method.
       real*8 s, sn, two
c       data s/2147647.0/
       sn = 7.0d0**5*s
       two = 2.0d0**31 - 1.0d0
       s = mod(sn,two)
       rann = s/(two+1.0d0)
       return
       end
c---------------------------------------------------------------
       Subroutine boot0(nobs,nparms,theta,fvec)
       include  'rkf.cmn'
       integer nobs,nparms
       real theta(nparms),fvec(nobs)
       seed = 8901234.0
       open(unit=16,file='bootf.out',recl=721)
       open(unit=15,file='ferb.out',recl=721)
       open(unit=17,file='fe95b.out',recl=721)
       open(unit=18,file='fe90b.out',recl=721)
 1     write(*,'(a\)') ' Enter number of replicates: '
       read(*,*) ibm
       if (ibm.lt.2 .or. ibm.gt.2000) goto 1
       call popmodel(nobs,nparms,theta,fvec)
       obetal = betal
       oalphar = alphar
       obetar = betar
       do 10 y = 2, ny
   10  orr(y) = rr(y)
       opni = pni
       do 20 i = 1, iy
   20  ogy(i) = gy(i)
c       do 30 i =1, 4
c   30  oslf(i) = slf(i)
       ii = 0
       do 40 a = 1, nl
       do 40 y = 1, ny
         ii = ii + 1
         fv(a,y) = fvec(ii)*(1.4142*wei(y))
   40  continue
       do 70 a = 1, nl
       do 70 y = 1, ny
   70  pnb(a,y) = pn(a,y)
       return
       end
c---------------------------------------------------------------
       Subroutine boot
       include  'rkf.cmn'
       betal = obetal
       alphar = oalphar
       betar = obetar
       do 10 y = 2, ny
   10  rr(y) = orr(y)
       pni = opni
       do 20 i = 1, iy
   20  gy(i) = ogy(i)
c       do 30 i =1, 4
c   30  slf(i) = oslf(i)
       do 50 y = 1, nyd
   31    er = rann(seed)
         iiy = int(er*nyd)+1
         if (iiy.eq.1) goto 31
         if (iiy .gt. nyd) iiy = nyd
         do 40 a = 1, nl
c          er = rann(seed)
c          iil = int(er*(nl-1))+1
c          if (iil .gt. (nl-1)) iil = nl-1
           opnl(a,iys(y)) = log(pnb(a,iys(y))+sc) - fv(a,iiy)
   40    continue
c        opnl(nl,y) = log(pnb(nl,y)+sc)-fv(nl,iiy)
   50  continue
       return
       end
c---------------------------------------------------------------
       subroutine update
       include  'rkf.cmn'
       wbetal(ib) = betal
       walphar(ib) = alphar
       wbetar(ib) = betar
       do 10 y = 2, ny
   10  wrr(y) = rr(y)
       wpni(ib) = pni
       do 15 i = 1, iy
   15  wgy(ib,i) = gy(i)
c       do 20 i =1, 4
c   20  wslf(ib,i) = slf(i)
       do 25 a = 1, nl
       do 25 y = 1, ny
   25  call upda(wpnm(a,y),wpns(a,y),pn(a,y),ib)
       i90 = int((90.0 - ilen1)/lenint) + 1
       i95 = int((95.0 - ilen1)/lenint) + 1
       do 40 y = 1, ny
         wp90(y) = 0.0
         wp95(y) = 0.0
         do 30 a = i90, nl
   30    wp90(y) = wp90(y) + pn(a,y)
         do 35 a = i95, nl
   35    wp95(y) = wp95(y) + pn(a,y)
   40  continue
       write(15,100) (wrr(y),y=2,ny)
       write(17,100) (wp95(y),y=1,ny)
       write(18,100) (wp90(y),y=1,ny)
100    format(1x,60(f10.2,1x))
       return
       end
c---------------------------------------------------------------
       subroutine bootout
       include  'rkf.cmn'
c       real sortr(2000)
       write(16,*) ibm
c       do 20 i = 1, ibm
c   20  write(16,120) (wslf(i,a),a=1,4)
       do 25 a = 1, nl
   25  write(16,110) (wpnm(a,y),y=1,ny)
       do 27 a = 1, nl
   27  write(16,115) (wpns(a,y),y=1,ny)
       do 30 i = 1, ibm
   30  write(16,100) wbetal(i),walphar(i),wbetar(i),wpni(i),
     +              (wgy(i,y),y=1,iy)
c       do 45 y = 1, ny
c         do 35 i = 1, ibm
c   35    sortr(i) = wp90(i,y)
c         call sort(ibm,sortr)
c         do 40 i = 1, ibm
c   40    wp90(i,y) = sortr(i)
c   45  continue
c       do 50 i = 1, ibm
c   50  write(16,110) (wp90(i,y),y=1,ny)
c       do 65 y = 1, ny
c         do 55 i = 1, ibm
c   55    sortr(i) = wp95(i,y)
c         call sort(ibm,sortr)
c         do 60 i = 1, ibm
c   60    wp95(i,y) = sortr(i)
c   65  continue
c       do 70 i = 1, ibm
c   70  write(16,110) (wp95(i,y),y=1,ny)
100    format(1x,f8.5,1x,f8.3,1x,f8.5,1x,f12.3,1x,4(f7.4,1x))
110    format(1x,60(f10.2,1x))
115    format(1x,60(f10.1,1x))
c120    format(1x,f7.4,1x,f8.3,1x,2(f12.9,1x)
       return
       end
c------------------------------------------------------------
c       subroutine sort(n,ra)
c       dimension ra(n)
c       L = n/2 + 1
c       ir = n
c10     continue
c         if (L .gt. 1) then
c           L = L - 1
c           rra = ra(L)
c         else
c           rra = ra(ir)
c           ra(ir) = ra(1)
c           ir = ir - 1
c           if (ir .eq. 1) then
c             ra(1) = rra
c             return
c           endif
c         endif
c         i = L
c         j = L + L
c20       if (j .le. ir) then
c           if (j.lt.ir) then
c             if (ra(j).lt.ra(j+1)) j = j + 1
c           endif
c           if (rra.lt.ra(j)) then
c             ra(i) = ra(j)
c             i = j
c             j = j + j
c           else
c             j = ir + 1
c           endif
c           goto 20
c         endif
c         ra(i) = rra
c       goto 10
c       return
c       end
c---------------------------------------------------------------
      subroutine upda(xn,s,x,n)
*--Update the mean and variation
      real xn,s,x,tem
      if (n.eq.1) then
        xn = x
        s = 0.0
        return
      endif
      tem = float(n)
      s = ((tem-2.0)*s + (tem-1.0)/tem*(x-xn)**2)/(tem-1.0)
      xn = ((tem-1.0)*xn + x)/tem
      return
      end
c------------------------------------------------------------------
