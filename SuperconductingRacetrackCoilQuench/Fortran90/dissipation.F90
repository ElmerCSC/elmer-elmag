FUNCTION getDissipation(Model, n, arg) RESULT(G)
  ! Frederic Trillaud <ftrillaudp@gmail.com> - July 4, 2020
  !elmerf90 -o dissipation.so dissipation.F90

  ! Elmer module
  USE DefUtils

  IMPLICIT NONE
  TYPE(Model_t) :: model
  INTEGER :: n
  REAL(KIND=dp) :: arg(*)
  REAL(KIND=dp) :: T, Bx, By, Bz, B, Jex, Jey, Jez, mu0
  REAL(KIND=dp) :: Tcm0, G, Tcs, lambda, f, beta, Top
  REAL(KIND=dp) :: sigma_m, gamma_m, sigma_sc, gamma_sc, Je, Jm, Jc, Jc_op, nValue, Ec, Jsc
  REAL(KIND=dp) :: tt, Q
  LOGICAL :: gotIt, visu

  ! variables needed inside function
  TYPE(ValueList_t), POINTER :: material, const
  ! get pointer on list for material
  material => GetMaterial()
  IF (.NOT. ASSOCIATED(material)) THEN
    CALL Fatal('getDissipation', 'No material found')
  END IF
  ! read in reference Critical Current density
  !nvalue = GetConstReal( material, 'N-Value', gotIt)
  !IF (.NOT. gotIt) THEN
  !  CALL Fatal('getDissipation', 'N-Value')
  !END IF
  ! read in reference Critical Temperature
  Tcm0 = GetConstReal( material, 'Critical Temperature', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getDissipation', 'Critical Temperature')
  END IF
  ! read in the electrical conductivity
  sigma_m = GetConstReal( material, 'Matrix Electric Conductivity', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getDissipation', 'Matrix Electric Conductivity')
  END IF
  ! read in the densities
  gamma_m = GetConstReal( material, 'Matrix Density', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getDissipation', 'Matrix Density')
  END IF
  gamma_sc = GetConstReal( material, 'Superconductor Density', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getDissipation', 'Superconductor Density')
  END IF
  lambda = GetConstReal( material, 'Matrix to Superconductor Ratio', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getDissipation', 'Matrix to Superconductor Ratio')
  END IF
  f = GetConstReal( material, 'Filling Factor', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getDissipation', 'Filling Factor')
  END IF
  Ec = GetConstReal( material, 'Critical Electrical Field', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getDissipation', 'Critical Electrical Field')
  END IF
  Top = GetConstReal( material, 'Operating Temperature', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getDissipation', 'Operating Temperature')
  END IF

  const => GetConstants()
  IF (.NOT. ASSOCIATED(const)) THEN
    CALL Fatal('getDissipation', 'No constant found')
  END IF
  mu0 = GetConstReal( const, 'Permeability of Vacuum', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getDissipation', 'Permeability of Vacuum')
  END IF

  visu = .FALSE.

  ! Get the variables from the input
  tt = arg(1)
  T = ABS(arg(2))
  Bx = arg(3)
  By = arg(4)
  Bz = arg(5)
  Jex = arg(6)
  Jey = arg(7)
  Jez = arg(8)

  B = SQRT(Bx**2+By**2+Bz**2)
  Je = SQRT(Jex**2+Jey**2+Jez**2)

  ! Patch on the temperature to limit the appearance of negative temperature and its possible impact
  IF (T < Top) THEN
    T = Top
  ELSE IF (T > 299.0) THEN
    T = 299.0 ! No material property data above 300 K
  END IF
  
  Jc_op = getJc(Top,B)
  Tcs = Tcm0 + f*(1.0+lambda)*(Je/Jc_op)*(Top-Tcm0)

  ! basic model, assuming s steep transition
  !IF (T < (0.5*(Tcs+Tcm0))) THEN
  !  PRINT *, "*** NO DISSIPATION ***"
  !  G = 0.0
  !ELSE
  !  PRINT *, "*** FULL DISSIPATION ***"
  !  G = ((f*(1.0+1.0/lambda)*Je)**2)/(sigma_m*gamma_m)
  !END IF

  ! Current sharing model
  IF (T < Tcs) THEN
    IF (visu) THEN
      PRINT *, "*** NO DISSIPATION ***"
    END IF
    G = 0.0
  ELSE IF (( T >= Tcs) .AND. (T < Tcm0 )) THEN
    IF (visu) THEN
      PRINT *, "*** MIXED DISSIPATION ***"
    END IF
    Jc = getJc(T,B)
    nValue = getNvalue(T,B)
    CALL NRSolver_Jsc(Jsc,Je,Jc,nValue,lambda,f,sigma_m,Ec)
    Jm = f*(1.0+1.0/lambda)*Je-(f/lambda)*Jsc
    sigma_sc = ((Jc**nValue)/Ec)*(Jsc**(1-nValue))
    IF (visu) THEN
      PRINT 1, Jc,nValue,Jm,sigma_sc
      1  FORMAT(' Jc: ', EN12.3, ' n-Value: ', EN12.3, ' Jm: ', EN12.3, ' sigma_sc: ', EN12.3)
    END IF
    G = (Jm**2)/(sigma_m*gamma_m)+(Jsc**2)/(sigma_sc*gamma_sc)
  ELSE
    IF (visu) THEN
      PRINT *, "*** FULL DISSIPATION ***"
    END IF
    G = ((f*(1.0+1.0/lambda)*Je)**2)/(sigma_m*gamma_m)
  END IF
  ! Adding the disturbance to the dissipation
  G = G + getQd(model, n, tt)

  IF (visu) THEN
    PRINT 2, Jc_op,B
    2  FORMAT(' Jc_op: ', EN12.3,', B: ', EN12.3)
    PRINT 3, T,Tcs,Top
    3  FORMAT(' T: ', EN12.3,', Tcs = ', EN12.3,', Top: ', EN12.3)
    PRINT 4, G
    4  FORMAT(' G: ', EN12.3)
  END IF

  CONTAINS
    FUNCTION getJc(arg_T,arg_B) RESULT(JJc)
      IMPLICIT NONE
      REAL(KIND=dp) :: arg_T, arg_B, JJc
      REAL(KIND=dp) :: Ca1, Ca2, e_0a, e_m, e_ap, e_a, e_sh, s
      REAL(KIND=dp) :: Tc0e, tt, Bc2m0, Bc2, bb, MDG, C1, p, q
      ! RP Nb3Sn: A. Godeke, "Characterization of High Current RRP Wires as a Function of Magnetic Field, Temperature, and Strain", IEEE-TAS, 2009
      Ca1 = 47.6
      Ca2 = 6.4
      e_0a = 0.273/100
      e_m = -0.09/100
      e_ap = 0.0
      e_a = e_ap+e_m
      e_sh = Ca2*e_0a/SQRT(Ca1**2-Ca2**2)
      s = (1/(1-Ca1*e_0a))*(Ca1*(SQRT(e_sh**2+e_0a**2)-SQRT((e_a-e_sh)**2+e_0a**2))-Ca2*e_a)+1

      Tc0e = Tcm0*s**(1/3)
      tt = arg_T/Tc0e

      Bc2m0 = 30.7
      MDG = 1-tt**1.52
      Bc2 = Bc2m0*MDG*s
      bb = arg_B/Bc2

      C1 = 30.0e8
      p = 0.5
      q = 2

      IF (arg_T < Tcm0) THEN
        JJc = (C1/arg_B)*s*(1-tt**1.52)*(1-tt**2)*(bb**p)*((1-bb)**q)
      ELSE
        JJc = 0.0
      END IF
    END FUNCTION getJc

    FUNCTION getNValue(arg_T,arg_B) RESULT(nValue)
      IMPLICIT NONE
      REAL(KIND=dp) :: arg_T, arg_B, nValue
      nValue = 15.0
    END FUNCTION getNValue

    SUBROUTINE NRSolver_Jsc(Jsc,Je,Jc,nValue,lambda,f,sigma_m,Ec)
      IMPLICIT NONE
      REAL(kind=dp), INTENT(IN) :: Je, Jc, nValue, lambda, f, sigma_m, Ec
      REAL(kind=dp), INTENT(OUT) :: Jsc
      REAL(kind=dp) :: fx, dfx, beta
      REAL(kind=dp) :: tol_rel, er_rel
      INTEGER :: k, max_iter

      beta = f*(Jc**nValue)/(sigma_m*lambda*Ec)
      IF (visu) THEN
        PRINT 5, beta
        5  FORMAT(' Beta: ', EN12.3)
      END IF
      
      k = 1
      max_iter = 2000
      tol_rel = 0.001
      er_rel = 1000.0
      Jsc = Je
      ! Newton loop
      DO WHILE ((er_rel > tol_rel) .AND. (k < max_iter))
        fx = (Jsc**nValue)+beta*Jsc-(1.0+lambda)*beta*Je
        dfx = nValue*(Jsc**(nValue-1))+beta
        Jsc = Jsc - fx/dfx
        IF (dfx == 0.0) THEN
          PRINT *, "*** WARNING: Derivative equal to zero, aborting Newton-Raphson algorithm ***"
          EXIT
        END IF
        er_rel = ABS(fx/(Jsc*dfx))
        IF (visu) THEN
          PRINT 6, fx,dfx,er_rel
          6  FORMAT(' fx: ', EN12.3, ', dfx: ', EN12.3, ', er. rel.: ', EN12.3)
          PRINT 7, k,Jsc
          7  FORMAT(' Newton-Raphson - iteration: ', I5, ', Jsc = ', EN12.3)
        END IF
        k = k+1
      END DO
    END SUBROUTINE NRSolver_Jsc

    FUNCTION getQd(model, n, tt) RESULT(Q)
      IMPLICIT NONE
      TYPE(Model_t) :: model
      INTEGER :: n
      REAL(KIND=dp) :: tt
      REAL(KIND=dp) :: X, Y, Z, t_ini_d, Dt_d, Qd, Q
      REAL(KIND=dp) :: R_d, x_d, y_d, z_d, d

      ! Parameters needed inside the function
      TYPE(ValueList_t), POINTER :: bfList
      bfList => getbodyforce()
      ! Value of the dissipation in the disturbance (W/kg)
      Qd = ListGetConstReal(bfList, 'Heat Disturbance', gotIt)
      IF ( .NOT. gotIt ) THEN
        CALL Warn('getDisturbance', 'Heat Disturbance not found')
      END IF
      ! Initial time of the durantion
      t_ini_d = ListGetConstReal(bfList, 'Heat Disturbance Initial Time', gotIt)
      IF ( .NOT. gotIt ) THEN
        CALL Warn('getDisturbance', 'Heat Disturbance Initial Time not found')
      END IF
      ! Time duration of the disturbance
      Dt_d = ListGetConstReal(bfList, 'Disturbance Duration', gotIt)
      IF ( .NOT. gotIt ) THEN
        CALL Warn('getDisturbance', 'Disturbance Duration not found')
      END IF
      ! Position of the center of the disturbance
      x_d = ListGetConstReal(bfList, 'Disturbance X Center', gotIt)
      IF ( .NOT. gotIt ) THEN
        CALL Warn('getDisturbance', 'Disturbance X Center not found')
      END IF
      y_d = ListGetConstReal(bfList, 'Disturbance Y Center', gotIt)
      IF ( .NOT. gotIt ) THEN
        CALL Warn('getDisturbance', 'Disturbance Y Center not found')
      END IF
      z_d = ListGetConstReal(bfList, 'Disturbance Z Center', gotIt)
      IF ( .NOT. gotIt ) THEN
        CALL Warn('getDisturbance', 'Disturbance Z Center not found')
      END IF
      ! Dimension of the spherical disturbance
      ! of the order of the length of geometry over number of elements
      R_d = ListGetConstReal(bfList, 'Disturbance Size', gotIt)
      IF ( .NOT. gotIt ) THEN
        CALL Warn('getDisturbance', 'Disturbance Size not found')
      END IF

      ! get local coordinates of the nodes
      X = model % Nodes % x(n)
      Y = model % Nodes % y(n)
      Z = model % Nodes % z(n)

      ! distance to center of disturbance
      d = SQRT((X-x_d)**2+(Y-y_d)**2+(Z-z_d)**2)
      
      IF (d > R_d) THEN
        Q = 0.0D00
      ELSE
        Q = Qd
        IF (tt <= t_ini_d) THEN
          Q = 0.0D00
        ELSE IF (( tt > t_ini_d) .AND. (tt < (t_ini_d+Dt_d))) THEN
          Q = Qd
        ELSE
          Q = 0.0D00
        END IF
      END IF
    END FUNCTION getQd

END FUNCTION getDissipation
