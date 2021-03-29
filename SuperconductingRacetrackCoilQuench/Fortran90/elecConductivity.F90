FUNCTION getElecConductivity(Model, n, arg) RESULT(S)
  ! Frederic Trillaud <ftrillaudp@gmail.com> - July 4, 2020
  !elmerf90 -o elecConductivity.so elecConductivity.F90

  ! Elmer module
  USE DefUtils

  IMPLICIT NONE
  TYPE(Model_t) :: model
  INTEGER :: n
  REAL(KIND=dp) :: arg(*)
  REAL(KIND=dp) :: T, Bx, By, Bz, B, Jex, Jey, Jez, mu0
  REAL(KIND=dp) :: S, Tcm0, Tcs, lambda, f, beta, Top
  REAL(KIND=dp) :: sigma_m, gamma_m, sigma_sc, gamma_sc, Je, Jm, Jc, Jc_op, nValue, Ec, Jsc
  LOGICAL :: gotIt

  ! variables needed inside function
  TYPE(ValueList_t), POINTER :: material, const
  ! get pointer on list for material
  material => GetMaterial()
  IF (.NOT. ASSOCIATED(material)) THEN
    CALL Fatal('getDissipation', 'No material found')
  END IF
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
  ! read in the non-Sc to SC ratio
  lambda = GetConstReal( material, 'Matrix to Superconductor Ratio', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getDissipation', 'Matrix to Superconductor Ratio')
  END IF
  ! read in the fill factor
  f = GetConstReal( material, 'Filling Factor', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getDissipation', 'Filling Factor')
  END IF
  ! read in critical electrical field
  Ec = GetConstReal( material, 'Critical Electrical Field', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getDissipation', 'Critical Electrical Field')
  END IF
  ! read in operating temperature
  Top = GetConstReal( material, 'Operating Temperature', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getDissipation', 'Operating Temperature')
  END IF

  ! Get the variables from the input
  T = arg(1)
  Bx = arg(2)
  By = arg(3)
  Bz = arg(4)
  Jex = arg(5)
  Jey = arg(6)
  Jez = arg(7)

  B = SQRT(Bx**2+By**2+Bz**2)
  Je = SQRT(Jex**2+Jey**2+Jez**2)
  
  Jc_op = getJc(Top,B)
  Tcs = Tcm0 + f*(1.0+lambda)*(Je/Jc_op)*(Top-Tcm0)

  ! Current sharing model
  IF (T <= Tcs) THEN
    PRINT *, "*** NO ELECTRICAL RESISTANCE ***"
    S = 1.0*1e18
  ELSE IF (( T > Tcs) .AND. (T < Tcm0 )) THEN
    PRINT *, "*** CURRENT SHARING ***"
    Jc = (1+lambda) * getJc(T,B)
    nValue = getNvalue(T,B)
    CALL NRSolver_Jsc(Jsc,Je,Jc,nValue,lambda,f,sigma_m,Ec)
    sigma_sc = ((Jc**nValue)/Ec)*(Jsc**(1-nValue))
    S = sigma_m + sigma_sc
    PRINT 1, Jc,nValue,sigma_sc
    1  FORMAT(' Jc: ', EN12.3,' n-Value: ', EN12.3,' sigma_sc: ', EN12.3)
  ELSE
    PRINT *, "*** MATRIX RESISTANCE ***"
    S = sigma_m
  END IF

  PRINT 2, Jc_op,B
  2  FORMAT(' Jc_op: ', EN12.3,', B: ', EN12.3)
  PRINT 3, T,Tcs,Top
  3  FORMAT(' T: ', EN12.3,', Tcs = ', EN12.3,', Top: ', EN12.3)
  PRINT 4, S
  4  FORMAT(' S: ', EN12.3)

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
      PRINT 5, beta
      5  FORMAT(' Beta: ', EN12.3)
      
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
        PRINT 6, fx,dfx,er_rel
        6  FORMAT(' fx: ', EN12.3, ', dfx: ', EN12.3, ', er. rel.: ', EN12.3)
        PRINT 7, k,Jsc
        7  FORMAT(' Newton-Raphson - iteration: ', I5, ', Jsc = ', EN12.3)
        k = k+1
      END DO
    END SUBROUTINE NRSolver_Jsc

END FUNCTION getElecConductivity
