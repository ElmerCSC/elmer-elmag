FUNCTION getElectricalConductivity(Model, n, arg) RESULT(S)
  !!! Frederic Trillaud <ftrillaudp@gmail.com> - July 4, 2020
  !!! elmerf90 -o electricalConductivity.so electricalConductivity.F90
  !!! Compute the local electric conductivity S(t,x)

  ! Elmer module
  USE DefUtils

  IMPLICIT NONE
  TYPE(Model_t) :: model
  INTEGER :: n
  REAL(KIND=dp) :: arg(*)
  REAL(KIND=dp) :: T, Bx, By, Bz, B, Jex, Jey, Jez, mu0
  REAL(KIND=dp) :: Tcm0, S, Tcs, lambda, f, beta, Top
  REAL(KIND=dp) :: sigma_m, gamma_m, sigma_sc, gamma_sc, Je, Jm, Jc, Jc_op, nv_ref, nValue, Ec, Jsc
  LOGICAL :: gotIt, visu

  !!! Get parameters from sif file:
  ! variables needed inside function
  TYPE(ValueList_t), POINTER :: material, const
  ! get pointer on list for material
  material => GetMaterial()
  IF (.NOT. ASSOCIATED(material)) THEN
    CALL Fatal('getDissipation', 'No material found')
  END IF
  ! read in reference n-value
  nv_ref = GetConstReal( material, 'N-Value', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getDissipation', 'N-Value')
  END IF
  ! read in reference critical Temperature
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

  !!! Get the variables from the input:
  T = ABS(arg(1))
  Bx = arg(2)
  By = arg(3)
  Bz = arg(4)
  Jex = arg(5)
  Jey = arg(6)
  Jez = arg(7)

  B = SQRT(Bx**2+By**2+Bz**2)
  Je = SQRT(Jex**2+Jey**2+Jez**2)
  
  Jc_op = getJc(Top,B)
  !!! Current-sharing temperature (K):
  Tcs = Tcm0 + f*(1.0+lambda)*(Je/Jc_op)*(Top-Tcm0)

  !!! Electric conductivity S (Siemens):
  ! Superconducting state
  IF (T < Tcs) THEN
    IF (visu) THEN
      PRINT *, "*** NO ELECTRICAL RESISTANCE ***"
    END IF
    S = 1.0*1e18
  ! Current-sharing model
  ELSE IF (( T >= Tcs) .AND. (T < Tcm0 )) THEN
    IF (visu) THEN
      PRINT *, "*** MIXED DISSIPATION ***"
    END IF
    Jc = getJc(T,B)
    nValue = getNvalue(T,B,nv_ref)
    CALL NRSolver_Jsc(Jsc,Je,Jc,nValue,lambda,f,sigma_m,Ec)
    Jm = f*(1.0+1.0/lambda)*Je-(f/lambda)*Jsc
    sigma_sc = ((Jc**nValue)/Ec)*(Jsc**(1-nValue))
    S = sigma_m + sigma_sc
    IF (visu) THEN
      PRINT 1, Jc,nValue,Jm,sigma_sc
      1  FORMAT(' Jc: ', EN12.3, ' n-Value: ', EN12.3, ' Jm: ', EN12.3, ' sigma_sc: ', EN12.3)
    END IF
  ! Normal resistive state
  ELSE
    IF (visu) THEN
      PRINT *, "*** MATRIX RESISTANCE ***"
    END IF
    S = sigma_m
  END IF

  IF (visu) THEN
    PRINT 2, Jc_op, B, Top
    2  FORMAT(' Jc_op(Top,B): ', EN12.3,', B: ', EN12.3, ', Top: ', EN12.3)
    PRINT 3, T, Tcs
    3  FORMAT(' T: ', EN12.3,', Tcs = ', EN12.3)
    PRINT 4, S
    4  FORMAT(' S: ', EN12.3)
  END IF

  CONTAINS
    !!! Compute Jc(T,B,eps) in A/m^2:
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

    !!! N-value, n(T,B):
    FUNCTION getNValue(arg_T,arg_B,nv_ref) RESULT(nValue)
      IMPLICIT NONE
      REAL(KIND=dp) :: arg_T, arg_B, nValue, nv_ref
      nValue = nv_ref
    END FUNCTION getNValue

    !!! Newton-Raphson algorithm, solve Jsc
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

END FUNCTION getElectricalConductivity
