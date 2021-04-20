SUBROUTINE getTherConductivity(Model, n, arg, Conductivity)
  !!! Frederic Trillaud <ftrillaudp@gmail.com> - July 12, 2020
  !!! elmerf90 -o therConductivity.so therConductivity.F90
  !!! Compute the tensor of thermal conductivity in the material frame and transform the tensor back into the global frame through a transformation matrix Q.

  ! Elmer module
  USE DefUtils

  IMPLICIT NONE
  TYPE(Model_t) :: model
  INTEGER :: n
  REAL(KIND=dp) :: arg(*)
  REAL(KIND=dp) :: T, Jex, Jey, Je, Jez, Bx, By, Bz, B, Top
  REAL(KIND=dp) :: eu_x, eu_y, eu_z, ev_x, ev_y, ev_z, ev, ew_x, ew_y, ew_z
  REAL(KIND=dp) :: k_11, k_22
  REAL(KIND=dp), POINTER ::  Conductivity(:,:)
  REAL(KIND=dp), DIMENSION(3,3) :: kk_lc, kk_gc, Q
  LOGICAL :: gotIt, visu

  !!! Get parameters from sif file:
  TYPE(ValueList_t), POINTER :: material, const
  ! get pointer on list for material
  material => GetMaterial()
  IF (.NOT. ASSOCIATED(material)) THEN
    CALL Fatal('getDissipation', 'No material found')
  END IF

  Top = GetConstReal( material, 'Operating Temperature', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getDissipation', 'Operating Temperature')
  END IF

  visu = .FALSE.

  ! Get the variables from the input
  T = ABS(arg(1))
  Bx = arg(2)
  By = arg(3)
  Bz = arg(4)
  Jex = arg(5)
  Jey = arg(6)
  Jez = arg(7)
  B = SQRT(Bx**2+By**2+Bz**2)
  Je = SQRT(Jex**2+Jey**2+Jez**2)

  !!! Patch on the temperature:
  IF (T < Top) THEN
    T = Top
  ELSE IF (T > 299.0) THEN
    T = 299.0
  END IF

  !!! Transformation matrix Q:
  !!! We use the current density and normalize it for the first vector of the local coordinate system. We bult to additional unit vectors orthoganal between them
  ! First unit vector e_u along current density vector
  eu_x = Jex/Je
  eu_y = Jey/Je
  eu_z = Jez/Je
  
  ! Second unit vector e_v (e_u . e_v = 0)
  IF (Jex /= 0.0) THEN
    ev_x = -(1/Jex)*(Jey+Jez)
    ev_y = 1.0
    ev_z = 1.0
    IF (visu) THEN
      PRINT *,"Jex /= 0"
    END IF
  ELSE IF (Jey /= 0.0) THEN
    ev_x = 1.0
    ev_y = -(1/Jey)*(Jex+Jez)
    ev_z = 1.0
    IF (visu) THEN
      PRINT *,"Jey /= 0"
    END IF
  ELSE
    ev_x = 1.0
    ev_y = 1.0
    ev_z = -(1/Jez)*(Jex+Jey)
    IF (visu) THEN
      PRINT *,"Jez /= 0"
    END IF
  END IF
  ev = SQRT(ev_x**2+ev_y**2+ev_z**2)
  ev_x = ev_x/ev
  ev_y = ev_y/ev
  ev_z = ev_z/ev

  ! Third unit vector e_w = e_u x e_v
  ew_x = eu_y*ev_z-eu_z*ev_y
  ew_y = -(eu_x*ev_z-eu_z*ev_x)
  ew_z = eu_x*ev_y-eu_y*ev_x

  Q = 0.0D00
  Q(1,1) = eu_x
  Q(2,1) = eu_y
  Q(3,1) = eu_z
  Q(1,2) = ev_x
  Q(2,2) = ev_y
  Q(3,2) = ev_z
  Q(1,3) = ew_x
  Q(2,3) = ew_y
  Q(3,3) = ew_z

  !!! Tensor of thermal conductivity in local coordinate system:
  kk_lc = 0.0D00
  kk_lc(1,1) = getk_11(T)
  kk_lc(2,2) = getk_22(T)
  kk_lc(3,3) = getk_22(T)

  ! Tensor of thermal conductivity in global coordinate system: kk_gc = Q.kk_lc.Q^t
  kk_gc = 0.0D00
  kk_gc = MATMUL(MATMUL(Q,kk_lc),TRANSPOSE(Q))

  Conductivity = 0.0D00
  Conductivity(1,1) = kk_gc(1,1)
  Conductivity(1,2) = kk_gc(1,2)
  Conductivity(1,3) = kk_gc(1,3)
  Conductivity(2,1) = kk_gc(2,1)
  Conductivity(2,2) = kk_gc(2,2)
  Conductivity(2,3) = kk_gc(2,3)
  Conductivity(3,1) = kk_gc(3,1)
  Conductivity(3,2) = kk_gc(3,2)
  Conductivity(3,3) = kk_gc(3,3)

  IF (visu) THEN
    PRINT 1, Jex,Jey,Jez
    1  FORMAT(' Jex: ', EN12.3,' Jey: ', EN12.3,' Jez: ', EN12.3)
    PRINT 2, Conductivity(1,1),Conductivity(1,2),Conductivity(1,3)
    2  FORMAT(' k_11: ', EN12.3,' k_12: ', EN12.3,' k_13: ', EN12.3)
    PRINT 3, Conductivity(2,1),Conductivity(2,2),Conductivity(2,3)
    3  FORMAT(' k_21: ', EN12.3,' k_22: ', EN12.3,' k_23: ', EN12.3)
    PRINT 4, Conductivity(3,1),Conductivity(3,2),Conductivity(3,3)
    4  FORMAT(' k_31: ', EN12.3,' k_32: ', EN12.3,' k_33: ', EN12.3)
  END IF

  CONTAINS
    !!! Thermal conductivity in uu direction:
    FUNCTION getk_11(arg_T) RESULT(k_11)
      IMPLICIT NONE
      REAL(KIND=dp) :: arg_T, k_11
      REAL(KIND=dp) :: a0, a1, a2, a3, a4, a5, a6, a7

      IF (arg_T < 30.0) THEN
        a0 = -741.8140267803362
        a1 = 869.4107979828448
        a2 = -245.28370175296817
        a3 = 33.35107558198684
        a4 = -2.3716994296593774
        a5 = 0.09226672308506112
        a6 = -0.0018692846640472706
        a7 = 1.545147766852096e-05
      ELSE
        a0 = 3146.0206839782554
        a1 = -90.37263023024096
        a2 = 1.1068277094065557
        a3 = -0.007015171938705109
        a4 = 2.387642426280633e-05
        a5 = -3.919811079281076e-08
        a6 = 1.56403854055945e-11
        a7 = 1.9874869900332475e-14
      END IF
      k_11 = a7*(arg_T**7)+a6*(arg_T**6)+a5*(arg_T**5)+a4*(arg_T**4)+a3*(arg_T**3)+a2*(arg_T**2)+a1*arg_T+a0
      IF (visu) THEN
        5  FORMAT(' k_uu: ', EN12.3)
        PRINT 5, k_11
      END IF
    END FUNCTION getk_11

    !!! Thermal conductivity in vv direction:
    FUNCTION getk_22(arg_T) RESULT(k_22)
      IMPLICIT NONE
      REAL(KIND=dp) :: arg_T, k_22
      REAL(KIND=dp) :: a0, a1, a2, a3, a4, a5, a6, a7

      IF (arg_T < 30.0) THEN
        a0 = -0.3883303161678264
        a1 = 0.4383674414035596
        a2 = -0.12739787992013332
        a3 = 0.017681047065171304
        a4 = -0.001276833483389979
        a5 = 5.06060978005742e-05
        a6 = -1.0442667466714241e-06
        a7 = 8.775300785342554e-09
      ELSE
        a0 = -0.6942880966732337
        a1 = 0.08320964196406444
        a2 = -0.00019072886854180467
        a3 = -1.7712313825376232e-06
        a4 = 2.1275204505207503e-08
        a5 = -9.926138106549589e-11
        a6 = 2.2203738969519415e-13
        a7 = -1.959020078703374e-16
      END IF
      k_22 = a7*(arg_T**7)+a6*(arg_T**6)+a5*(arg_T**5)+a4*(arg_T**4)+a3*(arg_T**3)+a2*(arg_T**2)+a1*arg_T+a0
      IF (visu) THEN
        6  FORMAT(' k_vv: ', EN12.3)
        PRINT 6, k_22
      END IF
    END FUNCTION getk_22

END SUBROUTINE getTherConductivity
