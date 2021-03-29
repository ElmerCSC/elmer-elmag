SUBROUTINE getTherConductivity(Model, n, arg, Conductivity)
  ! Frederic Trillaud <ftrillaudp@gmail.com> - July 12, 2020
  !elmerf90 -o therConductivity.so therConductivity.F90

  ! Elmer module
  USE DefUtils

  IMPLICIT NONE
  TYPE(Model_t) :: model
  INTEGER :: n
  REAL(KIND=dp) :: arg(*)
  REAL(KIND=dp) :: T, Jex, Jey, Je, Jez, Bx, By, Bz, B
  REAL(KIND=dp) :: k_11, k_22
  REAL(KIND=dp), POINTER ::  Conductivity(:,:)
  REAL(KIND=dp), DIMENSION(3,3) :: kk_lc, kk_gc, Q

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

  ! Transformation matrix
  Q = 0.0D00
  Q(1,1) = Jex/Je
  Q(1,2) = -Jey/Je
  Q(2,1) = Jey/Je
  Q(2,2) = Jex/Je
  Q(3,1) = Jez/Je
  Q(3,3) = 1.0

  ! Tensor of thermal conductivity in local coordinate system
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

  PRINT 1, Jex,Jey,Jez
  1  FORMAT(' Jex: ', EN12.3,' Jey: ', EN12.3,' Jez: ', EN12.3)
  PRINT 2, Conductivity(1,1),Conductivity(1,2),Conductivity(1,3)
  2  FORMAT(' k_11: ', EN12.3,' k_12: ', EN12.3,' k_13: ', EN12.3)
  PRINT 3, Conductivity(2,1),Conductivity(2,2),Conductivity(2,3)
  3  FORMAT(' k_21: ', EN12.3,' k_22: ', EN12.3,' k_23: ', EN12.3)
  PRINT 4, Conductivity(3,1),Conductivity(3,2),Conductivity(3,3)
  4  FORMAT(' k_31: ', EN12.3,' k_32: ', EN12.3,' k_33: ', EN12.3)

  CONTAINS
    FUNCTION getk_11(arg_T) RESULT(k_11)
      IMPLICIT NONE
      REAL(KIND=dp) :: arg_T, k_11
      k_11 = 1000.0
    END FUNCTION getk_11
    
    FUNCTION getk_22(arg_T) RESULT(k_22)
      IMPLICIT NONE
      REAL(KIND=dp) :: arg_T, k_22
      k_22 = 0.1
    END FUNCTION getk_22

END SUBROUTINE getTherConductivity
