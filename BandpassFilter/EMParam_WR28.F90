FUNCTION omega( model, n, dummyArgument ) RESULT(omg)
  ! modules needed
  USE DefUtils
  IMPLICIT None
  ! variables in function header
  TYPE(Model_t) :: model
  INTEGER :: n, stp
  REAL(KIND=dp) :: dummyArgument, x, y, z
  REAL(KIND=dp) :: te, fa, fe, a, b, c0, kc, omg, k0, bt0

  TYPE(Variable_t), POINTER :: TimeVar
  Real(KIND=dp) :: Time

  !x = model % Nodes % x(n)
  !y = model % Nodes % y(n)
  !z = model % Nodes % z(n)
  time = GetTime()
  !stp = GetTimeStep()

  te = 151
  fa = 35.0D09
  fe = 38.0D09
  a= 25.4D-03*0.28
  b=a/2
  c0=1/sqrt(8.854D-12*4.0*pi*1D-7)
  kc=pi/a

  omg = 2.0*pi*((fe-fa)/(te-1.0)*(time-1.0)+fa)

END FUNCTION omega

FUNCTION betaNull( model, n, dummyArgument ) RESULT(bt0)
  ! modules needed
  USE DefUtils
  IMPLICIT None
  ! variables in function header
  TYPE(Model_t) :: model
  INTEGER :: n, stp
  REAL(KIND=dp) :: dummyArgument, x, y, z
  REAL(KIND=dp) :: te, fa, fe, a, b, c0, kc, omg, k0, bt0, omega

  TYPE(Variable_t), POINTER :: TimeVar
  Real(KIND=dp) :: Time

  !x = model % Nodes % x(n)
  !y = model % Nodes % y(n)
  !z = model % Nodes % z(n)
  time = GetTime()
  !stp = GetTimeStep()

  te = 151
  fa = 35.0D09
  fe = 38.0D09
  a= 25.4D-03*0.28
  b=a/2
  c0=1/sqrt(8.854D-12*4.0*pi*1D-7)
  kc=pi/a

  omg = omega(model, n, dummyArgument)

  omg = 2.0*pi*((fe-fa)/(te-1.0)*(time-1.0)+fa)
  k0 = omg/c0
  bt0 = sqrt(k0**2.0-kc**2.0)

END FUNCTION betaNull

FUNCTION MagnBndLoad( model, n, dummyArgument ) RESULT(mbl)

  USE DefUtils
  IMPLICIT None
  TYPE(Model_t) :: model
  INTEGER :: n, stp
  REAL(KIND=dp) :: dummyArgument, x, y, z
  REAL(KIND=dp) :: te, fa, fe, a, b, c0, kc, omg, k0, bt0, mbl
  
  TYPE(Variable_t), POINTER :: TimeVar
  Real(KIND=dp) :: Time
  
  x = model % Nodes % x(n)
  !y = model % Nodes % y(n)
  !z = model % Nodes % z(n)
  time = GetTime()
  !stp = GetTimeStep()

  te = 151
  fa = 35.0D09
  fe = 38.0D09
  a= 25.4D-03*0.28
  b=a/2
  c0=1/sqrt(8.854D-12*4.0*pi*1D-7)
  kc=pi/a

  omg = 2.0*pi*((fe-fa)/(te-1.0)*(time-1.0)+fa)
  k0 = omg/c0
  bt0 = sqrt(k0**2.0-kc**2.0)
  mbl = -2.0*bt0*k0/kc*sin(kc*(x+a/2.0))

END FUNCTION MagnBndLoad
