FUNCTION getDisturbance(Model, n, tt) RESULT(Q)
  ! Frederic Trillaud <ftrillaudp@gmail.com> - July 4, 2020
  !elmerf90 -o disturbance.so disturbance.f90

  ! Elmer module
  USE DefUtils

  IMPLICIT NONE
  TYPE(Model_t) :: model
  INTEGER :: n
  REAL(KIND=dp) :: tt
  REAL(KIND=dp) :: X, Y, Z, t_ini_d, Dt_d, Qd, Q
  REAL(KIND=dp) :: R_d, x_d, y_d, z_d, d
  LOGICAL :: gotIt

  ! Parameters needed inside the function
  TYPE(ValueList_t), POINTER :: bfList
  bfList => getbodyforce()
  ! Value of the dissipation in the disturbance (W/kg)
  Qd = ListGetConstReal(bfList, 'Qd', gotIt)
  IF ( .NOT. gotIt ) THEN
    CALL Warn('getDisturbance', 'Qd not found')
  END IF
  ! Initial time of the durantion
  t_ini_d = ListGetConstReal(bfList, 't_ini_d', gotIt)
  IF ( .NOT. gotIt ) THEN
    CALL Warn('getDisturbance', 't_ini_d not found')
  END IF
  ! Time duration of the disturbance
  Dt_d = ListGetConstReal(bfList, 'Dt_d', gotIt)
  IF ( .NOT. gotIt ) THEN
    CALL Warn('getDisturbance', 'Delta_t not found')
  END IF
  ! Position of the center of the disturbance
  x_d = ListGetConstReal(bfList, 'x_d', gotIt)
  IF ( .NOT. gotIt ) THEN
    CALL Warn('getDisturbance', 'x_d not found')
  END IF
  y_d = ListGetConstReal(bfList, 'y_d', gotIt)
  IF ( .NOT. gotIt ) THEN
    CALL Warn('getDisturbance', 'y_d not found')
  END IF
  z_d = ListGetConstReal(bfList, 'z_d', gotIt)
  IF ( .NOT. gotIt ) THEN
    CALL Warn('getDisturbance', 'z_d not found')
  END IF
  ! Dimension of the spherical disturbance
  ! of the order of the length of geometry over number of elements
  R_d = ListGetConstReal(bfList, 'R_d', gotIt)
  IF ( .NOT. gotIt ) THEN
    CALL Warn('getDisturbance', 'R_d not found')
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

END FUNCTION getDisturbance
