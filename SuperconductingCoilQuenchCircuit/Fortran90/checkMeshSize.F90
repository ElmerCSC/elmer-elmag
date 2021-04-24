SUBROUTINE getCheckMeshSizeSolver(Model)
  !!! Frederic Trillaud <ftrillaudp@gmail.com> - April 18, 2021
  !!! elmerf90 -o checkMeshSize.so checkMeshSize.F90
  !!! Check if there is at least one node inside the volume of the heat disturbance

  ! Elmer module
  USE DefUtils

  IMPLICIT NONE
  TYPE(Model_t) :: model
  INTEGER :: i, numberOfNodes, nof_d, dist
  LOGICAL :: gotIt, visu
  REAL(KIND=dp) :: X, Y, Z, R_d, x_d, y_d, z_d, d

  ! Parameters needed inside the function
  TYPE(ValueList_t), POINTER :: bfList
  bfList => getbodyforce()
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
  ! Dimension of the spherical disturbance. It should be of the order of the length of geometry over number of elements
  R_d = ListGetConstReal(bfList, 'Disturbance Size', gotIt)
  IF ( .NOT. gotIt ) THEN
    CALL Warn('getDisturbance', 'Disturbance Size not found')
  END IF

  visu = .TRUE.

  IF (GetTimestep() == 1) THEN ! to make sure, it runs only once
    !!! Cycle over the nodes
    nof_d = 0 ! counter for number of nodes inside disturbance volume
    dist = 0
    DO i = 1, model % numberOfNodes
      ! get local coordinates of the nodes
      X = model % Nodes % x(i)
      Y = model % Nodes % y(i)
      Z = model % Nodes % y(i)
      ! Compute the distance from the center of the heat disturbance and the nodes
      d = SQRT((X-x_d)**2+(Y-y_d)**2+(Z-z_d)**2)
      ! Need to check if the mesh is fine enough
      IF ((R_d - d) >= 0.0) THEN
        dist = 1
      ELSE
        dist = 0
      END IF
      nof_d = nof_d + dist
      IF (visu) THEN
        PRINT *, ' nof_d = ', nof_d, ', R_d-d = ', (R_d - d)
      END IF
    END DO
  END IF

  !!! If there is no node inside the disturbance volume, we stop
  IF (nof_d >= 1) THEN
    PRINT *, '**** Proper size of mesh or heat disturbance ***'
    PRINT *, ' number of nodes: ', nof_d
  ELSE
    PRINT *, '*** WARNING: program stopped. ***'
    PRINT *, ' No nodes could be found inside the volume of the heat disturbance.'
    PRINT *, ' Solution: Increase the mesh fineness or the radius of the heat disturbance.'
    !STOP
  END IF

END SUBROUTINE getCheckMeshSizeSolver
