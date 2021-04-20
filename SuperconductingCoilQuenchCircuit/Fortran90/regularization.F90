SUBROUTINE getRegularizationSolver(Model, Solver)
  !!! Frederic Trillaud <ftrillaudp@gmail.com> - July 29, 2020
  !!!celmerf90 -o regularization.so regularization.F90
  !!! Smooth filter to avoid temperatures lower than the minimum temperature defined in the solver section

  ! Elmer module
  USE DefUtils

  IMPLICIT NONE
  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model
  TYPE(Variable_t), POINTER :: TempVar
  INTEGER, POINTER :: TempPerm(:), NodeIndexes
  REAL(KIND=dp), POINTER :: Temperature(:)
  INTEGER :: ElementNo, N, idn
  REAL(KIND=dp), ALLOCATABLE :: localTemp(:)
  TYPE(Element_t), POINTER :: Element
  TYPE(ValueList_t), POINTER :: params
  REAL(KIND=dp) :: Tmin
  LOGICAL :: gotIt, visu
  ALLOCATE(localTemp(CurrentModel % MaxElementNodes))

  !!! get parameters from sif file:
  params => GetSolverParams()
  IF (.NOT. ASSOCIATED(params)) THEN
    CALL Fatal('getRegularizationSolver', 'No Parameter found')
  END IF
  Tmin = GetConstReal( params, 'Minimum Temperature', gotIt)
  IF (.NOT. gotIt) THEN
    CALL Fatal('getRegularizationSolver', 'Minimum Temperature')
  END IF
  
  visu = .FALSE.
  IF (visu) THEN
    PRINT 1, Tmin
    1  FORMAT(' T_min (K): ', EN12.3)
  END IF

  !!! Get the temperature field:
  TempVar => VariableGet( Solver % Mesh % Variables, 'Temperature' )
  IF ( ASSOCIATED( TempVar) ) THEN
    TempPerm => TempVar % Perm
    Temperature => TempVar % Values
    ! stop if temperature field has not been found !!!!
  ELSE
    CALL Fatal('regularizationSolver', 'No variable Temperature found')
  END IF

  !!! Smooth filter to avoid temperatures below the minimum temperature:
  DO ElementNo = 1,Solver % NumberOfActiveElements
    Element => GetActiveElement(ElementNo)
    N = GetElementNOFNodes()
    localTemp(1:N) = Temperature(TempPerm(Element % NodeIndexes))
    DO idn = 1, N
      IF (localTemp(idn) < Tmin) THEN
        localTemp(idn) = Tmin
      END IF
    END DO
    ! how to reassign in the element the temperature at the node according to the node index. Just doing the way arround seems to work rather well
    Temperature(TempPerm(Element % NodeIndexes)) = localTemp(1:N)
  END DO

END SUBROUTINE getRegularizationSolver
