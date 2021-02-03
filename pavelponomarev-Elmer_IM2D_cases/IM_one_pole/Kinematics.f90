
! This solver dedicated for single kinematic equation
! Jr * dw/dt = Te - Tload
! This functionality might be included in future to MagnetDynamics.F90 module
! using "group 1 torque" by default

SUBROUTINE Kinematics_init( Model,Solver,dt,TransientSimulation )
!------------------------------------------------------------------------------
  USE DefUtils
  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver       !< Linear & nonlinear equation solver options
  TYPE(Model_t) :: Model         !< All model information (mesh, materials, BCs, etc...)
  REAL(KIND=dp) :: dt            !< Timestep size for time dependent simulations
  LOGICAL :: TransientSimulation !< Steady state or transient simulation
!------------------------------------------------------------------------------
  TYPE(ValueList_t), POINTER :: Params

  Params => Solver % Values

  ! When we introduce the variables in this way the variables are created
  ! so that they exist when the proper simulation cycle starts.
  ! This also keeps the command file cleaner.
  CALL ListAddString( Params,'Exported Variable 1', '-global Rotor Angle')
  CALL ListAddString( Params,'Exported Variable 2', '-global Rotor Velo')
  Solver % Values => Params

!------------------------------------------------------------------------------
END SUBROUTINE Kinematics_init
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
SUBROUTINE Kinematics( Model,Solver,dt,TransientSimulation )
!------------------------------------------------------------------------------
  USE DefUtils
  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver       !< Linear & nonlinear equation solver options
  TYPE(Model_t) :: Model         !< All model information (mesh, materials, BCs, etc...)
  REAL(KIND=dp) :: dt            !< Timestep size for time dependent simulations
  LOGICAL :: TransientSimulation !< Steady state or transient simulation
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
  LOGICAL :: Found, First=.TRUE.

  TYPE(Variable_t), POINTER, SAVE :: AngVar, VeloVar
  TYPE(Variable_t), POINTER :: TimeVar
  Real(KIND=dp) :: Time

  REAL(KIND=dp):: ang0=0._dp, velo0=0._dp
  REAL(KIND=dp), TARGET :: torq,imom=0._dp,tscale=1._dp,Tload=0._dp,ang=0._dp,velo=0._dp

  integer :: i
!------------------------------------------------------------------------------

  IF (First) THEN
    First = .FALSE.
    
    AngVar => DefaultVariableGet( 'Rotor Angle' )
    ! Variable should alreadt exist as it was introduced in the _init section.
    IF(.NOT. ASSOCIATED( AngVar ) ) THEN
      CALL Fatal('Kinematics','Variable > Rotor Angle < does not exist!')
    END IF
    ang = AngVar % Values(1)

    VeloVar => DefaultVariableGet( 'Rotor Velo' )
    IF(.NOT. ASSOCIATED( VeloVar ) ) THEN
      CALL Fatal('Kinematics','Variable > Rotor Velo < does not exist!')
    END IF
    velo = VeloVar % Values(1)

    ! Read parameters from the SIF's Simulation section
    imom = GetConstReal( GetSimulation(),'Jr') ! interatial moment of the rotor
    tscale = GetConstReal( GetSimulation(),'Torque scaling factor') !
  END IF

  ! Save values from the previous timestep
  ang0 = AngVar % Values(1)
  velo0 = VeloVar % Values(1)

  ! Here loading can be changed
  TimeVar => VariableGet (Solver % Mesh % Variables, 'Time')
  Time = TimeVar % Values(1)
  IF (Time > 0.5) THEN 
    Tload = 32.0
  END IF

  ! Time integration for the angular momentum equation
  !---------------------------------------------------
  torq = GetConstReal( GetSimulation(),'res: group 1 torque', Found)

  ! Here is the main kinematic equation
  ! It can be augumented by friction and additional loads as required
  IF(imom /= 0) THEN
    velo = velo0 + dt * (Tscale*torq-Tload) / imom
    ang  = ang0  + dt * velo
  END IF

  ! Export Kinematics variables for "SaveScalars":
  ! -----------------------------------------------------
  CALL ListAddConstReal(GetSimulation(),'res: time', GetTime())
  CALL ListAddConstReal(GetSimulation(),'res: Angle(rad)', ang)
  CALL ListAddConstReal(GetSimulation(),'res: Speed(rpm)', velo/(2._dp*pi)*60)

  !update global variables
  VeloVar % Values(1) = velo
  AngVar % Values(1) = ang
!------------------------------------------------------------------------------
END SUBROUTINE Kinematics
!------------------------------------------------------------------------------
