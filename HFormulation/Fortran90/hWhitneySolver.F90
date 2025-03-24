!> \ingroup Solvers (grouping for docxygen)
!Frederic Trillaud <ftrillaudp@gmail.com>
!August 2022
!Modified from "Elctromagnetic Waves" solver (EMWaveSolver)
!-------------------------------------------------------------------------------
MODULE HWhitneySolverUtils

   USE DefUtils
   IMPLICIT NONE

CONTAINS

END MODULE HWhitneySolverUtils


!> \ingroup Solvers
!---------------------------------------------------------------------------
SUBROUTINE HWhitneySolver_Init0( Model, Solver, dt, Transient )
!------------------------------------------------------------------------------
! Initialization: one can define specific elements, specific fields and results
  USE HWhitneySolverUtils

  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model
  REAL(KIND=dp) :: dt
  LOGICAL :: Transient
!------------------------------------------------------------------------------
  TYPE(ValueList_t), POINTER :: SolverParams
  LOGICAL :: Found, SerhoOrder, PiolaVersion
  REAL(KIND=dp) :: mu0
  INTEGER :: mat_id
  TYPE(ValueList_t), POINTER  :: List

! Get the parameters defined in the solver section of the sif file
  SolverParams => GetSolverParams()
  ! "Element" where is defined the type of elements
  ! By default use Piola if serho order elements. Check if there is an entry "Element" in the solver section
  IF ( .NOT.ListCheckPresent(SolverParams, "Element") ) THEN
    SerhoOrder = GetLogical( SolverParams, 'Quadratic Approximation', Found )
    IF( SerhoOrder ) THEN
      PiolaVersion = .TRUE.
    ELSE
      PiolaVersion = GetLogical(SolverParams, 'Use Piola Transform', Found )
    END IF
    IF( SerhoOrder ) THEN
      CALL ListAddString( SolverParams, "Element", &
          "n:0 e:2 -tri b:2 -quad b:4 -brick b:6 -pyramid b:3 -prism b:2 -quad_face b:4 -tri_face b:2" )
      ! CALL ListAddString( SolverParams, "Element", &
      !     "n:1 e:2 -tri b:2 -quad b:4 -brick b:6 -pyramid b:3 -prism b:2 -quad_face b:4 -tri_face b:2" )
    ELSE IF (PiolaVersion) THEN
      CALL ListAddString( SolverParams, "Element", "n:0 e:1 -quad b:2 -brick b:3 -quad_face b:2" )
      ! CALL ListAddString( SolverParams, "Element", "n:1 e:1 -quad b:2 -brick b:3 -quad_face b:2" )
    ELSE
    ! Basic edge elements if nothing else
      CALL ListAddString( SolverParams, "Element", "n:0 e:1" )
      ! adding the nodal elements n:1 instead of n:0
      ! CALL ListAddString( SolverParams, "Element", "n:1 e:1" )
    END IF
  END IF


  ! Use by some solvers e.g. SaveLine to acknowledge H as edge field
  CALL ListAddNewLogical( SolverParams, 'Hcurl Basis', .TRUE. )
  IF( ListGetLogical( SolverParams, 'Constant Bulk Matrix', Found ) ) THEN
    CALL ListAddNewLogical( SolverParams, 'Use Global Mass Matrix', .TRUE. )
  END IF

  CALL ListAddNewLogical( SolverParams, 'Variable Output', .FALSE. )
  CALL ListAddNewString( SolverParams, 'Variable', 'H')
  CALL ListAddNewLogical( SolverParams, 'Linear System Complex', .FALSE. )

  ! Set a multiplier for the relative keywords
  !--------------------------------------------------------------------
  mu0 = GetConstReal( Model % Constants, 'Permeability of Vacuum', Found )
  IF(.NOT. Found ) CALL Fatal( 'HWhitneySolver_Init0', '> Permeability of Vacuum < is required' )

!------------------------------------------------------------------------------
END SUBROUTINE HWhitneySolver_Init0
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!> Solve the magnetic field H from the curl-curl equation
!> curl rho * curl H + d/dt (mu * H) = 0
!> using edge elements (curl-conforming vector finite elements at most degree 2)
!MASS * dHdt + STIFF * H = FORCE, MASS associated to the derivative, if no dumping, falls back to the first derivative
!> \ingroup Solvers
!------------------------------------------------------------------------------
SUBROUTINE HWhitneySolver( Model, Solver, dt, Transient )
!------------------------------------------------------------------------------
  USE HWhitneySolverUtils
  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t), TARGET :: Solver
  TYPE(Model_t) :: Model
  REAL(KIND=dp) :: dt
  LOGICAL :: Transient
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
  LOGICAL :: AllocationsDone = .FALSE., Found
  TYPE(Element_t),POINTER :: Element
  TYPE(ValueList_t), POINTER :: BC
  INTEGER :: n,istat,i,nNodes,Active,dofs
  INTEGER :: NoIterationsMax
  TYPE(Mesh_t), POINTER :: Mesh
  REAL(KIND=dp) :: Norm
  REAL(KIND=dp), ALLOCATABLE :: STIFF(:,:), MASS(:,:), FORCE(:)
  LOGICAL :: PiolaVersion, SerhoOrder, EdgeBasis
  INTEGER, POINTER :: Perm(:)
  TYPE(ValueList_t), POINTER :: SolverParams
  REAL(KIND=dp) :: mu0
  TYPE(Solver_t), POINTER :: pSolver

  SAVE STIFF, MASS, FORCE, AllocationsDone
!------------------------------------------------------------------------------

  CALL Info( 'HWhitneySolver', 'Solving H field in time', Level=5 )

  SolverParams => GetSolverParams()

  SerhoOrder = GetLogical( SolverParams, 'Quadratic Approximation', Found )
  IF( SerhoOrder ) THEN
    PiolaVersion = .TRUE.
  ELSE
    PiolaVersion = GetLogical( SolverParams, 'Use Piola Transform', Found )
  END IF

  IF (CoordinateSystemDimension() == 2) THEN
    IF (.NOT. PiolaVersion) &
        CALL Fatal( 'HWhitneySolver', 'A 2D model needs Use Piola Transform = True' )
  END IF

! Get the unknowns or degrees of freedom associated with the variable H
  dofs = Solver % Variable % Dofs

  mu0 = GetConstReal( Model % Constants,  'Permeability of Vacuum' )

  ! Allocate some permanent storage, this is done first time only:
  !---------------------------------------------------------------
  Mesh => GetMesh()
  nNodes = Mesh % NumberOfNodes
  ! The pointer => is used to reuse the "Solver % Variable % Perm" variable anywhere as a local variable would do
  Perm => Solver % Variable % Perm
  pSolver => Solver

  IF ( .NOT. AllocationsDone ) THEN
    IF( dofs /= 1 ) CALL Fatal ( 'HWhitneySolver', 'Invalid variable size:'//TRIM(I2S(dofs)) )
    n = Mesh % MaxElementDOFs
    ALLOCATE( FORCE(n), STIFF(n,n), MASS(n,n), STAT=istat )
    IF ( istat /= 0 ) CALL Fatal( 'HWhitneySolver', 'Memory allocation error.' )
    AllocationsDone = .TRUE.
  END IF

  ! Resolve internal non.linearities, if requested:
  ! ----------------------------------------------
  NoIterationsMax = GetInteger( SolverParams, 'Nonlinear System Max Iterations', Found )
  IF(.NOT. Found) NoIterationsMax = 1

  EdgeBasis = .NOT. ListCheckPresent( SolverParams, 'Linear System Refactorize' ) .AND. &
      GetLogical( SolverParams, 'Edge Basis', Found )

  ! Initiliazation of the resolution
  CALL DefaultStart()

  ! Loop over the solver resolution to perform the non linear resolution: 'Nonlinear System Max Iterations'
  DO i=1,NoIterationsMax
    ! Do the matrix assembly for the volume
    CALL DoBulkAssembly()
    ! Integrate the boundary rhoitions in the assembly
    CALL DoBoundaryAssembly()

    ! Default routines for finishing assembly and solving the system
    Norm = DefaultSolve()
    IF( DefaultConverged() ) EXIT

    IF( EdgeBasis ) CALL ListAddLogical( SolverParams, 'Linear System Refactorize', .FALSE. )
  END DO
  IF ( EdgeBasis ) CALL ListRemove( SolverParams, 'Linear System Refactorize' )

! Finish to solve
  CALL DefaultFinish()

  CALL Info( 'HWhitneySolver', 'All done', Level=10 )


CONTAINS

!---------------------------------------------------------------------------------------------
  SUBROUTINE DoBulkAssembly()
!---------------------------------------------------------------------------------------------
    INTEGER :: n,nd,t
    LOGICAL :: Found, ConstantBulkInUse = .FALSE.
!---------------------------------------------------------------------------------------------

    ! use matrix from previous round
    !-----------------------------------------------
    IF( ConstantBulkInUse ) THEN
      CALL DefaultInitialize( UseConstantBulk = ConstantBulkInUse )
      RETURN
    END IF

    CALL DefaultInitialize()

    ! Get the active elements in the volume
    Active = GetNOFActive()
    DO t=1,active
     ! write the weak formulation on element basis
      Element => GetActiveElement(t)
      n  = GetElementNOFNodes() ! nodes
      nd = GetElementNOFDOFs()  ! dofs

      ! Get element local matrix and rhs vector:
      !----------------------------------------
      CALL LocalMatrix( MASS, STIFF, FORCE, Element, n, nd, PiolaVersion, t==1 )

      ! Update global matrix and rhs vector from local matrix & vector:
      !---------------------------------------------------------------
      CALL Default1stOrderTime( MASS, STIFF, FORCE )
      CALL DefaultUpdateEquations( STIFF, FORCE )
    END DO

    CALL DefaultFinishBulkAssembly()

    ConstantBulkInUse = ListGetLogical( SolverParams, 'Constant Bulk Matrix', Found )

!------------------------------------------------------------------------------
  END SUBROUTINE DoBulkAssembly
!------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------
  SUBROUTINE DoBoundaryAssembly()
!---------------------------------------------------------------------------------------------
    INTEGER :: n,nd,t,k
    LOGICAL :: Found, InitHandles
!---------------------------------------------------------------------------------------------
    TYPE(element_t), POINTER :: dummy_element

    InitHandles = .TRUE.

    ! Dirichlet BC in terms of H:
    !--------------------------------
    Active = GetNOFBoundaryElements()
    DO t=1,Active
      Element => GetBoundaryElement(t)
      BC => GetBC()
      IF (.NOT. ASSOCIATED(BC) ) CYCLE

      SELECT CASE(GetElementFamily())
      CASE(1)
        CYCLE
      CASE(2)
        k = GetBoundaryEdgeIndex(Element,1); Element => Mesh % Edges(k)
      CASE(3,4)
        k = GetBoundaryFaceIndex(Element)  ; Element => Mesh % Faces(k)
      END SELECT
      IF (.NOT. ActiveBoundaryElement(Element)) CYCLE

      nd = GetElementNOFDOFs(Element)
      n  = GetElementNOFNodes(Element)

      dummy_element => SetCurrentElement(Element)

      ! Only edge n:0 e:1
      CALL Default1stOrderTimeR( MASS, STIFF, FORCE(1:nd), UElement=Element )
      CALL DefaultUpdateEquationsR( STIFF, FORCE(1:nd), UElement=Element )

      InitHandles = .FALSE.
    END DO

    CALL DefaultFinishBoundaryAssembly()
    CALL DefaultFinishAssembly()
    ! Get the Dirichlet condition, taking the field H and imposing the conditions in the Boundary condition section of the sif file
    CALL DefaultDirichletBCs()

!------------------------------------------------------------------------------
  END SUBROUTINE DoBoundaryAssembly
!------------------------------------------------------------------------------



!-----------------------------------------------------------------------------
  SUBROUTINE LocalMatrix( MASS, STIFF, FORCE, Element, n, nd, PiolaVersion, InitHandles )
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: STIFF(:,:), FORCE(:), MASS(:,:)
    INTEGER :: n, nd
    TYPE(Element_t), POINTER :: Element
    LOGICAL :: PiolaVersion, InitHandles
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: DetJ, weight, mu, muinv, rho
    REAL(KIND=dp) :: Basis(n),dBasisdx(n,3),WBasis(nd,3),RotWBasis(nd,3)
    LOGICAL :: Stat
    INTEGER :: t, i, j, p, q
    TYPE(GaussIntegrationPoints_t) :: IP
    TYPE(Nodes_t), SAVE :: Nodes
    TYPE(ValueHandle_t) :: Mu_h, rho_h
    LOGICAL :: AllocationsDone = .FALSE.

    SAVE Mu_h, rho_h, AllocationsDone

    IF( InitHandles ) THEN
      ! These have been normalized by mu0 and eps0 in _init section
      CALL ListInitElementKeyword( Mu_h, 'Material', 'Relative Permeability')
      CALL ListInitElementKeyword( rho_h, 'Material', 'Electric Resistivity')
    END IF

    CALL GetElementNodes( Nodes, Element )

    STIFF = 0.0_dp
    FORCE = 0.0_dp
    MASS  = 0.0_dp

    ! Numerical integration:
    !----------------------
    ! PiolaVersion are complicated elements but they give good results
    IP = GaussPoints( Element, EdgeBasis = .TRUE., PReferenceElement = PiolaVersion )


    DO t=1,IP % n
	  ! detJ -> determinant of the matrix jacobian of transformation (change in space coordinates)
	  ! Basis: basis functions and dBasisdx, their spatial derivatives
      stat = ElementInfo(Element,Nodes,IP % u(t), IP % v(t), IP % w(t),detJ,Basis,dBasisdx, &
          EdgeBasis = Wbasis, RotBasis = RotWBasis, USolver = pSolver )

	  ! it is the weight of the element meaning its actual size (big == a lot of weights),
	  ! the gauss points convoluted with the change of corrdinates
      weight = detJ * (IP % s(t))

      mu = mu0 * ListGetElementReal( mu_h, Basis, Element, Found )
      rho = ListGetElementReal( rho_h, Basis, Element, Found )

      ! Nodes: 1 to n and edges from n+1 to nd or (nd-n to nd)
      DO p = 1,nd
        DO q = 1,nd
          ! the term: rho * curl H . curl v
          STIFF(p,q) = STIFF(p,q) + rho * SUM(RotWBasis(p,:) * RotWBasis(q,:)) * weight
          ! the term: d(mu * H)/dt .v for conducting materials
          MASS(p,q) = MASS(p,q) + mu * SUM(WBasis(p,:) * WBasis(q,:)) * weight
        END DO
      END DO
    END DO

!------------------------------------------------------------------------------
  END SUBROUTINE LocalMatrix
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE HWhitneySolver
!------------------------------------------------------------------------------

!> \ingroup Solvers
!------------------------------------------------------------------------------
SUBROUTINE HWhitneyCalcFields_Init0( Model, Solver, dt, Transient )
!------------------------------------------------------------------------------
  USE HWhitneySolverUtils

  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t), TARGET :: Solver
  TYPE(Model_t) :: Model

  REAL(KIND=dp) :: dt
  LOGICAL :: Transient
!------------------------------------------------------------------------------
  CHARACTER(LEN=MAX_NAME_LEN) :: sname,pname
  LOGICAL :: Found, ElementalFields
  INTEGER, POINTER :: Active(:)
  INTEGER :: mysolver,i,j,k,n,m,vDOFs,soln
  TYPE(ValueList_t), POINTER :: SolverParams
  TYPE(Solver_t), POINTER :: Solvers(:), PSolver

  SolverParams => GetSolverParams()

  ! Find the solver index of the primary solver by the known procedure name.
  ! (the solver is defined here in the same module so not that dirty...)
  soln = 0

  DO i=1,Model % NumberOfSolvers
    sname = GetString( Model % Solvers(i) % Values, 'Procedure', Found )
    j = INDEX( sname, 'HWhitneySolver' )
    IF( j > 0 ) THEN
      soln = i
      EXIT
    END IF
  END DO

  IF( soln == 0 ) THEN
    CALL Fatal( 'HWhitneyCalcFields_Init0', 'Cannot locate the primary solver: '//TRIM(I2S(soln)) )
  ELSE
    CALL Info( 'HWhitneyCalcFields_Init0', 'The primary solver index is: '//TRIM(I2S(soln)), Level=12 )
    CALL ListAddInteger( SolverParams, 'Primary Solver Index', soln )
  END IF

  ! In case we are solving truly discontinuous Galerkin fields then we do it by assembling
  ! normal linear system. Here we allocate for the DG type of fields that are computed elementwise
  ! while the FE fields are solved using standard Galerkin. Hence unintuitively we exit here
  ! for elemental fields if the primary field is itself elemental.
  !-----------------------------------------------------------------------------------------------
  IF( GetLogical( SolverParams,'Discontinuous Galerkin',Found) ) RETURN

  ElementalFields = GetLogical( SolverParams, 'Calculate Elemental Fields', Found )
  IF(Found .AND. .NOT. ElementalFields) RETURN

  PSolver => Solver
  DO mysolver=1,Model % NumberOfSolvers
    IF ( ASSOCIATED(PSolver,Model % Solvers(mysolver)) ) EXIT
  END DO

  ! Here we add a DG solver instance in a dirty way by extending the list of solvers
  ! and adding the new solver as an active one.
  n = Model % NumberOfSolvers
  DO i=1,Model % NumberOFEquations
    Active => ListGetIntegerArray( Model % Equations(i) % Values, 'Active Solvers', Found )
    m = SIZE(Active)
    IF ( ANY(Active==mysolver) ) &
        CALL ListAddIntegerArray( Model % Equations(i) % Values, 'Active Solvers', m+1, [Active, n+1] )
  END DO

  ALLOCATE(Solvers(n+1))
  Solvers(1:n) = Model % Solvers
  SolverParams => NULL()
  CALL ListAddLogical( SolverParams, 'Discontinuous Galerkin', .TRUE. )
  Solvers(n+1) % DG = .TRUE.
  Solvers(n+1) % Values => SolverParams
  Solvers(n+1) % PROCEDURE = 0
  Solvers(n+1) % ActiveElements => NULL()
  CALL ListAddString( SolverParams, 'Exec Solver', 'never' )
  CALL ListAddLogical( SolverParams, 'No Matrix', .TRUE. )
  CALL ListAddString( SolverParams, 'Equation', 'never' )
  CALL ListAddString( SolverParams, 'Procedure', 'AllocateSolver AllocateSolver', .FALSE. )
  CALL ListAddString( SolverParams, 'Variable', '-nooutput cf_dummy' )

  pname = ListGetString( Model % Solvers(soln) % Values, 'Mesh', Found )
  IF(Found) THEN
    CALL ListAddString( SolverParams, 'Mesh', pname )
  END IF

  ! Electric field is always computed
  CALL ListAddString( SolverParams, NextFreeKeyword('Exported Variable', SolverParams), "Hfield E[Hfield E:3]" );

  ! When requested we may also compute the 1st time derivative.
  ! They exist by default as whitney fields but also needs to be projected to nodal fields.
  !------------------------------------------------------------------------------
  IF (GetLogical( SolverParams, 'Calculate Time Derivatives',Found) ) THEN
    CALL ListAddString( SolverParams, NextFreeKeyword('Exported Variable', SolverParams), "dHdt E[dHdt E:3]" );
  END IF
  
  IF (GetLogical( SolverParams, 'Calculate Current density', Found) ) THEN
    CALL ListAddString( SolverParams, NextFreeKeyword('Exported Variable', SolverParams), "dHdx E[dHdx E:3]" );
  END IF

  DEALLOCATE(Model % Solvers)
  Model % Solvers => Solvers
  Model % NumberOfSolvers = n+1
!------------------------------------------------------------------------------
END SUBROUTINE HWhitneyCalcFields_Init0
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!> \ingroup Solvers
!------------------------------------------------------------------------------
SUBROUTINE HWhitneyCalcFields_Init( Model, Solver, dt, Transient )
!------------------------------------------------------------------------------
  USE HWhitneySolverUtils

  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model
  REAL(KIND=dp) :: dt
  LOGICAL :: Transient
!------------------------------------------------------------------------------
  LOGICAL :: Found, NodalFields
  TYPE(ValueList_t), POINTER :: SolverParams

  SolverParams => GetSolverParams()

  ! We compute the fields one component at a time.
  ! This is the dummy variable used for the computation. It is not saved.
  CALL ListAddString( SolverParams, 'Variable', '-nooutput hr_dummy' )

  ! The matrix is constant hence do not ever refactorize.
  CALL ListAddLogical( SolverParams, 'Linear System refactorize', .FALSE. )

  NodalFields = GetLogical( SolverParams, 'Calculate Nodal Fields', Found )
  IF(Found .AND. .NOT. NodalFields ) RETURN

  CALL ListAddString( SolverParams, NextFreeKeyword( 'Exported Variable', SolverParams), "Hfield[Hfield:3]" );

  IF (GetLogical( SolverParams, 'Calculate Time Derivatives', Found) ) THEN
    CALL ListAddString( SolverParams, NextFreeKeyword( 'Exported Variable', SolverParams), "dHdt[dHdt:3]" );
  END IF
  
  IF (GetLogical( SolverParams, 'Calculate Current Density', Found) ) THEN
    CALL ListAddString( SolverParams, NextFreeKeyword( 'Exported Variable', SolverParams), "dHdx[dHdx:3]" );
  END IF

!------------------------------------------------------------------------------
END SUBROUTINE HWhitneyCalcFields_Init
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!> Calculate fields resulting from the edge element formulation
!> \ingroup Solvers
!------------------------------------------------------------------------------
 SUBROUTINE HWhitneyCalcFields( Model, Solver, dt, Transient )
!------------------------------------------------------------------------------
   USE HWhitneySolverUtils

   IMPLICIT NONE
!------------------------------------------------------------------------------
   TYPE(Solver_t) :: Solver
   TYPE(Model_t) :: Model
   REAL(KIND=dp) :: dt
   LOGICAL :: Transient
!------------------------------------------------------------------------------
   TYPE(Variable_t), POINTER :: pVar
   TYPE(Variable_t), POINTER :: HF, dHF, dHFdx
   TYPE(Variable_t), POINTER :: HF_e, dHF_e, dHFdx_e

   INTEGER :: i,j,k,l,t,n,nd,p,q,dofs,dofcount,vDOFs

   TYPE(Solver_t), POINTER :: pSolver
   CHARACTER(LEN=MAX_NAME_LEN) :: Pname

   LOGICAL :: Found, stat

   TYPE(Element_t), POINTER :: Element

   INTEGER, ALLOCATABLE :: Pivot(:)

   REAL(KIND=dp), POINTER CONTIG :: Fsave(:)
   TYPE(Mesh_t), POINTER :: Mesh
   REAL(KIND=dp), ALLOCATABLE, TARGET :: MASS(:,:), FORCE(:,:), GForce(:,:)
   LOGICAL :: PiolaVersion, ElementalFields, NodalFields, SerhoOrder, AnyTimeDer, AnySpaceDer
   LOGICAL :: ConstantBulkMatrix, ConstantBulkInUse
   INTEGER :: soln
   TYPE(ValueList_t), POINTER :: SolverParams

   REAL(KIND=dp) :: mu, mu0

!-------------------------------------------------------------------------------------------
   CALL Info( 'HWhitneyCalcFields', 'Computing postprocessing fields' )

   SolverParams => GetSolverParams()

   mu0 = GetConstReal( Model % Constants, 'Permeability of Vacuum' )

   soln = ListGetInteger( SolverParams, 'Primary Solver Index', Found )
   IF( soln == 0 ) THEN
     CALL Fatal( 'HWhitneyCalcFields', 'We should know > Primary Solver Index <' )
   END IF

   ! Pointer to primary solver
   pSolver => Model % Solvers(soln)
   pVar => pSolver % Variable
   Pname = getVarName(pVar)
   CALL Info( 'HWhitneyCalcFields', 'Name of potential variable: '//TRIM(pName), Level=10 )

   ! Inherit the solution basis from the primary solver
   vDOFs = pVar % DOFs
   IF( vDofs /= 1 ) THEN
     CALL Fatal( 'HWhitneyCalcFields', 'Primary variable should have 1 dofs: '//TRIM(I2S(vDofs)) )
   END IF
   dofs = 3

   SerhoOrder = GetLogical( pSolver % Values, 'Quadratic Approximation', Found )
   IF( SerhoOrder ) THEN
     PiolaVersion = .TRUE.
   ELSE
     PiolaVersion = GetLogical( pSolver % Values,'Use Piola Transform', Found )
   END IF

   IF (PiolaVersion) CALL Info( 'HWhitneyCalcFields', 'Using Piola transformed finite elements', Level=5 )

   Mesh => GetMesh()

   HF => VariableGet( Mesh % Variables, 'Hfield')
   HF_e => VariableGet( Mesh % Variables, 'Hfield E')

   dHF => VariableGet( Mesh % Variables, 'dHdt')
   dHF_e => VariableGet( Mesh % Variables, 'dHdt E')

   dHFdx => VariableGet( Mesh % Variables, 'dHdx')
   dHFdx_e => VariableGet( Mesh % Variables, 'dHdx E')

   AnyTimeDer = ASSOCIATED( dHF ) .OR. ASSOCIATED( dHF_e )
   AnySpaceDer = ASSOCIATED( dHFdx ) .OR. ASSOCIATED( dHFdx_e )

   i = 0
   IF ( ASSOCIATED(HF) ) i=i+3
   IF ( ASSOCIATED(dHF) ) i=i+3
   IF ( ASSOCIATED(dHFdx) ) i=i+3
   NodalFields = ( i > 0 )

   IF(NodalFields) THEN
     ALLOCATE(GForce(SIZE(Solver % Matrix % RHS),i)); Gforce=0._dp
   END IF

   j = 0
   IF ( ASSOCIATED(HF_e) ) j=j+3
   IF ( ASSOCIATED(dHF_e) ) j=j+3
   IF ( ASSOCIATED(dHFdx_e) ) j=j+3
   ElementalFields = ( j > 0 )

   dofs = MAX( i,j )

   n = Mesh % MaxElementDOFs
   ALLOCATE( MASS(n,n), FORCE(n,dofs), Pivot(n) )

   ConstantBulkMatrix = ListGetLogical( SolverParams, 'Constant Bulk Matrix', Found )
   ConstantBulkInUse = ASSOCIATED(Solver % Matrix % BulkValues) .AND. ConstantBulkMatrix

   IF (NodalFields) THEN
     CALL DefaultInitialize(UseConstantBulk = ConstantBulkInUse )
   ELSE
     CALL DefaultInitialize()
   END IF

   DO t = 1, GetNOFActive()
     Element => GetActiveElement(t)
     n = GetElementNOFNodes()
     nd = GetElementNOFDOFs(uSolver=pSolver)

     CALL LocalAssembly( t==1, IntegrateMass = ElementalFields .OR. .NOT.ConstantBulkInUse )

     IF (NodalFields) THEN
       IF(.NOT. ConstantBulkInUse ) THEN
         CALL DefaultUpdateEquations( MASS, FORCE(:,1) )
       END IF
       Fsave => Solver % Matrix % RHS
       DO l=1,dofs
         Solver % Matrix % RHS => GForce(:,l)
         CALL DefaultUpdateForce(FORCE(:,l))
       END DO
       Solver % Matrix % RHS => Fsave
     END IF

     IF (ElementalFields) THEN
       dofcount = 0
       CALL LUdecomp(MASS,n,pivot)
       CALL LocalSol(HF_e, 3, n, MASS, FORCE, pivot, dofcount)
       CALL LocalSol(dHF_e, 3, n, MASS, FORCE, pivot, dofcount)
       CALL LocalSol(dHFdx_e, 3, n, MASS, FORCE, pivot, dofcount)
     END IF

   END DO

   ! Assembly of the face terms in case we have DG method where we
   ! want to average the fields within materials making them continuous.
   !-----------------------------------------------------------------
   IF (GetLogical( SolverParams,'Discontinuous Galerkin',Found)) THEN
     IF (GetLogical( SolverParams,'Average Within Materials',Found)) THEN
       IF(.NOT. ConstantBulkInUse ) THEN
         FORCE = 0.0_dp
         CALL AddLocalFaceTerms( MASS, FORCE(:,1) )
       END IF
     END IF
   END IF

   IF (NodalFields) THEN
     IF (ConstantBulkMatrix .AND. .NOT. ConstantBulkInUse) THEN
       CALL Info('HWhitneyCalcFields', 'Saving the system matrix', Level=6)
       CALL CopyBulkMatrix(Solver % Matrix, BulkRHS = .FALSE.)
     END IF

     Fsave => Solver % Matrix % RHS
     dofcount = 0
     CALL GlobalSol(HF, 3, Gforce, dofcount)
     CALL GlobalSol(dHF, 3, Gforce, dofcount)
     CALL GlobalSol(dHFdx, 3, Gforce, dofcount)
     Solver % Matrix % RHS => Fsave
   END IF


CONTAINS


  SUBROUTINE LocalAssembly(InitHandles, IntegrateMass)

    LOGICAL :: InitHandles, IntegrateMass

    TYPE(GaussIntegrationPoints_t) :: IP
    TYPE(Nodes_t), SAVE :: Nodes
    REAL(KIND=dp), ALLOCATABLE :: WBasis(:,:), sol(:), dsol(:)
    REAL(KIND=dp), ALLOCATABLE :: RotWBasis(:,:), Basis(:), dBasisdx(:,:)
    INTEGER, ALLOCATABLE :: Indexes(:)
    REAL(KIND=dp) :: detJ, s, u, v, w
    REAL(KIND=dp) :: HF_ip(3), dHF_ip(3), dHFdx_ip(3)
    LOGICAL :: AllocationsDone = .FALSE.
    TYPE(ValueHandle_t) :: Mu_h
    INTEGER :: i,j,k,n

    SAVE Mu_h, sol, dsol, Indexes, WBasis, RotWBasis, Basis, dBasisdx

    IF(.NOT. AllocationsDone ) THEN
      N = Mesh % MaxElementDOFs
      ALLOCATE( sol(n), dsol(n), Indexes(n), WBasis(n,3), RotWBasis(n,3), Basis(n), dBasisdx(n,3) )
      sol = 0.0_dp
      dsol = 0.0_dp
      AllocationsDone = .TRUE.
    END IF


    IF( InitHandles ) THEN
      ! This has been normalized by mu0 in _init section
      CALL ListInitElementKeyword( Mu_h, 'Material', 'Relative Permeability' )
    END IF


    CALL GetElementNodes( Nodes )

    n = GetElementDOFs( Indexes, Element, pSolver )

    DO i=1,n
      j = pVar % Perm( Indexes(i) )
      IF ( j > 0 ) THEN
        sol(i) = pVar % Values(j)
        ! These are only applicable as we know this is 2nd order PDE
        IF( AnyTimeDer ) THEN
          dsol(i) = pVar % PrevValues(j,1)
        END IF
      END IF
    END DO

    ! Calculate nodal fields:
    ! -----------------------
    IP = GaussPoints(Element, EdgeBasis=.TRUE., PReferenceElement=PiolaVersion)

    MASS  = 0._dp
    FORCE = 0._dp

    DO j = 1,IP % n
      u = IP % U(j)
      v = IP % V(j)
      w = IP % W(j)

      stat = ElementInfo(Element,Nodes,u,v,w,detJ,Basis,dBasisdx, &
          EdgeBasis = Wbasis, RotBasis = RotWBasis, USolver = pSolver )

      ! Not currently used as only trivial fields are computed.
      !----------------------------------------------------------
      !mu = mu0 * ListGetElementReal( mu_h, Basis, Element, Found )

      HF_ip = MATMUL(sol(1:nd),WBasis(1:nd,:))
      IF( AnyTimeDer ) THEN
        dHF_ip = MATMUL( dsol(1:nd), WBasis(1:nd,:) )
      END IF
      
      IF( AnySpaceDer ) THEN
        dHFdx_ip = MATMUL( sol(1:nd), RotWBasis(1:nd,:) )
      END IF

      s = IP % s(j) * detJ

      IF (IntegrateMass) THEN
        DO p=1,n
          DO q=1,n
            MASS(p,q) = MASS(p,q) + s * Basis(p) * Basis(q)
          END DO
        END DO
      END IF

      DO p=1,n
        k = 0
        IF ( ASSOCIATED(HF) .OR. ASSOCIATED(HF_e)) THEN
          FORCE(p,k+1:k+3) = FORCE(p,k+1:k+3) + s * (HF_ip) * Basis(p)
          k = k+3
        END IF
        IF ( ASSOCIATED(dHF) .OR. ASSOCIATED(dHF_e)) THEN
          FORCE(p,k+1:k+3) = FORCE(p,k+1:k+3) + s*(dHF_ip) * Basis(p)
          k = k+3
        END IF
        IF ( ASSOCIATED(dHFdx) .OR. ASSOCIATED(dHFdx_e)) THEN
          FORCE(p,k+1:k+3) = FORCE(p,k+1:k+3) + s * (dHFdx_ip) * Basis(p)
          k = k+3
        END IF
      END DO
    END DO

  END SUBROUTINE LocalAssembly


!------------------------------------------------------------------------------
 SUBROUTINE GlobalSol( Var, m, b, dofs )
!------------------------------------------------------------------------------
   REAL(KIND=dp), TARGET CONTIG :: b(:,:)
   INTEGER :: m, dofs
   TYPE(Variable_t), POINTER :: Var
!------------------------------------------------------------------------------
   INTEGER :: i
   REAL(KIND=dp) :: Norm
!------------------------------------------------------------------------------
   IF(.NOT. ASSOCIATED(var)) RETURN

   DO i=1,m
     dofs = dofs+1
     Solver % Matrix % RHS => b(:,dofs)
     Solver % Variable % Values=0
     Norm = DefaultSolve()
     var % Values(i::m) = Solver % Variable % Values
   END DO
!------------------------------------------------------------------------------
 END SUBROUTINE GlobalSol
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
 SUBROUTINE LocalSol( Var, m, n, A, b, pivot, dofs )
!------------------------------------------------------------------------------
   TYPE(Variable_t), POINTER :: Var
   INTEGER :: pivot(:), m,n,dofs
   REAL(KIND=dp) :: b(:,:), A(:,:)
!------------------------------------------------------------------------------
   INTEGER :: ind(n), i
   REAL(KIND=dp) :: x(n)
!------------------------------------------------------------------------------
   IF(.NOT. ASSOCIATED(var)) RETURN

   ind = Var % dofs*(Var % Perm(Element % DGIndexes(1:n))-1)
   DO i=1,m
      dofs = dofs+1
      x = b(1:n,dofs)
      CALL LUSolve(n,MASS,x,pivot)
      Var % Values(ind(1:n)+i) = x
   END DO
!------------------------------------------------------------------------------
 END SUBROUTINE LocalSol
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
 SUBROUTINE AddLocalFaceTerms( STIFF, FORCE )
!------------------------------------------------------------------------------
   REAL(KIND=dp) :: STIFF(:,:), FORCE(:)

   TYPE(Element_t),POINTER :: P1,P2,Face,Faces(:)
   INTEGER ::t,n,n1,n2,NumberOfFaces,dim

   dim = CoordinateSystemDimension()

   IF (dim==2) THEN
     Faces => Solver % Mesh % Edges
     NumberOfFaces = Solver % Mesh % NumberOfEdges
   ELSE
     Faces => Solver % Mesh % Faces
     NumberOfFaces = Solver % Mesh % NumberOfFaces
   END IF

   DO t=1,NumberOfFaces
     Face => Faces(t)
     IF ( .NOT. ActiveBoundaryElement(Face) ) CYCLE

     P1 => Face % BoundaryInfo % Left
     P2 => Face % BoundaryInfo % Right
     IF ( ASSOCIATED(P2) .AND. ASSOCIATED(P1) ) THEN
       IF(.NOT.ASSOCIATED(GetMaterial(P1),GetMaterial(P2))) CYCLE

       n  = GetElementNOFNodes(Face)
       n1 = GetElementNOFNodes(P1)
       n2 = GetElementNOFNodes(P2)

       CALL LocalJumps( STIFF,Face,n,P1,n1,P2,n2)
       CALL DefaultUpdateEquations( STIFF, FORCE, Face )
     END IF
   END DO
!------------------------------------------------------------------------------
  END SUBROUTINE AddLocalFaceTerms
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
  SUBROUTINE LocalJumps( STIFF, Face, n, P1, n1, P2, n2 )
!------------------------------------------------------------------------------
    IMPLICIT NONE
    REAL(KIND=dp) :: STIFF(:,:)
    INTEGER :: n,n1,n2
    TYPE(Element_t), POINTER :: Face, P1, P2
!------------------------------------------------------------------------------
    REAL(KIND=dp) :: FaceBasis(n), P1Basis(n1), P2Basis(n2)
    REAL(KIND=dp) :: Jump(n1+n2), detJ, U, V, W, S
    LOGICAL :: Stat
    INTEGER ::  p, q, t
    TYPE(GaussIntegrationPoints_t) :: IntegStuff

    TYPE(Nodes_t) :: FaceNodes, P1Nodes, P2Nodes
    SAVE FaceNodes, P1Nodes, P2Nodes
!------------------------------------------------------------------------------
    STIFF = 0._dp

    CALL GetElementNodes(FaceNodes, Face)
    CALL GetElementNodes(P1Nodes, P1)
    CALL GetElementNodes(P2Nodes, P2)
!------------------------------------------------------------------------------
!     Numerical integration over the edge
!------------------------------------------------------------------------------
    IntegStuff = GaussPoints( Face )

    DO t=1,IntegStuff % n
      U = IntegStuff % u(t)
      V = IntegStuff % v(t)
      W = IntegStuff % w(t)
      S = IntegStuff % s(t)

      ! Basis function values & derivatives at the integration point:
      !--------------------------------------------------------------
      stat = ElementInfo(Face, FaceNodes, U, V, W, detJ, FaceBasis)

      S = S * detJ

      ! Find basis functions for the parent elements:
      ! ---------------------------------------------
      CALL GetParentUVW(Face, n, P1, n1, U, V, W, FaceBasis)
      stat = ElementInfo(P1, P1Nodes, U, V, W, detJ, P1Basis)

      CALL GetParentUVW(Face, n, P2, n2, U, V, W, FaceBasis)
      stat = ElementInfo(P2, P2Nodes, U, V, W, detJ, P2Basis)

      ! Integrate jump terms:
      ! ---------------------
      Jump(1:n1) = P1Basis(1:n1)
      Jump(n1+1:n1+n2) = -P2Basis(1:n2)

      DO p=1,n1+n2
        DO q=1,n1+n2
          STIFF(p,q) = STIFF(p,q) + s * Jump(q)*Jump(p)
        END DO
      END DO
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE LocalJumps
!------------------------------------------------------------------------------

!------------------------------------------------------------------------
END SUBROUTINE HWhitneyCalcFields
!------------------------------------------------------------------------
