/*
Author: F. Trillaud <ftrillaudp@gmail.com>
Date: 03/05/2021
*/


Include "coil.par";


Group
{
Omega_inductors = Region[{coilID}];
Gamma_inductors = Region[{coilBoundaryID}];
Omega_air = Region[{airID}];
/// Non-conducting domains (sigma = 0)
Omega_nonConducting = Region[{Omega_inductors, airID}];
/// Linear domain (constant permeability):
Omega_linear = Region[{Omega_inductors, airID}];
Omega = Region[{Omega_linear}];

/// Boundary condition:
Gamma_dirichlet = Region[{airBoundaryID}];
/// Gauge:
Gamma_treeCotreeGauge = Region[{Gamma_dirichlet}];
}


Function
{
DefineConstant[
J = {1.274e8, Name "Input/1Electrical parameters/1Engineering current density (Am⁻2)"},
flag_CoulombsGauge = {0, Choices{0, 1}, AutoCheck 0, Name "Input/3Gauges/1Coulomb's gauge (div(a) = 0)"},
flag_treeCotreeGauge = {1, Choices{0, 1}, AutoCheck 1, Name "Input/3Gauges/2Tree-coTree gauge (a.w = 0)"},
visualization = {0, Choices{0, 1}, AutoCheck 1, Name "Output/PostProcessing/Visualization", Label "Real-time visualization"},
command = {"-solve -bin -v 3 -v2", Name "GetDP/10Compute command", Visible 0}
];

mu0 = 4*Pi*1e-7;
nu[Omega_linear] = 1./mu0;

directionalVector[] = Vector[-Sin[Atan2[Y[],X[]]#1], Cos[#1], 0];
Je[] = J*directionalVector[];
}


Include "integration.par";

Include "jacobian.par";


Constraint
{
  { Name A_Constraint; Type Assign;
    Case { { Region Gamma_dirichlet; Value 0.0;} }
  }
  { Name A_gauge; Type Assign;
    Case
    {
      If (flag_treeCotreeGauge == 1)
        { Region Omega_nonConducting; SubRegion Gamma_treeCotreeGauge; Value 0.0;}
      EndIf
    }
  }
 {
  Name Phi_Constraint; Type Assign;
  Case { { Region Gamma_dirichlet; Value 0.0; } }
 }
}


FunctionSpace
{
 {
  Name A_FunctionSpace;
  Type Form1;
  BasisFunction
  {
   {
    Name wA_Edge; NameOfCoef cA_Edge;
    Function BF_Edge; Support Omega; Entity EdgesOf[ All];
   }
  }
  Constraint
  {
   { NameOfCoef cA_Edge; EntityType EdgesOf; NameOfConstraint A_Constraint; }
    If (flag_treeCotreeGauge == 1)
      { NameOfCoef cA_Edge; EntityType EdgesOfTreeIn; EntitySubType StartingOn; NameOfConstraint A_gauge; }
    EndIf
  }
 }
  // scalar potential for Coulomb gauge: orthogonal to grad(Phi)
  {
    Name Phi_FunctionSpace;
    Type Form0;
    BasisFunction
    {
      {
        Name wPhi_Grad;
        NameOfCoef cPhi_Grad;
        Function BF_Node;
        Support Omega;
        Entity NodesOf[ All ];
      }
    }
    Constraint
    {
      { NameOfCoef cPhi_Grad; EntityType NodesOf; NameOfConstraint Phi_Constraint; }
    }
  }

  // correcting source interpolation Je[] so that (weakly) div j = 0
  {
    Name Phi_divJ_FunctionSpace;
    Type Form0;
    BasisFunction
    {
      {
        Name wPhi_Grad;
        NameOfCoef cPhi_Grad;
        Function BF_Node;
        Support Region[{Omega_inductors, Gamma_inductors}];
        Entity NodesOf[ All ];
      }
    }
    Constraint
    {
      { NameOfCoef cPhi_Grad; EntityType NodesOf; NameOfConstraint Phi_Constraint; }
    }
  }
}


Formulation
{
  {
    Name divJ0_Formulation;
    Type FemEquation;
    Quantity
    {
      { Name Phi; Type Local; NameOfSpace Phi_divJ_FunctionSpace; }
    }
    Equation
    {
      Galerkin
      {
        [Je[], {d Phi}]; In Omega;
        Jacobian jacobVol;
        Integration advancedIntegration;
      }
      Galerkin
      {
        [-Dof{d Phi}, {d Phi}]; In Omega;
        Jacobian jacobVol;
        Integration advancedIntegration;
      }
    }
  }

 {
  Name A_Formulation;
  Type FemEquation;
  Quantity
  {
   { Name A; Type Local; NameOfSpace A_FunctionSpace; }
   If (flag_CoulombsGauge == 1)
    { Name Phi; Type Local; NameOfSpace Phi_divJ_FunctionSpace; }
   EndIf
  }
  Equation
  {
   Galerkin
   {
    [nu[]*Dof{d A}, {d A}]; In Omega;
    Jacobian jacobVol;
    Integration advancedIntegration;
   }
   Galerkin
   {
    [-Je[], {A}]; In Omega_inductors;
    Jacobian jacobVol;
    Integration advancedIntegration;
   }

   If (flag_CoulombsGauge == 1)
      Galerkin
      {
        [{d Phi}, {A}]; In Omega;
        Jacobian jacobVol;
        Integration advancedIntegration;
      }
      Galerkin
      {
        [Dof{A}, {d Phi}]; In Omega;
        Jacobian jacobVol;
        Integration advancedIntegration;
      }
      Galerkin
      {
        [Dof{d Phi}, {A}]; In Omega;
        Jacobian jacobVol;
        Integration advancedIntegration;
      }
   EndIf

  }
 }

}


Resolution
{
 {
  Name resolution;
  System
  {
   { Name A_System; NameOfFormulation A_Formulation; }
   If (flag_CoulombsGauge == 1)
    { Name divJ0_System; NameOfFormulation divJ0_Formulation; }
   EndIf
  }
  Operation
  {
   CreateDirectory["Results"]; // create directory to store result files

   If (flag_CoulombsGauge == 1)
    InitSolution[divJ0_System];
    Generate[divJ0_System];
    Solve[divJ0_System];
    SaveSolution[divJ0_System];
   EndIf

   InitSolution[A_System];
   GenerateJac[A_System];
   SolveJac[A_System];
   SaveSolution[A_System];
  }
 }
}


PostProcessing
{
    {
        Name postProcessing;
        NameOfFormulation A_Formulation;
        NameOfSystem A_System;
        Quantity
        {
            { Name A; Value { Local { [Norm[{A}]]; In Omega; Jacobian jacobVol; } } }
            { Name vecB; Value { Local{ [{d A}]; In Omega; Jacobian jacobVol; } } }
            { Name B; Value { Local { [Norm[{d A}]]; In Omega; Jacobian jacobVol; } } }
            { Name Je; Value { Term { [Je[]]; In Omega_inductors; Jacobian jacobVol; } } }
        }
    }
}


PostOperation
{
    {
        Name postOperation;
        NameOfPostProcessing postProcessing;
        Operation
        {
          Print[A, OnElementsOf Omega_inductors, File "Results/magneticVectorPotential.pos", Name "A (T-m)"];
          Print[B, OnElementsOf Omega_inductors, File "Results/magneticFluxDensity.pos", Name "|B| (T)"];
          Print[vecB, OnElementsOf Omega_inductors, File "Results/magneticFluxDensityVector.pos", Name "B (T)"];
          Print[Je, OnElementsOf Omega_inductors, File "Results/negativeCurrentDensity.pos", Name "Je (A-m^-2)"];
       }
    }
}
