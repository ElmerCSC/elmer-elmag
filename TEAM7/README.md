TEAM Problem 7: Asymmetrical Conductor with a hole

This model showcases how to solve a 3D eddy current problem with a closed coil
and accounting for the induced currents on an aluminium plate. 

The model is solved as a transient problem, with a sinusoidal excitation current on a close coil. 


ElmerSolver uses the following modules to solve the problem:

1) Procedure = "CoilSolver" "CoilSolver"

Solves the current density in a closed coil at a fixed desired total current (Desired Coil Current).


2) Procedure = "MagnetoDynamics" "WhitneyAVSolver"

Takes the current density solved in the previous step and solves the A-v formulation including conducting and non-conducting domains.

3)Procedure = "ResultOutputSolve" "ResultOutputSolver"

Exports the solution of the field problem to be displayed in Gmsh and Vtu (Paraview) formats


Find description of the problem under TEAM7_ProblemDescription.pdf
See results using ElmerSolver under TEAM7_Results.png
Find data used under TEAM7_A1B1.csv 
