# Induction heating

Example for 3D steady-state induction heating modeling with Elmer.

## Setup

### Geometry

The setup consists of a graphite cylinder in an open copper coil. The mesh is generated using Gmsh and consists of 228962 tetrahedra. The skin layer on the coil surface is not resolved.

![setup](./setup-inductionheating.png)

The coil inner diameter is 140 mm, the graphite cylinder has a diameter of 120 mm and height of 50 mm. The surrounding domain has a diameter of 263 mm and height of 383 mm. The model is derived from the geometry shown in [this paper](https://doi.org/10.1016/j.jcrysgro.2022.126750).

### Electromagnetical simulation parameters

A frequency of 13.5 kHz is used. A total inductor current of 100 A is applied.

### Thermal simulation parameter

Only a dummy heat solver is used: the cylinder is heated inductively, its heat is transferred by conduction trough the air to the coil / outside boundary, where a temperature of 20°C is set.

## Implementations

There are various approaches to simulate induction heating in Elmer. One approach would be to resolve the skin layer of the electric current in the inductor, however, this requires a very fine mesh. Here, implementations without resolved skin layer are presented.

### CoilSolver

In the setup [case_coil-solver.sif](./case_coil-solver.sif) we use *CoilSolver* to compute a current density vector field which has known total flux over the ends. It is then used to initialize the *WhitneyAVHarmonicSolver*. Note that this solver requires a reduced electric conductivity in the coil to avoid self-induction. It is set to 1 S/m, as the coil solver does not allow for an electric conductivity of 0.

### Scaled conductivity

In the setup [case_scaled-conductivity.sif](./case_scaled-conductivity.sif) we assume massive coil. Unfornately then the current would be with realistic parameters very much only on the skin of the coil. This requires impratical meshes and is not really feasible. Hence we scale down the electric conductivity of the coil and use current density BCs with "-distribute" pragma to define the current. The user may experiment how high conductivity may be used.

### Layer conductivity

In the setup [case_scaled-conductivity.sif](./case_scaled-conductivity.sif) we assume that the frequency is such high that the current density in the inductor only recides on the boundary as the skin depth is so small. In order to be able to set Neumann BCs for the end of the skin we need to create line BCs on the fly as intersection of two surface BCs. Note that the layer BCs activate the computation of surface currents. No volume currents are postprocessed due to the layer BCs. Unfortunately the convergence of the solution is not too good. This should however provide a basis for critical comparison between different methods.
