If using main.sh, change the path for Gmsh

Generate the mesh in ./Gmsh forlder:
gmsh assembly.geo -3
Translation from Gmsh mesh *.msh to Elmer-CSC mesh format:
ElmerGrid 14 2 assembly.msh -out MESH

Creation of entities.sif in subfolder with name of mesh (mesh.names) to get the correspondence between the mesh groups and the bodies defined in Elmer-CSC

User-defined Functions (UDF) was implemented (see Fortran90):
hWhitneySolver.F90: H formulation based on electromagnetic wave solver

"bash compileUDF.sh" to compile all the external UDF at once. Run it before running the solver.

Run the solver on the case file: "ElmerSolver case.sif"
