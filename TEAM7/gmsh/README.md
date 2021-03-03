# Gmsh Geometry (.geo) and Mesh (.msh) files

To be able to access these extension files,
Please compile or download binaries from:

http://gmsh.info/

To mesh .geo file please run the following cmd/terminal 

gmsh TEAM7.geo -3 

Where the -3 implies 3D meshing. This will save the mesh as TEAM7.msh

If further mesh refinement is needed: 

gmsh TEAM7.msh -refine  

The command performs uniform mesh refinement. 
You can also perform barycentric refinement with command: -barycentric_refine

