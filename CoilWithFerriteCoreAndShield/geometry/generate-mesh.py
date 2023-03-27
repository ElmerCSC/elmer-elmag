#!.venv/bin/python

import gmsh

mm = 1/1000 # FreeCAD (and ourselves) use mm, Elmer wants SI units (i.e. meters)

gmsh.initialize()
gmsh.model.add("coil")

gmsh.option.setNumber("Geometry.OCCScaling", mm)

# Coil
(_, coil) = gmsh.model.occ.importShapes("coil.brep", highestDimOnly=True)[-1]

# Core
(_, core) = gmsh.model.occ.importShapes("core.brep", highestDimOnly=True)[-1]

# Metal Plate
(_, metal_plate) = gmsh.model.occ.importShapes("metal-plate.brep", highestDimOnly=True)[-1]

# Air/Infinity
sphere = gmsh.model.occ.addSphere(0, 0, 0, 300*mm)
[(_, air)], _ = gmsh.model.occ.cut([(3, sphere)], [(3, coil), (3, core), (3, metal_plate)], removeObject=False, removeTool=False)

gmsh.model.occ.synchronize()

# Add named physical volumes and surfaces as
# GMSH "physical groups" for better readability.
# This allows ElmerGrid to know which parts of the
# model are actually required in the simulation
# plus these names can be referenced in your SIF
# file directly without having to deal with numbers.
gmsh.model.addPhysicalGroup(3, [air], name="Air")
gmsh.model.addPhysicalGroup(3, [coil], name="Coil")
gmsh.model.addPhysicalGroup(3, [core], name="Core")
gmsh.model.addPhysicalGroup(3, [metal_plate], name="Metal Plate")

infinity = [ entity for [_, entity] in gmsh.model.getBoundary([(3, sphere)], oriented=False)]
gmsh.model.addPhysicalGroup(2, infinity, name="Infinity")
gmsh.model.removeEntities([(3, sphere)])

# GMSH uses the concept of "fields" to control
# mesh refinement. We create a denser mesh near
# the coil. Further away from the coil the mesh
# becomes gradually coarser.
gmsh.model.mesh.field.add("Box", 1)
gmsh.model.mesh.field.setNumber(1, "Thickness", 10*mm)
gmsh.model.mesh.field.setNumber(1, "XMin", -20*mm)
gmsh.model.mesh.field.setNumber(1, "XMax", 20*mm)
gmsh.model.mesh.field.setNumber(1, "YMin", -25*mm)
gmsh.model.mesh.field.setNumber(1, "YMax", 25*mm)
gmsh.model.mesh.field.setNumber(1, "ZMin", -40*mm)
gmsh.model.mesh.field.setNumber(1, "ZMax", 40*mm)
gmsh.model.mesh.field.setNumber(1, "VIn", 3*mm)
gmsh.model.mesh.field.setNumber(1, "VOut", 8*mm)
gmsh.model.mesh.field.setAsBackgroundMesh(1)

# Disable default mesh refinement rules as we
# specified our own.
gmsh.option.setNumber("Mesh.MeshSizeExtendFromBoundary", 0)
gmsh.option.setNumber("Mesh.MeshSizeFromPoints", 0)
gmsh.option.setNumber("Mesh.MeshSizeFromCurvature", 0)

# Mesh Algorithm: Delaunay
gmsh.option.setNumber("Mesh.Algorithm", 5)
gmsh.model.mesh.generate(3)

# Export the Mesh
gmsh.write("coil.msh")
gmsh.finalize()
