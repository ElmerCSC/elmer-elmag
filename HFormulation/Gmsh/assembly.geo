/// Frederic Trillaud <ftrillaudp@gmail.com>
/// October 12, 2020

DefineConstant[
bulkRadius = {0.0125, Name "Input/0Geometry/0Bulk radius (m)"},
bulkHeight = {0.01, Name "Input/0Geometry/1Bulk height (m)"},
materialMeshSize = {0.15, Name "Input/1Mesh/0Mesh size of material (-)"},
airMeshSize = {0.3, Name "Input/1Mesh/1Mesh size of air (-)"},
flag_sphereBND = {1, Choices{0,1}, Name "Input/1Mesh/2Sphere boundary {yes = 1, no = 0}"}
flag_mesh = {0, Choices{0,1}, Name "Input/1Mesh/2Progressive mesh {yes = 1, no = 0}"}
];
airRadius = 3*bulkRadius;
If (flag_sphereBND == 1)
 airHeight = 4*bulkHeight;
EndIf
If (flag_mesh == 1)
  materialMeshSize = 0.04;
  airMeshSize = 6;
EndIf
lc_1 = bulkHeight*materialMeshSize;
lc_2 = airRadius*airMeshSize;

// --- Constant definition for regions ---
materialID = 1;
airID = 2; airBoundaryID = 3;
fixedTemperatureID = 4;

/// --- Geometry and Meshing ----
SetFactory("OpenCASCADE");
General.ExpertMode = 1;

/// Geometry options:
Geometry.Color.Lines = Black;
Geometry.LineWidth = 1;
Geometry.PointSize = 1;

/// Remove duplicate geometrical elementary entities:
Coherence;

/// Remove duplicate nodes of the mesh:
Coherence Mesh;

/// Optimize mesh:
Mesh.Optimize = 1;

// Save mesh as binary file (to save storage): cannot be used with ElmerGrid, choose 0
Mesh.Binary = 0;

/// --- Build geometry ---
// Material:
Cylinder(1) = {0., 0., (-1)*0.5*bulkHeight, 0., 0., bulkHeight, bulkRadius};
surfaces_1() = Boundary{Volume{1};};
edges_1() = Boundary{Surface{surfaces_1()};};
verteces_1() = PointsOf{Line{edges_1()};};
Characteristic Length{verteces_1()} = lc_1;

// Air:
If (flag_sphereBND == 1)
 Sphere(2) = {0., 0., 0., airRadius};
 surfaces_2() = Boundary{Volume{2};};
 edges_2() = Boundary{Surface{surfaces_2()};};
 verteces_2() = PointsOf{Line{edges_2()};};
 Characteristic Length{verteces_2()} = lc_2;
Else
 Cylinder(2) = {0., 0., (-1)*0.5*airHeight, 0., 0., airHeight, airRadius};
 surfaces_2() = Boundary{Volume{2};};
 edges_2() = Boundary{Surface{surfaces_2()};};
 verteces_2() = PointsOf{Line{edges_2()};};
 Characteristic Length{verteces_2()} = lc_2;
EndIf
// Substract tool (1) from object (2)
volumeAir = BooleanDifference { Volume{2}; Delete; }{ Volume{1}; };

// Definition of volumes and boundaries //
Physical Volume("material", materialID) = {1};
Color Red {Volume {1};}
Physical Volume("air", airID) = {volumeAir};
Color Blue {Volume {volumeAir};}
// For ElmerGrid, we cannot have the same submesh in different Physical Surface.
If (flag_sphereBND == 1)
 Physical Surface("airBoundary", airBoundaryID) = {surfaces_2(0)};
Else
 Physical Surface("airBoundary", airBoundaryID) = {surfaces_2()};
EndIf
Physical Surface("fixedTemperature", fixedTemperatureID) = {surfaces_1(1)};

/// --- Refinement of mesh ---
If (flag_mesh == 1)
  // Attractors field on on edges:
  Field[1] = Attractor;
  Field[1].NNodesByEdge = 60; // #attractors on the edges
  Field[1].EdgesList = {edges_1(0), edges_1(2)};
  // Threshold field defined on the attractors
  Field[2] = Threshold;
  Field[2].IField = 1;
  Field[2].LcMin = lc_1; // char length inside DistMin
  Field[2].LcMax = lc_2; // char length outside DistMax
  Field[2].DistMin = lc_1;
  Field[2].DistMax = 1.;
  // Use minimum of all the fields as the background field
  Field[3] = Min;
  Field[3].FieldsList = {2};
  Background Field = 3;
  // Don't extend the elements sizes from the boundary inside the domain
  Mesh.CharacteristicLengthExtendFromBoundary = 0;
EndIf

/// --- Miscellaneous information ---
cpuTime = Cpu;
memoryUsage = Memory;
Printf("CPU time", cpuTime);
Printf("Memory usage", memoryUsage);
