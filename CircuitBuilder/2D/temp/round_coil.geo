//-----------------------------
// Boundary Layer Example
//-----------------------------

// Parameters
radc = 1e-3;
rada = 10e-3;

//Initial Mesh Control Parameters
mshc = 1e-4;
msha = 5e-4;

// model center
cx = 0;
cy = 0;
cz = 0;

// turn radius
d = 0.35e-2;

//Center
CentralPoint[] += newp; Point(newp) = {cx, cy, cz, mshc};

CenterC1[] += newp; Point(newp) = {cx+d, cy, cz, mshc};
CenterC2[] += newp; Point(newp) = {cx-d, cy, cz, mshc};


//Points that create the cylindrical round conductor------------------------------------------------
Conductor1Points[] += newp; Point(newp) = {radc+d, 0, 0, mshc};
Conductor1Points[] += newp; Point(newp) = {0+d,radc, 0, mshc};
Conductor1Points[] += newp; Point(newp) = {-radc+d, 0, 0, mshc};
Conductor1Points[] += newp; Point(newp) = {0+d, -radc, 0, mshc};
//--------------------------------------------------------------------------------------------------

//Lines that create the cylindrical round conductor-------------------------------------------------
Conductor1Lines[] += newl; Circle(newl) = {Conductor1Points[0], CenterC1[0], Conductor1Points[1]};
Conductor1Lines[] += newl; Circle(newl) = {Conductor1Points[1], CenterC1[0], Conductor1Points[2]};
Conductor1Lines[] += newl; Circle(newl) = {Conductor1Points[2], CenterC1[0], Conductor1Points[3]};
Conductor1Lines[] += newl; Circle(newl) = {Conductor1Points[3], CenterC1[0], Conductor1Points[0]};
//--------------------------------------------------------------------------------------------------

//Create surface of round conductor-----------------------------------------------------------------
Conductor1LineLoop[] += newll; Curve Loop(newll) = {Conductor1Lines[{0,1,2,3}]};
Conductor1Surface[] += news; Plane Surface(news) = {Conductor1LineLoop[0]};
//--------------------------------------------------------------------------------------------------   

//Points that create the cylindrical round conductor------------------------------------------------
Conductor2Points[] += newp; Point(newp) = {radc-d, 0, 0, mshc};
Conductor2Points[] += newp; Point(newp) = {0-d,radc, 0, mshc};
Conductor2Points[] += newp; Point(newp) = {-radc-d, 0, 0, mshc};
Conductor2Points[] += newp; Point(newp) = {0-d, -radc, 0, mshc};
//--------------------------------------------------------------------------------------------------

//Lines that create the cylindrical round conductor-------------------------------------------------
Conductor2Lines[] += newl; Circle(newl) = {Conductor2Points[0], CenterC2[0], Conductor2Points[1]};
Conductor2Lines[] += newl; Circle(newl) = {Conductor2Points[1], CenterC2[0], Conductor2Points[2]};
Conductor2Lines[] += newl; Circle(newl) = {Conductor2Points[2], CenterC2[0], Conductor2Points[3]};
Conductor2Lines[] += newl; Circle(newl) = {Conductor2Points[3], CenterC2[0], Conductor2Points[0]};
//--------------------------------------------------------------------------------------------------

//Create surface of round conductor-----------------------------------------------------------------
Conductor2LineLoop[] += newll; Curve Loop(newll) = {Conductor2Lines[{0,1,2,3}]};
Conductor2Surface[] += news; Plane Surface(news) = {Conductor2LineLoop[0]};
//--------------------------------------------------------------------------------------------------  


//Points that create the cylindrical non-conducting domain------------------------------------------
AirPoints[] += newp; Point(newp) = {rada, 0, 0, msha};
AirPoints[] += newp; Point(newp) = {0,rada, 0, msha};
AirPoints[] += newp; Point(newp) = {-rada, 0, 0, msha};
AirPoints[] += newp; Point(newp) = {0, -rada, 0, msha};
//--------------------------------------------------------------------------------------------------           
    
//Lines that create the cylindrical round conductor-------------------------------------------------
AirLines[] += newl; Circle(newl) = {AirPoints[0], CentralPoint[0], AirPoints[1]};
AirLines[] += newl; Circle(newl) = {AirPoints[1], CentralPoint[0], AirPoints[2]};
AirLines[] += newl; Circle(newl) = {AirPoints[2], CentralPoint[0], AirPoints[3]};
AirLines[] += newl; Circle(newl) = {AirPoints[3], CentralPoint[0], AirPoints[0]};


//Create surface of round conductor-----------------------------------------------------------------
AirLineLoop[] += newll; Curve Loop(newll) = {AirLines[{0,1,2,3}]};
AirSurface[]  += news; Plane Surface(news) = {AirLineLoop[0],Conductor1LineLoop[0],Conductor2LineLoop[0]};
//--------------------------------------------------------------------------------------------------           

// Physical Entities
//+
Physical Surface("Terminal1", 77) = {Conductor1Surface[]};
Physical Surface("Terminal2", 78) = {Conductor2Surface[]};
Physical Surface("Air", 79) = {AirSurface[]};
Physical Curve("Infinity_Magnetic_Insulation", 80) = {AirLines[{0,1,2,3}]};
//+


//Setup Boundary Layer Mesh-------------------------------------------------------------------------
Field[1] = BoundaryLayer;
Field[1].EdgesList = { Boundary{Surface{Conductor1Surface[],Conductor2Surface[]}; } };
Field[1].hfar = radc/40;                             // Element size far from the wall
Field[1].hwall_n = radc/100;                         // Mesh Size Normal to the The Wall
Field[1].thickness = radc/4;                         // Maximum thickness of the boundary layer
Field[1].ratio = 1.2;                                // Size Ratio Between Two Successive Layers
Field[1].ExcludedFaceList = {AirSurface[]};          // Don't create BL in this surface
Field[1].Quads = 1;                                  // Make quads
BoundaryLayer Field = 1;
