//-----------------------------
// Boundary Layer Example
//-----------------------------

// Parameters
radc = 1e-3;
rada = 10e-3;

//Initial Mesh Control Parameters
mshc = 1e-4;
msha = 5e-4;

//Center
CentralPoint[] += newp; Point(newp) = {0, 0, 0, mshc};

//Points that create the cylindrical round conductor------------------------------------------------
ConductorPoints[] += newp; Point(newp) = {radc, 0, 0, mshc};
ConductorPoints[] += newp; Point(newp) = {0,radc, 0, mshc};
ConductorPoints[] += newp; Point(newp) = {-radc, 0, 0, mshc};
ConductorPoints[] += newp; Point(newp) = {0, -radc, 0, mshc};
//--------------------------------------------------------------------------------------------------

//Lines that create the cylindrical round conductor-------------------------------------------------
ConductorLines[] += newl; Circle(newl) = {ConductorPoints[0], CentralPoint[0], ConductorPoints[1]};
ConductorLines[] += newl; Circle(newl) = {ConductorPoints[1], CentralPoint[0], ConductorPoints[2]};
ConductorLines[] += newl; Circle(newl) = {ConductorPoints[2], CentralPoint[0], ConductorPoints[3]};
ConductorLines[] += newl; Circle(newl) = {ConductorPoints[3], CentralPoint[0], ConductorPoints[0]};
//--------------------------------------------------------------------------------------------------

//Create surface of round conductor-----------------------------------------------------------------
ConductorLineLoop[] += newll; Curve Loop(newll) = {ConductorLines[{0,1,2,3}]};
ConductorSurface[] += news; Plane Surface(news) = {ConductorLineLoop[0]};
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
AirSurface[]  += news; Plane Surface(news) = {AirLineLoop[0],ConductorLineLoop[0]};
//--------------------------------------------------------------------------------------------------           


/*
//Setup Boundary Layer Mesh-------------------------------------------------------------------------
Field[1] = BoundaryLayer;
Field[1].EdgesList = {ConductorLines[]};    
Field[1].NodesList = {ConductorPoints[]};
Field[1].hfar = radc/10;                       //hfar Element size far from the wall
//Field[1].hwall_n =  msha;                    //hwall_n Mesh Size Normal to the The Wall
Field[1].ratio = 1.1;                          //ratio Size Ratio Between Two Successive Layers
Field[1].thickness = radc/20;                  //thickness Maximum thickness of the boundary layer
Background Field = 1;
*/
//+
Extrude {0, 0, -1e-2} {
  Surface{6}; Surface{12}; Layers {8}; Recombine;
}
//+
Physical Volume("Wire", 77) = {1};
//+
Physical Volume("Air", 78) = {2};
//+
Physical Surface("Infinity_Magnetic_Insulation", 79) = {12, 47, 59, 55, 51, 76};
//+
Physical Surface("Terminal_1", 80) = {6};
//+
Physical Surface("Terminal_2", 81) = {34};
