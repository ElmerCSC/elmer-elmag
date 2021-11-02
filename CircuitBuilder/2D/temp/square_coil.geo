//-----------------------------
// Boundary Layer Example
//-----------------------------

// Parameters
radc = 2e-3;
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

//CenterC1[] += newp; Point(newp) = {cx+d, cy, cz, mshc};
//CenterC2[] += newp; Point(newp) = {cx-d, cy, cz, mshc};


//Points that create the cylindrical round conductor------------------------------------------------
Conductor1Points[] += newp; Point(newp) = {radc/2+d, radc/2, 0, mshc};
Conductor1Points[] += newp; Point(newp) = {-radc/2+d,radc/2, 0, mshc};
Conductor1Points[] += newp; Point(newp) = {-radc/2+d,-radc/2, 0, mshc};
Conductor1Points[] += newp; Point(newp) = {radc/2+d,-radc/2, 0, mshc};
//--------------------------------------------------------------------------------------------------

//Lines that create the cylindrical round conductor-------------------------------------------------
Conductor1Lines[] += newl; Line(newl) = {Conductor1Points[0], Conductor1Points[1]};
Conductor1Lines[] += newl; Line(newl) = {Conductor1Points[1], Conductor1Points[2]};
Conductor1Lines[] += newl; Line(newl) = {Conductor1Points[2], Conductor1Points[3]};
Conductor1Lines[] += newl; Line(newl) = {Conductor1Points[3], Conductor1Points[0]};
//--------------------------------------------------------------------------------------------------

//Create surface of round conductor-----------------------------------------------------------------
Conductor1LineLoop[] += newll; Curve Loop(newll) = {Conductor1Lines[{0,1,2,3}]};
Conductor1Surface[] += news; Plane Surface(news) = {Conductor1LineLoop[0]};
//--------------------------------------------------------------------------------------------------   

//Points that create the cylindrical round conductor------------------------------------------------
Conductor2Points[] += newp; Point(newp) = {radc/2-d, radc/2, 0, mshc};
Conductor2Points[] += newp; Point(newp) = {-radc/2-d,radc/2, 0, mshc};
Conductor2Points[] += newp; Point(newp) = {-radc/2-d,-radc/2, 0, mshc};
Conductor2Points[] += newp; Point(newp) = {radc/2-d,-radc/2, 0, mshc};
//--------------------------------------------------------------------------------------------------

//Lines that create the cylindrical round conductor-------------------------------------------------
Conductor2Lines[] += newl; Line(newl) = {Conductor2Points[0], Conductor2Points[1]};
Conductor2Lines[] += newl; Line(newl) = {Conductor2Points[1], Conductor2Points[2]};
Conductor2Lines[] += newl; Line(newl) = {Conductor2Points[2], Conductor2Points[3]};
Conductor2Lines[] += newl; Line(newl) = {Conductor2Points[3], Conductor2Points[0]};
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

