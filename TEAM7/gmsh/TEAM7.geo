// Gmsh project created on Mon Feb  8 11:09:02 2021

// units
mm= 1/1000; //millimters


// Characteristic Lenghts
// Initial mesh control
lc_coil  = 20*mm; // used on results shown 15*mm;
lc_plate = 10*mm;  // 4*mm;
lc_hole  = 10*mm;  // 4*mm;
lc_air   = 30*mm; // 20*mm;

// geometrical parameters
plate_width  = 294*mm;// x-coordinate
plate_depth  = 294*mm;// y-coordinate
plate_thick = 19*mm; // z-coordinate

hole_width  = 108*mm; // x-coordinate
hole_depth  = 108*mm; // y-coordinate
hole_height = 19*mm;  // z-coordinate
hole_distx  = 18*mm;  // distance from plate edge x-coordinate
hole_disty  = 18*mm;  // distance from plate edge y-coordinate

coil_width  = 150*mm; // x-coordinate
coil_depth  = 150*mm; // y-coordinate
coil_height = 100*mm; // z-coordinate
coil_thick  = 25*mm;  // distance bewteen inner and outer coil curves

// pistance between coil and plate
cp_dist = 30*mm;

// points
PlateEdgesPoints[] += newp; Point(newp) = {0, 0, 0, lc_plate};
PlateEdgesPoints[] += newp; Point(newp) = {plate_width, 0, 0, lc_plate};
PlateEdgesPoints[] += newp; Point(newp) = {plate_width, plate_depth, 0, lc_plate};
PlateEdgesPoints[] += newp; Point(newp) = {0, plate_depth, 0, lc_plate};

HoleEdgesPoints[] += newp; Point(newp) = {hole_distx, hole_disty, 0, lc_hole};
HoleEdgesPoints[] += newp; Point(newp) = {hole_distx + hole_width, hole_disty, 0, lc_hole};
HoleEdgesPoints[] += newp; Point(newp) = {hole_distx + hole_width, hole_disty + hole_depth, 0, lc_hole};
HoleEdgesPoints[] += newp; Point(newp) = {hole_distx , hole_depth + hole_disty, 0, lc_hole};

// lines 
PlateLines[] += newl; Line(newl) = {PlateEdgesPoints[{0,1}]};
PlateLines[] += newl; Line(newl) = {PlateEdgesPoints[{1,2}]};
PlateLines[] += newl; Line(newl) = {PlateEdgesPoints[{2,3}]};
PlateLines[] += newl; Line(newl) = {PlateEdgesPoints[{3,0}]};

HoleLines[] += newl; Line(newl) = {HoleEdgesPoints[{0,1}]};
HoleLines[] += newl; Line(newl) = {HoleEdgesPoints[{1,2}]};
HoleLines[] += newl; Line(newl) = {HoleEdgesPoints[{2,3}]};
HoleLines[] += newl; Line(newl) = {HoleEdgesPoints[{3,0}]};


//outer points in coil
CoilOuterPoints[] += newp; Point(newp) = {plate_width-2*coil_thick, 0, cp_dist + plate_thick, lc_coil};
CoilOuterPoints[] += newp; Point(newp) = {plate_width, 2*coil_thick, cp_dist + plate_thick, lc_coil};
CoilOuterPoints[] += newp; Point(newp) = {plate_width, coil_depth, cp_dist + plate_thick, lc_coil};
CoilOuterPoints[] += newp; Point(newp) = {plate_width-2*coil_thick, 2*coil_thick+coil_depth, cp_dist + plate_thick, lc_coil};
CoilOuterPoints[] += newp; Point(newp) = {plate_width-coil_width, 2*coil_thick+coil_depth, cp_dist + plate_thick, lc_coil};
CoilOuterPoints[] += newp; Point(newp) = {plate_width-2*coil_thick-coil_width, coil_depth, cp_dist + plate_thick, lc_coil};
CoilOuterPoints[] += newp; Point(newp) = {plate_width-2*coil_thick-coil_width, 2*coil_thick, cp_dist + plate_thick, lc_coil};
CoilOuterPoints[] += newp; Point(newp) = {plate_width-coil_width, 0, cp_dist + plate_thick, lc_coil};

// inner points in coil
CoilInnerPoints[] += newp; Point(newp) = {plate_width-2*coil_thick, coil_thick, cp_dist + plate_thick, lc_coil};
CoilInnerPoints[] += newp; Point(newp) = {plate_width-coil_thick, 2*coil_thick, cp_dist + plate_thick, lc_coil};
CoilInnerPoints[] += newp; Point(newp) = {plate_width-coil_thick, coil_depth, cp_dist + plate_thick, lc_coil};
CoilInnerPoints[] += newp; Point(newp)  = {plate_width-2*coil_thick, coil_thick+coil_depth, cp_dist + plate_thick, lc_coil};
CoilInnerPoints[] += newp; Point(newp) = {plate_width-coil_width, coil_depth+coil_thick, cp_dist + plate_thick, lc_coil};
CoilInnerPoints[] += newp; Point(newp) = {plate_width-coil_width-coil_thick, coil_depth, cp_dist + plate_thick, lc_coil};
CoilInnerPoints[] += newp; Point(newp) = {plate_width-coil_thick-coil_width, 2*coil_thick, cp_dist + plate_thick, lc_coil};
CoilInnerPoints[] += newp; Point(newp) = {plate_width-coil_width, coil_thick, cp_dist + plate_thick, lc_coil};

//fillet bottom right
FilletPoints[] += newp; Point(newp) = {plate_width-2*coil_thick, 2*coil_thick, cp_dist + plate_thick, lc_coil};
FilletPoints[] += newp; Point(newp) = {plate_width-2*coil_thick, coil_depth, cp_dist + plate_thick, lc_coil};
FilletPoints[] += newp; Point(newp) = {plate_width-coil_width, coil_depth, cp_dist + plate_thick, lc_coil};
FilletPoints[] += newp; Point(newp) = {plate_width-coil_width, 2*coil_thick, cp_dist + plate_thick, lc_coil};

// outer coil outline
CoilOuterLines[] += newl; Circle(newl) = {CoilOuterPoints[0],FilletPoints[0],CoilOuterPoints[1]};
CoilOuterLines[] += newl; Curve(newl) = {CoilOuterPoints[1],CoilOuterPoints[2]};
CoilOuterLines[] += newl; Circle(newl) = {CoilOuterPoints[2],FilletPoints[1],CoilOuterPoints[3]};
CoilOuterLines[] += newl; Curve(newl) = {CoilOuterPoints[3],CoilOuterPoints[4]};
CoilOuterLines[] += newl; Circle(newl) = {CoilOuterPoints[4],FilletPoints[2],CoilOuterPoints[5]};
CoilOuterLines[] += newl; Curve(newl) = {CoilOuterPoints[5],CoilOuterPoints[6]};
CoilOuterLines[] += newl; Circle(newl) = {CoilOuterPoints[6],FilletPoints[3],CoilOuterPoints[7]};
CoilOuterLines[] += newl; Curve(newl) = {CoilOuterPoints[7],CoilOuterPoints[0]};

// inner coil outline
CoilInnerLines[] += newl; Circle(newl) = {CoilInnerPoints[0],FilletPoints[0],CoilInnerPoints[1]};
CoilInnerLines[] += newl; Curve(newl) = {CoilInnerPoints[1],CoilInnerPoints[2]};
CoilInnerLines[] += newl; Circle(newl) = {CoilInnerPoints[2],FilletPoints[1],CoilInnerPoints[3]};
CoilInnerLines[] += newl; Curve(newl) = {CoilInnerPoints[3],CoilInnerPoints[4]};
CoilInnerLines[] += newl; Circle(newl) = {CoilInnerPoints[4],FilletPoints[2],CoilInnerPoints[5]};
CoilInnerLines[] += newl; Curve(newl) = {CoilInnerPoints[5],CoilInnerPoints[6]};
CoilInnerLines[] += newl; Circle(newl) = {CoilInnerPoints[6],FilletPoints[3],CoilInnerPoints[7]};
CoilInnerLines[] += newl; Curve(newl) = {CoilInnerPoints[7],CoilInnerPoints[0]};

// Auxiliary line in case electrode needed
//ElectrodeLine[]  += newl; Curve(newl) = {CoilInnerPoints[1],CoilOuterPoints[1]};
//ElectrodeLine[]  += newl; Curve(newl) = {CoilInnerPoints[2],CoilOuterPoints[2]};

// initial surfaces

HoleLineLoop[] += newll; Curve Loop(newll) = {HoleLines[0], HoleLines[1], HoleLines[2], HoleLines[3]};
//TopHoleSurface[] += news; Plane Surface(news) = {HoleLineLoop[]};


PlateLineLoop[] += newll; Curve Loop(newll) = {PlateLines[0], PlateLines[1], PlateLines[2], PlateLines[3]};
TopPlateSurface[] += news; Plane Surface(news) = {PlateLineLoop[], HoleLineLoop[]};


CoilInnerLineLoop[] += newll; Curve Loop(newll) = {CoilInnerLines[0], CoilInnerLines[1],
  CoilInnerLines[2], CoilInnerLines[3], CoilInnerLines[4], CoilInnerLines[5], CoilInnerLines[6], CoilInnerLines[7]};
//TopInnerCoilSurface[] += news; Plane Surface(news) = {CoilInnerLineLoop[]};

CoilOuterLineLoop[] += newll; Curve Loop(newll) = {CoilOuterLines[0], CoilOuterLines[1],
  CoilOuterLines[2], CoilOuterLines[3], CoilOuterLines[4], CoilOuterLines[5], CoilOuterLines[6], CoilOuterLines[7]};
TopOuterCoilSurface[] += news; Plane Surface(news) = {CoilOuterLineLoop[], CoilInnerLineLoop[]};

// volumes by extrusion 
PlateExtrusion[] = Extrude {0, 0, plate_thick} {
  Line{HoleLines[]}; Line{PlateLines[]}; Surface{TopPlateSurface[0]}; Layers{5}; 
 };
  
plate_skin() = Abs(Boundary{ Volume{PlateExtrusion[33]}; });

CoilExtrusion[] =  Extrude {0, 0, coil_height} {
  Line{CoilInnerLines[]}; Line{CoilOuterLines[]}; Surface{TopOuterCoilSurface[0]};/* Line{ElectrodeLine[{0,1}]};*/ Layers{10}; 
}; // Line{ElectrodeLine[0]};

coil_skin() = Abs(Boundary{ Volume{CoilExtrusion[65]}; });
//coil_skin() -= Abs(Boundary{ Surface{CoilExtrusion[83]}; });
//coil_skin() -= Abs(Boundary{ Surface{CoilExtrusion[87]}; });


//Air Doimain
boxt = 200*mm;
AirPoints[] += newp; Point(newp) = {-boxt, -boxt, -boxt, lc_air};
AirPoints[] += newp; Point(newp) = {294*mm+boxt, -boxt, -boxt, lc_air};
AirPoints[] += newp; Point(newp) = {294*mm+boxt, 294*mm+boxt, -boxt, lc_air};
AirPoints[] += newp; Point(newp) = {-boxt, 294*mm+boxt, -boxt, lc_air};

AirBottomLines[] += newl; Curve(newl) = {AirPoints[0],AirPoints[1]};
AirBottomLines[] += newl; Curve(newl) = {AirPoints[1],AirPoints[2]};
AirBottomLines[] += newl; Curve(newl) = {AirPoints[2],AirPoints[3]};
AirBottomLines[] += newl; Curve(newl) = {AirPoints[3],AirPoints[0]};

AirLineLoop[] += newll; Curve Loop(newll) = {AirBottomLines[0], AirBottomLines[1], AirBottomLines[2], AirBottomLines[3]};
AirBottomSurface[] += news; Plane Surface(news) = {AirLineLoop[]};


AirBottomExtrusion[] = Extrude {0, 0, 394*mm+boxt} {
  Line{AirBottomLines[]};
 };

AirLineLoop2[] += newll; Curve Loop(newll) = {AirBottomExtrusion[0], AirBottomExtrusion[4], AirBottomExtrusion[8], AirBottomExtrusion[12]};
AirBottomSurface2[] += news; Plane Surface(news) = {AirLineLoop2[]};


//Surface Loop
AirSurfaceLoop[] += newll; Surface Loop(newll) = {AirBottomSurface2[0], AirBottomExtrusion[1], AirBottomSurface[0], AirBottomExtrusion[5], AirBottomExtrusion[9], AirBottomExtrusion[13]};
PlateSurfaceLoop[] += newll; Surface Loop(newll) = {plate_skin()};
CoilSurfaceLoop[] += newll; Surface Loop(newll) =  {coil_skin()};

AirVolume[] += newv;  Volume(newv) =  {AirSurfaceLoop[], PlateSurfaceLoop[], CoilSurfaceLoop[]};
// Get infinity surfaces by removing everything that's inside (plate and coil)
infinity() = Abs(Boundary{ Volume{AirVolume[0]}; });
infinity() -= Abs(Boundary{ Volume{PlateExtrusion[33]}; });
infinity() -= Abs(Boundary{ Volume{CoilExtrusion[65]}; });


// Physical Domains

COIL     = 1;
COILSKIN = 4;

PLATE     = 2;
PLATESKIN = 5;

AIR = 3;
INF = 6;

ELECTRODE1 = 7;
ELECTRODE2 = 8;


// Elmer Post Processing Test Aid 
PostProcessingPoints_bz1[] += newp; Point(newp) = {0, 72e-3, 34e-3, lc_plate};
PostProcessingPoints_bz1[] += newp; Point(newp) = {288e-3, 72e-3, 34e-3, lc_plate};
PostProcessingLines_bz1[] += newl; Line(newl) = {PostProcessingPoints_bz1[{0,1}]};

PostProcessingPoints_bz2[] += newp; Point(newp) = {0, 144e-3, 34e-3, lc_plate};
PostProcessingPoints_bz2[] += newp; Point(newp) = {288e-3, 144e-3, 34e-3, lc_plate};
PostProcessingLines_bz2[] += newl; Line(newl) = {PostProcessingPoints_bz2[{0,1}]};

/*
PostProcessingPoints_jy1[] += newp; Point(newp) = {0, 72e-3, 19e-3, lc_plate};
PostProcessingPoints_jy1[] += newp; Point(newp) = {288e-3, 72e-3, 19e-3, lc_plate};
PostProcessingLines_jy1[] += newl; Line(newl) = {PostProcessingPoints_jy1[{0,1}]};

PostProcessingPoints_jy2[] += newp; Point(newp) = {0, 72e-3, 0, lc_plate};
PostProcessingPoints_jy2[] += newp; Point(newp) = {288e-3, 72e-3, 0, lc_plate};
PostProcessingLines_jy2[] += newl; Line(newl) = {PostProcessingPoints_jy2[{0,1}]};
*/

Transfinite Curve {PostProcessingLines_bz1[0], PostProcessingLines_bz2[0]} = 60 Using Progression 1;

//Embedd Lines in Mesh Volume (Ensuring mesh nodes are on line)
Line{PostProcessingLines_bz1[0]} In Volume{AirVolume[0]};
Line{PostProcessingLines_bz2[0]} In Volume{AirVolume[0]};
//Line{PostProcessingLines_jy1[0]} In Volume{PlateExtrusion[33]};
//Line{PostProcessingLines_jy2[0]} In Volume{PlateExtrusion[33]};

//Surface{CoilExtrusion[83]} In Volume{CoilExtrusion[65]};


Physical Volume("Coil", COIL) = {CoilExtrusion[65]};
Physical Volume("Plate", PLATE) = {PlateExtrusion[33]};
Physical Volume("Air", AIR) = {AirVolume[0]};

Physical Surface("CoilSkin", COILSKIN) = {TopOuterCoilSurface[0], coil_skin()};
Physical Surface("PlateSkin", PLATESKIN) = {TopPlateSurface[0], plate_skin()};
Physical Surface("Inf", INF) = {infinity()};
//Physical Surface("Cut", ELECTRODE1) = {CoilExtrusion[83]};
//Physical Surface("Electrode2", ELECTRODE2) = {CoilExtrusion[87]};


//+

