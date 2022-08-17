SetFactory("OpenCASCADE");
waveguide = 0.011;
potato = 0.001;

Point(1) = {0.086,0,0.2,waveguide};
Point(2) = {0.086,0.043,0.2,waveguide};
Point(3) = {0,0.043,0.2,waveguide};
Point(4) = {0,0,0.2,waveguide};
Point(5) = {0.086,0,0,waveguide};
Point(6) = {0.086,0.043,0,waveguide};
Point(7) = {0,0,0,waveguide};
Point(8) = {0,0.043,0,waveguide};
Point(9) = {0.05, 0.0175, 0.11,potato};
Point(10) = {0.05, 0.0275, 0.11,potato};
Point(11) = {0.03, 0.0275, 0.11,potato};
Point(12) = {0.03, 0.0175, 0.11,potato};
Point(13) = {0.05, 0.0175, 0.1,potato};
Point(14) = {0.05, 0.0275, 0.1,potato};
Point(15) = {0.03, 0.0175, 0.1,potato};
Point(16) = {0.03, 0.0275, 0.1,potato};
//+
Line(1) = {6, 5};
//+
Line(2) = {6, 8};
//+
Line(3) = {8, 7};
//+
Line(4) = {5, 7};
//+
Line(5) = {7, 4};
//+
Line(6) = {8, 3};
//+
Line(7) = {6, 2};
//+
Line(8) = {5, 1};
//+
Line(9) = {3, 2};
//+
Line(10) = {4, 3};
//+
Line(11) = {2, 1};
//+
Line(12) = {4, 1};
//+
Line(13) = {14, 16};
//+
Line(14) = {14, 13};
//+
Line(15) = {13, 15};
//+
Line(16) = {15, 16};
//+
Line(17) = {13, 9};
//+
Line(18) = {9, 10};
//+
Line(19) = {10, 14};
//+
Line(20) = {10, 11};
//+
Line(21) = {11, 12};
//+
Line(22) = {12, 9};
//+
Line(23) = {12, 15};
//+
Line(24) = {16, 11};
//+
Curve Loop(1) = {4, -3, -2, 1};
//+
Plane Surface(1) = {1};
//+
Curve Loop(2) = {8, -11, -7, 1};
//+
Plane Surface(2) = {2};
//+
Curve Loop(3) = {8, -12, -5, -4};
//+
Plane Surface(3) = {3};
//+
Curve Loop(4) = {6, 9, -7, 2};
//+
Plane Surface(4) = {4};
//+
Curve Loop(5) = {9, 11, -12, 10};
//+
Plane Surface(5) = {5};
//+
Curve Loop(6) = {6, -10, -5, -3};
//+
Plane Surface(6) = {6};
//+
Curve Loop(7) = {20, -24, -13, -19};
//+
Plane Surface(7) = {7};
//+
Curve Loop(8) = {20, 21, 22, 18};
//+
Plane Surface(8) = {8};
//+
Curve Loop(9) = {22, -17, 15, -23};
//+
Plane Surface(9) = {9};
//+
Curve Loop(10) = {14, 15, 16, -13};
//+
Plane Surface(10) = {10};
//+
Curve Loop(11) = {24, 21, 23, 16};
//+
Plane Surface(11) = {11};
//+
Curve Loop(12) = {19, 14, 17, 18};
//+
Plane Surface(12) = {12};
//+
Surface Loop(1) = {1, 3, 2, 5, 4, 6};
//+
Volume(1) = {1};
//+
Surface Loop(2) = {9, 8, 7, 11, 10, 12};
//+
Volume(2) = {2};


BooleanDifference{ Volume{1}; Delete; }{ Volume{2}; }
//+


//+
Coherence;
