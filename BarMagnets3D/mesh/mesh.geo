// Gmsh project created on Mon Jul 24 09:22:14 2023

lm = 1; // mags
lc = 1000.0; // region

SetFactory("OpenCASCADE");

Point(1) = {-110, 15, 2.5, lm};
Point(2) = {-10, 15, 2.5, lm};
Point(3) = {-10, -15, 2.5, lm};
Point(4) = {-110, -15, 2.5, lm};
Point(5) = {-110, -15, -2.5, lm};
Point(6) = {-10, -15, -2.5, lm};
Point(7) = {-10, 15, -2.5, lm};
Point(8) = {-110, 15, -2.5, lm};
Point(9) = {10, 15, -2.5, lm};
Point(10) = {10, -15, -2.5, lm};
Point(11) = {110, -15, -2.5, lm};
Point(12) = {110, 15, -2.5, lm};
Point(13) = {110, 15, 2.5, lm};
Point(14) = {110, -15, 2.5, lm};
Point(15) = {10, -15, 2.5, lm};
Point(16) = {10, 15, 2.5, lm};

Line(1) = {1, 2};
Line(2) = {2, 7};
Line(3) = {7, 6};
Line(4) = {6, 3};
Line(5) = {3, 2};
Line(6) = {3, 4};
Line(7) = {4, 5};
Line(8) = {5, 6};
Line(9) = {8, 5};
Line(10) = {4, 1};
Line(11) = {1, 8};
Line(12) = {8, 7};
Line(13) = {9, 12};
Line(14) = {12, 11};
Line(15) = {11, 10};
Line(16) = {10, 9};
Line(17) = {9, 16};
Line(18) = {15, 10};
Line(19) = {12, 13};
Line(20) = {11, 14};
Line(21) = {14, 13};
Line(22) = {13, 16};
Line(23) = {16, 15};
Line(24) = {15, 14};

Curve Loop(1) = {16, 17, 23, 18};
Plane Surface(1) = {1};

Curve Loop(2) = {14, 20, 21, -19};
Plane Surface(2) = {2};

Curve Loop(5) = {18, -15, 20, -24};
Plane Surface(3) = {5};

Curve Loop(6) = {13, 19, 22, -17};
Plane Surface(4) = {6};

Curve Loop(7) = {13, 14, 15, 16};
Plane Surface(5) = {7};

Curve Loop(8) = {23, 24, 21, 22};
Plane Surface(6) = {8};

Curve Loop(9) = {3, -8, -9, 12};
Plane Surface(7) = {9};

Curve Loop(10) = {9, -7, 10, 11};
Plane Surface(8) = {10};

Curve Loop(11) = {2, 3, 4, 5};
Plane Surface(9) = {11};

Curve Loop(12) = {8, 4, 6, 7};
Plane Surface(10) = {12};

Curve Loop(13) = {12, -2, -1, 11};
Plane Surface(11) = {13};

Curve Loop(14) = {1, -5, 6, 10};
Plane Surface(12) = {14};

Surface Loop(1) = {12, 11, 7, 9, 10, 8};
Volume(1) = {1};

Surface Loop(2) = {6, 1, 5, 4, 2, 3};
Volume(2) = {2};

Point(17) = {-8000, -8000, -8000, lc};
Point(18) = {-8000, -8000, 8000, lc};
Point(19) = {-8000, 8000, -8000, lc};
Point(20) = {-8000, 8000, 8000, lc};
Point(21) = {8000, 8000, 8000, lc};
Point(22) = {8000, -8000, 8000, lc};
Point(23) = {8000, -8000, -8000, lc};
Point(24) = {8000, 8000, -8000, lc};

Line(25) = {21, 22};
Line(26) = {22, 23};
Line(27) = {23, 24};
Line(28) = {24, 21};
Line(29) = {21, 20};
Line(30) = {20, 18};
Line(31) = {18, 22};
Line(32) = {17, 23};
Line(33) = {17, 18};
Line(34) = {17, 19};
Line(35) = {19, 24};
Line(36) = {19, 20};

Curve Loop(15) = {27, -35, -34, 32};
Plane Surface(13) = {15};

Curve Loop(16) = {27, 28, 25, 26};
Plane Surface(14) = {16};

Curve Loop(17) = {32, -26, -31, -33};
Plane Surface(15) = {17};

Curve Loop(18) = {36, -29, -28, -35};
Plane Surface(16) = {18};

Curve Loop(19) = {34, 36, 30, -33};
Plane Surface(17) = {19};

Curve Loop(20) = {30, 31, -25, 29};
Plane Surface(18) = {20};

Surface Loop(8) = {18, 17, 13, 14, 16, 15};
Surface Loop(9) = {12, 11, 7, 9, 10, 8};
Surface Loop(10) = {6, 1, 5, 4, 2, 3};
Volume(3) = {8, 9, 10};
