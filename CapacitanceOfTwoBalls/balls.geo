//+
SetFactory("OpenCASCADE");
Sphere(1) = {1, 0, 0, 0.5, -Pi/2, Pi/2, 2*Pi};
//+
Sphere(2) = {-1, 0, 0, 0.5, -Pi/2, Pi/2, 2*Pi};
//+
Sphere(3) = {0, 0, 0, 5.0, -Pi/2, Pi/2, 2*Pi};


BooleanDifference(4) = { Volume{3}; Delete; }{ Volume{1}; Delete; };
BooleanDifference(5) = { Volume{4}; Delete; }{ Volume{2}; Delete; };

