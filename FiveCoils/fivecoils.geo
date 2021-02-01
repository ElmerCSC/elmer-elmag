algebraic3d
solid air = sphere(0.0,0.0,0.0; 2.500);
solid coil1 = torus( 0.809, 0.588,0.0;  0.588,-0.809, 0.0;  0.500;  0.100);
solid coil2 = torus(-0.309, 0.951,0.0;  0.951, 0.309, 0.0;  0.500;  0.100);
solid coil3 = torus(-1.000, 0.000,0.0;  0.000, 1.000, 0.0;  0.500;  0.100);
solid coil4 = torus(-0.309,-0.951,0.0; -0.951, 0.309, 0.0;  0.500;  0.100);
solid coil5 = torus( 0.809,-0.588,0.0; -0.588,-0.809, 0.0;  0.500;  0.100);
solid rest = air and not coil1 and not coil2 and not coil3 and not coil4 and not coil5;
tlo rest -col=[0,0,1];
tlo coil1;
tlo coil2;
tlo coil3;
tlo coil4;
tlo coil5;
