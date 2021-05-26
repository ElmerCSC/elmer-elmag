algebraic3d

# ------------- Waveguide -------------
solid UP_PIPE = plane(0,0,0;0,0,-1);
solid DOWN_PIPE = plane(0,0,300;0,0,1);
solid PIPE = cylinder(0,0,-100;0,0,400;80);

solid WAVEGUIDE = PIPE and DOWN_PIPE and UP_PIPE;

# ------------- Cavity -------------
solid UP_BOX = plane(0,0,240;0,0,-1);
solid DOWN_BOX = plane(0,0,720;0,0,1);
solid BOX = cylinder(0,0,200;0,0,800;151);

solid CAVITY = BOX and DOWN_BOX and UP_BOX;

# --------- Join waveguide + cavity ---------
solid MIC = WAVEGUIDE or CAVITY;

tlo MIC;		# generate object
