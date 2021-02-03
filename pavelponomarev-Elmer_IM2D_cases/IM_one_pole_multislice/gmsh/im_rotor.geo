// May 2013 - Authors: J. Gyselinck, R.V. Sabariego
// Modified July 2016 -- P. Ponomarev

Geometry.AutoCoherence = 0;
Mesh.Light = 0;

Qr = 40; // number of rotor teeth
N_rs = 10;

//Main rotor parameters
R_rin = 27;     // inner rotor radius
R_rout = 62;      // outer stator radius
R_g = 62.25;       // outer radius of moving band

//Slot dimensions
b_1 = 1;
h_1 = 0.4;
b_4 = 4.4;
b_5 = 2;
h_5 = 12;

//offset for the curvature of the slot opening in the gap
s = R_rout-Sqrt((R_rout)^2-(b_1/2)^2);

//ofsett of the top arc curvature at the tip
s2 = b_4/2-Sqrt((b_4/2)^2-(b_1/2)^2);

//Mesh density
m_coarse = 10;
m_normal = 8;
m_gap = 0.5;
m_sl_bot = 4;
m_sl_top = 1;
m_r_in = 10;




//build rotor slots
For i In {0:N_rs-1}

  //build two halfs
  For half In {0:1}
	//points of one half
    dP=newp;
    Point(dP+0)  = {0,0,0,m_coarse};
	
	//right toothtip points
    Point(dP+1)  = {b_1/2, R_rout-s, 0, 1.4*m_gap};
    Point(dP+2)  = {b_1/2, R_rout-h_1-s2, 0, m_sl_top};
	
	//center of the bottom circle
	Point(dP+3)  = {0, R_rout-h_1-b_4/2-h_5, 0, m_sl_bot};

	//b4 top circle side
    Point(dP+4)  = {b_4/2, R_rout-h_1-b_4/2, 0, 2*m_sl_top};
	
	//b5 bottom circle side
    Point(dP+5)  = {b_5/2, R_rout-h_1-b_4/2-h_5, 0, m_sl_bot};

	// central point of top circle
    Point(dP+6)  = {0, R_rout-h_1-b_4/2, 0, m_sl_top};
	
	// bottom slot point
    Point(dP+7)  = {0, R_rout-h_1-b_4/2-h_5-b_5/2, 0, m_sl_bot};

	// inner rotor sector
    Point(dP+8) = {R_rin*Sin(Pi/Qr), R_rin*Cos(Pi/Qr), 0, m_r_in};
	
	// inner rotor center
    Point(dP+9) = {0, R_rin, 0, m_r_in};

	// outer rotor sector
    Point(dP+10) = {R_rout*Sin(Pi/Qr), R_rout*Cos(Pi/Qr), 0, 2.5*m_gap};

	// sliding sector
    Point(dP+11) = {R_g*Sin(Pi/Qr), R_g*Cos(Pi/Qr), 0, 2.8*m_gap};
	
	//outer rotor center
    Point(dP+12) = {0, R_rout, 0, 1.5*m_gap};
	
	//sliding center
    Point(dP+13) = {0, R_g, 0, 1.4*m_gap};
	
	// rotate the built points to the i-th slot position
    For t In {dP+0:dP+13}
      Rotate {{0,0,1},{0,0,0}, 2*Pi*i/Qr+2*Pi/Qr/2} {Point{t};}
    EndFor
	
    If (half==1) //second half
      For t In {dP+0:dP+13}
        Symmetry {Cos(2*Pi*i/Qr+2*Pi/Qr/2),Sin(2*Pi*i/Qr+2*Pi/Qr/2),0,0} { Point{t}; }
      EndFor
    EndIf
	
	
    dR=newl-1;
	//vertical tooth tip line
    Line(dR+1) = {dP+1,dP+2};   

	//top arc
	Circle(dR+2) = {dP+2,dP+6,dP+4};
	
	//vertical slot side
    Line(dR+3) = {dP+4,dP+5};   

	//arc slot bottom
    Circle(dR+4) = {dP+5,dP+3,dP+7}; 
		
	//sliding 
	Circle(dR+5) = {dP+11,dP+0,dP+13};

	//tooth top arc
    Circle(dR+6) = {dP+10,dP+0,dP+1};

	//arc slot opening
    Circle(dR+7) = {dP+1,dP+0,dP+12};
	
	//inner rotor
    Circle(dR+8) = {dP+8,dP+0,dP+9};
	
	//vertical central slot opening
    Line(dR+9) = {dP+12,dP+7};
	
	//vertical from bot of the slot to the rotor inner
    Line(dR+10) = {dP+7,dP+9};
	
	//sector border via steel
    Line(dR+11) = {dP+10,dP+8};

		
	//sector border via airgap
    Line(dR+12) = {dP+11,dP+10};

	//vertical sector center via gap
    Line(dR+13) = {dP+13,dP+12};
	
	//filling the lists for boundaries
    InnerRotor_[] += dR+8;
	
    RotorBoundary_[] += {dR+8,dR+4,dR+3,dR+2,dR+1,dR+6};

	SlidingR_[] += {dR+5};
		
	//Periodic boundary
    If (Qr != N_rs)
	  //right boundary
      If (i==0 && half==0)
        RotorPeriod_Right_[] = {dR+11,dR+12};
      EndIf
	  //left boundary
      If (i == N_rs-1  && half==1)
        RotorPeriod_Left_[] = {dR+11,dR+12};
      EndIf
    EndIf

	//if mirrorred, then the lines order is reversed
	//direction is important defining the Line Loops
    rev = (half ? -1 : 1);
		
	//surface of the slot conductors
    Line Loop(newll) = {dR+9,-dR-4,-dR-3,-dR-2,-dR-1,dR+7};
    dH = news; Plane Surface(news) = -rev*{newll-1};
    RotorConductor_[] += dH;
	
	//surface of the stator iron
    Line Loop(newll) = {dR+1,dR+2,dR+3,dR+4,dR+10,-dR-8,-dR-11,dR+6};
    dH = news; Plane Surface(news) = -rev*{newll-1};
    RotorIron_[] += dH;
	
	//airgap stator
    Line Loop(newll) = {-dR-13,-dR-5,dR+12,dR+6,dR+7};
    dH = news; Plane Surface(news) = rev*{newll-1};
    RotorAirgapLayer_[] += dH;

  EndFor
EndFor

//----------------------------------------------------------------------------------------
// Physical regions
//----------------------------------------------------------------------------------------

Color Yellow {Surface{RotorConductor_[]};}

Physical Surface("Bar1") = {RotorConductor_[{0,1}]};
Physical Surface("Bar2") = {RotorConductor_[{2,3}]};
Physical Surface("Bar3") = {RotorConductor_[{4,5}]};
Physical Surface("Bar4") = {RotorConductor_[{6,7}]};
Physical Surface("Bar5") = {RotorConductor_[{8,9}]};
Physical Surface("Bar6") = {RotorConductor_[{10,11}]};
Physical Surface("Bar7") = {RotorConductor_[{12,13}]};
Physical Surface("Bar8") = {RotorConductor_[{14,15}]};
Physical Surface("Bar9") = {RotorConductor_[{16,17}]};
Physical Surface("Bar10") = {RotorConductor_[{18,19}]};

Physical Surface("RotorIron") = {RotorIron_[]};
Physical Surface("RotorAirgap") = {RotorAirgapLayer_[]};

Color SteelBlue {Surface{RotorIron_[]};}
Color SkyBlue {Surface{RotorAirgapLayer_[]};}

Physical Line("InnerRotor") = {InnerRotor_[]};
Physical Line("RotorRight") = {RotorPeriod_Right_[]};
Physical Line("RotorLeft") = {RotorPeriod_Left_[]};
Physical Line("Sliding_Rotor") = {SlidingR_[]};

Coherence;


show_rotor[] = CombinedBoundary{Surface{RotorIron_[]};};
Hide{ Point{Point '*'};}
Hide{ Line{Line '*'};}
Show{ Line{show_rotor[]};}

Mesh 2;



