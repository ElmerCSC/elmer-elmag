// May 2013 - Authors: J. Gyselinck, R.V. Sabariego
// Modified July 2016 -- P. Ponomarev

Geometry.AutoCoherence = 0;

//Stator
Qs = 48; // number of stator teeth
N_ss = 12;

//Main Stator parameters
R_sin = 62.5;     // inner stator radius
R_sout = 110;      // outer stator radius
R_g = 62.25;       // middle of the air gap radius

//Slot dimensions
b_5 = 6.8;	
b_1 = 2.8;
h_1 = 0.7;
h_2 = 0.2;
b_4 = 4;
h_5 = 24;

//Mesh density
m_coarse = 20;
m_normal = 12;
m_gap = 0.55;
m_sl_top = 7;
m_sl_bot = 2;
m_s_out = 12;

//offset for the curvature of the slot opening in the gap
s = R_sin-Sqrt(R_sin^2-(b_1/2)^2);


//build stator slots
For i In {0:N_ss-1}

  //build two halfs
  For half In {0:1}

	//points of one half
    dP=newp;
    Point(dP+0)  = {0,0,0,m_coarse};
	
	//right toothtip points
    Point(dP+1)  = {b_1/2, R_sin-s, 0, 2*m_gap};
    Point(dP+2)  = {b_1/2, R_sin-s+h_1, 0, m_sl_bot};
	
	//center of the top circle
	Point(dP+3)  = {0, R_sin-s+h_1+h_2+h_5, 0, m_sl_top};
	
	//h2-b4 bottom teeth point
    Point(dP+4)  = {b_4/2, R_sin-s+h_1+h_2, 0, m_sl_bot};
	
	//h5-b5 top teeth point
    Point(dP+5)  = {b_5/2, R_sin-s+h_1+h_2+h_5, 0, m_sl_top};
	
	// point of lower winding
    Point(dP+6)  = {b_4/2, R_sin-s+h_1+h_2+0.5, 0, m_sl_bot/2};
	
	// central point of lower winding
    Point(dP+7)  = {0, R_sin-s+h_1+h_2+0.5, 0, m_sl_bot};
	
	// top slot point
    Point(dP+8)  = {0, R_sin-s+h_1+h_2+h_5+b_5/2, 0, m_sl_top};
	
	// outer stator sector
    Point(dP+9) = {R_sout*Sin(Pi/Qs), R_sout*Cos(Pi/Qs), 0, m_s_out};
	
	// outer stator center
    Point(dP+10) = {0, R_sout, 0, m_s_out};
	
	// inner stator sector
    Point(dP+11) = {R_sin*Sin(Pi/Qs), R_sin*Cos(Pi/Qs), 0, 2.5*m_gap};
	
	// sliding sector
    Point(dP+12) = {R_g*Sin(Pi/Qs), R_g*Cos(Pi/Qs), 0, 2.4*m_gap};
	
	//inner stator center
    Point(dP+13) = {0, R_sin, 0, 1.5*m_gap};
	
	//sliding center
    Point(dP+14) = {0, R_g, 0, 1.5*m_gap};
	
    // rotate the built points to the i-th slot position
    For t In {dP+0:dP+14}
      Rotate {{0,0,1},{0,0,0}, 2*Pi*i/Qs+2*Pi/Qs/2} {Point{t};}
    EndFor
	
    If (half==1) //second half
      For t In {dP+0:dP+14}
        Symmetry {Cos(2*Pi*i/Qs+2*Pi/Qs/2),Sin(2*Pi*i/Qs+2*Pi/Qs/2),0,0} { Point{t}; }
      EndFor
    EndIf	
	
    dR=newl-1;
	//vertical tooth tip line
    Line(dR+1) = {dP+1,dP+2};   
	
	//line from tooth tip arc to start of the winding
    Line(dR+2) = {dP+4,dP+6};
	
	//vertical slot side
    Line(dR+3) = {dP+6,dP+5};   
	
	//horizontal winding bottom
    Line(dR+4) = {dP+6,dP+7};  
	
	//arc toothtip - side
	Line(dR+5) = {dP+2,dP+4};
	
	//arc top of the slot-side
    Circle(dR+6) = {dP+5,dP+3,dP+8}; 
	
	//sliding 
	Circle(dR+7) = {dP+12,dP+0,dP+14};
	
	//slot opening arc
    Circle(dR+8) = {dP+11,dP+0,dP+1};
	
	//arc inner teeth surface
    Circle(dR+9) = {dP+1,dP+0,dP+13};
	
	//outer stator
    Circle(dR+10) = {dP+9,dP+0,dP+10};
	
	//vertical central slot opening
    Line(dR+11) = {dP+13,dP+7};
	
	//vertical in the center of the slot
    Line(dR+12) = {dP+7,dP+8};   
	
	//vertical from top of the slot to the stator outer
    Line(dR+13) = {dP+8,dP+10};
	
	//sector border via steel
    Line(dR+14) = {dP+11,dP+9};
	
	//sector border via airgap
    Line(dR+15) = {dP+12,dP+11};
	
	//vertical sector center via gap
    Line(dR+16) = {dP+14,dP+13};

    //filling the lists for boundaries
    OuterStator_[] += dR+10;
    StatorBoundary_[] += {dR+10,dR+6,dR+3,dR+2,dR+5,dR+1,dR+8};
	Sliding_[] += {dR+7};
		
	//Periodic boundary
    If (Qs != N_ss)
	  //right boundary
      If (i==0 && half==0)
        StatorPeriod_Right_[] = {dR+14,dR+15};
      EndIf
	  //left boundary
      If (i == N_ss-1  && half==1)
        StatorPeriod_Left_[] = {dR+14,dR+15};
      EndIf
    EndIf

	//if mirrorred, then the lines order is reversed
	//direction is important defining the Line Loops
    rev = (half ? -1 : 1);
	
	//surface of the slot conductors
    Line Loop(newll) = {dR+12,-dR-6,-dR-3,dR+4};
    dH = news; Plane Surface(news) = -rev*{newll-1};
    StatorConductor_[] += dH;

	//surface of the stator iron
    Line Loop(newll) = {dR+1,dR+5,dR+2,dR+3,dR+6,dR+13,-dR-10,-dR-14,dR+8};
    dH = news; Plane Surface(news) = -rev*{newll-1};
    StatorIron_[] += dH;

	//wedges
    Line Loop(newll) = {dR+11,-dR-4,-dR-2,-dR-5,-dR-1,dR+9};
    dH = news; Plane Surface(news) = -rev*{newll-1};
    StatorWedge_[] += dH;

	//airgap stator
    Line Loop(newll) = {-dR-16,-dR-7,dR+15,dR+8,dR+9};
    dH = news; Plane Surface(news) = rev*{newll-1};
    StatorAirgapLayer_[] += dH;



  EndFor
EndFor



//----------------------------------------------------------------------------------------
// Physical regions
//----------------------------------------------------------------------------------------

//Assigning StatorConductors to phase Regions
//number of slots in a belt
qq=4;

//for each phase side
For f In {0:5}
  Con[]={};
  For i In {0:N_ss/qq-1}
    If (Fmod(i,6) == f)
      For j In {0:qq-1}
        Con[] += StatorConductor_[{2*i*qq+2*j, 2*i*qq+2*j+1}];
      EndFor
    EndIf
  EndFor
  
  //color the phase and assign to a physical region
  If (#Con[] > 0)
    If (f == 0) 
		Color Red {Surface{Con[]};}
		Physical Surface("U_plus") = {Con[]};
    EndIf
    If (f == 1) 
		Color SpringGreen {Surface{Con[]};}
		Physical Surface("W_minus") = {Con[]};
    EndIf
    If (f == 2)
		Color Gold {Surface{Con[]};}
		Physical Surface("V_plus") = {Con[]};
    EndIf
    If (f == 3) 
		Color Pink {Surface{Con[]};}
		Physical Surface("U_minus") = {Con[]};
    EndIf
    If (f == 4)
		Color ForestGreen {Surface{Con[]};}
		Physical Surface("W_plus") = {Con[]};
    EndIf
    If (f == 5) 
		Color PaleGoldenrod {Surface{Con[]};}
		Physical Surface("V_minus") = {Con[]};
    EndIf
  EndIf
EndFor


Physical Surface("StatorIron") = {StatorIron_[]};
Physical Surface("StatorWedges") = {StatorWedge_[]};
Physical Surface("StatorAirgap") = {StatorAirgapLayer_[]};

Color SteelBlue {Surface{StatorIron_[]};}
Color Black {Surface{StatorWedge_[]};}
Color SkyBlue {Surface{StatorAirgapLayer_[]};}

Physical Line("OuterStator") = {OuterStator_[]};

Physical Line("StatorRight") = {StatorPeriod_Right_[]};
Physical Line("StatorLeft") = {StatorPeriod_Left_[]};

Physical Line("Sliding_Stator") = {Sliding_[]};

Coherence;

show_stator[] = CombinedBoundary{Surface{StatorIron_[]};};
Hide{ Point{Point '*'};}
Hide{ Line{Line '*'};}
Show{ Line{show_stator[]};}


Mesh 2;
