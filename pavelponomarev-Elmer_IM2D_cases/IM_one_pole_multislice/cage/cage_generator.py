# Elmer circuit equations generator for a cage winding taking into account periodicity
# Author: P.Ponomarev 
# July 2016
# changelog:
# version 1.3 (03.2017) by PP: 
#       - added offset of the body numbers boffset
#    

from __future__ import print_function

# Settings:
ns = 4
nob = 10             # number of rotor bars simulated
boffset = 1        #number of the first bar body
antiperiodic = 1    # periodic or antiperiodic boundary
cn = 4              # circuit number which describes the rotor bars
ctype = "Stranded"   # Coil type Massive/Stranded
OUTFILE = 'cage.definitions'

# Rotor circuit
# Bar 1 to Bar N are FEM components of the modelled domain
# L_N and R_N are bar-to-bar inductance and resistance of the rotor end rings
# For 1-pole model (antiperiodic):
#    terminal 1 is connected to 2' and 2 is connected to 1'
# For 2-pole model (periodic):
#    terminal 1 is connected to 1' and 2 is connected to 2'
#                  (i,v)
#     1'           (0,1)           2'
#     O       + _________ -  I_bar O
#     |________|  Bar 1  |____\____|
#   + |        |_________|    /    |  
#    C                            C +
#    C  L_1l   _____________      C  L_1r 
#    C        |   U_loop    |     C
#     | (4,5) |             |      |   (2,3)  
#    <        |            \|/    <   
#    <  R_1l  |___          V     <  R_1r    
#    <                            <   
#   - |       + _________ -        |-       
#     |________|  Bar 2  |_________|
#     |        |_________|         |
#    C             (6,7)          C
#    C  L_2l                      C  L_2r
#    C                            C
#     |   (10,11)                  |      (8,9)
#    <                            <   
#    <  R_2l                      <  R_2r    
#    <                            <   
#     |                            |   
#     
#    ...
#               _________       
#     |________|  Bar N  |_________|
#     |        |_________|         |
#    C                            C
#    C  L_Nl                      C  L_Nr 
#    C                            C
#     |                            |      
#    <                            <   
#    <  R_Nl                      <  R_Nr    
#    <                            <   
#     |                            |
#     O                            O   
#     1                            2





barstxt = ""

###############################################################################
### Filling components section 
###############################################################################
# Coil Type can be Massive or Stranded
# assuming that rotor bar bodies are numbered 
# consequently starting from 1 onwards to N, where 1 and N are closest to 
# periodic boundaries bars:

for nbar in range(1,nob+1):
    s = "Component " + str(nbar) + "\n" + \
        "  Name = String RB" + str(nbar) + "\n" + \
        "  Body = Integer " + str(nbar+boffset-1) + "\n" + \
        "  Coil Type = String "+ ctype + "\n" + \
        "  Number of Turns = Real $" + str(ns)+"\n" + \
        "End" + "\n\n"
    barstxt = barstxt + s
    
###############################################################################
### Declare variables
###############################################################################

# first, the dimensions of the variable arrays are declared
s = "!----------------------------------------------------------\n" + \
    "! Equations for " + str(nob) + " rotor bars\n" + \
    "!----------------------------------------------------------\n\n" + \
    "$ C." + str(cn) + ".source.1 = 0\n\n" + \
    "! init matrices of Ax' + Bx = Source\n" + \
    "$ C." + str(cn) + ".variables = " + str(nob*3*2)+ "\n" + \
    "$ C." + str(cn) + ".perm = zeros(" + str(nob*3*2)+ ")\n" + \
    "$ C." + str(cn) + ".A = zeros(" + str(nob*3*2) + ", " + str(nob*3*2) + ")\n" + \
    "$ C." + str(cn) + ".B = zeros(" + str(nob*3*2) + ", " + str(nob*3*2) + ")\n" + \
    "$ C." + str(cn) + ".Mre = zeros(" + str(nob*3*2) + ", " + str(nob*3*2) + ")\n" + \
    "$ C." + str(cn) + ".Mim = zeros(" + str(nob*3*2) + ", " + str(nob*3*2) + ")\n" + \
    "! define circuit variables\n\n" 

barstxt = barstxt + s

# then, each variable receives its unique name
# each component and element is described by 2 circuit variables - "u" and "i"
# each bar is associated with 2 sections of the end ring - left (l) and right (r)
# each section is described by one single element of the circuit possesing R and L.

for nbar in range(0,nob):
    s = "$ C." + str(cn) + ".name." + str(nbar*6 + 1) + " = \"i_component(" + str(nbar+1) + ")\"\n" + \
        "$ C." + str(cn) + ".name." + str(nbar*6 + 2) + " = \"v_component(" + str(nbar+1) + ")\"\n" + \
        "$ C." + str(cn) + ".name." + str(nbar*6 + 3) + " = \"i_r" + str(nbar+1) + "\"\n" + \
        "$ C." + str(cn) + ".name." + str(nbar*6 + 4) + " = \"v_r" + str(nbar+1) + "\"\n" + \
        "$ C." + str(cn) + ".name." + str(nbar*6 + 5) + " = \"i_l" + str(nbar+1) + "\"\n" + \
        "$ C." + str(cn) + ".name." + str(nbar*6 + 6) + " = \"v_l" + str(nbar+1) + "\"\n\n\n"
    
    barstxt = barstxt + s

###############################################################################
### Kirchoff voltage law
###############################################################################

# describes voltages in each loop between two bars. Hence, each circuit segment
# contains 4 components(elements)
# loops directed clockwise
s = "! Kirchoff voltage law\n\n"
barstxt = barstxt + s

for nbar in range(0,nob-1):
    s = "!Bar" + str(nbar+1) + "\n" + \
        "$ C." + str(cn) + ".B(" + str(nbar*6+2) + "," + str(nbar*6+1) + ") = 1\n" + \
        "$ C." + str(cn) + ".B(" + str(nbar*6+2) + "," + str(nbar*6+3) + ") = 1\n" + \
        "$ C." + str(cn) + ".B(" + str(nbar*6+2) + "," + str(nbar*6+5) + ") = -1\n" + \
        "$ C." + str(cn) + ".B(" + str(nbar*6+2) + "," + str(nbar*6+7) + ") = -1\n\n"
    barstxt = barstxt + s

# last bar includes periodicity definition
s = "!Bar" + str(nob) + "\n" + \
    "$ C." + str(cn) + ".B(" + str((nob-1)*6+2) + "," + str((nob-1)*6+1) + ") = 1\n" + \
    "$ C." + str(cn) + ".B(" + str((nob-1)*6+2) + "," + str((nob-1)*6+3) + ") = 1\n" + \
    "$ C." + str(cn) + ".B(" + str((nob-1)*6+2) + "," + str((nob-1)*6+5) + ") = -1\n" + \
    "$ C." + str(cn) + ".B(" + str((nob-1)*6+2) + "," + str(1) + ") = " + str(1 if antiperiodic==1 else -1) + "\n\n\n"
barstxt = barstxt + s

###############################################################################
### Kirchoff current law
###############################################################################

# each bar is connected to two knots -- left and right
s = "! Kirchoff current law\n\n"
barstxt = barstxt + s

# bar 1 knots contain periodicity information

s = "!Bar" + str(1) + " right knot\n" + \
    "$ C." + str(cn) + ".B(" + str(0+0) + "," + str(0+0) + ") = 1\n" + \
    "$ C." + str(cn) + ".B(" + str(0+0) + "," + str(nob*6-(2 if antiperiodic == 1 else 4)) + ") = 1\n" + \
    "$ C." + str(cn) + ".B(" + str(0+0) + "," + str(0+2) + ") = -1\n" + \
    "!Bar" + str(1) + " left knot\n" + \
    "$ C." + str(cn) + ".B(" + str(0+4) + "," + str(0+4) + ") = -1\n" + \
    "$ C." + str(cn) + ".B(" + str(0+4) + "," + str(nob*6-(4 if antiperiodic == 1 else 2)) + ") = 1\n" + \
    "$ C." + str(cn) + ".B(" + str(0+4) + "," + str(0+0) + ") = -1\n\n" 
barstxt = barstxt + s

# other bars are composed similarly
for nbar in range(1,nob):
    s = "!Bar" + str(nbar+1) + " right knot\n" + \
        "$ C." + str(cn) + ".B(" + str(nbar*6+0) + "," + str(nbar*6+0) + ") = 1\n" + \
        "$ C." + str(cn) + ".B(" + str(nbar*6+0) + "," + str(nbar*6-4) + ") = 1\n" + \
        "$ C." + str(cn) + ".B(" + str(nbar*6+0) + "," + str(nbar*6+2) + ") = -1\n" + \
        "!Bar" + str(nbar+1) + " left knot\n" + \
        "$ C." + str(cn) + ".B(" + str(nbar*6+4) + "," + str(nbar*6+4) + ") = -1\n" + \
        "$ C." + str(cn) + ".B(" + str(nbar*6+4) + "," + str(nbar*6-2) + ") = 1\n" + \
        "$ C." + str(cn) + ".B(" + str(nbar*6+4) + "," + str(nbar*6+0) + ") = -1\n\n" 
    barstxt = barstxt + s 

###############################################################################
### Elemental equations
###############################################################################

# these equations describe R and L elements in the circuit
# v = vr+vl
# v -iR - Li' = 0

s = "! Elemental equations\n\n"
barstxt = barstxt + s

for nbar in range(0,nob):
    s = "$ C." + str(cn) + ".B(" + str(nbar*6+3) + "," + str(nbar*6+3) + ") = -1\n" + \
        "$ C." + str(cn) + ".B(" + str(nbar*6+3) + "," + str(nbar*6+2) + ") = R_er\n" + \
        "$ C." + str(cn) + ".A(" + str(nbar*6+3) + "," + str(nbar*6+2) + ") = L_er\n" + \
        "$ C." + str(cn) + ".B(" + str(nbar*6+5) + "," + str(nbar*6+5) + ") = -1\n" + \
        "$ C." + str(cn) + ".B(" + str(nbar*6+5) + "," + str(nbar*6+4) + ") = R_er\n" + \
        "$ C." + str(cn) + ".A(" + str(nbar*6+5) + "," + str(nbar*6+4) + ") = L_er\n\n" 

    barstxt = barstxt + s


with open(OUTFILE, 'w+') as f:
    f.write(barstxt)

print('Cage circuit equations for circuit number', cn,
        'with', ns, 'slices',
        'for', nob, 'bars with', 
        'antiperiodic' if antiperiodic == 1 else 'periodic', 
        'boundary conditions are saved to', OUTFILE)



