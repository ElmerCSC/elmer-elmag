#!/bin/sh

cd cage
python cage_generator.py
cd ..


ns=4
slen=0.04

rm -rf results im imms

# make rotor and stator elmer meshes from geo files
gmsh gmsh/im_stator.geo -2 -o im_stator.msh
ElmerGrid 14 2 im_stator.msh -2d -autoclean -names

gmsh gmsh/im_rotor.geo -2 -o im_rotor.msh
ElmerGrid 14 2 im_rotor.msh -2d -autoclean -names

rm -rf im_stator.msh im_rotor.msh

# unite and scale to SI
ElmerGrid 2 2 im_stator -in im_rotor -unite -autoclean -names -out im -scale 0.001 0.001 0.001

rm -rf im_stator im_rotor

# create slices
ElmerGrid 2 2 im -out imms -clone 1 1 $ns -clonesize 0 0 $slen 

# Partition slices
ElmerGrid 2 2 imms -partition 1 1 $ns 
#ElmerGrid 2 2 imms -partdual -metis 9 3 -partconnect $ns -connect 2 3 4 5 7 8 #-partlayers 0
#ElmerGrid 2 5 imms -partdual -metis 8 3 -partconnect $ns -connect 2 3 4 5 7 8 
#ElmerGrid 2 2 imms -partition 1 1 $ns -partconnect 2 
#-partition 1 1 $ns 
cp im/mesh.names imms/mesh.names
rm -rf im

#ElmerGrid 2 2 im -partdual -partlayers 0 -metis 10 4 -connect 2 3 4 5 7 8
#ElmerGrid 2 5 im -partdual -metis 8 3 -connect 2 3 4 5 7 8 



# Harmonic voltage driven simulation 
#echo "harmonic.sif" > ELMERSOLVER_STARTINFO
#mpirun -np 8 ElmerSolver_mpi 

# Time-transient voltage driven simulation 
echo "transient.sif" > ELMERSOLVER_STARTINFO
mpirun -np 4 ElmerSolver_mpi 

