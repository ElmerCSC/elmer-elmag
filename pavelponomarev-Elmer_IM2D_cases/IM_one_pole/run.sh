#!/bin/sh

rm -rf results im ELMERSOLVER_STARTINFO


# generate rotor cage winding equations and components
# parameters are in the begining of the cage_generator.py script
cd cage
python cage_generator.py
cd ..


# generate meshes for stator and rotor and then unite them
gmsh gmsh/im_stator.geo -2 -o im_stator.msh
ElmerGrid 14 2 im_stator.msh -2d -autoclean -names

gmsh gmsh/im_rotor.geo -2 -o im_rotor.msh
ElmerGrid 14 2 im_rotor.msh -2d -autoclean -names

ElmerGrid 2 2 im_stator -in im_rotor -unite -autoclean -names -out im
rm -rf im_stator im_rotor im_stator.msh im_rotor.msh


# parallel partitioning
#np=4     #number of partitions and parallel processes
#ElmerGrid 2 2 im -partdual -partlayers 0 -metis $np 3 -connect 2 3 4 5 7 8
#ElmerGrid 2 2 im -partdual -metis $np 4 -connect 2 3 4 5 7 8   # without partlayers 0 there is a discrepancy between NF torque and band torque


################################################################################
# simple model showing that the geometry and boundary conditions are defined correctly
ElmerSolver model.sif
# parallel execution
#echo "model.sif" > ELMERSOLVER_STARTINFO
#mpirun -np $np ElmerSolver_mpi 


################################################################################
# Transient voltage driven simulation from zero initial conditions
#ElmerSolver transient.sif
# parallel execution
#echo "transient.sif" > ELMERSOLVER_STARTINFO
#mpirun -np $np ElmerSolver_mpi 


################################################################################
# steady state (time-harmonic) AC voltage driven model with defined stator and rotor circuits
#ElmerSolver harmonic.sif
# parallel execution
#echo "harmonic.sif" > ELMERSOLVER_STARTINFO
#mpirun -np $np ElmerSolver_mpi 


################################################################################
#iron losses
#echo "transient_iron_loss.sif" > ELMERSOLVER_STARTINFO
#mpirun -np $np ElmerSolver_mpi 


################################################################################
# Transient speed up of IM from zero speed with kinematic equation
#elmerf90 -O -o Kinematics Kinematics.f90
#ElmerSolver transient_kinematics.sif
# parallel execution
#echo "transient_kinematics.sif" > ELMERSOLVER_STARTINFO
#mpirun -np $np ElmerSolver_mpi

