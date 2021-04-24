############
### BASH ###
############

### AUTHOR: Frederic Trillaud <ftrillaudp@gmail.com>
### PLACE: Instituto de Ingenieria, UNAM
### DATE (English): 07/30/2020

#! /bin/bash

### Variables ###
echo " "
echo "*** COMPILATION OF UDF (FORTRAN90) ***"
cd ./Fortran90
echo " "
echo "  *** COMPILATION: dissipation.f90 ***"
echo " "
elmerf90 -o dissipation.so dissipation.F90
echo " "
echo "  *** DONE ***"
echo " "
echo "  *** COMPILATION: therConductivity.f90 ***"
echo " "
elmerf90 -o therConductivity.so therConductivity.F90
echo " "
echo "  *** DONE ***"
echo " "
echo "  *** COMPILATION: regularization.f90 ***"
echo " "
elmerf90 -o regularization.so regularization.F90
echo " "
echo "  *** DONE ***"
echo " "
echo "  *** COMPILATION: electricalConductivity.f90 ***"
echo " "
elmerf90 -o electricalConductivity.so electricalConductivity.F90
echo " "
echo "  *** DONE ***"
echo " "
echo "  *** COMPILATION: checkMeshSize.f90 ***"
echo " "
elmerf90 -o checkMeshSize.so checkMeshSize.F90
echo " "
echo "  *** DONE ***"
cd ..
echo " "

