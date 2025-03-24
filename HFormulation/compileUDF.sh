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
echo "  *** COMPILATION: hWhitneySolver.f90 ***"
echo " "
elmerf90 -o hWhitneySolver.so hWhitneySolver.F90
echo " "
echo "  *** DONE ***"

cd ..
echo " "
