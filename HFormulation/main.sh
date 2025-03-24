############
### BASH ###
############

### AUTHOR: Frederic Trillaud <ftrillaudp@gmail.com>
### PLACE: Instituto de Ingenieria, UNAM
### DATE (English): 01/05/2020

#! /bin/bash

### Variables ###
### Change the path
GMSH="/opt/onelab-Linux64/gmsh"


### Get file name
name=$(find ./Gmsh -type f -name "assembly.geo")
### remove the extension for multiple purposes
filename=$(basename $name | cut -d. -f1)

cd ./Gmsh; $GMSH -3 $filename.geo; cd ../
cp -v ./Gmsh/$filename.msh ./

echo " "
echo "*** Conversion mesh: *.unv to *.mesh ***"
ElmerGrid 14 2 $filename.msh -out MESH

echo " "
echo "*** Computation solution: ElmerSolver ***"
rm log.txt
bash compileUDF.sh

ElmerSolver case.sif | tee -a log.txt
