#! /bin/bash

echo " "
rm ./RESU/coil_*
echo "*** ElmerSolver ***"
ElmerSolver coil.sif
./circ_resu.sh
