#!/bin/bash
ElmerGrid 14 2 geometry/coil.msh -out elmer -autoclean
ElmerSolver coil.sif
if [ $? -eq 0 ]; then
  paraview --state=coil.pvsm
fi
