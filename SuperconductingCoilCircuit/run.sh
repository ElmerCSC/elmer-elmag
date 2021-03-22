#! /bin/env zsh

echo " "
rm ./RESU/coil_*
echo "*** ElmerSolver ***"
ElmerSolver coil.sif
python plot_ramp.py
eog ramp.png
ElmerSolver coil_extraction.sif
python plot_extraction.py
eog extraction.png
