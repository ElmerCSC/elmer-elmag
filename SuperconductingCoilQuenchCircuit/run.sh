#!/bin/zsh

echo " "
rm ./RESU/coil_*
echo "*** ElmerSolver ***"
ElmerSolver coil.sif
python ./Python/plot_ramp.py
eog ./Figures/ramp.png
ElmerSolver coil_extraction.sif
python ./Python/plot_all.py
eog ./Figures/all_current.png
eog ./Figures/all_voltage.png



