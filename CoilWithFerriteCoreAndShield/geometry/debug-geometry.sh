#!/bin/bash

# This script lets ElmerGrid convert the grid
# and then opens it in GMSH for debugging purposes
# i.e. to check whether all required physical volumes
# and surfaces have been correctly exported. This
# helps you to see what Elmer sees in your simulation.

# ElmerGrid re-numbers volumes and surfaces. These
# numbers can be seen using GMSH's Visibility tool.
# As we're using named physical groups (see generate-mesh.py)
# we don't have to deal with such numbers directly
# (see coil.sif). If you don't use named physical groups,
# though, you'll have to take note of ElmerGrid's
# numbering and log files to find out by which numbers
# you may reference your volumes and surfaces in the
# simulation file.

ElmerGrid 14 4 coil.msh -out debug-grid.msh
gmsh debug-grid.msh
