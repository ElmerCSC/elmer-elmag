#!/bin/bash
gmsh gmsh/TEAM7.geo -3 -o TEAM7.msh
ElmerGrid 14 2 TEAM7.msh
ElmerSolver TEAM7.sif
octave pline.m
