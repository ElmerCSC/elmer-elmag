#!/bin/bash
ElmerGrid 2 2 TEAM7 -partdual -metiskway 3
mpirun -np 3 ElmerSolver_mpi 
cat res/f0*.dat > res/f.dat
octave pline.m
