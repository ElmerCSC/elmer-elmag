#!/bin/sh
ElmerGrid 14 2 stator_TEAM30a.msh -2d –autoclean –names
ElmerGrid 14 2 rotor_TEAM30a.msh -2d –autoclean –names
ElmerGrid 2 2 stator_TEAM30a -in rotor_TEAM30a -unite -autoclean -names -out TEAM30a
 
