#!/bin/bash
#UNZIP_NOT_FAILED=true
#unzip mesh.zip && echo "Mesh unzip succeeded" || UNZIP_NOT_FAILED=false
./compileUDF.sh
ElmerSolver case.sif
#$UNZIP_NOT_FAILED && echo "Done." || echo "Note that unzip command failed, make sure you have the access to unzip command or unzip 'mesh.zip' manually"

