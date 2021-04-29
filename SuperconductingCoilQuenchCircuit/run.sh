#!/bin/bash

if [ "$#" == "0" ]
then
	echo "Run name not set for logging purposes. You can give it with: ./run.sh run_name"
	logname=""
else
	echo "Run name set to $1."
	logname="$1-"
fi

DATE=$(date +%s)
LOGFILE="$logname$DATE-run.log"
echo " "
# ElmerGrid 8 2 *.unv -out MESH -autoclean
echo "Run the coil powering: coil.sif" | tee $LOGFILE
rm ./RESU/coil_* | tee -a $LOGFILE
echo "*** ElmerSolver ***"
ElmerSolver coil.sif | tee -a $LOGFILE
python ./Python/plot_ramp.py | tee -a $LOGFILE
eog ./Figures/ramp.png &
echo "Run the coil extraction: coil_extraction.sif" | tee -a $LOGFILE
ElmerSolver coil_extraction.sif | tee -a $LOGFILE
python ./Python/plot_all.py | tee -a $LOGFILE
eog ./Figures/all_current.png &
eog ./Figures/all_voltage.png &
mv $LOGFILE log/



