#!/bin/bash

for f in *.txt
do
	[ -f "$f" ] && sed 's/,/ /g' $f > $(basename $f | cut -d. -f1).dat
done
