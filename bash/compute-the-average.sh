#!/bin/bash
read LINES
TOTAL=0

for I in $(seq $LINES); do
	read LINE
	TOTAL=$(($TOTAL + $LINE))
done

echo "scale=3; $TOTAL / $LINES" | bc -l
