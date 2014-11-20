#!/bin/bash
read LINES
TOTAL=0

for I in $(seq $LINES); do
    read LINE
    TOTAL=$(($TOTAL + $LINE))
done

printf "%.3f\n" `echo $TOTAL / $LINES | bc -l`
