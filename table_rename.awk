#!/bin/bash

# This is an awk script that replaces text in one file
# using a lookup from in another file. It assumed that
# the first input is the file to be changed, and the
# second input is the lookup table. In the lookup table
# the first field is the replacement value and the
# second field is the value to be replaced.

awk 'NR==FNR{a[$2]=$1} NR>FNR{$1=a[$1];print}' OFS="\t" $2 $1
