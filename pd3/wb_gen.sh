#!/bin/bash

range_guard_w1=11
range_guard_w2=11
range_guard_b1=4
range_guard_b2=4

float=1000

icount=7
hcount=8
ocount=7

b1count=hcount
b2count=ocount
w1count=icount*hcount
w2count=hcount*ocount

flag=0

function generate_numbers {
	declare -a tmpArr

	for (( i=1; i<=$1; i++ ))
	do
		flag=$(( RANDOM % 2 ))
		if (( flag == 0 )) ; then
			tmpArr[$i]="-"
		fi
		tmpArr[$i]+=$(( RANDOM % $2 ))"."$(( RANDOM % float ))
		if (( i < $1 )) ; then
			tmpArr[$i]+=","
		fi
	done
	
	echo "  var $3 = ["${tmpArr[*]}"];"
}

echo ""
generate_numbers $(( w1count )) $(( range_guard_w1 )) "weights1"
generate_numbers $(( b1count )) $(( range_guard_b1 )) "bias1"
echo ""
generate_numbers $(( w2count )) $(( range_guard_w2 )) "weights2"
generate_numbers $(( b2count )) $(( range_guard_b2 )) "bias2"
echo ""
