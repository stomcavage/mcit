#!/bin/bash

for ((client=1; client<=$2; client++))
do
    RANDOM=$(head -1 /dev/urandom | od -N 2 | awk '{ print $2 }')
    ./tcpclient $1 $RANDOM &
    sleep 0.1
done
