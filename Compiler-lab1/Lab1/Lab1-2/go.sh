#!/usr/bin/env bash
# set -euxo pipefail

if [ ! -d ./output ]
then 
	echo "mkdir output"
	mkdir ./output
fi

for dir in `ls ./tests`
do
	if [ -d ./tests/$dir ]
	then
		echo "scan ./tests/$dir"

		if [ ! -d ./output/$dir ]
		then 
			echo "mkdir ./output/$dir"
			mkdir ./output/$dir
		fi

		for file in `ls ./tests/$dir`
		do 
			if [ -f ./tests/$dir/$file ]
			then
				echo "./build/astbuilder ./tests/$dir/$file > ./output/$dir/$file.output"
				./build/astbuilder ./tests/$dir/$file > ./output/$dir/$file.output
			fi
		done
	fi
done
