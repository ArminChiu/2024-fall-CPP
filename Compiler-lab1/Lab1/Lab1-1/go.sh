#!/usr/bin/env bash
# set -euxo pipefail

if [ ! -d ./output ]
then 
	echo "mkdir output"
	mkdir ./output
fi

cd grammar
echo "Compiling..."
java -Xmx500M -cp "/usr/local/lib/antlr-4.9.3-complete.jar:$CLASSPATH" org.antlr.v4.Tool \
	*.g4
javac *.java

for file in `ls ../tests`
do
	echo "grun SafeC compUnit -tree ../tests/$file > ../output/$file.output"
	java -Xmx500M -cp "/usr/local/lib/antlr-4.9.3-complete.jar:$CLASSPATH" org.antlr.v4.gui.TestRig \
		SafeC compUnit -tree ../tests/$file > ../output/$file.output
done
