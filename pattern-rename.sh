#!/bin/bash

OS=110
WIDTH=4

for OLDFILE in denxz-[0-9][0-9][0-9].png  #` | sort -r`
do
	OLDID=${OLDFILE:6:3}
	NEWID=`echo "$OLDID+$OS" | bc`
	NEWFILE=`printf "denxz-%0*d.png" $WIDTH $NEWID`
	mv $OLDFILE $NEWFILE
	#echo $OLDFILE $NEWFILE
done
