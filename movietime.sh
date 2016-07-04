#!/bin/bash
source movietime.functions
source movietime.settings
MODE=$1
DENPATH=$2

case $MODE in
	0)
		echo "Setting up the environment..."
		[ ! -f density ] && Compile
		[ ! -d Movie ] && makeDirStructure
		chooseBoxSize
		selectImpurity
		selectEnergy
		selectFirstFrame
		;;
	1)
		echo "Produce only densities..."
		for FILE in ${DENPATH}/density.*.dat
		do
			setFileID ${FILE}
			[ ! -f Movie/2D-densities/denxz-${ID}.dat ] && genDensity ${FILE}
			[ ! -f Movie/Params/param-${ID}.dat ] && genParams ${FILE}
		done
		;;
	2)
		echo "Produce densities and images..."
		for FILE in ${DENPATH}/density.*.dat
		do
			setFileID ${FILE}
			[ ! -f Movie/2D-densities/denxz-${ID}.dat ] && genDensity ${FILE}
			[ ! -f Movie/Params/param-${ID}.dat ] && genParams ${FILE}
			[ ! -f Movie/Images/denxz-${ID}.png ] && plotImage
			[ ! -f Movie/1D-images/denz-${ID}.png ] && plot1DImage
		done
		;;
	3)
		echo "Produce densities, images and a movie..."
		for FILE in ${DENPATH}/density.*.dat
		do
			setFileID ${FILE}
			[ ! -f Movie/2D-densities/denxz-${ID}.dat ] && genDensity ${FILE}
			[ ! -f Movie/Params/param-${ID}.dat ] && genParams ${FILE}
			[ ! -f Movie/Images/denxz-${ID}.png ] && plotImage	
			[ ! -f Movie/1D-images/denz-${ID}.png ] && plot1DImage	
		done
		compileMovie
		;;
	4)
		echo "Produce a single density and image..."
		setFileID ${DENPATH}
		[ ! -f Movie/2D-densities/denxz-${ID}.dat ] && genDensity ${DENPATH}
		[ ! -f Movie/Params/param-${ID}.dat ] && genParams ${DENPATH}
		[ ! -f Movie/Images/denxz-${ID}.png ] && plotImage
		[ ! -f Movie/1D-images/denz-${ID}.png ] && plot1DImage
		;;
	5)
		echo "Produce only images..."
		for FILE in ${DENPATH}/density.*.dat
		do
			setFileID ${FILE}
			[ ! -f Movie/Images/denxz-${ID}.png ] && plotImage
			[ ! -f Movie/1D-images/denz-${ID}.png ] && plot1DImage
		done
		;;		
	6)
		echo "Produce only a movie..."
		compileMovie
		;;
	42)
		echo "Sanitising the environment..."
		#rm -rvf Movie
		#make clean 2> /dev/null
		rm -vf *.o
		rm -vf density.jogger.dat
		rm -vf makefile
		rm -vf margins
		rm -vf impurity
		rm -vf energy
		rm -vf firstframe
		;;
	*)
		clear
		echo
		echo "Please choose one of:"
		echo "====================="
		echo 
		echo "   ./movietime.sh  0  				: SETUP environment. This should always be ran FIRST"
		echo
		echo "   ./movietime.sh  1  /path/to/DENSITIES/	: produces 2D densities"
		echo "   ./movietime.sh  2  /path/to/DENSITIES/	: produces 2D densities + images"		
		echo "   ./movietime.sh  3  /path/to/DENSITIES/	: produces 2D densities + images + movie"		
		echo "   ./movietime.sh  4  /path/to/DENSITY.*.DAT	: produces only a single 2D density and image"
		echo "   ./movietime.sh  5  /path/to/DENSITIES/	: produces only the images (needs 2D densities)"
		echo "   ./movietime.sh  6  /path/to/IMAGES/		: produces only the movie (needs images)"		
		echo
		echo "   ./movietime.sh 42 				: SANITISE environment. Be CAREFUL, this will also"
		echo "						  delete the 'Movie' directory structure AND its CONTENTS. You will"
		echo "						  have to run the SETUP-mode again afterwards."		
		echo
		;;
esac
