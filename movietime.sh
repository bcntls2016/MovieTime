#!/bin/bash
source movietime.settings
MODE=$1
DENPATH=$2

askOS () {
	while true
	do
		echo -n "Are you on a Mac? (yes/no): "
		read rMAC
		case $rMAC in
			yes)
				ln -sv makefile.mac makefile
				break
				;;
			no)
				ln -sv makefile.eos makefile
				break
				;;
			*)
				echo
				echo "Please choose either 'yes' if you are on a Mac, or"
				echo "                     'no' otherwise."
				echo
				;;
			esac
	done
}

Compile () {
	if [ -z ${MAC+x} ]
	then
		[ ! -f makefile ] && askOS
	else
		case $MAC in
			true)
				[ ! -f makefile ] && ln -sv makefile.mac makefile
				;;
			false)
				[ ! -f makefile ] && ln -sv makefile.eos makefile
				;;
			*)
				[ ! -f makefile ] && askOS
				;;
			esac
		fi
	[ ! -f density ] && make && make proper
}

makeDirStructure () {
	[ ! -d Movie ] && mkdir -v Movie
	[ ! -d Movie/1D-densities ] && mkdir -v Movie/1D-densities
	[ ! -d Movie/2D-densities ] && mkdir -v Movie/2D-densities
	[ ! -d Movie/Params ] && mkdir -v Movie/Params
	[ ! -d Movie/Images ] && mkdir -v Movie/Images
	[ ! -d Movie/1D-images ] && mkdir -v Movie/1D-images	
}

askBox () {
	while true
	do
		echo
		echo "Choose the box-size to use:"
		echo "    1: 38.4 x 38.4 x 38.4 Angstrom,"
		echo "    2: 40 x 40 x 40 Angstrom,"
		echo "    3: 40 x 40 x 50 Angstrom,"
		echo "    4: 40 x 40 x 51.2 Angstrom, or"
		echo -n "    5: 40 x 40 x 53.3 Angstrom, followed by [ENTER]: "
		read DIM
		case $DIM in
			1)
				ln -sv margins-38.4 margins
				break
				;;
			2)
				ln -sv margins-40 margins
				break
				;;
			3)
				ln -sv margins-40x40x50 margins
				break
				;;
			4)
				ln -sv margins-40x40x51.2 margins
				break
				;;
			5)
				ln -sv margins-40x40x53.3 margins
				break
				;;
			*)
				echo
				echo "Please choose only '1' for 38.4 x 38.4 x 38.4 Angstrom,"
				echo "                   '2' for 40 x 40 x 40 Angstrom,"
				echo "                   '3' for 40 x 40 x 50 Angstrom,"
				echo "                   '4' for 40 x 40 x 51.2 Angstrom, or"
				echo "                   '5' for 40 x 40 x 53.3 Angstrom."
				;;
			esac
	done
}

chooseBoxSize () {
	if [ -z ${BOX+x} ]
	then
		[ ! -f margins ] && askBox
	else
		case $BOX in
			38.4)
				[ ! -f margins ] && ln -sv margins-38.4 margins
				;;
			40)
				[ ! -f margins ] && ln -sv margins-40 margins
				;;
			40x40x50)
				[ ! -f margins ] && ln -sv margins-40x40x50 margins
				;;
			40x40x51.2)
				[ ! -f margins ] && ln -sv margins-40x40x51.2 margins
				;;
			40x40x53.3)
				[ ! -f margins ] && ln -sv margins-40x40x53.3 margins
				;;							
			*)
				[ ! -f margins ] && askBox
				;;
		esac
	fi
}

selectImpurity () {
	case $IMP in
		true)
			[ ! -f impurity ] && ln -sv impurity.on impurity
			;;
		false)
			[ ! -f impurity ] && ln -sv off impurity
			;;
		*)
			[ ! -f impurity ] && ln -sv impurity.on impurity
			;;
		esac
}

selectEnergy () {
	case $KEN in
		true)
			[ ! -f energy ] && ln -sv energy.on energy
			;;
		false)
			[ ! -f energy ] && ln -sv off energy
			;;
		*)
			[ ! -f energy ] && ln -sv energy.on energy
			;;
		esac
}

selectFirstFrame () {
	case $FFRAME in
		true)
			[ ! -f firstframe ] && ln -sv firstframe.on firstframe
			;;
		false)
			[ ! -f firstframe ] && ln -sv off firstframe
			;;
		*)
			[ ! -f firstframe ] && ln -sv firstframe.on firstframe
			;;
		esac
}

setFileID () {
	echo ${1}
	ID=${1##*/}
	ID=${ID#*.}
	ID=${ID%.*}
}

genDensity () {
	ln -s ${1} density.jogger.dat
	./density < density.settings
	RC=$?
	rm density.jogger.dat
	if [ $RC -eq 1 ]
	then
		echo
		echo
		echo "FATAL ERROR during file read. File read aborted by user."
		echo
		echo
		exit 1
	fi
	if [ $RC -ne 0 ]
	then
		echo
		echo
		echo "FATAL ERROR during file read. Please check if you use the correct \
			'readmode' in 'density.settings'."
		echo
		echo
		exit 1
	fi				
	mv denz.dat Movie/1D-densities/denz-${ID}.dat
	mv denxz.dat Movie/2D-densities/denxz-${ID}.dat
}

genParams () {
	ln -s ${1} density.jogger.dat
	./params < density.settings
	rm density.jogger.dat
	if [[ $OLD == "true" && $STATIC != "true" ]]
	then
		typeset TMP_FILE=$(mktemp)
		touch $TMP_FILE
		cp -p param.dat $TMP_FILE
		T=$(echo "$T0+$DT*$ID" | bc | sed 's/^\./0./')
		sed "s/$/ $T    0.0/" $TMP_FILE > param.dat
	fi
	if [[ $STATIC == "true" ]]
	then
		typeset TMP_FILE=$(mktemp)
		touch $TMP_FILE
		cp -p param.dat $TMP_FILE
		sed "s/$/ $RCM/" $TMP_FILE > param.dat
	fi
	mv param.dat Movie/Params/param-${ID}.dat
}

plotImage () {
	echo "plotImage() called..."
	echo "Movie/2D-densities/denxz-${ID}.dat"
	echo "Movie/Params/param-${ID}.dat"
	while read xmax zmax ximp zimp vimp ekin t zcm
	do
		$GNUPLOT -e "INPUT='Movie/2D-densities/denxz-${ID}.dat'; xmax='${xmax}'; \
			ymax='${zmax}'; ximp='${ximp}'; zimp='${zimp}'; vimp='${vimp}'; \
			ekin='${ekin}'; t='${t}'; zcm='${zcm}'" plotden.gnu
	done < Movie/Params/param-${ID}.dat
	pngtopnm denxz.png | pnmscale -reduce 4 | pnmtopng > Movie/Images/denxz-${ID}.png
	rm denxz.png
	#mv -v denxz.png Movie/Images/denxz-${ID}.png
}

plot1DImage () {
	while read xmax zmax ximp zimp vimp ekin t zcm
	do
		$GNUPLOT -e "INPUT='Movie/1D-densities/denz-${ID}.dat'; \
			xmax='${zmax}'; zimp='${zimp}'; t='${t}'" plot1Dden.gnu
	done < Movie/Params/param-${ID}.dat
	mv denz.png Movie/1D-images/denz-${ID}.png
}

compileMovie () {
	ffmpeg -r ${FPS} -f image2 -i Movie/Images/denxz-%03d.png -vcodec libx264 -preset \
		ultrafast -crf 18 -pix_fmt yuv420p Movie/movie-2D.mp4
	ffmpeg -r ${FPS} -f image2 -i Movie/1D-images/denz-%03d.png -vcodec libx264 -preset \
		ultrafast -crf 18 -pix_fmt yuv420p Movie/movie-1D.mp4
	#ffmpeg -r ${FPS} -i Movie/Images/denxz-%03d.png -c:v qtrle -pix_fmt rgb24 \
		#Movie/movie-lossless.mov
	#ffmpeg -r ${FPS} -i Movie/Images/denxz-%03d.png -codec png Movie/movie-d.mov
}

case $MODE in
	0)
		echo "Setting up the environment..."
		Compile
		makeDirStructure
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
		echo ${DENPATH} $ID
		[ ! -f Movie/2D-densities/denxz-${ID}.dat ] && echo "genDensity" && genDensity ${DENPATH}
		[ ! -f Movie/Params/param-${ID}.dat ] && echo "genParams" && genParams ${DENPATH}
		[ ! -f Movie/Images/denxz-${ID}.png ] && echo "plotImage" && plotImage
		[ ! -f Movie/1D-images/denz-${ID}.png ] && echo "plot1DImage" && plot1DImage
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
