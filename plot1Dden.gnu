#!/usr/local/bin/gnuplot

load "plotden.settings"
Title=sprintf("t = %s ps",t)
Title2="Xe-^4He_1_0_0_0 collision at v_0 = 200 m/s, b = 0"
xmin=-xmax
Term="pngcairo"

set samples 1000
set xrange[xmin:xmax]
set yrange[0:denmax]
set mxtics 4
set xlabel "z / [\305]"
set ylabel "{/Symbol r}_H_e / [\305^-^3]"
set label Title at xmin+2,0.0573 font "Arial,30"
set label Title2 at 15,0.058 font "Arial,20"
set term Term enhanced font "Helvetica-Light,30" size 1728,1080 #background rgb 'black'
set encoding iso_8859_1
set key off
set output "denz.png"
set object circle at zimp,0.011 size 1.2 fc rgb ImpurityColor fillstyle solid noborder front
set object circle at zimp,0.011 size 1.2 fc rgb '#000000' front
plot	INPUT w filledcurves below lc rgb '#1683fb', \
		INPUT w l lw 3 lc rgb '#000000'
