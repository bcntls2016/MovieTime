#!/usr/local/bin/gnuplot

load "plotden.settings"
Title=sprintf("t = %s ps",t)
Zimp=sprintf("z_X - z_C_M = %4.1f \305",zimp-zcm)
Vimp=sprintf("          v_X = %s m/s",vimp)
Ekin=sprintf("         E_X = %s K",ekin)
xmin=-xmax
ymin=-ymax
tics=4
Term="pngcairo"
load "margins"
set zero 1.e-15
set nokey
set view 0,0,1,1
set border linecolor rgb TextColor
set xtics textcolor rgb TextColor
set ytics textcolor rgb TextColor
set title textcolor rgb TextColor
set xlabel textcolor rgb TextColor
set ylabel textcolor rgb TextColor
set pm3d at s interpolate 10, denmax corners2color c4
set palette defined (0 "#000000", 0.166667 "#5F2281", 0.333333 "#C94245", 0.5 "#F9730D", 0.666667 "#FFAE21", 0.833333 "#FFE07D", 1 "#FFFFFF")
set cntrparam levels 10
set cntrparam cubicspline
set cntrparam points  30
set cntrparam order  9
set isosample 40,40
set nosurface
set pm3d map
set xrange[xmin:xmax]
set yrange[ymin:ymax]
set cbrange[0.:denmax]
set size ratio -1
set mxtics tics
set mytics tics
set xlabel "x / [\305]"
set ylabel "z / [\305]"
set label Title at TitleXPos,TitleYPos  front tc rgb TimeColor font "Arial,90"
set label Zimp at ZimpXPos,ZimpYPos front tc rgb TextColor font "Arial,70"
set label Vimp at VimpXPos,VimpYPos front tc rgb TextColor font "Arial,70"
load "energy"
load "firstframe"
set term Term enhanced font "Helvetica,80" size ImgWidth,ImgHeight background rgb 'black'
set encoding iso_8859_1
set key off
set output "denxz.png"
load "impurity"
splot INPUT u 1:2:3 w l lt 4
