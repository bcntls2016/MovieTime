PROGRAM Params

IMPLICIT NONE
character (len = 15)	:: a(4)				! Crap we don't need
character (len = 15)	:: b(2)				! Crap we don't need
character (len = 40)	:: fileden = "density.jogger.dat"! Input file containing the wavefunction
character (len = 40)	:: outfile1 = "denxyz.dat"	! Output file containing the 2D density
character (len = 40)	:: outfile2 = "denxz.dat"	! Output file containing the 2D density
character (len = 40)	:: outfile3 = "denz.dat"	! Output file containing the 1D density
character (len = 40)	:: outfile4 = "param.dat"	! Output file containing the parameters
character (len = 1) 	:: cchar = "#"				! Character to be skipped on routine titols
logical (kind = 4)	:: limp = .false.	! Treat impurity CM coord quantum/classical
integer (kind = 4)	:: nx, ny, nz		! Number of points of the grid in each direction
integer (kind = 4)	:: isalto			! Number of lines to skip at reading
integer (kind=4)	:: nthreads = 1		! Number of cpu cores to use 
integer (kind=4)	:: denmode = 42		! Density reading mode 
integer (kind=4)	:: parammode = 42	! Density reading mode 
real (kind = 4)		:: time,rcm(3)		! current time, rCM of droplet 
real (kind = 8)		:: xmax, ymax, zmax	! Maximum values of x, y and z
real (kind = 8)		:: hx, hy, hz		! x, y and z step for the grid
real (kind = 8)		:: rimp(3)			! Vector storing the position of the impurity
real (kind = 8)		:: vimp(3)			! Vector storing the velocity of the impurity
real (kind = 8)		:: mimpur			! Impurity mass (au)
real (kind = 8)		:: Ekin	= 0			! Kinetic Energy
real (kind = 8) , parameter	:: mp_u = 0.020614837554503673d0  	! proton mass in Angs**-2 * Kelvin**-1, go figure!
real (kind = 8)	, parameter	::	Ktops=7.638235070684233d0		! Convert K to ps

40 format(5e26.15)
namelist /input/ fileden,outfile1,outfile2,outfile3,outfile4,nthreads,denmode,parammode,mimpur
read(5,nml=input)

mimpur = mimpur * mp_u

select case (parammode)
	case (1) ! Statics
		open (unit=1, file=fileden)
		call titols(1,cchar,isalto)
		read(1,*) xmax,ymax,zmax,hx,hy,hz,nx,ny,nz,limp,rimp
		close(1)
		open(unit = 2, file = outfile4)
		write(2,7106) xmax, zmax, rimp(1), rimp(3), 0, 0, 0
		close(unit = 2)
	case (2) ! Dynamics
		open (unit=1, file=fileden)
		call titols(1,cchar,isalto)
		read(1,*) xmax,ymax,zmax,hx,hy,hz,nx,ny,nz,rimp,vimp
		close(1)
		Ekin = 0.5 * mimpur * sum(vimp * vimp)
		open(unit = 2, file = outfile4)
		write(2,7105) xmax, zmax, rimp(1), rimp(3), 100*vimp(3)/Ktops, Ekin
		close(unit = 2)
	case (3) ! Dynamics
		open(unit=1, file=fileden)
		read(1,*)
		read(1,*)
		read(1,*) a,time
		read(1,*) b,rcm
		rewind(1)
		call titols(1,cchar,isalto)
		read(1,*) xmax,ymax,zmax,hx,hy,hz,nx,ny,nz,rimp,vimp
		close(1)
		Ekin = 0.5 * mimpur * sum(vimp * vimp)		
		open(unit = 2, file = outfile4)
		write(2,7106) xmax, zmax, rimp(1), rimp(3), 100*vimp(3)/Ktops, Ekin, time, rcm(3)
		close(2)
	case (4) ! Dynamics
		open(unit=1, file=fileden)
		read(1,*)
		read(1,*)
		read(1,*)
		read(1,*) a,time
		read(1,*)
		read(1,*)
		read(1,*)
		read(1,*) a,rcm
		read(1,*)
		read(1,*)
		read(1,*)
		read(1,*)
		read(1,*)
		read(1,*) b,Ekin
		rewind(1)
		call titols(1,cchar,isalto)
		read(1,*) xmax,ymax,zmax,hx,hy,hz,nx,ny,nz,rimp,vimp
		close(1)
		open(unit = 2, file = outfile4)
		write(2,7106) xmax, zmax, rimp(1), rimp(3), 100*vimp(3)/Ktops, Ekin, time, rcm(3)
		close(2)
	case default
		write(*,*)
		write(*,*) "You have chosen a 'parammode' unequal to {1,2}. Please modify 'density.settings' and choose one of:"
		write(*,*)
		write(*,*) "parammode = 1:	Static helium density/wave-function."
		write(*,*) "parammode = 2:	Dynamic helium wave-function. Time and COM info added later."
		write(*,*) "parammode = 3:	Dynamic helium wave-function. Time and COM info from WF-file"
		write(*,*) "parammode = 4:	Dynamic helium wave-function. Time and COM info from WF-file, and more"
		call EXIT(10)
end select

7100 format(2F15.5,1x,20E12.3)
7105 format(F5.2, 2x, F5.2, 2x, F6.2, 2x, F6.2, 2x, F6.1, 2x, F6.1)
7106 format(F5.2, 2x, F5.2, 2x, F6.2, 2x, F6.2, 2x, F6.1, 2x, F6.1, 2x, F5.1, 2x, F8.4)
7110 format()

END PROGRAM


!----------------------------------------------------------------------
!--                 Subroutine TITOLS                               ---
!----------------------------------------------------------------------
!
!   Esta rutina posiciona el puntero de lectura de un fichero
!   hasta la primera linea cuya primera columna NO comience por el
!   caracter  'cchar' y devuelve en la variable isalto el numero de
!   lineas que se ha saltado.
!
!   La variable ulog indica el numero de unidad logica a utilizar.
!
subroutine titols(ulog,cchar,isalto)
implicit none
character (LEN=1) :: pchar, pcolumn, cchar
integer :: nl,i,ulog,isalto

nl = 0
pcolumn = cchar
do while(pcolumn.eq.cchar)
	read(ulog,5000,end = 9000) pchar
	pcolumn = pchar
	nl = nl + 1
end do
rewind(ulog)
isalto = nl - 1
do i = 1,isalto
	read(ulog,*)
end do
return
9000 print *,' Ey Mister this file is too short ...'  
     error stop 'STOP due to severe errors in routine TITOLS'
5000 format(A1)
end
