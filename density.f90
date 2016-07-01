PROGRAM Density

IMPLICIT NONE
character (len = 40)	:: fileden = "density.jogger.dat"! Input file containing the wavefunction
character (len = 40)	:: outfile1 = "denz.dat"	! Output file containing the 2D density
character (len = 40)	:: outfile2 = "denxz.dat"	! Output file containing the 2D density
character (len = 40)	:: outfile3 = "denxyz.dat"	! Output file containing the 1D density
character (len = 40)	:: outfile4 = "param.dat"	! Output file containing the parameters
character (len = 1) 	:: cchar = "#"				! Character to be skipped on routine titols
logical (kind = 4)	:: OMP_Dynamic_Enable = .false.	! Enable/disable OpenMP dynamic resource allocation
logical (kind = 4)	:: limp = .false.	! Treat impurity CM coord quantum/classical
logical (kind = 4)	:: old	= .false.	! Treat impurity CM coord quantum/classical
integer (kind = 4)	:: nx, ny, nz		! Number of points of the grid in each direction
integer (kind = 4)	:: ix, iy, iz		! Counters for grid loops
integer (kind = 4)	:: ninvar			! Number of components of lambda
integer (kind = 4)	:: isalto			! Number of lines to skip at reading
integer (kind=4)	:: nthreads = 1		! Number of cpu cores to use 
integer (kind=4)	:: denmode = 42	! Density reading mode 
integer (kind=4)	:: parammode = 42	! Density reading mode 
real (kind = 8)		:: xmax, ymax, zmax	! Maximum values of x, y and z
real (kind = 8)		:: hx, hy, hz		! x, y and z step for the grid
real (kind = 8)		:: rimp(3)			! Vector storing the position of the impurity
real (kind = 8)		:: vimp(3)			! Vector storing the velocity of the impurity
real (kind = 8)		:: mimpur			! Impurity mass (au)
real (kind = 8), ALLOCATABLE	:: x(:), y(:), z(:)	! Values in X, Y and Z
real (kind = 8), ALLOCATABLE	:: den(:,:,:)		! Helium density
complex(kind = 8), ALLOCATABLE	:: invar(:)			! Lambda (internal variables)
complex(kind = 8), ALLOCATABLE	:: psi(:,:,:)		! Wave Function. Density = MOD(psi)**2

40 format(5e26.15)
namelist /input/ fileden,outfile1,outfile2,outfile3,outfile4,nthreads,denmode,parammode,old,mimpur
read(5,nml=input)

!$ CALL OMP_SET_DYNAMIC(OMP_Dynamic_Enable)
!$ CALL OMP_SET_NUM_THREADS(nthreads)

select case (denmode)
	case (1)
		open (unit=1, file=fileden)
		call titols(1,cchar,isalto)
		read(1,*) xmax,ymax,zmax,hx,hy,hz,nx,ny,nz,limp,rimp
		ALLOCATE (den(nx,ny,nz))
		ALLOCATE (x(nx))
		ALLOCATE (y(ny))
		ALLOCATE (z(nz))
		read(1,*) den
		close(1)
	case (2)
		open (unit=1, file=fileden)
		call titols(1,cchar,isalto)
		read(1,*) xmax,ymax,zmax,hx,hy,hz,nx,ny,nz,limp,rimp
		ALLOCATE (psi(nx,ny,nz))
		ALLOCATE (den(nx,ny,nz))
		ALLOCATE (x(nx))
		ALLOCATE (y(ny))
		ALLOCATE (z(nz))
		read(1,*) psi
		close(1)
		den = Conjg(psi) * psi
	case (3)
		open (unit=1, file=fileden)
		call titols(1,cchar,isalto)
		read(1,*) xmax,ymax,zmax,hx,hy,hz,nx,ny,nz,rimp,vimp
		ALLOCATE (psi(nx,ny,nz))
		ALLOCATE (den(nx,ny,nz))
		ALLOCATE (x(nx))
		ALLOCATE (y(ny))
		ALLOCATE (z(nz))
		read(1,*) psi
		close(1)
		den = Conjg(psi) * psi
	case (4)
		open (unit=1, file=fileden)
		call titols(1,cchar,isalto)
		read(1,*) xmax,ymax,zmax,hx,hy,hz,nx,ny,nz,rimp,vimp,ninvar
		ALLOCATE (invar(ninvar))
		ALLOCATE (psi(nx,ny,nz))
		ALLOCATE (den(nx,ny,nz))
		ALLOCATE (x(nx))
		ALLOCATE (y(ny))
		ALLOCATE (z(nz))
		read(1,*) invar
		read(1,*) psi
		close(1)
		den = Conjg(psi) * psi
	case default
		write(*,*)
		write(*,*) "You have chosen a 'denmode' unequal to {1,2,3,4}. Please modify '2Dden.settings' and choose one of:"
		write(*,*)
		write(*,*) "denmode = 1:	Static helium DENSITY with/without impurity."
		write(*,*) "denmode = 2:	Static helium WAVE FUNCTION with/without a vortex/impurity."
		write(*,*) "denmode = 3:	Dynamic helium wave function with impurity in an excited 'SIMPLE'"
		write(*,*) "		SYMMETRIC electronic state, IONISED GROUND STATE or GROUND STATE."
		write(*,*) "denmode = 4:	Dynamic helium wave function with impurity in an EXCITED"
		WRITE(*,*) "		ANISOTROPIC electronic state / internal state."
		call EXIT(10)
end select

		!$OMP PARALLEL
		!$OMP DO PRIVATE(ix)    
		DO ix = 1,nx  !.................... Grid X
			x(ix) = -xmax + hx * (ix - 1)
		END DO
		!$OMP END DO NOWAIT

		!$OMP DO PRIVATE(iy)    
		DO iy = 1,ny  !.................... Grid Y
			y(iy) = -ymax + hy * (iy - 1)
		END DO
		!$OMP END DO NOWAIT

		!$OMP DO PRIVATE(iz)    
		DO iz = 1,nz  !.................... Grid  Z
			z(iz) = -zmax + hz * (iz - 1)
		END DO
		!$OMP END DO
		!$OMP END PARALLEL
		open(unit = 1, file = outfile1)
		DO iz = 1,nz
			WRITE(1,7101) z(iz), den(nx/2+1,ny/2+1,iz)
		END DO
		close(unit = 1)
		open(unit = 2, file = outfile2)
		DO ix = 1,nx
			DO iz = 1,nz
				WRITE(2,7102) x(ix), z(iz), den(ix,ny/2+1,iz)
			END DO
			WRITE(2,7110)
		END DO
		close(unit = 2)

7101 format(1F15.5,1x,20E12.3)
7102 format(2F15.5,1x,20E12.3)
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
