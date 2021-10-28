	Program Caminata
	Implicit none
	character(len=70) :: fn
	Integer:: i, m0, mf, n, j, k, dm, l
	Integer:: x0, xf
	Real*8:: rn, d, s, pd, sdv
	Real*8 :: r(1,2)
	real*8, dimension (:,:), allocatable :: vsdv    

!	Nota: Si quieres que te imprima todas las
!	trayectorias, descomenta las lineas marcadas con
!	"#####"

	Write(*,*)'Dame el rango de pasos que voy a tomar'
	Read(*,*) m0, mf
	Write(*,*)'¿ Cuantos pasos le sumo en cada iteración ?'
	Read(*,*) dm
	Write(*,*)'Dame el número de caminatas'
	Read(*,*) n
	
	allocate(vsdv(n,1))	
	
	k = 0
	l = (mf-m0)/dm

	call ImpDistancia(m0,mf)
	open(unit=40,file='DistanciaFin.dat', form='formatted')	

!	open(unit=30,file='PosiciónyDistancia.dat', form='formatted')
!	Write(30,*) 'x				y		Distancia'

	Do while(k .le. l)

	pd = 0
	j = 0

! **************************************************
	  Do while (j .le. n)
	  r(1,1) = 0
	  r(1,2) = 0 
	  i = 1 
!       Construye NombreDelArchivo -- i.dat
! ####	write(fn,fmt='(i0,a)') j, '.dat'
! ####	open(unit=10,file=fn, form='formatted')
	     Do While (i .le. m0)
	 	  Call random_number(rn)
		  Call Paso(r,rn)
! ####		write(10,*) r(1,1), r(1,2)
		  i = i + 1
	     End DO 
! ####	close(10)
	  d = sqrt((r(1,1)**2)+(r(1,2)**2))
	  vsdv(j,1) = d 
	  pd = pd + d
	  j = j + 1
!	Write(30,*) r(1,1), r(1,2), d
	  End Do
! ************************************************
	 pd = pd/real(n)

	  Do j=0 , n 
	   sdv = (pd - vsdv(j,1))**2
	  End DO 
	  
	 sdv = sqrt(sdv/real(n))

	 Write(40,*) m0, pd, sdv

	 m0 = m0 + dm
	 k = k + 1

	End Do
	close(40) 
	
! ####	Call ImpTraye(pd,j,n,fn)

	End Program

	Subroutine Paso(r,rn)
	Real*8 :: r(1,2),rn 
		If (rn .gt. 0) then
		 If (rn .le. 0.25) then
		 r(1,1) = r(1,1) - 1
		 End If 
		End If

		If (rn .gt. 0.25) then
		 If (rn .le. 0.50) then
		  r(1,2) = r(1,2) + 1
		 End If 
		End If 
		
		If (rn .gt. 0.50) then
		 If (rn .le. 0.75) then
		  r(1,2) = r(1,2) - 1
		 End If 
		End If 
		
		If (rn .gt. 0.75) then
		 If (rn .le. 1.00) then
		  r(1,1) = r(1,1) + 1
		 End If 
		End If 

	End Subroutine 

	Subroutine ImpTraye(pd,j,n,fn)
	Integer:: j, n, x0, xf 
	character(len=70) :: fn
	Real*8:: pd 
	x0 = -1*(pd + 50)
	xf = pd + 50
	! ********** Genera el Archivo para GNUplot **************	
	open(unit=20,file='Trayectorias.dat',form='formatted')
	  Write(20,*) 'set size square'
	  Write(20,*) 'set origin 0,0'
	  Write(20,*) 'set xzeroaxis'
	  Write(20,*) 'set yzeroaxis'
	  Write(20,*) 'set xrange [', x0,':', xf,']'
	  Write(20,*) 'set yrange [', x0,':', xf,']'
	  Write(20,*) 'set title "Trayectorias"'
	  Write(20,*) 'set xlabel "x"'
	  Write(20,*) 'set ylabel "y"'
	  Write(20,*) "plot '1.dat' notitle w d lw 2 lc 1"
		Do j=2 , n
		  write(fn,fmt='(i0,a)') j, ".dat'"
		  write(20,*) "replot '",fn,"notitle w d lw 2 lc",j
		End Do 
	Close(20)
	! **********************************************************
	End Subroutine

	Subroutine ImpDistancia(m0,mf)
	Integer:: m0, mf, x0, xf
 	x0 = sqrt(real(m0)) - 10
	xf = sqrt(real(mf)) + 10 
	
	! ********** Genera el Archivo para GNUplot **************	
	open(unit=20,file='ImpDistanciaFin.dat',form='formatted')
	  Write(20,*) 'set size square'
	  Write(20,*) 'set origin 0,0'
	  Write(20,*) 'set xzeroaxis'
	  Write(20,*) 'set yzeroaxis'
	  write(20,*) 'set logscale x 10'
	  Write(20,*) 'set xrange [', m0,':', mf,']'
	  Write(20,*) 'set yrange [', x0,':', xf,']'
	  Write(20,*) 'set title "DistanciaFin"'
	  Write(20,*) 'set xlabel "x"'
	  Write(20,*) 'set ylabel "y"'
	  Write(20,*) "plot 'DistanciaFin.dat' w p lw 2 lc 3" 
	  Write(20,*) "replot 'DistanciaFin.dat' notitle w yerrorbars"	
	 
	Close(20) 
	! **********************************************************
	End Subroutine





