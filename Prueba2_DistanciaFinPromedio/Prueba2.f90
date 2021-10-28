	Program Rip
	Implicit none
	character(len=70) :: fn
	Integer:: i, m, n, j
	Integer:: x0, xf
	Real*8:: rn, d, s, pd
	Real*8 :: r(1,2)

	Write(*,*)'Dame el número de pasos'
	Read(*,*) m
	Write(*,*)'Dame el número de caminatas'
	Read(*,*) n
	
	j = 1
	s = 0

	open(unit=30,file='PosiciónyDistancia.dat', form='formatted')
	Write(30,*) 'x				y		Distancia'

	Do while (j .le. n)
	r(1,1) = 0
	r(1,2) = 0 
	i = 0 
!       Construye NombreDelArchivo -- i.dat
	write(fn,fmt='(i0,a)') j, '.dat'
! **************************************************

	open(unit=10,file=fn, form='formatted')

	   Do While (i .lt. m)
		Call random_number(rn)
		Call Paso(r,rn)
		write(10,*) r(1,1), r(1,2)
		i = i + 1
	   End DO 

	close(10)

	d = sqrt((r(1,1)**2)+(r(1,2)**2))
	j = j + 1
	s = s + d

	Write(30,*) r(1,1), r(1,2), d

	End Do

	pd = s/real(n)
	Write(30,*) 'Promedio de distancia:', pd
	close(30)
	Write(*,*) 'Promedio de distancia:', pd

	Call ImpTraye(pd,j,n,fn)

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





