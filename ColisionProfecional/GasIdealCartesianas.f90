	Program Rip
	Implicit none
	character(len=70) :: fn
	Integer:: i, m, n, j, c, ri, rj, jj, cc
	Integer:: x0, xf, lf, l, k, dl, nn, ii,l0
	Real*8:: rnn, kp, g, sd, y0, yf
	Real*8, dimension (:,:), allocatable :: r, rn, sdv

	Write(*,*)'Dame el número de pasos'
	Read(*,*) m
	Write(*,*)'Dame el número de partículas'
	Read(*,*) n
	Write(*,*)'Dame la distancia del centro a las paredes de la caja, inicial y final'
	Read(*,*) l, lf
	Write(*,*)'Dame el aumento en la distancia del centro a las parede de la caja'
	Read(*,*) dl
	Write(*,*)'Cuantas Repeticiones hago :C ?'
	Read(*,*) nn

	l0 = l
	 	
	
	Open(unit=n+3,file='GraficaPV.dat',form='formatted')
	
		 Allocate(r(3,n))
		 Allocate(rn(1,n))
		 Allocate(sdv(1,nn))	

	Do while (l .le. lf)
	 kp = 0
	  Do ii=1, nn	
	
	   Open(unit=n+2,file='Resultados.dat',form='formatted')
	   Open(unit=n+1,file='Colisiones.dat',form='formatted')	 

	   Do j=1, n
	      write(fn,fmt='(i0,a)') j, '.dat'
	      open(unit=j,file=fn, form='formatted')	
	   End Do	
	 
! *************** POSICIÓN ALEATORIA *****************************	
	   Do i=1, m
	     Do j=1, n
	  	    Call random_number(rnn)
		    Call Paso(r,rnn,j)
		    Call ColisionWall(r,n,l,c)
	     End Do
	   End Do
			
	   c = 0
	   cc = 0	
	
	   Do j=1, n
	 	  write(j,*) r(1,j), r(2,j), r(3,j)
	   End Do
! ****************************************************************
! *********** MOVIMIENTO Y COLISIONES ****************************
	   Do i=1, m
	  
	     Do j=1, n
	  	    Call random_number(rnn)
		    rn(1,j)=rnn
		    Call Paso(r,rnn,j)
		    Call ColisionWall(r,n,l,c)
	     End Do

	     Do j=1, n
		    Call ColisionP(r,n,rn,cc)
		    write(j,*) r(1,j), r(2,j), r(3,j)
	     End Do

	   End Do
! ***************************************************************		
	   Do j=1, n
	       close(j)
	   End Do
	
	   Close(n+1) 
		  Write(n+2,*) 'Número de colisiones con Paredes:',c	
	   	  Write(n+2,*) 'Número de colisiones con Partículas:',cc	
		  k = cc + c
		  Write(n+2,*) 'Número Total de Colisiones:',k
		  Write(n+2,*) 'Voy en el radio:',l
	   Close(n+2)
	
	   Call ImpTraye(l,j,n)
	   sdv(1,ii) = real(k) 
	   kp = kp + real(k)
	  End Do
	 kp = kp/real(nn)

	   Do ii=1, nn
		sd = sd+(kp-sdv(1,ii))**2
	   End Do
	
	  sd = sqrt(sd/real(nn))		

	 Write(n+3,*) l,kp,sd

	  If (l .eq. l0) then
		g=kp
	  End If

	 l = l + dl
	End Do 

	Close(n+3)
	 
	Call ImpPV(l,g)

	
	End Program

! ##################################################################
! ************* SUBRUTINAS *****************************************
! ##################################################################

	Subroutine Paso(r,rn,j)
	Real*8 :: rn 
	Real*8 :: r(3,j)

		If (rn .gt. 0) then
		 If (rn .le. 0.166666) then
		 r(1,j) = r(1,j) - 1
		 End If 
		End If

		If (rn .gt. 0.166666) then
		 If (rn .le. 0.333333) then
		  r(1,j) = r(1,j) + 1
		 End If 
		End If 
		
		If (rn .gt. 0.333333) then
		 If (rn .le. 0.500000) then
		  r(2,j) = r(2,j) - 1
		 End If 
		End If 
		
		If (rn .gt. 0.500000) then
		 If (rn .le. 0.666666) then
		  r(2,j) = r(2,j) + 1
		 End If 
		End If 
		
		If (rn .gt. 0.666666) then
		 If (rn .le. 0.833333) then
		  r(3,j) = r(3,j) + 1
		 End If 
		End If 
		
		If (rn .gt. 0.833333) then
		 If (rn .le. 1.000000) then
		  r(3,j) = r(3,j) - 1
		 End If 
		End If 

	End Subroutine 

! ****************************************************************

	Subroutine ColisionP(r,n,rn,cc)
	Real*8 :: r(3,n), rn(1,n)
	Integer :: n,rj,cc,jj
		jj=1
		Do rj=1, n
		 jj = rj + 1
		 Do while (jj .le. n)

			  If (r(1,rj) .eq. r(1,jj)) then
			   If (r(2,rj) .eq. r(2,jj)) then
			    If (r(3,rj) .eq. r(3,jj)) then
		
			    cc = cc + 1
			    write(n+1,*) r(1,rj), r(2,rj), r(3,rj) 
		
			    	If (rn(1,rj) .gt. 0) then
		 		 If (rn(1,rj) .le. 0.166666) then
		 		  r(1,rj) = r(1,rj) + 1
		 		 End If 
				End If

				If (rn(1,rj) .gt. 0.166666) then
		 		 If (rn(1,rj) .le. 0.333333) then
		  		  r(1,rj) = r(1,rj) - 1
		 		 End If 
				End If 
		
				If (rn(1,rj) .gt. 0.333333) then
		 		 If (rn(1,rj) .le. 0.500000) then
		  		  r(2,rj) = r(2,rj) + 1
		 		 End If 
		  	        End If 
		
				If (rn(1,rj) .gt. 0.500000) then
				 If (rn(1,rj) .le. 0.666666) then
		  		  r(2,rj) = r(2,rj) - 1
		 		 End If 
				End If 
		
				If (rn(1,rj) .gt. 0.666666) then
		 		 If (rn(1,rj) .le. 0.833333) then
		  		  r(3,rj) = r(3,rj) - 1
		 		 End If 
				End If 
		
				If (rn(1,rj) .gt. 0.833333) then
		 		 If (rn(1,rj) .le. 1.000000) then
		  		  r(3,rj) = r(3,rj) + 1
				 End If 
				End If 
							    	
				If (rn(1,jj) .gt. 0) then
		 		 If (rn(1,jj) .le. 0.166666) then
		 		  r(1,jj) = r(1,jj) + 1
		 		 End If 
				End If

				If (rn(1,jj) .gt. 0.166666) then
		 		 If (rn(1,jj) .le. 0.333333) then
		  		  r(1,jj) = r(1,jj) - 1
		 		 End If 
				End If 
		
				If (rn(1,jj) .gt. 0.333333) then
		 		 If (rn(1,jj) .le. 0.500000) then
		  		  r(2,jj) = r(2,jj) + 1
		 		 End If 
		  	        End If 
		
				If (rn(1,jj) .gt. 0.500000) then
				 If (rn(1,jj) .le. 0.666666) then
		  		  r(2,jj) = r(2,jj) - 1
		 		 End If 
				End If 
		
				If (rn(1,jj) .gt. 0.666666) then
		 		 If (rn(1,jj) .le. 0.833333) then
		  		  r(3,jj) = r(3,jj) - 1
		 		 End If 
				End If 
		
				If (rn(1,jj) .gt. 0.833333) then
		 		 If (rn(1,jj) .le. 1.000000) then
		  		  r(3,jj) = r(3,jj) + 1
				 End If 
				End If 


			    End IF
			   End If		
			  End If
		    jj = jj + 1
          	 End Do
		End Do 

	End Subroutine 

! ***********************************************************************

	Subroutine ColisionWall(r,n,l,c)
	Real*8 :: r(3,n)
	Integer :: n,rj,ri,c, l

		Do rj=1, n
		 Do ri=1, 3

		  If (r(ri,rj) .eq. l) then	
		    c = c + 1
		    write(n+1,*) r(1,rj), r(2,rj), r(3,rj) 
		    r(ri,rj) = r(ri,rj) -1 		
		  End If

		 End Do 
		End Do 

		Do rj=1, n
		 Do ri=1, 3

		  If (r(ri,rj) .eq. -1*l) then	
		    c = c + 1 
		    write(n+1,*) r(1,rj), r(2,rj), r(3,rj) 
		    r(ri,rj) = r(ri,rj) +1 
		  End If

		 End Do 
		End Do 

	End Subroutine 

! **************************************************************************	

	Subroutine ImpTraye(l,j,n)
	Integer:: j, n, x0, xf, l
	character(len=70) :: fn
	
	x0 = -1*(l + 2)
	xf = l + 2
	! ********** Genera el Archivo para GNUplot **************	
	open(unit=20,file='Partículas.dat',form='formatted')
	  Write(20,*) 'set size square'
	  Write(20,*) 'set origin 0,0'
	  Write(20,*) 'set xzeroaxis'
	  Write(20,*) 'set yzeroaxis'
	  Write(20,*) 'set zzeroaxis'
	  Write(20,*) 'set xrange [', x0,':', xf,']'
	  Write(20,*) 'set yrange [', x0,':', xf,']'
	  Write(20,*) 'set zrange [', x0,':', xf,']'
	  Write(20,*) 'set title "Partículas"'
	  Write(20,*) 'set xlabel "x"'
	  Write(20,*) 'set ylabel "y"'
	  Write(20,*) 'set zlabel "z"'
	  Write(20,*) "splot '1.dat' notitle w d lw 3 lc 1"
		Do j=2 , n
		  write(fn,fmt='(i0,a)') j, ".dat'"
		  write(20,*) "replot '",fn,"notitle w d lw 3 lc",j
		End Do
	  Write(20,*) "replot 'Colisiones.dat' w p lw 3 lc 8"
	Close(20)
	! **********************************************************
	End Subroutine

! ************************************************************************
	Subroutine ImpPV(l,g)
	Integer:: l
	Real*8:: g, y0, yf
 	y0 = real(l) + 2
	yf = g + 100
	
	! ********** Genera el Archivo para GNUplot **************	
	open(unit=20,file='ImpGraficaPV.dat',form='formatted')
	  Write(20,*) 'set size square'
	  Write(20,*) 'set origin 0,0'
	  Write(20,*) 'set xzeroaxis'
	  Write(20,*) 'set yzeroaxis'
	  write(20,*) 'set logscale y 10'
	  Write(20,*) 'set xrange [0 :',y0,']'
	  Write(20,*) 'set yrange [0 :',yf,']'
	  Write(20,*) 'set title "P vs V"'
	  Write(20,*) 'set xlabel "Radio (V)"'
	  Write(20,*) 'set ylabel "Colisiones (P)"'
	  Write(20,*) "plot 'GraficaPV.dat' w p lw 2 lc 1" 
	  Write(20,*) "replot 'GraficaPV.dat' notitle w yerrorbars"	
	 
	Close(20) 
	! **********************************************************
	End Subroutine


