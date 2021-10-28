	Program Colisiones
	Implicit none
	Real*16 :: m1, m2, vi1, vi2, vf1, vf2
	Real*16 :: k, p, m, u, a, b, c, d
	Real*16 :: x1, x2, r1, r2, xv, yv, yc, y
	Integer :: s
	
	!Write(*,*) 'Necesito saber la masa de 1 y 2'
	!Read(*,*) m1
	!Read(*,*) m2
	!Write(*,*) 'Con que velocidad lanzaste 1'
	Write(*,*)'Cuantos digitos de pi quieres calcular con colisiones'
	Read(*,*) d
	m1= 100**(d-1)
	m2=1
 	vi1 = -1 
	vi2 = 0
	s=0
	yc=1
	y=2
	Do while (y .ge. yc)
	Write(*,*) '********COLISIONES', s, 'Y',s+1,'********'	
	 p = m1*vi1 + m2*vi2
	 k = m1*(vi1**2) + m2*(vi2**2)
	Write(*,*) 'momento',p,'energía',k	
	 m = sqrt(m1/m2)
	 u = p/sqrt(m2)
	!Write(*,*) 'm',m,'u',u	
	 a = 1 + (m**2)
	 b = -2*m*u
	 c = (u**2)-k 
	!Write(*,*) 'a',a,'b',b,'c',c	
	 r1 = (-b + sqrt((b**2)-4*a*c))/(2*a)
	 r2 = (-b - sqrt((b**2)-4*a*c))/(2*a)
	!Write(*,*) 'r1',r1,'r2',r2
		If (r2 .gt. r1) then 
			xv = r2
		else
			xv = r1
		end if 
	!write(*,*) 'xv', xv
	 yv = -1*m*xv + u
	 y = -1*yv
	!write(*,*) 'yv', yv
	!write(*,*) 'y', y
	 Vi1 = xv/sqrt(m1)	
	 Vi2 = -1*yv/sqrt(m2)
	write(*,*) 'velocidad del blocke 1', vi1
	write(*,*) 'velocidad del blocke 2', vi2
	 s = s + 2
	!write(*,*) 's',s
	 yc = sqrt(m2/m1)*xv 
	!write(*,*) 'yc', yc
	End do
	s= s-1
	write(*,*) 'Estos son los dijistos de pi calculados'	
	write(*,*) s

	End program 



