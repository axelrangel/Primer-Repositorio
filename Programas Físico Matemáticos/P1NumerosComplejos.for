	Program Numeroscomplejos
	Implicit None
	Real*8 :: x, y, u, v, t, r, yp, z1, z2, rx, ry 
	Integer :: op, o

	Write (*,*) 'Escribe 1 si quieres cambiar la forma del'
	Write (*,*) 'numero complejo, escribe 2 si quieres'
	write (*,*) 'Realizar alguna operacion entre numeros'	

	Read (*,*) op

	If (op==1) then

	 Write (*,*) 'Elije la opción que desees'
	 Write (*,*) '1- Conjugado de NC'
	 Write (*,*) '2-Cambiar de cartesianas a polares'
	 Write (*,*) '3-Cambiar de polares a cartesianas'
	 Write (*,*) '4-Calcular modulo y argumento'

10	 Read (*,*) o

	   Select case (o)
		case(1)

		 Write (*,*)'Escribe la parte real "x"' 
		 Read (*,*) x
		 Write (*,*)'Escribe la parte imaginaria "y"'
		 Read (*,*) y 
		 Write(*,*) 'Este es el número que me diste'
		 Write(*,*) x, '+', 'i', y
		 Write(*,*) 'Este es el conjugado'
		  yp = y*(-1)
		 Write(*,*) x, '+', 'i', yp
	
		case(2)
		 
		 Write (*,*)'Escribe la parte real "x"' 
		 Read (*,*) x
		 Write (*,*)'Escribe la parte imaginaria "y"'
		 Read (*,*) y 
		 Write(*,*) 'Este es el número que me diste'
		 Write(*,*) x, '+','i', y
 		   r = sqrt((x**2)+(y**2))
		   t = atan(y/x)
		 Write(*,*) 'Este es tu número en polares (en radianes)'
		 Write(*,*) 'teta =', t, 'r =',r

		case(3) 

		 Write (*,*)'Escribe el angulo teta del número (en radianes)' 
		 Read (*,*) t
		 Write (*,*)'Escribe el radio r del numero'
		 Read (*,*) r 
		 Write(*,*) 'Este es el número que me diste'	
		 Write(*,*) 'teta =', t, 'r =',r
		   x = cos(t)*r
		   y = sin(t)*r
		 Write(*,*) 'Este es tu número en cartesianas'
		 Write(*,*) x, '+','i', y

		case(4)

		 Write (*,*)'Escribe la parte real "x"' 
		 Read (*,*) x
		 Write (*,*)'Escribe la parte imaginaria "y"'
		 Read (*,*) y 
		 Write(*,*) 'Este es el número que me diste'
		 Write(*,*) x, '+','i', y
	 	    r = sqrt((x**2)+(y**2))
		    t = atan(y/x)
		 Write(*,*) 'Este es el modulo y el argumento'
		 Write(*,*) 'argumento =', t, 'modulo =',r

		case default
		 Write (*,*) 'Elije una de las opciones anteriores'
		 goto 10
	
	   End Select 	
	
	else 

	  If (op==2) then

	 Write (*,*) 'Elije la opción que desees'
	 Write (*,*) '1- Suma'
	 Write (*,*) '2- Resta'
	 Write (*,*) '3- Multiplicación'
	 Write (*,*) '4- División'
	 Write (*,*) '5- Potenciación'

20	 Read (*,*) o

	   Select case (o)
		case(1)

		 Write (*,*)'Escribe la parte real del primer número "x"' 
		 Read (*,*) x
		 Write (*,*)'Escribe la parte imaginaria del primer número"y"'
		 Read (*,*) y 
		 Write (*,*)'Escribe la parte real del segundo número "u"' 
		 Read (*,*) u
		 Write (*,*)'Escribe la parte imaginaria del segundo número"v"'
		 Read (*,*) v 
		 Write(*,*) 'Estos son los números que me diste'
		 Write(*,*) x, '+', 'i', y
		 Write(*,*) u, '+', 'i', v
		 Write(*,*) 'Esta es la suma'
		  	z1 = x + u
			z2 = y +v
		 Write(*,*) z1, '+', 'i', z2
	
		case(2)
		 
		 Write (*,*)'Escribe la parte real del primer número "x"' 
		 Read (*,*) x
		 Write (*,*)'Escribe la parte imaginaria del primer número"y"'
		 Read (*,*) y 
		 Write (*,*)'Escribe la parte real del segundo número "u"' 
		 Read (*,*) u
		 Write (*,*)'Escribe la parte imaginaria del segundo número"v"'
		 Read (*,*) v 
		 Write(*,*) 'Estos son los números que me diste'
		 Write(*,*) x, '+', 'i', y
		 Write(*,*) u, '+', 'i', v
		 Write(*,*) 'Esta es la resta'
		  	z1 = x - u
			z2 = y - v
		 Write(*,*) z1, '+', 'i', z2

		case(3) 

		 Write (*,*)'Escribe la parte real del primer número "x"' 
		 Read (*,*) x
		 Write (*,*)'Escribe la parte imaginaria del primer número"y"'
		 Read (*,*) y 
		 Write (*,*)'Escribe la parte real del segundo número "u"' 
		 Read (*,*) u
		 Write (*,*)'Escribe la parte imaginaria del segundo número"v"'
		 Read (*,*) v 
		 Write(*,*) 'Estos son los números que me diste'
		 Write(*,*) x, '+', 'i', y
		 Write(*,*) u, '+', 'i', v
		 Write(*,*) 'Esta es la multiplicación'
		  	z1 = x*u - y*v
			z2 = y*u + v*x
		 Write(*,*) z1, '+', 'i', z2

		case(4)

		 Write (*,*)'Escribe la parte real del primer número "x"' 
		 Read (*,*) x
		 Write (*,*)'Escribe la parte imaginaria del primer número"y"'
		 Read (*,*) y 
		 Write (*,*)'Escribe la parte real del segundo número "u"' 
		 Read (*,*) u
		 Write (*,*)'Escribe la parte imaginaria del segundo número"v"'
		 Read (*,*) v 
		 Write(*,*) 'Estos son los números que me diste'
		 Write(*,*) x, '+', 'i', y
		 Write(*,*) u, '+', 'i', v
		 Write(*,*) 'Esta es la división'
			z1 = (x*u+y*v)/((u**2)+(v**2))
			z2 = (u*y-v*x)/((u**2)+(v**2))
		 Write(*,*) z1, '+', 'i', z2

		case(5)
		
		 Write (*,*)'Escribe la parte real del primer número "x"' 
		 Read (*,*) x
		 Write (*,*)'Escribe la parte imaginaria del primer número"y"'
		 Read (*,*) y 
		 Write (*,*)'Escribe la parte real del segundo número "u"' 
		 Read (*,*) u
		 Write (*,*)'Escribe la parte imaginaria del segundo número"v"'
		 Read (*,*) v 
		 Write(*,*) 'Estos son los números que me diste'
		 Write(*,*) x, '+', 'i', y
		 Write(*,*) u, '+', 'i', v
		 Write(*,*) 'Esta es la división'
			t = atan(y/x)
			rx = cos((u*t)+(0.5*v*log((x**2)+(y**2))))
			ry = sin((u*t)+(0.5*v*log((x**2)+(y**2))))
			z1 = (((x**2)+(y**2))**(u/2))*(exp(-v*t))*rx
			z2 = (((x**2)+(y**2))**(u/2))*(exp(-v*t))*ry
		 Write(*,*) z1, '+', 'i', z2


		case default
		 Write (*,*) 'Elije una de las opciones anteriores'
		 goto 20
	
	   End Select 	
	

	end if
	end if

	End Program 

	
