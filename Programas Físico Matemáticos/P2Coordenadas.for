	Program Coordenadas
	Implicit none
	Real*8:: x, y, z, r, t, p
	Integer:: op, o

	Write(*,*) '¿Qué tipo de coordenadas tienes?'
	Write(*,*) '1 - Cartesianas'
	Write(*,*) '2 - Cilindricas'
	Write(*,*) '3 - Esfericas'
	Write(*,*) 'Nota: Todos los angulos se miden en radianes ;)'
	
10	Read(*,*) op

	Select case(op)

	 case(1)
		Write(*,*)'Escribe tus coordenas x, y, z en ese orden'
		Read(*,*)x
		Read(*,*)y
		Read(*,*)z
		Write(*,*)'¿A qué sistema las vamos a pasar?'
		Write(*,*)'1 - Esfericas'
		write(*,*)'2 - Cilindricas'

20		 Read(*,*) o
 
		Select Case(o)
		 
		 case(1)

		 Write(*,*)'Estás son tus coordenas en cartesianas'
		 Write(*,*)'x :', x, 'y :', y, 'z :', z
		 Write(*,*)'Estás son tus coordenas en esfericas'
		  r= sqrt((x**2)+(y**2)+(z**2))
		  t= atan((sqrt((x**2)+(y**2))/z))
		  p= atan(y/x)
		 Write(*,*)'r :', r, 'teta :', t, 'Phi :', p
		
		 case(2)
		
		 Write(*,*)'Estás son tus coordenas en cartesianas'
		 Write(*,*)'x :', x, 'y :', y, 'z :', z
		 Write(*,*)'Estás son tus coordenas en cilindricas'
		  r=sqrt((x**2)+(y**2))
		  p= atan((y/x))
		  z= z
 		 Write(*,*)'r :', r, 'phi :', p, 'z :', z
		
		 case default
		 write(*,*) 'Elije otra opción'
		 go to 20
		
		 End Select

	 case(2)

		Write(*,*)'Escribe tus coordenas r, phi, z en ese orden'
		Read(*,*)r
		Read(*,*)p
		Read(*,*)z
		Write(*,*)'¿A qué sistema las vamos a pasar?'
		Write(*,*)'1 - Esfericas'
		write(*,*)'2 - Cartesianas'

30		 Read(*,*) o
 
		Select Case(o)
		 
		 case(1)

		 Write(*,*)'Estás son tus coordenas en cilindricas'
		 Write(*,*)'r :', r, 'phi :', p, 'z :', z
		 Write(*,*)'Estás son tus coordenas en esfericas'
		  r= sqrt((r**2)+(z**2))
		  t= atan(r/z)
		  p= p
		 Write(*,*)'r :', r, 'teta :', t, 'Phi :', p
		
		 case(2)
		
		 Write(*,*)'Estás son tus coordenas en cilindricas'
		 Write(*,*)'r :', r, 'phi :', p, 'z :', z
		 Write(*,*)'Estás son tus coordenas en cartesianas'
		  x= r*cos(p)
		  t= r*sin(p)
		  z= z
 		 Write(*,*)'x :', x, 'y :', y, 'z :', z
		
		 case default
		 write(*,*) 'Elije otra opción'
		 go to 30
		
		 End Select

	 case(3)

		Write(*,*)'Escribe tus coordenas r, phi, teta en ese orden'
		Read(*,*)r
		Read(*,*)p
		Read(*,*)t
		Write(*,*)'¿A qué sistema las vamos a pasar?'
		Write(*,*)'1 - Cilindricas'
		write(*,*)'2 - Cartesianas'

40		 Read(*,*) o
 
		Select Case(o)
		 
		 case(1)

		 Write(*,*)'Estás son tus coordenas en esfericas'
		 Write(*,*)'r :', r, 'phi :', p, 'teta :', t
		 Write(*,*)'Estás son tus coordenas en cilindricas'
		  r= r*sin(t)
		  p= p
		  z= r*cos(t)
		 Write(*,*)'r :', r, 'z :', z, 'Phi :', p
		
		 case(2)
		
		 Write(*,*)'Estás son tus coordenas en esfericas'
		 Write(*,*)'r :', r, 'phi :', p, 'teta :', t
		 Write(*,*)'Estás son tus coordenas en cartesianas'
		  x= r*sin(t)*cos(p)
		  y= r*sin(t)*sin(p)
		  z= r*cos(t)
		
 		 Write(*,*)'x :', x, 'y :', y, 'z :', z
		
		 case default
		 write(*,*) 'Elije otra opción'
		 go to 40
		
		 End Select


	 case default

	 write(*,*) 'Elije otra opción'
	 go to 10

	End Select

	End Program
