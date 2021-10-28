	Program Rip
	Implicit none
	Integer:: i, m, n, j, a, b, br,ar 
	Real*8:: rn, p, s, ss, sp

	Write(*,*) 'Dame a y b iniciales, dame las iteraciones m y n'
	Read(*,*) ar,br, m, n

	open(unit=20,file='Revision.dat',form='formatted')
	i = 0 
	j = 0
	Do while (j .le. n)
	a = ar
	b = br
	   Do While (i .le. m)
		Call srand(a)
			a = a*b - a/b
			rn = rand(a)
		write(20,*) rn
		i = i + 1
	   End DO 
	ar = abs(ar*3 - 7)
	br = abs(br*5 - 11)
	i = 0 
	j = j + 1
	End Do
	End Program
