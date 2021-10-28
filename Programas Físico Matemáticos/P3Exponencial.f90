  PROGRAM ExponencialTaylor
      IMPLICIT NONE
        INTEGER :: n,k ,factor           
        REAL*8 :: estimate, exponente     
        REAL*8 e,errorRP, eraprox , potencia
        INTEGER :: l         
	Integer i, j, u, un	
	Integer, allocatable :: p(:,:)
	Integer, allocatable :: f(:,:)

      
         n = 12
         WRITE(*,*) "Entra el exponente (valor de x) : "
        READ(*,*) exponente
        Potencia=exp(exponente) 
	
	Allocate(p(n,1))
	Do i=1,n
	 p(i,1) = i
	End do

	Allocate (f(n,1))	
	u=1
   	un= 1

	Do i= 1,n
	 u=u*un
	un=un+1	
	f(i,1)=u
	End do 

       factor=1
       estimate=1.0
        DO k = 1, n, 1
                factor = factor*k
                estimate = estimate + (exponente)**k/REAL(factor)
                eraprox=Potencia-estimate
                errorRP= (Potencia-estimate)/Potencia*100.0
	Write(*,*) k, estimate 
        END DO
        WRITE(*,*) "  "
        WRITE(*,*) " Potencia analitica es: ", Potencia
         WRITE(*,*) "  "
        WRITE(*,*) "La estimacion de la funcion es: ", estimate
         WRITE(*,*) "  "
        WRITE(*,*) "Error de aproximacion:", eraprox
        WRITE(*,*) "Error relativo porcentual: ", errorRP
	
	
	Write(*,*) "terminos del polinomio de Taylor calculados"
	Write(*,*) "	0 ,	 1"
	Do i= 1,n
	Write(*,*) i, " , 1/", f(i,1), "*X^", p(i,1)	
	End do

        PAUSE
        STOP
	
        END 

