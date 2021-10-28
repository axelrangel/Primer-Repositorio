#include <stdio.h>
#include <math.h>

int main() {
	double a, b, c, discriminante, raiz1, raiz2;
		
	printf("A ver, pasame los a, b y c de tu cuadratica \n");
	scanf("%lf %lf %lf", &a, &b, &c);
	//printf("%lf %lf %lf \n", a, b, c);
	discriminante = b*b - 4*a*c;
	
	if (discriminante == 0){
		raiz1 = - b / 2*a;
		printf("las raices son: X1 = %lf y X2 = %lf \n", raiz1, raiz1); 
	}
	else if (discriminante > 0){
		raiz1 = (- b + sqrt(discriminante)) / 2*a;
		raiz2 = (- b - sqrt(discriminante)) / 2*a;
		printf("las raices son: X1 = %lf y X2 = %lf \n", raiz1, raiz2);
	}
	else { 
	       raiz1 = -b/ 2*a;
	       raiz2 = sqrt(-discriminante)/2*a;
	       printf("las raices son X1 = %lf + i%lf \n", raiz1, raiz2);
	       printf("X2 = %lf - i%lf \n", raiz1, raiz2);
	}
	return 0;
 }
