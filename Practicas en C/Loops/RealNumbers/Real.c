#include <stdio.h>

int main() {

	double num;
	
	printf("Dame tu Número tut: \n");
	scanf("%lf", &num);
	if (num >= 0.0) {
	   if (num == 0.0) {
	       printf("Tu número es 0.00 \n");
	   } else
	       printf("Tu número es positivo \n");
	} else
	    printf("Tu número es negativo \n"); 
     	return 0;
   }	



