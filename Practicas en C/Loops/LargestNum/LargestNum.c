#include <stdio.h> 

int main() {
	int i;
	double n1, n2, n3;
	
	printf("escribe tus númeritos bb ;) \n");
	scanf("%lf %lf %lf", &n1, &n2, &n3);

	if (n1 >= n2) {
	   if (n1 >= n3)
		printf("El número mayor es %.2lf \n", n1);
	   else 
		printf("El número mayor es %.2lf \n", n3);
	} 
	else {
	    if (n2 >= n3)
		printf("El número mayor es %.2lf \n", n2);
	    else 
		printf("El número mayor es %.2lf \n", n3);
	}
	
	return 0;
}	






