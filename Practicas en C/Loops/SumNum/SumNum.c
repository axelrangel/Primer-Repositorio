#include  <stdio.h> 

int main() {
	int n, i, s = 0;
	
	printf("¿Hasta que número te calculo la suma?\n");
	scanf("%d", &n);
	
	for(i = 1; i <= n; ++i) {

	 s += i;

	}

	printf("Aqui ta' %d \n", s);	

	return 0; 
}

