#include <stdio.h>

int main() {
	int year;
	printf("A ver, dime un año \n");
	scanf("%d", &year);
	
	if (year % 4 == 0) {
	   if (year % 100 == 0) {
	      if (year % 400 == 0) {
		printf("%d es un año bisiesto \n", year);
		   	
	      } else
	       printf("%d no es un año bisiesto\n", year);
	   } else
	    printf("%d es un año bisiesto \n", year);
	} else
	   printf("%d no es un año bisiesto \n", year);
	
	return 0;
}
