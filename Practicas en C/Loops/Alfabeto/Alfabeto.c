#include <stdio.h>

int main() {
	char f;
	printf("Dame un caracter \n");
	scanf("%c", &f);
	
	if ((f >= 'a') && ( f <= 'z')) {

	     printf("tu caracter pertenece al alfabeto \n");	
	}
	else if ((f >= 'A') && (f <= 'Z')) {

	     printf("tu caracter pertenece al alfabeto \n");
	}

	else {
	     printf("tu caracter no pertenece al alfabeto \n");
	}

	
	return 0;
}
