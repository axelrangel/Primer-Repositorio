#include <stdio.h>

int main() {
    char c;
    int minusculas;
    printf("Dame una letra minuscula \n");
    scanf("%c", &c);

    minusculas = (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u');
    if (minusculas == 1) 
	printf("tu letra es una vocal %c \n", c);
    else
	printf("no es una vocal :C \n");
    return 0;
}
