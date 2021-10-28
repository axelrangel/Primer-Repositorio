#include <stdio.h> 
#include <math.h>

double funcion (double x0, double y0);

int main () 
{
	double x0, y0, x, y, xf, k1, k2, k3, k4, h;
	int i, n; 
	FILE *fptr;

	   h = 0.01;	   		
	   y0 = 1;
	   x0 = 1;
	   xf = 5;
	   n = xf/h;

	fptr = fopen("/home/axel/Documentos/Cpracticas/RungeKutta/Grafica.dat", "w");	
		
	for ( i = 1; i <= n; ++i)
	 {	
	   x = x0;
	   y = y0;
	   k1 = h*funcion(x,y);

	   x = x0 + h/2;
	   y = y0 + k1/2;
	   k2 = h*funcion(x,y);

	   x = x0 + h/2;
	   y = y0 + k2/2;
	   k3 = h*funcion(x,y);

	   x = x0 + h;
	   y = y0 + k3;
	   k4 = h*funcion(x,y);
	
	   x0 = x0 + h;
	   y0 = y0 + k1/6 + k2/3 + k3/3 + k4/6;

	   fprintf(fptr,"%f \t %f \n", x0, y0);
	 } 
	
	fclose(fptr);

	return 0;
}

double funcion (double x0, double y0)
{
	double f0;
	f0 = pow(sin(x0),2)/x0;
	return f0;
}

