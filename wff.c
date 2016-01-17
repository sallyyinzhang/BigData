
/*Welford’s method to calculate*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

/*
x -- the variable which is to calculate it's variance
length -- the length of the x, it is int
var -- the variance of x that we need, it is double


*/
void welf_var( double *x, int *length, double *var)
{
	int N = *length, i;
	double M = 0, S = 0, Mprev = 0; /*Mprev means the previous value of M*/

/*use the iteration mentioned in the report part 2.2.1*/	
	for ( i = 0; i < N; i++) {
		Mprev = M;
		M += (x[i] - Mprev)/(i+1);
                S += (x[i] - Mprev)*(x[i] - M);
       	}
*var = S/(N-1);
}
