
/*one-pass algorithm to calculate the covariance*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

/*
x -- the variable which is to calculate it's variance
length -- the length of the x, it is int
var -- the variance of x that we need, it is double
mx -- mean of x
my -- mean of y
mxprev -- mx(mean of x) previous value
myprev --my(mean of y) previous value

*/
void mycov(double *x, double *y, int *length, double *cov, double *meanx, double *meany)
{
	int N = *length, i;
	double C = 0,mx = 0, my = 0, mxprev = 0, myprev = 0; /* Cprev means the previous value of C */
	
	for ( i = 0; i < N; i++) {
		mxprev = mx;
                myprev = my;
                mx += (x[i] - mxprev)/(i+1);
                my += (y[i] - myprev)/(i+1);
                C += (x[i] - mx)*(y[i] - myprev);
       	}
*cov = C/(N-1);
*meanx = mx;
*meany = my;
}

