#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
/*
The suff function is a function which used to calculate the sufficient statistics (mentioned in report part2.2.1Methods)
Input:
fare: charactor, the file name of trip_fare, pay attention to put whole path of the file 
data: charactor, the file name of trip_data
x: one of the sufficient statsitics, sum x(trip time), should be double because there might be decimals
y: sum of y(less value of total and tolls), double for same reason as x
xy: sum of trip time *less value, double for same reason as x
x2: the square sum of trip time, double for same reason as x

output:
the same as the input, the value that we need is x, y, xy, x2
*/



void suff(char **fare, char **data, double *x, double *y, double *xy, double *x2)   /* argv[] is the file name */
{
    FILE *fp = fopen(fare[0], "r");
    FILE *dp = fopen(data[0], "r"); /* only read the filname, and the file name is such as ""/home/data/NYCTaxis/trip_fare_1.csv */
    
    char linef[1024], lined[1024];
    char *toll = NULL, *total = NULL, *t = NULL;  
    int i, j;
    double less = 0, tt = 0, sumx = *x, sumy = *y, sumxy = *xy, sumx2 = *x2;

    fgets(linef,1024,fp);  /* except the first line which is for the names for fare */
    fgets(lined,1024,dp); /* except the first line which is for the name for data */
     
    while( fgets(linef,1024,fp) ) /*if not end of the file*/
    {   fgets(lined,1024,dp); 

        char *f = linef;
        char *d = lined;

        /*calculate the sum of total amount less then tolls */
        for(i = 0; i<10; i++){
            toll = strsep(&f, ","); /*tolls_amount*/
        }
            total = strsep(&f, "/n"); /* total_amount*/
        less = atof(total) - atof(toll);
        sumy = sumy + less;
        
        /*calculate the sum of trip time*/
        for(j = 0; j<9; j++){
            t = strsep(&d, ","); /*tolls_amount*/
        }
        tt = atof(t);
        sumx = sumx + tt;
        
        /*the other sufficient statistics, sum of xy and the sum of square x */
        sumxy = sumxy + less*tt;
        sumx2 = sumx2 + pow(tt,2);
        
    }
*x = sumx;
*y = sumy;
*xy = sumxy;
*x2 = sumx2;

}
