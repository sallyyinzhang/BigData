#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/*
function for calculation the less time of between total and tolls
input:
filename: ths file that we want to calculate, pay attention the filename should include the whole path of the file
fee: it is a array, which is the less value of the the whole file in each line 
output:
The same as input, and the fee is what we need of the less value
*/

void cdif(char **filename, double *fee)   
{
    FILE *fp = fopen(filename[0], "r"); /* only read the filname, and the file name is such as ""/home/data/NYCTaxis/trip_fare_1.csv */
    
    char line[1024];
    char *toll = NULL, *total = NULL;  
    int n = 0, i;

    fgets(line,1024,fp); /* except the first line which is for */
     
    while( fgets(line,1024,fp) ) /*if not end of the file*/
    {    
        char *b = line;
        for(i = 0; i<10; i++){
            toll = strsep(&b, ","); /*tolls_amount*/
        }
            total = strsep(&b, "/n"); /* total_amount*/
        fee[n] = atof(total) - atof(toll);
        n = n + 1;
    }
}
