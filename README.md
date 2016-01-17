# BigData

Working with the New York taxi data. These are available http://www.andresmh.com/nyctaxitrips/ and there is a description of the fields at http://publish.illinois.edu/dbwork/open-data/.

tasks are:

1 Compute the deciles of the total amount less the tolls.
2 Fit a linear regression predicting total amount less the tolls using trip time as the predictor.

If doing the computations by individual row or blocks of row, make certain to account for numerical imprecision, e.g., when computing the mean by maintaining the total and the sample size. For example, consider Welford's method described in John Cook's blog entry. Also, see Wikipedia. Welford's paper is available via JSTOR and is only 2 pages with some algebra to illustrate/derive the updating formula.

Get your approaches working on one of the files, then extend the computations to the other files.

You might be interested in exploring other packages such as bigmemory, data.table and its fread() function.


