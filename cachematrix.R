##------------------------------------------------------------
## Notes:  there are some good resources at the following:
##  http://www.ats.ucla.edu/stat/r/library/matrix_alg.htm
##  https://class.coursera.org/rprog-013/forum/list?forum_id=10012
##------------------------------------------------------------
## Store a matrix object and it's inverse.
## This is a storage object only, no calculations are done here.
makeCacheMatrix <- function(x = matrix())
{
    inverseMatrix <- NULL;

    ##--------------------
    ## Stores a new input matrix and NULL's the cached inverse.
    ## The '<<-' operator is used to store values to objects in
    ## different environment spaces.
    set <- function(newMatrix)
    {
        x <<- newMatrix;
        inverseMatrix <<- NULL;
    }

    ##--------------------
    ## Returns the matrix.
    get <- function()
    {
        return (x);
    }

    ##--------------------
    ## Stores the inverse matrix.
    ## The '<<-' operator is used to store values to objects in
    ## different environment spaces.
    setInverse <- function(inverse)
    {
        inverseMatrix <<- inverse;
    }

    ##--------------------
    ## Returns the cached inverse matrix (or NULL if one hasn't been set).
    getInverse <- function(x)
    {
        return (inverseMatrix);
    }

    ##--------------------
    ## Return a list containing the 4 setMatrix/getMatrix and
    ## setInverse/getInverse functions.
    list (
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    );
}

##------------------------------------------------------------
## Computes the inverse of the matrix.  If the inverse has already been
## calculated and is stored above, then the cached inverse is simply retrieved.
cacheSolve <- function(x, ...)
{
    ## Get the inverse matrix from the 'x' parameter.
    inverseMatrix <- x$getInverse();

    ## If the inverseMatrix is not null, then use it.
    if (!is.null(inverseMatrix))
    {
        ## Print a courtesy message.
        print("Using cached inverse matrix...");
    }
    else
    {
        ## Print a courtesy message.
        print("Calculating inverse matrix...");

        ## Get the matrix data from 'x'.
        matrixData = x$get();

        ## Calculate the inverse matrix and store it in inverseMatrix.
        inverseMatrix <- solve(matrixData, ...);

        ## Store the inverseMatrix in the 'x' storage area.
        x$setInverse(inverseMatrix);
    }

    ## Return a matrix that is the inverse of 'x'
    return (inverseMatrix);
}

##------------------------------------------------------------
## This is a benchmarking routine that calculates the time it
## takes to calculate an inverse matrix vs. retrieving it from cache.
## Parameters:
##      matrix -- must be an invertable matrix.
benchmarks = function(testMatrix)
{
    ## Create a 'cache matrix object' using the makeCacheMatrix function above.
    cacheMatrix = makeCacheMatrix(testMatrix);

    ## Now, do a couple of cacheSolve iterations and compare run times.

    ##---------------
    ## The first call to cacheSolve() will calculate the inverse matrix.

    ## Save the starting time.
    time0 = Sys.time();

    ## Do the cacheSolve process.
    cacheSolve(cacheMatrix);

    ## Save the stop time.
    time1 = Sys.time();

    ## Calculate the duration.
    message(sprintf("Calculating the inverse matrix took: %f seconds.", (time1 - time0)));

    ##---------------
    ## The 2nd call to cacheSolve() will use the cached data.

    ## Save the starting time.
    time0 = Sys.time();

    ## Do the cacheSolve process.
    cacheSolve(cacheMatrix);

    ## Save the stop time.
    time1 = Sys.time();

    ## Calculate the duration.
    message(sprintf("Retrieving a cached inverse matrix took: %f seconds.", (time1 - time0)));
}
