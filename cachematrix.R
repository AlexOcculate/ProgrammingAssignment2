##
##   Function : makeCacheMatrix
##
##   Parameter: 'x' of type matrix()
##
##      Output: R-Object/List embedding the matrix passed as parameter.
##
##     Purpose: To create a R-Object that caches the result of matrix inversion
##              calculation.
##
## Assumptions: This function assumes the matrix supplied is always invertible!
##              Otherwise, a error will be generated.
##
##    Overview: For every [matrix] passed to this function, it will create a new
##              R-Object with 2 internal variables and 4 methods. The R-Object
##              created by this function should be used via [cacheSolve] function.
##              The [matrix inverse] will be calculated via the cacheSolve(). 
##
##   Variables:
##              - x : [matrix value]
##              - m : [matrix inverse]
##
##     Methods:
##              - set : the setter method for the [matrix value] and [resetter]
##                      for the [matrix inverse].
##              - get : the getter method for the [matrix value].
##              - setMatrixInverse : changes the [matrix inverse] cached.
##              - getMatrixInverse : reads the [matrix inverse] cached if it exists, NULL otherwise.
##
##         Use:
##
##              > D = matrix( c( 2, 3, 1, 5  ,  1, 0, 3, 1  ,  0, 2, -3, 2  ,  0, 2, 3, 1 ) , nrow=4 , ncol=4 )
##              > d <- makeCacheMatrix( D )      # no [matrix inverse] calculation do so far...
##              > dInv <- cacheSolve( d )        # calculation is done now by the first time...
##              > dInv <- cacheSolve( d )        # no calculation, since the previous was cached...
##              getting cached data
##              > d$set( matrix( c( 1,0,0 , 0,1,0 , 0,0,1 ) , nrow=3, ncol=3 ) )   # change the internal [matrix value]...
##              > dInv <- cacheSolve( d )        # calculation is done again because the [matrix value] changed...
##              > dInv <- cacheSolve( d )        # no calculation, since the previous was cached...
##              getting cached data
##              > d$get()                        # getting the [matrix value]...
##                   [,1] [,2] [,3]
##              [1,]    1    0    0
##              [2,]    0    1    0
##              [3,]    0    0    1
##
## Limitations: See cacheSolve().
##
##         See: cacheSolve()
##
makeCacheMatrix <- function(x = matrix()) {

   m <- NULL   # init [chache] for the [matrix inverse]...

   set <- function(y) {  # the setter for the [matrix value]...
      x <<- y            # save [actual] [matrix value].
      m <<- NULL         # signal the [matrix value] [changed] and postpone the calculation...
   }

   get <- function() x   # the getter for the [matrix value]...

   setMatrixInverse <- function( matrixInverse ) m <<- matrixInverse   # [cache] [matrix inverse] calculations...

   getMatrixInverse <- function() m   # get cached [matrix inverse] calculation...

   # this creates and returns the [R-Object/List] that will be used to handle the matrix...
   list( set = set
       , get = get
       , setMatrixInverse = setMatrixInverse
       , getMatrixInverse = getMatrixInverse
   )
}

##
##   Function : cacheSolve
##
##   Parameter: x is a R-Object previously created by makeCacheMatrix().
##
##      Output: Inverted matrix.
##
##     Purpose: To handle the R-Object created via makeCacheMatrix() and keep it
##              in a consistent state.
##
## Assumptions: This function assumes the matrix supplied is always invertible!
##              Otherwise, a error will be generated.
##
##    Overview: This function handles the verification, calculation, caching and
##              reading of a [matrix inverse] of a [matrix value] embedded in a
##              R-Object create by makeCacheMatrix().
##              The [matrix inverse] will be calculated via the R-function solve(). 
##
##   Variables:
##              - m    : [matrix inverse]
##              - data : [matrix value]
##
##     Methods: NONE.
##
##         Use: A good example can be found at 'makeCacheMatrix()' documentation! ;) 
##
## Limitations: After the [matrix inverse] was calculated and cached, if the original
##              [matrix value] has its value changed via others means than the 'set()'
##              method created by 'makeCacheMatrix()', the outputed [matrix inverse]
##              won't be in sync with the new value!
##              WORKAROUND: Always use the 'set()' method to change the [matrix value],
##                          or recreate it calling 'makeCacheMatrix()' with the new
##                          value.
##
##         See: makeCacheMatrix()
##
cacheSolve <- function(x, ...) {

   m <- x$getMatrixInverse()   # get [previous result], if it exists...

   if(!is.null(m)) {                  # if [previous result] does exist...
      message("Huh!..Huh!..Free lunch! Getting cached [matrix inverse]...")
      return(m)                       # return it...
   }

   data <- x$get()           # if [previous result] does not exist...
   m <- solve(data, ...)     # calculate the [matrix inverse]...
   x$setMatrixInverse(m)     # and cache it...
   m                         # and return it to the caller!
}
