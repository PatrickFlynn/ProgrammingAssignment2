##the following functions create a cached matrix that will get and set the value 
## of the matrix as well as setting and getting the inverse of the matrix. If the
## inverse is not already cached, a "cacheSolve" function will create the inverse
## (and will also return the inverse if already cached!)

##a set of commands are available at the end of the code for testing functionality


## This function creates a special matrix that allows you to set/get the values of the matrix
## and set/get the values of the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inverse <<- solve
    getInverse <- function() inverse
    list(set = set, get = get)
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns the inverse of a given matrix if it has already been
## cached, OR it solves the inverse of the matrix and assigns it to the given matrices 
## setInverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}


##Testing Functionality
B = matrix(c(2, 4, 3, 1), nrow=2, ncol=2)
test <- makeCacheMatrix(B) #creates special matrix
test$get() #should return value of matrix
test$getInverse() #Should return NULL
cacheSolve(test) #will now calculate value of inverse
test$getInverse() #Should now return inverse
cacheSolve(test) #should properly return "getting cached data"

