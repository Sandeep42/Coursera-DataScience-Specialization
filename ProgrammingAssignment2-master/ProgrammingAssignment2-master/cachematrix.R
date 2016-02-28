## Cachematrix.R evaluates the inverse of a function. If the inverse is already calculated,
## it returns the answer from the cacher, otherwise it computes the inverse.
##

## makeCacheMatrix returns a list of  four functions, one sets the values of matrix, one gets the values of the matrix, one computes the inverse, and one gets the inverse.


makeCacheMatrix <- function(x = matrix()) {
        inv <<- NULL
        set <-function(y) {
                x <<-y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse= setinverse, getinverse=getinverse)
        

}


## cacheSolve checks for the cached inverse, if the inverse is already computed it returns from cache. Otherwise it computes the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cache")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
##Trail Run
# > x<- matrix(1:4,2,2)
# > m<- makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# getting cache
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
