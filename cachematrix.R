#makeCacheMatrix and cacheSolve functions help achieve matrix inversion which is usually a costly 
#computation and there is some benefit to caching the inverse of a matrix rather than computing it repeatedly

# makeCacheMatrix creates a list containing a function to
# a. Set & Get the value of the matrix
# b. Set & Get the value of inverse of the matrix

makeCacheMatrix  <- function(x = matrix())
{
        inv <- NULL
        set <- function(y)
        {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse  <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse=setinverse,
             getinverse=getinverse)
}


#The following function returns the inverse of the matrix. 
# a. It first checks if the inverse of the matrix has already been computed.
# b. If so, it gets the result and skips the computation
# c. If not, it computes the inverse of the matrix and sets the value in the cache via setinverse function.

cacheSolve  <- function(x, ...) 
{
        inv <- x$getinverse()
        if(!is.null(inv)) # Check if the inverse of the matrix is already calculated and is stored in cache
        {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


# Sample input


# for a 2 X 2 Matrix

x<- matrix(data=1:4, nrow=2, ncol=2)

inv=makeCacheMatrix(x)
inv$get()
# upon running cacheSolve(inv) from the second time onwards, the result is fetched from cache
cacheSolve(inv)

# for a 3X3 Matrix

x<-matrix(data=c(3,2,0,0,0,1,2,-2,1), nrow=3, ncol=3)

inv=makeCacheMatrix(x)
inv$get()
# upon running cacheSolve(inv) from the second time onwards, the result is fetched from cache
cacheSolve(inv)

