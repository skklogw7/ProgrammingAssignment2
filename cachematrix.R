## These functions make it more efficient to
## calculate the inverse of a matrix by creating a cached matrix
## that can be called instead of having to re-calculate an inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #Set the 'inv' variable to NULL so 
        #function doesn't hang on first time through
        set <- function(y) {  # y is numeric arg passed
                # into makeCacheMatrix 
                x <<- y       # Set 'x' for the function environment to 'y'
                inv <<- NULL  # Set 'inv' for the 'makeCacheMatrix' 
                # environment to NULL
        }
        get <- function() x   # Create a function 'get' in the 'makeCacheMatrix'
        # parent and assigns a matrix to it.  Note that
        # this makes little sense without the context
        # of the CacheSolve function.
        setinverse <- function(inverse) inv <<- inverse #Takes value 'inverse'
        #and sets it to the
        #value of 'inv' in the 
        #'makeCacheMatrix' frame
        getinverse <- function() inv # returns the value of 'inv' from the
        # 'makeCacheMatrix' frame. Needs context of 
        # the 'CacheSolve' function to make sense.
        list(set=set, get=get,       # Lists out the values of the functions in 
             # the 'makeCacheMatrix' frame.
             setinverse=setinverse, 
             getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()   # Goes to the'x' environment and assigns the 
        # 'inv' value from that environment to this one
        if(!is.null(inv)) {     # # If the 'x' environment has been evaluated 
                # before, the function prints the message and
                # the value of inv (the cached inverted matrix).
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()         # If this particular 'x' has never been
        # evaluted before, pull the x-matrix into a
        # local varaible called 'data'.
        inv <- solve(data)      # Calculate the inverse value of the matrix x 
        # by calling 'solve' function on the local
        # 'data' variable.
        
        x$setinverse(inv)       # Assign the calculated matrix to the 'x'
        # environment using the 'setinverse' function.
        inv                     # # Display the calculated inverted matrix.
}