## As requested by the original assignment, these functions calculate and then cache the inverse of a matrix.

## makeCacheMatrix: This function creates a list of four functions. They are
## 1. a function that sets two variables for a different environment
## 2. a function that returns the original matrix
## 3. a function that moves a local variable to a different environment
## 4. a function that returns the value stored in step 3.

makeCacheMatrix <- function(originalMatrix = matrix()) {
            # set the local variable "theInverse" to Null
            theInverse <- NULL
            # create a function 'set' that takes a single parameter and places it into the variable originalMatrix, not just in the function, but in the broader environment
            # it also sets the value of theInverse to NULL too.
            set <- function(y) {
                    originalMatrix <<- y
                    theInverse <<- NULL
            }
            # create a function 'get' that returns the original matrix
            get <- function() originalMatrix
            # create a function 'setInverse' that takes in a value and stores it to the variable theInverse
            setinverse <- function(inverseValue) theInverse <<- inverseValue
            # create a function 'getInverse' that returns theInverse 
            getinverse <- function() theInverse
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

## after makeCacheMatrix has been run, there's a new list of four functions & theInverse is NULL
}



## This function computes the inverse of the matrix used by make returned by `makeCacheMatrix` above.
## It takes in the list of functions and uses it to find the stored inverse (assuming that it has already been calculated and stored)
## or to compute and then store the inverse (in "theInverse") of the matrix if it has not been calculated or stored.

cacheSolve <- function(matrixCacheFunctions, ...) {
            # find the value stored in theInverse - assign it to 'checkInverse'
            checkInverse <- matrixCacheFunctions$getinverse()
            
            # if cacheSolve has been run before, theInverse will not be NULL.
            # send a message to the user and return the inverse matrix. End of Function
            if(!is.null(checkInverse)) {
                    message("getting cached data")
                    return(checkInverse)
            }
            # if cacheSolve has not been run before or someone has deleted the stored inverse then:
            # get the original matrix
            originalMatrix <- matrixCacheFunctions$get()
            # find the inverse of the original matrix, assign it to 'calculatedInverse'
            calculatedInverse <- solve(originalMatrix, ...)
            # also, assign it to "theInverse" for future use
            matrixCacheFunctions$setinverse(calculatedInverse)
            # finally, return the inverse matrix.
            calculatedInverse
}

