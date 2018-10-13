## The following functions cache the inverse of a given matrix

##Created a matrix object that inverses the matrix 
 makeCacheMatrix <- function( mat = matrix() ) {

	    i <- NULL
 ## Using the set method
    set <- function( matrix ) {
            mat <<- matrix
            i <<- NULL
    }
 ## get method
    get <- function() {
        	mat
    }
##set the inverse of the matrix
    setInv <- function(inverse) {
        i <<- inverse
    }

##Get the inverse of the matrix
    getInv <- function() {
              i
    }

    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Compute invere of te matrix returned by the above function
##If inverse is found before, function finds its calculated inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       mat <- x$getInv()
      #previously cached inverse
       if( !is.null(mat) ) {
               message("getting cached data")
               return(mat)
    }
    inv <- x$get()

        mat <- solve(inv) %*% inv
       x$setInv(mat)

    return(mat)
}

