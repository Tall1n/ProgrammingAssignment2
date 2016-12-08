#### Peer-graded Assignment: Programming Assignment 2: Lexical Scoping #####
        ### Idea: cache potentially time-consuming computations ###
            ## Assignment: Caching the Inverse of a Matrix ##


#
#               1. Creating a random square testmatrix and check the inverse to see what the final result should look like
#
testmatrix <- matrix(rnorm(9),3,3)
testmatrix
solve(testmatrix)


#
#               2. Writing the makeCacheMatrix function 
#
#       The function will creates a special "matrix" object that can cache its inverse. 
#       which is actually a list containing a function to 
#       set the values of the matrix
#       get the values of the matrix
#       set the values of the inverse
#       get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#
#               3. Creating the special "matrix" of our testmatrix
#

testmatrix_c <- makeCacheMatrix(testmatrix)
# to see if it worked correctly we can check which object the get() function returned in our special "matrix"
# and compare it with the matrix it was suppose to use
testmatrix_c$get()
testmatrix


#
#               4. Writing the cacheSolve function.
#
#     This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#     If the inverse has already been calculated (and the matrix has not changed), it
#     will retrieve the inverse from the cache instead of calculating it again.
# 

cacheSolve <- function(x, ...) {
        
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
        
}


#
#               5. Testing if the cacheSolve Function used with our special "matrix" will return the desired results
#
cacheSolve(testmatrix_c)
solve(testmatrix)
cacheSolve(testmatrix_c)
                  # it returns the correct inverse, and if applied again after the inverse was already calculated, 
                  # it will return the cached data





############## Additional Information ####################

#### here you can test matricies of diffrent sizes to see how fast it gets really time consuming to calculate the inverse ###
#### with a diag_length of 1000 will create a matrix of size 7,6 Mb and it takes a few seconds to calculate the inverse
#### but if you increase the diag_length to 3000 the matrix will have 67 Mb already and the calculation of the inverse
#### will take a few minutes. 
diag_length <- 1000
matx <- matrix(rnorm(diag_length*diag_length),diag_length,diag_length)
inverse_matx <- solve(matx)

matx_c <- makeCacheMatrix(matx)
cache_solved_inv <- cacheSolve(matx_c)
cache_solved_inv <- cacheSolve(matx_c)



