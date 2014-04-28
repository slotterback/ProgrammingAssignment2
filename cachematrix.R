## The following are a set of wrapper functions for a matrix that allow us to 
## calculate the inverse of a matrix and store that inverse in memory for 
## future use.
## NOTE: The code for this assignment has been modified by Steve
##       Slotterback from example code provided in the description of this 
##       assignment* on 27 April 2014
## * url:https://class.coursera.org/rprog-002/human_grading/view/courses/ ...
##         972078/assessments/3/submissions

## Function: makeCacheMatrix
## Description: takes in a numeric matrix and creates an construct consisting of
##              functions to store and set the matrix and its inverse
## Inputs: x: a matrix to be inverted
## Outputs: A list of functions to get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	#create m to store the cached matrix inverse
	m <- NULL
	# set: sets x to y, deletes any prior cached inverse matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
	# get: returns the original matrix x
    get <- function() x
	# setsolve: sets the value of the cached inverse m
    setsolve <- function(solve) m <<- solve
	# getsolve: returns the cached inverse matrix
    getsolve <- function() m
	# return the functions used to get and set the cached matrix and 
	# inverse.
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Function: cacheSolve
## Description: Takes a construct, specifically the type created by the
##              function "makeCacheMatrix", and returns the inverse of the
##              matrix that was originally passed to "makeCacheMatrix".  If
##              the inverse has not yet been set, it will calculate the inverse
##              and store it before returning the inverse.
## Inputs: The output of makeCacheMatrix for the matrix you want the inverse 
##         of.  Note that this function does not accept an ordinary matrix.
## Outputs: The inverse of the matrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {# checks to see if the inverse has already been found
                message("getting cached data")
                return(m)
        }
		# only calculates the inverse of the matrix if it hasn't yet done so
        data <- x$get() #get the original matrix
        m <- solve(data, ...) #calculate the inverse of the matrix
        x$setsolve(m) #store the inverse for future reference
        m #return the inverse
}
