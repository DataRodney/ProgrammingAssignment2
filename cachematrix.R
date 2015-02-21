
## Part 1:  makeCacheMatrix: This function creates a special matrix that
## can calculated the inverse matrix using 'solve'.  When contents of the
## matrix are not changing, the value is called from a different environment
## (i.e. cached) instead of recomputing each time the inverse is needed.

makeCacheMatrix <- function(x = matrix()) {      ## create an empty matrix object

  m <- NULL						## clear any matrix content that may exist within m

  set <- function(y){			## save an object we may insert into the matrix
    x <<- y				## x is assigned y from a different or cached environment
    m <<- NULL			## Re-set 'm', ensuring an updated matrix argument is processed
  }

  get <- function() x					## get function returns the matrix stored in x

  setInverse <- function(solve) m <<- solve		## setInverse overrides the previous value of 'm'

  getInverse <- function() m				# getInverse returns the Inverse

  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  ##return the four functions as a list

}

# Part 2: cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  I <- x$getInverse()    				## Retrieve the most recent value of  getInverse

  if(!is.null(I)){					## if the Inverse value is not null, it was previously 							## calculated and cacheSolve returns that value
    message("getting cached data")			## send a message to the console
    return(I)						##  return the inverse value 'I'

  }

  ## Else the value of Inverse is determined to be NULL, and matrix x is retrieved and the inverse is calculated with the solve() function

  message("newly calculating data")			# tell the console the function is 'calculating'

  data <- x$get()					#data gets assigned the matrix content
  I <- solve(data, ...)					# calculate the matrix inversion
  x$setInverse(I)					# Sets Inverse to the newly calculated value
  I 							#Return the new Inverse matrix value

}


## Test correctly using multiple tests from forum ...405

## credits and references to:

## https://github.com/jtleek/modules/blob/master/ALL%20UNUSED%20CONTENT/slidifydev_roger/classes-methods/cachemean.R
## http://www.ats.ucla.edu/stat/r/modules/subsetting.htm
## http://ncalculators.com/matrix/2x2-inverse-matrix-calculator.htm
## http://www.endmemo.com/program/R/solve.php
## http://cran.r-project.org/doc/manuals/r-release/fullrefman.pdf
## https://class.coursera.org/rprog-011/forum/thread?thread_id=405
## https://class.coursera.org/rprog-011/forum/thread?thread_id=429
## https://class.coursera.org/rprog-011/forum/thread?thread_id=105
## https://class.coursera.org/rprog-011/forum/thread?thread_id=538
## https://class.coursera.org/rprog-011/forum/thread?thread_id=186
