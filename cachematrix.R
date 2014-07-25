## cachematrix.R
##
## Coursera R Programming Course
## Programming Assignment 2: Lexical Scoping
##
## Last Updated: 2014-07-25
## By: Jason Bailey
## 
## This file contains a pair of functions that cache the inverse of a matrix. 
## The purpose of this exercise is to demonstrate how to save time and costly 
## computing resources by caching a value that may be needed again in the future.
## 
## For simplicity, we assume the matrix passed is an invertable square matrix.
##


makeCacheMatrix <- function(m = matrix()) {
    ## Creates a special matrix ojbect that can cache its inverse.
    ##
    ## Args: 
    ##   m: Default is empty matrix, or matrix passed from caller
    ##
    ## Returns: 
    ##   instance of makeCacheMatrix object (list of functions)
    ##
    ## Methods:
    ##  setMatrix, getMatrix, setMatrixInverse, getMatrixInverse
    
    
    mInverse <- NULL
    
    # set method to store new matrix value and reset matrixInverse value
    setMatrix <- function(newM) {
        m <<- newM
        mInverse <<- NULL
    }
    
    getMatrix <- function() m
    
    setMatrixInverse <- function(newInverse) mInverse <<- newInverse
    
    getMatrixInverse <- function() mInverse
    
    # return instance of makeCacheMatrix object
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}


cacheSolve <- function(x, ...) {
    ## Computes and returns the inverse of the special matrix returned by 
    ## makeCacheMatrix.  If the inverse already exists, and the matrix hasn't 
    ## changed, retrieves the inverse from cache.
    ##
    ## Args:
    ##   x: instance of makeCacheMatrix (list)
    ##
    ## Returns:
    ##   mInverse: inverse of matrix "m" (matrix)
    
    # check if matrix inverse value is cached and has not changed
    mInverse <- x$getMatrixInverse()
    
    # If cached, return value
    if ( !is.null(mInverse) ) {
        message("Getting matrix inverse from cache.")
        return(mInverse)
    }
    
    # If not cached, compute and cache the matrix inverse
    m <- x$getMatrix()
    mInverse <- solve(m)
    x$setMatrixInverse(mInverse)
    mInverse
}
