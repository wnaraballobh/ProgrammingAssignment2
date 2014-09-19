## makeCacheMatrix function will generate matrix from input process 
## then calculate invertable possibility by check 1) n*n matrix 
## 2) non-singular matrix. Then stroage result in list format including 
## generate_matrix, main_matrix, set_invert and invert_value 
## within same environment. 

makeCacheMatrix <- function(a,nR,nC){
cache_value <<- NULL
temp_matrix <- matrix(a,nR,nC)
  ## test n*n matrix  
    if(nrow(temp_matrix) == ncol(temp_matrix)){
  ## test non-singular matrix
        if(det(temp_matrix) != 0){
  ## make function to assign value in case of return environment display
  ## mention on set_invert will use super-assignmen on cache_value        
          main_matrix  <- function() temp_matrix
          set_invert <- function(x) cache_value <<- x   
          invert_value <- function() cache_value
  ## result in list format           
          list(generate_matrix = temp_matrix, 
          main_matrix = main_matrix, 
          set_invert = set_invert,
          invert_value = invert_value)       
        }else{
          print("Input Value Must Not Be Singular Matrix (det(A) != 0)")
          temp_matrix <- NULL
        }                    
    }else{
      print("Input Value Must Be n*n Matrix")
      temp_matrix <- NULL
    }
}

## cacheSolve function will calculate invertible matrix and returns its inverse
## if invert_cache is NULL. If not, use previous invert_cache instead  

cacheSolve <- function(y, ...) {
  ## retrive invert_value from previous list result 
invert_cache <- y$invert_value()
  ## test status of invert_cache data       
        if(is.null(invert_cache)){       
          message("Run cacheSolve 1st Time")                
  ## get matrix data
          invert_cache <- y$main_matrix()        
  ## calulate invert matrix
          invert_cache <- solve(invert_cache)        
  ## assign new value to invert_cache
          y$set_invert(invert_cache)      
          invert_cache
        }
        else{   
          message("Getting Invert Matrix Cached Data")
          return(invert_cache)
        }
}

## Test function and Result Session ##

source("cachematrix.R")
x <- makeCacheMatrix(5:8, 2, 2)
#
x
#$generate_matrix
#     [,1] [,2]
#[1,]    5    7
#[2,]    6    8
#
#$main_matrix
#function () 
#temp_matrix
#<environment: 0x0000000005d2a188>
#
#$set_invert
#function (x) 
#cache_value <<- x
#<environment: 0x0000000005d2a188>
#
#$invert_value
#function () 
#cache_value
#<environment: 0x0000000005d2a188>
#
y <- makeCacheMatrix(rnorm(10), 4, 2)
#1] "Input Value Must Be n*n Matrix"
#
y
#NULL
#
z <- makeCacheMatrix(10, 2, 2)
#[1] "Input Value Must Not Be Singular Matrix (det(A) != 0)"
#
z
#NULL
#
cacheSolve(x)
#     [,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5
# (comment display) Run cacheSolve 1st Time
#
cacheSolve(x)
#     [,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5
# (comment display) Getting Invert Matrix Cached Data
