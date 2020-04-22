#' Euclidean Algorithm
#'
#' Euclidean algorithm finds the greatest common divisor (gcd) between scalars \code{a} and \code{b}.
#' 
#' The algorithm works by following two rules. 
#' 
#' (1) If \code{b=0}, then \code{gcd(a,b)=a} and the algorithm ends.
#' 
#' (2) Elsewhere (\code{b!=0}), then \code{gcd(a,b)=gcd(b,r)} where \code{r} is the reminder of dividing \code{a} by \code{b}. \code{gcd(a,b)=gcd(b,r)} will be calculated until the remainder equals 0. 
#'
#' @param a numeric scalar object.
#' @param b numeric scalar object.
#'
#' @return \code{euclidean} returns a scalar value of the greatest common divisor.
#' 
#' If the arguments of the function are not numeric scalars or integers an error will occur.
#'
#' @examples
#' euclidean(123612, 13892347912)  # = 4
#' euclidean(10, 100)              # = 10
#' 
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @importFrom utils tail
#'
#' @export
#'

euclidean <- function(a,b){
  stopifnot(is.numeric(a), is.numeric(b)) # Ensure that the arguments are numeric scalars
  r = vector()    # reminders vector
  r[1] = a %% b   # first reminder value
  k = 2           # index
  
  # while loop
  while(tail(r, 1) != 0){ # while the last reminder isn't equal to 0
    a = b
    b = r[k-1]
    r[k] = a %% b
    k = k + 1
  }
  return(b)  # return last divisor 
}