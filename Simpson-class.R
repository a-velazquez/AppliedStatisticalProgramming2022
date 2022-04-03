#' An integrated value object using Simpson's Rule
#' 
#' Objects of class \code{Simpson} are created by the \code{IntegrateIt} function
#'
#' 
#' An object of the class `Simpson' has the following slots:
#' \itemize{
#' \item \code{result} The numerical approximation of an integral F(x) over some interval (a,b) according to Simpson's Rule 
#' \item \code{x} A vector of partition values over the interval [a,b]
#' \item \code{y} The function f evaluated over [a,b] 
#' }
#'
#' @author Alma Velazquez: \email{a.m.velazquez@@wustl.edu}
#' @aliases Simpson-class initialize,IntegrateIt-method 
#' @rdname Simpson
#' @export
setClass(Class="Simpson", 
         representation = representation(
           result = "numeric",
           x = "numeric",
           y = "numeric"
         ),
         prototype = prototype(
           result = 0,
           x = 0,
           y = 0
         )
)

setValidity("Simpson", function(object){
  
  test1 <- any(is.infinite(object@x))
  test2 <- any(is.infinite(object@y))
  
  test3 <- any(is.na(object@x))
  test4 <- any(is.na(object@y))
  
  a <- object@x[1]
  b <- object@x[length(object@x)]
  
  f_a <- object@y[1]
  f_b <- object@y[length(object@y)]
  
  n <- length(object@x)
  
  x_vals <- object@x[a < object@x & object@x < b]
  y_vals <- object@y[f_a < object@y & object@y < f_b]
  
  h <- (b-a)/n
  
  correct_result <- (h/3) * (sum(4*y_vals[c(TRUE,FALSE)]) + sum(2*y_vals[c(FALSE,TRUE)]) + f_a + f_b)
  
  test5 <- correct_result == object@result
  
  if(any(test1,test2,test3,test4,test5)){return("@result is not a valid value")}
}
)

#' @export
setMethod("initialize", "Simpson", 
          function(.Object, ...){
            value = callNextMethod()
            validObject(value)
            return(value)
          }
) 

#' @rdname Simpson

#' @export
setMethod(f = "print",
          signature(x = "Simpson"),
          definition = function(x){
            print(x@result)
          })


