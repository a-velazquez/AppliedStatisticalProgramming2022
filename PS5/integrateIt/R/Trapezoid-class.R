#' An integrated value object using the Trapezoidal Rule
#' 
#' Objects of class \code{Trapezoid} are created by the \code{integrateIt} function
#'
#' 
#' An object of the class `Trapezoid' has the following slots:
#' \itemize{
#' \item \code{result} The numerical approximation of an integral F(x) over some interval \code{[a,b]} according to the Trapezoidal Rule 
#' \item \code{x} A vector of partition values over the interval \code{[a,b]}
#' \item \code{y} The function f evaluated over \code{[a,b]}
#' }
#'
#' @author Alma Velazquez: \email{a.m.velazquez@@wustl.edu}
#' @aliases Trapezoid-class initialize,integrateIt-method 
#' @rdname Trapezoid
#' @export
setClass(Class="Trapezoid", 
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

setValidity("Trapezoid", function(object){
  # First 4 tests make sure all x and y values are defined.
  test1 <- any(is.infinite(object@x))
  test2 <- any(is.infinite(object@y))
  test3 <- any(is.na(object@x))
  test4 <- any(is.na(object@y))
  
  # The next steps recreate integrateIt to check that the result is valid.
  
  # Extract upper and lower bounds from x values.
  a <- object@x[1]
  b <- object@x[length(object@x)]
  
  # Extract the evaluated values of a and b from y values.
  f_a <- object@y[1]
  f_b <- object@y[length(object@y)]
  
  # Extract n
  n <- length(object@x)
  
  # Define the non-endpoint values to integrate over.
  x_vals <- object@x[a < object@x & object@x < b]
  y_vals <- object@y[f_a < object@y & object@y < f_b]
  
  # Define h
  h <- (b-a)/n
  
  # Define what the integral should be given x and y values.
  correct_result <- (h/2) * (sum(2*y_vals) + f_a + f_b)
  
  # The final test makes sure the result in the object matches the correct result.
  test5 <- correct_result == object@result
  
  # An error message is returned if the object doesn't pass these tests.
  if(any(test1,test2,test3,test4,test5)){return("@result is not a valid value")}
}
)


#' @export
setMethod("initialize", "Trapezoid", 
          function(.Object, ...){
            value = callNextMethod()
            # Call validator
            validObject(value)
            return(value)
          }
) 

#' @rdname Trapezoid


#' @export
setMethod(f = "print",
          # Define signature and definition in terms of x;
          # this is what the print generic expects
          signature(x = "Trapezoid"),
          definition = function(x){
            print(x@result)
          })

