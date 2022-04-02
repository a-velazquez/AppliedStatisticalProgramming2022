#' An integrated value object using the Trapezoidal Rule
#' 
#' Objects of class \code{Trapezoid} are created by the \code{IntegrateIt} function
#'
#' 
#' An object of the class `Trapezoid' has the following slots:
#' \itemize{
#' \item \code{result} The numerical approximation of an integral F(x) over some interval (a,b) according to the Trapezoidal Rule 
#' \item \code{x} A vector of partition values over the interval [a,b]
#' \item \code{y} The function f evaluated over [a,b] 
#' }
#'
#' @author Alma Velazquez: \email{a.m.velazquez@@wustl.edu}
#' @aliases Trapezoid-class initialize,IntegrateIt-method 
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

#' @export
setMethod("initialize", "Trapezoid", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#' @rdname Trapezoid


#' @export
setMethod(f = "print",
          signature(x = "Trapezoid"),
          definition = function(x){
            print(x@result)
          })

