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

#' @export
setMethod("initialize", "Simpson", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#' @rdname Simpson

