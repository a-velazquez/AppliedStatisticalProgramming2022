#' Integrating values
#'
#' Integrates function over interval using numerical approximation methods.
#'
#' @param x A vector of partition values containing the interval [a,b].
#' @param fx A function defined over all values of \code{x}.
#' @param interval The lower and upper bounds of the interval [a,b].
#' @param rule \code{'Simpson'} or \code{'Trapezoid'} method of numerical integration.
#'
#' @return An object of class \code{'Simpson'} or \code{'Trapezoid'} containing
#'  \item{result}{The approximated numerical integral}
#'  \item{x}{A subset of the original vector of inputs over [a,b]} 
#'  \item{y}{The evaluated function values over [a,b]}
#' @author Alma Velazquez
#' @note Approximation accuracy increases with length of interval.
#' @examples
#' 
#' text_x <- seq(from = 1, to = 3, by = 0.001)
#' test_fx <- function(x){x^2}
#' IntegrateIt(text_x, text_fx, c(1,3), rule="Trapezoid")
#' 
#' @rdname integrateIt
#' @aliases IntegrateIt,ANY-method
#' 
#' @export
setGeneric(name="integrateIt",
           def=function(x, fx, interval, rule)
           {standardGeneric("integrateIt")}
)
#' @export
setMethod(f="integrateIt",
          definition=function(x, fx, interval, rule){
            
            a <- interval[1]
            
            b <- interval[2]
            
            n <- length(x[a <= x & x <= b])
            
            x_vals <- x[a < x & x < b]
            y_vals <- unlist(lapply(x_vals, fx))
            
            f_a <- unlist(lapply(a, fx))
            f_b <- unlist(lapply(b, fx))
            
            h <- (b-a)/n
            
            if(rule=="Trapezoid"){
              
              result <- (h/2) * (sum(2*y_vals) + f_a + f_b)
              
            }
            
            else if(rule=="Simpson"){
              
              result <- (h/3) * (sum(4*y_vals[c(TRUE,FALSE)]) + sum(2*y_vals[c(FALSE,TRUE)]) + f_a + f_b)
              
            }
            
            else{error("Rule is invalid")}
            
            return(new(rule, 
                       result = result, 
                       x = c(a, x_vals, b), 
                       y = c(f_a, y_vals, f_b)))
          }
)
