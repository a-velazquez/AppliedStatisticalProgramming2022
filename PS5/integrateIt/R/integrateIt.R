#' Integrating values
#'
#' Integrates function over interval using numerical approximation methods.
#'
#' @param fx A function defined over [a,b].
#' @param n The number of desired subintervals over [a,b] used to define x values.
#' @param interval The lower and upper bounds of the interval [a,b].
#' @param rule \code{'Simpson'} or \code{'Trapezoid'} method of numerical integration.
#'
#' @return An object of class \code{'Simpson'} or \code{'Trapezoid'} containing
#'  \item{result}{The approximated numerical integral}
#'  \item{x}{A subset of the original vector of inputs over [a,b]} 
#'  \item{y}{The evaluated function values over [a,b]}
#' @author Alma Velazquez
#' @note Approximation accuracy increases with size of n.
#' @examples
#' 
#' test_fx <- function(x){x^2}
#' integrateIt(text_fx, 2000, c(1,3), rule="Trapezoid")
#' 
#' @rdname integrateIt
#' @aliases IntegrateIt,ANY-method
#' 
#' @export
setGeneric(name="integrateIt",
           def=function(fx, n, interval, rule)
           {standardGeneric("integrateIt")}
)
#' @export
setMethod(f="integrateIt",
          definition=function(fx, n, interval, rule){
            
            a <- interval[1]
            
            b <- interval[2]
            
            x <- seq(from=a, to=b, by=(b-a)/n)
            
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
