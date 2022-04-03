#' Integrating values
#'
#' Integrates function over interval using numerical approximation methods.
#'
#' @param fx A function defined over \code{[a,b]}.
#' @param n The number of desired subintervals over \code{[a,b]} used to define x values.
#' @param interval A vector containing the lower and upper bounds of integration \code{[a,b]=c(a,b)}.
#' @param rule \code{'Simpson'} or \code{'Trapezoid'} method of numerical integration.
#'
#' @return An object of class \code{'Simpson'} or \code{'Trapezoid'} containing
#'  \item{result}{The approximated numerical integral}
#'  \item{x}{A subset of the original vector of inputs over \code{[a,b]}} 
#'  \item{y}{The evaluated function values over \code{[a,b]}}
#' @author Alma Velazquez
#' @note Approximation accuracy increases with size of n.
#' @examples
#' 
#' test_fx <- function(x){x^2}
#' integrateIt(text_fx, 2000, c(1,3), rule="Trapezoid")
#' @seealso tolTest, Trapezoid-class, Simpson-class
#' @rdname integrateIt
#' @aliases integrateIt,ANY-method
#' 
#' @export
setGeneric(name="integrateIt",
           def=function(fx, n, interval, rule)
           {standardGeneric("integrateIt")}
)
#' @export
setMethod(f="integrateIt",
          definition=function(fx, n, interval, rule){
            # Extract lower and upper bounds from interval.
            a <- interval[1]
            b <- interval[2]
            
            # Create a sequence of x values using the bounds of integration
            # and desired n.
            x <- seq(from=a, to=b, by=(b-a)/n)
            
            # Select non-endpoint x values.
            x_vals <- x[a < x & x < b]
            
            # Evaluate x values using user-supplied function.
            y_vals <- unlist(lapply(x_vals, fx))
            
            # Evaluate the upper and lower bounds of integration separately.
            f_a <- unlist(lapply(a, fx))
            f_b <- unlist(lapply(b, fx))
            
            # Define h
            h <- (b-a)/n
            
            if(rule=="Trapezoid"){
              # Apply Trapezoidal Rule formula.
              result <- (h/2) * (sum(2*y_vals) + f_a + f_b)
              
            }
            
            else if(rule=="Simpson"){
              # Apply Simpson's Rule formula.
              result <- (h/3) * (sum(4*y_vals[c(TRUE,FALSE)]) + sum(2*y_vals[c(FALSE,TRUE)]) + f_a + f_b)
              
            }
            # Otherwise, raise an error.
            else{error("Rule is invalid")}
            
            # Return an object whose class matches the rule argument given.
            return(new(rule, 
                       result = result, 
                       x = c(a, x_vals, b), 
                       y = c(f_a, y_vals, f_b)))
          }
)
