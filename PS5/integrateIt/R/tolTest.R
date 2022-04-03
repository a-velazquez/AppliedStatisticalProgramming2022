#' Improve integration accuracy
#'
#' Finds appropriate n for integration accuracy.
#'
#' @param fx A function defined over \code{[a,b]}.
#' @param start_n The number of desired subintervals over \code{[a,b]} used to define x values.
#' @param tol The tolerated absolute margin of error.
#' @param interval A vector containing the lower and upper bounds of integration \code{[a,b]=c(a,b)}.
#' @param rule \code{'Simpson'} or \code{'Trapezoid'} method of numerical integration.
#'
#' @return A list object containing all inputs and the final n and associated absolute error
#'  \item{function_input}{The original function}
#'  \item{start_n}{The user-defined tolerated absolute margin of error} 
#'  \item{desired_tol}{The user-defined function values over \code{[a,b]}}
#'  \item{integration_method}{The numerical integration method used}
#'  \item{interval}{The original interval \code{[a,b]}}
#'  \item{final_n}{Final numer of subintervals that yields a value within the tolerated error margin}
#'  \item{absolute_error}{The final absolute error of the estimare}
#'  
#' @author Alma Velazquez
#' @examples
#' 
#' test_fx <- function(x){x^2}
#' tolTest(text_fx, 20, tol=0.001, c(1,3), rule="Trapezoid")
#' @seealso integrateIt, Trapezoid-class, Simpson-class
#' @rdname tolTest
#' @aliases tolTest,ANY-method
#' 
#' @export
setGeneric(name="tolTest",
           def=function(fx, start_n, tol, interval, rule)
           {standardGeneric("tolTest")}
)
#' @export
setMethod(f="tolTest",
          definition=function(fx, start_n, tol, interval, rule){
            
            # Extract lower and upper bounds from interval.
            a <- interval[1]
            b <- interval[2]
            
            # Define the correct integral to match.
            correct <- integrate(fx, lower=a, upper=b)$value
            
            # Set an initial value of n to test, given by user.
            test_n <- start_n
            # Initialize a stopping condition
            stopper <- FALSE
            
            # While the stopping condition is false...
            while(!stopper){
              # Make a guess,
              guess <- integrateIt(fx, test_n, interval, rule)@result
              
              # record the absolute error of this guess,
              abs_err <- abs(correct-guess)
              
              # if this guess is within desired margin, engage stopping condition.
              if(abs_err<=tol){
                stopper <- TRUE
              } 
              else {
                # Otherwise, increment n.
                test_n <- test_n + 1
              }
            }
            
            # Return inputs and the last n and error recorded.
            return(list(function_input = fx,
                        start_n = start_n,
                        desired_tol = tol,
                        integration_method = rule,
                        interval = interval,
                        final_n = test_n, 
                        absolute_error = abs_err))
            
          }
)
