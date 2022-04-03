#' Improve integration accuracy
#'
#' Finds appropriate n for integration accuracy.
#'
#' @param fx A function defined over [a,b].
#' @param start_n The number of desired subintervals over [a,b] used to define x values.
#' @param tol The tolerated absolute margin of error.
#' @param interval A vector containing the lower and upper bounds of integration [a,b].
#' @param rule \code{'Simpson'} or \code{'Trapezoid'} method of numerical integration.
#'
#' @return A list object containing all inputs and the final n and associated absolute error
#'  \item{function_input}{The original function}
#'  \item{start_n}{The user-defined tolerated absolute margin of error} 
#'  \item{desired_tol}{The user-defined function values over [a,b]}
#'  \item{integration_method}{The numerical integration method used}
#'  \item{interval}{The original interval [a,b]}
#'  \item{final_n}{Final numer of subintervals that yields a value within the tolerated error margin}
#'  \item{absolute_error}{The final absolute error of the estimare}
#'  
#' @author Alma Velazquez
#' @examples
#' 
#' test_fx <- function(x){x^2}
#' tolTest(text_fx, 20, tol=0.001, c(1,3), rule="Trapezoid")
#' 
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
            
            a <- interval[1]
            
            b <- interval[2]
            
            correct <- integrate(fx, lower=a, upper=b)$value
            
            test_n <- start_n
            stopper <- FALSE
            
            while(!stopper){
              guess <- print(integrateIt(fx, test_n, interval, rule))
              abs_err <- abs(correct-guess)
              if(abs_err<=tol){
                stopper <- TRUE
              } 
              else {
                test_n <- test_n + 1
              }
            }
            
            return(list(function_input = fx,
                        start_n = start_n,
                        desired_tol = tol,
                        integration_method = rule,
                        interval = interval,
                        final_n = test_n, 
                        absolute_error = abs_err))
            
          }
)
