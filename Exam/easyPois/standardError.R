#' Calculate the standard error in \code{PoisMLE} object
#'
#' For internal use only. This function calculates the standard error for the 
#' maximum likelihood estimator in the \code{PoisMLE} class using one of two methods
#' as given by the user. See 'Arguments'. The computed value is ultimately returned when 
#' initializing an object of class \code{PoisMLE}.
#'
#' @param y A vector of positive integer observations.
#' @param SEtype A character string of \code{"basic"} or \code{"bootstrap"}; see 'Details'.
#' @param B An integer; the default is set to \code{B=10}. This argument is not used
#' if \code{SEtype="basic"}.
#' 
#' @details 
#' The user specifies which method to use for this calculation when initializing a \code{PoisMLE} object with the 
#' \code{estimatePois} function. If \code{"basic"}, the calculation is performed according to
#' \deqn{\sqrt(\frac{MLE}{n})}. If \code{"bootstrap"}, the SE is the standard deviation of \code{B} bootstrapped 
#' resamplings of \code{y}.
#' 
#' @return The standard error of the MLE object in class \code{PoisMLE}
#'  \item{SE}{The standard error}
#' @author Alma Velazquez
#' @examples
#' \donttest{y_vec <- c(1,2,3,4)
#' standardError(y_vec, "bootstrap", B=10)}
#' @seealso mle, PoisMLE-class
#' @rdname standardError
#' @aliases standardError
#' 
#' @keywords internal
standardError <- function(y, SEtype, B=10){
  n <- length(y)
  if(SEtype=="basic"){
    se_result <- sqrt(mle(y)/n)
  }
  else if(SEtype=="bootstrap"){
    
    for(i in 1:B){
      sample_col <- sample(y, n, replace = TRUE)
      all_samples[,i] <- sample_col
    }
    
    
    sample_mles <- apply(all_samples, 2, mle)
    
    se_result <- sd(sample_mles)
  }
  
  return(se_result)
}
