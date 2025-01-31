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
#' \dontrun{y_vec <- c(1,2,3,4)
#' standardError(y_vec, "bootstrap", B=10)}
#' @seealso mle, PoisMLE-class
#' @rdname standardError
#' @aliases standardError
#' 
#' @keywords internal
standardError <- function(y, SEtype, B=10){ # B has a default of 10
  n <- length(y)
  # When SEtype is basic...
  if(SEtype=="basic"){
    # Define the result as this.
    se_result <- sqrt(mle(y)/n)
  }
  # Otherwise, 
  else if(SEtype=="bootstrap"){
    # For the indices from 1 to B, randomly sample n
    # from observed data. Store these in a matrix with B columns and n rows.
    all_samples <- matrix(unlist(lapply(c(1:B), 
                                        function(x){sample(y, n, replace = TRUE)})), 
                          ncol = B, nrow = n,
                          byrow = FALSE)
    
    
    # Apply mle() to each column of the matrix..
    sample_mles <- apply(all_samples, 2, mle)
    # Take the standard deviation of the sample,
    # save to result.
    se_result <- stats::sd(sample_mles)
  }
  
  return(se_result)
}
