#' Calculate MLE estimator for \eqn{\lambda} in \code{PoisMLE} object
#'
#' For internal use only. This function calculates the maximum likelihood estimator for 
#' the \eqn{\lambda} parameter in a Poisson distribution, such that 
#' \deqn{\Pr(Y_i =y) = \frac{\lambda^{y_i} exp^{-\lambda}}{y_i!}}.
#' The estimator is then used and ultimately returned when initializing an object of class \code{PoisMLE}.
#'
#' @param y A vector of positive integer observations.
#'
#' @return A maximum likelihood estimator for consumption of objects of class \code{PoisMLE}
#'  \item{MLE}{The maximum likelihood estimator for \eqn{\lambda}}
#' @author Alma Velazquez
#' @examples
#' \dontrun{y_vec <- c(1,2,3,4)
#' mle(y_vec)}
#' @seealso standardError, logLik, PoisMLE-class
#' @rdname mle
#' @aliases mle
#' 
#' @keywords internal
mle <- function(y){
  n <- length(y)
  # Apply formula for MLE of lambda parameter.
  mle_result <- sum(y, na.rm = TRUE) / n
  return(mle_result)
}
