#' A fitted MLE Poisson model
#' 
#' Objects of class \code{PoisMLE} are created by the \code{estimatePois} function
#'
#' 
#' An object of the class `PoisMLE' has the following slots:
#' \itemize{
#' \item \code{y} Original vector of non-negative integer observations.
#' \item \code{MLE} The maximum likelihood estimator for given observations.
#' \item \code{LL} The log likelihood calculated from the observed data using the MLE estimator.
#' \item \code{SE} The standard error for the MLE.
#' \item \code{SEtype} The method used to calculate the standard error.
#' }
#'
#' @author Alma Velazquez: \email{a.m.velazquez@@wustl.edu}
#' @aliases PoisMLE-class initialize,estimatePois-method 
#' @rdname PoisMLE
#' @export
setClass(Class="PoisMLE", 
         representation = representation(
           y = "numeric",
           MLE = "numeric",
           LL = "numeric",
           SE = "numeric",
           SEtype = "character"
         ),
         prototype = prototype(
           y = 0,
           MLE = 0,
           LL = 0,
           SE = 0,
           SEtype = "basic"
         )
)

#' @export
setMethod("initialize", "PoisMLE", 
          function(.Object, ...){
            value = callNextMethod()
            # Call validator
            # validObject(value)
            return(value)
          }
) 