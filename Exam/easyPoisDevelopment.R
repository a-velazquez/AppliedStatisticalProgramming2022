### Creating and testing functions

# Test objects
l <- test_fun1(vec)
vec = c(1,2,3,4)
N <- length(vec)
LL <- test_fun2(vec, l)

### MLE
test_fun1 <- function(y){
  n <- length(y)
  mle_result <- sum(y, na.rm = TRUE) / n
  return(mle_result)
}

test_fun1(vec)


test_fun2 <- function(y, lambda){
  n <- length(y)
  ll_result <- (-n * lambda) - sum(log(gmp::factorialZ(y))) + log(lambda) * sum(y)
  return(ll_result)
}

test_fun2(vec, test_fun1(vec))


