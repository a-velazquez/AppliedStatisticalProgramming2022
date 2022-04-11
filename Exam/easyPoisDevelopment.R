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

test_fun3 <- function(y, SEtype, B=10){
  n <- length(y)
  if(SEtype=="basic"){
    se_result <- sqrt(test_fun1(y)/n)
  }
  else if(SEtype=="bootstrap"){
    
    all_samples <- matrix(nrow = n, ncol = B)

    for(i in 1:B){
      sample_col <- sample(y, n, replace = TRUE)
      all_samples[,i] <- sample_col
    }

    sample_mles <- apply(all_samples, 2, test_fun1)
    
    se_result <- sd(sample_mles)
  }
  
  return(se_result)
}

test_fun3(vec, "basic", B=1000)
test_fun3(vec, "bootstrap", B=1000)
test_fun3(vec, "bootstrap")

