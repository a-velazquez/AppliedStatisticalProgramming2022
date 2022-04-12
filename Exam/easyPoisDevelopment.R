library(devtools)
library(roxygen2)
# This sets the working directory to where this file is saved.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Uncomment and run once to create the package directory, then moved files into the R folder.
# devtools::create("easyPois")


## This can be run many times as the code is updates
current.code <- as.package("easyPois")
load_all(current.code)
document(current.code)


# Check for errors
devtools::check(current.code)


# Test both types of SE methods, store one
estimatePois(c(50, 70, 80, 99, 82, 43), "basic", 223)
new_test <- estimatePois(c(50, 70, 80, 99, 82, 43), "bootstrap", 223)

# Create another test using rpois
testPOIS <- estimatePois(rpois(40, 2.5), "bootstrap", B=70000)

# Plot this one 
plot(testPOIS)

# Plot the other one
plot(new_test)


# This installs the package
devtools::install(current.code)

# This builds the package
devtools::build("easyPois")
