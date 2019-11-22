# -----------------------------------------------------------------------------
# Example 1 - Investigate the differences between RRO and OSR, through two 
# separate R sessions

R.version			# Check the underlying version of R
installed.packages	# What packages are installed with R
Revo.version		# Check if we're running RRO/P or OSR
getOption("repos")	# We can also look and see what mirror we're pointing to

# OSR PCA example to check performance
if(require(Revobase)){setMKLthreads(4)} 

# Now let's test our R versions against a standard benchmark 
url.show('http://r.research.att.com/benchmarks/R-benchmark-25.R')
source('http://r.research.att.com/benchmarks/R-benchmark-25.R')


# -----------------------------------------------------------------------------
# Example 2 - Performance improvements for code with linear algebra

# Make sure version.compare is installed
install.packages("devtools")
library(devtools)

install_github("andrie/version.compare")
library('version.compare')

# The basis of this package is using Rscript for a remote call
system("Rscript -e rnorm(5)")
system("Rscript --version")


# Once installed we need to let it know where to find the versions of R that are
# of interest
findRscript()  # Will automatically search R's normal install paths
(installedR <- switch(Sys.info()["sysname"],
                      Windows = c("c:/program files/RRO/R-3.1.2/bin/x64/Rscript.exe",
                                  "c:/program files/R/R-3.1.2/bin/x64/Rscript.exe"),
                      Linux = c("/usr/lib64/RRO/R-3.1.2/lib/R/bin/Rscript",
                                "/usr/lib/R/bin/Rscript")))

installedR <- "c:/program files/RRO/R-3.1.2/bin/x64/Rscript.exe"

# Let's first test a simple example -------------------------------------------
version.time({  cat("Hello World\n")
                mean(rnorm(1e6))
             }, installedR)
  
  
# version.time requires an expression to make code entry a bit easier we can 
# also make use of "quote" which creates an expression
foo <- quote({  
  cat("Hello World\n")
  mean(rnorm(1e6))
})

version.time(foo, installedR)



# Now let's test a matrix cross product where we'd expect to see a perf diff --
foo <- quote({
  set.seed(5)
  m <- 2000
  n <- 5000
  A <- matrix(runif(m*n), m, n)
  system.time(B <- crossprod(A))   
  
})

version.time(foo, installedR)


# One more example, Principal Components Analysis -----------------------------
foo <- quote({
  m <- 5000
  n <- 500
  A <- matrix (runif (m*n),m,n)
  #system.time(P <- prcomp(A))
  prcomp(A)   # results will be displayed according to the script, if we 
              # just printed the results we'd get a cluttered output
})

(vcResults <- version.time(foo, installedR))



# Let's create a helper fn to display the results
vtDisplay <- function (version.timeResults, rscripts){
  vcFormatted <- data.frame("R.version"=NA,"Elapsed.time.sec"=NA)  
  
  for (i in 1:length(installedR)){
    vcFormatted[i,] <- c(rscripts[i],version.timeResults[[i]]$time[['elapsed']])
  }
  return(vcFormatted)
}

View(vtDisplay(vcResults,installedR))

# We can also source other files, like a standard benchmark -------------------
foo <- quote({
  source('http://r.research.att.com/benchmarks/R-benchmark-25.R')
})
version.time(foo, installedR)


# -----------------------------------------------------------------------------
# Example 3 - Use the 'checkpoint' package for reproducible code

# checkpoint can be installed with OSR or will be pre-installed with RRO
#install.packages('checkpoint')
#library(checkpoint)

# Specify a date for the checkpoint 
checkpoint("2015-01-01", project = 'C:/Data/R/Demos/v7/v7 Demos/RRO')
library(ggplot2)
ggplot(mtcars, aes(mpg, cyl)) +  geom_point()


# -----------------------------------------------------------------------------
# The above examples address som of the shortcomings of OSR, RRE extends these
# capabilities by also addressing:
#	- Larger than memory data sets
#	- Code portability across platforma
#	- Parallelized algorithms
