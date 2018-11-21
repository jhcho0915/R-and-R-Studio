# This script runs a logistic growth model and plots the behavior for the
# first N time points

rm(list=ls())  # clear out any unnecessary variables
r <- 3  # growth rate parameter
N <- 20  # number of time points to run
x <- rep(0,N)  # initialize a vector of zeros (to be filled in with populations)
x[1] <- 3.7
# initial population (must be between 0 and 1)

# Run the population model for N time points
for (i in 2:N) {
  x[i] <- r*(x[i-1]^3)*exp(-(x[i-1]))
}

plot(1:N, x, xlab="time", ylab="population")


