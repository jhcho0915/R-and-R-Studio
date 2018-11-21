rm(list=ls())

tmax=1000  # simulation time
IC <- c(X = -10, Y = -10, Z = 30)  # initial condition

parameters <- c(b = 8/3, sigma = 10, r = 28)

# Define the Lorenz system
Lorenz<-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    # rate of change
    dX <- sigma*(Y-X)
    dY <- r*X-Y-X*Z
    dZ <- X*Y-b*Z
    
    # return the rate of change
    list(c(dX, dY, dZ))
  }) # end with(as.list ...
}

times <- seq(0, tmax, by = 0.01)  # times to solve the system for

library(deSolve)
traj <- ode(y = IC, times = times, func = Lorenz, parms = parameters)
traj <- as.data.frame(traj)

z <- traj$Z

# Find all the peak z values
zmax <- 0
counter <- 1
for (i in 2:(length(z)-1)) {
  if (z[i-1]<z[i] & z[i+1]<z[i]) {
    zmax[counter] <- z[i]
    counter <- counter+1
  }
}

plot(zmax[1:(counter-1)], zmax[2:counter], xlab="z value at peak",
     ylab="z value at next peak")
