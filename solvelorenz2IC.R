rm(list=ls())

tmax=20  # simulation time
IC1 <- c(X = -10, Y = -10, Z = 30)  # first initial condition
IC2 <- c(X = -10.0001, Y = -10, Z = 30)  # second initial condition

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

times <- seq(0, tmax, by = 0.01)  # times to solve the equations for

library(deSolve)  # Load the deSolve package for solving differential equations
# Solve the system for the first initial condition
traj1 <- ode(y = IC1, times = times, func = Lorenz, parms = parameters)
traj1 <- as.data.frame(traj1)

# Solve the system for the second initial condition
traj2 <- ode(y = IC2, times = times, func = Lorenz, parms = parameters)
traj2 <- as.data.frame(traj2)

# Plot x versus time for both trajectories
plot(traj1$time, traj1$X, type="l", xlab="time", ylab="X", col="blue")
lines(traj2$time, traj2$X, col="red")
