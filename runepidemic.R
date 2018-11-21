rm(list=ls())

# Set up initial condition
N0 <- 1000000  # total population
In0 <- 10  # initial infectives
S0 <- N0-In0  # initially, everyone else is susceptible
R0 <- 0  # initially, nobody has recovered
IC <- c(S=S0, In=In0, R=R0)

tmax = 2000  # number of years to run

# Parameter values (units:  per year)
parameters <- c(d=0.02, # per capita birth and death rate
  b=120, # infection transmission rate
r=100 # recovery rate
)

# Define the epidemic model
epimodel<-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    N <- S+In+R
    # rate of change
    dS <- d*N - b*S*In/N - d*S
    dIn <- b*S*In/N - r*In - d*In
    dR <- r*In - d*R
    
    # return the rate of change
    list(c(dS, dIn, dR))
  }) # end with(as.list ...
}

times <- seq(0, tmax, by = 1)  # times to solve the system for

library(deSolve)
# Solve the system
traj <- ode(y = IC, times = times, func = epimodel, parms = parameters,
            atol = 1e-7, rtol = 1e-5)
traj <- as.data.frame(traj)

plot(traj$time, traj$In, type="l", 
     xlab="time (years)", ylab="number infected",
     ylim=c(0,100))
     
