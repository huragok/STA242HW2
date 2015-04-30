library('BMLGrid')

r <- 0
c <- 99
p_red <- 0.5 # -0.5, 2
numSteps <- 10000
rho <- 0.3 #-0.8, 0

ncars <- c(red = round(r * c * rho * p_red), blue = round(r * c * rho * (1 - p_red)))
g <- createBMLGrid(r, c, ncars)
g.out <- runBMLGrid(g, numSteps)
plot(g)

