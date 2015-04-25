# UC Davis STA 242 2015 Spring Assignment 2
# Simulation on Biham-Middleton-Levine (BML) traffic model
# Author: Wenhao Wu
# Email: wenhaowu1989@hotmail.com
# Date: April 6th 2015

library(animation)

# Construction and initialization function of the BMLGrid class
BMLGrid <- function(r, c, ncars) {
  cars <- sample(1 : (r * c), ncars['red'] + ncars['blue']) # The vector index of the cars in the grid
  red <- sample(cars, ncars['red'])  # The vector index of the red cars in the grid
  blue <- setdiff(cars, red)  # The vector index of the blue cars in the grid
  grid <- matrix(0, nrow = r, ncol = c) # The matrix representing the cars, 0 indicates no cars on that grid
  grid[red] <- 1 # 1 indicates a red car on the grid
  grid[blue] <- 2 # 2 indicates a blue car on the grid
  
  instance_BMLGrid <- list(grid = grid, red = red, blue = blue)
  class(instance_BMLGrid) <- 'BMLGrid'
  return(instance_BMLGrid)
}

# Vectorized function to get the vector index of the right grid right to the current grid
idx_right <- function(idx, r, c) {
  return((idx + r - 1) %% (r * c) + 1)
}

# Vectorized function to get the vector index of the right grid right to the current grid
idx_up <- function(idx, r, c) {
  return(idx %% r + 1 + ((idx - 1) %/% r) * r)
}

# Mehthod to plot BMLGrid class object
plot.BMLGrid <- function(g) {
  colormap <- c("white", "red", "blue")
  image(1 : ncol(g$grid), 1 : nrow(g$grid), t(g$grid), col = colormap, xlab = '', ylab = '')
}

# Method to summarize BMLGrid class object
summary.BMLGrid <- function(g) {
  lines <- c("BMLGrid class object.", paste(c(" -", toString(nrow(g$grid)), 'rows,', toString(ncol(g$grid)), 'columns'), collapse = ' '), paste(c(" -", toString(length(g$red)), 'red,', toString(length(g$blue)), 'blue.\n'), collapse = ' '))
  return(cat(paste(lines, collapse = '\n')))
}

# Let us execute this constructor
r <- 100
c <- 99
rho <- 0.3
p_red <- 0.5
numSteps <- 10000
profile <- TRUE # Parameter to determine whether to profile the program or not
movie <- FALSE # Parameter to determine whether to record a movie or not

ncars <- c(red = round(r * c * rho * p_red), blue = round(r * c * rho * (1 - p_red)))

g <- BMLGrid(r, c, ncars)

# Let there be traffic
if (movie){
  par(bg = "white") # ensure the background color is white
  plot(c, r, type = "n")
  ani.record(reset = TRUE) # clear history before recording
  plot(g) # Plot the initial g
  ani.record() # record the current frame
}
if (profile){
  Rprof("ProfBMLGrid.out", line.profiling=TRUE) # Profiling the program
}

for (step in seq(1, numSteps)) {
  if (step %% 2 == 1) { # Red cars move to right by 1 grid
    red_right <- idx_right(g$red, r, c) # The vector index of the right grids to current red cars
    red_new <- ifelse(g$grid[red_right] == 0, red_right, g$red) # If not occupied, move to right. Else stay at the current grid
    
    g$grid[g$red] <- 0 # Update grid
    g$red <- red_new
    g$grid[g$red] <- 1
  } else { # Blue cars move upward by 1 grid
    blue_up <- idx_up(g$blue, r, c) # The vector index of the right grids to current red cars
    blue_new <- ifelse(g$grid[blue_up] == 0, blue_up, g$blue) # If not occupied, move to right. Else stay at the current grid
    
    g$grid[g$blue] <- 0 # Update grid
    g$blue <- blue_new
    g$grid[g$blue] <- 2
  }
  if (movie){
    plot(g) # Plot g
    ani.record() # record the current frame
  }
}

if (profile){
  Rprof(NULL)
  summaryRprof("ProfBMLGrid.out")
  summaryRprof("ProfBMLGrid.out", lines = "show")
}

if (movie){
  oopts = ani.options(interval = 1)
  saveHTML(ani.replay(), img.name = "record_plot") # export the animation to an HTML page
}

plot(g)

