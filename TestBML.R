# UC Davis STA 242 2015 Spring Assignment 2
# Simulation on Biham-Middleton-Levine (BML) traffic model
# Author: Wenhao Wu
# Email: wenhaowu1989@hotmail.com
# Date: April 6th 2015

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
r <- 8
c <- 6
rho <- 0.5
p_red <- 0.5

ncars <- c(red = round(r * c * rho * p_red), blue = round(r * c * rho * (1 - p_red)))

g <- BMLGrid(r, c, ncars)

numSteps <- 5


