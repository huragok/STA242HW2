#' Constructor for S3 class BMLGrid
#'
#' @param r A non-negative integer, the number of rows of the grid.
#' @param c A non-negative integer, the number of columns of the grid.
#' @param ncars A named vector of 2 non-negative integers where \code{ncars['red']}, \code{ncars['blue']} represent the number of red/blue cars in the grid, respectively.
#' @return A BMLGrid class object with 3 attirbutes: grid, red and blue.
#' @examples
#' library(BMLGrid)
#' g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100, blue = 100))
#' @export
createBMLGrid <- function(r, c, ncars) {
  cars <- sample(1 : (r * c), ncars['red'] + ncars['blue']) # The vector index of the cars in the grid
  red <- sample(cars, ncars['red'])  # The vector index? of the red cars in the grid
  blue <- setdiff(cars, red)  # The vector index of the blue cars in the grid
  grid <- matrix(0, nrow = r, ncol = c) # The matrix representing the cars, 0 indicates no cars on that grid
  grid[red] <- 1 # 1 indicates a red car on the grid
  grid[blue] <- 2 # 2 indicates a blue car on the grid
  
  instance_BMLGrid <- list(grid = grid, red = red, blue = blue)
  class(instance_BMLGrid) <- 'BMLGrid'
  return(instance_BMLGrid)
}

#' BMLGrid class validating function
#' 
#' Validate that the 3 components of a BMLGrid class object is consistent
#'
#' @param g A BMLGrid class object.
#' @examples
#' library(BMLGrid)
#' g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100, blue = 100))
#' validateBMLGrid(g)
#' g.out = runBMLGrid(g, numSteps = 10000)
#' validateBMLGrid(g.out)
#' @export
validateBMLGrid <- function(g) {
  white <- setdiff(setdiff(seq_len(length(g$grid)), g$red), g$blue)
  return(all(g$grid[g$red] == 1) && all(g$grid[g$blue] == 2) && all(g$grid[white] == 0))
}

#' plot method for BMLGrid class object
#'
#' Plot the cars on the grid as red/blue squares over a white background.
#'
#' @param g A BMLGrid class object.
#' @param ... Other input arguments are simply ignored.
#' @examples
#' library(BMLGrid)
#' g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100, blue = 100))
#' plot(g)
#' @export
plot.BMLGrid <- function(g, ...) {
  colormap <- c("white", "red", "blue")
  image(seq_len(ncol(g$grid)), seq_len(nrow(g$grid)), t(g$grid), col = colormap, xlab = '', ylab = '')
}

#' summary method for BMLGrid class object
#'
#' The summary includes information on the grid size and the number of red and blue cars in the grid.
#'
#' @param g A BMLGrid class object.
#' @param ... Other input arguments are simply ignored.
#' @examples
#' library(BMLGrid)
#' g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100, blue = 100))
#' summary(g)
#' @export
summary.BMLGrid <- function(g, ...) {
  lines <- c("BMLGrid class object.", paste(c(" -", toString(nrow(g$grid)), 'rows,', toString(ncol(g$grid)), 'columns'), collapse = ' '), paste(c(" -", toString(length(g$red)), 'red,', toString(length(g$blue)), 'blue.\n'), collapse = ' '))
  return(cat(paste(lines, collapse = '\n')))
}

#' Simulator for Biham-Middleton-Levine Traffic Model.
#' 
#' The function that actuall runs the Biham-Middleton-Levine Traffic Model from an initial state by a given number of steps.
#'
#' @import animation 
#' @param g A BMLGrid class object representing the initial state of the grid.
#' @param numSteps Number of moves/periods.
#' @param movieName If specified as a non-NULL string, functions from package 'animation' will be used to record the BML process as a movie.
#' @param recordSpeed The flag value indicating whether to record and return the average speed of the red and blue cars ar each step.
#' @return If recordSpeed is unspecified or specified as \code{FALSE}, returns a \code{BMLGrid} object representing the final state of the simulation; otherwise return a list where the first element is the final-state grid object and the 2nd and 3rd elements record the average speed of red cars and blue cars, respectively.
#' @examples
#' library(BMLGrid)
#' g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100, blue = 100))
#' g.out = runBMLGrid(g, numSteps = 10000)
#' plot(g.out)
#' g.out = runBMLGrid(g, numSteps = 10000, movieName = 'movieBMLGrid', recordSpeed = TRUE)
#' plot(g.out$g)
#' summary(g.out$v.blue)
#' summary(g.out$v.red)
#' @export
runBMLGrid <- function(g, numSteps, movieName = NULL, recordSpeed = FALSE) {
  r <- nrow(g$grid)
  c <- ncol(g$grid)
  
  if (r == 0 || c == 0 || (length(g$red) + length(g$blue)) == 0) { # Degenerate cases, return immediatly
    warning('Degenerate BMLGrid object!')
    flush.console()
    return(g)
  }
  flag_movie <- !is.null(movieName)
  if (flag_movie){
    par(bg = "white") # ensure the background color is white
    plot(c, r, type = "n")
    animation::ani.record(reset = TRUE) # clear history before recording
    plot(g) # Plot the initial g
    animation::ani.record() # record the current frame
  }
  
  if (recordSpeed) {
    nmoved <- rep(0, numSteps + 1) # Record the number of cars moved at each step
    nmoved[1] <- get_nmoved(g$grid, r, c, g$blue, 'up') 
  }
  movable_any <- TRUE
  for (step in seq_len(numSteps)) {
    if (step %% 2 == 0) { # Red cars move to right by 1 grid
      red_right <- idx_right(g$red, r, c) # The vector index of the right grids to current red cars
      movable <- (g$grid[red_right] == 0)
      g$grid[g$red[movable]] <- 0 # Update grid
      g$grid[red_right[movable]] <- 1
      g$red <- c(red_right[movable], g$red[!movable])
      if (recordSpeed) {
        nmoved[step + 1] <- get_nmoved(g$grid, r, c, g$red, 'up')  # Record the number of cars moved at each step
      }
    } else { # Blue cars move upward by 1 grid
      blue_up <- idx_up(g$blue, r, c) # The vector index of the right grids to current red cars
      movable <- (g$grid[blue_up] == 0)
      g$grid[g$blue[movable]] <- 0 # Update grid
      g$grid[blue_up[movable]] <- 2
      g$blue <- c(blue_up[movable], g$blue[!movable])
      if (recordSpeed) {
        nmoved[step + 1] <- get_nmoved(g$grid, r, c, g$blue, 'right')  # Record the number of cars moved at each step
      }
    }
    if (!movable_any && !any(movable)) {
      #warning('fuckyou!')
      warning(paste('Grid lock detected at step', toString(step)))
      flush.console()
      break # We have entered a grid lock, no need to continue
    } else {
      movable_any <- any(movable)
    }
    if (flag_movie){
      plot(g) # Plot g
      animation::ani.record() # record the current frame
    }
  }
  if (flag_movie){
    oopts = animation::ani.options(interval = 1)
    animation::saveHTML(animation::ani.replay(), img.name = toString(movieName)) # export the animation to an HTML page
  }
  if (recordSpeed) {
    n_moved_blue <- nmoved[seq(1, numSteps, by=2)]
    n_moved_red <- nmoved[seq(2, numSteps, by=2)]
    return(list(g = g, v.blue = n_moved_blue / length(g$blue), v.red = n_moved_red / length(g$red)))
  }
  return(g)
}

# Vectorized function to get the vector index of the right grid right to the current grid
idx_right <- function(idx, r, c) {
  return((idx + r - 1) %% (r * c) + 1)
}

# Vectorized function to get the vector index of the right grid right to the current grid
idx_up <- function(idx, r, c) {
  return(idx %% r + 1 + ((idx - 1) %/% r) * r)
}

# Function to compute the number of cars that moved given the index of cars we would like to move, the current grid and whether we would like to move up or right
get_nmoved <- function(grid, r, c, cars, direction) {
  if (direction == 'up') {
    return(sum(grid[idx_up(cars, r, c)] == 0))
  } else {
    return(sum(grid[idx_right(cars, r, c)] == 0))
  }
}
