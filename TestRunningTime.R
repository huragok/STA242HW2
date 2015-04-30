library(BMLGrid)

len_edge <- c(128, 256, 512, 1024) # Edge length of the grid
rho <- seq(0.1, 0.7, by=0.1) # different rho
col_range <- c('blue', 'red', 'black', 'darkgreen')
numSteps <- 10000
nrep <- 5
p_red <- 0.5

timeRunBMLGrid <- function(len_edge, rho, p_red, numSteps) {
  ncars <- c(red = round(len_edge * len_edge * rho * p_red), blue = round(len_edge * len_edge * rho * (1 - p_red)))
  g <- createBMLGrid(len_edge, len_edge, ncars)
  g.out <- runBMLGrid(g, numSteps)
  return(g.out)
}

running_time <- list()
idx <- 1
for (i_len_edge in seq_along(len_edge)) {
  for (i_rho in seq_along(rho)) {
    running_time[[idx]] <- system.time(replicate(nrep, g.out <- timeRunBMLGrid(len_edge[i_len_edge], rho[i_rho], p_red, numSteps))) / nrep
    idx <- idx + 1
    print(paste('Edge length =', toString(len_edge[i_len_edge]), 'rho =', toString(rho[i_rho]), 'completed.'))
  }
}

user_time <- do.call(rbind, running_time)[, 1]
pdf('TestRunningTime.pdf')
plot(c(0, 0.8), c(0, 200), type = 'n',  xlab="rho", ylab="User Time (s)")

for (i_len_edge in seq_along(len_edge)) {
   lines(rho, user_time[((i_len_edge-1) * length(rho) + 1) : (i_len_edge * length(rho))], type="o",  col=col_range[i_len_edge], lwd=2.5, lty = 1, pch = 1, cex=1)
}
legend(0.6, 200, legend = paste(rep_len('l =', length(len_edge)), len_edge), lty=rep_len(1, length(len_edge)), lwd=rep_len(2.5, length(len_edge)), col=col_range)
dev.off()
