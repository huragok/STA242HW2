library('BMLGrid')

r <- 100
c <- 99
p_red <- 0.5
numSteps <- 10000

pdf("TestVelocity.pdf")
plot(c(0, numSteps), c(0, 1), type = 'n',  xlab="t", ylab="Average Velocity")
rho_range <- c(0.2, 0.33, 0.38, 0.43, 0.5)
col_range <- c('cyan', 'blue', 'black', 'red', 'yellow')
for (idx in seq_along(rho_range)) {
  rho <- rho_range[idx]
  print(paste('rho =', toString(rho)))
  ncars <- c(red = round(r * c * rho * p_red), blue = round(r * c * rho * (1 - p_red)))
  g <- createBMLGrid(r, c, ncars)
  print(validateBMLGrid(g))
  g.out <- runBMLGrid(g, numSteps, recordSpeed = TRUE)
  print(validateBMLGrid(g.out$g))
  
  lines(2 * seq_along(g.out$v.blue) - 1, g.out$v.blue, type="l",  col=col_range[idx], lwd=2.5, lty = 1, pch = '.', cex=1)
  #lines(2 * seq_along(g.out$v.red), g.out$v.red, type="l",  col=col_range[idx], lwd=2.5, lty = 2, pch = '.', cex=1)
}
legend(7000, 0.35, legend = paste(rep_len('rho =', 5), rho_range), lty=c(1,1,1,1,1), lwd=c(2.5,2.5,2.5,2.5, 2.5),col=col_range)
dev.off()
