dPosteriorH1 <- function(delta, obsDelta, n, sigmaSlab) {
  sigma1 <- 1 / (n + 1/sigmaSlab^2)
  mu1 <- n * obsDelta * sigma1
  return(dnorm((delta - mu1) / sigma1))
}

pPosteriorH1 <- function(delta, obsDelta, n, sigmaSlab) {
  sigma1 <- 1 / (n + 1/sigmaSlab^2)
  mu1 <- n * obsDelta * sigma1
  return(pnorm((delta - mu1) / sigma1))
}

pss <- function(x, par) {
  # cdf of spike and sliab
  if (x < 0) {
    p <- (1 - par[1]) * pnorm(x, par[2], par[3])
  } else {
    p <- par[1] + (1 - par[1]) * pnorm(x, par[2], par[3])
  }
  return(p)
}


error <- function(x, p, par) log((p - pss(x, par))^2)
myQ <- function(p, par) optimize(error, c(-2, 2), p = p, par = par)$minimum

postStat <- function(par) {
  lower <- myQ(.025, par)
  upper <- myQ(.975, par)
  me <- par[2] * (1 - par[1])
  return(c(lower, upper, me))
}

bf <- function(d, n, v0) {
  a <- 1 / sqrt(n * (v0 + 1))
  b <- n^2 * d^2
  c <- 2 * (n + 1 / v0)
  return(a * exp(b / c))
}


updatePar <- function(rho0, v0, N, ybar) {
  prec <- (N + 1 / v0)
  v1 <- 1 / prec
  c <- N * ybar
  mu1 <- v1 * c
  w <- bf(ybar, N, v0)
  rho1 <- rho0 / (rho0 + (1 - rho0) * w)
  return(c(rho1, mu1, sqrt(v1)))
}


updateEV <- function(rho0, v0, N, ybar) {
  prec <- (N + 1 / v0)
  v1 <- 1 / prec
  c <- N * ybar
  mu1 <- v1 * c
  w <- bf(ybar, N, v0)
  rho1 <- rho0 / (rho0 + (1 - rho0) * w)
  return((1 - rho1) * mu1)
}

obs <- seq(-1, 1, .005)
I <- length(obs)
N <- 40

statS <- stat1 <- matrix(ncol = 3, nrow = I)
for (i in 1:I) {
  statS[i, ] <- postStat(updatePar(.5, 1, N, obs[i]))
  stat1[i, ] <- postStat(updatePar(0, 1, N, obs[i]))
}

x <- c(obs, rev(obs))
yS <- c(statS[, 1], rev(statS[, 2]))
y1 <- c(stat1[, 1], rev(stat1[, 2]))
loS <- max(obs[statS[, 2] < -.01])
hiS <- -loS
lo1 <- max(obs[stat1[, 2] < -.01])
hi1 <- -lo1



# pdf("fig5.pdf", width = 10, height = 10)
par(mfrow = c(2, 2), mgp = c(2, 1, 0), cex = 1.2)
par(mar = c(4, 4, 1, 1))

plot(obs, updateEV(0, 10^2, N, obs),
  type = "l", lty = 3, col = "darkred", lwd = 2,
  ylab = "", xlab = expression(paste("Observed Effect, ", d)),
  axes = FALSE
)
lines(obs, updateEV(0, 1^2, N, obs), lwd = 2)
lines(obs, updateEV(0, .5^2, N, obs), lwd = 2, lty = 2, col = "darkgreen")
abline(0, 1, lwd = 3, col = rgb(.5, .5, .5, .5))
axis(1)
axis(2)
mtext(side = 3, adj = .5, cex = 1.2, "Slab Only", line = -1)
mtext(side = 3, adj = 0, cex = 1.3, "A.")

par(xpd = NA)
legend(.5, -.1,
  legend = c(expression(sigma[0] == .5), expression(sigma[0] == 1), expression(sigma[0] == 10)),
  col = c("darkgreen", "black", "darkred"), lty = c(2, 1, 3), lwd = 2
)
par(xpd = FALSE)

par(mar = c(4, 1, 1, 4))

plot(obs, updateEV(.5, 10^2, N, obs),
  type = "l", lty = 3, col = "darkred", lwd = 2,
  ylab = expression(paste("Estimated Effect, ", delta)), xlab = expression(paste("Observed Effect, ", d)),
  axes = FALSE
)
lines(obs, updateEV(.5, 1^2, N, obs), lwd = 2)
lines(obs, updateEV(.5, .5^2, N, obs), lwd = 2, lty = 2, col = "darkgreen")
abline(0, 1, lwd = 3, col = rgb(.5, .5, .5, .5))
axis(1)
mtext(side = 3, adj = .5, cex = 1.2, "Spike-And-Slab", line = -1)
mtext(side = 3, adj = 0, cex = 1.3, "B.")

plot(obs, stat1[, 3],
  type = "n", ylim = c(min(stat1), max(stat1)),
  ylab = expression(paste("Estimated Effect, ", delta)), xlab = expression(paste("Observed Effect, ", d)),
  axes = FALSE
)
abline(v = c(lo1, hi1), lty = 3)
polygon(x, y1, col = "lightblue", border = NA)
lines(obs, stat1[, 3])
axis(1)
axis(2)
mtext(side = 3, adj = .5, cex = 1.2, "Slab Only", line = -1)
mtext(side = 3, adj = 0, cex = 1.3, "C.")


plot(obs, statS[, 3],
  type = "n", ylim = c(min(statS), max(statS)),
  ylab = expression(paste("Estimated Effect, ", delta)), xlab = expression(paste("Observed Effect, ", d)),
  axes = FALSE
)
abline(v = c(loS, hiS), lty = 3)
polygon(x, yS, col = "lightblue", border = NA)
lines(obs, statS[, 3])
axis(1)
axis(2)
mtext(side = 3, adj = .5, cex = 1.2, "Spike-and-Slab", line = -1)
mtext(side = 3, adj = 0, cex = 1.3, "D.")


# dev.off()
