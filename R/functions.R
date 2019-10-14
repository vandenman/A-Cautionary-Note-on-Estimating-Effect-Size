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

modelAveragedCdf <- function(x, pH0, cdfH1 = pnorm, ...) {
	return((x >= 0) * pH0 + (1 - pH0) * cdfH1(x, ...))
}

modelAveragedQuantile <- function(q, pH0, cdfH1 = pnorm, ...) {
	return(
	  optimize(function(x, pH0, q, cdfH1, ...) log((modelAveragedCdf(x, pH0, cdfH1, ...) - q)^2),
	           interval = c(-2, 2), pH0 = pH0, q = q, cdfH1 = cdfH1, ...)[["minimum"]]

		# uniroot(function(x, p, q, cdfH1, ...) modelAveragedCdf(x, p, cdfH1, ...) - q, interval = c(-5, 5),
		# 	p = pH0, q = q, cdfH1 = cdfH1, ...)[["root"]]
	)
}

modelAveragedCRI <- function(pH0, cdfH1 = pnorm, h = 0.95, ...) {
	# h = 0.95 means a 95% credible interval
	lower <- (1 - h) / 2
	upper <- 1 - lower
	estLower <- modelAveragedQuantile(lower, pH0, cdfH1, ...)
	estUpper <- modelAveragedQuantile(upper, pH0, cdfH1, ...)
	return(tibble(lower = estLower, upper = estUpper))
}

# graphical options
getColors <- function(n, name = "Dark2") RColorBrewer::brewer.pal(8, name)[seq_len(n)]
scale_color_viridis_d <- ggplot2::scale_color_viridis_d
formals(scale_color_viridis_d)$option <- "C"

saveFigure <- function(graph, filename, width = 7, height = 7, ...,
                       directory = "figures") {


  file <- file.path(getwd(), directory, filename)
  ext <- tools::file_ext(filename)
  assertthat::assert_that(
    ext %in% c("pdf", "tex", "tikz"),
    !file.exists(file) || assertthat::is.writeable(file)
  )

  switch(ext,
    "pdf"  = grDevices::pdf(file, width, height, ...),
    "tex"  = tikzDevice::tikz(file, width, height, ...),
    "tikz" = tikzDevice::tikz(file, width, height, ...),
    stop("file should have extension .pdf or .tex!")
  )
  print(graph)
  grDevices::dev.off()
}

getCohenD <- function(x, y, alpha = 0.05) {
  # Computes cohen d for a one-sample t-test on the difference scores.
  # Based on:
  #   - http://www.real-statistics.com/students-t-distribution/one-sample-t-test/confidence-interval-one-sample-cohens-d/
  #   - Hedges & Olkin (1985)
  diff <- x - y
  n <- length(x)
  d <- mean(diff) / sd(diff)
  se <- sqrt(1 / n + d^2 / (2*n))
  crit <- qnorm(1 - alpha / 2 )
  ans <- c(d, d - se * crit, d + se * crit)
  return(ans)
}

erfcinv <- function(x) qnorm(x / 2, lower.tail = FALSE) / sqrt(2)
inverseCdf <- function(p, par) {

  thresh0 <- pss(0, par)
  thresh1 <- pss(0 + 1e-100, par)
  out <- numeric(length(p))

  idx1 <- p < thresh0 | p > thresh1 # where F(p) != 0
  idx0 <- p[idx1] >= thresh1 # where F(p) < 0

  out[idx1] <- par[2] - sqrt(2) * par[3] * erfcinv(2 * (par[1]*idx0 - p[idx1]) / (par[1] - 1))
  return(out)
}

# rounding function
mceiling <- function(x, base) base * ceiling(x / base)
mround   <- function(x, base) base * round(x / base)

# courtesy of Jeff Rouder ----
# pss <- function(x, par) {
#   if (x < 0) {
#     p <- (1 - par[1]) * pnorm(x, par[2], par[3])
#   } else {
#     p <- par[1] + (1 - par[1]) * pnorm(x, par[2], par[3])
#   }
#   return(p)
# }
pss <- function(x, par) {
  return((x > 0) * par[1] + (1 - par[1]) * pnorm(x, par[2], par[3]))
}


error <- function(x, p, par) log((p - pss(x, par))^2)
myQ <- function(p, par) optimize(error, c(-2, 2), p = p, par = par)$minimum
# myQ <- function(p, par) optim(par = list(0), fn = error, p = p, par0 = par, method = "Brent",
#                               lower = -2, upper = 2)$par

postStat <- function(par) {
  # lower <- myQ(.025, par=par)
  # error(lower, 0.025, par) > error(0.1, 0.025, par)
  # diff <- 0.025 - pss(lower, par)
  # upper <- myQ(.975 - diff, par)
  # browser()
  # curve(pss(x, par), from = -2, to = 2, n = 2^12)
  lower <- inverseCdf(0.025, par)
  diff <- 0.025 - pss(lower, par)
  upper <- inverseCdf(0.975 - diff, par)
  if (upper == 0) upper <- upper + 1e-100
  diff <- 0.975 - pss(upper, par)
  lower <- inverseCdf(0.025 - diff, par)
  pss(upper, par) - pss(lower, par)
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
