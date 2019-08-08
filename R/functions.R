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
	(x > 0) * pH0 + (1 - pH0) * cdfH1(x, ...)
}

modelAveragedQuantile <- function(q, pH0, cdfH1 = pnorm, ...) {
	return(
		uniroot(function(x, p, q, cdfH1, ...) modelAveragedCdf(x, p, cdfH1, ...) - q, interval = c(-5, 5),
			p = pH0, q = q, cdfH1 = cdfH1, ...)[["root"]]
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
scale_color_viridis_d <- ggplot2::scale_color_viridis_d
formals(scale_color_viridis_d)$option <- "C"

saveFigure <- function(graph, filename, width = 7, height = 7, ..., directory = "figures") {

  file <- file.path(getwd(), directory, filename)
  if (isNamespaceLoaded("assertthat")) {
    assert_that(
      has_extension(filename, "pdf"),
      !file.exists(file) || is.writeable(file)
    )
  }
  grDevices::pdf(file, width, height, ...)
  print(graph)
  grDevices::dev.off()
}

# courtesy of Jeff Rouder ----
pss <- function(x, par) {
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
