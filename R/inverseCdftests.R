par2 <- c(.5, .1, 1.2)
curve(pss(x, par2), -3, 0, n = 2^12)

ps <- seq(1e-10, (pss(0, par2) + pss(0- 1e-100, par2)) / 2, length.out = 100)
qs <- qnorm(ps, par2[2], par2[3]) * (1 - par2[1])
# qs <- qnorm(ps, par2[2], par2[3])
lines(qs, ps, col = 2, lty = 2)


pss(0-1e-10, par2)
qs


curve()



erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
## (see Abramowitz and Stegun 29.2.29)
## and the so-called 'complementary error function'
erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)
## and the inverses
erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)
erfcinv <- function (x) qnorm(x/2, lower = FALSE)/sqrt(2)



m <- 2
s <- 2
h0 <- .7
par2 <- c(h0, m, s)
foo <- function(p, m, s, h0) m - sqrt(2) * s * erfcinv(- 2 * p / (h0 - 1))
foo2 <- function(p, m, s, h0) m - sqrt(2) * s * erfcinv( 2 * (h0 - p) / (h0 - 1))

# \[Mu] - Sqrt[2] \[Sigma] InverseErfc[(2 (h0 - #1))/(-1 + h0)]

ps <- seq(pss(-3, par2), pss(0, par2), length.out = 100)
qs <- foo(ps, m, s, h0)
ps2 <- seq(pss(0+1e-10, par2), pss(3, par2), length.out = 100)
qs2 <- foo2(ps2, m, s, h0)


curve(pss(x, par2), -3, 3, n = 2^12)
lines(qs, ps, col = 2, lty = 2)
lines(qs2, ps2, col = 3, lty = 2)


inverseCdf <- function(p, par) {

  thresh0 <- pss(0, par)
  thresh1 <- pss(0+1e-100, par)
  out <- numeric(length(p))

  idx1 <- p < thresh0 | p > thresh1 # where F(p) != 0
  idx0 <- p[idx1] >= thresh1 # where F(p) < 0

  out[idx1] <- par[2] - sqrt(2) * par[3] * erfcinv(2 * (h0*idx0 - p[idx1]) / (h0 - 1))
  return(out)
}

curve(pss(x, par2), -3, 3, n = 2^12)
ps <- seq(0.0001, 0.9999, 0.0001)
lines(inverseCdf(ps, par2), ps, col = 2, lty = 2)


par3 <- c(.5, .3, .1)

inverseCdf(c(0.025), par3)
inverseCdf(c(0.975), par3)

curve(pss(x, par3), -3, 3)
