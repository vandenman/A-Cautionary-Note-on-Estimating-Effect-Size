rm(list = ls())

library(dplyr)
library(ggplot2)
library(effsize)
source("R/functions.R")
source("R/ggplotTheme.R")

simulateData <- function(n, cohenD, sd = 1, baseMu = 0, empirical = FALSE) {
  sdDiff <- sqrt(2 * sd^2)
  deltaMu <- cohenD * sdDiff
  return(MASS::mvrnorm(n, mu = c(baseMu + deltaMu, baseMu), Sigma = diag(sd^2, 2), empirical = empirical))
}

getResBayesian <- function(x, y, priorPH0, sigmaSlab = 1) {
  n <- length(x)
  ybar <- mean(x - y) / sd(x - y)
  upMA <- updatePar(priorPH0, sigmaSlab, n, ybar)
  # print(upMA)
  return(c(postStat(upMA)[c(3, 1, 2)], upMA[1]))
}

getCohenD2 <- function(x, y) {
  return(unlist(cohen.d.default(x, y, paired = TRUE, noncentral = FALSE)[c("estimate", "conf.int")]))
}

ns <- c(50, 100, 200, 300, 400, 1000)#c(seq(50, 400, 50), 1e3)
# ns <- 100
# sds <- c(.1, .3, .5, .7)
# ds <- seq(0, .3, length.out = 7)
ds <- mround(seq(-.3, .3, .1), .1)
nrep <- 1e3#40#25

params <- as.matrix(expand.grid(n = ns, d = ds, rep = 1:nrep))
nsim <- nrow(params)
params <- cbind(
  params,
  # empty matrix for results
  matrix(NA_real_, nsim, 9, dimnames = list(NULL, c(
    paste0("MABay",  c("mean", "lower", "upper")),
    paste0("H1freq", c("mean", "lower", "upper")),
    paste0("H1Bay" , c("mean", "lower", "upper"))
  ))),
  "postprobH0" = NA
)

prog <- dplyr::progress_estimated(nsim)
for (i in seq_len(nsim)) {

  n <- params[i, "n"]
  d <- params[i, "d"]

  set.seed(i)
  X <- simulateData(n, d)
  x <- X[, 1L]
  y <- X[, 2L]

  tmp <- getResBayesian(x, y, 0.5)
  params[i, 4:6]   <- tmp[1:3]
  params[i, 7:9]   <- getCohenD(x, y)#unlist(cohen.d.default(x, y, paired = TRUE, noncentral = FALSE)[c("estimate", "conf.int")])
  params[i, 10:12] <- getResBayesian(x, y, 0)[1:3]
  params[i, 13] <- tmp[4]
  prog$tick()$print()

}

df <- reshape2::melt(as.data.frame(params), id = c("n", "d", "rep", "postprobH0"),
                     variable.name = "variable",
                     factorsAsStrings = TRUE)
df$variable <- as.character(df$variable)

dfPoints <- df[endsWith(df$variable, "mean"), ]
colnames(dfPoints)[colnames(dfPoints) == "value"] <- "mean"
dfCRIs   <- df[endsWith(df$variable, "lower"), ]
dfCRIs   <- cbind(dfCRIs, df[endsWith(df$variable, "upper"), "value"])
tmp <- ncol(dfCRIs)
colnames(dfCRIs)[(tmp - 1):tmp] <- c("lower", "upper")

un <- unique(dfPoints$n)
obj <- setNames(paste("N =", un), un)
labeller <- as_labeller(obj)
dfPoints$variable <- dplyr::recode(dfPoints$variable,
              H1Baymean  = "Posterior Mean Slab",
              H1freqmean = "Frequentist point estimate",
              MABaymean  = "Posterior Mean Spike and Slab"
)

# posDodge <- position_dodge()
# ggplot(data = dfPoints, aes(x = d, y = mean, group = variable, color = variable)) +
#   geom_abline() +
#   geom_point() +
#   geom_rangeframe() +
#   facet_wrap(~n, labeller = labeller, scales = "free") +
#   labs(color = NULL, x = "True Effect Size", y = "Estimate - True") +
#   myTheme(base_size = 28, legend.position = "right", legend.justification = "center")

ggplot(data = dfPoints, aes(x = mean, y = postprobH0, group = n, color = factor(n))) +
  geom_line() +
  scale_x_continuous(breaks = c(-1, -.5, 0, .5, 1), limits = c(-1, 1)) +
  facet_wrap(~variable) +
  geom_rangeframe() +
  labs(color = "n", x = expression(hat(delta)), y = expression(p(H[0]))) +
  myTheme(legend.position = "top")

# ggplot(data = dfPoints, aes(x = d, y = mean, group = n, color = factor(n))) +
#   geom_point(position = position_dodge(0.1)) +
#   scale_x_continuous(breaks = c(-1, -.5, 0, .5, 1), limits = c(-1, 1)) +
#   scale_y_continuous(breaks = c(-1, -.5, 0, .5, 1), limits = c(-1, 1)) +
#   facet_wrap(~variable) +
#   geom_rangeframe() +
#   labs(color = "n", x = expression(hat(delta)), y = expression(p(H[0]))) +
#   myTheme(legend.position = "top")

posDodge <- position_dodge(0.6)
ggplot(data = dfPoints, aes(x = factor(d), y = (mean - d), group = variable, color = variable)) +
  geom_hline(yintercept =  0) +
  geom_point(position = posDodge) +
  geom_rangeframe() +
  facet_wrap(~n, labeller = labeller, scales = "fixed") +
  labs(color = NULL, x = "True Effect Size", y = "Estimate - True") +
  myTheme(base_size = 28, legend.position = "top", legend.justification = "center")


rmse0 <- function(x, y = 0) sqrt(mean((x - y)^2))
perf <- tapply(dfPoints$mean - dfPoints$d, list(dfPoints$n, dfPoints$d, dfPoints$variable), rmse0)
mean(perf[, , 1] > perf[, , 3]) # frequency where freq  estimate is farther of than spike and slab
mean(perf[, , 1] > perf[, , 2]) # frequency where freq  estimate is farther of than spike
mean(perf[, , 2] > perf[, , 3]) # frequency where spike estimate is farther of than spike and slab
apply(perf, 3, mean)

# coverage is meh! Double check the way CRIs are calculateed!
dfCRIs$dInCRI <- dfCRIs$d > dfCRIs$lower & dfCRIs$d < dfCRIs$upper
tapply(dfCRIs$dInCRI, list(dfCRIs$variable), mean)
tapply(dfCRIs$dInCRI, list(dfCRIs$n, dfCRIs$d, dfCRIs$variable), mean)

tapply(abs(dfPoints$mean), list(dfPoints$n, dfPoints$d, dfPoints$variable), mean)
pointEsts <- tapply(abs(dfPoints$mean - dfPoints$d), list(dfPoints$n, dfPoints$d, dfPoints$variable), mean)
pointEsts[, , 1] > pointEsts[, , 3]
pointEsts[, , 2] > pointEsts[, , 3]
pointEsts[, , 1] > pointEsts[, , 2]

ud <- unique(params[, "d"])
idx <- params[, "d"] == ud[7]
mean(abs(params[idx, "MABaymean"] - params[idx, "d"]) < abs(params[idx, "H1freqmean"] - params[idx, "d"]) )
mean(abs(params[idx, "H1Baymean"] - params[idx, "d"]) < abs(params[idx, "H1freqmean"] - params[idx, "d"]) )

dfPoints$absDiff <- abs(dfPoints$mean - dfPoints$d)
ss <- split(dfPoints, list(paste("n =", dfPoints$n, paste("d =", dfPoints$d))))
dim(ss) <- c(length(unique(dfPoints$d)), length(unique(dfPoints$n)))
dimnames(ss) <- list(paste("d =", unique(dfPoints$d)), paste("n =", unique(dfPoints$n)))

nms <- unique(ss[[1, 2]]$variable)
propBetter <- array(NA, dim = c(dim(ss), 3), dimnames = list(rownames(ss), colnames(ss), c(
  "Posterior Mean Spike and Slab < Frequentist point estimate",
  "Posterior Mean Spike and Slab < Posterior Mean Slab",
  "Posterior Mean Slab < Frequentist point estimate"
)))
for (i in seq_len(nrow(ss))) {
  for (j in seq_len(ncol(ss))) {

    idxMABay  <- ss[[i, j]]$variable == nms[1]
    idxH1freq <- ss[[i, j]]$variable == nms[2]
    idxH1Bay  <- ss[[i, j]]$variable == nms[3]

    propBetter[i, j, 1] <- mean(ss[[i, j]]$absDiff[idxMABay] < ss[[i, j]]$absDiff[idxH1freq])
    propBetter[i, j, 2] <- mean(ss[[i, j]]$absDiff[idxMABay] < ss[[i, j]]$absDiff[idxH1Bay])
    propBetter[i, j, 3] <- mean(ss[[i, j]]$absDiff[idxH1Bay] < ss[[i, j]]$absDiff[idxH1freq])

  }
}
propBetter


