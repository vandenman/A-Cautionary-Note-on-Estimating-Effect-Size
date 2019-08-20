rm(list = ls())

library(dplyr)
library(ggplot2)
library(effsize)
source("R/functions.R")
source("R/ggplotTheme.R")

simulateData <- function(n, cohenD, sd = 1, baseMu = 0) {
  deltaMu <- cohenD * sd
  return(MASS::mvrnorm(n, mu = c(baseMu + deltaMu, baseMu), Sigma = diag(sd^2, 2), empirical = FALSE))
}

getResBayesian <- function(x, y, priorPH0, sigmaSlab = .1) {
  n <- length(x)
  ybar <- mean(x - y) / sd(x - y)
  upMA <- updatePar(priorPH0, sigmaSlab, n, ybar)
  # print(upMA)
  return(postStat(upMA)[c(3, 1, 2)])
}

ns <- c(seq(50, 400, 50), 1e3)
# ns <- 100
# sds <- c(.1, .3, .5, .7)
ds <- seq(0, .4, length.out = 5)
nrep <- 25

params <- as.matrix(expand.grid(n = ns, d = ds, rep = 1:nrep))
nsim <- nrow(params)
params <- cbind(
  params,
  # empty matrix for results
  matrix(NA_real_, nsim, 9, dimnames = list(NULL, c(
    paste0("MABay",  c("mean", "lower", "upper")),
    paste0("H1freq", c("mean", "lower", "upper")),
    paste0("H1Bay" , c("mean", "lower", "upper"))
  )))
)

prog <- dplyr::progress_estimated(nsim)
for (i in seq_len(nsim)) {

  n <- params[i, "n"]
  d <- params[i, "d"]

  set.seed(i)
  X <- simulateData(n, d, 1)
  x <- X[, 1L]
  y <- X[, 2L]

  params[i, 4:6]   <- getResBayesian(x, y, 0.5)
  params[i, 7:9]   <- unlist(cohen.d.default(x, y, paired = TRUE, noncentral = FALSE)[c("estimate", "conf.int")])
  params[i, 10:12] <- getResBayesian(x, y, 0)
  prog$tick()$print()

}

df <- reshape2::melt(as.data.frame(params), id = c("n", "d", "rep"), factorsAsStrings = TRUE)
df$variable <- as.character(df$variable)

dfPoints <- df[endsWith(df$variable, "mean"), ]
colnames(dfPoints)[5] <- "mean"
dfCRIs   <- df[endsWith(df$variable, "lower"), ]
dfCRIs   <- cbind(dfCRIs, df[endsWith(df$variable, "upper"), "value"])
colnames(dfCRIs)[5:6] <- c("lower", "upper")

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

# posDodge <- position_dodge(0.6)
# ggplot(data = dfPoints, aes(x = factor(d), y = (mean - d), group = variable, color = variable)) +
#   geom_hline(yintercept =  0) +
#   geom_point(position = posDodge) +
#   geom_rangeframe() +
#   facet_wrap(~n, labeller = labeller, scales = "free") +
#   labs(color = NULL, x = "True Effect Size", y = "Estimate - True") +
#   myTheme(base_size = 28, legend.position = "right", legend.justification = "center")

# coverage is meh! Double check the way CRIs are calculateed!
dfCRIs$`df[endsWith(df$variable, "upper"), "value"]`
dfCRIs$dInCRI <- dfCRIs$d > dfCRIs$lower & dfCRIs$d < dfCRIs$upper
tapply(dfCRIs$dInCRI, list(dfCRIs$variable), mean)
tapply(dfCRIs$dInCRI, list(dfCRIs$n, dfCRIs$d, dfCRIs$variable), mean)
