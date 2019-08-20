rm(list = ls())

library(ggplot2)
library(effsize)
library(patchwork)
library(tibble)
source("R/functions.R")
source("R/ggplotTheme.R")

# simulate data ----
if (!file.exists("data/simulatedDataset.rds")) {
  set.seed(123)
  n <- 100
  cohenD <- 0.35
  sd <- 2
  deltaMu <- cohenD * sd

  X <- MASS::mvrnorm(n, mu = c(7 + deltaMu, 7), Sigma = diag(sd^2, 2), empirical = TRUE)
  x <- X[, 1]
  y <- X[, 2]

  dd <- cohen.d.default(x, y, paired = TRUE, noncentral = FALSE)[c("estimate", "conf.int")]
  dd # 0.35 == sqrt(2) * mean(x - y) / sd(x - y)
  t.test(x = x, y = y, paired = TRUE)
  BFobj <- BayesFactor::ttestBF(x = x, y = y, paired = TRUE)
  ee <- BayesFactor::posterior(BFobj, iterations = 1e4)
  mean(ee[, "delta"])
  bf(mean(x - y) / sd(x - y), n, v0 = 1)

  ybar <- mean(x - y) / sd(x - y)
  # ybar <- sqrt(2) * mean(x - y) / sd(x - y)
  up0 <- updatePar(0.5, 1, n, ybar)
  cr0 <- postStat(up0)
  up1 <- updatePar(0, 1, n, ybar)
  cr1 <- postStat(up1)
  round(cr0, 2)
  round(cr1, 2)

  curve(pss(x, up0), from = -.5, to = 1)
  curve(pss(x, up1), col = 2, add = TRUE)
  points(cr0, pss(cr0, up0), col = 1, pch = 16)
  points(cr1, pss(cr1, up1), col = 2, pch = 16)

  dat <- tibble(
    x = x,
    y = y,
    d = x - y,
  )
  saveRDS(dat, file = "data/simulatedDataset.rds")
}

dat <- readRDS("data/simulatedDataset.rds")
datLong <- tibble(
  dependent = c(dat[["x"]], dat[["y"]]),
  condition = rep(paste0("Group", 1:2), each = nrow(dat))
)

cohenD <- cohen.d.default(dat[["x"]], dat[["y"]], paired = TRUE, noncentral = FALSE)[c("estimate", "conf.int")]
effectSize <- tibble(
  Estimate = cohenD[["estimate"]],
  Lower    = cohenD[["conf.int"]][1L],
  Upper    = cohenD[["conf.int"]][2L]
)

# ttestResults <- t.test(x = dat[["x"]], y = dat[["y"]], paired = TRUE)
# ns <- c(nrow(dat), nrow(dat))
# effectSize <- tes(t = ttestResults[["statistic"]], n.1 = ns[1], n.2 = ns[2], level = 95, verbose = FALSE)[c(4, 6:7)]
# colnames(effectSize) <- c("Estimate", "Lower", "Upper")

getUpperCi <- function(x) mean(x) + qnorm(0.975) * sd(x) / sqrt(length(x))
getLowerCi <- function(x) mean(x) + qnorm(0.025) * sd(x) / sqrt(length(x))
datSum <- tibble(
  condition = c("Talking", "Control"),
  means     = c(mean(dat[["x"]]), mean(dat[["y"]])),
  upperci   = c(getUpperCi(dat[["x"]]), getUpperCi(dat[["y"]])),
  lowerci   = c(getLowerCi(dat[["x"]]), getLowerCi(dat[["y"]]))
)

base_size <- 34
pointSize <- 8
lineSize  <- 1.25
gp <- geom_point(size = pointSize)
ge <- geom_errorbar(width = 0.2, size = lineSize)
gl <- geom_line(position = position_dodge(0.2), size = lineSize)

ybreaks <- pretty(unlist(datSum[-1]))
groupNames <- c("Talking", "Control")
graphLeft <- ggplot(data = datSum, aes(x = condition, y = means, ymin = lowerci, ymax = upperci, group = 1)) +
  gp + ge + gl +
  labs(x = NULL, y = "Growth") +
  scale_y_continuous(breaks = ybreaks, limits = range(ybreaks)) +
  geom_rangeframe() + myTheme(base_size = base_size)

ybreaks <- pretty(unlist(effectSize))
graphRight <- ggplot(data = effectSize, aes(ymin = Lower, ymax = Upper, x = factor(0), y = Estimate)) +
  gp + ge +
	scale_x_discrete(name = NULL, labels = paste0(groupNames, collapse = " - ")) +
  scale_y_continuous(breaks = ybreaks, limits = range(ybreaks)) +
	labs(x = NULL, y = "Effect size d") +
  geom_rangeframe() + myTheme(base_size = base_size) + theme(axis.ticks.x = element_blank())

graph <- graphLeft + graphRight
graph
# write.csv(x = effectSize, "tables/effectSizeExample.csv", quote = FALSE)
# saveFigure(graph = graph, filename = "descriptivesPlot.pdf", width = 18, height = 7)
