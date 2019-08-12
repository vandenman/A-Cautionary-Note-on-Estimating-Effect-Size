rm(list = ls())

library(readr)
library(ggplot2)
library(effsize)
library(patchwork)
source("R/functions.R")

# simulate data ----
if (!file.exists("data/simulatedDataset.rds")) {
  set.seed(123)
  n <- 50
  d <- 0.3
  sd <- 0.6
  dmu <- d# * sd
  x <- rnorm(n / 2, 0, sd)
  y <- rnorm(n / 2, 0, sd)
  y <- y - mean(y)
  x <- x - mean(x) + dmu

  t.test(x = x, y = y, paired = TRUE)
  BayesFactor::ttestBF(x = x, y = y, paired = TRUE)
  bf(mean(x - y) / sd(x - y), n, v0 = 10^2)
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

cohenD <- cohen.d.default(dat[["x"]], dat[["y"]], paired = TRUE, noncentral = TRUE)[c("estimate", "conf.int")]
effectSize <- tibble(
  Estimate = cohenD[["estimate"]],
  Lower    = cohenD[["conf.int"]][1L],
  Upper    = cohenD[["conf.int"]][2L]
)

# ttestResults <- t.test(x = dat[["x"]], y = dat[["y"]], paired = TRUE)
# ns <- c(nrow(dat), nrow(dat))
# effectSize <- tes(t = ttestResults[["statistic"]], n.1 = ns[1], n.2 = ns[2], level = 95, verbose = FALSE)[c(4, 6:7)]
# colnames(effectSize) <- c("Estimate", "Lower", "Upper")

graph1 <- ggplot(data = datLong, aes(x = condition, y = dependent)) +
	geom_boxplot(outlier.alpha = 0) +
	ggbeeswarm::geom_quasirandom(color = "grey60", alpha = .8, size = 3) +
  scale_x_discrete(name = NULL, labels = c("Group 1", "Group 2")) +
  labs(x = NULL, y = "Dependent") +
	theme_bw(24)

graph2 <- ggplot(data = effectSize, aes(ymin = Lower, ymax = Upper, x = factor(0), y = Estimate)) +
	geom_point(size = 3) +
	geom_errorbar(width = .2) +
	scale_x_discrete(name = NULL, labels = "Group 1 - Group 2") +
	labs(x = NULL, y = "Effect size, d") +
	theme_bw(24)# + theme(axis.ticks.x = element_blank())

graph12 <- graph1 + graph2
write.csv(x = effectSize, "tables/effectSizeExample.csv", quote = FALSE)
saveFigure(graph = graph12, filename = "descriptivesPlot.pdf", width = 12, height = 7)
