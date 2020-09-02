rm(list = ls())

library(ggplot2)
library(effsize)
library(patchwork)
library(tibble)
source(file.path("R", "functions.R"))
source(file.path("R", "ggplotTheme.R"))

# simulate data ----
if (!file.exists("data/simulatedDataset.rds")) {
  set.seed(123)
  n <- 50
  cohenD <- 0.3#5
  sd <- 2
  sdDiff <- sqrt(2 * sd^2)
  deltaMu <- cohenD * sdDiff#sd / sqrt(2)

  X <- MASS::mvrnorm(n, mu = c(5 + deltaMu, 5), Sigma = diag(sd^2, 2), empirical = TRUE)
  x <- X[, 1]
  y <- X[, 2]

  getCohenD(x, y)
  # dd <- cohen.d.default(x, y, paired = TRUE, noncentral = FALSE)[c("estimate", "conf.int")]
  # dd # 0.35 == sqrt(2) * mean(x - y) / sd(x - y)
  t.test(x = x, y = y, paired = TRUE)
  BFobj <- BayesFactor::ttestBF(x = x, y = y, paired = TRUE, rscale = 10000)
  ee <- BayesFactor::posterior(BFobj, iterations = 1e4)
  colMeans(ee)
  quantile(ee[, 3], c(0.025, 0.975))

  BFobj0 <- BayesFactor::ttestBF(x = x - y)
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

# cohenD <- cohen.d.default(dat[["x"]], dat[["y"]], paired = TRUE, noncentral = FALSE)[c("estimate", "conf.int")]
# effectSize <- tibble(
#   Estimate = cohenD[["estimate"]],
#   Lower    = cohenD[["conf.int"]][1L],
#   Upper    = cohenD[["conf.int"]][2L]
# )
cohenD <- getCohenD(dat[["x"]], dat[["y"]])
effectSize <- tibble(
  Estimate = cohenD[[1]],
  Lower    = cohenD[[2]],
  Upper    = cohenD[[3]],
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
pointSize <- 10
lineSize  <- 1.25
gp <- geom_point(size = pointSize)
ge <- geom_errorbar(width = 0.2, size = lineSize)
h <- 0.1
dfLine <- as.data.frame(approx(2:1, datSum$means, c(2 - h, 1 + h)))
gl <- geom_line(data = dfLine, mapping = aes(x = x, y = y), position = position_dodge(0.5), size = lineSize, inherit.aes = FALSE)

ybreaks <- pretty(unlist(datSum[-1]))
groupNames <- c("Talking", "Control")
graphLeft <- ggplot(data = datSum, aes(x = condition, y = means, ymin = lowerci, ymax = upperci, group = 1)) +
  gp + ge + gl +
  labs(x = NULL, y = "Growth (grams)") +
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
