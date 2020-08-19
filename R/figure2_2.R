<<<<<<< HEAD
rm(list = ls())
library(ggplot2)
library(tibble)
source("R/functions.R")
source("R/ggplotTheme.R")

priorPH0 <- 0.5
sigmaSlab <- 1
groupNames <- c("Spike and Slab", "Slab Only")

dat <- readRDS("data/simulatedDataset.rds")
n <- nrow(dat)

priorPH1 <- 1 - priorPH0
# ybar <- cohen.d.default(dat[["x"]], dat[["y"]], paired = TRUE, noncentral = FALSE)
ybar <- getCohenD(dat[["x"]], dat[["y"]])[1]
# diffScore <- dat[["d"]]
# ybar <- sqrt(2) * mean(diffScore) / sd(diffScore)

upMA <- updatePar(priorPH0, sigmaSlab, n, ybar)
ciMA <- postStat(upMA)
upH1 <- updatePar(0.0, sigmaSlab, n, ybar)
ciH1 <- postStat(upH1)
postH0 <- 1 - upMA[1]

xgrid <- seq(-0.5, 1.0, length.out = 1e4)
yvals <- dnorm(xgrid, upMA[2], upMA[3])#dPosteriorH1(xgrid, ybar, n, upMA[3])
criMA <- tibble(lower = ciMA[1], upper = ciMA[2]) # modelAveragedCRI(priorPH0, cdfH1 = pPosteriorH1, obsDelta = ybar, n = n, sigmaSlab = sigmaSlab)
criH1 <- tibble(lower = ciH1[1], upper = ciH1[2]) #modelAveragedCRI(0.0, cdfH1 = pPosteriorH1, obsDelta = obsDelta, n = n, sigmaSlab = sigmaSlab)

dfLineH1 <- tibble(x = xgrid, y = yvals)
dfLineH0 <- tibble(x = c(0, 0), y = c(0, priorPH0))

# labelH0 <- paste("p(H[0]~'|'~data) ==", formatC(upMA[1]))
labelH0 <- paste("$p(\\mathcal{H}_0 \\mid \\mathrm{data} = ", formatC(upMA[1], digits = 3), ")$")
dfTxtH0  <- add_column(dfLineH0[-1, ], label = labelH0)

dfBarCriMA <- add_column(criMA, y = 1.15 * max(yvals, priorPH0))
dfBarCriH1 <- add_column(criH1, y = 1.25 * max(yvals, priorPH0))
dfBarCri   <- add_column(rbind(dfBarCriMA, dfBarCriH1), group = groupNames)

# rescale posterior mode to PH1
dfLineH1_r <- dfLineH1
dfLineH1_r$y <- dfLineH1_r$y / max(dfLineH1_r$y) * (1 - upMA[1])
dfLineH0$y <- c(0, upMA[1])
dfTxtH0$y  <- upMA[1]
dfBarCri_r <- dfBarCri
dfBarCri_r$y <- c(0.725, 0.9)
height <- mceiling(dfBarCri_r$y[1] - max(dfLineH1_r$y), .1)
dfBarCri_r$x <- (dfBarCri_r[["lower"]] + dfBarCri_r[["upper"]]) / 2
dfTxtCRI <- tibble(
  x = (dfBarCri_r[["lower"]] + dfBarCri_r[["upper"]]) / 2,
  y = dfBarCri_r[["y"]] + 0.01,
  l = dfBarCri_r[["group"]]
)
dfPoint <- tibble(
  x = c(ciMA[3], ciH1[3]),
  y = dfBarCri_r$y
)

height <- 0.1
lineSizeH0 <- 1.5
lineSizeH1 <- 1.25
pointSize  <- 4
points <- geom_point(data = dfPoint, aes(x = x, y = y), size = pointSize)
lineH1 <- geom_line(data = dfLineH0,   aes(x = x, y = y), size = lineSizeH0, color = "grey60")
lineH0 <- geom_line(data = dfLineH1_r, aes(x = x, y = y), size = lineSizeH1)
CRIBar <- geom_errorbarh(data = dfBarCri_r, aes(xmin = lower, xmax = upper, y = y, group = group),#, color = group),
						 height = height, size = lineSizeH1)
textH0  <- geom_text(data = dfTxtH0, aes(x = x, y = y, label = label), nudge_x = -.3, size = 6, parse = FALSE)
textCRI <- geom_text(data = dfBarCri_r, aes(x = x, y = y, label = group), nudge_y = .05, size = 10)


graph <- ggplot(dfLineH0) +
	lineH1 + lineH0 + CRIBar + textCRI + points +
  labs(x = expression(Population~effect~ delta), y = "Scaled Density", color = NULL) +
  # labs(x = "Population effect $\\delta$", y = NULL, color = NULL) +
	scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0, 1.0)) +
  geom_rangeframe() + myTheme(base_size = 32)

graph

tb <- as.data.frame(rbind(c(ciMA, upMA[1]), c(ciH1, upH1[1])))
colnames(tb) <- c("Lower", "Upper", "mean", "ph0")
# write.csv(tb, file = "tables/posteriorProbH0.csv", quote = FALSE)
# saveFigure(graph, filename = "spikeAndSlabPosteriorRescaledPosteriorMode.pdf", width = 14, height = 7)
# optionally, save the plot as tikz so that we can use \mathcal in the labels
# saveFigure(graph, filename = "spikeAndSlabPosteriorRescaledPosteriorMode.tikz", width = 10, height = 4)

=======
rm(list = ls())
library(ggplot2)
library(tibble)
source("R/functions.R")
source("R/ggplotTheme.R")

priorPH0 <- 0.5
sigmaSlab <- 1
groupNames <- c("Spike and Slab", "Slab Only")

dat <- readRDS("data/simulatedDataset.rds")
n <- nrow(dat)

priorPH1 <- 1 - priorPH0
# ybar <- cohen.d.default(dat[["x"]], dat[["y"]], paired = TRUE, noncentral = FALSE)
ybar <- getCohenD(dat[["x"]], dat[["y"]])[1]
# diffScore <- dat[["d"]]
# ybar <- sqrt(2) * mean(diffScore) / sd(diffScore)

upMA <- updatePar(priorPH0, sigmaSlab, n, ybar)
ciMA <- postStat(upMA)
upH1 <- updatePar(0.0, sigmaSlab, n, ybar)
ciH1 <- postStat(upH1)
postH0 <- 1 - upMA[1]

xgrid <- seq(-0.5, 1.0, length.out = 1e4)
yvals <- dnorm(xgrid, upMA[2], upMA[3])#dPosteriorH1(xgrid, ybar, n, upMA[3])
criMA <- tibble(lower = ciMA[1], upper = ciMA[2]) # modelAveragedCRI(priorPH0, cdfH1 = pPosteriorH1, obsDelta = ybar, n = n, sigmaSlab = sigmaSlab)
criH1 <- tibble(lower = ciH1[1], upper = ciH1[2]) #modelAveragedCRI(0.0, cdfH1 = pPosteriorH1, obsDelta = obsDelta, n = n, sigmaSlab = sigmaSlab)

modeMA <- xgrid[which.max(yvals)]

dfLineH1 <- tibble(x = xgrid, y = yvals)
dfLineH0 <- tibble(x = c(0, 0), y = c(0, priorPH0))

# labelH0 <- paste("p(H[0]~'|'~data) ==", formatC(upMA[1]))
labelH0 <- paste("$p(\\mathcal{H}_0 \\mid \\mathrm{data} = ", formatC(upMA[1], digits = 3), ")$")
dfTxtH0  <- add_column(dfLineH0[-1, ], label = labelH0)

dfBarCriMA <- add_column(criMA, y = 1.15 * max(yvals, priorPH0))
dfBarCriH1 <- add_column(criH1, y = 1.25 * max(yvals, priorPH0))
dfBarCri   <- add_column(rbind(dfBarCriMA, dfBarCriH1), group = groupNames)

# rescale posterior mode to PH1
dfLineH1_r <- dfLineH1
dfLineH1_r$y <- dfLineH1_r$y / max(dfLineH1_r$y) * (1 - upMA[1])
dfLineH0$y <- c(0, upMA[1])
dfTxtH0$y  <- upMA[1]
dfBarCri_r <- dfBarCri
dfBarCri_r$y <- c(0.725, 0.9)
height <- mceiling(dfBarCri_r$y[1] - max(dfLineH1_r$y), .1)
dfBarCri_r$x <- (dfBarCri_r[["lower"]] + dfBarCri_r[["upper"]]) / 2
dfTxtCRI <- tibble(
  x = (dfBarCri_r[["lower"]] + dfBarCri_r[["upper"]]) / 2,
  y = dfBarCri_r[["y"]] + 0.01,
  l = dfBarCri_r[["group"]]
)
dfPoint <- tibble(
  x = c(ciMA[3], ciH1[3]),
  y = dfBarCri_r$y
)

height <- 0.1
lineSizeH0 <- 1.5
lineSizeH1 <- 1.25
pointSize  <- 4
points <- geom_point(data = dfPoint, aes(x = x, y = y), size = pointSize)
lineH1 <- geom_line(data = dfLineH0,   aes(x = x, y = y), size = lineSizeH0, color = "grey60")
lineH0 <- geom_line(data = dfLineH1_r, aes(x = x, y = y), size = lineSizeH1)
CRIBar <- geom_errorbarh(data = dfBarCri_r, aes(xmin = lower, xmax = upper, y = y, group = group),#, color = group),
						 height = height, size = lineSizeH1)
textH0  <- geom_text(data = dfTxtH0, aes(x = x, y = y, label = label), nudge_x = -.3, size = 6, parse = FALSE)
textCRI <- geom_text(data = dfBarCri_r, aes(x = x, y = y, label = group), nudge_y = .05, size = 10)


graph <- ggplot(dfLineH0) +
	lineH1 + lineH0 + CRIBar + textCRI + points +
  labs(x = expression(Population~effect~ delta), y = "Scaled Density", color = NULL) +
  # labs(x = "Population effect $\\delta$", y = NULL, color = NULL) +
	scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0, 1.0)) +
  geom_rangeframe() + myTheme(base_size = 32)

graph

tb <- as.data.frame(rbind(c(ciMA, upMA[1], 1 - upMA[1], modeMA), c(ciH1, upH1[1], 1 - upH1[1], NA)))
colnames(tb) <- c("Lower", "Upper", "mean", "ph0", "ph1", "mode")
write.csv(tb, file = "tables/posteriorProbH0.csv", quote = FALSE)
# saveFigure(graph, filename = "spikeAndSlabPosteriorRescaledPosteriorMode.pdf", width = 14, height = 7)
# optionally, save the plot as tikz so that we can use \mathcal in the labels
# saveFigure(graph, filename = "spikeAndSlabPosteriorRescaledPosteriorMode.tikz", width = 10, height = 4)
>>>>>>> e7d736cc7f84b7bc2a07ba785cbdbc7c3ad021bd
