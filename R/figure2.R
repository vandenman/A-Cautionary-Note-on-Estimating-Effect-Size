rm(list = ls())
library(assertthat)
library(ggplot2)
library(tibble)
library(effsize)
source("R/functions.R")
source("R/ggplotTheme.R")
mceiling <- function(x, base) base * ceiling(x / base)
mround   <- function(x, base) base * round  (x / base)

priorPH0 <- 0.5
priorPH1 <- 1 - priorPH0
sigmaSlab <- 1
dat <- readRDS("data/simulatedDataset.rds")

n <- nrow(dat)
# cohenD <- cohen.d.default(dat[["x"]], dat[["y"]], paired = TRUE, noncentral = TRUE)
diffScore = dat[["d"]]
ybar <- mean(diffScore) / sd(diffScore)

upMA <- updatePar(priorPH0, sigmaSlab, n, ybar)
ciMA <- postStat(upMA)
upH1 <- updatePar(0.0, sigmaSlab, n, ybar)
ciH1 <- postStat(upH1)

xgrid <- seq(-0.5, 1.5, length.out = 1e4)
yvals <- dnorm(xgrid, upMA[2], upMA[3])#dPosteriorH1(xgrid, ybar, n, upMA[3])
criMA <- tibble(lower = ciMA[1], upper = ciMA[2]) # modelAveragedCRI(priorPH0, cdfH1 = pPosteriorH1, obsDelta = ybar, n = n, sigmaSlab = sigmaSlab)
criH1 <- tibble(lower = ciH1[1], upper = ciH1[2]) #modelAveragedCRI(0.0, cdfH1 = pPosteriorH1, obsDelta = obsDelta, n = n, sigmaSlab = sigmaSlab)

dfLineH1 <- tibble(x = xgrid, y = yvals)
dfLineH0 <- tibble(x = c(0, 0), y = c(0, priorPH0))
dfTxtH0  <- add_column(dfLineH0[-1, ], label = paste("p(H[0]~'|'~data) ==", formatC(upMA[1])))

dfBarCriMA <- add_column(criMA, y = 1.15 * max(yvals, priorPH0))
dfBarCriH1 <- add_column(criH1, y = 1.25 * max(yvals, priorPH0))
dfBarCri   <- add_column(rbind(dfBarCriMA, dfBarCriH1), group = c("Spike and Slab", "Slab Only"))

lineSizeH0 <- 1.5
lineSizeH1 <- 1.25
lineH1 <- geom_line(data = dfLineH0, aes(x = x, y = y), size = lineSizeH0, color = "grey60")
lineH0 <- geom_line(data = dfLineH1, aes(x = x, y = y), size = lineSizeH1)
CRIBar <- geom_errorbarh(data = dfBarCri, aes(xmin = lower, xmax = upper, y = y, group = group, color = group),
						 height = .1 * dfBarCriMA$y, size = lineSizeH1)
textH0 <- geom_text(data = dfTxtH0, aes(x = x, y = y, label = label), nudge_x = -.3, size = 8, parse = TRUE)

by <- 0.5
maxYLeft <- mceiling(1.25 * max(yvals, priorPH0), by)

graph1 <- ggplot() +
	lineH1 + lineH0 + CRIBar + textH0 +
	scale_color_manual(values = getColors(2)) +
	# scale_color_viridis_d() +
	ggrepel::geom_text_repel() +
	labs(x = expression(paste("Population effect, ", delta)), y = "Density", color = NULL) +
	scale_y_continuous(breaks = seq(0, maxYLeft, by), limits = c(0, maxYLeft)) +
	theme_bw(24) +
	theme(
		legend.position   = c(.15, .9),
		legend.background = element_rect(fill = "transparent", color = "transparent")
	)

# same thing with an axis on the right for probability
dfLineH0$y <- dfLineH0$y * maxYLeft
dfTxtH0$y  <- dfTxtH0$y  * maxYLeft

lineSizeH0 <- 1.5
lineSizeH1 <- 1.25
lineH1 <- geom_line(data = dfLineH0, aes(x = x, y = y), size = lineSizeH0, color = "grey60")
lineH0 <- geom_line(data = dfLineH1, aes(x = x, y = y), size = lineSizeH1)
CRIBar <- geom_errorbarh(data = dfBarCri, aes(xmin = lower, xmax = upper, y = y, group = group, color = group),
						 height = .1 * dfBarCriMA$y, size = lineSizeH1)
textH0 <- geom_text(data = dfTxtH0, aes(x = x, y = y, label = label), nudge_x = -.3, size = 8, parse = TRUE)

graph2 <- ggplot() +
	lineH1 + lineH0 + CRIBar + textH0 +
	scale_color_manual(values = getColors(2)) +
	# scale_color_viridis_d() +
	ggrepel::geom_text_repel() +
	labs(x = expression(paste("Population effect, ", delta)), y = "Density", color = NULL) +
	scale_y_continuous(breaks = seq(0, maxYLeft, by), limits = c(0, maxYLeft), sec.axis = sec_axis(
		name = "Probabilty",
		trans = function(x) x / maxYLeft, breaks = seq(0, 1, .25)
	)) +
	theme_bw(24) +
	theme(
		legend.position   = c(.15, .9),
		legend.background = element_rect(fill = "transparent", color = "transparent")
	)

# rescale posterior mode to PH1
dfLineH1_r <- dfLineH1
dfLineH1_r$y <- dfLineH1_r$y / max(dfLineH1_r$y) * (1 - upMA[1])
dfLineH0$y <- c(0, upMA[1])
dfTxtH0$y  <- upMA[1]
dfBarCri_r <- dfBarCri
dfBarCri_r$y <- c(0.8, 0.9)
height <- mround(dfBarCri_r$y[1] - max(dfLineH1_r$y), .1)


lineSizeH0 <- 1.5
lineSizeH1 <- 1.25
lineH1 <- geom_line(data = dfLineH0,   aes(x = x, y = y), size = lineSizeH0, color = "grey60")
lineH0 <- geom_line(data = dfLineH1_r, aes(x = x, y = y), size = lineSizeH1)
CRIBar <- geom_errorbarh(data = dfBarCri_r, aes(xmin = lower, xmax = upper, y = y, group = group, color = group),
						 height = height, size = lineSizeH1)
textH0 <- geom_text(data = dfTxtH0, aes(x = x, y = y, label = label), nudge_x = -.3, size = 8, parse = TRUE)

graph3 <- ggplot(dfLineH0) +
	lineH1 + lineH0 + CRIBar + textH0 +
	scale_color_manual(values = getColors(2)) +
	# scale_color_viridis_d() +
	# ggrepel::geom_text_repel() +
	labs(x = expression(paste("Population effect, ", delta)), y = NULL, color = NULL) +
	scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0, 1.0)) +
	# theme_bw(24) +
	# theme(
	# 	legend.position   = c(.15, .9),
	# 	legend.background = element_rect(fill = "transparent", color = "transparent")
	# ) +
  geom_rangeframe() + myTheme()

graph1
graph2
graph3
# saveFigure(graph1, filename = "spikeAndSlabPosterior.pdf", width = 10, height = 7)
# saveFigure(graph2, filename = "spikeAndSlabPosteriorRightAxis.pdf", width = 10, height = 7)
# saveFigure(graph3, filename = "spikeAndSlabPosteriorRescaledPosteriorMode.pdf", width = 10, height = 7)
