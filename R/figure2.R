rm(list = ls())
library(assertthat)
library(ggplot2)
library(tibble)
source("R/functions.R")

pH0 <- 0.5
pH1 <- 1 - pH0

n <- 10
obsDelta <- .5
sigmaSlab <- 1

xgrid <- seq(-1, 1, length.out = 1e4)
yvals <- dPosteriorH1(xgrid, obsDelta, n, sigmaSlab)
criMA <- modelAveragedCRI(pH0, cdfH1 = pPosteriorH1, obsDelta = obsDelta, n = n, sigmaSlab = sigmaSlab)
criH1 <- modelAveragedCRI(0.0, cdfH1 = pPosteriorH1, obsDelta = obsDelta, n = n, sigmaSlab = sigmaSlab)

dfLineH1 <- tibble(x = xgrid, y = yvals)
dfLineH0 <- tibble(x = c(0, 0), y = c(0, pH0))
dfTxtH0  <- add_column(dfLineH0[-1, ], label = paste("p(H[0]~'|'~X) ==", formatC(pH0)))

dfBarCriMA <- add_column(criMA, y = 1.15 * max(yvals, pH0))
dfBarCriH1 <- add_column(criH1, y = 1.25 * max(yvals, pH0))
dfBarCri   <- add_column(rbind(dfBarCriMA, dfBarCriH1), group = c("Spike and Slab", "Slab Only"))

lineSizeH0 <- 1.5
lineSizeH1 <- 1.25
lineH1 <- geom_line(data = dfLineH0, aes(x = x, y = y), size = lineSizeH0, color = "grey60")
lineH0 <- geom_line(data = dfLineH1, aes(x = x, y = y), size = lineSizeH1)
CRIBar <- geom_errorbarh(data = dfBarCri, aes(xmin = lower, xmax = upper, y = y, group = group, color = group),
						 height = .1 * dfBarCriMA$y, size = lineSizeH1)
textH0 <- geom_text(data = dfTxtH0, aes(x = x, y = y, label = label), nudge_x = -.3, size = 8, parse = TRUE)

mceiling <- function(x, base) base * ceiling(x / base)
maxYLeft <- mceiling(1.25 * max(yvals, pH0), 0.2)

graph <- ggplot() +
	lineH1 + lineH0 + CRIBar + textH0 +
	scale_color_viridis_d() +
	ggrepel::geom_text_repel() +
	labs(x = expression(paste("Population effect, ", delta)), y = "Density", color = NULL) +
	scale_y_continuous(breaks = seq(0, maxYLeft, .2), limits = c(0, maxYLeft)) +
	theme_bw(24) +
	theme(
		legend.position   = c(.15, .9),
		legend.background = element_rect(fill = "transparent", color = "transparent")
	)
graph
# saveFigure(graph, filename = "spikeAndSlabPosterior.pdf", width = 10, height = 7)

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
	scale_color_viridis_d() +
	ggrepel::geom_text_repel() +
	labs(x = expression(paste("Population effect, ", delta)), y = "Density", color = NULL) +
	scale_y_continuous(breaks = seq(0, maxYLeft, .2), limits = c(0, maxYLeft), sec.axis = sec_axis(
		name = "Probabilty",
		trans = function(x) x / maxYLeft, breaks = seq(0, 1, .25)
	)) +
	theme_bw(24) +
	theme(
		legend.position   = c(.15, .9),
		legend.background = element_rect(fill = "transparent", color = "transparent")
	)
graph2
# saveFigure(graph2, filename = "spikeAndSlabPosteriorRightAxis.pdf", width = 10, height = 7)

# rescale posterior mode to PH1
dfLineH1_r <- dfLineH1
dfLineH1_r$y <- dfLineH1_r$y / max(dfLineH1_r$y) * pH1
dfLineH0$y <- c(0, pH0)
dfTxtH0$y  <- pH0


lineSizeH0 <- 1.5
lineSizeH1 <- 1.25
lineH1 <- geom_line(data = dfLineH0,   aes(x = x, y = y), size = lineSizeH0, color = "grey60")
lineH0 <- geom_line(data = dfLineH1_r, aes(x = x, y = y), size = lineSizeH1)
CRIBar <- geom_errorbarh(data = dfBarCri, aes(xmin = lower, xmax = upper, y = y, group = group, color = group),
						 height = .1 * dfBarCriMA$y, size = lineSizeH1)
textH0 <- geom_text(data = dfTxtH0, aes(x = x, y = y, label = label), nudge_x = -.3, size = 8, parse = TRUE)

graph3 <- ggplot() +
	lineH1 + lineH0 + CRIBar + textH0 +
	scale_color_viridis_d() +
	ggrepel::geom_text_repel() +
	labs(x = expression(paste("Population effect, ", delta)), y = NULL, color = NULL) +
	scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0, 1)) +
	theme_bw(24) +
	theme(
		legend.position   = c(.15, .9),
		legend.background = element_rect(fill = "transparent", color = "transparent")
	)
graph3
# saveFigure(graph3, filename = "spikeAndSlabPosteriorRescaledPosteriorMode.pdf", width = 10, height = 7)
