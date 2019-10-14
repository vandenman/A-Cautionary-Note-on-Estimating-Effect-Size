rm(list = ls())
# library(colorspace)
library(ggplot2)
library(tibble)
library(assertthat)
source("R/functions.R")

makeFigure3 <- function(tib, tibRibbon = NULL) {

	g <- ggplot(data = tib, mapping = aes(x = d, y = e, group = sigma, linetype = sigma, color = sigma)) +
		geom_abline(size = 1.2, color = "gray60") +
		geom_line(size = 1.2) +
		scale_color_manual(values = getColors(3)) +
		# scale_color_viridis_d() +
		labs(x = "Observed effect, d", y = expression(hat(delta)), linetype = expression(sigma), color = expression(sigma)) +
		# labs(x = expression(delta), y = expression(hat(delta)), linetype = expression(sigma), color = expression(sigma)) +
		theme_bw(24) +
		theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

	if ("ss" %in% colnames(tib))
		g <- g +
			facet_grid(~ss) +
			theme(strip.background = element_rect(fill = "transparent", color = "transparent"),
				  legend.position = c(.075, .85),
				  legend.background = element_rect(fill = "transparent", color = "transparent")
			)
	if (!is.null(tibRibbon))
	  g <- g + geom_ribbon(data = tibRibbon, aes(x = d, ymin = l, ymax = u), inherit.aes = FALSE,
	                       alpha = .35, fill = getColors(3)[2])
	return(g)
}

dTrue <- seq(-1, 1, .005)
N <- 40

sigmas <- c(.5, 1, 10)


tib1 <- tibble(
	d = rep(dTrue, length(sigmas)),
	e = c(sapply(sigmas, function(s) updateEV(0, s^2, N, dTrue))),
	sigma = factor(rep(sigmas, each = length(dTrue)))
)

tib2 <- tibble(
	d = rep(dTrue, length(sigmas)),
	e = c(sapply(sigmas, function(s) updateEV(0.5, s^2, N, dTrue))),
	sigma = factor(rep(sigmas, each = length(dTrue)))
)

tib12 <- rbind(tib1, tib2)
tib12$ss <- rep(c("Slab Only", "Spike and Slab"), each = nrow(tib1))

g1 <- makeFigure3(tib1)
g2 <- makeFigure3(tib2)
g12 <- makeFigure3(tib12)
# g12
saveFigure(g12, filename = "posteriorMeanVsSampleDelta.pdf", width = 10, height = 7)


statS <- sapply(dTrue, function(obs) postStat(updatePar(.5, 1, N, obs)))
tib3a <- tibble(
	d = dTrue,
	e = statS[3, ],
	sigma = 1,
  ss = "Model Averaged Uncertainty"
)
tib3b <- tibble(
  d = dTrue,
  l = statS[1, ],
	u = statS[2, ],
  sigma = 1,
  ss = "Model Averaged Uncertainty"
)
tib123 <- rbind(tib12, tib3a)
tib123[["ss"]] <- factor(rep(1:3, c(nrow(tib1), nrow(tib2), nrow(tib3a))), labels = unique(tib123$ss))
tib3b[["ss"]] <- factor(tib3b[["ss"]])
g123 <- makeFigure3(tib123, tib3b)
g123
saveFigure(g123, filename = "posteriorMeanVsSampleDelta3panel.pdf", width = 15, height = 7)
