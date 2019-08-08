rm(list = ls())
# library(colorspace)
library(ggplot2)
library(tibble)
library(assertthat)
source("R/functions.R")

makeFigure3 <- function(tib) {

	g <- ggplot(data = tib, mapping = aes(x = d, y = e, group = sigma, linetype = sigma, color = sigma)) +
		geom_abline(size = 1.2, color = "gray60") +
		geom_line(size = 1.2) +
		scale_color_viridis_d() +
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
