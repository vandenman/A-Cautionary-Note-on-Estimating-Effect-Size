rm(list = ls())

library(colorspace)
library(ggplot2)
library(tibble)
source("R/functions.R")

xgrid <- seq(-5, 5, length.out = 1e4)
pH0 <- c(0, .35, .7)

tib1 <- tibble(
	x = rep(xgrid, length(pH0)),
	y = unlist(lapply(pH0, function(p, x) modelAveragedCdf(x, p), x = xgrid)),
	g = factor(rep(pH0, each = length(xgrid)))
)

coordsCRI <- do.call(rbind, lapply(pH0, modelAveragedCRI))
coords <- tibble(x = rep(unlist(coordsCRI), each = 2),
	   y = c(rep(c(0, 0.025), length(pH0)), rep(c(0, 0.975), length(pH0))),
	   group = factor(rep(1:(2*length(pH0)), each = 2)),
	   color = rep(factor(rep(pH0, each = 2)), 2)
)

ggplot(data = tib1, mapping = aes(x = x, y = y, group = g, color = g, linetype = g)) +
	geom_line(mapping = aes(x = x, y = y, group = group, color = color),#, linetype = color),
			  data = coords, inherit.aes = FALSE, alpha = 0.55) +
	geom_line() +
	labs(x = expression(delta), y = expression(P(delta)), color = expression(P(H[0])), linetype = expression(P(H[0]))) +
	scale_color_discrete_qualitative() +
	theme_bw(24) +
	theme(axis.title.y = element_text(angle = 0, vjust = 0.5))


