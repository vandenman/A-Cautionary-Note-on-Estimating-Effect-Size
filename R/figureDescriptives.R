rm(list = ls())

library(readr)
library(ggplot2)
library(dplyr)
library(compute.es)

dat <- read_csv("data/Ackerman et al. (2010) - Replicaiton Data .csv")
dim(dat) # should be 641 27!

dat <- dat %>%
	filter(`filter_$` == 1)
# replicate results on osf
org <- t.test(DV ~ Condition, data = dat)
ns <- table(dat[["Condition"]])
effectSize <- tes(t = org[["statistic"]], n.1 = ns[1], n.2 = ns[2], level = 95, verbose = FALSE)
effectSize[1:7]

graph1 <- ggplot(data = dat, aes(x = Condition, y = DV)) +
	geom_boxplot(outlier.alpha = 0) +
	ggbeeswarm::geom_quasirandom(color = "grey60", alpha = .8) +
	theme_bw(24)

graph2 <- ggplot(data = effectSize, aes(ymin = l.d, ymax = u.d, x = factor(0), y = d)) +
	geom_point(size = 3) +
	geom_errorbar(width = .2) +
	scale_x_discrete(name = NULL, labels = "Replications of Ackerman et al. (2010)") +
	labs(x = NULL, y = "Effect size, d") +
	theme_bw(24)

graph1
graph2

RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)
