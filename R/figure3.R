rm(list = ls())
# library(colorspace)
library(ggplot2)
library(tibble)
library(assertthat)
source(file.path("R", "functions.R"))
source(file.path("R", "ggplotTheme.R"))

makeFigure3 <- function(tib, tibRibbon = NULL, base_size = 24, gridVsWrap = "wrap", scales = "free", addHline = TRUE,
                        dfAnnotate = NULL, legend.position = "none", legendTitle = expression(sigma),
                        nrow = NULL, ncol = NULL, noFacet = FALSE) {

  multiPanel <- "ss" %in% colnames(tib)
  mapping <- if (multiPanel && scales == "fixed")
    aes(x = d, y = e, group = sigma, linetype = sigma, color = sigma, sides = sides)
  else aes(x = d, y = e, group = sigma, linetype = sigma, color = sigma)

  hline <- if (addHline) geom_hline(yintercept = 0, linetype = 8, col = "gray60") else NULL

  g <- ggplot(data = tib, mapping = mapping) +
    hline + geom_abline(size = 1.2, color = "gray60") +
    geom_line(size = 1.2) +
    scale_color_manual(values = getColors(length(unique(tib$sigma)))) +
    # scale_color_viridis_d() +
    labs(x = "Observed effect, d", y = expression(hat(delta)), linetype = legendTitle, color = legendTitle) +
    myTheme(base_size = base_size, legend.position = legend.position) +
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
    geom_rangeframe()

  if (!is.null(dfAnnotate))
    g <- g + geom_segment(inherit.aes = FALSE, data = dfAnnotate, mapping = aes(x = x, xend = xend, y = y, yend = yend))

  if ("ss" %in% colnames(tib) && !noFacet) {
    g <- g +
      if (gridVsWrap == "grid") facet_grid(~ss, scales = scales)
      else                      facet_wrap(~ss, scales = scales, nrow = nrow, ncol = ncol)
  }

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
  sigma = factor(rep(sigmas, each = length(dTrue))),
  sides = "bl"
)

tib2 <- tibble(
  d = rep(dTrue, length(sigmas)),
  e = c(sapply(sigmas, function(s) updateEV(0.5, s^2, N, dTrue))),
  sigma = factor(rep(sigmas, each = length(dTrue))),
  sides = "b"
)

tib12 <- rbind(tib1, tib2)
tib12$ss <- rep(c("Slab Only", "Spike and Slab"), each = nrow(tib1))

g1 <- makeFigure3(tib1)
g2 <- makeFigure3(tib2)
figure_3a <- makeFigure3(tib12, legend.position = "right")
# figure_3a

# the same but with a sample size of 100
dTrue <- seq(-1, 1, .005)
N <- 100

sigmas <- c(.5, 1, 10)


tib1b <- tibble(
  d = rep(dTrue, length(sigmas)),
  e = c(sapply(sigmas, function(s) updateEV(0, s^2, N, dTrue))),
  sigma = factor(rep(sigmas, each = length(dTrue))),
  sides = "bl"
)

tib2b <- tibble(
  d = rep(dTrue, length(sigmas)),
  e = c(sapply(sigmas, function(s) updateEV(0.5, s^2, N, dTrue))),
  sigma = factor(rep(sigmas, each = length(dTrue))),
  sides = "b"
)
tib12b <- rbind(tib1b, tib2b)
tib12b$ss <- rep(c("Slab Only", "Spike and Slab"), each = nrow(tib1))


g1b  <- makeFigure3(tib1b)
g2b  <- makeFigure3(tib2b)
figure_3b <- makeFigure3(tib12b, legend.position = "right")
# figure_3b

# i0 <- g2b$data$d > 0
# i1 <- g2b$data$d - g2b$data$e < 0.01
# i2 <- g2b$data$d > 0.1
# i3 <- which(i0 & i1 & i2)[1L]
# g2b$data$d[i3]
#
# dfAnnotate <- data.frame(x = g2b$data$d[i3], xend = g2b$data$d[i3],
#                          y = 0, yend = g2b$data$e[i3], ss = "Spike and Slab")
#
# makeFigure3(tib12b, dfAnnotate = dfAnnotate)

# as a function of p(H0)
dTrue <- seq(-1, 1, .005)
N <- 40
ph0s <- seq(.1, .9, .2)
tib1c <- tibble(
  d = rep(dTrue, length(ph0s)),
  e = c(sapply(ph0s, function(ph0) updateEV(ph0, 1, N, dTrue))),
  sigma = factor(rep(ph0s, each = length(dTrue))), # reuse sigma as P(H0)
  sides = "bl"
)
# figure_4 <- makeFigure3(tib1c, legend.position = "right", legendTitle = expression(p(H[0]))) + theme(legend.justification = "center")
figure_4 <- makeFigure3(tib1c, legend.position = "right", legendTitle = expression(p(spike))) + theme(legend.justification = "center")
# figure_4 <- makeFigure3(tib1c, legend.position = c(.1, 1), legendTitle = expression(p(H[0])))

tib12d  <- tib12
tib12bd <- tib12b

tib1d <- rbind(tib12d, tib12bd)
tib1d$sides <- rep(c("l", "", "bl", "b"), each = 3*length(dTrue))
tib1d$N <- factor(rep(c("N = 40", "N = 100"), c(nrow(tib12d), nrow(tib12bd))), levels = c("N = 40", "N = 100"))
tib1d$method <- tib1d$ss
# tib1d$ss <- NULL

tapply(tib1d$sides, list(tib1d$method, tib1d$N), unique)

figure_5 <- makeFigure3(tib1d, legend.position = "right") + lemon::facet_rep_grid(N ~ method) + theme(legend.justification = "center")


saveFigure(figure_3a, filename = "posteriorMeanVsSampleDelta_sigma_n_40.pdf", width = 14, height = 7)
saveFigure(figure_3b, filename = "posteriorMeanVsSampleDelta_sigma_n_100.pdf", width = 14, height = 7)
saveFigure(figure_4,  filename = "posteriorMeanVsSampleDelta_ph0_n_40.pdf", width = 7, height = 7)
saveFigure(figure_5,  filename = "posteriorMeanVsSampleDelta_4_panel.pdf", width = 14, height = 14)
# makeFigure3(tib12, gridVsWrap = "wrap", scales = "fixed")

# saveFigure(g12, filename = "posteriorMeanVsSampleDelta.pdf", width = 10, height = 7)
#
#
# statS <- sapply(dTrue, function(obs) postStat(updatePar(.5, 1, N, obs)))
# tib3a <- tibble(
#   d = dTrue,
#   e = statS[3, ],
#   sigma = 1,
#   ss = "Model Averaged Uncertainty"
# )
# tib3b <- tibble(
#   d = dTrue,
#   l = statS[1, ],
#   u = statS[2, ],
#   sigma = 1,
#   ss = "Model Averaged Uncertainty"
# )
# tib123 <- rbind(tib12, tib3a)
# tib123[["ss"]] <- factor(rep(1:3, c(nrow(tib1), nrow(tib2), nrow(tib3a))), labels = unique(tib123$ss))
# tib3b[["ss"]] <- factor(tib3b[["ss"]])
# g123 <- makeFigure3(tib123, tib3b)
# g123
# saveFigure(g123, filename = "posteriorMeanVsSampleDelta3panel.pdf", width = 15, height = 7)