rm(list = ls())

library(ggplot2)
library(tibble)
library(BayesFactor)
library(papaja)
source(file.path("R", "functions.R"))
source(file.path("R", "ggplotTheme.R"))

getRobustnessData <- function(priorPH0_vals, ybar, n, sigmaSlab = 1) {
  mat <- matrix(NA, length(priorPH0_vals), 5, dimnames = list(NULL, c("ph0", "ph0|data", "lower", "upper", "ma")))

  for (i in seq_along(priorPH0_vals)) {
    ph0 <- priorPH0_vals[i]
    upMA <- updatePar(ph0, sigmaSlab, n, ybar[1L])
    ciMA <- postStat(upMA)
    mat[i, ] <- c(ph0, upMA[1], ciMA)
  }
  return(as_tibble(mat))
}

getRobustnessPlot <- function(priorPH0_vals, ybar, n, sigmaSlab = 1) {


  mat <- matrix(NA, length(priorPH0_vals), 5, dimnames = list(NULL, c("ph0", "ph0|data", "lower", "upper", "ma")))

  for (i in seq_along(priorPH0_vals)) {
    ph0 <- priorPH0_vals[i]
    upMA <- updatePar(ph0, sigmaSlab, n, ybar[1L])
    ciMA <- postStat(upMA)
    mat[i, ] <- c(ph0, upMA[1], ciMA)
  }
  df <- as_tibble(mat)

  df_hline <- tibble(x = range(priorPH0_vals), y = rep(upMA[2], 2))

  gImpl <- ggplot(data = df, aes(x = ph0, y = ma, ymin = lower, ymax = upper)) +
    geom_line(data = df_hline, aes(x = x, y = y), inherit.aes = FALSE, show.legend = TRUE, linetype = "longdash") +
    geom_line() +
    geom_point(size = 2) +
    geom_ribbon(alpha = .2) +
    scale_x_continuous(name = "p(spike)", breaks = seq(.05, .95, length.out = 7)) +
    labs(y = "Model averaged estimate") +
    geom_rangeframe(sides = "bl") +
    myTheme(legend.position = "right")

}

datExpl <- readRDS(file.path("data", "twoMinds.rds"))   # Expl = Explicit
datImpl <- readRDS(file.path("data", "twoMinds_2.rds")) # Impl = Implicit

# replication ----

# functions from Heycke et al., 2018.

#scientific notation for print BayesFactor functions below
as.scientific <- function(number){
  num <- format(number, scientific = TRUE, digits = 3)
  p <- as.integer(gsub(".*e+", "", num))
  b <- as.numeric(gsub( "e+.*", "", num))
  paste0('\\mathit{', b, '}', '\\times', '{',10, '}', '^', '{', p, '}')
}

#print Bayes factor ANOVA outputs
printBF <- function(BF, Hypothesis = 1, index = 1, OutputSize = 99999.99, HStyle = 0){
  if(Hypothesis == "1" & as.vector(BF[index]) >= 1) return("\\linebreak  __BayesFactor larger 1, but Output for H1 selected__ \\linebreak ")
  if(Hypothesis == "0" & as.vector(BF[index]) < 1 & HStyle == 0) return("\\linebreak  __BayesFactor smaller 1, but Output for H0 selected__ \\linebreak ")
  if(Hypothesis == "0") return(ifelse(as.vector(BF[index])>OutputSize, paste0('$\\mathit{BF}_{01} = ', as.scientific(as.vector(BF[index])), '$'), paste0('$\\mathit{BF}_{01} = ', printnum(as.vector(BF[index])), '$')))
  if(Hypothesis == "1") return(ifelse(1/as.vector(BF[index])>OutputSize, paste0('$\\mathit{BF}_{10} = ', as.scientific(1/as.vector(BF[index])), '$'), paste0('$\\mathit{BF}_{10} = ', printnum(1/as.vector(BF[index])), '$')))
}

#t test print function
printBFt <- function(BF, HStyle = 0, index = 1, OutputSize = 99999.99 , postit = 100000){
  if(as.vector(BF[index]) < 1 & HStyle == 0){
    b <- 1/as.vector(BF[index])
    num <- "01"
  }else{
    b <- as.vector(BF[index])
    num <- "10"
  }
  if(as.character(class(BF@numerator[[names(BF@numerator)[index]]])) == "BFoneSample"){
    rBF <- BayesFactor::ttestBF(BF@data[,1], mu = BF@numerator[[names(BF@numerator)[index]]]@prior$mu, rscale = BF@numerator[[names(BF@numerator)[index]]]@prior$rscale)
  }
  if(as.character(class(BF@numerator[[names(BF@numerator)[1]]])) == "BFindepSample"){
    rBF <- BayesFactor::ttestBF(subset(BF@data, BF@data[,2] == "x")[,1] , subset(BF@data, BF@data[,2] == "y")[,1], rscale = BF@numerator[[names(BF@numerator)[index]]]@prior$rscale, paired = FALSE)
  }
  post <- BayesFactor::posterior(rBF, index = index, iterations = postit)
  d <- median(post[, "delta"])
  HDI <- coda::HPDinterval(post[, "delta"])
  # modified
  d_mean <- mean(post[, "delta"])
  txt <- ifelse(b > OutputSize,
         paste0('$\\mathit{BF}_{', num, '} = ', as.scientific(b), '$', ', ', '$d = ', round(d, 4), '$', ', ', '95% HDI [', printnum(HDI[1]), ', ', printnum(HDI[2]), ']', ' d_mean = ', round(d_mean, 4)),
         paste0('$\\mathit{BF}_{', num, '} = ', printnum(b),      '$', ', ', '$d = ', round(d, 4), '$', ', ', '95% HDI [', printnum(HDI[1]), ', ', printnum(HDI[2]), ']', ' d_mean = ', round(d_mean, 4))
  )
  return(list(d_mean, txt))
  # original
  # ifelse(b > OutputSize,
  #        paste0('$\\mathit{BF}_{', num, '} = ', as.scientific(b), '$', ', ', '$d = ', printnum(d), '$', ', ', '95% HDI [', printnum(HDI[1]), ', ', printnum(HDI[2]), ']', ' d_mean = ', printnum(d_mean)),
  #        paste0('$\\mathit{BF}_{', num, '} = ', printnum(b),      '$', ', ', '$d = ', printnum(d), '$', ', ', '95% HDI [', printnum(HDI[1]), ', ', printnum(HDI[2]), ']', ' d_mean = ', printnum(d_mean))
  # )
}
printBFt(VB1expltBF)


ttestpr <- sqrt(2)/2
# replicates results from Heycke et al., 2018. code copied from Manuscript_RR.Rmd
#ValenceBlock 1
VB1explt <- t.test(subset(datExpl, ValenceBlock == "1" & Block == "1")$DV,
                   subset(datExpl, ValenceBlock == "1" & Block == "2")$DV,
                   paired = TRUE)

VB1expltBF <- ttestBF(subset(datExpl, ValenceBlock == "1" & Block == "1")$DV,
                      subset(datExpl, ValenceBlock == "1" & Block == "2")$DV,
                      paired = TRUE,
                      rscale = ttestpr)


#ValenceBlock 2
VB2explt <- t.test(subset(datExpl, ValenceBlock == "2" & Block == "2")$DV,
                   subset(datExpl, ValenceBlock == "2" & Block == "1")$DV,
                   paired = TRUE)

VB2expltBF <- ttestBF(subset(datExpl, ValenceBlock == "2" & Block == "2")$DV,
                      subset(datExpl, ValenceBlock == "2" & Block == "1")$DV,
                      paired = TRUE,
                      rscale = ttestpr)

#ValenceBlock 1
VB1implt <- t.test(subset(datImpl, ValenceBlock == "1" & Block == "2")$DV,
                   subset(datImpl, ValenceBlock == "1" & Block == "1")$DV,
                   paired = TRUE)

VB1impltBF <- ttestBF(subset(datImpl, ValenceBlock == "1" & Block == "2")$DV,
                      subset(datImpl, ValenceBlock == "1" & Block == "1")$DV,
                      paired = TRUE,
                      rscale = ttestpr)


#ValenceBlock 2
VB2implt <- t.test(subset(datImpl, ValenceBlock == "2" & Block == "1")$DV,
                   subset(datImpl, ValenceBlock == "2" & Block == "2")$DV,
                   paired = TRUE)

VB2impltBF <- ttestBF(subset(datImpl, ValenceBlock == "2" & Block == "1")$DV,
                      subset(datImpl, ValenceBlock == "2" & Block == "2")$DV,
                      paired = TRUE,
                      rscale = ttestpr)

# explicit test
apa_print(VB1explt)$statistic
Heycke_expl <- printBFt(VB1expltBF)
print(Heycke_expl[[2]])

# implicit test
apa_print(VB1implt)$statistic
Heycke_impl <- printBFt(VB1impltBF)
print(Heycke_impl[[2]])

# reanalysis ----

priorPH0  <- 0.5
sigmaSlab <- 1

# explicit
datExplBlock1 <- subset(datExpl, ValenceBlock == "1" & Block == "1")$DV
datExplBlock2 <- subset(datExpl, ValenceBlock == "1" & Block == "2")$DV

ybarExpl <- getCohenD(datExplBlock1, datExplBlock2)
nExpl <- length(datExplBlock1)

upMAExpl <- updatePar(priorPH0, sigmaSlab, nExpl, ybarExpl[1L])
ciMAExpl <- postStat(upMAExpl)

tbExplicit <- data.frame(t(c(upMAExpl, ciMAExpl)))
names(tbExplicit) <- c("ph0", "mu1", "sd1", "Lower", "Upper", "modelAveraged")

# implicit
datImplBlock1 <- subset(datImpl, ValenceBlock == "1" & Block == "1")$DV
datImplBlock2 <- subset(datImpl, ValenceBlock == "1" & Block == "2")$DV

ybarImpl <- getCohenD(datImplBlock2, datImplBlock1)
nImpl <- length(datImplBlock1)

upMAImpl <- updatePar(priorPH0, sigmaSlab, nImpl, ybarImpl[1L])
ciMAImpl <- postStat(upMAImpl)

tbImplicit <- data.frame(t(c(upMAImpl, ciMAImpl)))
names(tbImplicit) <- c("ph0", "mu1", "sd1", "Lower", "Upper", "modelAveraged")

tbBoth <- rbind(tbExplicit, tbImplicit)
tbBoth$ph1 <- 1 - tbBoth$ph0
# tbBoth

# writeTable(tbBoth, file = "reanalysis.csv")

# robustness plot

priorPH0_vals <- seq(.05, .95, .025)
robExpl <- getRobustnessData(priorPH0_vals, ybarExpl[1L], nExpl)
robImpl <- getRobustnessData(priorPH0_vals, ybarImpl[1L], nImpl)
robExpl$analysis <- "Explicit Evaluation"
robImpl$analysis <- "Implicit Evaluation"

robustnessData <- rbind(robExpl, robImpl)
robustnessData$sides <- "bl"

hlineDf <- tibble(
  x = rep(range(priorPH0_vals), 2),
  y = c(rep(upMAExpl[2], 2), rep(upMAImpl[2], 2)),
  analysis = rep(c("Explicit Evaluation", "Implicit Evaluation"), each = 2)
)

breaksSelector <- function(limits, n) {
  if (all(limits > 0))
    return(seq(1.5, 2.5, .5))
  else
    return(seq(0, -.8, -.4))
}
limitsSelector <- function(limits) {
  if (all(limits > 0))
    return(c(1.5, 2.5))
  else
    return(limits)
}

graph <- ggplot(data = robustnessData, aes(x = ph0, y = ma, ymin = lower, ymax = upper, sides = sides)) +
  # lemon::geom_pointline(size = 2) +
  geom_line() +
  geom_point(size = 2) +
  geom_line(data = hlineDf, aes(x = x, y = y), col = 3, inherit.aes = FALSE, show.legend = TRUE, linetype = "longdash") +
  geom_ribbon(alpha = .2) +
  scale_x_continuous(name = "p(spike)", breaks = seq(.05, .95, length.out = 7)) +
  scale_y_continuous(name = "Model averaged estimate", breaks = breaksSelector, limits = limitsSelector) +
  labs(y = "Model averaged estimate") +
  lemon::facet_rep_wrap(~analysis, scales = "free") +
  geom_rangeframe(sides = "bl") +
  myTheme(legend.position = "right", base_size = 28)
graph
saveFigure(graph, "robustnessReanalysis_big_font.pdf", width = 14)

# df_hline <- tibble(x = range(priorPH0_vals), y = rep(upMA[2], 2))
#
# gImpl <- ggplot(data = df, aes(x = ph0, y = ma, ymin = lower, ymax = upper)) +
#   geom_line(data = df_hline, aes(x = x, y = y), inherit.aes = FALSE, show.legend = TRUE, linetype = "longdash") +
#   # geom_line(linetype = "dashed") +
#   geom_line() +
#   geom_point(size = 2) +
#   geom_ribbon(alpha = .2) +
#   scale_x_continuous(name = "p(spike)", breaks = seq(.05, .95, length.out = 7)) +
#   labs(y = "Model averaged estimate") +
#   geom_rangeframe(sides = "bl") +
#   myTheme(legend.position = "right")
#
# saveFigure(g, "robustnessReanalysis.pdf")
#
