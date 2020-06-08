rm(list = ls())

source("R/functions.R")

dat <- readRDS("data/twoMinds.rds")

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
  ifelse(b > OutputSize, paste0('$\\mathit{BF}_{', num, '} = ', as.scientific(b), '$', ', ', '$d = ', printnum(d), '$', ', ', '95% HDI [', printnum(HDI[1]), ', ', printnum(HDI[2]), ']'), paste0('$\\mathit{BF}_{', num, '} = ', printnum(b), '$', ', ', '$d = ', printnum(d), '$', ', ', '95% HDI [', printnum(HDI[1]), ', ', printnum(HDI[2]), ']'))
}
printBFt(VB1expltBF)


ttestpr <- sqrt(2)/2
# replicate results from Heycke et al., 2018.
#ValenceBlock 1
VB1explt <- t.test(subset(e, ValenceBlock == "1" & Block == "1")$DV,
                   subset(e, ValenceBlock == "1" & Block == "2")$DV,
                   paired = TRUE)

VB1expltBF <- ttestBF(subset(e, ValenceBlock == "1" & Block == "1")$DV,
                      subset(e, ValenceBlock == "1" & Block == "2")$DV,
                      paired = TRUE,
                      rscale = ttestpr)


#ValenceBlock 2
VB2explt <- t.test(subset(e, ValenceBlock == "2" & Block == "2")$DV,
                   subset(e, ValenceBlock == "2" & Block == "1")$DV,
                   paired = TRUE)

VB2expltBF <- ttestBF(subset(e, ValenceBlock == "2" & Block == "2")$DV,
                      subset(e, ValenceBlock == "2" & Block == "1")$DV,
                      paired = TRUE,
                      rscale = ttestpr)
# reanalysis ----

priorPH0  <- 0.5
sigmaSlab <- 1

ybar <- getCohenD(
  subset(dat, ValenceBlock == "2" & Block == "2")$DV,
  subset(dat, ValenceBlock == "2" & Block == "1")$DV
)

n <- length(subset(dat, ValenceBlock == "2" & Block == "1")$DV)

upMA <- updatePar(priorPH0, sigmaSlab, n, ybar[1L])
ciMA <- postStat(upMA)

xgrid <- seq(0.0, 4.0, length.out = 1e4)
yvals <- dnorm(xgrid, upMA[2], upMA[3])

plot(xgrid, yvals)


