# --- analyze JEE-Main cutoffs:
c.path.local <- "C://Users/SONY/Desktop/preBachelors/Dscoring/"
cDecimalDigits <- 5 # coz fractions with over a million examinees
getJMNdata <- function(myData=read.csv(file=paste0(c.path.local, "dataJMNgenCutoff.csv"), stringsAsFactors=FALSE)){
  ans <- myData
  ans$year <- as.factor(ans$year)
  ans <- cbind(ans, rGenCutoff=round(ans$genCutoffMarks / ans$totMarks, cDecimalDigits),
    rOpenCRL=round(ans$openCRL / ans$appearCandidates, cDecimalDigits))
  str(ans)
  # print(summary(ans$rGenCutoff)); print(summary(ans$rOpenCRL))
  print(ans)
  print(
    "as years progress, rGenCutoff seems reducing from about 32% to 20%; rOpenCRL seems increasing from about 6% to 11%")
  return(ans)
}

JMN <- getJMNdata()
# consider bubble chart for JMN-JAD score mapping, for those who wrote both JMN and JAD.
# show screenshots of 2013 rank-mark plots from JEE Reports, considering stmt: "very few score same or more".

stop()
# --- analyze JMAD crosstab:
warning('source("C:\\Users\\SONY\\Desktop\\preBachelors\\Dscoring\\srcJEE-common.R")')
studyJMAD <- function(jj, label){
  jj.colSums <- colSums(jj[,-1], na.rm=TRUE) # coz leftmost column has those who did not qualify to write JAD
  jj.rowSums <- rowSums(jj, na.rm=TRUE)
  # stopifnot(sum(jj.colSums) == sum(jj.rowSums))
  # title(sub=paste0("t",label))
  # mtext(label, side=4) # side=4 for right of plot. 1,2,3,4 are in clockwise seq, starting from bottom of plot
  # text(1,2, labels=paste0("m",label), pos=3)
  # for(s in 1:4) mtext(paste("mtext(...,side=", s ,")"), side = s)
  print(paste(label, "tabulated data for JAD jj.colPcts then JMN jj.rowPcts"))
  jj.colPcts <- jj.colSums / sum(jj.colSums); print(round(jj.colPcts, cDecimalDigits))
  plot(jj.colPcts); plot(cumsum(jj.colPcts), xlab=label)
  jj.rowPcts <- jj.rowSums / sum(jj.rowSums); print(round(jj.rowPcts, cDecimalDigits))
  plot(jj.rowPcts[-1], xlab="JMN right tail"); plot(cumsum(jj.rowPcts), xlab=label)
  return(jj.rowPcts) # JMN
}
JMAD <- read.csv(file=paste0(c.path.local, "dataJMADcrosstab.csv"), stringsAsFactors=TRUE)
JMAD$year <- as.factor(JMAD$year)
JMAD.cols <- c(3:13)
levels(JMAD$JMpctBin) <- colnames(JMAD[,JMAD.cols])
str(JMAD)

dJMN <- NULL # density JMN distribution
par(ask=TRUE)
layout(matrix(1:8, 2, 4, byrow = TRUE)) # matrix object specifying location of N figures to plot. alt: par()
for(y in levels(JMAD$year)){
  dJMN[[y]] <- studyJMAD(JMAD[JMAD$year==y, JMAD.cols], y)
}

  # require(extraDistr) # nsbeta distribution
  c.distribs.explore <- c("norm", "beta", "unif", "binom",  "nbinom", "geom", "hyper", "pois",
    "weibull", "lnorm", "logis", "nsbeta")
    # c("norm", "beta", "unif", "lnorm")
  # promptToContinue("distribs chosen? its fixArg ok, eg, beta (0,1) only?")
  par(ask=TRUE)
  # cFitMethods <- c("mle", "mle", "mme", "qme")
fdcy <- jmny <- NULL
for(y in levels(JMAD$year)[2]){
  jj.rowPcts.y <- dJMN[[y]]
  jj.rowPcts.y <- c(sum(jj.rowPcts.y[c(1:2)]), jj.rowPcts.y[-c(1:2)])
    # from levels(JMAD$JMpctBin): combine (-25,00) with (00,10)
  jj.rowPcts.y.stdCount <- round(jj.rowPcts.y * 10^3)
  c.rMarks.leftNA <- NA # -0.25 coz [-25%,+100%] is real interval of rMarks, as per JEE-Advanced Reports on www
  cJMN <- data.frame( # just like output from mixR::bin()
    a=c((c.rMarks.leftNA*100), (1:9)*10), # left=(c.rMarks.leftNA*100)
    b=(1:10)*10, # right=
    freq=jj.rowPcts.y.stdCount # count
  )
  cJMN[, c("a", "b")] <- cJMN[, c("a", "b")] / 100 # to convert to interval [-0.25, 1.00]
  cJMN.mx <- as.matrix(cJMN)
  cJMN.fdp <- cJMN[rep(1:nrow(cJMN), cJMN$freq), c("a", "b")]
  # cJMN.fdp[(cJMN.fdp$left==0.01),"left"] <- -0.25 # is.na(cJMN.fdp$left)
  colnames(cJMN.fdp) <- c("left", "right")
  print(str(cJMN.fdp)); print(head(cJMN.fdp))
  jmny[[y]] <- cJMN.fdp
  aVar <- cJMN.fdp; fdcList <- NULL
  for(distrib in c.distribs.explore[c(1,2,10,11)]){ # alt: [c(1,2,10,11)]
    fdcList[[distrib]] <- fitdistcens(aVar, distrib)
      # [As for non censored data, one or more parametric distributions can be fitted to the censored data set]
      # [start= A named list giving the initial values of parameters of the named distribution]
      # values must be positive to fit an Weibull/lognormal  distribution
    exploreFdplusDistrib.CInew(aVar, fdcList[[distrib]], distrib, CIcdf.xq=c(0.23,0.10), # 23% min est JEEMainAdvanced/
      c.niter=101, # =501
      isIntervalLeftRight=TRUE) 
  }
  # betash12 <- c(0.0594, 1.6279)
  # fdcList[[distrib]] <- fitdistcens(aVar, distr="nsbeta", start=list(shape1=betash12[1], shape2=betash12[2]))
  fdcy[[y]] <- fdcList
  cdfcompcens(fdcy[[y]]); mtext(y, side=4)
  # getGofShowFit(fdcy[[y]][[1]]) # 1:5 # expects fitdist objects, not fitdistcens!
}
print(str(fdcy))

stop()
### --- alt package for fitting distributions for interval data:
require(mixR)
# gc() # freq order of 1000 => Error: cannot allocate vector of size xxxx Gb
fit2 <- mixfit(cJMN.mx, ncomp=1) # family = c("normal", "weibull", "gamma", "lnorm")
plot(fit2) # smoothness=10*10 # ps=c("base", "ggplot2"). smoothness=512 default, but [
# Error in seq.default(xlow, xupp, length = smoothness) : 
#   'from' must be a finite number
# ]


# JAD jj.colPcts initially differ for 2013 and 2014.; then they are more similar. JMN jj.rowPcts are similar.
#
# what works with censored *and* binned data, at least *binned and* Left-censored data (where right limit is known)?
# https://www.stats-et-al.com/2018/10/parameter-estimation-of-binned-data.html has an introduction
# to the problem of binned data before going on to fitting a distribution.
# https://cran.r-project.org/web/packages/binsmooth/binsmooth.pdf says:
# [
# Data are assumed to be nonnegative,
# the top bin is assumed to have no upper bound, but the bin widths need
# be equal. ... In practice, an estimate for
# the mean of the distribution should be supplied as an optional argument.
# Doing so greatly improves the reliability of statistics computed from
# the smoothed density functions.
# ]
# https://cran.r-project.org/web/packages/mixR/mixR.pdf
# http://www.statslab.cam.ac.uk/~rjs57/LogConcDEAD.pdf
#

# --- copy censored-distribution-fitting code from srcJEE-enlighten.R:
# ref https://cran.r-project.org/web/packages/fitdistrplus/vignettes/paper2JSS.pdf (page 14) says [
# Observations only known to be under a limit of detection are left-censored data.
# ]
#
# ref https://pdixon.public.iastate.edu/stat505/Chapter%2011.pdf says: [
# A sample is Type I censored when the censoring levels are known in advance. The number of censored
# observations c (and hence the number of uncensored observations n) is a random outcome, even if the total
# sample size, N, is fixed. Environmental data are almost always type I censored.
# A sample is Type II censored if the sample size N and number of censored observations c (and hence the
# number of uncensored observations n) are fixed in advance. The censoring level(s) are random outcomes.
# Type II censored samples most commonly arise in time-to-event studies that are planned to end after a
# specified number of failures ...
# A sample is Randomly Censored when both the number of censored observations and the censoring levels
# are random outcomes. ...
# A sample is singly censored (e.g., singly left censored) if there is only one censoring level T. (Technically,
# left censored data are singly left censored only if all n uncensored observations are greater than or equal
# to T, and right-censored data are singly right censored only if all n uncensored observations are less than
# or equal to T (Nelson, 1982, p.7); otherwise, the data are considered to be multiply censored.)
# ]

# in year-wise JEE-Advanced CRL-Marks lists, marks appear to be Type II censored samples, while ranks seem Type I.
# They can be treated as Left censored. Or by considering minimum marks (or rank) possible, treat as Interval censored.
