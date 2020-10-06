# ref https://stackoverflow.com/questions/34569256/link-to-filenames-with-spaces-in-bitbucket-markdown
# ref https://github.com/thomasp85/patchwork/issues/55:
#   devtools::install_github("r-lib/rlang", build_vignettes = TRUE)

filedir <- "C://Users/SONY/Desktop/preBachelors/topicPerf/"
filename <- "JanJEEy0-track - topicTrack.csv"
fileTopicRisk <- "data-topicRisk-RotationalMechanics.csv"
cADPlist <- list(DPvec=c(0.50, 0.23, (60*3)/(25*3)), # from committed results of Community Project JanJEEy0
  ltyVec=c("solid", "dashed", "dashed"),
  colVec=c("orange", "green", "grey")
)
get.topicTrack <- function(file=paste0(filedir, filename)){
  topicTrack <- read.csv(file, stringsAsFactors=TRUE, skip=1)
  # print(str(topicTrack))

  ind1DP <- topicTrack$defectCount / topicTrack$questionCount
  ind2DP <- topicTrack$defectCount.1 / topicTrack$questionCount.1
  ind1minQ <- topicTrack$minutes / topicTrack$questionCount
  ind2minQ <- topicTrack$minutes.1 / topicTrack$questionCount.1
  topt <- cbind(topicTrack, ind1DP=ind1DP, ind2DP=ind2DP, ind1minQ=ind1minQ, ind2minQ=ind2minQ)
  print(str(topt))
  return(topt)
}
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  # ref https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
get.topicRisk <- function(file=paste0(filedir, fileTopicRisk)){
  topicRisk <- read.csv(file, stringsAsFactors=TRUE, skip=0)
  # print(str(topicRisk))
  topicRisk$subtopic <- as.factor(topicRisk$subtopic)
  stopifnot(levels(topicRisk$q5)[4] == "NoKey") # no solution key to verify learner's answer
  levels(topicRisk$q5)[4] <- NA
  topicRisk <- topicRisk[, -7]
  topicRisk$q5 <- as.numeric.factor(topicRisk$q5)
  return(topicRisk)
}
df2matrix <- function(df, byrow=TRUE, dim1, dim2){
  # ref https://stackoverflow.com/questions/35441382/change-dimension-of-a-data-frame
  mx <- matrix(unlist(t(df)), byrow=byrow, dim1, dim2)
  return(mx)
}
get.topicGrp <- function(file=paste0(filedir, "dat-topicGrp.csv")){
  topicGrp <- read.csv(file, stringsAsFactors=TRUE, skip=0)
  return(topicGrp)
}


par(ask=TRUE)
topt <- get.topicTrack()
dat.tgrp <- get.topicGrp()
subSec <- dat.tgrp$topicGrp
# subSec <- paste0(substr((topt$subject), 1, 4), substr(topt$topic, 1, 3)) # leftmost 4 Subject chars & 3 Section chars
subSec.fac <- as.factor(subSec); summary(subSec.fac)
topt <- cbind(topt, subSec=subSec.fac)
table(topt$subject, topt$subSec)
summary(topt)
if(any(summary(topt$subSec) < 3)){
  print("combine some subSec levels before proceeding coz small sample")
  stop()
  # was: levels(topt$subSec)[c(5,6)] <- "physAB-" # presently combine physA-U:20 and physB-U:1
  print(levels(topt$subSec)); print(summary(topt))
} # else continue

summary(topt$ind2DP); summary(topt$ind2minQ)
  # ind2DP median 0.23 (mean too); 3rd quartile 0.29.
hist(topt$ind2DP); myPlotOverlay(hTvF=FALSE, ixVec=c(1:2), limList=cADPlist, myLabel="")
# topt$topic[(! is.na(topt$ind2DP)) & topt$ind2DP >= 0.4]
hist(topt$ind2minQ); myPlotOverlay(hTvF=FALSE, ixVec=c(3), limList=cADPlist, myLabel="")
  # 2019Dec28: ind2 to note the peak to the right of the (optimal) specified limit. let's take that on (and dissolve it)!
topt.ssti2 <- topt[order(topt$ind2DP, na.last=NA, decreasing=TRUE), c("subSec", "topic", "ind2DP", "ind2minQ")]
str(topt.ssti2)
plot(ind2DP ~ 1, data=topt.ssti2); myPlotOverlay(hTvF=TRUE, ixVec=c(1:2), limList=cADPlist, myLabel="topics sorted")
boxplot(ind2DP ~ subject, data=topt); myPlotOverlay(hTvF=TRUE, ixVec=c(1:2), limList=cADPlist, myLabel="ind2DP")
  # 2020Jan04: consider overlay with jitter to show raw observations which might be masked inadvertently by boxplot.
  #   physics boxplot shows least variation, relative to others.
  # 2019Dec28: can ind2 plan for these:
  # chemistry shows the most variation. physics is missing those low-DP scores.
  # can maths and physics be broken into subsec (though JEE Syllabus doesn't say so), eg, by clustering
  # (or grouping separated by median) on both ind2DP and ind2minQ which then allows quadrants to be targeted
  # with outputs including sets of 7 questions and Spacing at least 3 times?  (This is now available in 2020.)
boxplot(ind2DP ~ subSec, data=topt.ssti2); myPlotOverlay(hTvF=TRUE, ixVec=c(1:2), limList=cADPlist, myLabel="ind2DP")
  # 2020Jan04: cC-O shows most variation and exceedance of higher DP limit; then mOther.
  #   mGeometry shows highest median; cA-P shows least median.  Consider jitter for raw observations.
  # till 2019Dec28: [B] ind2DP: "chemC-O" shows largest quartile intervals; so locate source & lower it.
  #   "chemA-P" "chemB-I" show narrow; so exploit
boxplot(ind2minQ ~ subSec, data=topt.ssti2); myPlotOverlay(hTvF=TRUE, ixVec=c(3), limList=cADPlist, myLabel="ind2minQ")
  # 2020Jan04: pOther (with also highest variation) pMechanics then mCalculus then cA-P show relatively high ind2minQ.
  #   cB-I shows least ind2minQ.
  # till 2019Dec28:
  # ind2minQ: "chemB-I" shows least quartile intervals; so exploit.
  # "mathUni" highest quartiles beyond (mean) limit, then "physAB-"; so locate source & lower it.
  # overall: (a) maximize low DP & quick (topics of) "chemB-I" then "chemA-P".
  # (b) ref [B] above.
plot(ind2DP ~ ind2minQ, data=topt.ssti2)
  myPlotOverlay(hTvF=TRUE, ixVec=c(1:2), limList=cADPlist, myLabel="")
  myPlotOverlay(hTvF=FALSE, ixVec=c(3), limList=cADPlist, myLabel="")
  # quadrant view to prioritize action.

# now, when using a central value for ind2DP, how to adjust for its dispersion?  Needed to incorporate uncertainty
# into its model.
myDiscrete <- function(x){
  return(round(100*x, 0))
}
ind2DPpct <- myDiscrete(topt.ssti2$ind2DP) # without NAs. maybe defects*HCF(denom) to revert to discrete nature
date(); summary(ind2DPpct)
dExplore <- c("geom", "pois", "nbinom", "norm") # "binom", "hyper" needs start arg
  # [If discrete is TRUE, the represented distributions are ... and the normal distribution to which previous discrete
  # distributions may converge.]
fd <- fitdist(ind2DPpct, dExplore[1])
exploreFdplusDistribs(ind2DPpct, distribs.explore=dExplore,
  myFixArg="", CIcdf.xq=c(myDiscrete(cADPlist$DPvec[2]), 0.10), c.niter=501, isDiscrete=TRUE)
  # 2020Jan04: "norm" fits well, considering the Cull and Frey graph, CICDF plot, diagnostics plot, and AIC BIC:
  # Earlier, "nbinom" fitted better: 50:50 chance of DP meeting the 0.23 target!

exploreFdplusDistrib.CInew(ind2DPpct, fd, distribs.explore=dExplore,
  CIcdf.xq=c(myDiscrete(cADPlist$DPvec[2]), 0.10), c.niter=501, isIntervalLeftRight=FALSE, isDiscrete=TRUE)

exploreFdplusDistribs(topt.ssti2$ind2DP, distribs.explore=c.distribs.explore[c(1)],
  myFixArg="", CIcdf.xq=c(cADPlist$DPvec[2], 0.10), c.niter=101, isDiscrete=FALSE)

```r
Fitting of the distribution ' norm ' by maximum likelihood 
Parameters : 
      estimate Std. Error
mean 0.2262676 0.01477347
sd   0.1191075 0.01044310
Loglikelihood:  46.07138   AIC:  -88.14275   BIC:  -83.79398
```
# let's get the quantiles from the well-fitted `normal` distribution:
fdnorm <- fitdist(topt.ssti2$ind2DP, "norm") # discrete=FALSE
summary(fdnorm)
quantile(fdnorm)
```r
> quantile(fdnorm)
Estimated quantiles for each specified probability (non-censored data)
              p=0.1     p=0.2     p=0.3     p=0.4     p=0.5     p=0.6     p=0.7
estimate 0.07362526 0.1260243 0.1638076 0.1960921 0.2262676 0.2564432 0.2887277
            p=0.8   p=0.9
estimate 0.326511 0.37891
> quantile(fdnorm, prob=0.95)
Estimated quantiles for each specified probability (non-censored data)
           p=0.95
estimate 0.422182
```

### --- Risk Prioritization within a Topic:
topicRisk <- get.topicRisk()
print(str(topicRisk))
trisk <- topicRisk[, c(2:6)]
# trisk.colmeans <- colMeans(trisk, na.rm=TRUE) # means of each column
trisk.rowmeans <- rowMeans(trisk, na.rm=TRUE) # alt: median for robustness
# hist(trisk.rowmeans)
topicRisk <- cbind(topicRisk, meanDP=trisk.rowmeans)
hist(topicRisk$meanDP) # masks/hides raw scores

trisk.mx <- df2matrix(trisk, byrow=FALSE, dim(trisk)[1] * dim(trisk)[2], 1)
dim(trisk.mx) <- NULL # ref https://stat.ethz.ch/pipermail/r-help/2005-March/068063.html
trisk <- as.vector(na.omit(trisk.mx)) # alt: na.exclude() which might be handled differently in modeling functions
print(str(trisk))
hist(trisk)
mean(trisk, na.rm=TRUE) # 0.583

stop()
trisk <- floor(trisk); trisk <- as.integer(trisk); print(str(trisk))
  # stricter evaluation of question answers: 0.5 becomes 0
fitdist(trisk, distr="pois") # "binom" needs starting values
fitdist(trisk, distr="nbinom")
descdist(trisk, discrete=TRUE, boot=101)

aVar <- trisk
exploreFdplusDistribs(aVar, distribs.explore=c("pois", "nbinom"),
  myFixArg="", CIcdf.xq=c(0.35,0.10), c.niter=101)
# exploreFdplusDistrib.CInew(aVar, fd, distribs.explore,
#   CIcdf.xq=c(0.35,0.10), c.niter=101, isIntervalLeftRight=FALSE)

### --- use Binomial Distribution with n-Bernoulli Trials:
# ref http://www.r-tutor.com/elementary-statistics/probability-distributions/binomial-distribution:
# beware if only fair probabilities are expected. alt: "Poisson trials", Bernoulli; Discrete {e1071}?
marks.lim <- c(0.33, 0.70)
pAttempt <- 0.95 # proportion of questions that will be attempted, ie, where trials will happen
DP <- 0.4 # Defect Probability, where trials happen ie conditional to pAttempt
myRound <- function(x, decimals=0){
  return(round(x, decimals))
}
calcFmarks <- function(pAttempt, DP, q.MCQ=20, q.NumAns=5, mark.MCQ.negPos=c(-1,4), mark.NumAns.negPos=c(0,4)){
  stopifnot(q.MCQ != q.NumAns)
  probSuccess <- 1 - DP # Defect Probability
    # nMultiChoice <- 4; probSuccess <- (1/nMultiChoice) if nMultiChoice options to choose from per question,
    # unless there's a better estimate
  qSec.seq <- c(q.MCQ, q.NumAns); qSec.seq <- qSec.seq[!(qSec.seq < 1)]
  for(qSec in qSec.seq){
    nTrials <- myRound(qSec*pAttempt) # independent (random) trials, ie, presumes no shock (carry-over) effect
    nCorrect <- 0:nTrials # enumerate, for now
      # pCorrect.lim <- marks.lim # beware: this is marks% not the same as proportion of correct (answered) questions
      # nCorrect <- myRound(pCorrect.lim * nTrials)
    # dbinom(nCorrect, size=nTrials, prob=probSuccess)
      # density function at a point dbinom() for exactly nCorrect
      # sum(dbinom(x1..xN, ...)) to sum over a range
    myArgs <- paste0(pAttempt, ";", DP, ";", qSec)
    gpbinomList[[myArgs]] <<- pbinom(nCorrect, size=nTrials, prob=probSuccess, lower.tail=FALSE)
      # (cumulative) distribution function pbinom() for (if lower.tail==TRUE, up to nCorrect) or
      #   (if lower.tail==FALSE, beyond nCorrect).
      # lower.tail logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].
    print(paste("myArgs", myArgs, "gpbinom:")); print(gpbinomList[[myArgs]])

    # now, apply markScheme:
    nDefect <- nTrials - nCorrect
    if(qSec == q.MCQ){
      mark.MCQ <- (mark.MCQ.negPos[1] * nDefect) + (mark.MCQ.negPos[2] * nCorrect)
      print("mark.MCQ:"); print(mark.MCQ)
      plot(gpbinomList[[myArgs]] ~ mark.MCQ, main="Cumulative Probability of Exceeding x Marks")
    } else {
      if(q.NumAns < 1) next # else continue
      mark.NumAns <- (mark.NumAns.negPos[1] * nDefect) + (mark.NumAns.negPos[2] * nCorrect)
      print("mark.NumAns:"); print(mark.NumAns)
      plot(gpbinomList[[myArgs]] ~ mark.NumAns, main="Cumulative Probability of Exceeding x Marks")
    }
  }

  pExceed <- c(); mark.tot <- c() # init
  # for the 2 types of questions, add marks and maybe multiply probabilities (= intersection algebra). beware!
  for(iMCQ in 1:q.MCQ){
    myArgs.MCQ <- paste0(pAttempt, ";", DP, ";", q.MCQ)
    if(q.NumAns < 1){
      pExceed <- c(pExceed, gpbinomList[[myArgs.MCQ]][iMCQ] * 1)
      mark.tot <- c(mark.tot, mark.MCQ[iMCQ] + 0)
    } else {
      for(iNumAns in 1:q.NumAns){
        myArgs.NumAns <- paste0(pAttempt, ";", DP, ";", q.NumAns)
        pExceed <- c(pExceed, gpbinomList[[myArgs.MCQ]][iMCQ] * gpbinomList[[myArgs.NumAns]][iNumAns])
        mark.tot <- c(mark.tot, mark.MCQ[iMCQ] + mark.NumAns[iNumAns])
      }
    }
  }
  mark.max <- (mark.MCQ.negPos[2] * q.MCQ) + (mark.NumAns.negPos[2] * q.NumAns)
  # q.tot <- q.MCQ + q.NumAns # per subject
  mark.pct <- mark.tot / mark.max
  pExceedMark <- list(pExceed=pExceed, mark.pct=mark.pct)
  plot(pExceed ~ mark.pct, data=pExceedMark, main="Cumulative Probability of Exceeding x Marks%")
  # ref https://stackoverflow.com/questions/2752323/how-can-i-concatenate-a-vector
  myArgs.MCQ.NumAns <- paste0("att;DP;qMCQ;qNumAns==", pAttempt, ";", DP, ";",
    q.MCQ, "[", paste(mark.MCQ.negPos, collapse=":"), "];",
    q.NumAns, "[", paste(mark.NumAns.negPos, collapse=":"), "]")
  mtext(text=myArgs.MCQ.NumAns, side=4)
  abline(v=marks.lim, lty=c("solid", "dashed"), col=c("red", "green"))
  return(pExceedMark)
}
negApprox <- ((-1)*20 + 0*5) / (20+5) # alt: q.MCQ=20, q.NumAns=5 but not mult cumprob!
gpbinomList <- NULL
# Scenario `att0.95DP0.23`:
pExceedMark <- calcFmarks(pAttempt=0.95, DP=0.23, q.MCQ=25, q.NumAns=0, mark.MCQ.negPos=c(negApprox,4))

# Scenario `att0.7DP0.12`:
pExceedMark <- calcFmarks(pAttempt=0.7, DP=0.12, q.MCQ=25, q.NumAns=0, mark.MCQ.negPos=c(negApprox,4))

# Scenario `att0.9DP0.5`:
pExceedMark <- calcFmarks(pAttempt=0.9, DP=0.5, q.MCQ=25, q.NumAns=0, mark.MCQ.negPos=c(negApprox,4))


stop()
# --- code for reuse:
boxplot(ind2DP ~ subSec, data=topt)

median.rmNA(topt$ind1DP); mean(topt$ind1DP, na.rm=TRUE) # summary(topt$ind1DP)
median.rmNA(topt$ind1minQ); mean(topt$ind1minQ, na.rm=TRUE)

print(aggregate(topt$ind2DP, by=list(topt$subject), FUN=summary))
print(aggregate(topt$ind2DP, by=list(topt$subject), FUN=median.rmNA))
