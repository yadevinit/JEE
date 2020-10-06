cDecimalDigits <- 3 # by default
cPtile <- c(0, 1/10, 1/4, 1/3, 1/2); cPtile <- unique(sort(round(c(cPtile, 1-cPtile), cDecimalDigits)))
c.path.local <- "C://Users/SONY/Desktop/preBachelors/Dscoring/"
c.distribs.explore <- c("norm", "beta", "unif", "weibull", "lnorm", "gamma") # "nsbeta" needs start=
  # for pkg fitdistrplus used by exploreFdplusDistribs()
cFitMethods <- c("mle", "mle", "mme", "qme")
cDistribs <- c("binomial","beta binomial","Poisson","negative binomial","geometric", # discrete
  "zeta","normal","log normal","inverse Gauss","logistic",
  "Laplace","Cauchy","Student t","exponential","Pareto",
  "gamma","Weibull") # ref https://rdrr.io/cran/gnlm/src/R/fitdist.r

# for getDscores() etc:
cSubjects <- c("P", "C", "M", "S", "N") # "N" denotes "PCM" combined, eg, when only combined scores are given
cSelf <- "ind"
cColsKeyPrefix <- c("date", "pattern", "JADremark")
cColsSub.tobe <- c(cSelf, "avg", "max") # was: , paste0("avg.", cSelf), paste0("max.", cSelf))

asNumChrPct <- function(vecChrPct){
  return(as.numeric(sub("%", "e-2", vecChrPct))) # ref https://stat.ethz.ch/pipermail/r-help/2005-November/082625.html
}
promptToContinue <- function(msg){
  msg.suffix <- ": To stop, enter no: "
  toContinue <- tolower(readline(paste(msg, msg.suffix)))
  stopifnot(toContinue != "no")
  # else quietly continue
  return()
}
meanNArm <- function(vec){
  return(mean(vec, na.rm=TRUE))
}
sdNArm <- function(vec){
  return(sd(vec, na.rm=TRUE))
}
mySummary <- function(x, digits=cDecimalDigits, ...){
  return(summary(x, digits, ...))
}
myBoxPlots <- function(output, input, qaTable, ...){ # cex.axis=0.5, las=2
  # par() # shows various settings for graphics
  # par(cex.axis=0.5) # default 1. reduces size of axes labels
  # par(las=2) # las= for labels at angle to axis
  ret.summ <- mySummary(output); hist(output)
  ret.aggr <- aggregate(output, by=list(input), data=qaTable, FUN=mySummary)
  promptToContinue("saved histogram before boxplots get displayed")
  rb <- boxplot(output ~ input, ...)
  title("Comparing boxplot()s and non-robust mean +/- SD")
  mn.t <- tapply(output, input, meanNArm)
  sd.t <- tapply(output, input, sdNArm)
  xi <- 0.3 + seq(rb$n)
  points(xi, mn.t, col = "orange", pch = 18)
  arrows(xi, mn.t - sd.t, xi, mn.t + sd.t,
    code = 3, col = "pink", angle = 75, length = .1)
  return(list(ret.summ, ret.aggr))
}
myQtiles <- function(x){
  ans <- round(quantile(x, probs=cPtile), cDecimalDigits)
  return(ans)
}
substrRight <- function(x, n){
  # ref https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
  # substr(x, nchar(x)-n+1, nchar(x)) not vectorized
  xchar <- as.character(x)
  sapply(xchar, function(xx)
         substr(xx, (nchar(xx)-n+1), nchar(xx))
         ) # and return this
}
asNumChrPct <- function(vecChrPct){
  return(as.numeric(sub("%", "e-2", vecChrPct))) # ref https://stat.ethz.ch/pipermail/r-help/2005-November/082625.html
}
myReshape <- function(inData, i){
  mdColnames <- colnames(inData)
  iSub <- cSubjects[i]
  colsSub <- grep(paste0("^", iSub), mdColnames, ignore.case=FALSE)
  colsSub <- colsSub[1:min(length(colsSub), length(cColsSub.tobe))]
  ixRowsSub <- which(! is.na(inData[,colsSub[1]])) # scan first column. was an exact match: which(! is.na(inData[,iSub]))
  colsKey <- c(1:3); print(colnames(inData[colsKey])); stopifnot(colnames(inData[colsKey]) == cColsKeyPrefix)
  mdSub <- inData[ixRowsSub, c(colsKey, colsSub)]
  mdSubTmp <- cbind(subject=iSub, mdSub)
  # colsSub.tobe <- gsub(iSub, cSelf, mdColnames, ignore.case=FALSE)
  if(length(colsSub) >= 3){ # ie: ind, avg, max ...
    colsSub.tobe <- cColsSub.tobe
  } else { # missing self ('ind')
    colsSub.tobe <- tail(cColsSub.tobe, -1)
  }
  colnames(mdSubTmp) <- c(colnames(mdSubTmp)[1:(1+length(cColsKeyPrefix))], colsSub.tobe)[1:ncol(mdSubTmp)]
    # was: cColsSub.tobe
  str(mdSubTmp)
  return(mdSubTmp)
}
getDscores <- function(myData=read.csv(file=paste0(c.path.local, "dataDscores.csv"),
  stringsAsFactors=FALSE, nrows=(94+2))){ # as on 2019-Nov-08, 2019-Nov-14
  myData$date <- as.Date(myData$date, format="%d/%b/%Y") # "%d-%b-%Y"
  myData$pattern <- as.factor(myData$pattern)
  myData$P <- asNumChrPct(myData$P)
  myData$Pavg <- asNumChrPct(myData$Pavg)
  myData$Pmax <- asNumChrPct(myData$Pmax)
  myData$C <- asNumChrPct(myData$C)
  myData$Cavg <- asNumChrPct(myData$Cavg)
  myData$Cmax <- asNumChrPct(myData$Cmax)
  myData$M <- asNumChrPct(myData$M)
  myData$Mavg <- asNumChrPct(myData$Mavg)
  myData$Mmax <- asNumChrPct(myData$Mmax)
  myData$CS <- asNumChrPct(myData$CS)
  myData$CSavg <- asNumChrPct(myData$CSavg)
  myData$CSmax <- asNumChrPct(myData$CSmax)
  str(myData)
  col.CS <- c(19:21); col.CSasis <- c("CS", "CSavg", "CSmax"); col.CStobe <- c("S", "Savg", "Smax")
    # Software not Chemistry
  stopifnot(colnames(myData)[col.CS] == col.CSasis); colnames(myData)[col.CS] <- col.CStobe
  str(myData)

  # locate duplicated scores and re-label (coz really PCM combined scores, not individual subject's):
  ix.combo <- which(myData$Pavg == myData$Cavg & myData$Cavg == myData$Mavg &
    myData$Pmax == myData$Cmax & myData$Cmax == myData$Mmax) # rownames of subject "N" denoting combined "PCM"
  Ntuple <- myData[ix.combo, c("P", "Pavg", "Pmax")] # or copy from any other (equivalent) avg+max pair
  colnames(Ntuple) <- c("N", "Navg", "Nmax")
  Ntuple$N <- round(rowMeans(myData[ix.combo, c("P","C","M")],
    na.rm=TRUE), cDecimalDigits) # coz there are no individual "N" ie combined "PCM" scores. There might be NAs
  myData[ix.combo,
    c("Pavg","Pmax","Pavg.P","Pmax.P","Cavg","Cmax","Cavg.C","Cmax.C","Mavg","Mmax","Mavg.M","Mmax.M")] <- NA
  myData <- cbind(myData, N=as.numeric(NA), Navg=as.numeric(NA), Nmax=as.numeric(NA))
  myData[ix.combo, c("N", "Navg", "Nmax")] <- Ntuple
  col.N <- c(22:24); col.Nasis <- c("N", "Navg", "Nmax")
  stopifnot(colnames(myData)[col.N] == col.Nasis)
  str(myData)

  # reshape into columns: subject, self, avg, max.  ignore ratios on them for now
  # alt:
  #  md.colnames <- colnames(md); mdSub.colnames <- colnames(mdSub)
  #  extraCols <- md.colnames %in% mdSub.colnames
  #  mdSub[extraCols] <- as.numeric(NA)
  md <- data.frame()
  for(i in 1:length(cSubjects)){
    mdSub <- myReshape(myData, i)
    md.ncol <- ncol(md)
    mdSub.ncol <- ncol(mdSub)
    if(md.ncol > mdSub.ncol){ # must pad/fill required cols with NAs
      extraCols <- (mdSub.ncol+1) : (md.ncol)
      # extraColnames <- cColsSub.tobe[extraCols]
      extraColnames <- colnames(md)[extraCols]
      mdSub[, extraColnames] <- as.numeric(NA)
      str(mdSub)
    } # else continue
    md <- rbind(md, mdSub)
  }
  summary(md)

  # avg/ind etc for subject S,N
  md$avg.ind <- round(md$avg / md$ind, cDecimalDigits)
  md$max.ind <- round(md$max / md$ind, cDecimalDigits)
  avgByMax <- round(md$avg / md$max, cDecimalDigits)
  class12 <- md$date > '2019-02-25' # dateGrp

  cJMA <- "JMA" # else JAD (sub) pattern for competitive pattern
  subPattern <- rep("JAD", length(md$JADremark)) # init default
  ix.PU <- which(md$pattern == "PU"); subPattern[ix.PU] <- "PU"
  ix.JMA <- grep(paste0("^",cJMA,"."), md$JADremark, ignore.case=FALSE); subPattern[ix.JMA] <- cJMA
  subPattern <- as.factor(subPattern)

  md <- cbind(md, avgByMax=avgByMax, class12=class12, subPattern=subPattern,
    daysIn1112=as.integer(md$date - min(md$date)))
  md <- md[order(md$date),]
  str(md); summary(md)
  return(md)
}
getGofShowFit <- function(fd){
  # fitdistrplus says:
  # [An approximate Kolmogorov-Smirnov test is performed by assuming the distribution parameters
  # known. The critical value defined by Stephens (1986) for a completely specified distribution is used
  # to reject or not the distribution at the significance level 0.05. Because of this approximation, the
  # result of the test (decision of rejection of the distribution or not) ... Results of the tests are not printed but
  # stored in the output of the function.]

  print(summary(fd))
  fd.gofs <- gofstat(fd); print(fd.gofs)
  plot(fd)
  mtext(paste0(fd$distname, "::", fd$method), side=4)
    # side= (1=bottom, 2=left, 3=top, 4=right). col="gray"
  promptToContinue("saved plot fitdist?")
  return(fd.gofs)
}
exploreFdplusDistribs <- function(aVar, distribs.explore=c.distribs.explore, # was: [1:3],
  myFixArg="", CIcdf.xq=c(0.35,0.10), c.niter=501){ # c.niter=1001 default for bootdist(..., niter, ...)
  require(fitdistrplus)
  # ref https://cran.r-project.org/web/packages/fitdistrplus/vignettes/paper2JSS.pdf says:
  # [Skewness and kurtosis are known not to be robust. In order to take into account the uncertainty of the estimated
  # values, a nonparametric bootstrap procedure ... can be performed by using the argument boot. ...
  # Looking at the results on this example with a positive skewness and a kurtosis not far from 3, the fit of three
  # common right-skewed distributions could be considered, Weibull, gamma and lognormal distributions. ...
  # The density plot and the CDF plot may be considered as the basic classical goodness-of-fit plots. The two other
  # plots are complementary and can be very informative in some cases. The Q-Q plot emphasizes the lack-of-fit at the
  # distribution tails while the P-P plot emphasizes the lack-of-fit at the distribution center.
  # ]
  # for avg, myFixArg.default <- list(distr=?, meth=?, fixArg=?)
  # promptToContinue("distribs chosen? its fixArg ok, eg, beta (0,1) only?")
  # descdist(aVar); promptToContinue("saved plot?")
  descdist(aVar, discrete=FALSE, method="unbiased", graph=TRUE, boot=c.niter); promptToContinue("saved descdist::boot?")
  # plotdist(aVar, histo=TRUE, demp=TRUE); promptToContinue("saved plot?")
  for(d in distribs.explore){
    if((length(distribs.explore) >=2) && (d == "beta") && (min(aVar) < 0 || max(aVar) > 1)){
      # aVar unsuitable for beta distrib?
      warning("aVar unsuitable for beta distrib coz not [0,1]")
      next # to next iteration of for()
    } # else continue
    fdmle <- fitdist(aVar, distr=d, method=cFitMethods[1]); print("now fix arg:"); print(names(fdmle$estimate))
    # fdmleFix <- fitdist(aVar, distr=d, method=cFitMethods[2])
      # was for avg: , fix.arg=list(min=0.08, max=0.77))
      # list(shape2=4.8)) for "beta" reduce shape2 coz >stderr
      # list(sd=0.22) 0.19 for "norm"
      # list(min=-0.1, max=1.0) for "unif", but even list(min=+0.08, max=0.77) is failing!
      # prefer gradual "sigmoid" rise in CDF, considering histogram of Max (uniform with tail spike) and histogram of Ind
    # print("beware min **-ve; mle fix preferred coz expanded to Ind like")
      # max=myFixArg
      # Parameters with fixed value are thus NOT estimated by this maximum likelihood procedure.
      # The use of this argument is not possible if method="mme" and a closed-form formula is used.
      # fix.arg.fun=... list(mean=md$avg) see qmedist for details
    ## fdmme <- fitdist(aVar, distr=d, method=cFitMethods[3])
    ## promptToContinue(paste("fdmme: fix.arg/probs length matches moment vector", length(fdmme$estimate), "?"))
      # (fd$estimate). "mme" moment matching
    ## fdqme <- fitdist(aVar, distr=d, method=cFitMethods[4], probs=c(1/10, 9/10))
    ## print("beware: fdqme needs probs=; earlier avgMax sample might miss min max")
      # was for avg: , probs=c(1/10, 9/10))
      # c(2/10, 8/10) c(1/10, 9/10) c(85/100, 95/100) # focus around particular quantles near max
      # probs defining the probabilities for which the quantile matching is performed. The length of this vector must be
      # equal to the number of parameters to estimate.
    fd.gofs <- getGofShowFit(fdmle)
    # fd.gofs <- getGofShowFit(fdmleFix)
    ## fd.gofs <- getGofShowFit(fdmme)
    ## fd.gofs <- getGofShowFit(fdqme)
    cdfcomp(list(fdmle), legend=cFitMethods[1]) # coz +fdmle
    ## cdfcomp(list(fdmle, fdmme, fdqme), legend=cFitMethods[-2]) # coz -fdmleFix
    # was: cdfcomp(list(fdmle, fdmleFix, fdmme, fdqme), legend=cFitMethods)
    # alt: Weighted version of the estimation process is available for method = "mle", "mme", "qme" by using weights=...
    mtext(d, side=4)
    promptToContinue("saved cdfcomp?")

    CIout <- "quantile" #  # CI on x values (quantiles). alt: "probability" CI on y values (probabilities)
    fd <- fdmle # alt: fdmme
    bmeth <- "nonparam" # alt was: "param"
      # [bootmethod= A character string coding for the type of resampling : "param" for a parametric
      # resampling and "nonparam" for a nonparametric resampling of data]
    bd <- bootdist(fd, bootmethod=bmeth, niter=c.niter)
    print(summary(bd)) # bd$CI
    plot(bd)
    mtext(paste0(bd$fitpart$distname, "::", bd$fitpart$method, "::", bd$method), side=4)
      # side= (1=bottom, 2=left, 3=top, 4=right). col="gray"
    promptToContinue("saved bootdist()?")
    CIcdfplot(bd, CI.output=CIout)
    mtext(paste0(bd$fitpart$distname, "::", bd$fitpart$method, "::", bd$method), side=4)
      # side= (1=bottom, 2=left, 3=top, 4=right). col="gray"
    abline(h=CIcdf.xq[2], lty=2, col='blue')
      # line corresponding to a CDF of quantile specified. confidence intervals on any quantiles
    abline(v=CIcdf.xq[1], lty=2, col='green') # line corresponding to x specified
    promptToContinue("saved CIcdfplot?")
  }
  return()
}
namedVecToNamedList <- function(namedVec){
  # ref https://stackoverflow.com/questions/46251725/convert-named-vector-to-list-in-r
  a.list <- split(unname(namedVec), names(namedVec))
  return(a.list)
}
exploreFdplusDistrib.CInew <- function(aVar, fd, distribs.explore,
  CIcdf.xq=c(0.35,0.10), c.niter=501){
  require(fitdistrplus)
  descdist(aVar, discrete=FALSE, method="unbiased", graph=TRUE, boot=c.niter)
  mtext(distribs.explore, side=4); promptToContinue("saved descdist::boot?")

    CIout <- "quantile" #  # CI on x values (quantiles). alt: "probability" CI on y values (probabilities)
    bmeth <- "nonparam" # alt was: "param"
      # [bootmethod= A character string coding for the type of resampling : "param" for a parametric
      # resampling and "nonparam" for a nonparametric resampling of data]
    bd <- bootdist(fd, bootmethod=bmeth, niter=c.niter)
    print(summary(bd)) # bd$CI
    plot(bd)
    mtext(paste0(bd$fitpart$distname, "::", bd$fitpart$method, "::", bd$method), side=4)
      # side= (1=bottom, 2=left, 3=top, 4=right). col="gray"
    promptToContinue("saved bootdist()?")
    CIcdfplot(bd, CI.output=CIout)
    mtext(paste0(bd$fitpart$distname, "::", bd$fitpart$method, "::", bd$method), side=4)
      # side= (1=bottom, 2=left, 3=top, 4=right). col="gray"
    abline(h=CIcdf.xq[2], lty=2, col='blue')
      # line corresponding to a CDF of quantile specified. confidence intervals on any quantiles
    abline(v=CIcdf.xq[1], lty=2, col='green') # line corresponding to x specified
    promptToContinue("saved CIcdfplot?")

  return()
}
