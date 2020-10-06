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

warning("source srcJEE-common.R")
cCRLinterval <- 100 # gap between adjacent observations
c.rMarks.leftNA <- -0.25 # [-25%,+100%] is real interval of rMarks, as per JEE-Advanced Reports on www
getCRLmarks <- function(myData=read.csv(file=paste0(c.path.local, "dataJEE-CRLmarks.csv"), stringsAsFactors=FALSE)){
  ans <- myData
  ans$JADyear <- as.factor(ans$JADyear)
  return(ans)
}
exploreFdplusDistribs.avgMax <- function(mdscore){
  mdscore.use <- mdscore[which(! is.na(mdscore$avg)),]; avgMax <- mdscore.use[, c("avg", "max")]; summary(avgMax)
  mdscore.use <- mdscore.use[which(mdscore.use$pattern == 'competitive'),]
  avgMax <- mdscore.use[, c("avg", "max")]; summary(avgMax)
  aVar <- avgMax$avg
  # myFixArg <- quantile(avgMax$max, probs=c(90/100)) # median(avgMax$max)
    # alt: quantile(avgMax$max, probs=c(90/100)) possibly separately for different Pattern
  return(exploreFdplusDistribs(aVar)) # , myFixArg)
}
exploreCensDistribsTBD <- function(d){
  require(fitdistrplus)
    # [Many extensions ... planned in the future: we target to extend to censored data some methods for the moment
    # only available for non-censored data, especially concerning goodness-of-t evaluation and fitting methods.]
  distribs.explore <- c("norm", "beta", "unif", "binom", "nbinom", "geom", "hyper", "pois")
    # c("norm", "beta", "unif", "lnorm")
  promptToContinue("distribs chosen? its fixArg ok, eg, beta (0,1) only?")
  cFitMethods <- c("mle", "mle", "mme", "qme")

  ixDist <- 8
  fd <- fitdistcens(d, distribs.explore[ixDist])
    # [As for non censored data, one or more parametric distributions can be fitted to the censored data set]
    # [start= A named list giving the initial values of parameters of the named distribution]
  bd <- bootdistcens(fd, niter=101) # [only proposes nonparametric
    # bootstrap. Indeed, it is not obvious to simulate censoring within a parametric bootstrap resampling procedure.]
    # [We choose a low number of bootstrap replicates in order to satisfy CRAN running times
    # constraint. For practical applications, we recommend to use at least niter=501 or niter=1001.]
  # print(bd)
  summary(bd)
  plot(bd)
  print(quantile(bd))
  CIcdfplot(bd, CI.output = "quantile")
  # plotdistcens(d, distr=distribs.explore[ixDist], para=TBD, leftNA=x.leftNA, Turnbull=TRUE) # from bd
    # beware: [if TRUE the Turnbull algorithm is used ... and arguments leftNA and rightNA are not used]
    # [second method may be interesting but is certainly less rigorous than the Turnbull method that should be prefered.]
    # [para=named list giving the parameters of the named distribution ... may be omitted only if distr is omitted.]
}

cProbSafe <- 0.10 # 0.05
minSafe <- function(x, prob=cProbSafe){
  ans <- quantile(x, prob)
  return(ans)
}

require(fitdistrplus)
CRLmarks <- getCRLmarks()
aggregate(CRLmarks["rMarks"], by=list(CRLmarks$JADyear), FUN=min)
#   Group.1  rMarks
# 1    2013 35.0000
# 2    2014 35.0000
# 3    2016 20.1613
# 4    2017 34.9727
# 5    2018 25.0000
aggregate(CRLmarks["rMarks"], by=list(CRLmarks$JADyear), FUN=minSafe)
#   Group.1   rMarks
# 1    2013 36.11110
# 2    2014 36.11110
# 3    2016 20.88706
# 4    2017 36.06560
# 5    2018 26.38890

for(levY in levels(CRLmarks$JADyear)){
  # levY <- levels(CRLmarks$JADyear)[5] # for example
  md <- CRLmarks[CRLmarks$JADyear==levY,]
  yimp.start <- tail(md$ComRankL, 2)[1] + 2*cCRLinterval
  yimp.start <- ifelse(tail(md$ComRankL, 1) + cCRLinterval < yimp.start, yimp.start - cCRLinterval, yimp.start)
    # coz a non-uniformly-gapped CRL might have been appended to include the data limit
  yimp.seq <- seq(yimp.start, max(md$appearedIn2Papers), cCRLinterval)
  ximp.seq <- rep(NA, length(yimp.seq))
  x <- c(md$Marks, ximp.seq); y <- c(md$ComRankL, yimp.seq)
  x.min <- min(x, na.rm=TRUE) # {marks discrete | rMarks ~continuous | ranks}
  x.leftNA <- c.rMarks.leftNA * ifelse(x.min <= 1, 1, max(md$maxMarks)) # else real limit
  d <- data.frame(
    left= ifelse(is.na(x) | (x <= x.min), NA, x),   # Assign left side as NA when censored
    right=ifelse(is.na(x) | (x <= x.min), x.min, x) # Assign right side as x.min when censored
    # beware: not || coz that returns scalar, whereas | can return vector!
  )

  # descdist(d, discrete=TRUE) # Error in descdist(d) : data must be a numeric vector
  plotdistcens(d, NPMLE=TRUE) # [uses another more performant non parametric maximum likelihood estimation (NPMLE)
    # approach developped by Wang ... by calls to the npsurv function from the npsurv package. ...
    # Grey filled rectangles in such a plot represent the regions of non uniqueness of the NPMLE ECDF.]
  plotdistcens(d, NPMLE=FALSE) # [A less rigorous but sometimes more illustrative plot ...
    # This plot enables to see the real nature of censored data, as points and intervals, but the diculty in building
    # such a plot is to dene a relevant ordering of observations.]
}

Dscores.org <- getDscores()
Dscores <- Dscores.org[which(Dscores.org$subject != 'S'),]; Dscores <- droplevels(Dscores) # eg, drop for Subject "S"
summary(Dscores)
print(aggregate(Dscores, by=list(Dscores$pattern), FUN=summary))
ind.ptiles <- aggregate(Dscores['ind'], by=list(Dscores$subPattern), FUN=myQtiles)
colnames(ind.ptiles)[2] <- ""; print(ind.ptiles)
myBoxplots(Dscores[,"ind"], Dscores$subPattern, Dscores)
ind.ptiles <- aggregate(Dscores['ind'], by=list(Dscores$pattern, Dscores$subject), FUN=mySummary)
# > print(ind.ptiles)
#       Group.1 Group.2   ind.Min. ind.1st Qu. ind.Median   ind.Mean ind.3rd Qu.   ind.Max.
# 1 competitive       P  0.0000000   0.3415000  0.5250000  0.4875484   0.6525000  0.8800000
# 2          PU       P  0.6000000   0.7550000  0.8200000  0.8107500   0.8860000  0.9200000
# 3 competitive       C  0.0000000   0.2300000  0.3870000  0.4432759   0.6780000  1.0000000
# 4          PU       C  0.4100000   0.7000000  0.7600000  0.7639412   0.8800000  0.9710000
# 5 competitive       M -0.0480000   0.3440000  0.5000000  0.5007586   0.6560000  1.0000000
# 6          PU       M  0.5800000   0.8450000  0.8800000  0.8500000   0.9200000  0.9600000
# 7 competitive       N  0.2370000   0.3600000  0.5080000  0.4545556   0.5270000  0.6780000

# --- fit distribution for outcome variable of interest:
require(fitdistrplus)
# exploreFdplusDistribs.avgMax(Dscores)
exploreFdplusDistribs(Dscores[Dscores$pattern=='competitive', "ind"], distribs.explore=c(c.distribs.explore[1]),
  CIcdf.xq=c(0.35,0.10))
print("right of green vertical line & above blue horizontal line is where merit is recognized & scored almost certainly")
# Goodness-of-fit statistics
#                              1-mle-norm
# Kolmogorov-Smirnov statistic 0.04225611
# Cramer-von Mises statistic   0.02258332
# Anderson-Darling statistic   0.24346067
#
# Goodness-of-fit criteria
#                                1-mle-norm
# Akaike's Information Criterion   13.52595
# Bayesian Information Criterion   18.69588

exploreFdplusDistribs(Dscores[Dscores$subPattern=='JMA', "ind"], distribs.explore=c(c.distribs.explore[1]),
  CIcdf.xq=c(0.35,0.10))
exploreFdplusDistribs(Dscores[Dscores$pattern=='PU', "ind"], # distribs.explore=c(c.distribs.explore[1]),
  CIcdf.xq=c(0.75,0.10), # PU min 75% for JEE-Advanced selection. Each subject?
  c.niter=101) # from a small number of bootstrap iterations
  # gamma, beta, norm preferred considering AIC, AD?  But CI shows: 0.4+ risk of missing PU cutoff; at best 0.15 risk

# CDF plot shows horizontal lines: discrete distribution, rather than continuous?
# ref http://www.stat.rice.edu/~dobelman/courses/DistributionCompendium.pdf:
# for right skew: consider actuar::Burr, LogNormal, InverseNormal (=Wald), Chi, Gamma, Gumbel (=Gompertz),
# Fisk, ExtremeLB.
# for left skew: GenLogistic, HyperbolicSecant.
# for symmetric: Logistic (could be alt for Normal/Gaussian), Students-T.
# versatile: Weibull (=Frechet, gen of Rayleigh & Exponential & ExtremeLB).

# ref https://stackoverflow.com/questions/50025858/is-there-a-pure-markdown-way-to-include-two-images-side-by-side-in-jupyter?noredirect=1&lq=1
# for awesome 45% sizing of images side by side!

myCdfcompGofQtiles <- function(fds, distribs.explore, aVar.label, labelSide=4,
  cQprobs=seq(0.45, 0.65, by=0.05), dataLimit=0.35){ # coz 35% JAD rMarks min for safe CRL
  require(fitdistrplus)
  par(mfrow=c(2, 2)) # was mistakenly: rep(round(sqrt(length(distribs.explore))), 2)
  denscomp(fds, legendtext=distribs.explore); abline(v=dataLimit, col="gray60", lty="dotdash")
  qqcomp(fds, legendtext=distribs.explore)
  cdfcomp(fds, legendtext=distribs.explore); abline(v=dataLimit, col="gray60", lty="dotdash")
  ppcomp(fds, legendtext=distribs.explore)
  mtext(aVar.label, side=labelSide)
  promptToContinue("?")

  print(gofstat(fds))
    # considering aVar.lim=0.35 (near mode), prefer beta weibull gamma, unless tail or other consideration
  cdfcomp(fds,
    xlogscale=TRUE, ylogscale=TRUE, # magnify initial left tail
    legendtext=distribs.explore)
  abline(v=dataLimit, col="gray60", lty="dotdash")
  mtext(aVar.label, side=labelSide)
  promptToContinue("?")

  diQtiles <- list()
  for(di in distribs.explore){
    diQtiles[[di]] <- as.numeric(quantile(fds[[di]], probs=cQprobs)$quantiles)
  }
  empir.data <- fds[[1]]$data
  diQtiles[["empirical"]] <- as.numeric(quantile(empir.data, probs=cQprobs))
  diQtiles.df <- t(as.data.frame(diQtiles)); colnames(diQtiles.df) <- cQprobs; print(diQtiles.df)

  return(diQtiles.df)
}

  aVar <- Dscores$avg[Dscores$pattern=='competitive']; aVar <- aVar[!is.na(aVar)]
  aVar.label <- "Dscores$avg[Dscores$pattern=='competitive']"; aVar.lim <- 0.35 # min JAD rMarks or marks%
  print(paste0(aVar.label, ": un-imputed data; so has NAs. What is CICDF for min JAD ", aVar.lim, "?"))
  distribs.explore <- c.distribs.explore[-3] # -3 to drop "unif". -2 to drop "beta" where unable to est eg $max

  c.niter <- 101
  hist(aVar) # appears right skewed
  mtext(aVar.label, side=4)
  promptToContinue("?")
  descdist(aVar, discrete=FALSE, method="unbiased", graph=TRUE, boot=c.niter)
  mtext(aVar.label, side=4)
  promptToContinue("?")

  fds <- list()
  for(d in distribs.explore){
    fds[[d]] <- fitdist(aVar, d)
    print(summary(fds[[d]]))
  }

# ref https://stats.stackexchange.com/questions/376634/how-to-pick-starting-parameters-for-massfitdist-with-the-beta-distribution
beta_mom <- function(x) {

  m_x <- mean(x, na.rm = TRUE)
  s_x <- sd(x, na.rm = TRUE)

  alpha <- m_x*((m_x*(1 - m_x)/s_x^2) - 1)
  beta <- (1 - m_x)*((m_x*(1 - m_x)/s_x^2) - 1)

  return(list(alpha = alpha, beta = beta))

}
  # beware: penalize unnecessarily complex (parametric) distributions! prefer the simple.
  # [Until here, we did not have to define starting values (in the optimization process) as reasonable starting values are
  # implicity defined within the fitdist ...
  # the distribution parameters have to be supplied in the argument start, as a named list with initial values for each
  # parameter (as they appear in the d, p, q functions). Having defined reasonable starting values1, various distributions
  # can be fitted and graphically compared.
  # 1The plotdist function can plot any parametric distribution with specified parameter values in argument para. It can
  # thus help to find correct initial values for the distribution parameters in non trivial cases, by iterative calls if
  # necessary (see the reference manual for examples
  # ]
  require("actuar")
  require(extraDistr) # for nsbeta (Non-Standard Beta) distribution [a,b] where a < b and [a,b]==[0,1] is not necessary
    # beware: pkg masks some from actuar

  fds1 <- list()
  d <- "nsbeta" # dnsbeta(x, shape1, shape2, min = 0, max = 1, log = FALSE)
  d.estBasis <- "beta"
  # d <- distribs.explore.chosen[1]
  # I don't now have much competence to choose init/start estimates for various distributions
  # >   fds1[[d]] <- fitdist(aVar, d, start=startList, min=-0.25, max=1.00)
  # <simpleError in fn(par, ...): unused arguments (min = -0.25, max = 1)>
  # Error in fitdist(aVar, d, start = startList, min = -0.25, max = 1) : 
  #   the function mle failed to estimate the parameters, 
  #                 with the error code 100

  startList <- namedVecToNamedList(fds[[d.estBasis]]$estimate)
  fds1[[d]] <- fitdist(aVar, d, start=startList) # without myMinMax
  if(d=="nsbeta"){ # try start= which might fail to estimate with MLE!
    # beware: for myMinMax, might have to consider method="mme" with order= arg if MLE fails to estimate parameters
    myMinMax <- c(-0.25, 1.00)
    myStartList1 <- list(shape1=startList$shape1, shape2=startList$shape2, min=myMinMax[1], max=myMinMax[2])
    fds1d.alt1 <- fitdist(aVar, d, start=myStartList1)
    bmom <- beta_mom(aVar)
    myStartList2 <- list(shape1=bmom$alpha, shape2=bmom$beta, min=myMinMax[1], max=myMinMax[2])
    fds1d.alt2 <- fitdist(aVar, d, start=myStartList2)
      # min= max= considers up to 1/4th -ve marks/ question
      # beware: including log=FALSE fails coz <... "log" matched by multiple actual arguments>
    # jusTest <- dnsbeta(aVar, shape1=startList$shape1, shape2=startList$shape2, min=-0.25, max=1.00, log=FALSE)
    fds1[[d]] <- fds1d.alt1 # or fds1d.alt2
  } # else continue
  print(summary(fds1[[d]])) # not specifying min max in start= alters parameter estimates only slightly (from Beta)
  fds[[d]] <- fds1[[d]]; distribs.explore <- c(distribs.explore, d)

  # fendo.ll <- fitdist(aVar, "llogis", start = list(shape = 1, scale = 500))
  #   # [dllogis(x, shape, rate = 1, scale = 1/rate, log = FALSE)]. so apparently, at least shape= must be specified
  # fendo.P <- fitdist(aVar, "pareto", start = list(shape = 1, scale = 500))
  # fendo.B <- fitdist(aVar, "burr", start = list(shape1 = 0.3, shape2 = 1, rate = 1))
  # gofstat(list(fendo.ln, fendo.ll, fendo.P, fendo.B)) # , fitnames = c("lnorm", "llogis", "Pareto", "Burr"))

  diQtiles.df <- myCdfcompGofQtiles(fds, distribs.explore, aVar.label, labelSide=4,
    cQprobs=seq(0.45, 0.65, by=0.05), # primarily 0.55 as per CDFplot
    dataLimit=0.35) # coz 35% JAD rMarks min for safe CRL
  print("prefer gamma, considering this xylogscale view of initial left tail fit. else lognormal? nsbeta?")
  distribs.explore.chosen <- c.distribs.explore[c(6,2,4)] # c("gamma", "beta", "weibull")
  print("gamma quantile probs=0.65 at 36% marks: relatively least! nsbeta 37% marks")
  # as.numeric(quantile(fds[["nsbeta"]], probs = c(0,0.05))$quantiles)
  myc.niter <- 501
  myCIcdf.xq <- c(0.35,0.10)
    # myCIcdf.xq <- c(0.55,0.10) # $max seeks upper limit (say 0.55) rather than $avg min 0.35 cutoff in c(0.35,0.10)
  exploreFdplusDistrib.CInew(aVar, fds[[d]], distribs.explore=d, # "nsbeta"
    CIcdf.xq=myCIcdf.xq, c.niter=myc.niter)
    # c(0.55,0.10), c.niter=501) # $max seeks upper limit (say 0.55) rather than $avg min 0.35 cutoff in c(0.35,0.10)

  fendo.B <- fitdist(aVar, "burr", start = list(shape1 = 0.3, shape2 = 1, rate = 1))
  cdfcomp(list(fendo.B,  fds[[distribs.explore[6]]]), xlogscale=TRUE, ylogscale=TRUE)

  myGofs <- c("ADR", "AD2R") # for left weight: c("ADL", "AD2L"). else for right weight: c("ADR", "AD2R")
  myLogscale <- "L" %in% substrRight(myGofs, 1) # Left-tail emphasis calls for log-scale axes
stop() TBD
  # ref https://stats.stackexchange.com/questions/84076/negative-values-for-aic-in-general-mixed-model: lesser AIC
  # indicates a better fit even if AIC is more -ve, unless:
  # ref https://www.r-bloggers.com/how-do-i-interpret-the-aic/ "AIC basic principles"
  # if some models having high +ve AIC and some -ve AIC
  # might indicate models fitted to different data sets or different modeling tools coz different additive constants.

  # [As giving more weight to distribution tails, the Anderson-Darling statistic is of special interest when it matters to
  # equally emphasize the tails as well as the main body of a distribution. This is often the case in risk assessment ...
  # Moreover, such a statistic, as Cramer-von Mises and Kolmogorov-Smirnov ones ... could systematically promote the
  # selection of the more complex distributions in the other case. Looking at classical penalized criteria based on the
  # loglikehood (AIC, BIC) seems thus also interesting, especially to discourage overfitting.]
  # [method to "mge" in the call to fitdist and to specify the argument gof coding for the chosen goodness-of-fit
  # distance. This function is intended to be used only with continuous non-censored data.
  # Maximum goodness-of-fit estimation may be useful to give more weight to data at one tail of the distribution.]
  fds.AD <- fds.AD2 <- list()
  # for(d in distribs.explore.chosen){
    # d <- distribs.explore.chosen[1]
    fds.AD[[d]] <- fitdist(aVar, d, method="mge", gof=myGofs[1])
    fds.AD2[[d]] <- fitdist(aVar, d, method="mge", gof=myGofs[2])
    print(summary(fds.AD[[d]])) # changed AIC slightly, estimates altered!
    print(summary(fds.AD2[[d]])) # changed AIC & estimates!
  # }
  # d <- distribs.explore.chosen[1]
  cdfcomp(list(fds[[d]], fds.AD[[d]], fds.AD2[[d]]),
    xlogscale=myLogscale, ylogscale=myLogscale, # magnify initial left tail if needed
    # main=paste(d, "distribution being fitted"),
    xlegend="bottomright",
    legendtext=paste0(d, ":", c("MLE", paste0("Tail", myGofs))))
  mtext(aVar.label, side=4)
  abline(v=myCIcdf.xq[1], col="green", lty="dotdash")
  abline(h=myCIcdf.xq[2], col="blue", lty="dotdash")
  promptToContinue("?")
  print("prefer gamma:AD2?")

stop()
  (HC5.estimates <- c(
    empirical = as.numeric(quantile(aVar, probs = 0.05)),
    Burr = as.numeric(quantile(fendo.B, probs = 0.05)$quantiles),
    lognormal_MLE = as.numeric(quantile(fendo.ln, probs = 0.05)$quantiles),
    lognormal_AD2 = as.numeric(quantile(fendo.ln.ADL, probs = 0.05)$quantiles),
    lognormal_AD2L = as.numeric(quantile(fendo.ln.AD2L, probs = 0.05)$quantiles)))

  # [
  # MME method provides a more cautious estimation of the insurance risk as the MME-fitted distribution function (resp.
  # MLE-fitted) underestimates (overestimates) the empirical distribution function for large values of claim amounts. ...
  # Maximum likelihood and moment matching estimations are certainly the most commonly used method for fitting
  # distributions (Cullen and Frey, 1999). Keeping in mind that these two methods may produce very different results,
  # the user should be aware of its great sensitivity to outliers when choosing the moment matching estimation. This may
  # be seen as an advantage in our example if the objective is to better describe the right tail of the distribution,
  # but it may be seen as a drawback if the objective is different. ...
  # Each time a numerical minimization is carried out ... the optim function of the stats
  # package is used by default with the "Nelder-Mead" method for distributions characterized by more than one parameter
  # and the "BFGS" method for distributions characterized by only one parameter. Sometimes the default algorithm fails
  # to converge. It is then interesting to change some options of the optim function or to use another optimization
  # function ... Even if no error is raised when computing the optimization, changing the algorithm is of particular
  # interest to enforce bounds on some parameters
  # ]
