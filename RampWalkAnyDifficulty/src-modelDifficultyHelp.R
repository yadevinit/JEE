getBaseContrastVsY <- function(facXname, Yname, dat, myFUN=mean, wantHigh=TRUE){ # for setting contr.treatment(... base=).
  YbyX.myFUN <- aggregate(dat[,Yname], by=list(dat[,facXname]), FUN=myFUN)
  YbyX.myFUN <- YbyX.myFUN[order(YbyX.myFUN[,2], decreasing=wantHigh),]
    # order(YbyX.myFUN[,Yname] or order(YbyX.myFUN[,2])?
  ans <- contr.treatment(levels(dat[,facXname]), grep(YbyX.myFUN[1,1], levels(dat[,facXname])))
  return(ans)
}
cLowLimWorryDispersion <- 1.2
  # ref https://mac-theobio.github.io/QMEE/Generalized_linear_models.html says of overdispersion:
  # [too much variance: (residual deviance)/(residual df) should be ˜1. (Ratio >1.2 worrisome; ratio>3,
  # v. worrisome (check your model & data!)].
myGOFglm.stat <- function(fit){
  # ref https://data.princeton.edu/wws509/r/overdispersion:
  # Beware: [The over-dispersed Poisson and negative binomial models have different variance functions.]
  if("negbin" %in% class(fit)){ # Was: class(fit.glmpois1)
    fit.theta <- fit$theta; fit.estVar <- 1 / fit.theta # fit$theta==NULL if not negbinom.
  } else {
    fit.theta <- as.numeric(NA); fit.estVar <- as.numeric(NA) # Instead of as.numeric(NA) if just NA, it becomes `logical`
  }
  fit.logLik <- logLik(fit) # summary() shows (-2 * logLik(fit)).
  chkDispersion <- deviance(fit) / df.residual(fit)
  diags <- list(deviance(fit), df.residual(fit), fit.theta, AIC(fit), fit.logLik, fit.estVar, chkDispersion)
  diags.df <- data.frame(diags)
  colnames(diags.df) <- c("deviance", "df", "theta (Shape or Dispersion for NegBinom)",
    "AIC", "logLik", "estVar", "chkDispersion")
  diags.df <- round(diags.df, cDecimalDigits)
  # Was:
  # print(paste0(
  #   "deviance, df, theta (viz. Shape or Dispersion parameter for Negative Binomial), AIC, logLik, ",
  #   "estVar, chkDispersion"))
  # print(round(c(deviance(fit), df.residual(fit), fit$theta, AIC(fit), fit.logLik, fit.estVar, chkDispersion),
  #   cDecimalDigits))
    # https://stats.stackexchange.com/questions/27773/how-does-glm-nb-work says:
    #   [(Dispersion parameter for Negative Binomial(163.3237) family taken to be 1) ...
    #   (Edit: The estimated shape parameter in your example is 163.32)].
  return(diags.df)
}
myGOFglm <- function(fit, lowLimWorryDispersion=cLowLimWorryDispersion, withEffects=FALSE){
  fit.stat <- myGOFglm.stat(fit)
  # https://stats.stackexchange.com/questions/66586/is-there-a-test-to-determine-whether-glm-overdispersion-is-significant
  # [odTest from the pscl library which compares the log-likelihood ratios of a Negative Binomial regression to the
  # restriction of a Poisson regression mu=var. ...
  # Here the null of the Poisson restriction is rejected in favour of my negative binomial regression NegBinModel. Why?
  # Because the test statistic 52863.4998 exceeds 2.7055 with a p-value of < 2.2e-16.]
  if("negbin" %in% class(fit)){ # Was: class(fit.glmpois1)
    print(odTest(fit)) # this function only works for objects of class negbin
  } # else continue

    # ref https://stats.stackexchange.com/questions/365623/how-to-report-negative-binomial-regression-results-from-r:
    # [linkinv function that comes with the model fit. This is a safer way to back transform from glm fits as you
    # can't accidentally use the wrong function if you change your link function.]
  if(withEffects){
    library(effects)
    print(summary(allEffects(fit)))
  } # else continue

  par(ask=TRUE)
  print(coeftest(fit, vcov=sandwich)); print(summary(fit)); plot(fit) # summary.negbin(... dispersion=1) by default.
  stopifnot(fit.stat$chkDispersion <= lowLimWorryDispersion)
    # [R's theta is the precision of the multiplicative random effect].
    # https://stats.stackexchange.com/questions/10419/what-is-theta-in-a-negative-binomial-regression-fitted-with-r:
    # [theta is the shape parameter of the negative binomial distribution, and no, you cannot really interpret it as
    # a measure of skewness. ...
    # usually interpreted as a measure of overdispersion with respect to the Poisson distribution. ...
    # There seems to be a lot of misinformation about the negative binomial model, and especially with respect to the
    # dispersion statistic and dispersion parameter.]
  return(fit.stat$chkDispersion)
}
doGroupVec.markScheme <- function(mslevVec, negPattern="mark-"){
  grp <- rep("+ve", length(mslevVec))
  grp[grep(negPattern, mslevVec)] <- "-ve"
  return(grp)
}
getCoefs.stdEst <- function(fit.chosen, decreasingAbsImpact=c(NULL,NA,TRUE,FALSE)){
stop() # unable to get beta stdized coefs!!
  # Consider http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/glm.html.

#https://stats.stackexchange.com/questions/46312/how-to-get-the-standardized-beta-coefficients-from-glm-nb-regression-in-r
  # ref https://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r for any one of:
  # detach("package:QuantPsyc", unload=TRUE) # if error, add character.only=TRUE
  # unloadNamespace("QuantPsyc")
  # library(QuantPsyc)
  # BEWARE: http://finzi.psych.upenn.edu/R/library/QuantPsyc/html/lm.beta.html and
  # https://rdrr.io/cran/QuantPsyc/man/lm.beta.html say:
  # [This function does not produce 'correct' standardized coefficients when interaction terms are present].
  # https://github.com/cran/QuantPsyc/blob/master/R/lm.beta.R seems to have older code.

  # BEWARE: following seems to be a different pkg:
  # ref https://cran.r-project.org/web/packages/lm.beta/lm.beta.pdf:
  # # standardize
  # lm.D9.beta <- lm.beta(lm.D9)
  # print(lm.D9.beta)
  # summary(lm.D9.beta)
  # coef(lm.D9.beta)

  # stdBetaFromBcoefs <- QuantPsyc::lm.beta(fit.chosen) # Fails even after installing from source on R-3.6.2.
    # [Error ... Calling var(x) on a factor x is defunct.
    # Use something like 'all(duplicated(x)[-1L])' to test for a constant vector.]
  # Alt: reghelper::beta() ref https://rdrr.io/cran/reghelper/man/beta.html.
  library(reghelper)
  # ref https://cran.r-project.org/web/packages/reghelper/reghelper.pdf
  stdBetaFromBcoefs <- reghelper::beta(fit.chosen)
  if(is.null(decreasingAbsImpact) || is.na(decreasingAbsImpact)){
    ord.coefs <- 1:nrow(stdBetaFromBcoefs) # or length()?
  } else {
    ord.coefs <- order(abs(stdBetaFromBcoefs), decreasing=decreasingAbsImpact)
  }
  print(stdBetaFromBcoefs[ord.coefs])

  # QQ plot not straight line if cOutNamesNew==notrOtot3!!
    # now 2020Feb27: YP has some significant impact.
  # fit <- MASS::glm.nb(update(mdl.spec, ~ . - qaType.grp - 1 - YP), data=myData); m2 <- fit
    # Earlier 2019July: drop YP too coz insignificant impact
  # coeftest(fit, vcov=sandwich); summary(fit); plot(fit)

  est.betahat <- round(exp(cbind(Estimate=coef(fit.chosen), confint(fit.chosen))), cDecimalDigits)
  print(est.betahat) # Alt: print(est.betahat[ord.coefs,])

  # ref: https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
  # We might be interested in looking at incident rate ratios rather than coefficients. So, exponentiate
  # incident rate (IRR) for levelA is Est times the incident rate for the reference group
  # For a constant value of other variables, for every one-point increase in chosen variable (in the following),
  # estimated odds of modeled response are multiplied by ...  exp(betahat) iff logit link for Odds Ratio
  # betahat <- fit$coefficients; round(exp(betahat), cDecimalDigits)
  # The two degree-of-freedom chi-square test if < alpha=0.05 indicates that factor is a statistically significant
  # predictor of response

  # TBD: additionally, you can predict (and plot) for a data set to get what the model generates:
  # lines(myYear, predict(fit, type="response", list(year=myYear)))
  fit.pred.link <- predict(fit.chosen, type="link"); fit.pred.resp <- predict(fit.chosen, type="response")
  print(str(fit.pred.link)); print(str(fit.pred.resp))
  plot(fit.pred.link); plot(fit.pred.resp)

  # fit <- glm(rOtot ~ ., weights=tot, family=binomial(link="logit"))
    # As a numerical vector with values between 0 and 1, interpreted as the proportion of successful cases (with the
    # total number of cases given by the weights)
  # fit <- glm(cbind(right, (tot - right)) ~ ., data=myData, weights=tot, family=binomial(link="logit"))
    # As a two-column integer matrix: the first column gives the number of successes and the second the number of failures
    # weights when response has counts, e.g., for binomial GLM, prior weights are used to give number of trials

  ans <- list(fit.chosen.stdBetaFromBcoefs, est.betahat)
  return(ans)
}
myCompareFits <- function(arg.mdls){
  # m1 <- glm.nb(); m2 <- update(m1, . ~ . - X1 + X2); anova(m2, m1)
  # R: Likelihood Ratio Tests for Negative Binomial GLMs.
  fits.anova <- do.call(anova, arg.mdls)
  fits.anova.colnames <- colnames(fits.anova)
  stopifnot(fits.anova.colnames == c("Resid. Df","Resid. Dev","Df","Deviance"))
  fits.lrtest <- do.call(lmtest::lrtest, arg.mdls)
  # print(fits.anova); print(fits.lrtest)
  al <- cbind(fits.anova, fits.lrtest)
  diags.mdls <- lapply(arg.mdls, FUN=myGOFglm.stat)
  # ref https://stackoverflow.com/questions/2851327/convert-a-list-of-data-frames-into-one-data-frame:
  diags.mdls.df <- do.call(rbind, diags.mdls) # ; str(diags.mdls.df)
  al.diags <- cbind(al, diags.mdls.df); print(str(al.diags))
  return(al.diags)
}
