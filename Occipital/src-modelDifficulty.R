# TBD 2020Feb28:
# - Share preliminary draft with IISc+PanIIT forum, DrAA, MHRD, Harish, FIITJEE, Resonance, APU+ProfS.
# - Explore interaction effects eg multi-topic ones.
# - Include ntokens and FKmeasure into covariates.
# - Difficulty-modeling insights -> Distractor design -> Learning to Pre-empt failing to Distractors ...
#   "Be Vidyashankar meditatively while Rambha Menaka Urvashi Apsara distractors dance to tempt."
# - Para for Qs cleaning in QOSs.

# Beware: modeling here with lesser digits of precision in notrOtot3 than was done for JEEinsight (2019-July).
doGroup.qaType <- function(qat){
  grp <- switch(qat,
    MultCorrAns = qat,
    SingCorrAns = qat,
    SingDigitInteger = qat,
    "notMultSingCorrDigit" # by default ie for c("", "Compr", "NumAns", "ParaSingleAns", "TwoListMatchSingleAns")
  )
  return(grp)
}
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
myGOFglm <- function(fit, lowLimWorryDispersion=cLowLimWorryDispersion, withEffects=FALSE){
  # ref https://data.princeton.edu/wws509/r/overdispersion:
  # Beware: [The over-dispersed Poisson and negative binomial models have different variance functions.]
  fit.estVar <- 1 / fit$theta; fit.logLik <- logLik(fit) # summary() shows (-2 * logLik(fit)).
  chkDispersion <- deviance(fit) / df.residual(fit)
  print(
    "deviance, df, theta (viz. Shape or Dispersion parameter for Negative Binomial), AIC, logLik, estVar, chkDispersion")
  print(round(c(deviance(fit), df.residual(fit), fit$theta, AIC(fit), fit.logLik, fit.estVar, chkDispersion),
    cDecimalDigits))
    # https://stats.stackexchange.com/questions/27773/how-does-glm-nb-work says:
    #   [(Dispersion parameter for Negative Binomial(163.3237) family taken to be 1) ...
    #   (Edit: The estimated shape parameter in your example is 163.32)].

  # https://stats.stackexchange.com/questions/66586/is-there-a-test-to-determine-whether-glm-overdispersion-is-significant
  # [odTest from the pscl library which compares the log-likelihood ratios of a Negative Binomial regression to the
  # restriction of a Poisson regression mu=var. ...
  # Here the null of the Poisson restriction is rejected in favour of my negative binomial regression NegBinModel. Why?
  # Because the test statistic 52863.4998 exceeds 2.7055 with a p-value of < 2.2e-16.]
  print(odTest(fit))

    # ref https://stats.stackexchange.com/questions/365623/how-to-report-negative-binomial-regression-results-from-r:
    # [linkinv function that comes with the model fit. This is a safer way to back transform from glm fits as you
    # can't accidentally use the wrong function if you change your link function.]
  if(withEffects){
    library(effects)
    print(summary(allEffects(fit)))
  } # else continue

  par(ask=TRUE)
  print(coeftest(fit, vcov=sandwich)); print(summary(fit)); plot(fit) # summary.negbin(... dispersion=1) by default.
  stopifnot(chkDispersion <= lowLimWorryDispersion)
    # [R's theta is the precision of the multiplicative random effect].
    # https://stats.stackexchange.com/questions/10419/what-is-theta-in-a-negative-binomial-regression-fitted-with-r:
    # [theta is the shape parameter of the negative binomial distribution, and no, you cannot really interpret it as
    # a measure of skewness. ...
    # usually interpreted as a measure of overdispersion with respect to the Poisson distribution. ...
    # There seems to be a lot of misinformation about the negative binomial model, and especially with respect to the
    # dispersion statistic and dispersion parameter.]
  return(chkDispersion)
}


if(FALSE){ # init
  responses.stmTheta <- datJADplus <- getFiledQOSs(suffix=suffix.restmt, doUnlistRec=FALSE)
} # else continue
JEEdata.use <- responses.stmTheta

### --- following code reused from src-from-azureJEEinisght.R (2019-Nov-05):
cDecimalDigits <- 3
# RELATE IN:
QOS.nchar <- unlist(lapply(JEEdata.use["QOS"], FUN=nchar)); print(summary(QOS.nchar))
colnames(JEEdata.use) <- gsub(pattern="jeeadv", replacement="", x=colnames(JEEdata.use))
with(JEEdata.use, table(qaType, YP)) # table(exclude=) levels to remove for all factors in ...
with(JEEdata.use, table(qaType, subject))
qaType.grp <- JEEdata.use$qaType
levels(qaType.grp) <- unlist(lapply(levels(JEEdata.use$qaType), FUN=doGroup.qaType))
with(JEEdata.use, table(markScheme, YP)) # -ve and +ve markScheme could be grouped, but no finer coz biased sample
markScheme.grp <- JEEdata.use$markScheme
doGroupVec.markScheme <- function(mslevVec, negPattern="mark-"){
  grp <- rep("+ve", length(mslevVec))
  grp[grep(negPattern, mslevVec)] <- "-ve"
  return(grp)
}
levels(markScheme.grp) <- doGroupVec.markScheme(levels(markScheme.grp))
  # was: c("-ve", "-ve", "-ve", "+ve", "+ve") # {-103 -104 -201234} {+02 +03} and no finer
JEEdata.use <- cbind(JEEdata.use, qaType.grp=qaType.grp, markScheme.grp=markScheme.grp, QOS.nchar=QOS.nchar)
summary(JEEdata.use)

JEEdata.use <- JEEdata.use[JEEdata.use$uOtot3 > 0,]; nrow(JEEdata.use)
# RELATE OUT:
cProbs <- c(0, 0.01, 0.05, 0.1, 0.25, 0.333); cProbs <- sort(c(cProbs, 0.5, 1 - cProbs))
require(corrplot)
summary(JEEdata.use)

# MODEL OUT-IN:
cInNames <- c("subject", "YP", "qaType", "qaType.grp", "markScheme.grp",
  "QOS.nchar",
  paste0("topicProp", 1:7)) # Topic Proportion (or Prevalence) from STM model fitting.
summary(JEEdata.use[,cInNames])
# cInNames <- cInNames[-(2:2)]
cOutNames <- "rOtot3" # was: "notrOtot3" # "log(notrOtot)". was: "right".
mdl.spec <- as.formula(paste(cOutNames, "~", paste(cInNames, collapse=" + "))); mdl.spec
mdl.spec.plot <- update(mdl.spec, log(.) ~ .); mdl.spec.plot # replace LHS with log(LHS)
plot(mdl.spec.plot, data=JEEdata.use)
plot(log(rOtot3) ~ qaType * markScheme.grp, data=JEEdata.use)
aggregate(JEEdata.use[cOutNames] /1000, by=list(JEEdata.use[,"YP"]), FUN=mean)
  # was: FUN=function(x) round(median(x, na.rm=TRUE), cDecimalDigits))
# notrOtot3.by.subject.mean <- aggregate(JEEdata.use["notrOtot3"] /1000, by=list(JEEdata.use[,"subject"]), FUN=mean)
# notrOtot3.by.subject.mean <- notrOtot3.by.subject.mean[order(notrOtot3.by.subject.mean$notrOtot3, decreasing=TRUE),]
# notrOtot3.by.subject.mean[1,1]
aggregate(JEEdata.use[cOutNames] /1000, by=list(JEEdata.use[,"qaType"]), FUN=mean)
aggregate(JEEdata.use[cOutNames] /1000, by=list(JEEdata.use[,"subject"]), FUN=mean)

  # ref: https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf
  require(MASS); require(lmtest); require(sandwich); require(pscl)
  myData <- JEEdata.use; mdl.spec.bak <- mdl.spec
  myData$qaType <- droplevels(myData$qaType)
  contrasts(myData$qaType) <- getBaseContrastVsY(facXname="qaType", Yname=cOutNames, dat=myData, wantHigh=TRUE)
  # was: contrasts(myData$qaType) <- contr.treatment(levels(myData$qaType), base=3)
    # was: "NumAns" coz that goes with lowest mean
  contrasts(myData$subject) <- getBaseContrastVsY(facXname="subject", Yname=cOutNames, dat=myData, wantHigh=FALSE)
  contrasts(myData$YP) <- getBaseContrastVsY(facXname="YP", Yname=cOutNames, dat=myData, wantHigh=FALSE)
  # was: contrasts(myData$subject) <- contr.treatment(levels(myData$subject),
  #   grep(notrOtot3.by.subject.mean[1,1], levels(myData$subject)))
    # was: base=2) # "maths" coz that has extreme mean
  # contr.treatment(n, base=1, contrasts=TRUE, sparse=FALSE); C()

  # ?glm says:
  # Non-NULL weights can be used to indicate that different observations have different dispersions (with the values in
  # weights being inversely proportional to the dispersions); or equivalently, when the elements of weights are positive
  # integers w_i, that each response y_i is the mean of w_i unit-weight observations. For a binomial GLM prior weights are
  # used to give the number of trials when the response is the proportion of successes: they would rarely be used for a
  # Poisson GLM.
  tot <- rep(log(1000), nrow(myData))
  mdl.spec <- update(mdl.spec, ~ . + offset(log(tot)))
    # append offset(log(tot)) to indicate term with a fixed coefficient of one

  fit.glmpois1 <- glm(mdl.spec, data=myData, family=poisson)
    # prefer rate of incidence (not count)
    # =loglm() cf Negative Binomial, possibly with Hurdle and Zero-Inflated Regression Models. package pscl?
    # family=negative.binomial(link="log")
    # Count data often have an exposure variable, which indicates the number of times the event could have happened.
    # This variable should be incorporated into a Poisson model with the use of the offset option.
  myGOFglm(fit.glmpois1)
  # coeftest(fit, vcov=sandwich); summary(fit); plot(fit)
  # 2020Feb27:
  #     Null deviance: 35777  on 357  degrees of freedom
  # Residual deviance: 21931  on 337  degrees of freedom
  # AIC: 24537

  fit.glmpois2 <- glm(update(mdl.spec, ~ . - qaType.grp), data=myData, family=poisson)
    # Drop qaType.grp coz coefficient NAs [Coefficients: (3 not defined because of singularities)]
    # Similarly due to NA coefficients, drop topicProp7 + topicProp2 + topicProp1.
  myGOFglm(fit.glmpois2)

  fit.glmpois3 <- glm(update(mdl.spec, ~ . - qaType.grp - topicProp7 - topicProp2 - topicProp1),
    data=myData, family=poisson)
    # Similarly due to NA coefficients, also drop - topicProp7 - topicProp2 - topicProp1.
  myGOFglm(fit.glmpois3) # Still overdispersed.

  fit.glmnb1 <- MASS::glm.nb(mdl.spec, data=myData) # glm.nb(... link=log) by default.
  myGOFglm(fit.glmnb1)
  # 2020Feb27:
  #     Null deviance: 565.14  on 357  degrees of freedom
  # Residual deviance: 374.91  on 337  degrees of freedom
  # AIC: 4451.5
  #               Theta:  3.633 
  #           Std. Err.:  0.266 
  #  2 x log-likelihood:  -4407.469

    # 2020Feb27: 374.91 / 337 = 1.11 is "not worrisome" though (Shape or) Dispersion parameter theta=3.6 estimated.
    # was 2019July: so, 715.6/661=1.08 is therefore "not worrisome"; Dispersion parameter 2.89

    # 2018::physics 1.7x r count as per NB
    # myData[c(44, 49, 66, ?99 if -subject?),] are plotted as outliers in
    # Residuals vs Fitted, Q-Q, & Scale-Location plots; investigate
    # myData[c(61, 261, 594),] are plotted as outliers in Residuals vs Leverage plot; investigate
    # prefer rate of incidence (not count). was: offset(log(wur))
    # ref https://mac-theobio.github.io/QMEE/Generalized_linear_models.html says of offsets:
    # "constant terms added to a model
    # what if we want to model densities rather than counts?
    # log-link (Poisson/NB) models: mu0=exp(beta0+beta1x1+...)
    # if we know the area then we want mu=A * mu0
    # equivalent to adding log(A) to the linear predictor (exp(log(mu0)+log(A))=mu0 * A)
    # use ... + offset(log(A)) in R formula"
    # so, by including offset(log(tot)), density of 'right' (over an "area" of 'tot' JEE candidates) is being modeled

  fit.glmnb2 <- MASS::glm.nb(update(mdl.spec, ~ . - qaType.grp), data=myData)
  myGOFglm(fit.glmnb2)

  fit.glmnb201 <- MASS::glm.nb(update(mdl.spec, ~ . - qaType.grp - 1), data=myData)
    # drop intercept coz we want to estimate relative impacts of variables, not necessarily estimate the response itself
    #   (~ + 0) or (~ . - 1) can equivalently drop the intercept
    # was: Null Deviance went up from 880 to 3394 after dropping intercept from model spec, while AIC stays 15434
  myGOFglm(fit.glmnb201)

  fit.glmnb202 <- MASS::glm.nb(update(mdl.spec, ~ . - qaType.grp - 1 - topicProp7 - topicProp2 - topicProp1), data=myData)
    # Similarly due to NA coefficients, also drop - topicProp7 - topicProp2 - topicProp1.
  myGOFglm(fit.glmnb202)

  fit.glmnb203 <- MASS::glm.nb(update(mdl.spec, ~ . -qaType.grp-1-topicProp7-topicProp2-topicProp1-topicProp6-topicProp5),
    data=myData)
  myGOFglm(fit.glmnb203)

stop()
  anova(fit.glmpois1, fit.glmpois2, fit.glmpois3, fit.glmnb1, fit.glmnb2, fit.glmnb201, fit.glmnb202, fit.glmnb203)
#   fit <- MASS::glm.nb(update(mdl.spec, ~ . - qaType.grp - 1 - YP - subject), data=myData); m3 <- fit
#  anova(m1, m2, m3) # factor subject is stat-significant predictor of response; so, avoid dropping subject and prefer m2.
    # dropping paper and qaType.grp is ok coz they are not significant predictors
  fit.chosen <- fit.glmnb203 # preferred model coz least (Resid Dev / Resid DF).

  # QQ plot not straight line if cOutNames==notrOtot3!!
    # now 2020Feb27: YP has some significant impact.
  # fit <- MASS::glm.nb(update(mdl.spec, ~ . - qaType.grp - 1 - YP), data=myData); m2 <- fit
    # Earlier 2019July: drop YP too coz insignificant impact
  # coeftest(fit, vcov=sandwich); summary(fit); plot(fit)

  est.betahat <- round(exp(cbind(Estimate=coef(fit.chosen), confint(fit.chosen))), cDecimalDigits)
  print(est.betahat)
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
  str(fit.pred.link); str(fit.pred.resp)
  plot(fit.pred.link); plot(fit.pred.resp)

  # fit <- glm(rOtot ~ ., weights=tot, family=binomial(link="logit"))
    # As a numerical vector with values between 0 and 1, interpreted as the proportion of successful cases (with the
    # total number of cases given by the weights)
  # fit <- glm(cbind(right, (tot - right)) ~ ., data=myData, weights=tot, family=binomial(link="logit"))
    # As a two-column integer matrix: the first column gives the number of successes and the second the number of failures
    # weights when response has counts, e.g., for binomial GLM, prior weights are used to give number of trials

# ---
# WAY FORWARD:
