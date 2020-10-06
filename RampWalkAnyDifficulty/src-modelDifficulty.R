  date()
  require(MASS); require(lmtest); require(sandwich); require(pscl)
# ref https://notebooks.azure.com/help/jupyter-notebooks/package-installation/r
# options(unzip = 'internal')
# library(devtools)
# install_github('<user>/<repo>')

# - Plot effect of topic 2 (or others)---possibly with covariate interactions---to illustrate *after* investigating!
#   ref https://www.jstatsoft.org/article/view/v008i15/effect-displays-revised.pdf.
#   For example, here's a theory: some papers (YP) have a relatively-harder Subject when certain Topics prevail
#   over others as evident in the questions examinees face.  YP*subject*topicPropX interaction deserves investigation.
#   In fact, the Occipital question is: what are those Topics and interacting factors that explain question difficulty?!
#   Consider including these IEs into STM too!
# - Check why contrast() didn't work. Consistently use contrast levels to highlight coeffs that lower rOtot3:
#   eg subjectP, YP2018P1, markScheme.grp+ve, qaType.
# - Dropped. https://www.youtube.com/watch?v=3ta4BmumQKc "Jinhen Naaz Hai Hind Par, Pyaasa song"
#   and (Lant Pritchett) thick-thin request to Ministerial leaders!
# - Done. Add notes for each category of leader of human edu and dev eg recommend dataQ & true experiment to assess
#  causality.
# - Done. For stripping out HTML tages from MathJax inline-embedded text, use stm::textProcessor(..., stripHTML=TRUE) or
#   consider the following as an ensemble with manual check where the methods' outputs don't match:
# ref https://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r:
# library(tm.plugin.webmining)
# extractHTMLStrip("junk junk<a href=\"/wiki/abstraction_(mathematics)\" title=\"abstraction (mathematics)\"> junk junk")
#
# textclean::replace_html()
#
# cleanFun <- function(htmlString) {
#   return(gsub("<.*?>", "", htmlString))
#   # BUT beware: it might gobble up enclosed strings that have `>`. Disallow `<>` to be matched within.
# }
#
# library(rvest)
# strip_html <- function(s) {
#     html_text(read_html(s))
# }
#
# require(XML)
# xData <- htmlParse('yourfile.html')
# xpathSApply(xData, 'appropriate xpath', xmlValue)
#
# - Done. (a) Confirm ok order of covars and exclusion of intercept. (b) Explore growing {covars}.
#   (c) http://psych.colorado.edu/~carey/qmin/qminChapters/QMIN09-GLMIntro.pdf says:
#   [It is good practice to put any control variables into the equation before the variable of interest ...
#   Usually, these terms are paired: dependent with independent, predicted with predictors, and response with explanatory.
#   An independent, predictor or explanatory variable that is measured with
#   numbers is called a numeric or quantitative variable or a covariate. One that is
#   not numeric (or uses numbers to indicate groups) is called a factor. The specific
#   groups within a factor are termed the levels of that factor ...
#   A beta (coef) gives the predicted change in Y for a one unit increase in X. ...
#   Note carefully that the actual magnitude of a betas is a function of the units of measurement of its X ...
#   never compare the betas across variables to determine the importance of the variables in prediction ...
#   Standardized betas may be used to compare the relative predictive effects of the independent variables. ...
#   predict that a one standard deviation change in variable X1 will result in a beta1 standard deviation change in Y
#   ... causal interpretation of a GLM depends on the design of
#   the study. True experiments (i.e., direct experimental manipulation, random
#   assignment, and strict control) permit inferences about causality ...
#   Technically, a GLM applied to non-experimental observational research does
#   not permit inferences about causality. But one must be reasonable here because
#   interpretation of a GLM must be taken in the context of existing data and
#   theory].
#   So, apart from the order of variables and terminology used, consider if intercept makes sense after
#   zeroing all variables.
# - Done. Risking lowered relatability, should covars be "normalized" to allow coef comparison? Or contrast() didn't work?
# - Done. Beware: modeling here with lesser digits of precision in notrOtot3 than was done for JEEinsight (2019-July). So,
#   retain original *count* data (right,tot) and use that for modeling, rather than rOtot3.
# - Done: Y2012 coefs around 1 vs. 1.3+ for other YP indicates central differences in their (effect on) rOtot3 ...
#   regime shift? Variability too???


if(FALSE){ # init
  # suffix.restmt: Get from ./src-eduCTeDtopic.R.
  # Was: "InsightPlusSTM2020Mar17", "InsightPlusSTM2020Mar15", "InsightPlusSTM2020Mar13", "InsightPlusSTM2020Feb27"
  responses.stmTheta <- datJADplus <- getFiledQOSs(suffix=suffix.restmt, doUnlistRec=FALSE)
  # identical(datJADplus, responses.stmTheta)
} # else continue
JEEdata.use <- responses.stmTheta

### --- following code reused from src-from-azureJEEinisght.R (2019-Nov-05):
cDecimalDigits <- 3
# RELATE IN:

# QOS.nchar <- unlist(lapply(JEEdata.use["QOS"], FUN=nchar)); print(summary(QOS.nchar))
colnames(JEEdata.use) <- gsub(pattern="jeeadv", replacement="", x=colnames(JEEdata.use))
with(JEEdata.use, table(qaType, YP)) # table(exclude=) levels to remove for all factors in ...
with(JEEdata.use, table(qaType, subject))
qaType.grp <- JEEdata.use$qaType
levels(qaType.grp) <- unlist(lapply(levels(JEEdata.use$qaType), FUN=doGroup.qaType))
with(JEEdata.use, table(markScheme, YP)) # -ve and +ve markScheme could be grouped, but no finer coz biased sample
markScheme.grp <- JEEdata.use$markScheme
levels(markScheme.grp) <- doGroupVec.markScheme(levels(markScheme.grp))
  # was: c("-ve", "-ve", "-ve", "+ve", "+ve") # {-103 -104 -201234} {+02 +03} and no finer
JEEdata.use <- cbind(JEEdata.use, qaType.grp=qaType.grp, markScheme.grp=markScheme.grp) # Was: QOS.nchar=QOS.nchar)
summary(JEEdata.use)

JEEdata.use <- JEEdata.use[JEEdata.use$uOtot3 > 0,]; nrow(JEEdata.use)
# RELATE OUT:
cProbs <- c(0, 0.01, 0.05, 0.1, 0.25, 0.333); cProbs <- sort(c(cProbs, 0.5, 1 - cProbs))
require(corrplot)
summary(JEEdata.use)

# MODEL OUT-IN:
# k <- 7 # count of topics generated from modeling.
# Was: cInNames <- c("subject", "YP", "qaType", "qaType.grp", "markScheme.grp",
#   "QOSnchar", "QOSntoken", "QOSflesch",
#   paste0("topicProp", 1:cKtopics)) # Topic Proportion (or Prevalence) from STM model fitting.
cInNamesIE <- c("YP", "markScheme.grp", "qaType.grp", # Coz "qaType.grp" is more robust than "qaType".
  "qaType", "QOSnchar", "QOSntoken", "QOSflesch",
  "subject", paste0("topicProp", 1:cKtopics))
  # These latter variables are relatively more workable for examinees and teaching-learning.
cInNames <- cInNamesIE
summary(JEEdata.use[,cInNames])
# cInNames <- cInNames[-(2:2)]
cOutNamesNew <- "right"
cOutNames.rOtot3 <- "rOtot3" # Note: rOtot3 is not just right scaled to 1000, but adjusts for population of examinees.
  # Alt: "notrOtot3", "log(notrOtot)". was: "right".
mdl.spec <- as.formula(paste(cOutNamesNew, "~", paste(cInNames, collapse=" + "))); print(mdl.spec)
  # Note: modeling function glm() or glm.nb() might take Y's log depending on link function. Was: cOutNames.rOtot3
mdl.spec.plot <- as.formula(paste0("log(", cOutNames.rOtot3, ") ~", paste(cInNames, collapse=" + ")))
print(mdl.spec.plot)
  # Was: update(mdl.spec, log(.) ~ .); mdl.spec.plot # replace LHS with log(LHS)
# plot(log(rOtot3) ~ qaType.grp, data=JEEdata.use)
plot(mdl.spec.plot, data=JEEdata.use)

aggregate(JEEdata.use[cOutNames.rOtot3] /1000, by=list(JEEdata.use[,"YP"]), FUN=mean)
  # was: FUN=function(x) round(median(x, na.rm=TRUE), cDecimalDigits))
# notrOtot3.by.subject.mean <- aggregate(JEEdata.use["notrOtot3"] /1000, by=list(JEEdata.use[,"subject"]), FUN=mean)
# notrOtot3.by.subject.mean <- notrOtot3.by.subject.mean[order(notrOtot3.by.subject.mean$notrOtot3, decreasing=TRUE),]
# notrOtot3.by.subject.mean[1,1]
aggregate(JEEdata.use[cOutNames.rOtot3] /1000, by=list(JEEdata.use[,"qaType"]), FUN=mean)
aggregate(JEEdata.use[cOutNames.rOtot3] /1000, by=list(JEEdata.use[,"subject"]), FUN=mean)

rYPvar <- aggregate(JEEdata.use["rOtot3"] /1000, by=list(JEEdata.use[,"YP"]), FUN=var)
colnames(rYPvar)[2] <- paste0(colnames(rYPvar)[2], "var"); print(str(rYPvar))
rYPmean <- aggregate(JEEdata.use["rOtot3"] /1000, by=list(JEEdata.use[,"YP"]), FUN=mean)
colnames(rYPmean)[2] <- paste0(colnames(rYPmean)[2], "mean"); print(str(rYPmean))
rYPmv <- cbind(rYPmean, rOtot3var=rYPvar$rOtot3var); print(str(rYPmv)) # Was: rYPvar=
print(cbind(rYPmv, meanOverVar=rYPmv$rOtot3mean / rYPmv$rOtot3var))
  # Y2012 has highest meanOverVar, quantifying what's apparent in plot of rOtot3. Y2016 is closest next.
  # Y2013 is least, but Y2018P2 comes close. So, one can't judge suitability of exclusion just by this.

  # ref: https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf
  require(MASS); require(lmtest); require(sandwich); require(pscl)
  myData <- JEEdata.use # Was: mdl.spec.bak <- mdl.spec
  myData$qaType <- droplevels(myData$qaType) # maybe obsolete now.
  contrasts(myData$qaType) <- getBaseContrastVsY(facXname="qaType", Yname=cOutNames.rOtot3, dat=myData, wantHigh=TRUE)
    # was: contrasts(myData$qaType) <- contr.treatment(levels(myData$qaType), base=3)
    # was: "NumAns" coz that goes with lowest mean
  contrasts(myData$subject) <- getBaseContrastVsY(facXname="subject", Yname=cOutNames.rOtot3, dat=myData, wantHigh=TRUE)
  contrasts(myData$YP) <- getBaseContrastVsY(facXname="YP", Yname=cOutNames.rOtot3, dat=myData, wantHigh=TRUE)
    # Was: wantHigh=FALSE
  # was: contrasts(myData$subject) <- contr.treatment(levels(myData$subject),
  #   grep(notrOtot3.by.subject.mean[1,1], levels(myData$subject)))
  #   # was: base=2) # "maths" coz that has extreme mean
  # contr.treatment(n, base=1, contrasts=TRUE, sparse=FALSE); C()

  # ?glm says:
  # Non-NULL weights can be used to indicate that different observations have different dispersions (with the values in
  # weights being inversely proportional to the dispersions); or equivalently, when the elements of weights are positive
  # integers w_i, that each response y_i is the mean of w_i unit-weight observations. For a binomial GLM prior weights are
  # used to give the number of trials when the response is the proportion of successes: they would rarely be used for a
  # Poisson GLM.
  # Was: tot <- rep(log(1000), nrow(myData))
  # Can drop intercept coz we want to estimate relative impacts of variables, not necessarily estimate the response itself
  #   (~ + 0) or (~ . - 1) can equivalently drop the intercept
  mdl.spec <- update(mdl.spec, ~ . + offset(log(tot))); print(mdl.spec)
    # Note: myData$tot is from original data, as in 2019 JEEinsight.
    # append offset(log(tot)) to indicate term with a fixed coefficient of one

  fit.glmpois1 <- stats::glm(mdl.spec, data=myData, family=poisson)
    # prefer rate of incidence (not count)
    # =loglm() cf Negative Binomial, possibly with Hurdle and Zero-Inflated Regression Models. package pscl?
    # family=negative.binomial(link="log")
    # Count data often have an exposure variable, which indicates the number of times the event could have happened.
    # This variable should be incorporated into a Poisson model with the use of the offset option.
  myGOFglm(fit.glmpois1)
  # coeftest(fit, vcov=sandwich); summary(fit); plot(fit)

  fit.glmnb1 <- MASS::glm.nb(mdl.spec, data=myData) # glm.nb(... link=log) by default.
  myGOFglm(fit.glmnb1)
    # prefer rate of incidence (not count). was: offset(log(wur))
    # ref https://mac-theobio.github.io/QMEE/Generalized_linear_models.html says of offsets:
    # "constant terms added to a model
    # what if we want to model densities rather than counts?
    # log-link (Poisson/NB) models: mu0=exp(beta0+beta1x1+...)
    # if we know the area then we want mu=A * mu0
    # equivalent to adding log(A) to the linear predictor (exp(log(mu0)+log(A))=mu0 * A)
    # use ... + offset(log(A)) in R formula"
    # so, by including offset(log(tot)), density of 'right' (over an "area" of 'tot' JEE candidates) is being modeled

  fit.glmnb11 <- MASS::glm.nb(update(mdl.spec, ~ . - QOSnchar - QOSntoken - QOSflesch), data=myData)
    # - topicProp7 - subject - 1 # Retain these plus some values/levels of YP and qaType.
  myGOFglm(fit.glmnb11)
  fit.glmnb12 <- MASS::glm.nb(update(mdl.spec, ~ . - QOSnchar - QOSntoken - QOSflesch - qaType), data=myData)
    # - topicProp7 - subject - 1 # Retain these plus some values/levels of YP, but drop qaType.
  myGOFglm(fit.glmnb12)
  fit.glmnb2 <- MASS::glm.nb(update(mdl.spec, ~ . + subject*YP + subject*topicProp1 + subject*topicProp2 +
    subject*topicProp3 + subject*topicProp4 + subject*topicProp5 + subject*topicProp6 + subject*topicProp7),
    data=myData)
  myGOFglm(fit.glmnb2)
  fit.glmnb21 <- MASS::glm.nb(update(mdl.spec, ~ . + subject*YP + subject*topicProp1 + subject*topicProp2 +
    subject*topicProp3 + subject*topicProp4 + subject*topicProp5 + subject*topicProp6 + subject*topicProp7
    - QOSnchar - QOSntoken - QOSflesch),
    data=myData)
  myGOFglm(fit.glmnb21)
  fit.glmnb22 <- MASS::glm.nb(update(mdl.spec, ~ . + subject*YP + subject*topicProp1 + subject*topicProp2 +
    subject*topicProp3 + subject*topicProp4 + subject*topicProp5 + subject*topicProp6 + subject*topicProp7
    - QOSnchar - QOSntoken - QOSflesch - qaType - topicProp7 - subject*topicProp7),
    data=myData)
  myGOFglm(fit.glmnb22)
  fit.glmnb220 <- MASS::glm.nb(update(mdl.spec, ~ . + subject*YP + subject*topicProp1 + subject*topicProp2 +
    subject*topicProp3 + subject*topicProp4 + subject*topicProp5 + subject*topicProp6 + subject*topicProp7
    - QOSnchar - QOSntoken - QOSflesch - qaType - topicProp7 - subject*topicProp7 - 1),
    data=myData)
  myGOFglm(fit.glmnb220)
  fit.glmnb221 <- MASS::glm.nb(update(mdl.spec, ~ subject*YP + . + subject*topicProp1 + subject*topicProp2 +
    subject*topicProp3 + subject*topicProp4 + subject*topicProp5 + subject*topicProp6 + subject*topicProp7
    - QOSnchar - QOSntoken - QOSflesch - qaType - topicProp7 - subject*topicProp7 - 1),
    data=myData)
  myGOFglm(fit.glmnb221)

### --- Difficulty-modeling comments from 2020Mar26:
# ++ Consider (a) cross validation (leave sample behind) but Taddy (2012) & Wesslen argue against that;
# (b) expose stm's GoF;
# (DONE c) qaTypeGrp instead of qaType during stm() itself;
# (DONE d) Xs as per chosen model and Ys maybe 10:90 rOtotGrp levels.
# Note labelTopics() esp:
# - words vs covariate rOtotGrp.
# - effect rOtot3 on topic4 Prevalence.
# - boxplot qaType.grp on log(rOtot3).
# - YP impact 18.4/31.4=~60% Y2012P1 /Y2014P2! meanOverVar varies from 7.17 to 24.95 ie over 3x! Not in examinees' ctrl!
# - Impact of topicProp 2,5,6 appears significant.
# - outlierObs <- c(*675, 565, 602,*607,*660,513,*156, *387,*655,644) # as per glm.nb() diagnostic
# plots for further investigation.
# - From fit.glmnb1, insignficant vars are {QOS*}, {?subject,1?}. So drop them while retaining {?*?} for another round
# eg subjectM appearing significant in fit.glmnb11.
# {?YP levels other than YP2012*?}
# - From fit.glmnb11, NAs coz [Coefficients: (3 not defined because of singularities)] for
# {qaTypeMultCorrAns,qaTypeSingDigitInteger,topicProp7}. So drop qaType first coz qaType.grp is more robust with
# more observations and logically grouped as per the author.
# Later, consider including IE qaType*topicProp or subject*topicProp etc!!
# - From fit.glmnb12, NAs coz [Coefficients: (1 not defined because of singularities)] for {topicProp7}. Consider IEs.
# This author opines that some years have papers for some subjects that are reported by experts and others as relatively
# difficult or easy. So, {subject*YP, subject*topicProp}.
# est.betahat (relatively within var): YPY2012P2 0.599. qaType.grpCreateNewAnsNumInt 0.551. subjectM 0.779. t5*0.458.
# stm() used:
#   f.prevalence <- as.formula(paste0("~ ", "subject + YP + jeeadvqaType"))
#   f.content <- as.formula(paste0("~ ", "rOtotGrp"))
# - fit.glmnb2 shows some IEs are significant, even after controlling for other variables. QOS* vars can be dropped.
# - fit.glmnb21 NAs coz [Coefficients: (5 not defined because of singularities)]. So drop
# qaType (though some levels are significant) and topicProp7 (plus related IEs).
# - fit.glmnb22 shows Intercept insignificant; so, drop Intercept next. Significant IEs:
# Y2012P1,2 Y2018P1,2 Y2012P1M* Y2016P1M* Y2014P2M* t5 t1 t6 (but *Linear* whereas stm() allows s() etc. though for
# Prevalence and Content) Pt6 Pt1 Mt1.
# - fit.glmnb220 looks mostly significant BUT no subject main effect!! So, next try reordering.
# - fit.glmnb221 gives identical results as fit.glmnb220.
# - Outputs from getKneeInflectionPlotYears() show UIKnee varies across YP.
# For abscissa rDifficultyRank, UIKnee=20.679 (~rOtot ~0.35), Inflection=70.679.

  fits <- list(fit.glmpois1, fit.glmnb1, fit.glmnb11, fit.glmnb12, fit.glmnb2, fit.glmnb21,
    fit.glmnb22, fit.glmnb220, fit.glmnb221)
  al.diags <- myCompareFits(fits)
  fit.chosen <- fit.glmnb12
    # fit.glmnb12 has least theta & chkDispersion. fit.glmnb1 has least AIC. fit.glmnb2 has highest logLik & least estVar.   myGOFglm(fit.chosen)
  # JEEdata.use.UIKinflect <- includeUIKinflect(JEEdata.use) refer src-*topic.R.


stop()
###--- Model with Interaction Effects:
  # cInNamesIE <- c("YP", "markScheme.grp", "qaType", "qaType.grp", "QOSnchar", "QOSntoken", "QOSflesch",
  #   "subject", paste0("topicProp", 1:cKtopics))
  mdl.specIE01 <- as.formula(
    paste(cOutNamesNew, "~", paste(cInNamesIE[1], " * ", cInNamesIE[length(cInNamesIE) - cKtopics]), " + ",
    paste(cInNamesIE[(1+1):(length(cInNamesIE) - cKtopics - 1)], collapse=" + "), " + ",
    paste(paste0(cInNamesIE[length(cInNamesIE) - cKtopics], " * ", tail(cInNamesIE, cKtopics)), collapse=" + ")))
  mdl.specIE01 <- update(mdl.specIE01, ~ . - 1 + offset(log(tot))); mdl.specIE01
    # -1 coz we care for relative effect of explanatory variables rather than accurately modeling difficulty itself.

  fit.glmnbIE01 <- MASS::glm.nb(mdl.specIE01, data=myData)
    # topicProp2 is nearest to being significant; others further away.  subject::C vs PM.
  myGOFglm(fit.glmnbIE01)

  cInNamesIE2 <- c("YP", "markScheme.grp", "qaType",
    "subject", paste0("topicProp", 1:cKtopics))
  topic.ie <- "topicProp2"
  mdl.specIE02 <- as.formula(
    paste(cOutNamesNew, "~", paste(cInNamesIE2[1], " * ", cInNamesIE2[length(cInNamesIE) - cKtopics]), " + ",
    paste(cInNamesIE2[(1+1):(1+3)], collapse=" + "), " + ",
    paste(paste0(cInNamesIE2[length(cInNamesIE) - cKtopics], " * ", topic.ie), collapse=" + "), " + ",
    paste(setdiff(tail(cInNamesIE, cKtopics), c(topic.ie)), collapse=" + ")
  ))
  mdl.specIE02 <- update(mdl.specIE02, ~ . - 1 + offset(log(tot))); mdl.specIE02
  fit.glmnbIE02 <- MASS::glm.nb(mdl.specIE02, data=myData)
  myGOFglm(fit.glmnbIE02)

  mdl.specIE021 <- as.formula(
    paste(cOutNamesNew, "~", paste(cInNamesIE2[1], " * ", cInNamesIE2[length(cInNamesIE) - cKtopics]), " + ",
    paste(cInNamesIE2[(1+1):(1+3)], collapse=" + "), " + ",
    paste(setdiff(tail(cInNamesIE, cKtopics), c()), collapse=" + ")
  ))
  mdl.specIE021 <- update(mdl.specIE021, ~ . - 1 + offset(log(tot))); mdl.specIE021
  fit.glmnbIE021 <- MASS::glm.nb(mdl.specIE021, data=myData)
  myGOFglm(fit.glmnbIE021)
  # Observations c(387, 655, 660) worth investigating as per QQplot.

stop()
  print(anova(fit.glmpois1, fit.glmpois2, fit.glmpois3,
    fit.glmnb1, fit.glmnb2, fit.glmnb201, fit.glmnb202, fit.glmnb203,
    fit.glmnbIE01, fit.glmnbIE02, fit.glmnbIE021
  ))
  # wtst <- lmtest::waldtest(fit.glmpois1, fit.glmnb1, vcov=vcovHC)
    # [Error ... there are aliased coefficients in the model].
    # ref https://cran.r-project.org/web/packages/lmtest/lmtest.pdf:
    # [If vcov is specified, HC and HAC estimators can also be plugged into waldtest ...
    # fit two competing, non-nested models and their encompassing model].

#   fit <- MASS::glm.nb(update(mdl.spec, ~ . - qaType.grp - 1 - YP - subject), data=myData); m3 <- fit
#  anova(m1, m2, m3) # factor subject is stat-significant predictor of response; so, avoid dropping subject and prefer m2.
    # dropping paper and qaType.grp is ok coz they are not significant predictors
  fit.chosen <- fit.glmnbIE021 # Was: fit.glmnb203 # preferred model coz least (Resid Dev / Resid DF).
  myGOFglm(fit.chosen, withEffects=TRUE)

# ---
# WAY FORWARD:
datJADplus.ord.notrOtot3 <- datJADplus.ord.notrOtot3[order(datJADplus.ord.notrOtot3$notrOtot3, decreasing=TRUE),]
head(datJADplus.ord.notrOtot3, 1)
cQOSncharTry <- 25
datJADplus.ord.notrOtot3$QOS <- headCharVec(datJADplus.ord.notrOtot3$QOS, n=cQOSncharTry)
colnames(datJADplus.ord.notrOtot3)[1] <- paste0("QOS", cQOSncharTry)
head(datJADplus.ord.notrOtot3, 1)
fname.ord <- paste0(cWorkDir, "/ord2020Mar17.csv")
write.csv(datJADplus.ord.notrOtot3, file=fname.ord)

stop()
### --- Time-specific GLM-modeling comments from before 2020Mar25:
  # 2020Feb27:
  #     Null deviance: 35777  on 357  degrees of freedom
  # Residual deviance: 21931  on 337  degrees of freedom
  # AIC: 24537

#  fit.glmpois2 <- glm(update(mdl.spec, ~ . - qaType.grp), data=myData, family=poisson)
    # Drop qaType.grp coz coefficient NAs [Coefficients: (3 not defined because of singularities)]
    # Similarly due to NA coefficients, drop topicProp7 + topicProp2 + topicProp1.
#  myGOFglm(fit.glmpois2)

#  fit.glmpois3 <- glm(update(mdl.spec, ~ . - qaType.grp - topicProp7 - QOSnchar - QOSntoken - QOSflesch),
      # Was: (mdl.spec, ~ . - qaType.grp - topicProp7 - topicProp2 - topicProp1)
#    data=myData, family=poisson)
    # Similarly due to NA coefficients, also drop - topicProp7 - topicProp2 - topicProp1.
#  myGOFglm(fit.glmpois3) # Still overdispersed.
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

### ---
  fit.glmnb201 <- MASS::glm.nb(update(mdl.spec, ~ . - qaType.grp - 1), data=myData)
    # drop intercept coz we want to estimate relative impacts of variables, not necessarily estimate the response itself
    #   (~ + 0) or (~ . - 1) can equivalently drop the intercept
    # was: Null Deviance went up from 880 to 3394 after dropping intercept from model spec, while AIC stays 15434
  myGOFglm(fit.glmnb201)
  fit.glmnb202 <- MASS::glm.nb(update(mdl.spec, ~ . - qaType.grp - 1 - topicProp7 - topicProp6 - topicProp5 - topicProp1
      - QOSnchar - QOSntoken - QOSflesch),
    data=myData)
    # Was: update(mdl.spec, ~ . - qaType.grp - 1 - topicProp7 - topicProp2 - topicProp1)
    # Similarly due to NA coefficients, also drop - topicProp7 - topicProp2 - topicProp1.
  myGOFglm(fit.glmnb202)
  fit.glmnb203 <- MASS::glm.nb(update(mdl.spec, ~ . - qaType.grp - 1 - topicProp7 - topicProp6 - topicProp5 - topicProp1
      - QOSnchar - QOSntoken - QOSflesch - topicProp4),
    data=myData)
    # Was: update(mdl.spec, ~ . -qaType.grp-1-topicProp7-topicProp2-topicProp1-topicProp6-topicProp5)
  myGOFglm(fit.glmnb203)

    # myData[c(44, 49, 66, ?99 if -subject?),] are plotted as outliers in
    # Residuals vs Fitted, Q-Q, & Scale-Location plots; investigate
    # myData[c(61, 261, 594),] are plotted as outliers in Residuals vs Leverage plot; investigate

              subjectP           qaTypeNumAns              YPY2016P2 
         -1.334090e-04          -1.247340e-04           3.512336e-05 
              subjectM              YPY2017P1    qaTypeParaSingleAns 
         -3.340911e-05           2.610560e-05          -2.203760e-05 
     qaTypeMultCorrAns              YPY2016P1            qaTypeCompr 
         -1.963971e-05           6.488982e-06          -6.442909e-06 
             YPY2017P2              YPY2014P2 qaTypeSingDigitInteger 
          6.329820e-06           6.066316e-06          -5.587836e-06 
            topicProp2              YPY2013P1             topicProp3 
         -5.217804e-06           4.651604e-06           4.266476e-06 
             YPY2018P1              YPY2014P1              YPY2013P2 
          4.072053e-06           4.030563e-06           3.859620e-06 
             YPY2018P2      qaTypeSingCorrAns      markScheme.grp+ve 
          3.817939e-06          -3.308903e-06           2.296386e-06 
             YPY2012P2 
          3.436065e-07 

                       Estimate 2.5 % 97.5 %
subjectC                  0.301 0.234  0.390
subjectM                  0.246 0.192  0.316
subjectP                  0.265 0.207  0.341
YPY2012P2                 1.007 0.811  1.250
YPY2013P1                 1.398 1.142  1.711
YPY2013P2                 1.621 1.285  2.044
YPY2014P1                 1.549 1.240  1.932
YPY2014P2                 1.529 1.210  1.938
YPY2016P1                 1.313 1.058  1.631
YPY2016P2                 1.419 1.140  1.767
YPY2017P1                 1.671 1.329  2.102
YPY2017P2                 1.577 1.266  1.968
YPY2018P1                 1.664 1.297  2.140
YPY2018P2                 1.513 1.181  1.943
qaTypeCompr               0.637 0.456  0.897
qaTypeMultCorrAns         0.438 0.363  0.527
qaTypeNumAns              0.288 0.214  0.388
qaTypeParaSingleAns       0.648 0.520  0.808
qaTypeSingCorrAns         0.788 0.646  0.957
qaTypeSingDigitInteger    0.497 0.397  0.622
markScheme.grp+ve         1.283 1.124  1.465
topicProp2                0.694 0.578  0.837
topicProp3                1.196 1.024  1.402

### --- End of Time-specific ...
