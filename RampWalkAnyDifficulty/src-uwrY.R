dat <- responses.orig
stopifnot(colnames(dat)[21] == "jeeadvqaType")
colnames(dat)[21] <- "qaType"

options(jupyter.plot_mimetypes = "image/svg+xml")
  # Change mimetype to PNG for IE and other browsers. SVG+XML iff browser supports well
  # getOption("repr.plot.width"); getOption("repr.plot.height")
options(repr.plot.width = 10, repr.plot.height = 5)
par(cex.axis=0.5) # is for x-axis. par(cex.lab=1.5) is for y-axis
wur.xtabs <- xtabs(cbind(wrong, unattempted, right) ~ qaType, data=dat)
  # count or frequency sums, ie, contingency table
print(round(wur.xtabs / sum(wur.xtabs), cDecimalDigits))
  # for approx comparability of cells of contingency tables
boxplot(rOwur ~ qaType, data=dat, las=2) # ratio of candidates who get questions of qaType right.
# boxplot for factor-wise distribution. las= for labels at angle to axis

risk.tab <- as.data.frame(cbind(freq=rowSums(wur.xtabs), freq.prop=(rowSums(wur.xtabs) / sum(wur.xtabs)),
  # relative frequency of question type
wOr=(wur.xtabs[,"wrong"] / wur.xtabs[,"right"])))
  # for question type, relative risk of wrong over right answer
risk.tab <- cbind(risk.tab, prop.scaled=scale(risk.tab$freq, center=FALSE, scale=TRUE),
  wOr.scaled=scale(risk.tab$wOr, center=FALSE, scale=TRUE))
risk.tab <- round(cbind(risk.tab,
  RPNadd=rowMeans(risk.tab[,c("prop.scaled", "wOr.scaled")]),
  RPNmult=apply(X=risk.tab[,c("prop.scaled", "wOr.scaled")], MARGIN=1, FUN=prod)),
    # MARGIN= 1 for rows, 2 for columns
  cDecimalDigits)
# assuming cost of impact does not vary with question type, e.g., through the markingscheme
# alt: risk.tab$freq * risk.tab$wOrrisk.tab
print(risk.tab)
print(paste0("freqSum=", sum(wur.xtabs)))
