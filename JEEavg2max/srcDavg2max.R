warning("source srcJEE-common.R")
# The author is presently writing this section, considering data imputation (of `avg,max`) and clustering of tests,
# which can be a basis for new actions for a raised `avg`.

Dscores.org <- getDscores()
Dscores <- Dscores.org[which(Dscores.org$subject != 'S'),]; Dscores <- droplevels(Dscores) # eg, drop for Subject "S"
Dscores <- Dscores[,-c(4,8:10)] # drop c("JADremark", 3 ratios)
summary(Dscores)
print(aggregate(Dscores[c("ind","avg","max")], by=list(Dscores$pattern), FUN=summary))
Dc <- Dscores[Dscores$pattern=='competitive',] # c("date","avg","ind","max")]

myAxis.minMax <- c(-0.25, 1)
plot(Dc$date, c(myAxis.minMax, rep(mean(myAxis.minMax), # or 0/anything in between myAxis.minMax interval
  length(Dc$ind) - length(myAxis.minMax))), ylab="rMarks", # was: paste0(colnames(Dc)[-1], collapse=","), # drop "date"
  type="n")
points(Dc$date, Dc$avg, col="red", pch="a")
points(Dc$date, Dc$max, col="green", pch="m")
lines(Dc$date, Dc$max, col="green", pch="m")
points(Dc$date, Dc$ind, col="blue", pch="i") # plot this last so that it's not masked by other points
axis(side=1, Dc$date, format(Dc$date, "%b"), cex.axis = .7)

# plot(avg ~ date, data=Dc, xaxt="n", pch="d")
# axis(side=1, Dc$date, format(Dc$date, "%b"), cex.axis = .7)
  # axis(1, Dc$date, format(Dc$date, "%b"), cex.axis = .7) # axis.Date(side, x, at, format, labels = TRUE, ...)
  # axis.Date(1, at=seq(as.Date("2001/1/1"), max(random.dates)+6, "weeks"))
  # myat <- seq(min(Dc$date), max(Dc$date)+30*2, "weeks")
  # axis(side=1, Dc$date, myat, cex.axis = .7)

print("max seems to oscillate between 1.00 and 0.40. recall 35% JEE min.")
print("hunch: natural up trend with D raising Difficulty at 1.00")
print("ind extremely oscillated initially, including below 'a'. later, lesser")

# explore how Dc$max varies. fit distribution. get CI
aVar <- Dc$max; aVar <- aVar[!is.na(aVar)]
summary(aVar)
  aVar.label <- "Dscores$max[Dscores$pattern=='competitive']"; aVar.lim <- 0.35 # min JAD rMarks or marks%
  print(paste0(aVar.label, ": un-imputed data; so has NAs. What is CICDF for min JAD ", aVar.lim, "?"))
  distribs.explore <- c.distribs.explore[-2] # -3 to drop "unif". -2 to drop "beta"
print("now do as per srcJEE-enlighten.R")

  myc.niter <- 501
  myCIcdf.xq <- c(0.55,0.10)
  # myCIcdf.xq <- c(0.35,0.10)
    # myCIcdf.xq <- c(0.55,0.10) # $max seeks upper limit (say 0.55) rather than $avg min 0.35 cutoff in c(0.35,0.10)
  exploreFdplusDistrib.CInew(aVar, fds[[d]], distribs.explore=d, # "nsbeta"
    CIcdf.xq=myCIcdf.xq, c.niter=myc.niter)
  d <- "weibull" # prefer weibull then normal
  print("almost certainly > JEE CRL min limit. so, explore right skew ADR AD2R")
  exploreFdplusDistrib.CInew(aVar, fds[[d]], distribs.explore=d, # "nsbeta"
    CIcdf.xq=myCIcdf.xq, c.niter=myc.niter)
  myGofs <- c("ADR", "AD2R") # for left weight: c("ADL", "AD2L"). else for right weight: c("ADR", "AD2R")
  myLogscale <- "L" %in% substrRight(myGofs, 1) # Left-tail emphasis calls for log-scale axes
