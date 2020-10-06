# ref https://stackoverflow.com/questions/14675913/changing-image-size-in-markdown for image resizing in README.md:
# ![](./pic/pic1_50.png =100x20) # With certain Markdown implementations
# ![](./pic/pic1s.png =250x) # You can skip the HEIGHT
# <img src="http://....jpg" width="200" height="200" />
# <img src="Assets/icon.png" width="200">

cDecimalDigits <- 3 # by default
c.path.local <- "C://Users/SONY/Desktop/preBachelors/Dscoring/"
cTransparency <- 0.6 # [0,1]
getCRLmarks <- function(myData=read.csv(file=paste0(c.path.local, "dataJEE-CRLmarks.csv"), stringsAsFactors=FALSE)){
  # some pre-processing ref https://stackoverflow.com/questions/35441382/change-dimension-of-a-data-frame:
  # d2013 <- read.csv(file=paste0(c.path.local, "dataJEE-d2013.csv"),stringsAsFactors=FALSE)
  # str(d2013); dim(d2013)
  # d2013 <- as.data.frame(matrix(unlist(t(d2013)), nrow = nrow(d2013)/2, ncol=2, byrow = TRUE))
  # colnames(d2013) <- c("ComRankL", "Marks"); str(d2013)
  # write.csv(d2013, file=paste0(c.path.local, "dataJEE-d2013t.csv"), row.names=FALSE)
  # # fails dim(d2013) <- c(nrow(d2013)/2, 2); d2013 <- t(d2013)
  # stop()

  ans <- myData
  ans$JADyear <- as.factor(ans$JADyear)
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
getJEEmarks <- function(myData=read.csv(file=paste0(c.path.local, "JEEmarksDistrib.csv"), stringsAsFactors=FALSE)){
  ans <- myData
  ans$exam <- as.factor(ans$exam)
  ans$year <- as.factor(ans$year)
  ans$subject <- as.factor(ans$subject)
  print(str(ans))
  return(ans)
}

# also ref https://www.researchgate.net/publication/268977798_Reliable_computations_of_knee_point_for_a_curve_and_
# \introduction_of_a_unit_invariant_estimation (2014)
addKneeLineText <- function(x, y, myColour){
  # convexcave1 <- check_curve(x, y); print(convexcave1)
  knee <- uik(x, y); print(knee) # note: not uik(dh$x, dh$y)
    # returns the x-abscissa which is the UIK estimation for the knee point.
    # Unit Invariant Knee (UIK) method for finding the knee point of a curve
    # first checks the curve by using check_curve and classifies it as convex, concave or convex/concave, concave/convex
  abline(v=knee, col=myColour) # v=knee for vertical. h= for horizontal
  text(knee, median(y), labels=paste0("knee=", round(knee, cDecimalDigits)), col=myColour, srt=90)
    # col="gray60", pos=2)
    # adj=c(0, -.1)). srt= degrees to rotate text. pos=1:4 for positions below, left, above, right of xy coordinate
    # was: text(median(x), ...)
  return(knee)
}
myFunYperUniqX <- function(dhxyp, FUN=median){
  UniqXs <- sort(unique(dhxyp[,1]))
  UniqXs.mx <- matrix(UniqXs, nrow=length(UniqXs), byrow=TRUE)
  myfn <- function(uqx) { FUN(dhxyp[dhxyp[,1]==uqx, 2]) }
  FunY <- apply(UniqXs.mx, 1, myfn)
  # FunY <- FUN(dhxyp[dhxyp[,1]==ux,2])
  return(FunY)
}
myAggFun <- function(y1y2, FUNy1=mean, FUNy2=unique){
  ans.y1y2 <- c(FUNy1(y1y2[,1]), FUNy2(y1y2[,2]))
  return(ans.y1y2)
}
getUniqPs <- function(xp.agg.x){
  UniqPs <- NULL
  for(i in 1:length(xp.agg$x)){
    UniqPs[i] <- length(unique(xp.agg$x[[i]]))
  }
  return(UniqPs)
}
getKneePlotYears <- function(dhxyp, xylab=colnames(dhxyp), isScurve=FALSE){
  # ref R package: https://cran.r-project.org/web/packages/inflection/inflection.pdf
  # ref https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3043076 (2017)
  # Given a planar curve with discrete (xi,yi) points this function can find if it is convex, concave or for
  # the sigmoid case if it is convex/concave or concave/convex.
  require(inflection)
  require(colorspace) # for HCL colours, unless R3.6.0+ eg on Azure
    # cRGBtransparency <- rgb(0, 0, 0, 0.4)
    # ref https://bookdown.org/rdpeng/exdata/plotting-and-color-in-r.html
    # get a better sense of the varying density of the points in the plot
  require(TBD)
  x <- dhxyp[,1]; y <- dhxyp[,2]

  # md <- melt(dhxyp, id=c(1))
  # xy.agg <-  # cast()
  # UniqXs <- sort(unique(dhxyp[,1]))
  # FunY <- myFunYperUniqX(dhxyp, FUN=median)
  # xy.agg <- aggregate(dhxyp[,2:3], by=list(round(x, 1)), FUN=myAggFun, FUNy1=mean, FUNy2=unique) # or median()

  xy.agg <- aggregate(y, by=list(round(x, 1)), FUN=mean) # or median()
  xp.agg <- aggregate(dhxyp[,3], by=list(round(x, 1)), FUN=paste) # for each x, count years whose data is present
  UniqPs <- getUniqPs(xp.agg$x)
  UniqPs.1start <- min(which(UniqPs==1))
  xy.agg.multiP <- xy.agg[1:(UniqPs.1start - 1),]
  # xy.agg.trim <- xy.agg[xy.agg[,1] <= 17,]
  str(xy.agg.multiP); summary(xy.agg.multiP)
  plot(xy.agg.multiP, xlab=xylab[1], ylab=xylab[2], main="Multi-year CRL-Marks plot: mean Y for 0.1 X"); grid()
  rCRL.want <- 1 # suppose D wants this or better ranks ie 1 * 150000/100 = 1500 or better
  knee.agg <- addKneeLineText(xy.agg.multiP[,1], xy.agg.multiP[,2], myColour="black")
  print(CRL.knee.agg <- knee.agg * 150000/100)
  print(xy.agg.multiP[(xy.agg.multiP[,1] == knee.agg) | (xy.agg.multiP[,1] == rCRL.want),])
  print(head(xy.agg.multiP,12))
  print("but this is farther right than any of the year-wise knees! this process not robust!?")
  promptToContinue("saved?")

  plot(x, y, xlab=xylab[1], ylab=xylab[2], type="n"); grid()
    # plot axes and grid alone as a "window frame", without any points or curves.
    # consider SVG or alt for small/large display screens or prints.
    # was: pch=myPch, cRGBtransparency) # , xlab=colnames(dhxyp)[1], ylab=colnames(dhxyp)[2])
    # , cex=0.1) # beware: formal xlab matches multiple if >2 cols
    # The difference between pch = 16 and pch = 19 is that the latter uses a border and so is perceptibly larger when
    # lwd is large relative to cex.
    # alt: smoothScatter() for smoothed color density representation. uses cRGBtransparency
  knee.agg1 <- addKneeLineText(x, y, myColour='black')
  print(CRL.knee.agg1 <- knee.agg1 * 150000/100)
  myJitter <- 0.1
  print(summary(dhxyp[(x >= knee.agg1-myJitter) & (x <= knee.agg1+myJitter), ]))
  print(summary(dhxyp[(x >= rCRL.want-myJitter) & (x <= rCRL.want+myJitter), ]))

  if(isScurve){ # get inflection point regardless of whether for sigmoidal/S curve. was: if(isScurve){
    stop("test xy and vh switches and colours etc. in this code block before use")
    # was: x=round(JAD$rMarks). apparently, x=int is a must for chi from findiplist()
    convexcave2 <- check_curve(x, y); print(convexcave2) # coz may be S sigmoid curve
    A=findiplist(x, y, index=convexcave2$index, doparallel=FALSE)
      # If data is convex/concave then index=0. If data is concave/convex then index=1. consider index=check_curve()$index
    print(A); myESEchi <- A[1,3]
    abline(v=myESEchi, col='blue')
    text(myESEchi, max(y), labels=paste0("inflection=", round(myESEchi, cDecimalDigits)),
      col='blue', pos=2, adj=c(0,0)) # col="gray60"
      # adj=c(0, -.1))
    # abline(v=A[2,3], col='green')
  } # else continue

  count.l3 <- length(levels(dhxyp[,3])) # count of (factor) levels
  # ref https://developer.r-project.org/Blog/public/2019/04/01/hcl-based-color-palettes-in-grdevices/ says:
  # [Starting with R 3.6.0 a new hcl.colors() function is available in grDevices, providing a wide range of HCL-based
  # color palettes with much better perceptual properties than existing RGB/HSV-based palettes like rainbow().
  # ]
  # hcl_palettes("qualitative", plot = TRUE)  # hcl.pals(type="qualitative")
  # colours.kneeLine <- rainbow_hcl(count.l3, alpha=cTransparency)
    # ref https://cran.r-project.org/web/packages/colorspace/colorspace.pdf
    # rainbow(count.l3) # Note that the rainbow function implements the (in-)famous rainbow (or jet) color palette that
    # was used very frequently in many software packages but has been widely criticized for its many perceptual problems.
  colours.kneeLine <- colours.tailCurve <- qualitative_hcl(count.l3, palette="dark3", alpha=cTransparency)
  # colours.tailCurve <- hcl.colors(count.l3, palette="viridis", alpha=cTransparency) # R3.6.0+ eg on Azure
    # palette="viridis" default; ="dark2", ="dark3" colour-blind friendly. alpha= transparency [0,1]. alt: topo.colors()
    # For example, "Dark 3" works well for shading points or lines in up to five groups, "YlGnBu" is a sequential
    # palette similar to "viridis" but with aligned chroma/luminance, and "Green-Brown" or "Blue-Red 3" are
    # colorblind-safe diverging palettes.
  # alt: https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
  for(il3 in 1:count.l3){
    il3.lev <- levels(dhxyp[,3])[il3]
    myPch <- substrRight(il3.lev, 1) # from that level, extract rightmost char to use as labeling point later
    dhxyp.il3 <- dhxyp[dhxyp[,3]==il3.lev,]
    x.il3 <- dhxyp.il3[,1]; y.il3 <- dhxyp.il3[,2]
    points(x.il3, y.il3, pch=myPch, col=colours.tailCurve[il3]) # was: col=cRGBtransparency)
    addKneeLineText(x.il3, y.il3, myColour=colours.kneeLine[il3])
  }

  # promptToContinue("saved plot?")
  return() # was: list(convexcave1, knee))
}

JAD <- getCRLmarks()
plot(rComRankL ~ rMarks, data=JAD)
dxyp.r <- JAD[,c("rComRankL", "rMarks", "JADyear")]
  # origin could be shifted to 100 to be percentile rank =(100 - JAD$rComRankL)
# dxyp.i <- JAD[,c("ComRankL", "Marks", "JADyear")]
getKneePlotYears(dxyp.r, xylab=colnames(dxyp.r), isScurve=FALSE)
# getKneePlotYears(dxyp.i, xylab=colnames(dxyp.i), isScurve=FALSE)
aggregate(JAD["rMarks"], by=list(JAD$JADyear), FUN=summary)
