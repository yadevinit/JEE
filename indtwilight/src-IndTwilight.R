# 1: Median 61% weAttempt, and when we do, median 45% weCorrect.  (Means are similar.)  That's approx 28% (=.61*.45)
# Right.  That's higher than the 1/5 Right from JEEinsight.  So, the JAD is easier (consider the Difficulty mix), or the
# Deeksha students are stronger, or the lesser JAD topics explains it (<=1/4 portions vs. JEEinsight), or some of these.
#
# 2: Typically, iScore 0; on average 1.03.  1/4 questions gets iScore >=3.  iDeviate typically -0.8min, mean being 0.32.
# This means typically, student i takes almost 1min lesser than "optimal" time of (180/(3*18) =) 3.3min; on average,
# i takes 1/3min longer than "optimal".  (Student i says he took longer for Mathematics since he was not allowed to leave
# the exam earlier.)
#
# 3: not independent!  Chapter "Inverse Trigonometric Functions" 58% Correct; "p-Block Elements" 21%.
# "Relations and Functions" 2.25 med 3 iScore, "Dual Nature" 2.4 med 3.  "p-Block Elements" mean 0.33 med 0 iScore.
# "Capacitors" 71% mean weAttempt; "Vectors" 45%.
# {weAttempt MULTI-MODAL *subject, *Difficulty, **Chapter30:90, ***qaType+0:3. (not shift)
# weCorrect *Difficulty, *shift, **subject-chemistry, **Chapter17:57, ***qaType-1:3.
# iScore **Chapter, **subject-chemistry, **qaType+0:3.
# iDeviate *Difficulty & others.
#

cDecimalDigits <- 2 # by default
c.path.local <- "C://Users/SONY/Desktop/preBachelors/JEEadvanced-RiskPrioritization/"
cy.qNum <- -1.5 # coz free space
cAddForOdds <- 0.05 # +0.05 to deal with 0 denominator
qNum.doLabel <- function(df, col="black", y=cy.qNum, lty="solid"){
  # print(head(df, 10)) # beware: too many columns wide
  abline(v=row.names(df), col=col, lty=lty)
  text(row.names(df), y, labels=df$qNum, col=col)
  return()
}
myLegNames <- function(origNameVec, split="qao."){ # drop matched 'split' from origNames
  splitList <- strsplit(origNameVec, split=split, fixed=TRUE)
  ans <- sapply(splitList, FUN=function(x) paste(x, collapse="")) # to collapse the dropped "" in each list element
  return(ans)
}

# --- get data:
qao.orig <- read.csv(file=paste0(c.path.local, "qaoJAD.csv"), stringsAsFactors=FALSE) # was: "qaJAD.csv" till 2019Nov27
str(qao.orig); qao <- qao.orig
# qao$qNum <- as.factor(qao$qNum)
qao$Difficulty <- as.factor(qao$Difficulty)
qao$subject <- as.factor(qao$subject)
qao$shift <- as.factor(qao$shift)
qao$qaType <- as.factor(qao$qaType)
qao$DifficultyGrp <- as.factor(qao$DifficultyGrp)
qao$qaTypeGrp <- as.factor(qao$qaTypeGrp)
# qao$CorrectAnswer <- as.factor(qao$CorrectAnswer)
# iResponse <- as.factor(qao$YourResponse)
qao$iAttempt <- (qao$YourResponse != "Unanswered")
qao <- qa[order(qao$shift, qao$subject, qao$qNum, decreasing=FALSE),] # beware: affects ordered plots
levels(qao$subject) <- substr(levels(qao$subject), 1, 1) # shorten to PCM for labeling ease
row.names(qao) <- 1:nrow(qao)
qao.id <- paste0(qao$shift, qao$subject)
# iSco.fac <- cfac(qao$iScore, sort(unique(qao$iScore)))
qao <- cbind(qao, qao.id=qao.id, iScore.fac=as.factor(qao$iScore))
str(qao)
summary(qao)


# --- What Happened Then:
require(colorspace) # for HCL colours, unless R3.6.0+ eg on Azure
devAskNewPage(ask=TRUE)
  # ask=NULL or a logical value. If TRUE, the user will in future be prompted before a new page of output is started.
cTransparency <- 1 # for darkest colours. beware: it overrides earlier value!
colVec <- c("blue", "blue", "gray60", "red", "red", "grey", "green", "yellow", "orange", "darkgreen")
# colVec <- qualitative_hcl(length(colVec), palette="Dark 2", alpha=cTransparency)
colVec <- qualitative_hcl(length(colVec), palette="Dark 3", alpha=cTransparency)
# colVec <- sequential_hcl(length(colVec), palette="Viridis", alpha=cTransparency)
# colVec <- divergingx_hcl(length(colVec), "Zissou 1", alpha=cTransparency)
# colVec <- sequential_hcl(length(colVec), palette="Plasma", alpha=cTransparency)
  # alt: "Viridis" "Plasma". else "Pastel 1" "Set 3" "Paired". alt: rainbow_hcl()
pchVec <- c('i', NA, "2c", "w", NA, NA, rep(NA, 4)) # was: [9]="t"
ltyVec <- c(NA, "solid", NA, NA, "solid", "solid", c("dashed", "dotted", "dotdash", "longdash")) # or "twodash"
# ltyVec <- c(NA, "solid", NA, NA, "solid", "solid", c("solid", "solid", "dotdash", "dotdash")) # or "twodash"
qao.plist <- list(
  qao.iScore = qao$iScore,
  qao.iAttempt.01 = as.integer(qao$iAttempt),
  qao.id.q1 = qao$qao.id[qao$qNum==1],
  'qao.weCorrect*4/100' = qao$weCorrect *4/100,
  'qao.weAttempt*4/100' = qao$weAttempt *4/100,
  'qao.iDeviate/3' = qao$iDeviate/3 # coz min(iDeviate) >= -3
)
ix<-1; plot(qao.plist[[ix]], type="n", main="JAD2019Jun27 graph",
  xlab="Index of questions by 'shift' (paper) and 'subject'",
  ylab=paste(myLegNames(names(qao.plist)[[ix]]), "(and more in legend)"))
  grid()
    # x.grid <- max(qao$qNum). col="blue"
    # grid(nx=nrow(qao)/x.grid, ny=NULL) # not ny=max(qao$iScore)-min(qao$iScore))
  points(qao.plist[[ix]], col=colVec[ix], pch=pchVec[ix]) # lines(qao$iScore, col=colVec[ix])
ix<-ix+1; lines(qao.plist[[ix]], col=colVec[ix], lty=ltyVec[ix]) # , pch='*')
ix<-ix+1; text(seq(from=1, to=nrow(qao), by=max(qao$qNum)), min(qao$iScore), qao.plist[[ix]], col=colVec[ix])
  # min(qao$iScore)-0.1 for precise location but might not portable. alt: pos=1 below. side=1 bottom. was: 1:nrow(qao)
ix<-ix+1; points(qao.plist[[ix]], col=colVec[ix], pch=pchVec[ix]) # was: qao$weAtCorrect
ix<-ix+1; lines(qao.plist[[ix]], col=colVec[ix], lty=ltyVec[ix])
ix<-ix+1; lines(qao.plist[[ix]], col=colVec[ix], lty=ltyVec[ix]) # pch=pchVec[ix]
# ix<-ix+1; points(qao$iDeviate/3, col=colVec[ix], pch=pchVec[ix])
leg.ixs <- 1:length(qao.plist) # was: c(5:10)
legend("bottomright", # "top", # [legend position]
  legend=myLegNames(names(qao.plist[leg.ixs])),
    # c("iScoLow.weCorHigh", "iScoNotHigh.weCorHigh", "iScoHigh.weCorLow"),
    # [names to display]
  bty='n', # [type of box around the legend]
  cex=0.5, # char expansion factor
  horiz=FALSE,
  inset=c(0.06, 0.35), # [% (from 0 to 1) to draw the legend away from legend pos (as origin) along x and y axis]
    # was: c(0.03, 0.20). beware: inset= is sensitive to present physical size of Graphics Device window!!
  pch=pchVec[leg.ixs], lty=ltyVec[leg.ixs], col=colVec[leg.ixs])

iScoLow <- qao$iScore %in% c(-2, -1)
iScoHigh <- qao$iScore %in% c(3, 4)
iDevLow <- qao$iDeviate <= summary(qao$iDeviate)[2]
iDevHigh <- qao$iDeviate >= summary(qao$iDeviate)[5]

# add vertical lines and labels:
# weAttempt
weCorLow <- qao$weCorrect <= summary(qao$weCorrect)[2] # was: weAtCor* and $weAtCorrect
weCorHigh <- qao$weCorrect >= summary(qao$weCorrect)[5]
leg.ixs <- (max(leg.ixs) + 1):length(colVec) # was: 1:4
qao.vlist <- list(
  qao.iScoHigh.iDevLow = qao[iScoHigh & iDevLow,],
  qao.iScoNotHigh.iDevHigh = qao[(! iScoHigh) & iDevHigh,],
  # qao.iScoLow.iDevHigh <- qao[iScoLow & iDevHigh,]
  # qao.iScoLow.weCorHigh = qao[iScoLow & weCorHigh,],
  qao.iScoNotHigh.weCorHigh = qao[(! iScoHigh) & weCorHigh,],
  qao.iScoHigh.weCorLow = qao[iScoHigh & weCorLow,]
)
for(ix in leg.ixs){
  qNum.doLabel(qao.vlist[[ix - min(leg.ixs) + 1]], col=colVec[ix], lty=ltyVec[ix])
}
legend("bottomright", # "top", # [legend position]
  legend=myLegNames(names(qao.vlist[leg.ixs - min(leg.ixs) + 1])),
  # legend=strsplit(names(qao.vlist[leg.ixs - min(leg.ixs) + 1]), split="qao.", fixed=TRUE)[[2]],
    # c("iScoLow.weCorHigh", "iScoNotHigh.weCorHigh", "iScoHigh.weCorLow"),
    # [names to display]
  bty='n', # [type of box around the legend]
  cex=0.5, # char expansion factor
  horiz=FALSE,
  inset=c(0.01, 0.20), # [% (from 0 to 1) to draw the legend away from legend pos (as origin) along x and y axis]
    # was: c(-0.02, 0.20)
  pch=pchVec[leg.ixs], lty=ltyVec[leg.ixs], col=colVec[leg.ixs])
colsInterest <- c("qao.id", "qNum", "qaType", "iAttempt", "iScore", "iDeviate", "weAttempt", "weCorrect")
for(ix in (leg.ixs - min(leg.ixs) + 1)){
  print(names(qao.vlist)[ix])
  print((qao.vlist[[ix]])[,colsInterest])
}


# --- Attempt to Score:
# we scores better in shift2, whereas ind scored more in Maths and less in other 2 subjects, including 0 in Chemistry.
# iAttempt is lesser in shift2 && incorrect responses up!
#   C inc 9/11 -ve 3 iScore 3-3=0 which would eliminate CRL; P inc 6/14 -ve 8 iScore 28-8=20 so riskier incs!
# what's amazing is ind contained impact of incorrectness; it didn't spill over! autocorrel might be unlikely too!
# i seems quicker than avg & topper too (except for M wait)!
#   but i missed on Easy qs (unlike topper), while solving Difficult unlike even the topper!
#   but got P2:Easy correct.
# ind attempted (75:33)/108 vs we 61%:39%.
# given what ind attempted: where does ind lose more {-2,-1} (downside)?  where could ind have got more {4,3}?
# where could ind raise attempts: where weAtCorrect high (upside)?
#
# DID I LOOK BACK AT THIS TEST?  AND CONFIRM CORRECT ANSWER?  AND FILL HOLES?  ELSE WILL AnAb CAUSE THESE FOR EACH OTHER?
#   ... FOR EACH TEST THAT'S HAPPENED, INCLUDING MOCK ONES?  JUDGING (systematic) OUTLOOK!!!
# maybe An goes max out on Maths and Ab on Chem.
# if required, one can id relatively "low-hanging fruit" eg those weAttempt>median && weCorrect>median corresponding
# to iAttempt && iScore>0 ... weighted by iDeviate.

plot(iScore.fac ~ iAttempt, data=qao,
  col=rev(divergingx_hcl(length(levels(qao$iScore.fac)), "Geyser", alpha=cTransparency)))
  # upon iAttempt, avoid iScore==0 ie get iScore>=3 as in M; raise iAttempt
summary(qao$iAttempt); 75/(33+75)
aggregate(qao["iScore"], by=list(qao$shift, qao$subject), FUN=sum)
summary(qao$weAttempt)
for(out.var in c("iAttempt", "iScore.fac")){
  if(TRUE){ # was: (out.var == "iAttempt"){
    df <- qao
  } else {
    df <- qao[qao$iAttempt==TRUE,]
  }
  for(in.var in c("qaType", "qao.id")){ # qao.id has ("shift", "subject") combo
    print(paste("impact of", in.var, "on", out.var, ":"))
    myAgg <- aggregate(df[out.var], by=list(df[,in.var]), FUN=summary)
    print(myAgg)
    # qaType: iAttempt all +0:3 which is healthy
    #   but iAttempt riskier -2:4 more than safer -1:3. Ind could consider attempting -1:3 before -2:4.
    #   iScore pay-offs from both -2:4 and -1:3 seem fine. Ind must alter the many 0s in +0:3.
    # qao.id: iAttempt Physics more, though not translated to iScore
    #   3s 4s weren't scored in 2c as much as in 1m or 2p though -ves better than 2p & 0s like 1m. explains 2c iScore=0.
  }
}

myAgg <- aggregate(qao["iScore.fac"], by=list(qao$qao.id=='2c'), FUN=summary); print(myAgg)
oddsHiSco <- NULL
oddsHiSco["2c"] <- (1+1) / (18-(1+1))
oddsHiSco["not2c"] <- (22+17) / (108-18-(22+17))
print(oddsHiSco); print(oddsHiSco["not2c"] / oddsHiSco["2c"])
myAgg <- colSums(myAgg)[c(1:5)+1]; print(myAgg) # ignore "Group" var
sum(myAgg[4:5]) / sum(myAgg[1:3])
# Given a random question from this sample, Odds of high iScore `{3,4}` (over not-high iScore `{-2,-1,0}`) are 0.612.
# Given a question whose `shift subject` is known to be `2c`, Odds of high iScore (over not-high iScore) outcome
# are 0.125.
# Given a question whose `shift subject` is *not* `2c`, Odds of high iScore
# are raised to 6.1 times that for `2c`.
# (That "6.1 times" equals Odds of high iScore being 0.765.)
# Put concisely, Odds Ratio of high iScore for `not 2c` is 6.1 times that for `2c` category.


stop()
# --- exploratory code that was used before developing above code:
aggregate(qao["weAtCorrect"]/100, by=list(qao$shift, qao$subject), FUN=sum)
boxplot(weAttempt ~ iAttempt, data=qao)
boxplot(weCorrect ~ iScore.fac, data=qao)
boxplot(weAtCorrect ~ iScore.fac, data=qao)
aggregate(qao["iScore.fac"], by=list(qao$iAttempt, qao$subject), FUN=summary)
plot(iScore.fac ~ iAttempt, data=qao); print("upon iAttempt, avoid iScore==0 ie get iScore>=3 as in M; raise iAttempt")
#
   Group.1 Group.2 iScoreGrp.-2 iScoreGrp.-1 iScoreGrp.0-2 iScoreGrp.3
1   FALSE       c            0            0            12           0
2    TRUE       c            3            5             6          10
3   FALSE       m            0            0            14           0
4    TRUE       m            3            1             4          14
5   FALSE       p            0            0             7           0
6    TRUE       p            5            2             5          17
#
aggregate(qao["iAttempt"], by=list(qao$qaTypeGrp), FUN=summary)
aggregate(qao["iAttempt"], by=list(qao$shift), FUN=summary)
# plot(iAttempt ~ shift + subject + qaType + qaTypeGrp, data=qao)
aggregate(qao["iAttempt"], by=list(qao$subject), FUN=summary)
aggregate(qao["iAttempt"], by=list(qao$qaType), FUN=summary)

# qao.id vs iScoreGrp says 3s 4s weren't scored in 2c as much as in 1m or 2p though -ves better than 2p & 0s like 1m.
# that explains 2c sum(iScore)==0.
  Group.1 iScoreGrp.-2 iScoreGrp.-1 iScoreGrp.0-2 iScoreGrp.3
1      1c            1            2             7           8
2      1m            2            0            10           6
3      1p            2            0             7           9
4      2c            2            3            11           2
5      2m            1            1             8           8
6      2p            3            2             5           8
3
#
# more importantly, iAttempt riskier-payoff -2:4 more than iAttempt -1:3!
> aggregate(qao["iAttempt"], by=list(qao$qaType), FUN=summary)
  Group.1        iAttempt
1    -1:3 logical, 16, 20
2    -2:4 logical, 17, 31
3    +0:3     logical, 24
#
# iAttempt much more in P (yet that doesn't score):
  Group.1 iAttempt.Mode iAttempt.FALSE iAttempt.TRUE
1       c       logical             12            24
2       m       logical             14            22
3       p       logical              7            29
#
     -2 -1  0 3 4
[1,]  2  3 11 1 1
     -2 -1  0  3  4
[1,]  8  6 37 22 17
> (1+1)/(2+3+11); (22+17)/(8+6+37)
[1] 0.125
[1] 0.7647059

    if(out.var == "iScore.fac"){ # beware: conditional data
      # iSco.mux <- sort(unique(qao$iScore))
      muxSco <- as.integer(colnames(myAgg[,2])); muxHi <- c( 0, 0,0,1,1); muxNotHi <- c( 1, 1,1,0,0)
      muxHiq    <- (myAgg[,2]) %*% muxHi
      muxNotHiq <- (myAgg[,2]) %*% muxNotHi
      muxHiSco    <- (myAgg[,2]) %*% (muxSco * muxHi)    # c( 0, 0,0,3,4)
      muxNotHiSco <- (myAgg[,2]) %*% (muxSco * muxNotHi) # c(-2,-1,0,0,0)
      muxHiq.odds <- (muxHiq + cAddForOdds) / (muxNotHiq + cAddForOdds)
      muxHiSco.odds <- (muxHiSco + cAddForOdds) / (muxNotHiSco + cAddForOdds)
      print("muxHiq.odds"); print(muxHiq.odds)
      print("muxHiSco.odds"); print(muxHiSco.odds)
    } # else continue
