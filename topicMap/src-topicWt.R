# TBD done:
# - read qTopicWt-2002to2019-ResonanceAcIn.xlsx sheet ResonancePCM directly instead of expecting .csv.
#   Note yellow and orange highlights cells where (a) typos are fixed or (b) approximations are made while author
#   mapped 98 Resonance topics to 65 JEE-Main (2020) topics. Some cells have comments too.
# - code for topicJEE that (a) are missing (for 2020) or (b) are new and have no past data.
# - code for combo topicJEE. Use that (not Resonance `topic`) for aggregating qTot year-wise then computing
#   q densities year-wise.
# - use (row) ID for topicJEE: after 4 chars of earlier subsec, append all following digits including any `;` separator.
# - explore subsec as factor.
# - sort on pcm.
# - write .csv with densities.
# - overlay plot with recency eg using "heat-map" colours that are redder for data nearer 2019.
# - upload .csv into Project with due acknowledgment to Resonance pdf.
# ref https://jeemain.resonance.ac.in/Assets/posts/JEE-Weightage-Analysis-Handout-v1.pdf for the base data
# which this project built on.
# - rather than relying on overall median or mean, re-consider year-wise densities.  Do that for (a) subject boxplot,
# (b) subSec boxplot, (c) pre2019 vs. 2019on scatterplot, and (d) a new (aggregated-topic) subSec map, which might
# be more robust, eg, syllabus changes could change topic names and relative content but unlikely to alter subSec
# "Inorganic Chemistry" or "Physical Chemistry".


getSubsec <- function(subject, topic){
  subSec <- paste0(substr((subject), 1, 4), substr(topic, 1, 3))
    # leftmost (upto) 4 Subject chars & leftmost 3 Section chars
  ans <- subSec # no need to worry about concatenated topic names coz they presently happen to be in same subSec!
  return(ans)
}
overlayJitterBoxplot <- function(dat.namesValue, colXs.ixPre2019, colXs.ix2019on){
  # ref https://www.r-graph-gallery.com/96-boxplot-with-jitter.html
  require(colorspace)
  # colrVec <- hcl(h=0, c=35, l=85, alpha=0.25, fixup=TRUE)
  colrVec <- rev(heat_hcl(length(colXs.ixPre2019), alpha=0.65)) # rev() to go from light to dark (over light background)
  colrVec <- c(colrVec, rep(colrVec[length(colXs.ixPre2019)], length(colXs.ix2019on)))
    # for each in colXs.ix2019on, repeat tail colour of colXs.ixPre2019
  # plot(3:36, 1:34, pch=20, col=colrVec, cex=0.75)
  boxplot(dat.namesValue)
  # Add data points
  dat.namesValue.dimnames <- dimnames(dat.namesValue)
  dat.names <- dat.namesValue.dimnames[[2]] # levels(data$names)
  levelProportions <- 60/34 # (summary(dat.names)["Length"]) / nrow(dat.namesValue) # summary(dat.namesValue[, dat.names])

  for(i in 1:length(dat.names)){
    thislevel <- dat.names[i]
    thisvalues <- dat.namesValue[, thislevel] # "value"]
    # take the x-axis indices and add a jitter, proportional to the N in each level
    # myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions/2) # was: amount=levelProportions[i]/2)
      # jitter(x, factor = 1, amount = NULL)
    myjitter <- jitter(rep(i, length(thisvalues)))
    points(myjitter, thisvalues, pch=20, col=colrVec, cex=0.75) # was: col=rgb(0,0,0,.9). size=0.3, alpha=0.5
  }
  return()
}
df2vec <- function(df, rowWise=TRUE){
  # ref https://stackoverflow.com/questions/2545228/convert-a-dataframe-to-a-vector-by-rows
  if(rowWise){
    vec <- as.vector(t(df))
  } else {  # you want to do it by columns you should use
    vec <- unlist(df)
  }
  return(vec)
}


date()
filexlsx <- "qTopicWt-2002to2019-ResonanceAcIn.xlsx" # was: filename <- "qTopicWt-2002to2019-ResonanceAcIn.csv"
fileout <- "qTopicDensityJEE.csv"
filedir <- "C://Users/SONY/Desktop/preBachelors/topicMap/"
require(xlsx)

  par(ask=TRUE)
  topicWt <- read.xlsx(paste(filedir, filexlsx, sep=""), sheetName="ResonancePCM",
    rowIndex=NULL, startRow=2, as.data.frame=TRUE, header=TRUE, colClasses=NA)
    # was: topicWt <- read.csv(paste(filedir, filename, sep=""), stringsAsFactors=FALSE, skip=1)
  print(str(topicWt))
  colnames(topicWt)[grep("qTopic.qTot.", colnames(topicWt))] <- "avgWt"
  # colnames(topicWt)[ncol(topicWt)] <- "avgWt"
  # topicWt$avgWt <- as.numeric(sub("%", "e-2", topicWt$avgWt)) # not as.numeric(topicWt$avgWt)
  # topicWt$subject <- as.factor(topicWt$subject)
  # topicWt$topic <- as.factor(topicWt$topic)
  print(str(topicWt))
  # for some reason for Maths `subject=m`, Resonance's `avgWt` column has values about 0.05 more than what's got by
  # dividing `qTopic / qTot` where `qTot=1240`. Other subjects' data matches.
  print(summary(topicWt))
    # while median q is 1 for most years, max is (17 in 2003 and) 5 in recent years across 92 topics,
    #   with physics having lesser (31) topics compared to 35 or 34 of chemistry and maths!
    # avgWt median and mean about 0.029, but beware this is not an average of year-wise topic proportions and might skew
    # weightage towards years nearer 2003 when more total 75 questions were there per subject.
  colXs <- grep("X20*", colnames(topicWt))
  topicsNA <- which(is.na(topicWt$qTopic) | is.na(topicWt$topicJEE)); print(topicWt[topicsNA,])
  print("dropping these JEE topics coz no prior data")
    # p B-Unit21: experimental skills
    # c C-Organic Chemistry-Unit27: chemistry in everyday life
  print("or dropping these Resonance topics coz not mapped to JEE-Main 2020 Syllabus topics")
    # m HEIGHT & DISTANCE
    # m SOLUTION OF TRIANGLE
    # m STATICS & DYNAMICS

  # colXs.subject.colSums <- aggregate(topicWt[,colXs], by=list(topicWt$subject), FUN=colSums)
  twt.org <- topicWt[-topicsNA,]
  tjee.NA <- topicWt$topicJEE[topicsNA]
  colXs.subject.colSums <- NULL
  for(asub in levels(twt.org$subject)){
    colXs.subject.colSums[[asub]] <- colSums(twt.org[twt.org$subject==asub, colXs])
  }
  str(colXs.subject.colSums)
  # for(asub in levels(twt.org$subject)){
    # vectorizing didn't work correctly:
    # twt.org[twt.org$subject==asub, colXs] <- 100* (twt.org[twt.org$subject==asub, colXs] /
    #   colXs.subject.colSums[[asub]])
  # }

  ### --- now, compute topicJEE q densities:
  twt <- twt.org[0,]; twt.ixTail <- 1 # init
  for(tjee in levels(twt.org$topicJEE)){ # was: 1:nrow(twt.org)
    if(tjee %in% tjee.NA){
      next # skip this topic
    } else { # continue
      colXs.colSums.tjee <- colSums(twt.org[twt.org$topicJEE==tjee, colXs])
      asub.tjee <- (twt.org$subject[twt.org$topicJEE==tjee])[1] # [1] just for cases where multiple topicJEE values occur
      # twt[twt TBD $topicJEE==tjee, colXs]
      tjeeDensities <- (100* (colXs.colSums.tjee / colXs.subject.colSums[[asub.tjee]]))
      twt[twt.ixTail, "subject"] <- asub.tjee # twt$subject[twt.ixTail] fails coz attempt to `$` from non-extant row
      twt[twt.ixTail, "topicJEE"] <- tjee
      twt[twt.ixTail, colXs] <- tjeeDensities
      twt.ixTail <- twt.ixTail + 1 # index to next row that could be appended into
      # incorrect coz chars: tjeeDensitiesRow <- cbind(subject=asub.tjee, topicJEE=tjee, t(tjeeDensities))
      # twt[[tjee]] <- tjeeDensities # asub.tjee?
      # tjeeDensitiesRow <- cbind(subject=rep(asub.tjee, length(colXs.colSums.tjee)),
      #    100* (colXs.colSums.tjee / colXs.subject.colSums[[asub.tjee]]))
      # if(! is.null(twt)){
      #   twt <- rbind(twt, tjeeDensitiesRow)
      # } else {
      #   twt <- tjeeDensitiesRow
      # }
    }
  }
  str(twt)
  twt.bak <- twt
  summary(twt)
    # typically, q density median 3.3 mean 5.0 max up to 20 in 2019 itself.  Max more than 4x median!

  colXs.ixTailPre2019 <- colXs[grep("X2018", colnames(twt[, colXs]))]
  colXs.ixPre2019 <- colXs[1] : colXs.ixTailPre2019
  colXs.ix2019on <- (colXs.ixTailPre2019 + 1) : tail(colXs, 1)
  tdens.vec.Xs <- df2vec(df=twt[, colXs], rowWise=TRUE)
  hist(tdens.vec.Xs); abline(v=c(1:5)*100/25, col="grey", lty="dashed")
    # mark integer count of questions in a 25-question paper.
  summary(tdens.vec.Xs)
  twt$avgWt <- rowMeans(twt[, colXs])
  hist(twt$avgWt) # topic-wise mean question densities across years (papers)
    # peak at about 1q /topic; right tail ends around 3q.
  plot(rowMeans(twt[, colXs.ixPre2019]) ~ rowMeans(twt[, colXs.ix2019on])) # consider individual observation, not means
  lm(rowMeans(twt[, colXs.ixPre2019]) ~ rowMeans(twt[, colXs.ix2019on]) + 0) # sans intercept, just for slope
    # 0.9263* pre2019 ie 2019on is more, possibly due to fewer 0 questions maybe coz multiple paper samples
    # within 1 year 2019.

  # twt.qTopicMeans <- rowMeans(twt[, colXs])
  # twt.qtmPre2019 <- rowMeans(twt[, c(3:20)])
  # twt.qtm2019 <- rowMeans(twt[, c(21:36)])
  # plot(twt.qTopicMeans)
  # points(twt.qTopicMeans, lty="solid", col="blue", pch="o")
  # points(twt.qtmPre2019, lty="solid", col="grey", pch="p")
  # points(twt.qtm2019, lty="solid", col="orange", pch="f")
  # plot(twt.qtmPre2019 ~ twt.qtm2019)
    # 0.89*twt.qtm2019 ie twt.qtm2019 is more, possibly due to fewer 0 questions maybe coz multiple paper samples
    # within 1 year 2019.
  # print(str(twt.qTopicMeans))
  # twt <- cbind(twt, qTopicMeans=twt.qTopicMeans)

  summary(twt$avgWt) # summary(twt$qTopicMeans)
  #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.2278  2.9581  4.5070  5.0000  6.2110 15.2170 
  tdens.vec.Xs.subjects <- list()
  for(asub in levels(twt$subject)){
    tdens.vec.Xs.subjects[[asub]] <- df2vec(df=twt[twt$subject==asub, colXs], rowWise=TRUE)
  }
  str(tdens.vec.Xs.subjects)
# > summary(twt$subject)
#  c  m  p 
# 26 16 18
  summary(twt$subject)
  boxplot(tdens.vec.Xs.subjects)
    # subject=m has greater proportion of topics with higher question densities, relative to other subjects.
    # consider overlaying raw observations with recency and jitter
  abline(h=c(1:5)*100/25, col="grey", lty="dashed") # mark integer count of questions in a 25-question paper.
  plot(avgWt ~ subject, data=twt)
    # recall c has much more topics relatively, which might explain:
    # c distribution for avgWt is lower relatively.  avgWt varies more for m.  p has fewer low-density topics.

  ### --- now get sample quantiles and boxplots:
  cProbs <- c(0:4) / 4
  tSumm <- apply(twt[, colXs], 1, function(rowVec) quantile(rowVec, prob=cProbs))
  twt.summ <- cbind(twt, t(tSumm))
  twt.summOrd <- twt.summ[order(twt.summ[,"50%"], twt.summ$avgWt, decreasing=TRUE),] # order on median then mean
  col.qTopicMeans <- c("subject", "topicJEE", "50%", "avgWt", "75%") # c(1, 2, 37:39)
  head(twt.summOrd[, col.qTopicMeans])
  tail(twt.summOrd[, col.qTopicMeans])
  twt.summOrd.Xst <- t(twt.summOrd[, colXs])
    # ref https://stackoverflow.com/questions/27942242/boxplot-for-each-row-of-a-data
  # boxplot(twt.summOrd.Xst) # boxplot(twt.ord[, colXs] ~ twt.ord$topic)
  overlayJitterBoxplot(twt.summOrd.Xst, (colXs.ixPre2019 - 2), (colXs.ix2019on - 2)) # -2 coz subject,topic cols dropped
  title(main="Boxplots of topic-wise (100*) density of questions", sub="in descending order of median then mean",
    xlab="topic ID related to JEE-Main Syllabus 2020", ylab="100*density of questions")
  abline(h=c(1:5)*100/25, col="grey", lty="dashed") # mark integer count of questions in a 25-question paper.
    # topics on left extreme (~6) typically (median) get disproportionately more questions asked.
    # similarly (~8) topics exist on right extreme with disproportionately fewer questions.
  points(twt.summOrd$avgWt, col="green", pch=8) # "*" overlay topic-wise means as well
  print(twt.summOrd[(twt.summOrd[,"50%"] >= (2*100/25)), col.qTopicMeans])
    # mark/show topics with median >=2*100/25 as surely worth allocating more resources.
  print(twt.summOrd[(twt.summOrd[,"75%"] >= (2*100/25)), col.qTopicMeans])
    # mark/show topics with 3rdQ >=2*100/25 as also worth allocating more resources.
    # these now include subject=c too, despite its more topics.
    # beware: there are 10 outliers that exceeded 16% (one in right extreme 2017!!),
    # and there are 6 boxplot whiskers that exceeded 16%!  Rarer but occurred!
  print(twt.summOrd[(twt.summOrd[,"50%"] < (1*(3/4)*(100/25))), col.qTopicMeans])
    # mark/show topics with median <(1*(3/4)*(100/25)) as candidates for diverting resources away from.
  print(head(twt.summOrd[(twt.summOrd$subject == 'c'), col.qTopicMeans]))
    # c has most topics, which might explain its topics coming lower in q density, but within c, what's it?

  # now, consider subsec densities:
  subSec <- getSubsec(twt.summOrd$subject, twt.summOrd$topicJEE)
  subSec.fac <- as.factor(subSec)
  str(subSec.fac) # note this doesn't include pB-U eg Experimental Skills with 20% weightage coz no past data!
  summary(subSec.fac)
# >   summary(subSec.fac)
# cA-P cB-I cC-O mUni pA-U 
#   10    8    8   16   18 
# >
  twt.summOrd <- cbind(twt.summOrd, subsec=subSec.fac)
  colXs.subSec.colSums <- NULL
  for(asubSec in levels(twt.summOrd$subsec)){
    colXs.subSec.colSums[[asubSec]] <- colSums(twt.summOrd[twt.summOrd$subsec==asubSec, colXs])
  }
  str(colXs.subSec.colSums)
  # boxplot(colXs.subSec.colSums)
  colXs.subSec.colSums.df <- as.data.frame(colXs.subSec.colSums)
  overlayJitterBoxplot(colXs.subSec.colSums.df, (colXs.ixPre2019 - 2), (colXs.ix2019on - 2))
    # -2 coz no subject,topic cols prior
  title(main="Boxplots of (100*) question density from (aggregated) topic groups", sub="",
    xlab="subject-topic groups related to JEE-Main Syllabus 2020", ylab="100*density of questions")
    # within subject chemistry, apparently, "cA-P" (Physical Chemistry median 40%) gets greater proportion of questions
    # in papers; "cC-O" (Organic Chemistry median about 30%) is next and this varies least across (year-wise) papers.
    # (There's only one group each for Maths and Physics; so, their question densities stay at 100%.)
    # Now, consider the raw data shown as points with jitter:
    # - for "cA-P", redder (recent-papers') points appear to be lower than less-red ones
    # - for "cB-I", redder points appear to be higher.
    # So, let's investigate their summaries:
  for(i in 1:3){
    asubSec <- dimnames(colXs.subSec.colSums.df)[[2]][i]
    print(paste0(asubSec, " pre2019 then 2019on:"))
    print(summary(colXs.subSec.colSums.df[(colXs.ixPre2019 - 2), asubSec]))
    print(summary(colXs.subSec.colSums.df[(colXs.ix2019on - 2), asubSec]))
  }

  # boxplot(twt.summOrd[, "50%"] ~ subSec.fac) # note: cA-P cB-I medians hardly vary!  in contrast, mUni varies the most.
  # boxplot(twt.summOrd[, "75%"] ~ subSec.fac) # now cA-P cB-I too vary: cB-I varies least
  # boxplot(twt.summOrd[, "avgWt"] ~ subSec.fac) # cA-P cB-I too vary: cA-P varies least & tends to be higher than cB-I
  # twt.summOrd <- cbind(twt.summOrd, subsec=subSec.fac)

  write.csv(twt.summOrd, file=paste0(filedir, fileout), row.names=TRUE) # TRUE coz ID. row.names=FALSE, col.names=TRUE

stop()
### --- output with JEE topics:
>   print(twt.summOrd[(twt.summOrd[,"50%"] >= (2*100/25)), col.qTopicMeans])
   subject                                       topicJEE       50%     avgWt
47       m                   Unit11: co-ordinate geometry 16.666667 15.217004
59       m Unit8: limit, continuity and differentiability 13.333333 12.749495
13       p                       A-Unit11: electrostatics 10.000000  8.853005
60       m                       Unit9: integral calculus 10.000000  8.749959
12       p               A-Unit10: oscillations and waves 10.000000  8.651833
14       p                  A-Unit12: current electricity  8.666667  8.033995
>     # mark/show topics with median >=2*100/25 as worth allocating more resources.
>
>   print(twt.summOrd[(twt.summOrd[,"75%"] >= (2*100/25)), col.qTopicMeans])
   subject                                                                 topicJEE       50%     avgWt       75%
47       m                                             Unit11: co-ordinate geometry 16.666667 15.217004 17.241379
59       m                           Unit8: limit, continuity and differentiability 13.333333 12.749495 13.793103
13       p                                                 A-Unit11: electrostatics 10.000000  8.853005 10.000000
60       m                                                 Unit9: integral calculus 10.000000  8.749959 10.344828
12       p                                         A-Unit10: oscillations and waves 10.000000  8.651833 10.000000
14       p                                            A-Unit12: current electricity  8.666667  8.033995 10.000000
24       p                                               A-Unit5: rotational motion  7.333333  8.309396 10.000000
18       p                                                         A-Unit16: optics  7.333333  8.164120 10.000000
49       m                                                   Unit13: vector algebra  6.920498  7.451155 10.000000
50       m                                       Unit14: statistics and probability  6.896552  7.569305  8.173077
19       p A-Unit18: atoms and nuclei;A-Unit17: dual nature of matter and radiation  6.666667  8.781704 10.000000
41       c          C-Organic Chemistry-Unit23: organic compounds containing oxygen  6.666667  7.488859 10.000000
5        c     A-Physical Chemistry-Unit4: chemical bonding and molecular structure  6.666667  6.120830  9.166667
28       p                                         A-Unit9: kinetic theory of gases  6.666667  5.588235 10.000000
38       c   C-Organic Chemistry-Unit20: some basic principles of organic chemistry  4.857143  5.896677  9.375000
>
>   print(twt.summOrd[(twt.summOrd[,"50%"] < (1*(3/4)*(100/25))), col.qTopicMeans])
   subject
22       p
37       c
44       c
40       c
42       c
31       c
56       m
                                                                             topicJEE
22                                                            A-Unit3: laws of motion
37 C-Organic Chemistry-Unit19: purification and characterisation of organic compounds
44              C-Organic Chemistry-Unit28: principles related to practical chemistry
40                  C-Organic Chemistry-Unit22: organic compounds containing halogens
42                  C-Organic Chemistry-Unit24: organic compounds containing nitrogen
31                                             B-Inorganic Chemistry-Unit13: hydrogen
56                                                      Unit5: mathematical induction
        50%    avgWt
22 2.159091 2.146613
37 1.666667 2.279475
44 1.666667 2.156863
40 0.000000 2.098294
42 0.000000 1.950980
31 0.000000 1.176471
56 0.000000 0.227797
>     # mark/show topics with median <(1*(3/4)*(100/25)) as candidates for diverting resources away from.
>
>   print(head(twt.summOrd[(twt.summOrd$subject == 'c'), col.qTopicMeans]))
   subject                                                                                                    topicJEE      50%    avgWt
41       c                                             C-Organic Chemistry-Unit23: organic compounds containing oxygen 6.666667 7.488859
35       c B-Inorganic Chemistry-Unit17: co-ordination compounds;B-Inorganic Chemistry-Unit16: d- and f-block elements 6.666667 6.127769
5        c                                        A-Physical Chemistry-Unit4: chemical bonding and molecular structure 6.666667 6.120830
43       c                               C-Organic Chemistry-Unit25: polymers;C-Organic Chemistry-Unit26: biomolecules 6.666667 5.701490
1        c                                                A-Physical Chemistry-Unit1: some basic concepts in chemistry 6.190476 5.495607
38       c                                      C-Organic Chemistry-Unit20: some basic principles of organic chemistry 4.857143 5.896677
>
### --- end

### --- earlier output with Resonance topics:
>   print(twt.summOrd[(twt.summOrd[,"50%"] >= (1*100/25)), col.qTopicMeans])
   subject                                        topic      50% qTopicMeans
2        p                          CURRENT ELECTRICITY 8.666667    8.033995
28       p                               MODERN PHYSICS 6.666667    8.781704
63       m                                       VECTOR 6.666667    7.246499
4        p                       ELECTRO MAGNETIC FIELD 6.666667    6.460848
41       m                       MATRIES & DETERMINANTS 6.666667    6.345238
84       c TRANSITION ELEMENTS & COORDINATION CHEMISTRY 6.666667    6.127769
56       m                         DEFINITE INTEGRATION 6.666667    5.740896
96       c        CARBOHYDRATES, AMINO ACIDS & POLYMERS 6.666667    5.701490
8        p                         KTG & THERMODYNAMICS 6.666667    4.607843
30       p        ELECTROMAGNETIC WAVES & COMMUNICATION 6.666667    4.411765
72       c                             CHEMICAL BONDING 6.190476    5.606570
1        p                               ELECTROSTATICS 6.000000    5.529221
54       m             APPLICATION OF DERIVATIVES (AOD) 5.523810    5.707283
25       p                          RIGID BODY DYNAMICS 5.393939    5.347403
38       m                            SEQUENCE & SERIES 4.500000    4.912465
64       m                     COORDINATE GEOMETRY (3D) 4.500000    3.790616
13       p                           GEOMETRICAL OPTICS 4.000000    4.897313
>     # mark/show topics with median >=1*100/25 as worth allocating more resources.
>   print(twt.summOrd[(twt.summOrd[,"50%"] < 1), col.qTopicMeans])
   subject                               topic       50% qTopicMeans
94       c CARBOXYLIC ACID & THEIR DERIVATIVES 0.6666667  1.93048128
92       c               ALDEHYDES AND KETONES 0.6666667  1.74509804
47       m      INVERSE TRIGONOMETRIC FUNCTION 0.6666667  1.68697479
77       c        GENERAL ORGANIC CHEMISTRY-II 0.0000000  2.25490196
90       c                       ALKYL HALIDES 0.0000000  2.09829386
95       c                              AMINES 0.0000000  1.95098039
18       p                   PROJECTILE MOTION 0.0000000  1.56862745
6        p                 ALTERNATING CURRENT 0.0000000  1.47459893
46       m              TRIGONOMETRIC EQUATION 0.0000000  1.32843137
57       m                    AREA UNDER CURVE 0.0000000  1.27521008
15       p                    UNIT & DIMENSION 0.0000000  1.26050420
75       c            HYDROGEN & ITS COMPOUNDS 0.0000000  1.17647059
20       p             NEWTON'S LAWS OF MOTION 0.0000000  1.12700535
62       m                  STATICS & DYNAMICS 0.0000000  1.10784314
21       p                            FRICTION 0.0000000  1.01960784
23       p                     CIRCULAR MOTION 0.0000000  1.00000000
9        p                       HEAT TRANSFER 0.0000000  0.98039216
60       m                SOLUTION OF TRIANGLE 0.0000000  0.91666667
48       m                   HEIGHT & DISTANCE 0.0000000  0.72619048
97       c                    STEREO CHEMISTRY 0.0000000  0.72619048
87       c                QUALITATIVE ANALYSIS 0.0000000  0.70588235
19       p                     RELATIVE MOTION 0.0000000  0.58823529
83       c                   NUCLEAR CHEMISTRY 0.0000000  0.51426025
43       m              MATHEMATICAL INDUCTION 0.0000000  0.22128852
61       m            EXPONENTIAL & LOG SERIES 0.0000000  0.07352941
>     # mark/show topics with median <1 as candidates for diverting resources away from.
> date()
[1] "Tue Dec 31 22:25:08 2019"
> 
### ---

stop()
  # UIK knees could be estimated, but first, do a basic robustness check eg by splitting <=2018 and >2018 data.
  # alt: cluster topics considering factors such as subject along with exam-wise q proportions.
  # alt: ARIMA or other econometric methods could be considered for forecasting future q count,
  # considering holdout/validation.

  # consider tsfeatures R package https://cran.r-project.org/web/packages/tsfeatures/vignettes/tsfeatures.html.
