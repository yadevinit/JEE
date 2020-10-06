# 1: ï»
# ref https://stackoverflow.com/questions/20889996/how-do-i-remove-all-non-ascii-characters-with-regex-and-notepad:
# > duh<-read.table(fname, stringsAsFactors=FALSE)
# > duh[,1]
# [1] "4+2" "ï»"  "3+6"
# > sub("[^\x1F-\x7F]+", "", duh[,1])
# [1] "4+2" ""    "3+6"

# https://wesslen.github.io/assets/documents/papers/tmm.pdf guides choices, for new users too. It says:
# [(1) hypothesis and question formation, (2) design and data collection, (3) data pre-processing, and (4) topic modeling]
# For Project Occipital:
# (1) Hypothesis: there are features of a question that explain its difficulty, as evidenced by JEE-Advanced data.
# Features include topics associated with a question.  Those (latent) topic features can be modeled by non-expert humans.
# Those topic features explain difficulty over and above question type and marking scheme.  Those impact estimates
# are what the world misses knowing to subsequently (reproducibly) inform teaching-learning of a population.
# (2) Already addressed by the Project.
# (3) Let's begin:

# ref https://github.com/wesslen/topicApp
packages <- c("shiny","quanteda","shinydashboard","RColorBrewer","DT","visNetwork","ggwordcloud",
              "igraph","tm","reshape","grid","tidyverse","shinyjs","shinyBS","stm")
if(FALSE){ # Run if packages have not been installed.
  install.packages(packages)
  install.packages("devtools")
  install.packages("pdftools")
  install.packages("tesseract")
  devtools::install_github("wesslen/topicApp")
  # devtools::install_github("trinker/textreadr")
} # else continue

for(pkg in packages){
  library(pkg, character.only=TRUE)
  # "tm" attaches "NLP". annotate is masked from package:ggplot2.
  # {as.DocumentTermMatrix, stopwords} are masked from package:quanteda.
}
library(pdftools)
library(tesseract)
library(topicApp)
# library(textreadr)

cWorkDir <- paste0(path.expand("~"), "/../Desktop/preBachelors/www/solnJEEAdvanced")
cFile.noOCR <- paste0(cWorkDir, "/jads-nocr.rds")
cFile.OCR <- paste0(cWorkDir, "/jads-ocr.rds")
cFF <- "\r\n\f" # Form Feed or Page Break
concatPages <- function(jads){ # while maintaining each file as distinct
  print(str(jads))
  jads.out <- jads
  for(iFile in 1:length(jads.out)){
    jads.out[[iFile]] <- paste(jads.out[[iFile]], collapse=cFF)
  }
  print(str(jads.out))
  return(jads.out)
}
cYPCSubSec.order <- 1:5 # order of YPCSubSec
cYPCSubSec <- list(
  patY="201[[:digit:]]", # Year. "2007", "2067", \n"2016" also occur in files.
    # [[:digit:]] is portable, not [0-9]
  patP="PAPER[^\r\n]*[12]", # (PAPER|SHIFT) Paper or Shift. '^' sometimes is negation.
  patC="CODE[^\r\n]*[[:digit:]]{1,2}", # Code, which varies ordering of questions for the same Paper
  patSub="PART[^\r\n]*[I]{1,3}[^\r\n]* | PHYSICS | CHEMISTRY | MATH", # PART...Subject, possibly till end of line (not $).
  patSec="SECTION[^\r\n]*[[:digit:]]") # Section within Paper*Subject, possibly till end of line (not $).
cPatQOS <- list(
  "(Q\\.[123456789][[:digit:]]*|[123456789][[:digit:]]*\\.[[:space:]])",
    # beware: "[123456789][[:digit:]]*\\.[^[:digit:]]" matches "7.B" too; so "[[:space:]]" instead of "not digit".
    # Question: Q.1 or 1; sometimes *Q.1 coz indicating 11th std portions. "100." spuriously matched.
    # [\< and \> match the empty string at the beginning and end of a word.] but POSIX locale whereas Windows C locale!
    # [^0[:digit:]] instead of less-portable [1-9], but negates 0 as well as any digit!
    # was: "(Q\\.[123456789][[:digit:]]*|[123456789][[:digit:]]*\\.)" but matched 2.00 wrongly.
    # was: "^[*]?Q\\.[123456789][[:digit:]]|^[123456789][[:digit:]]\\."
    # was: "Q\\.[123456789][[:digit:]]|[123456789][[:digit:]]\\.[[:space:]]"
  "\\(A)", # Options. "(A)" is treated as a parenthesized regular expression!
    # was: "^[[:space:]]*\\(A)"
    # was: "^\\(A)", "[[:space:]]*\\(A\\)[[:space:]]+"
  "(Sol|Ans)") # (Answer-key ie correct Option and) Solution
    # was: "^(Sol|Ans)"
    # [regexpr and gregexpr support 'named capture'. If groups are named, e.g., "(?<first>[A-Z][a-z]+)"
    # then the positions of the matches are also returned by name.]
myregmatches.jad <- function(jads, j, locYPCSubSec, wantUniquePattern){
  ans.jad <- list()
  for(iYPCSubSec in 1:length(locYPCSubSec)){
    ans.jad[[iYPCSubSec]] <- regmatches(jads[j], locYPCSubSec[[iYPCSubSec]][j])
    if(iYPCSubSec %in% wantUniquePattern){
      ans.jad[[iYPCSubSec]] <- list(unique(ans.jad[[iYPCSubSec]][[1]]))
    } # else continue
  }
  return(ans.jad)
}
myregmatches <- function(jads, locYPCSubSec, wantUniquePattern){
  ans <- list()
  for(j in 1:length(jads)){
    ans[[j]] <- myregmatches.jad(jads, j, locYPCSubSec, wantUniquePattern)
  }
  return(ans)
}
locatePattern <- function(jads, pattern){
  locPattern <- list()
  for(iPattern in 1:length(pattern)){
    locPattern[[iPattern]] <- gregexpr(pattern[[iPattern]], jads)
      # alt: gregexpr() for global; or regexec() for first match.
      # [fixed = FALSE, perl = FALSE: use POSIX 1003.2 extended regular expressions (the default).]
      # Regarding substring():
      # [These functions are often used with nchar to truncate a display. That does not really work (you want to limit
      # the width, not the number of characters, so it would be better to use strtrim), but at least make sure you use
      # the default nchar(type = "c").]
  }
  return(locPattern)
}
myPDFtoOCR <- function(files, iFiles){
  for(iFile in iFiles){
    pngfile <- pdftools::pdf_convert(files[iFile], dpi=600)
    # Converting page 1 to ocrscan_1.png... done!
    text <- tesseract::ocr(pngfile)
    print(sum(nchar(text))); print(glimpse(text))
    catext <- concatPages(list(text))
    print(nchar(catext)); print(glimpse(catext))
    fname <- paste0(substring(files[iFile], first=1, last=(nchar(files[iFile]) - 4)), # default: last = 1000000L
      "-ocr.rds")
    saveRDS(catext, file=fname) # preferable over save() coz readRDS() "functional" and not
      # attached to earlier object names. default: ascii=FALSE which might be more portable but less efficient.
    # catext2 <- readRDS(file=fname); identical(catext2, catext)
  }
  return()
}
myChooseFiles <- function(wd=paste0(cWorkDir, "/*.*"), filext=c("pdf", "All")){
  if(interactive() && .Platform$OS.type == "windows"){
    files <- choose.files(default=wd, filters=Filters[filext,])
  } else { # error, for now!
    stop()
  }
  # alt: list.files(pattern="pdf$")
  # following fails likely coz extended regular expressions haven't been selected somewhere; or locale matters.
  # list.files(mywd, pattern="*20[0-9]{2}*[.]pdf", recursive=FALSE, include.dirs=FALSE, no..=TRUE)
  #   [:digit:] depends on POSIX locale.
  return(files)
}
myChoosePDFnoOCR <- function(){
  # ref https://uvastatlab.github.io/2019/05/14/reading-pdf-files-into-r-for-text-mining/
  files <- myChooseFiles(filext=c("pdf"))
  jads <- lapply(files, pdf_text)
  # [PDF error: Expected the optional content group list, but wasn't able to find it, or it isn't an Array].
  # Ignore such (harmless) errors coz [The required key /OCGs is indeed missing] from some .pdf files, as per
  # ref paste0("https://tex.stackexchange.com/questions/66108",
  #   "/syntax-error-expected-the-optional-content-group-list-but-wasnt-able-to-find")
  print(length(jads)); print(lapply(jads, nchar))
  # (3b.1) Concat the pages. Stray headers/footers might have to be dropped; later.
  jads <- concatPages(jads)
  for(iFile in 1:length(files)){
    fname <- paste0(substring(files[iFile], first=1, last=(nchar(files[iFile]) - 4)), # default: last = 1000000L
      "-noc.rds") # not OCR
    saveRDS(jads[iFile], file=fname) # jads[[iFile]] is a char string, whereas we want a list containing that
      # (char string) for consistency
  }
  return(files)
}
myreadRDS <- function(files){
  # jads <- list()
  # for(iFile in 1:length(files)){
  #   jads[[iFile]] <- readRDS(file=files[iFile])
  # }
  jads <- lapply(files, readRDS)
  return(jads)
}
myunlist <- function(nestedList){
  # ref https://stackoverflow.com/questions/16300344/how-to-flatten-a-list-of-lists
  ans <- unlist(nestedList, recursive=FALSE)
  return(ans)
}
substrXY <- function(Xi, Yi, moreArgsList){
  stopifnot(length(moreArgsList) == 1)
  ans <- substring(moreArgsList[[1]], first=Xi, last=Yi)
  return(ans)
}
substringQOS <- function(jad, iChars){
  iChars.len <- length(iChars)
  stopifnot(iChars.len >= 2)
  QOSs <- mapply(FUN=substrXY, X=iChars[1:(iChars.len-1)], Y=(iChars[2:(iChars.len)] - 1),
    MoreArgs=list(jad), SIMPLIFY=TRUE, USE.NAMES=TRUE)
    # last= # iChars + attr(iChars, "match.length"))
  return(QOSs)
}
mySaveQOSs <- function(jads, locQOS, forOCR=FALSE,
  mustWrite=FALSE){
  QOSs <- mapply(FUN=substringQOS, jads, locQOS[[1]], # list of vectors of int where matched. was: jads[[1]]
    MoreArgs=NULL, SIMPLIFY=TRUE, USE.NAMES=TRUE)
  if(mustWrite){
    fname <- paste0(cWorkDir, "/jads-", ifelse(forOCR, "ocr", "nocr"), "-qos.rds")
    saveRDS(QOSs, file=fname) # default: ascii=FALSE.
  } # else continue
  return(QOSs)
}
list2matrix <- function(aList){
  # ref https://
  # stackoverflow.com/questions/15201305/how-to-convert-a-list-consisting-of-vector-of-different-lengths-to-a-usable-data
  n.obs <- sapply(aList, length)
  seq.max <- seq_len(max(n.obs))
  mat <- t(sapply(aList, "[", i = seq.max))
  return(mat)
}
mylocQ.toCSV <- function(locQ,
  fname=paste0(cWorkDir, "/jads-", ifelse(forOCR, "ocr", "nocr"), "-locQOS.csv")){
  locQm <- list2matrix(locQ)
  # alt: as.dataframe() [If a list is supplied, each element is converted to a column in the data frame.]
  write.csv(locQm, file=fname)
  return()
}
cCharWidth <- 11 # Count of chars to display in window.
headCharVec <- function(chvec, n=cCharWidth){
  ans <- substring(chvec, first=1, last=n)
  # print(ans)
  return(ans)
}
mergeCharVec <- function(chVec, mergeSpecs, collapseChar=" "){
  mergedVec <- c()
  imv <- 1; ich <- 1; jms <- 1
  while(ich <= length(chVec)){
    if(jms <= length(mergeSpecs)){
      mspec <- mergeSpecs[[jms]]
    } else {
      mspec <- c()
    }
    if(ich %in% mspec){
      mergedVec[imv] <- paste0(chVec[mspec], collapse=collapseChar) # was: collapse="" which showed words without spaces!
      ich <- max(mspec) + 1
      jms <- jms + 1
    } else {
      mergedVec[imv] <- chVec[ich]
      ich <- ich + 1
      # jms stays as is.
    }
    imv <- imv + 1
  }
  return(mergedVec)
}
moreChVec <- function(chVec, ichVec, n=350){
  print(nchar(chVec[ichVec])); print(paste("showing nchar=", n))
  print(headCharVec(chVec[ichVec], n))
  return()
}
cPattern.PfQ <- "Paragraph for Question"
myPrefixPostQnum <- function(prefix, ontoString){
  # qnum <- substring(ontoString, first=1, last=3) # or 4.
  qnumReplacement <- paste0("\\1 ", prefix)
  ans <- sub(pattern="^([[:digit:]]+[.])", replacement=qnumReplacement, x=ontoString, fixed=FALSE)
  # [include backreferences "\1" to "\9" to parenthesized subexpressions of pattern.]
  return(ans)
}
multMetaSelectedQOSs <- function(till.imetaYPq, metaYPq, mcv){
  mmsqoss <- lapply(1:till.imetaYPq, FUN=metaSelectedQOSs, metaYPq, mcv)
  return(mmsqoss)
}
metaSelectedQOSs <- function(imetaYPq, metaYPq, mcv){
  iSelectVec <- metaYPq[[imetaYPq]]$iMerged
  msqoss <- mcv[iSelectVec] # ...[[1]]$iMerged==1:61
  # write.table(msqoss, file=fname, append=TRUE, row.names=FALSE, col.names=FALSE) # Beware: append= here.
  return(msqoss)
}
myCollapseQ <- function(absRowNum, countRows, qoss.upd, mySep=""){
  stopifnot(countRows >= 2) # else no need to collapse.
  ans <- paste0(qoss.upd[absRowNum:(absRowNum + countRows - 1),], collapse=mySep) # was: [,1].
  return(ans)
}
getNextQOS <- function(relativeRowNum, zeroRowNum, qoss.upd, metaYP){
  chrrNum <- paste0("r", relativeRowNum)
  ans <- switch(metaYP, # Beware: switch() behaves distinctly for EXPR char vs. integer. Latter args needed in sequence.
    Y2013P2 = Y2013P2.getNextQOS(chrrNum, relativeRowNum, zeroRowNum, qoss.upd, metaYP),

    # default case for metaYP:
    do.call(paste0(metaYP, ".getNextQOS"), args=list(chrrNum, relativeRowNum, zeroRowNum, qoss.upd, metaYP),
      quote=FALSE)
    # try(do.call()); upon failure stop(paste(metaYP, "unknown"))
  )
  mnxtq <- ans[! is.na(ans)]
  return(mnxtq)
}
getAllQOS <- function(absoluteRowNums, zeroRowNum, qoss.upd, metaYP){
  relativeRowNums <- absoluteRowNums - zeroRowNum # zeroRowNum is an offset.
  allQOS <- lapply(relativeRowNums, FUN=getNextQOS, zeroRowNum, qoss.upd, metaYP)
  return(unlist(allQOS))
}
textOptionOCR <- function(forOCR){
  return(ifelse(forOCR, "ocr", "nocr"))
}
old.putFileQOSs <- function(qoss, prefix=paste0(cWorkDir, "/jads-"), fromOCR=TRUE, suffix="-QOSs", ext=".csv"){
  # use .rds instead of problematic .csv for QOS and other strings. So, rewrite following code before reuse:
  fname <- paste0(prefix, textOptionOCR(fromOCR), suffix, ext); print(fname)
  write.table(NULL, file=fname, append=FALSE, row.names=FALSE, col.names=FALSE)
  for(i in 1:length(qoss)){
    # was: concatPages(qoss) but that places QOSs into one char string collapsed by cFF.
    # whereas each QOS is to be seen as a Document for Topic Modeling.
    qosVec <- qoss[[i]]
    # alt: write.table(qosVec, file=fname, append=TRUE, row.names=FALSE, col.names=FALSE) # Beware: append=TRUE here.
    # till 2020Feb02 was:
    for(j in 1:length(qosVec)){
      write.table(qosVec[j], file=fname, append=TRUE, row.names=FALSE, col.names=FALSE) # Beware: append=TRUE here.
    }
  }
  return()
}
putFileQOSs <- function(qoss, prefix=paste0(cWorkDir, "/datJADqos-"), fromOCR=TRUE, suffix="", ext=".rds"){
  fname <- paste0(prefix, textOptionOCR(fromOCR), suffix, ext); print(fname)
  saveRDS(qoss, file=fname) # default: ascii=FALSE.
  return()
}
getFiledQOSs <- function(prefix=paste0(cWorkDir, "/datJADqos-"), fromOCR=TRUE, suffix="", ext=".rds", doUnlistRec=TRUE){
  # (fromOCR=c(TRUE, FALSE), ext=c(".rds", ".csv"))
  fname <- paste0(prefix, textOptionOCR(fromOCR), suffix, ext); print(fname)
  qoss <- if(ext == ".rds"){
    dat.rds <- myreadRDS(fname)
    if(doUnlistRec){
      dat.rds.flat <- unlist(dat.rds, recursive=TRUE) # alt: myunlist(myunlist(dat.rds))
    } else {
      dat.rds.flat <- dat.rds
    }
    dat.rds.flat.df <- as.data.frame(dat.rds.flat, stringsAsFactors=FALSE)
  } else { # ".csv" or other ext.
    read.table(file=fname, header=FALSE, stringsAsFactors=FALSE)
  }
  print(dim(qoss)); print(str(qoss))
  return(qoss)
}
getMetaSubjectsVec <- function(y, metaYPq, whichSubs=c("P", "C", "M")){
  subvec <- c() # init
  # subnames <- names(metaYPq[[y]])
  my <- metaYPq[[y]]
  for(asub in whichSubs){
    subvec[my[[asub]]] <- asub # names(my[[asub]])
  }
  return(subvec)
}
metaToVars <- function(metaYPq, whichSubs=c("P", "C", "M")){
  yvec <- c() # init
  # names(metaYPq[[names(metaYPq)[1]]])
  Ys <- names(metaYPq)
  YsSubjectsVec <- lapply(Ys, FUN=getMetaSubjectsVec, metaYPq, whichSubs=whichSubs)
  for(i in 1:length(Ys)){
    yvec <- c(yvec, rep(Ys[i], times=length(YsSubjectsVec[[i]])))
  }
  YsSubjectsVec.flat <- unlist(YsSubjectsVec, recursive=TRUE)
  mvarsdf <- data.frame(subject=YsSubjectsVec.flat, YP=yvec)
  return(mvarsdf)
}
pasteYP <- function(ivec, YPchrvec){
  ivec3 <- ivec * 3
  ans <- paste0(YPchrvec[ivec3 - 1], YPchrvec[ivec3])
  return(ans)
}
YPintvec <- function(YPchrvec){
  yyyy.p.vec <- unlist(strsplit(YPchrvec, split="Y|P"))
  stopifnot(length(yyyy.p.vec) == 3*length(YPchrvec))
  yyyypvec <- unlist(lapply(1:(length(yyyy.p.vec) / 3), FUN=pasteYP, yyyy.p.vec))
  # yyyypvec <- paste0(yyyy.p, collapse="") # alt: paste0(yyyy.p[2], yyyy.p[3])
  ans <- as.integer(yyyypvec)
  return(ans)
}
YPint <- function(YPchr){
  yyyy.p <- unlist(strsplit(YPchr, split="Y|P"))
  stopifnot(length(yyyy.p) == 3)
  yyyyp <- paste0(yyyy.p, collapse="") # alt: paste0(yyyy.p[2], yyyy.p[3])
  ans <- as.integer(yyyyp)
  return(ans)
}
thoughtQuotesForTopicPair <- function(anSTMfit, topics=c(1,2), shortdoc, nCovar=3){
  # There would be example documents highly associated with given topics (pair). This prints them to a graphics device.
  thoughts1 <- findThoughts(anSTMfit, texts = shortdoc, n = nCovar,
    topics[1])$docs[[1]]
  thoughts2 <- findThoughts(anSTMfit, texts = shortdoc, n = nCovar,
    topics[2])$docs[[1]]
  op <- par(mfrow = c(1, length(topics)), mar = c(0.5, 0.5, 1, 0.5))
  plotQuote(thoughts1, width = 30, main = paste0("Topic ", topics[1]))
  plotQuote(thoughts2, width = 30, main = paste0("Topic ", topics[2]))
  # At end of plotting, reset to previous settings:
  par(op)
  return()
}
thoughtQuotesForTopicVec <- function(anSTMfit, topics=c(1,2), shortdoc, nMostAssociatedDocs=5){
  # There would be example (nCovar count of) documents highly associated with given topics (vector).
  # This prints them to a graphics device.
  thoughts <- findThoughts(anSTMfit, texts = shortdoc, n=nMostAssociatedDocs, topics)
    # Complex queries eg [where = treatment==1 & Topic2>.2] are possible.
    # [Returns the top n documents ranked by the MAP estimate of the topic's theta value (which captures the modal
    # estimate of the proportion of word tokens assigned to the topic under the model).]
  op <- par(mfrow = c(1, length(topics)), mar = c(0.5, 0.5, 1, 0.5))
  # plot(NULL, main=paste0("Topics ", paste0(min(topics), ":", max(topics)))
  plot(thoughts, main=paste0("Topics ", paste0(min(topics), ":", max(topics))))
    # Beware: main= repeats on sub-plots, and title= did not place any title :-(.
    # plot() plots 1 thoughts (ie its associated docs) at a time. That's why the par() prior.
  # alt: lapply(thoughts$docs, FUN=plotQuote)
  # plotQuote(thoughts$docs[[1]], width = 30, main = paste0("Topics ", paste0(topics, collapse=",")))
  # thoughts$docs[[1]][2:3] to select a subset of examples.
  # At end of plotting, reset to previous settings:
  par(op)
  return(thoughts)
}
visNetworkCorrel <- function(ctmFit, topicNames, topic){
  # Let's create a network correlation plot. We’ll use a static network first.

  library(igraph); library(visNetwork)

  # Beware: topic from tmFit, where stmFit topics differ from ctmFit ones.
  # Maybe this works only for ctmFit, not stmFit coz:
  # [Error in apply(topicNames$prob, 1, function(x) paste0(x, collapse = " \n ")) : 
  #  dim(X) must have a positive length]
  # but fails there too.
  mod.out.corr <- topicCorr(ctmFit, cutoff=.01); plot(mod.out.corr)

  # output links and simplify
  links2 <- as.matrix(mod.out.corr$posadj)
  net2 <- graph_from_adjacency_matrix(links2, mode = "undirected")
  net2 <- igraph::simplify(net2) 

  # create the links and nodes
  links <- igraph::as_data_frame(net2, what="edges")
  nodes <- igraph::as_data_frame(net2, what="vertices")

  # set parameters for the network
  nodes$shape <- "dot"  
  nodes$title <- paste0("Topic ", topic$TopicNumber)
  nodes$label <- apply(topicNames$prob, 1, function(x) paste0(x, collapse = " \n ")) # Node label
  nodes$size <- (topic$TopicProportions / max(topic$TopicProportions)) * 30
  nodes$font <- "18px"
  nodes$id <- as.numeric(1:k)

  visNetwork(nodes, links, width="100%",  height="800px", main="Topics") %>% 
    visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical")) %>%
    visNodes(scaling = list(max = k)) %>% # was: 60.
    visIgraphLayout(smooth = T) %>%
    visInteraction(navigationButtons = T)

  return()
}
updateMergedCharVec <- function(mcv){
    # - Define Y2012P2.get*() that post-processes to *move* ParaForQuestions into Qs where missed; then drops exclusive
    #   paras since they are not Qs completely and have no separate ID.
    # move PfQ in mcv[73,76,79] into corresponding following 2 char strings. Then empty those PfQs. Then update metaYPq.
    para0910 <- mcv[73]
    q09 <- sub(pattern="9,", replacement="9.", x=mcv[74]) # coz "9," at start makes myPrefixPostQnum() miss pasting PfQ!
    q09 <- myPrefixPostQnum(para0910, q09)
    q10 <- myPrefixPostQnum(para0910, mcv[75])
    para1112 <- mcv[76]
    q11 <- myPrefixPostQnum(para1112, mcv[77])
    q12 <- myPrefixPostQnum(para1112, mcv[78])
    para1314 <- mcv[79]
    q13 <- myPrefixPostQnum(para1314, mcv[80])
    q14 <- myPrefixPostQnum(para1314, mcv[81])
    mcv[73:81] <- c("", q09, q10, "", q11, q12, "", q13, q14)
    return(mcv)
}
myConvUTF8 <- function(x){
  Encoding(x) <- "UTF-8" # alt: "latin1"
  xx <- iconv(x, "UTF-8", "UTF-8", sub='') ## replace any non UTF-8 by ''
  return(xx)

  # mydata[,2:3] <- apply(mydata[,2:3], 2, function(x) iconv(x, to="utf-8"))
  # ref https://stackoverflow.com/questions/17291287/how-to-identify-delete-non-utf-8-characters-in-r:
  # x <- "fa\xE7ile"
  # Encoding(x) <- "UTF-8"
  # iconv(x, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''
  # "faile"
  # Here note that if we choose the right encoding:
  # x <- "fa\xE7ile"
  # Encoding(x) <- "latin1"
  # xx <- iconv(x, "latin1", "UTF-8",sub='')
  # facile
}
mySTMlabeltype <- function(model){
  # ref https://github.com/bstewart/stm/blob/master/R/labelTopics.R
  # ref https://github.com/mroberts/stmBrowser/blob/master/R/stmBrowser.R
  logbeta <- model$beta$logbeta
  #make a switch for presence of content covariate
  aspect <- length(logbeta)>1
  labt <- ifelse(!aspect, "prob", # alt: "frex", "lift", "score".
    "topics") # coz content covariates are present and demanded a different structure.
  return(labt)
}
joinOCRqaNum <- function(qoss.meta, dmc2.ordYPq){
  qm.idVec <- 1:nrow(qoss.meta)
  dmc2o.ocrqaNum <- dmc2.ordYPq$ocrqaNum
  jtKey <- intersect(qm.idVec, dmc2o.ocrqaNum); print(length(jtKey)) # 357.
  jtDiff <- setdiff(qm.idVec, dmc2o.ocrqaNum); print(length(jtDiff)); print(jtDiff)
  # print(dmc2.ordYPq[dmc2.ordYPq$ocrqaNum %in% c(71,82,120),]) # See some known missing rows.
  jtdf <- cbind(qoss.meta[jtKey,], dmc2.ordYPq[dmc2o.ocrqaNum %in% jtKey,])
    # Beware: jtKey is not row number coz intersect() returns common elements, not their locations, and any way,
    # row numbers might not match across the two structures being merged! So, dmc2.ordYPq[jtKey,] is not ok to merge with!
  # grep("subject", colnames(responses.orig))
  # stopifnot(jtdf[,2] == jtdf[,4]) # & more...
  print(str(jtdf))
  return(jtdf)
}
myCategorize <- function(xvec, redPreProb=0.25, redLabel="below", otherLabel="above"){
  xvecGrp <- c()
  # TBD include its binary covariate too, (i) cut at *rOtot* 10th or 25th percentile and (ii) later level for red.
  cutAt <- quantile(xvec, prob=redPreProb)
  xvecIsPreProb <- (xvec <= cutAt) # TRUE for red zone coz redPreProb denotes red Pre Probability.
  xvecGrp[xvecIsPreProb] <- paste0(redLabel, redPreProb*100, "tile")
  xvecGrp[! xvecIsPreProb] <- paste0(otherLabel, redPreProb*100, "tile")  
  return(xvecGrp)
}
getJADdata <- function(suffix=paste0("-till", "Y2014P2"), meta="metaVars", fromOCR=TRUE){
  datqoss <- getFiledQOSs(prefix=paste0(cWorkDir, "/jads-"), fromOCR=TRUE, suffix=suffix, ext=".rds")
  colnames(datqoss) <- "QOS" # Note: corresponding "tmp" file column name indicates date and time saved.
  # file.qossTxt <- paste0(cWorkDir, "/tmpQOSjads-", textOptionOCR(fromOCR), suffix, ".txt")
  # write.table(datqoss, file=file.qossTxt)
  fname.meta1 <- paste0(cWorkDir, "/jads-", textOptionOCR(fromOCR), suffix, meta, ".csv")
    # prefix=paste0(cWorkDir, "/jads-"), fromOCR=TRUE, suffix=paste0(suffix, meta), ext=".csv"
  datmeta1 <- read.csv(file=fname.meta1)
  print(str(datmeta1))
  qoss.meta <- cbind(datqoss, datmeta1); print(str(qoss.meta))
  # read.table(file=fname.meta1, col.names=FALSE, stringsAsFactors=FALSE) # stringsAsFactors=TRUE gets factors!

  fname.meta2 <- paste0(cWorkDir, "/../../eduCTeD/out-qaJEEadvanced-Occipital.csv")
  datmeta2 <- read.csv(file=fname.meta2)
  print(str(datmeta2))
  # datmeta2$YP <- YPintvec(as.character(datmeta2$YP)) # for continuous-time modeling data as input to STM later.
  colnames(datmeta2) <- sub(pattern="tmpQOSjads.ocr.tillY2014P2", replacement="ocrqaNum", x=colnames(datmeta2))
    # was: ,"jeeadvqaNum","jeeadvqaType","jeeadvmarkScheme")]) <- c("ocrqaNum","jeeqaNum","jeeqaType","jeemarkScheme")
  # cols.meta2 <- c("subject","paper","year", # drop {qaNum,qaType,markScheme,right,tot}
  #   "uOtot","rOtot",
  #   "ocrqaNum","jeeadvqaNum","jeeadvqaType","jeeadvmarkScheme","YP")
  cols.meta2 <- colnames(datmeta2) # retain {right, tot} too for any subsequent modeling with/without topics!
  levels(datmeta2$subject) <- toupper(substring(levels(datmeta2$subject), first=1, last=1)) # alt: c("C","M","P")
  # datmeta2$subject <- as.character(datmeta2$subject) # coz factor.
  print(str(datmeta2))

  # Subset datmeta2[, cols.meta2] with matching row numbers of qoss.meta *and* in same order, which might differ at start.
  # Then with satisfied assertions, attempt merge.
  dmc2 <- datmeta2[, cols.meta2]
  dmc2 <- dmc2[which(! is.na(dmc2$ocrqaNum)),] # Drop those NAs.
    # Beware: "obsolete" levels might still be around, if datmeta2 has incomplete data eg missing some years.
  # stopifnot(levels(dmc2))
  print(str(dmc2))
  dmc2.ordYPq <- dmc2[order(dmc2$YP, dmc2$ocrqaNum, decreasing=FALSE),]
    # within each YP *and* in (PCM) order matching qoss$subject order, which is how it appeared in question papers?
    # [For factors, this sorts on the internal codes, which is particularly appropriate for ordered factors.]
  print(head(dmc2.ordYPq)); print(tail(dmc2.ordYPq))
  # Drop qoss NAs too? BUT no use holding (eventually few) QOS rows that don't have this meta data?? Or is there?
  qoss.meta12 <- joinOCRqaNum(qoss.meta, dmc2.ordYPq)

  # TBD notrOtot3=(1-rOtot)*10^3 as continuous covariate, considering red in colour scheme of stmBrowser().
  qoss.meta12 <- cbind(qoss.meta12,
    uOtot3=round((10^3)*qoss.meta12$uOtot, 0),
    rOtot3=round((10^3)*qoss.meta12$rOtot, 0),
    notrOtot3=round((1 - qoss.meta12$rOtot)*(10^3), 0),
      # as continuous covariate, considering red in colour scheme of stmBrowser() to visualize topic interactions.
    rOtotGrp=myCategorize(xvec=qoss.meta12$rOtot, redPreProb=0.25),
    idYPQ=paste0(qoss.meta12$YP, "Q", qoss.meta12$jeeadvqaNum))
  return(qoss.meta12)

  # TBD on 2020Feb18:
  # - Remind to update JEEinsight, considering meta-data {qaType, markScheme} inconsistencies.
  # - DONE: Append columns {uOtot3, rOtot3} as as.integer(round((10^3)*?, 0))
  # - DONE: Code the merge/join on (YP, possibly with subject) in *row sequence* where ==60.
  #   Else (eg for Y2012P2) match jeeqaNum with row#.
  # - DONE: Define Y2012P2.get*() that post-processes to *move* ParaForQuestions into Qs where missed; drops exclusive
  #   paras since they are not Qs completely and have no separate ID.
  # - DONE: put*() those 60*6 into datmeta1 .rds. Update meta file too to change 63 rows for Y2012P2 Physics back to 60.
  # - DONE: rename .R files suitably and reflect that into source() stmts.
  # Beware:
  # - QOS tmpQOSjads-ocr-tillY2014P2.txt {69,72,75} missed in outJADinsight. Also, relying on *row* numbers for QOS!
  # - 2012P2 Chemistry qaNum {3/23} missed in outJADinsight; that would have be QOS tmpQOSjads-ocr-tillY2014P2.txt {85-3}.
  #     Its Maths qaNum {20/60} missed in outJADinsight; that would have been QOS tmpQOSjads-ocr-tillY2014P2.txt {123-3}.
}
dropPattern <- function(pattern, txtvec.orig, retries=5, my.ignore.case=TRUE, my.max.distance=2){
  txtvec <- txtvec.orig
  for(i in 1:retries){
    fuzMatch <- aregexec(pattern, txtvec, max.distance=my.max.distance, ignore.case=my.ignore.case)
      # max.distance=0.1 default. fuzzy matching of the pattern.
    duh <- regmatches(txtvec, fuzMatch, invert=FALSE) # BUT 241,248,... also matched relevant QOS parts!
      # [For vector match data, if invert is FALSE, value should be a character vector with length the number of matched
      # elements in m.]
    if(length(unlist(duh)) == 0) break # else continue
    stopifnot(length(unlist(duh)) > 0)
    print(glimpse(unlist(duh)))
    regmatches(txtvec, fuzMatch, invert=FALSE) <- "" # Erase matched part of strings.
  }
  # Now, confirm pattern has been completely dropped:
 fuzMatch <- aregexec(pattern, txtvec, max.distance=my.max.distance, ignore.case=my.ignore.case)
  duh <- regmatches(txtvec, fuzMatch, invert=FALSE)
  stopifnot(length(unlist(duh)) == 0)

  return(txtvec)
}
dropMorePatterns <- function(txtvec.orig, patternVec){
  txtvec <- txtvec.orig
  for(p in patternVec){
    txtvec <- dropPattern(p, txtvec)
  }
  return(txtvec)
}
