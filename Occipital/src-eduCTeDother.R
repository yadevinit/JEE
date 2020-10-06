stop()
### --- earlier code for problematic saving into text ".csv" files.
###
###
{
fname <- paste0(cWorkDir, "/jads-", ifelse(forOCR, "ocr", "nocr"), "-till",
  names(metaYPq)[imetaYPq], # ...[1]=="Y2012p1"
  ".csv")
write.table(NULL, file=fname, append=FALSE, row.names=FALSE, col.names=FALSE) # Beware: append= here.
for(jmcv in 1:till.imetaYPq){
  # stop() MORE Q THAN MCV
  imcv <- metaYPq[[jmcv]]$iMerged
  mcv.sav <- mcv[imcv] # ...[[1]]$iMerged==1:61
  write.table(mcv.sav, file=fname, append=TRUE, row.names=FALSE, col.names=FALSE) # Beware: append= here.
}
mcvQOSs <- read.table(file=fname, header=FALSE, stringsAsFactors=FALSE)
glimpse(mcvQOSs)
fname <- paste0(cWorkDir, "/jads-", ifelse(forOCR, "ocr", "nocr"), "-QOSs.csv")
# txtfname <- paste0(cWorkDir, "/jads-", ifelse(forOCR, "ocr", "nocr"), "-QOSs-wascsv.txt")
# txtjusqoss <- read.table(file=fname, header=FALSE, stringsAsFactors=FALSE)
# glimpse(txtjusqoss); headCharVec(txtjusqoss[,1])
jusqoss <- read.table(file=fname, header=FALSE, quote="\"", stringsAsFactors=FALSE)
headCharVec(jusqoss[,1])
# "\\" > (," within a QOS string in the file seems to have upset and split subsequent content!
# ref https://github.com/Rdatatable/data.table/issues/1109.
# So, considered package readr eg read_file() read_file_raw() write_file() or finer-grained read_lines(),
# but those too don't seem to address it. Binary write read would have worked, but that requires going
# back to the .rds files and processing from there skipping the .csv generated etc. :-(
qoss.readr <- read_lines(file=fname) # read_file(file=fname)
glimpse(qoss.readr) # headCharVec(qoss.readr[,1])
# > ?write.table says:
# [
# qmethod: a character string specifying how to deal with embedded double quote characters when quoting strings.
# Must be one of "escape" (default for write.table), in which case the quote character is escaped in C style by a
# backslash, or "double" (default for write.csv and write.csv2), in which case it is doubled. You can specify just
# the initial letter.
# ]
# Maybe I shouldn't have used write.table() to a .csv file name.
}

stop()
# Earlier thought: correct locQOS[[1]] of above file before continuing; then use locQOS.ok.
locLen <- function(){
  # ref https://stat.ethz.ch/pipermail/r-help/2006-January/085871.html
  # [R] gregexpr() - length of the matched text to a vector
stop()
  lapply(gregexpr("foo", txt), function(x) cbind(x, attr(x, "match.length")))
}
locQOS.ok <- mylocQ.fromCSV(fname=fname)
mylocQ.fromCSV(fname){
  ans <- read.csv(file=fname)
  # [By default there is no column name for a column of row names. If col.names = NA and row.names = TRUE
  # a blank column name is added ... Note that such CSV files can be read in R by
  #   read.csv(file = "<filename>", row.names = 1)].
stop()
  # get rid of appended NAs
  return()
}

### ---
# (3c) some questions or answers/solutions have graphs, math formulae, chemical formulae, etc., in picture form, not
# text. Convert those to text. prior to baselining the Document-Feature Matrix (DFM) or other data for modeling.
# (3d) create a mapping from outJEEAdvanced.csv qaIDs to the qa media (text+picText) being included as data.
#
# [
# First, text can be organized into a document-term matrix that
# quantifies the occurrence of each word (columns) by each
# document (rows). The input to the LDA algorithm is thus a
# document-term matrix that requires specifying the number of
# topics. Second, the model is a Bayesian mixture model.
# Documents are made of a mixture (probability distribution)
# of topics instead of just a single topic. ...
# it "summarizes" the information in the word counts
# down to a reduced number of columns. This leads to the first
# output of the algorithm which is the document-topic matrix. In
# this matrix, each document is scored as a probability across all
# the identified topics. As a third key property, the model used
# can be considered as a hierarchical mixture model as it includes
# a hierarchy of two probability mixtures. At the top of
# the model, the documents compose a mixture of topics, and at
# the bottom, the topics are a mixture of words.
# ]

corp <- VCorpus(VectorSource(jads))
  # creates a volatile corpus, which means it's kept in memory for the duration of the R session
cBounds <- c(3, Inf) # only count words that appear at least N times
jads.tdm <- TermDocumentMatrix(corp, # Runs for few minutes.
  control=list(removePunctuation = TRUE,
    stopwords = TRUE, # alt: TRUE, FALSE.
    tolower = TRUE, # alt: TRUE, FALSE.
    stemming = FALSE, # alt: TRUE, FALSE. Beware: might adversely affect topic modeling.
    removeNumbers = FALSE, # alt: TRUE, FALSE
    bounds = list(global = cBounds)))
str(jads.tdm)
stopifnot(dim(jads.tdm)[1] > 10); inspect(jads.tdm[1:10,]); inspect(jads.tdm[(dim(jads.tdm)[1] -10) : dim(jads.tdm)[1],])
# corp <- tm_map(corp, removePunctuation, ucp = TRUE)

ft <- findFreqTerms(jads.tdm, lowfreq = 100, highfreq = Inf)
  # [1] "correct"  "mark"     "option"   "question"
  # These appear to be from the instructions. Consider dropping these extremely-frequent ones before modeling.
ft.tdm <- as.matrix(jads.tdm[ft,])
ft.tdm.sumSort <- sort(apply(ft.tdm, 1, sum), decreasing=TRUE)
glimpse(ft.tdm.sumSort)


stop()
# topicApp::runApp()
# alt: trinker package textreadr eg as_transcript()
# iFile <- 1; jads.qos <- as_transcript(jads[[iFile]], col.names=c("qNum", "QOS"), person.regex="(Q\\.[1-9][0-9]*)")
# as_transcript("Q.1 test q\r\n Options (A)\r\n (D)\r\n Soln. Answer.", col.names=c("qNum", "QOS"), sep="Q\\.[1-9][0-9]*")
# Consider ensemble approach to comparing classifiers, including Logistic Regression, Naive Bayes Classifier (NBC),
# Random Forest, etc.
# consider automatic keyword extraction (via conditional random fields (CRFs)), essentiality, etc., as key semantics.
# Struct Topic Model superior to LDA coz metamodel co-variant.  Word Embeddings via GloVe (Wikipedia trained) is superior
# for better semantics.  Supported by Azure Notebooks?  SEE RECENT PDFs
# Some prefer Lemmatization (preserve roots of words) over Stemming (truncating), but 2016 research shows no benefit
# as topic modeling already groups root words.
#
### --- relevant literature:
# https://www.aclweb.org/anthology/K17-1010.pdf "Learning What is Essential in Questions" (2017) says
# [These are the only publicly available state-level science exams. http://www.nysedregents.org/Grade4/Science/].
# Considering that, what's being attempted---adversarial-question (not answering but) classification---through
# Project Occipital seems unprecedented and yet to be robustly researched.
# https://www.semanticscholar.org/paper/From-'F'-to-'A'-on-the-N.Y.-Regents-Science-Exams%3A-Clark-Etzioni/f1bef051587001a843c548124bfdd5652877dd7e says:
# [
# AI has achieved remarkable mastery over games such as Chess, Go, and Poker, and even Jeopardy, but the rich variety of
# standardized exams has remained a landmark challenge. Even in 2016, the best AI system achieved merely 59.3% on an 8th
# Grade science exam challenge.
# This paper reports unprecedented success on the Grade 8 New York Regents Science Exam, where for the first time a system # scores more than 90% on the exam's non-diagram, multiple choice (NDMC) questions. In addition, our Aristo system,
# building upon the success of recent language models, exceeded 83% on the corresponding Grade 12 Science Exam NDMC
# questions.
# ]
# Nearest data projects might be:
# http://data.allenai.org/sciq/.
# https://leaderboard.allenai.org/open_book_qa/submissions/about
#
# https://wesslen.github.io/text%20mining/topic-networks/
#
# https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf
# https://epub.wu.ac.at/3558/1/main.pdf
# https://github.com/trinker/topicmodels_learning
# https://github.com/cpsievert/LDAvis


### --- errors from installing packages on R 3.4:
# Package which is only available in source form, and may need compilation of C/C++/Fortran: ‘fastmap’
# Do you want to attempt to install these from sources?
# ...
# Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
#   namespace 'rlang' 0.3.4 is being loaded, but >= 0.4.0 is required
# ERROR: lazy loading failed for package 'lifecycle'
# > install.packages("rlang") # from src 0.4.3
# > install.packages("fastmap") # again from src
# > install.packages(packages) # not from src, but from (older-version) bin
# ERROR: dependency 'ISOcodes' is not available for package 'stopwords'
#   namespace 'pkgconfig' 2.0.1 is being loaded, but >= 2.0.2 is required
# ERROR: lazy loading failed for package 'ggplot2'
#   namespace 'vctrs' 0.1.0 is being loaded, but >= 0.2.1 is required
# ERROR: lazy loading failed for package 'hms'
#   namespace 'htmltools' 0.3.6 is being loaded, but >= 0.4.0 is required
# ERROR: lazy loading failed for package 'shiny'
#   namespace 'pkgconfig' 2.0.1 is already loaded, but >= 2.0.2 is required
# ERROR: lazy loading failed for package 'modelr'
# ERROR: dependencies 'dbplyr', 'modelr' are not available for package 'tidyverse'
# install.packages("ISOcodes") # again from src ... but Depends: R (≥ 3.5.0)

### --- textread package:
# get encoding from filename
filenames <- list.files(cWorkDir, "*\\.pdf$")

head(filenames)
## [1] "IndianTreaty_English_UTF-16LE.txt" 
## [2] "IndianTreaty_English_UTF-8-BOM.txt"
## [3] "UDHR_Arabic_ISO-8859-6.txt"        
## [4] "UDHR_Arabic_UTF-8.txt"             
## [5] "UDHR_Arabic_WINDOWS-1256.txt"      
## [6] "UDHR_Chinese_GB2312.txt"

# Strip the extension
filenames <- gsub(".pdf$", "", filenames)
parts <- strsplit(filenames, "_")
fileencodings <- sapply(parts, "[", 3)

head(fileencodings)
## [1] "UTF-16LE"     "UTF-8-BOM"    "ISO-8859-6"   "UTF-8"       
## [5] "WINDOWS-1256" "GB2312"

# Check whether certain file encodings are not supported
notAvailableIndex <- which(!(fileencodings %in% iconvlist()))
fileencodings[notAvailableIndex]
## [1] "UTF-8-BOM"

