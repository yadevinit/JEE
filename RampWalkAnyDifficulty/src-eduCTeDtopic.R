
# Pending TBD 2020-Feb-14:
# - Included Appendix. Along with the present unigrams, consider bigrams (or trigrams) for raised accuracy, at the cost
#   of efficiency. Consider Dependency Analysis (including KWIC) for raised accuracy, at the cost of efficiency.
#   Using the Threshold plot, choose removal of documents etc. along with suitable count of topics.
#   Consider searchK(), manyTopics(), selectModel(). (lower.thresh= is not % but a count.)
#   Remind to update JEEinsight, considering the newly-corrected meta-data {qaType, markScheme} inconsistencies.
# - Done. Consider rOtot distribution, including knee effect. So, IE: (prevalence ~ YP*subject, content ~ not.rOtotGrp)
# - Done. Can spurious topics be dropped, eg containing address and contact info in footers, booklet instructions,
#   fiitjee, metamodel covars?stm. Safer to drop spurious text *before* preparing corpus() etc for fitting stm().
# - Done. notrOtot3=(1-rOtot)*10^3 as continuous covariate, considering red in colour scheme of stmBrowser().
#   As integer, rOtot can be considered as a (continuous) covariate, more importantly than YP. Marking scheme?
#   Or maybe rOtot can be a "binary effect modification covariate" required by estimateEffect().
#   [Sparse additive generative model (SAGE) ... framework makes it straightforward to add covariate effects into the
#   content portion of the model.]
# - Done. Include its binary covariate too, (i) cut at *rOtot* 10th or 25th percentile and (ii) later level for red.
# - Done: can topic generation be aided by including covariates such as Subject?
# - Done. 40 Topics might be too much for ~360 (QOS) documents, esp since we are varying with subject, YP, and possibly
#   other covariates.
# - Done. If Semantic Coherence Topic Quality data can be got for STM models, eg, by taking
#   a subject at a time, maybe we could regress Topic Prevalence? Yes, via FREX scores.

# https://www.tidytextmining.com/topicmodeling.html says: [
# Latent Dirichlet allocation ...
# Without diving into the math behind the model, we can understand it as being guided by two principles.
# - Every document is a mixture of topics. We imagine that each document may contain words from several topics
# in particular proportions. For example, in a two-topic model we could say "Document 1 is 90% topic A and 10% topic B,
# while Document 2 is 30% topic A and 70% topic B."
# ... per-document-per-topic probabilities, called gamma. (STM documents refer to this as theta, not gamma.)
# - Every topic is a mixture of words. ... per-topic-per-word probabilities called beta. ...
# This is an advantage of topic modeling as opposed to "hard clustering" methods: topics used in natural language could
# have some overlap in terms of words.
# As an alternative, we could consider the terms that had the greatest difference in beta
# between topic 1 and topic 2. This can be estimated based on the log ratio of the two: log2 (beta2 / beta1)
# (a log ratio is useful because it makes the difference symmetrical: beta2
# being twice as large leads to a log ratio of 1, while beta1 being twice as large results in -1).
# ]
# https://scholar.harvard.edu/files/dtingley/files/topicmodelsopenendedexperiments.pdf says regarding STM: [
# Rather than assume that topical prevalence (i.e., the frequency
# with which a topic is discussed) and topical content (i.e.,
# the words used to discuss a topic) are constant across all
# participants, the analyst can incorporate covariates over
# which we might expect to see variance. ...
# Thus, there are three critical differences in the STM as
# compared to the LDA model described above: (1) topics
# can be correlated; (2) each document has its own prior
# distribution over topics, defined by covariate X rather
# than sharing a global mean; and (3) word use within a
# topic can vary by covariate U. These additional covariates
# provide a way of “structuring” the prior distributions in
# the topic model, injecting valuable information into the
# inference procedure. ...
# Our model extends this framework by allowing topical
# prevalence to vary with any user-specified covariate. We
# also extend the framework to topical content. Word use
# within a particular topic comes from a regression, in this
# case a multinomial logistic regression, where the treatment condition and other covariates can change the rate
# of use for individual words within a topic. ...
# We can also use the model to summarize the semantic meaning of a topic. Generally, these summaries
# are the highest probability words within a topic ...
# we label topics using simplified frequency-exclusivity (FREX) scoring ... This summarizes words with
# the harmonic mean of the probability of appearance under a topic and the exclusivity to that topic. ...
# Choices in Model Specification. ... The analyst will want to include a covariate in the
# topical prevalence portion of the model (X) when she
# believes that the observed covariate will affect how much
# the respondent is to discuss a particular topic. The analyst
# also has the option to include a covariate in the topical
# content portion of the model (U) when she believes that
# the observed covariate will affect the words which a respondent uses to discuss a particular topic. These two sets
# of covariates can overlap ...
# The STM includes shrinkage
# priors or regularization, which draws the covariate effects
# toward zero. An analyst concerned about overfitting to
# the covariates can increase the degree of regularization.
# The analyst must also choose the number of topics. There is no "right" answer to this choice. Varying
# the number of topics varies the level of granularity of the view into the data ...
# "prevalence" can be expressed as a formula that can include multiple covariates and
# factorial or continuous covariates. For example, by using the formula setup we can enter
# other covariates additively. Additionally users can include more flexible functional forms of
# continuous covariates, including standard transforms like log(), as well as ns() or bs() from
# the splines package. The stm package also includes a convenience function s(), which selects
# a fairly flexible b-spline basis. ...
# ]
#

### --- Rationale:
# For Project Occipital, here's a rationale for covariate choices during STM model specification:
# - Given a set of QOSs, a count k of (latent) topics to generate, and nothing else, a topic modeler could use FREX
#   and other scores (on DTMs) to map QOSs to k topics.
# - From exam domain, we know that there are as many QOSs in one subject as there are in each of the other two.
# - So, Subject could be included as a covariate for Topic Prevalence when specifying the model.
# - From exam domain, we also know that Topic Prevalence can vary for each Paper. So, Year.Paper might not be
#   a good covariate for Topic Prevalence, esp considering that across a pooled set of Papers, Subject might suffice
#   as a stabler covariate for Prevalence. But, Year.Paper could "structure" a further-limiting of Topic Prevalence,
#   since Subject within each Year.Paper limits Prevalence, not just as a "pooled" corpus across the universe of
#   Year.Paper.
# - From subject domain, we know some word tokens are likelier to occur (in QOSs) for one subject than for another eg
#   "refraction" is likelier for Physics and "phenol" for Chemistry while "vector" could appear for Maths as well as
#   Physics. Since we know each QOS's Subject, Subject could be included as a covariate when specifying the model.
#   But that's back to Topic Prevalence (occurring) and Subject is already considered.
# - As a researcher for Project Occipital, what matters is (a model) to explain Difficulty faced in answering each QOS.
#   (Complement of Difficulty=) Ease faced by a population of examinees and measured as rOtot3 is already known for
#   each QOS. We don't have that model today, but we want to influence the (machine's) choice of (word tokens relating)
#   Topics. So, rOtot3 could be included as a covariate for influence Topic Content.
# - What about other available variables: {"uOtot3","jeeadvqaType","jeeadvmarkScheme"}? uOtot3 could influence Topic
#   Content, just as rOtot3. From research domain, we know {jeeadvqaType,jeeadvmarkScheme} interact with other
#   variables to impact (Difficulty or) Ease of each QOS eg impact of -ve marking scheme and Numerical-Answer type
#   features has been researched earlier.
# - To summarize covariate choices for model specification: {Subject} for Topic Prevalence;
#   {rOtot3,uOtot3,jeeadvqaType,jeeadvmarkScheme} for Topic Content. Whether these need to be transformed can be
#   explored via estimateEffect() after including them into stm() model specification.
# - And the researcher for Project Occipital might like to know:
#   (a) how well the Topic Model explains (Difficulty or) Ease---or how much can the Model be depended on---and
#   (b) to "Ramp Walk" any Difficulty, what's an "understanding" of the generated Topics.

# In response:
  # TBD 2020Feb25: subject P,C,M effect topics 7,3,6 correspondingly. rOtotGrp effect on Topic 7.
  # notrOtot3 effect on topics prevalence: topic 7 (prevalence) reduces with increasing notrOtot3; still ends higher.
  # BUT that "linear effect" disappears with s(), which shows topic 3C (median?) going up at extreme notrOtot3.
  # and 4 seems to reduce.

# (a1) checkResiduals() did not indicate latent topics chosen being too few to account for any overdispersion.
# checkResiduals(stmFit ...) indicates 7 topics (with rOtotGrp covariate) is too few!
# (a2) checkBeta() did not indicate "overloaded" topics or "erroneous" words that exclusively overload a topic.
# (a3) As per 2020Feb19 Model summary(estimateEffect()) of (Topic-Prevalence) regressions ~subject+s(rOtot3):
# Topic #1: 0.18P. #2: 0.08M. #3 insignificant. #4: -0.27P-0.12M. #5: 0.19M. #6: -0.11M-0.11P. #7 0.14P.
# Here, coefficients are relative to baseline Subject C(hemistry), while holding other variables constant at their
# baseline (median?) values. And s(rOtot3) seems to have been categorized into 10 categories.
# These coefficients are visualized by plot(estimateEffect()) eg that Topic #1,7 P; #4,6 C; #5,2 M; #3 common.
#
# (b1) stmBrowser() via Chrome: can it be hosted via notebooks.azure.com or else github?
# (b2) Topics' FREX word tokens?
# Further, plot(thoughts) shows most-associated (thought) documents (QOSs), but that seems imprecise for improvement
# action.
# Now labelTopics(stmFit) says:
# [Topic 2: maths, hindi, b, g, resonance, pvt, raj 
#  Topic 3: know, city, cg, resonsence, ipia, jnalawar, mall].
# This corroborates [#3 insignificant] from (Topic-Prevalence) regressions. But Topic #2 too seems irrelevant.
# So, drop Topic #2,3? Or first clean irrelevant word tokens before Topic Modeling?
# (b3) Consider rOtot3 10th percentile eg 85 as a cut point to categorize into a binary variable. Coz behaviour seems
# gradual on both sides of such a cut point. Seems simpler to model this than a continuous variable.
# (b4) Correct all PfQs (dropping repetitive text till each "SECTION" prior) since it impacts readability via
# stmBrowser().

date()
cFooter.Resonance <- c("Resonance Eduventures Pvt\\. Ltd\\.
CORPORATE OFFICE : CG Tower, A-46 & 52, IPIA, Near City Mall, Jnalawar Road, Kota \\(Raj.\\) - 324005
Tel\\. No\\. : 0744-3192222, 3012222, 3022222 \\| Toll Free : 1800 200 2244 \\| To Know more : sms RESO at 96677
Website : www\\.resonance\\.ac\\.in \\| Email : contact@resonance\\.ac\\.in
This solution was download from Resonance JEE ADVANCED 2014 Solution portal Page \\| "
  # alt: "Resonance Eduventures .* Solution portal Page |" # within the same string
  # alt: "Resonance Eduventures .* contact@resonance\\.ac\\.in" # within the same string
)
cFooter.FIITJEE <- c("FIITJEE Ltd\\., FIITJEE House, 29-A, Kalu Sarai, Sarvapriya Vihar, New Delhi -110016, Ph 46106000, 26569493, Fax 26513942
website: www\\.fiitjee\\.com\\."
)
cExtraStop.old <- c("jee","advanced","ans","solution","sol","page","choices","multiple","section", # Beware: lower case!
  "kota","road","office","ltd","website","www.resonance.ac.in","www","fax","city",
  "contact@resonance.ac.in","email","eduventures","corporate","download","portal","sms",
  "fiitjee","fiitjee.com","kalu","sarai","sarvapriya","delhi"
)
  # code-7, 22-05-2016, 2016, advanced-2016 ... worth removing? Also drop numbers?
  # "facebook.com","youtube.com"
  # "resonence","resonsence","besonence","resonsnce","resonance","correct", and single-letter tokens, possibly
  # inside a QOS, might be worthy retaining.
  # u   f0b5   f0ad   f0bc   f0ae ... b c d
  # email IDs, addresses ...
  # Above tokens might have to be removed.

# library(LDAvis)
# library(servr)
par(ask=TRUE)
print(cYPtill) # ensure till latest, as per src/src-eduCTeDupdateQ.R.
fname.meta.relPath <- paste0(cWorkDir, "/out-qaJEEadvanced-Occipital.csv")
  # Was: paste0(cWorkDir, "/../../eduCTeD/out-qaJEEadvanced-Occipital.csv")
print(fname.meta.relPath)

responses <- responses.orig <- getJADdata(fname.meta2=fname.meta.relPath)
str(responses); summary(responses)
  # Done: Later, merge with outJADinsight.csv too; then use rOtot possibly as binary factor as Topical-Content covariate.
# Fixed:
# idYPQ: Factor w/ 605 levels!! YP:: Y2012P2: 58!! mark-201235: 2!! Seem suspect! Was jeeadvqaNum correctly suffixed as Q?

responses.covars <- c("subject", "YP", "jeeadvqaType", "jeeadvmarkScheme", "uOtot3", "rOtot3", "notrOtot3",
  "rOtotGrp", "idYPSQ", "right", "tot") # Was: "idYPQ". 2020Mar15: included c("right", "tot").
  # column names of covariates or docvars.
# duh <- dropPattern(cFooter.FIITJEE, txtvec.noR)
# duh2 <- dropPattern(cFooter.FIITJEE, duh)
# duh7 <- dropMorePatterns(responses[,1], patternVec=c(cFooter.Resonance, cFooter.FIITJEE))
# responses[,1] <- duh
responses[,1] <- dropMorePatterns(responses[,1], patternVec=c(cFooter.Resonance, cFooter.FIITJEE))
responses <- responses[which(! is.na(responses$rOtot3)), c("QOS", responses.covars)]; summary(responses)

library(quanteda)
library(dplyr)
text <- responses[,1]
readability <- textstat_readability(text) # measure="Flesch" by default.
QOSnchar <- nchar(text); QOSntoken <- ntoken(text); QOSflesch <- readability$Flesch
# QOSentropy <- quanteda::textstat_entropy(x=text, margin="documents", base=2) # Alt: "features".
qaTypeGrp <- responses$jeeadvqaType
levels(qaTypeGrp) <- unlist(lapply(levels(responses$jeeadvqaType), FUN=doGroup.qaType))

responses <- cbind(responses, qaTypeGrp=qaTypeGrp, QOSnchar=QOSnchar, QOSntoken=QOSntoken, QOSflesch=QOSflesch,
  YPint=YPintvec(as.character(responses$YP))) # for continuous-time modeling data as input to STM later.
  # , QOSentropy=QOSentropy)
responses <- includeUIKinflect(responses)
summary(responses)
responses.covars <- c(responses.covars, tail(colnames(responses), 1+3+2))
# Include nchar(), ntoken(), readability$Flesch, and textstat_entropy() into covariates.

### --- Topic Modeling (CTM)
# ref https://rawgit.com/wesslen/Topic-Modeling-Workshop-with-R/master/part2-ctm.html
# https://raw.githubusercontent.com/wesslen/text-analysis-org-science/master/01-datacleaning-exploration.Rmd

if(! runSTMeffectsOnly){
hist(readability$Flesch, xlab = "Flesch-Kincaid Score", main = "Histogram of Flesch-Kincaid Scores")
  # OCR: mostly close to 80; else for noOCR, it seemed uniformly distributed across xlim ie low-readability
  # responses existed.
# hist(readability$Flesch, # was: $Flesch.Kincaid.
#      xlim = c(0,40), # 40 here might not be based on chosen count of topics cKtopics.
#      breaks = 200, xlab = "Flesch-Kincaid Score", main = "Histogram of Flesch-Kincaid Scores")
hist(QOSntoken,
  # OCR: less-spread (word count of) responses.
  # noOCR: > 95th percentile (2000) for file 2016p1 responses. < 5th percentile (~10) for 2013p1.
     breaks = 20,  
     main = "# of Words/Tokens per Response", 
     xlab = "Number of Words/Tokens")
hist(QOSnchar, 
     breaks = 20,  
     main = "# of Characters per Response", 
     xlab = "Number of Characters")
minWords <- summary(QOSntoken)[1] # min.
minWords <- max(minWords, 30); print(responses[which(QOSntoken < minWords), 1]) # Check for any "outliers"?
maxWords <- summary(QOSntoken)[6] # max.
print(responses[which(QOSntoken >= maxWords), 1]) # Check for any "outliers"?


### Covariates: Explore & Extraction
if(length(responses.covars) >= 1){ # once covariates are included
  responses %>% 
    group_by(subject) %>% # was: (Q8, Q9). But group_by(responses.covars) did not work coz maybe colnames required.
      # Even group_by(responses.covars[1]) does not work, even if it's a factor and not char. Maybe coz quoted.
    summarise(Count = n())
} # else continue
} # else continue. if(! runSTMeffectsOnly){

responses.covars.docLevel <- setdiff(responses.covars, c("QOS")) # Alt: c("YP") considering it's corpus level.
my.meta.corpusLevel <- list(source=
  "data for 6*2 JEE-Advanced papers: jads-ocr-tillY2018P2.rds 2020Mar12; out-qaJEEadvanced-Occipital.csv 2020Mar12.")
    # Auto include filename later.
myCorpus <- corpusCustom(text=responses[, "QOS"],
  meta.docnames=responses[, "idYPSQ"], # vector of char (names) for docs (QOSs).
    # Alt: responses$ocrqaNum easing tallying.
  meta.docLevel=responses[, responses.covars.docLevel],
  meta.corpusLevel=my.meta.corpusLevel)
  # Was: myCorpus <- quanteda::corpus(text) # from char vector coz corpus() function prototype/interface/sign changes.

### Tokenize, Stemming, Uni/Bi/Tri-grams, Stop Words (dfm)
if(! runSTMeffectsOnly){
dfm1 <- dfm(myCorpus, 
           remove = c(stopwords("english")), 
           ngrams=1L, # alt: unigrams, bigrams
           stem = F, 
           remove_numbers = T, 
           remove_punct = T, 
           remove_symbols = T, 
           remove_hyphens = F)
dfm.orig <- dfm # just as a backup.
topfeatures(dfm1,25)

if(FALSE){
  # extra.stop <- cExtraStop.old # Let's explore removing sparse terms.
  dfm2 <- dfm(myCorpus, 
           remove = c(stopwords("english"), cExtraStop.old), 
           ngrams = 1L, 
           stem = F, 
           remove_numbers = T, 
           remove_punct = T, 
           remove_symbols = T, 
           remove_hyphens = F)
} # else continue

# stop() # Is following dfm_trim() inconsistent with Project Occipital's purpose of modeling question difficulty?
# dfm21 <- dfm_trim(dfm2, min_docfreq = 2) # docfreq_type = "prop"
  # alt: [
  # mydfm.un.trim <-
  #   dfm_trim(
  #     mydfm,
  #     min_docfreq = 0.075,
  #     # min 7.5%
  #     max_docfreq = 0.90,
  #     # max 90%
  #     docfreq_type = "prop"
  #   )
  # ]
topfeatures(dfm21,25)

# Let's plot two word clouds -- one as a whole and the second by a covariate. 
library(RColorBrewer)

textplot_wordcloud(
  dfm21,
  scale = c(3.5, .75),
  colors = brewer.pal(8, "Dark2"),
  random.order = F,
  rot.per = 0.1,
  max.words = 100
)

# now without those extra.stop words and in comparison with a covariate
cdfm1 <- dfm(
  myCorpus,
  groups = responses.covars[1], # alt: "subject", (but not continuous covar) "YP". was: "Country",
  remove = c(stopwords("english"), cExtraStop.old),
  stem = F,
  remove_numbers = T,
  remove_punct = T,
  remove_symbols = T,
  remove_hyphens = F
)

textplot_wordcloud(
  cdfm1,
  comparison = T,
  scale = c(3.5, .75),
  colors = brewer.pal(8, "Dark2"),
  random.order = F,
  rot.per = 0.1,
  max.words = 100
)

# [Yet a problem with exploratory words clouds is that they do not measure the difference -- especially with statistical
# inference. Let's keep this in mind for when we run topic modeling.]
textplot_wordcloud(
  tfidf(dfm21),
  scale = c(3.5, .75),
  colors = brewer.pal(8, "Dark2"),
  random.order = F,
  rot.per = 0.1,
  max.words = 100
  )

# We can use word clustering to identify words that co-occur together.
wordDfm <- dfm_sort(dfm_weight(dfm21, "frequency"))
  # [Warning message:
  # scheme = "frequency" is deprecated; use dfm_weight(x, scheme = "count") instead]
wordDfm <- t(wordDfm)[1:50,] # because transposed
wordDistMat <- dist(wordDfm)
wordCluster <- hclust(wordDistMat)
plot(wordCluster, xlab="", main="Raw Frequency weighting")
# [Think of this plot as a crude way of identifying topics. Also, this plot (called a dendrogram) can help us identify
# "meaningless" words (e.g. also, like, really) that we could eliminate as stop words. ...
# Another interesting take on this plot is not use the raw frequencies (word counts) but instead use the [TF-IDF]
# weightings, which re-weights the words focusing less on words that are 
# either rarely used or used too frequently. 
# We can rerun the plot with TF-IDF by changing the "frequency" to "tfidf" in the second line of the code.]
wordDfm <- dfm_sort(dfm_weight(dfm21, "tfidf")) # was: "tfidf"
  # Warning message: scheme = "tfidf" is deprecated; use dfm_tfidf(x) instead.
  # dfm_tfidf(wordDfm, scheme_tf = "logcount")
wordDfm <- t(wordDfm)[1:50,]  # because transposed
wordDistMat <- dist(wordDfm)
wordCluster <- hclust(wordDistMat)
plot(wordCluster, xlab="", main="TF-IDF weighting")
} # else continue after if(! runSTMeffectsOnly){

# https://rawgit.com/wesslen/Topic-Modeling-Workshop-with-R/master/part2-ctm.html
# Correlated Topic Model (CTM) with stm package
library(stm)
# ? tpout <- textProcessorCustom(arg.documents=responses$QOS, arg.metadata=responses, arg.striphtml=FALSE)
# } # else continue after if(! runSTMeffectsOnly){
# QOSentropy <- quanteda::textstat_entropy(x=dfm3, margin="documents", base=2) # Alt: "features".
# responses <- cbind(responses, QOSentropy=QOSentropy); summary(responses)
# responses.covars <- c(responses.covars, tail(colnames(responses), 1))
  # Include {nchar(), ntoken(), readability$Flesch, and} textstat_entropy() into covariates.
# responses.covars.docLevel <- setdiff(responses.covars, c("QOS")) # Alt: c("YP") considering it's corpus level.
# myCorpus1 <- ...
# dfm31 <- quanteda::dfm(x=myCorpus1, ...
dfm3 <- quanteda::dfm(x=myCorpus, remove=c(stopwords("english"), cExtraStop.old),
  ngrams=1, # Alt: 1:2 but 10,000+ tokens!! and fails to alloc 700+MB, # unigrams and bigrams; no skipgrams.
  tolower=TRUE, stem=FALSE
  # select=NULL, remove=NULL, dictionary=NULL, thesaurus=NULL, valuetype=c("glob", "regex", "fixed"),
  # case_insensitive=TRUE, groups=NULL, verbose=quanteda_options("verbose"),
  # ... # [additional arguments passed to tokens; not used when x is a dfm] eg remove_punct=TRUE.
)
stmdfm3 <- quanteda::convert(dfm3, to="stm", docvars=docvars(myCorpus))
  # docvars=NULL by default. omit_empty=TRUE by default.
  # Alt: [to = c("lda", "tm", "stm", "austin", "topicmodels", "lsa", "matrix", "data.frame", "tripletlist")].
  # Later: stmdfm3$documents, stmdfm3$vocab, stmdfm3$meta.
pdout <- prepDocumentsCustom(arg.documents=stmdfm3$documents, arg.vocab=stmdfm3$vocab,
  arg.meta=stmdfm3$meta, arg.lower.thresh=1)
  # Was: out <- prepDocumentsCustom(documents=stmdfm$documents, vocab=stmdfm$vocab, meta=stmdfm$meta)
  # 2020Mar24:
  # > Removing 63554 of 84887 terms (63554 of 256855 tokens) due to frequency 
  # > Your corpus now has 677 documents, 21333 terms and 193301 tokens.[1] 63554
shortdoc <- headCharVec(text, n=250) # was: n=200 for this shortened form for fitting display frame.
mustGenSTMCTMmodels <- TRUE # was: FALSE

file.CTM <- paste0(cWorkDir, "/CTMfit-", textOptionOCR(forOCR), "-q", nrow(responses),
    # Was till 2020Mar31: "t", cKtopics, "covar",
  tStamp(), ".RData")
if(mustGenSTMCTMmodels && (! runSTMeffectsOnly)){
  # You can run the model (which will take several minutes). This time, let's consider running a cKtopics model.
  ctmFit <- stm(pdout$documents, pdout$vocab, K=cKtopics,
    max.em.its=cMaxEMits, data=pdout$meta, init.type="Spectral", seed=300) # was: max.em.its=150 for k=40 topics.
  save(ctmFit, file=file.CTM)
} # else continue after if(! runSTMeffectsOnly){

# http://www.cs.cornell.edu/~xanda/winlp2017.pdf recommends:
# [
# 1. Document duplication can alter model inference, but requires substantial quantities of repetition and can be
# sequestered into individual topics.
# 2. Aside from extremely frequent stopwords, removal of stopwords does little to impact the
# inference of topics on non-stopwords.
# 3. Topic model inference often places words
# sharing morphological roots in the same topics, making morphological conflation such as
# stemming redundant and potentially damaging to the resulting model.
# ...  As a result, the methodology of
# post-processing a corpus instead of pre-processing
# can allow practitioners the option to test out preprocessing options on one trained model to decide
# on a treatment best suited for their application.
# The results also suggest that when possible, it is good for practitioners to pre-process more lightly
# to avoid discarding useful word information.
# ]
cKtopics <- 0
  # K=0 [The core idea of the spectral initialization is to approximately find the vertices of the convex hull of
  # the word co-occurrences.]
  # Was: 7. 2 unacceptable to stmFit; ctmFit too warns.
  # 65 Topics as per JEE-Main 2020 Syllabus. Was: 40 in CTM ref likely coz visualizability on computer display.
  # 2 might not be a bad place to start; one can see the topic model evolve, viewing differences in topic-word beta etc.
  # 2*subjects=2*3=6 topics might be the minimum acceptable, as that allows (Difficulty or) Ease to be categorized per
  # Subject into {hard,notHard}. Next option would be Difficulty to be categorized into {hard,medium,easy}; so
  # cKtopics=3*3=9 topics.
  # To allow for a topic that doesn't covary with subject, cKtopics=6+1=7 topics might be another option; 7 is also
  # believed to limit human comprehension :-).
  # To choose cKtopics, consider searchK() or [When initialization type is set to "Spectral" the user can specify K = 0].
cMaxEMits <- 30 # Was: 200. cKtopics*4 might not suffice for convergence.
  # Beware: stmCustom() ahead ran for over 8 hours to converge in about 25 iterations!
  # Beware: K=0 took over 9.5GB RAM on Google Colab, where max freely given is 12GB, crashed there; then took over 12.5GB!

file.STM <- sub(pattern="CTM", replacement="STM", x=file.CTM)
# stop()
# save(responses.orig, responses, text, shortdoc, pdout, # stmFit, prep,
#  file=sub(pattern="STMfit", replacement="rortsp", x=file.STM))
cEMtol <- 1e-05 # 100*1e-05 # Earlier, stm() converged for 10*10e-05.
  # [emtol= Defaults to .00001. You can set it to 0 to have the algorithm run max.em.its number of steps]
warning(paste0("retry finer tolerance than:", cEMtol))

  # STM takes the rest of this CTM source code to another level.
  date() # What can also be varied for superior results: {unigrams, bigrams, trigrams} with 5k limit on vocab/terms.
  # f.prevalence <- as.formula(paste0("~", "subject + rOtot3")) # alt: as.formula(paste0("~", "subject + s(rOtot3)"))
  # f.content <- as.formula(paste0("~", "rOtot3"))
  # plot(rOtotGrp80 ~ subject*YP*qaTypeGrp*jeeadvqaType, data=responses)
  f.prevalence1 <- as.formula(paste0("~ ", "subject + rOtotGrp80 + s(YPint) + qaTypeGrp"))
  f.prevalence2 <- as.formula(paste0("~ ", "subject + s(rOtot3) + s(YPint) + qaTypeGrp"))
# Include https://wesslen.github.io/assets/documents/papers/topic-models-beyond.pdf into .md:
# [
# Topic model inference is a NP-hard problem [61], [62]. This hardness
# leads to the problem of multi-modality, i.e., an optimization
# problem (like maximum likelihood) can be solved locally but
# cannot, with certainty, be solved globally.11 Multi-modality
# is important when an algorithm’s output can change with
# initialization parameters. In other words, different starting
# parameters can alter results and threatened the legitimacy of
# the approach and results!12 While this implication cannot be
# directly solved (as its a product of the algorithms hardness),
# they argue the solution for researchers is to use improved
# initialization (spectral) methods and posterior predictive (stability)
# checks [63] to achieve the best of all possible local
# optima. Further, Chuang et al. (2015) provide an example of
# how a visualization interface can help researchers understand
# the effect of multi-modality on the stability of their results.
# ]
f.prevalence3 <- as.formula(paste0("~ ", "subject"))
    # YPint <- YPintvec(as.character(responses$YP)) # for continuous-time modeling data as input to STM later?
    # try "+ rOtot3" to tackle overdispersion?
    # In order of author's decreasing ordering/organizing emphasis for (Topic) Prevalence.
    # Was till 2020Mar28: "subject + YP + jeeadvqaType"
    # alt: "rOtotGrp80 : subject"
  f.content1 <- as.formula(paste0("~ ", "rOtot3")) # Alt: "rOtot3" though without plot("perspectives"). "rOtotGrp80"
  f.content2 <- as.formula(paste0("~ ", "rOtotGrp80")) # Alt: "rOtot3" though without plot("perspectives"). "rOtotGrp80"
  f.prevalence <- f.prevalence1; f.content <- f.content2
  # f.prevalence <- f.prevalence2; f.content <- f.content2
  # f.prevalence <- f.prevalence3; f.content <- f.content2

  stmFit <- stmCustom(pdout, arg.prevalence=f.prevalence, arg.content=f.content,
    arg.K=cKtopics, arg.max.em.its=cMaxEMits, arg.emtol=cEMtol, # Increase to restart incomplete modeling from arg.model.
    arg.model=NULL, # Incomplete model to restart stm() with.
    mustSave=TRUE)
  # fstmrds <- paste0(cWorkDir, "/", tStamp(), ".rds"); print(fstmrds); saveRDS(stmFit, file=fstmrds)
  # Consider saving the workspace right here, like a checkpoint, to load and continue as needed. load("wkspa.rda").
  # Alt: stm(... K=0, "Spectral") or searchK(... K=c(k1,k2), "Spectral")
  # sub("STMfit.*RData$", "STMfit-q677t7covar20200330T064140.RData", file.STM)
# } else {
#   # The code simply loads the file.
#   load(file=file.STM) # since that's superior.
# }

  # [When not using the spectral initialization, the analyst should estimate many models, each
  # from different initializations, and then evaluate each model according to some separate standard
  # (we provide several below). The function selectModel automates this process to facilitate
  # finding a model with desirable properties.]
  # selectModel(runs=20); plotModels(stm.selmodel) to see [semantic coherence and exclusivity for each model and topic].
  #   init.type=stmFit,
  # [Latent Dirichlet Allocation (LDA), Dirichlet Multinomial Regression Topic Model (DMR), a random initialization
  # or a previous STM object.]
  #   to.disk=TRUE,
  # [... at the current directory in a separate RData file. This is most useful if one needs to run multiSTM() ...]
  # runs=20, # =50 default. [Total number of STM runs used in the cast net stage. Approximately 15 percent
  # of these runs will be used for running a STM until convergence.]
  # [net.max.em.its=2 default=Maximum EM iterations used when casting the net].
  # )
  # plotModels(smcFit); smcFit.sparsity <- smcFit$sparsity
  # [Percent sparsity for the covariate and interaction kappas for models with a content covariate.]
  # smcFit.semcoh <- smcFit$semcoh; smcFit.runout <- smcFit$runout; smcFit.runout.1 <- smcFit.runout[[1]]

  # [searchK uses a data-driven approach to selecting the number of topics. The
  # function will perform several automated tests to help choose the number of topics including
  # calculating the held-out log-likelihood (Wallach, Murray, Salakhutdinov, and Mimno 2009)
  # and performing a residual analysis (Taddy 2012).]
  # [Due to the need to calculate the heldout-likelihood N documents have proportion of the documents
  # heldout at random. This means that even with the default spectral initialization the results can
  # change from run to run. When the number of heldout documents is low or documents are very
  # short, this also means that the results can be quite unstable.]

stop()
# stmFit.pre1con2 <- stmFit
# stmFit.pre2con2 <- readRDS(paste0(cWorkDir, "/20200401T131137.rds"))
# plotModels(list(stmFit.pre1con2, stmFit.pre2con2)) # Try but fails coz not output of selectModel().
# plot(stmFit.pre1con2)
# plot(stmFit.pre2con2)
tmFit <- stmFit # or ctmFit, as needed.

# Exploring the results through stm visualizations.
if(! runSTMeffectsOnly){
plot(ctmFit, # Beware: not stmFit.
         type = "summary", 
         # was: xlim = c(0,.16), 
         n = 5, 
         labeltype = "prob", # Error with stmFit: Cannot specify label type for content covariate models.
         main = "Topics", 
         text.cex = 0.8)
} # else continue after if(! runSTMeffectsOnly){

# Drop topic 2 (coz contact details)?
# Can we see almost identical topics; if so, this is a good sign? Are our topics "stable" across runs, even in this
# case after we added in prevalent covariates?
topicNames.STM <- labelTopics(stmFit, n=7)
  # [n=7 by default ... desired number of words (per type) used to label each topic.
  # frexweight=0.5 by default ... A weight used in our approximate FREX scoring algorithm.]
topicNumProp.STM <- data.frame(TopicNumber=1:cKtopics, TopicProportions=colMeans(stmFit$theta)) # was: topic <-
str(topicNames.STM); print(topicNames.STM)
str(topicNumProp.STM); print(topicNumProp.STM)
topics.cf <- c(1:cKtopics) # i <- c(1:3) # c(9, 18)

par(ask=TRUE)
plot(stmFit, type="summary") # Top topics (prevalence).
thoughts <- thoughtQuotesForTopicVec(stmFit, topics=topics.cf, shortdoc, nMostAssociatedDocs=5)
# thoughtQuotesForTopicPair(stmFit, topics=topics.cf, shortdoc)
  # Beware: topic differs for CTM and STM (with covar).

# Exploring the effects of the (Topical-Prevalence) covariates:
# Next, we want to explore the effect of the covariates on the topic proportions (likelihood of the topic).
# First, we'll need to use the estimateEffect function to estimate this effect.
f.prevalence.estEffect <- update.formula(f.prevalence, 1:cKtopics ~ .)
prep <- estimateEffect(f.prevalence.estEffect,
  stmFit, meta=pdout$meta, uncertainty="Global")
  # Was: 1:cKtopics ~ subject * rOtotGrp, # was: subject + rOtot3. subject + rOtotGrp,
  # 1:cKtopics ~ subject + jeeadvqaType: not ok [Covariate matrix is singular.]
  # 1:cKtopics ~ subject + rOtotGrp + jeeadvqaType: not ok [Covariate matrix is singular.]
  # 1:cKtopics ~ subject + rOtotGrp + jeeadvqaType + jeeadvmarkScheme: not ok [Covariate matrix is singular.]
  # was: 1:cKtopics ~ subject + rOtotGrp
  # was: estimateEffect(1:cKtopics ~ subject + s(rOtot3), stmFit, meta=out$meta, uncertainty="Global")
  # [outcome is the proportion of each document about a topic in an STM model].
  # Seems explained by https://www.jstatsoft.org/article/view/v008i15/effect-displays-revised.pdf.
  # formula for topical prevalence only.
  # If penalty, within the range of the data TBD.
  # If a continuous var "goes" outside [0,1] within the range of the data, consider a spline with greater degrees
  # of freedom.
  # alt: ~ subject + s(rOtot3) but earlier "Error in (1 - h) * qs[i] : non-numeric argument to binary operator"
  # maybe coz s(YP) was factor on string.
  # SAGE topics are enabled automatically when using a covariate in the content model.
  # [metadata=A dataframe where all predictor variables in the formula can be found. If NULL R will look for the
  # variables in the global namespace. It will not look for them in the STM object ...
  # This function performs a regression where topic-proportions are the outcome variable. ... The formula specifies the
  # nature of the linear model. On the left hand-side we use a vector of integers to indicate the topics to be included
  # as outcome variables. If left blank then the default of all topics is used.]
  # [
  # gamma.prior = "L1" which uses the glmnet package (Friedman et al.
  # 2010) to allow for grouped penalties between the L1 and L2 norm. In these settings we
  # estimate a regularization path and then select the optimal shrinkage parameter using a user-tunable
  # information criterion. By default selecting the L1 option will apply the L1 penalty
  # by selecting the optimal shrinkage parameter using AIC. The defaults have been specifically
  # tuned for the STM but almost all the relevant arguments can be changed through the control
  # argument. Changing the gamma.enet parameter by specifying control = list(gamma.enet
  # = 0.5) allows the user to choose a mix between the L1 and L2 norms. When set to 1 (as by
  # default) this is the lasso penalty, when set to 0 it is the ridge penalty.
  # ]
  # [
  # The function will automatically check whether the covariate matrix is singular which generally results from linearly
  # dependent columns. Some common causes include a factor variable with an unobserved level, a spline with degrees of
  # freedom that are too high, or a spline with a continuous variable where a gap in the support of the variable results
  # in several empty basis functions. In these cases the function will still estimate by adding a small ridge penalty to
  # the likelihood. However, we emphasize that while this will produce an estimate it is only identified by the penalty.
  # In many cases this will be an indication that the user should specify a different model.
  # The function can handle factors and numeric variables. Dates should be converted to numeric variables before
  # analysis. ... If a continuous variable goes above 0 or 1 within the range of the data it may indicate that a more   
  # flexible non-linear specification is needed (such as using a spline or a spline with greater degrees of freedom).
  # ]
  # [
  # At this point you can only have a single variable as a content covariate, although that variable can have
  # any number of groups. It cannot be continuous. Note that the computational cost of this type of model rises
  # quickly with the number of groups and so it may be advisable to keep it small. ...
  # However, the plot method for ‘estimateEffect’ objects only supports interactions with at
  # least one binary effect modification covariate. ...
  # An additional option is the use of local regression (loess). In this case, because multiple covariates are not
  # possible a separate function is required, plotTopicLoess, which contains a help file for interested users. ...
  # Sparse additive generative model (SAGE)
  # The sparse additive generative (SAGE) model conceptualizes topics as sparse deviations from
  # a corpus-wide baseline (Eisenstein et al. 2011). While computationally more expensive, this
  # can sometimes produce higher quality topics . Whereas LDA tends to assign many rare words
  # (words that appear only a few times in the corpus) to a topic, the regularization of the SAGE
  # model ensures that words load onto topics only when they have sufficient counts to overwhelm
  # the prior. In general, this means that SAGE topics have fewer unique words that distinguish
  # one topic from another, but those words are more likely to be meaningful. Importantly for
  # our purposes, the SAGE framework makes it straightforward to add covariate effects into the
  # content portion of the model.
  # Covariate-free SAGE. While SAGE topics are enabled automatically when using a covariate
  # in the content model, they can also be used even without covariates. To activate
  # SAGE topics simply set the option LDAbeta = FALSE.
  # Covariate-topic interactions. By default when a content covariate is included in the
  # model, we also include covariate-topic interactions. In our political blog corpus for example
  # this means that the probability of observing a word from a Conservative blog in Topic 1
  # is formed by combining the baseline probability, the Topic 1 component, the Conservative
  # component and the Topic 1-Conservative interaction component.
  # Users can turn off interactions by specifying the option interactions = FALSE.
  # ]
  #
  # [We strongly recommend the Global approximation as it provides the best tradeoff of accuracy and
  # computational tractability.]
  # method=[
  # "pointestimate" estimates mean topic proportions for
  # each value of the covariate. "difference" estimates the mean difference in topic
  # proportions for two different values of the covariate (cov.value1 and cov.value2
  # must be specified). "continuous" estimates how topic proportions vary over the
  # support of a continuous covariate.
  # ]
  # [While stm estimates the relationship for the (cKtopics −1)
  # simplex, the workhorse function for extracting the relationships and associated uncertainty
  # on all cKtopics topics is estimateEffect.]

summary(prep, topics = 1) # Beware: outputs as many regressions that have been run!
# summary(prep, topics=topics.cf)
  # Each topic (regression) shows significant coefficients with 1-2 subjects. s(rOtot3) insignificant.
Result <- plot(prep, "subject", # responses.covars[1] might not work coz expected quoted-string name of covariate.
  method="pointestimate",
  # method = "difference", cov.value1 = "Social Science", cov.value2 = "Computing",
  verbose.labels = T,
  ylab = "Expected Difference in Topic Probability (with 95% CI)",
  # xlab = "More Likely Computing Not Significant More Likely Social Science",
  main = "Effect of Covariate on Topic Prevalence",
  # xlim = c(-0.1, 0.1)
)
# was: Result <- plot(
# prep,
# "Subject",
# method = "difference",
# cov.value1 = "Social Science",
# cov.value2 = "Computing",
# verbose.labels = F,
# ylab = "Expected Difference in Topic Probability by Subject (with 95% CI)",
# xlab = "More Likely Computing Not Significant
# More Likely Social Science",
# main = "Effect of Subject on Topic Prevelance for UNCC Research",
# xlim = c(-0.1, 0.1)
# )

# Let’s redo this plot but rank the topics.
# order based on Expected Topic Proportion
Result.means <- unlist(Result$means)
Result.means.noNA <- Result.means[! is.na(Result.means)]
rank = order(Result.means.noNA)
topicRnk <- topicNumProp.STM[rank, ]
whichTopics <- topicRnk$TopicNumber # was: topicRnk$TopicNumber but with NAs
  # maybe coz some topics occur with one subject and not with another subject, which is ok.
whichTopics <- whichTopics[! is.na(whichTopics)]
plot(prep, "subject", model=stmFit, # responses.covars[1]
  # method = "difference",
  # cov.value1 = "Social Science",
  # cov.value2 = "Computing",
  verbose.labels=T, topics=whichTopics,
  # labeltype="frex", n=10,
  #labeltype = "custom",
  #custom.labels = apply(topicNames$prob, 1, function(x) paste0(x, collapse = " + ")), # Works for CTM alone?
  ylab = "Expected Difference in Topic Probability (with 95% CI)",
  # xlab = "More Likely Computing Not Significant More Likely Social Science",
  main = "Effect of Subject on Topic Prevalence" # , xlim = c(-0.1, 0.1)
)

# Effect of Time:
covar.chosen <- responses.covars[6] # 7 for "notrOtot3". 6 for "rOtot3" shows impact *apparently*!
is.covar.chosen.continuous <- TRUE
if(is.covar.chosen.continuous){ 
op <- par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
# stopifnot(length(topics.cf) <= 9) # else choose alt (Qualitative?) palette.
cColourVec <- brewer.pal(length(topics.cf), "Set1")
  # Dark2 Set1 Set2 Paired in decreasing order of preference.
plot(prep, covar.chosen, method="continuous", topics=topics.cf,
  main=paste0("Topics ", paste0(min(topics.cf), ":", max(topics.cf)), " by ", covar.chosen),
    # was: paste0(topics.cf, collapse=",")
  printlegend=T, ylab="Exp. Topic Prob", xlab=covar.chosen # was: , ylim = c(-0.01, 0.16)
  # , linecol=cColourVec # Default colour palette looks ok comparatively.
  # , lwd=4 # lwd= times the default width. BUT does not work here!
)
  # [linecol= For continuous covariates only. A vector ...]
  # topic 4 Exp Topic Prob reduces with increasing rOtot3. It seems to move differently from topic 6.
  # Higher topic 4 prevalence (or presence) relates with higher difficulty (ie lesser rOtot3)!?
  # Reverse seems to be the case for topic 6.
  # Beware: -ve Exp Topic Prob. Indicative of modeling action needed?
par(op)
} # else (! is.covar.chosen.continuous); so continue.

### --- More diagnostics for deeper understanding:
# Regarding alignCorpus() for future use, beware: [all words not appearing in old removed.] Instead, consider
# fitNewDocuments() [A function for predicting thetas for an unseen document based on the previously fit model]
# to confirm whether a new QOS doc chosen for a particular pre-generated topic actually associates that way.
labelTopics(stmFit) # , topics=topics.cf) # Seems insightful! Already stored in topicNames.STM.
sgLabels <- sageLabels(stmFit, n=7) # n=[The number of words to print per topic/topic-covariate set.]
  # [for each topic-covariate group, sageLabels provides a list of the highest marginal] {probability, FREX,
  # lift, score} [words, where marginal means it is summing over all potential covariates. It also provides each topic's
  # Kappa (words associated with each topic) and baselined Kappa (baseline word distribution). ...
  # more detailed alternative to labelTopics.]
# sgLabels List of K:
# iTopic <- 1; sgLabels[[iTopic]]$marginal; sgLabels[[iTopic]]$kappa; sgLabels[[iTopic]]$kappa.m
# $cov.betas List of 250!
print(sgLabels$K); print(sgLabels$n) # sgLabels$marginal
print(sgLabels$kappa) # $kappa seems to be what stmBrowser() shows for topic selection.

# plot(stmFit, type="hist")
  # [histogram of the MAP estimates of the document-topic loadings across all documents. The median is also denoted
  # by a dashed red line.]
plot(stmFit, type="labels") # Cannot specify "frex" [labeltype for content covariate models.]
# duh <- findTopic(stmFit, list=c("magnetic", "parabola"), type="frex", verbose=TRUE); print(duh) # just an example.
  # arg [May also be the output from sageLabels].
library(wordcloud)
mod.out.corr <- topicCorr(stmFit, cutoff=.01); plot(mod.out.corr)
cloud(stmFit, topic=6) # not conclusive.
  # [topic=NULL to plot the marginal distribution of words in the corpus, or a single integer].
### --- exploring estimateEffect() further:
# [
# Plotting covariate interactions: Another modification that is possible in this framework is to allow for interactions
# between covariates such that one variable may “moderate” the effect of another variable. ...
# plot method for ‘estimateEffect’ objects only supports interactions with at least one binary effect modification
# covariate. ...
# plot(prep, covariate = "day", model = poliblogInteraction,
# + method = "continuous", xlab = "Days", moderator = "rating",
# + moderator.value = "Liberal", linecol = "blue", ylim = c(0, 0.12),
# + printlegend = FALSE)
# ]

# [
# Topical content: We can also plot the influence of a topical content covariate. A topical content variable allows
# for the vocabulary used to talk about a particular topic to vary. First, the STM must be
# fit with a variable specified in the content option. ...
# type="perspectives". This function shows which words within a topic are more associated with one covariate value
# versus another. ...
# R> plot(poliblogContent, type = "perspectives", topics = 10)
# This function can also be used to plot the contrast in words across two topics. ... model that did not include a
# content covariate ...
# ]
estEffectTopicwisePlot.srOtot3 <- getEstEffectTopicwisePlot.srOtot3(stmFit, pdout, kTopics=cKtopics, interactTopics=c(4))
# plot(estEffectTopicwisePlot.srOtot3[[4]], "rOtot3", method="continuous", verbose.labels=T)
  # topic4 prevalence is high for low rOtot3.

stop() # Run what's needed from following code:
plot(stmFit, type="perspectives", # labeltype="frex",
  n=30, topics=topics.cf[c(4,5)], text.cex=0.8)

if(FALSE){
prep.deep <- estimateEffect(1:cKtopics ~ subject + s(rOtot3),
  stmFit, meta=out$meta, uncertainty="Global")
  # Was: subject + s(notrOtot3)
  # was: ~ subject + rOtotGrp + jeeadvqaType + jeeadvmarkScheme + notrOtot3
  # was: ~ subject.
myvar <- "subject"; mymethod="pointestimate"
  # [When the covariate of interest is binary, or users are interested in a particular contrast, the method="difference"].
  # alt: "difference" with cov.value1= cov.value2= eg for binary rOtotGrp.
  # ylab="Expected Difference in Topic Probability (with 95% CI)"
  # topics=c(...), ..., labeltype="custom", custom.labels=c(...) for each corresponding topic.
myvar <- "rOtotGrp80"; mymethod="pointestimate"
Result.deepPt <- plot(prep.deep, myvar, method=mymethod, verbose.labels=T)
myvar <- "rOtot3"; mymethod="continuous" # Was: "notrOtot3"
Result.deepConti <- plot(prep.deep, myvar, method=mymethod, verbose.labels=T)
  # 2020Mar14: Topics 3,2 more prevalent with higher notrOtot3. Topic 6 moves the other way.
  # was: Topics 5,7,3 more prevalent with higher notrOtot3. Topics 1,4,6 move the other way. Topic 2 unaffected.
prep.deepB <- estimateEffect(1:cKtopics ~ subject + s(notrOtot3), stmFit, meta=out$meta, uncertainty="Global")
Result.deepBConti <- plot(prep.deepB, myvar, method=mymethod, verbose.labels=T)
  # was:But after s(), that linear effect disappeared eg for Topic 5.  Only topic 7 seems to rise steadily with notrOtot3.
} # else continue.
### ---

if(FALSE){ # init
  # ref https://stackoverflow.com/questions/26570912/error-in-installation-a-r-package to deal with errors.
  # ref https://github.com/mroberts/stmBrowser:
  install.packages("stmBrowser")
    # [Warning message: package ‘stmBrowser’ is not available (for R version 3.6.2)]
  if(!require(devtools)) install.packages("devtools")
  library(devtools)
  install_github("mroberts/stmBrowser", dependencies=TRUE)
  # ref https://github.com/cschwem2er/stminsights:
  # devtools::install_github('rstudio/shiny')
  # devtools::install_github('cschwem2er/stminsights')

  # file.STMinsights <- sub(pattern="STM", replacement="STMinsights", x=file.STM)
  save(responses.orig, responses, text, myCorpus, dfm3, shortdoc, pdout, stmFit, prep,
    file=sub(pattern="STMfit", replacement=paste0("rortcdspsp", tStamp()), x=file.STM)) # Was: "STMinsights"
  # eg load(paste0(cWorkDir, "/STMinsightsfit-ocr-q677topics7covar.RData")
  # eg load(paste0(cWorkDir, "/STMinsightsfit-ocr-q355topics7covar.RData"))
}
# library(stminsights)
# run_stminsights()
library(stmBrowser)
# Was: setwd(tempdir())
responses.utf8 <- responses; responses.utf8[,1] <- myConvUTF8(responses.utf8[,1])
  # for more columns eg apply(responses.utf8[,1:3], MARGIN=2, myConvUTF8)
stmBrowser(stmFit, data=responses.utf8,
  c("jeeadvqaType", "notrOtot3", "subject"), # vector of covariates you want to visualize.
  # data =/= out$meta coz stmBrowser() expects data for stm model. Instead, data= whose documents == responses[,"QOS"].
  text="QOS", # text: name of covariate where the text is held coz stmBrowser() refers data[, text].
  n=1000, # max documents to browse.
  labeltype=mySTMlabeltype(stmFit), # was: "prob" as per stmBrowser() 2015.
  id="idYPSQ", # id=for user to locate original *Solution* QOS for complete human readability.
  # was: id=NULL which translates to row numbers being used as ID for documents (QOS in this case).
  directory=cRepo.tmp # was: directory=getwd().
)
# unlink("stm-visualization", recursive=TRUE) #Remove files

### --- Extract model estimates for further use:
print(str(stmFit$mu)) # [mu=The corpus mean of topic prevalence and coefficients.] Includes gamma for content covariate?
stmBeta <- stmFit$beta # str(stmbeta). [beta=List containing the log of the word probabilities for each topic.]
stmTheta <- stmFit$theta # [theta=Number of Documents by Number of Topics matrix of topic proportions.]
responses.stmTheta <- cbind(responses, stmTheta, responses.orig$ocrqaNum) # responses.orig$ocrqaNum included 2020Mar17.
colnames(responses.stmTheta) <- c(colnames(responses), paste0("topicProp", 1:ncol(stmTheta)), "ocrqaNum")
print(str(responses.stmTheta))
print(summary(responses.stmTheta))
suffix.restmt <- paste0("InsightPlusSTM", tStamp()) # Was: "InsightPlusSTM2020Mar17"
  # Was: "InsightPlusSTM2020Mar15", "InsightPlusSTM2020Mar13", "InsightPlusSTM2020Feb27"
fname.restmt <- paste0(paste0(cWorkDir, "/datJADqos-"), textOptionOCR(TRUE), suffix.restmt, ".csv"); print(fname.restmt)
putFileQOSs(responses.stmTheta, suffix=suffix.restmt) # saveRDS() for perfect machine reading.
write.table(responses.stmTheta, file=fname.restmt, append=FALSE, row.names=FALSE, col.names=TRUE)
  # alt: write.csv() for human readability.
# responses.stmTheta <- datJADplus <- getFiledQOSs(suffix=suffix.restmt, doUnlistRec=FALSE)

### --- From STM, now, back to CTM:
# The four plots are:
# summary - plots topic proportions and names.
# labels - plots the top words for a specific topic.
# perspectives - compares two topics’ words. [always uses highest probability words]
# hist - a histogram of the expected topic proportions across documents for a topic.]
plot(ctmFit, 
         type = "labels", # stmFit Error: Cannot specify label type for content covariate models.
         labeltype="frex", # alt: "prob", "frex", "lift".
           # FREX stands for frequent-exclusive words
         n = 30, 
         topics = topics.cf[1:2], # [4:7], # [1:3], # was: 25. topics.cf[8:10] 43, 45, 60 to drop.
         text.cex = 1.0, # was: 1.2, 
         width = 70) # was: 50)

# Visualizations Example: Correlated Topic Model
stop() # get topicNumProp.CTM first
visNetworkCorrel(ctmFit, topicNames, topicNumProp.CTM)

# plot perspectives for researcher-selected topics:
plot(ctmFit, type="perspectives", labeltype="prob", n=30, topics = topics.cf[c(2,6)], text.cex=0.8)

# Semantic Coherence & Exclusivity
tq <- topicQuality(tmFit, out$documents) # Which topics got have high Semantic Coherence & Exclusivity? And not?
  # [Does not support models with content covariates.]
  # was: 10, 28, 30 are bolder topics!?
topic.cf.q6topic40 <- c(c(13,24,22,20,18), c(9,26,5,3), 34)
topic.cf.q6topic65 <- c(c(54, 62, 61, 42, 20, 60), c(26, 21, 43))
topics.cf <- topic.cf.q6topic40
  # was: c(c(23, 3, 48, 25, 35, 63), c(5, 43, 45, 60)) # OCR. noOCR: c(7, 8, 11, 12, 22, 40)

# toLDAvis(mod=tmFit, docs=out$documents) # Error: [This function does not yet allow content covariates.]
sessionInfo()

# for new QOS, alignCorpus(...); fitNewDocuments(...)
# [
# For users who simply want to find topic proportions for
# previously unseen documents we have the easier to use fitNewDocuments function. In order to
# use fitNewDocuments the vocabularies between the new documents and old must be aligned
# which can be accomplished through the helper function alignCorpus.
# ]

### ---
# [
# 3.5. Understand: Interpreting the STM by plotting and inspecting results
# ... options:
# 1. Displaying words associated with topics (labelTopics, plot method for ‘STM’ objects
# with argument type = "labels", sageLabels, plot method for ‘STM’ objects with
# argument type = "perspectives") or documents highly associated with particular topics (findThoughts, plotQuote).
# 2. Estimating relationships between metadata and topics as well as topical content (estimateEffect).
# 3. Calculating topic correlations (topicCorr).
#
# Understanding topics through words and example documents
# Estimating metadata/topic relationships
# 3.6. Visualize: Presenting STM results
# Summary visualization
# Metadata/topic relationship visualization
# Topical content
# Plotting covariate interactions
# 3.7. Extend: Additional tools for interpretation and visualization
# cloud(), topicCorr(), stmBrowser package, ...
# Customizing visualizations
# ]
