
# Pending TBD 2020-Feb-14:
# - Consider bigrams (or other n-grams) and Dependency Analysis (including KWIC) for raised accuracy, at the cost
#   of efficiency.
# - Understand Threshold plot eg removal of documents etc. to choose k. Consider searchK(), selectModel().
#   Is lower.thresh % or count?
# - Remind to update JEEinsight, considering the newly-corrected meta-data {qaType, markScheme} inconsistencies.
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

# For Project Occipital, here's a rationale for covariate choices during STM model specification:
# - Given a set of QOSs, a count k of (latent) topics to generate, and nothing else, a topic modeler could use FREX
#   and other scores (on DTMs) to map QOSs to k topics.
# - From exam domain, we know that there are as many QOSs in one subject as there are in each of the other two.
# - So, Subject could be included as a covariate for Topic Prevalence when specifying the model.
# - From exam domain, we also know that Topic Prevalence can vary for each Paper. So, Year.Paper might not be
#   a good covariate for Topic Prevalence, esp considering that across a pooled set of Papers, Subject might suffice
#   as a stabler covariate for Prevalence.
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

forOCR <- TRUE
cK <- 7 # 2 unacceptable to stmFit; ctmFit too warns.
  # 65 Topics as per JEE-Main 2020 Syllabus. Was: 40 in CTM ref likely coz visualizability on computer display.
  # 2 might not be a bad place to start; one can see the topic model evolve, viewing differences in topic-word beta etc.
  # 2*subjects=2*3=6 topics might be the minimum acceptable, as that allows (Difficulty or) Ease to be categorized per
  # Subject into {hard,notHard}. Next option would be Difficulty to be categorized into {hard,medium,easy}; so
  # cK=3*3=9 topics.
  # To allow for a topic that doesn't covary with subject, cK=6+1=7 topics might be another option; 7 is also
  # believed to limit human comprehension :-).
  # To choose cK, consider searchK() or [When initialization type is set to "Spectral" the user can specify K = 0].

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
cExtraStop.old <- c("jee","advanced","ans","solution","sol","page","choices","multiple","section",
  "kota","road","office","ltd","website","www.resonance.ac.in","www","fax","city",
  "contact@resonance.ac.in","email","eduventures","corporate","download","portal","sms",
  "fiitjee","fiitjee.com","kalu","sarai","sarvapriya","delhi"
)
  # "resonence","resonsence","besonence","resonsnce","resonance","correct", and single-letter tokens, possibly
  # inside a QOS, might be worthy retaining.
  # u   f0b5   f0ad   f0bc   f0ae ... b c d
  # email IDs, addresses ...
  # Above tokens might have to be removed.

# library(LDAvis)
# library(servr)
par(ask=TRUE)
responses <- responses.orig <- getJADdata(); str(responses); summary(responses)
  # Done: Later, merge with outJADinsight.csv too; then use rOtot possibly as binary factor as Topical-Content covariate.
stopifnot(responses[,2] == responses[,10]) # subject cols match.
stopifnot(responses[,3] == responses[,27]) # YP cols match.
  # [Error ...   level sets of factors are different]. But that's coz one col has till Y2018 and so is ok.
cols2drop <- c(10,27,13:15,28:30)
  # [1] 10 27 13 14 15 28 29 30
responses <- responses[, -cols2drop]
responses.covars <- c("subject", "YP", "jeeadvqaType", "jeeadvmarkScheme", "uOtot3", "rOtot3", "notrOtot3",
  "rOtotGrp", "idYPQ")
  # column names of covariates or docvars.
# duh <- dropPattern(cFooter.FIITJEE, txtvec.noR)
# duh2 <- dropPattern(cFooter.FIITJEE, duh)
# duh7 <- dropMorePatterns(responses[,1], patternVec=c(cFooter.Resonance, cFooter.FIITJEE))
# responses[,1] <- duh
responses[,1] <- dropMorePatterns(responses[,1], patternVec=c(cFooter.Resonance, cFooter.FIITJEE))
responses <- responses[which(! is.na(responses$rOtot3)), c("QOS", responses.covars)]; summary(responses)


### --- Topic Modeling (CTM)
# ref https://rawgit.com/wesslen/Topic-Modeling-Workshop-with-R/master/part2-ctm.html
# https://raw.githubusercontent.com/wesslen/text-analysis-org-science/master/01-datacleaning-exploration.Rmd

library(quanteda)
readability <- textstat_readability(responses[,1])
hist(readability$Flesch, xlab = "Flesch-Kincaid Score", main = "Histogram of Flesch-Kincaid Scores")
  # OCR: mostly close to 80; else for noOCR, it seemed uniformly distributed across xlim ie low-readability
  # responses existed.
# hist(readability$Flesch, # was: $Flesch.Kincaid.
#      xlim = c(0,40), # 40 here might not be based on chosen count of topics cK.
#      breaks = 200, xlab = "Flesch-Kincaid Score", main = "Histogram of Flesch-Kincaid Scores")
hist(ntoken(responses[,1]),
  # OCR: less-spread (word count of) responses.
  # noOCR: > 95th percentile (2000) for file 2016p1 responses. < 5th percentile (~10) for 2013p1.
     breaks = 20,  
     main = "# of Words/Tokens per Response", 
     xlab = "Number of Words/Tokens")
hist(nchar(responses[,1]), 
     breaks = 20,  
     main = "# of Characters per Response", 
     xlab = "Number of Characters")
minWords <- summary(ntoken(responses[,1]))[1] # min.
minWords <- max(minWords, 30); responses[which(ntoken(responses[,1]) < minWords), 1] # Check for any "outliers"?

### Covariates: Explore & Extraction
if(length(responses.covars) >= 1){ # once covariates are included
  responses %>% 
    group_by(subject) %>% # was: (Q8, Q9). But group_by(responses.covars) did not work coz maybe colnames required.
      # Even group_by(responses.covars[1]) does not work, even if it's a factor and not char. Maybe coz quoted.
    summarise(Count = n())
} # else continue

text <- responses[,1]
myCorpus <- corpus(text)
  # metacorpus = list(source="From JAD QOS rds.")
docvars(myCorpus, field=responses.covars) <- responses[, responses.covars]
  # add in the attributes about the responses
  # alt: <- c(responses$subject, responses$YP)
# docvars(myCorpus, "ManagerGender") <- c(responses$Q8,responses$Q8)
# the document-feature matrix (DFM) includes the document-term matrix (counts of each term in each document)
# along with the covariates (features) we just provided above.
### Tokenize, Stemming, Uni/Bi/Tri-grams, Stop Words (dfm)
dfm <- dfm(myCorpus, 
           remove = c(stopwords("english")), 
           ngrams=1L, # alt: unigrams, bigrams
           stem = F, 
           remove_numbers = T, 
           remove_punct = T, 
           remove_symbols = T, 
           remove_hyphens = F)
dfm.orig <- dfm # just as a backup.
topfeatures(dfm,25)
extra.stop <- cExtraStop # Let's explore removing sparse terms.
dfm <- dfm(myCorpus, 
           remove = c(stopwords("english"), extra.stop), 
           ngrams = 1L, 
           stem = F, 
           remove_numbers = T, 
           remove_punct = T, 
           remove_symbols = T, 
           remove_hyphens = F)
dfm <- dfm_trim(dfm, min_docfreq = 2) # docfreq_type = "prop"
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
topfeatures(dfm,25)

# Let's plot two word clouds -- one as a whole and the second by a covariate. 
library(RColorBrewer)

textplot_wordcloud(
  dfm,
  scale = c(3.5, .75),
  colors = brewer.pal(8, "Dark2"),
  random.order = F,
  rot.per = 0.1,
  max.words = 100
)

# now without those extra.stop words and in comparison with a covariate
cdfm <- dfm(
  myCorpus,
  groups = responses.covars[1], # alt: "subject", (but not continuous covar) "YP". was: "Country",
  remove = c(stopwords("english"), extra.stop),
  stem = F,
  remove_numbers = T,
  remove_punct = T,
  remove_symbols = T,
  remove_hyphens = F
)

textplot_wordcloud(
  cdfm,
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
  tfidf(dfm),
  scale = c(3.5, .75),
  colors = brewer.pal(8, "Dark2"),
  random.order = F,
  rot.per = 0.1,
  max.words = 100
  )

# We can use word clustering to identify words that co-occur together.
wordDfm <- dfm_sort(dfm_weight(dfm, "frequency"))
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
wordDfm <- dfm_sort(dfm_weight(dfm, "tfidf")) # was: "tfidf"
  # Warning message: scheme = "tfidf" is deprecated; use dfm_tfidf(x) instead.
  # dfm_tfidf(wordDfm, scheme_tf = "logcount")
wordDfm <- t(wordDfm)[1:50,]  # because transposed
wordDistMat <- dist(wordDfm)
wordCluster <- hclust(wordDistMat)
plot(wordCluster, xlab="", main="TF-IDF weighting")


# https://rawgit.com/wesslen/Topic-Modeling-Workshop-with-R/master/part2-ctm.html
# Correlated Topic Model (CTM) with stm package
library(stm)

# use quanteda converter to convert our Dfm
stmdfm <- convert(dfm, to = "stm") # stmdfm$documents, stmdfm$vocab, stmdfm$meta
  # alt: stmdfm <- convert(dfm, to = "stm", docvars = docvars(myCorpus))
# Unlike the topicmodels packages, stm has built in features to help analysts reduce sparse terms (minDoc or minCount).
plotRemoved(stmdfm$documents, lower.thresh = seq(1, 100, by = 10))

stop() why not lower.thresh=0 or original 5 with upper.thresh? # considering purpose is modeling for QOS difficulty?
out <- 	(documents=stmdfm$documents, vocab=stmdfm$vocab, meta=stmdfm$meta) # lower.thresh=1 default.
otrem <- out$tokens.removed; owrem <- out$words.removed; print(otrem); print(head(owrem))
  # Consider Project Occipital's intent to model question difficulty before altering lower.thresh using plotRemoved().
  # Removing 4526 of 6982 terms (4526 of 22906 tokens) due to frequency 
  # Your corpus now has 358 documents, 2456 terms and 18380 tokens.> 
  # Consider Project Occipital's intent to model
  # question difficulty before altering lower.thresh using plotRemoved().
  #
  # 2020Feb23: [Removing 4525 of 6992 terms (4525 of 26361 tokens) due to frequency 
  # Your corpus now has 358 documents, 2467 terms and 21836 tokens.]
  # was: lower.thresh=5: [Removing 1670 of 2386 terms (4701 of 18806 tokens) due to frequency
  # Your corpus now has 363 documents, 716 terms and 14105 tokens.]
  # [lower.thresh=Words which do not appear in a number of documents greater than lower.thresh will be dropped and both
  # the documents and vocab files will be renumbered accordingly. ... If the vocabulary is in excess of 5000 entries
  # inference can slow quite a bit.]
  # upper.thresh= is also possible.
print(out$tokens.removed)
vocab.wrem <- stmdfm$vocab[out$words.removed]; glimpse(vocab.wrem)
summary(out$meta) # Confirm no NAs; else stmFit() fails.
if(FALSE){
  ### --- ref https://github.com/bstewart/stm/issues/144:
  #find the missing values
  keep <- !is.na(out$meta$subject)
  #keep only observed values in meta and docs
  outmeta <- out$meta[keep,]
  outmetadocs <- out$meta$docs[keep]
  #rerun prepDocuments to deal with missing vocab
  #now everything should be fine
  ### ---
}
k <- cK; cMaxEMiter <- 200 # k*4 might not suffice for convergence.
  # Beware: stm() ahead ran for over 5 hours converged in 22 Iterations!
mustGenSTMCTMmodels <- TRUE # was: FALSE
file.CTM <- paste0(cWorkDir, "/", "CTMfit-", textOptionOCR(forOCR), "-q", nrow(responses), "topics", k, "covar", ".RData")
file.STM <- sub(pattern="CTM", replacement="STM", x=file.CTM)
if(mustGenSTMCTMmodels){
  # You can run the model (which will take several minutes). This time, let's consider running a k-topic model.
  ctmFit <- stm(out$documents, out$vocab, K = k,
    max.em.its=cMaxEMiter, data = out$meta, init.type = "Spectral", seed = 300) # was: max.em.its=150 for k=40 topics.
  save(ctmFit, file=file.CTM)

  # STM takes the rest of this CTM source code to another level.
  stmFit <- stm(out$documents, out$vocab, K=k,
    prevalence =~ subject + rOtotGrp + jeeadvqaType + jeeadvmarkScheme, content =~ notrOtot3,
      # prefer: prevalence =~ jeeadvmarkScheme+subject+rOtotGrp, content =~ notrOtot3, # coz MultCorrAns explains notrOtot
      # alt: prevalence =~ subject + rOtotGrp, content =~ notrOtot3,
      # was: prevalence =~ subject + s(rOtot3), content =~ rOtot3
      # Later, replace with content=~ rOtot (or its binary factor), possibly in combo with subject as 1 var.
      # [Currently content can only contain one variable.]
    max.em.its = cMaxEMiter, data = out$meta, init.type = "Spectral", seed = 300) # seed= for reproducibility.
      # Recommended "Spectral" algorithm has Reference to Dr Anima Anandkumar's publication: I feel proud!
      # was: max.em.its=75 for k=20 topics.
  # https://raw.githubusercontent.com/bstewart/stm/master/vignettes/stmVignette.pdf (2019Oct) says:
  # [Topical content ...
  # It is important to note that this is a completely new model, and so the
  # actual topics may differ in both content and numbering compared to the previous example
  # where no content covariate was used.]

  chkb <- checkBeta(stmFit) # [Looks for words that load exclusively onto a topic]. Nothing seems to be "overloaded".
  stopifnot(chkb$check) # [boolean representing if the check was passed].
  chkr <- checkResiduals(stmFit, out$documents)
  stopifnot(chkr$dispersion <= 1)
    # [If we calculate the sample dispersion and the value is greater than one, this implies that the number of topics
    # is set too low, because the latent topics are not able to account for the overdispersion.]

  save(stmFit, file=file.STM)
} else {
  # The code simply loads the file.
  load(file = file.STM) # since that's superior.
}
tmFit <- stmFit # or ctmFit, as needed.

# Exploring the results through stm visualizations.
plot(ctmFit, # Beware: not stmFit.
         type = "summary", 
         # was: xlim = c(0,.16), 
         n = 5, 
         labeltype = "prob", # Error with stmFit: Cannot specify label type for content covariate models.
         main = "Topics", 
         text.cex = 0.8)
# Drop topic 2 (coz contact details)?
# Can we see almost identical topics; if so, this is a good sign? Are our topics "stable" across runs, even in this
# case after we added in prevalent covariates?
topicNames.STM <- labelTopics(stmFit, n=5) # n=count of tokens or terms to include?
topicNumProp.STM <- data.frame(TopicNumber=1:k, TopicProportions=colMeans(stmFit$theta)) # was: topic <-
str(topicNames.STM); str(topicNumProp.STM)
topics.cf <- c(1:k) # i <- c(1:3) # c(9, 18)

plot(stmFit, type="summary") # Top topics (prevalence).
shortdoc <- headCharVec(text, n=250) # was: n=200 for this shortened form for fitting display frame.
thoughts <- thoughtQuotesForTopicVec(stmFit, topics=topics.cf, shortdoc, nMostAssociatedDocs=5)
# thoughtQuotesForTopicPair(stmFit, topics=topics.cf, shortdoc)
  # Beware: topic differs for CTM and STM (with covar).

# Exploring the effects of the (Topical-Prevalence) covariates:
# Next, we want to explore the effect of the covariates on the topic proportions (likelihood of the topic).
# First, we'll need to use the estimateEffect function to estimate this effect.
prep <- estimateEffect(1:k ~ subject + rOtotGrp,
  stmFit, meta=out$meta, uncertainty="Global")
  # 1:k ~ subject + jeeadvqaType: not ok [Covariate matrix is singular.]
  # 1:k ~ subject + rOtotGrp + jeeadvqaType: not ok [Covariate matrix is singular.]
  # 1:k ~ subject + rOtotGrp + jeeadvqaType + jeeadvmarkScheme: not ok [Covariate matrix is singular.]
  # was: 1:k ~ subject + rOtotGrp
  # was: estimateEffect(1:k ~ subject + s(rOtot3), stmFit, meta=out$meta, uncertainty="Global")
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
  # [While stm estimates the relationship for the (K −1)
  # simplex, the workhorse function for extracting the relationships and associated uncertainty
  # on all K topics is estimateEffect.]

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
topicRnk <- topic[rank, ]
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
op <- par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
covar.chosen <- responses.covars[7] # 7 for "notrOtot3". 6 for "rOtot3"
stopifnot(length(topics.cf) <= 9) # else choose alt (Qualitative?) palette.
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

### --- More diagnostics for deeper understanding:
# labelTopics(tmFit) # , topics=topics.cf) # Seems insightful! Already stored in topicNames.STM.
# Regarding alignCorpus() for future use, beware: [all words not appearing in old removed.] Instead, consider
# fitNewDocuments() [A function for predicting thetas for an unseen document based on the previously fit model]
# to confirm whether a new QOS doc chosen for a particular pre-generated topic actually associates that way.
sgLabels <- sageLabels(stmFit, n=7) # n=[The number of words to print per topic/topic-covariate set.]
  # [for each topic-covariate group, sageLabels provides a list of the highest marginal] {probability, FREX,
  # lift, score} [words, where marginal means it is summing over all potential covariates. It also provides each topic's
  # Kappa (words associated with each topic) and baselined Kappa (baseline word distribution). ...
  # more detailed alternative to labelTopics.]
# sgLabels List of K:
# iTopic <- 1; sgLabels[[iTopic]]$marginal; sgLabels[[iTopic]]$kappa; sgLabels[[iTopic]]$kappa.m
# $cov.betas List of 250!
sgLabels$K; sgLabels$n; # sgLabels$marginal
sgLabels$kappa # $kappa seems to be what sgBrowser() shows for topic selection.

# plot(stmFit, type="hist")
  # [histogram of the MAP estimates of the document-topic loadings across all documents. The median is also denoted
  # by a dashed red line.]
plot(stmFit, type="labels") # Cannot specify "frex" [labeltype for content covariate models.]
? <- findTopic(stmFit, list=c("magnetic", "parabola"), type="frex", verbose=TRUE)
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
prep.deep <- estimateEffect(1:k ~ subject + s(notrOtot3),
  stmFit, meta=out$meta, uncertainty="Global")
  # was: ~ subject + rOtotGrp + jeeadvqaType + jeeadvmarkScheme + notrOtot3
  # was: ~ subject.
myvar <- "subject"; mymethod="pointestimate"
  # [When the covariate of interest is binary, or users are interested in a particular contrast, the method="difference"].
  # alt: "difference" with cov.value1= cov.value2= eg for binary rOtotGrp.
  # ylab="Expected Difference in Topic Probability (with 95% CI)"
  # topics=c(...), ..., labeltype="custom", custom.labels=c(...) for each corresponding topic.
myvar <- "rOtotGrp"; mymethod="pointestimate"
Result.deepPt <- plot(prep.deep, myvar, method=mymethod, verbose.labels=T)
myvar <- "notrOtot3"; mymethod="continuous"
Result.deepConti <- plot(prep.deep, myvar, method=mymethod, verbose.labels=T)
  # was: Topics 5,7,3 more prevalent with higher notrOtot3. Topics 1,4,6 move the other way. Topic 2 unaffected.
prep.deep2 <- estimateEffect(1:k ~ subject + s(notrOtot3), stmFit, meta=out$meta, uncertainty="Global")
Result.deep2Conti <- plot(prep.deep2, myvar, method=mymethod, verbose.labels=T)
  # was:But after s(), that linear effect disappeared eg for Topic 5.  Only topic 7 seems to rise steadily with notrOtot3.
prep.deep7 <- estimateEffect(c(7) ~ subject + s(notrOtot3), stmFit, meta=out$meta, uncertainty="Global")
Result.deep7Conti <- plot(prep.deep7, myvar, method=mymethod, verbose.labels=T)
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

  file.STMinsights <- sub(pattern="STM", replacement="STMinsights", x=file.STM)
  save(responses.orig, responses, text, shortdoc, out, stmFit, prep, file=file.STMinsights)
  # eg load("c://users/SONY/Desktop/preBachelors/www/solnJEEAdvanced/STMinsightsfit-ocr-q355topics7covar.RData")
}
# library(stminsights)
# run_stminsights()
library(stmBrowser)
setwd(tempdir())
responses.utf8 <- responses; responses.utf8[,1] <- myConvUTF8(responses.utf8[,1])
  # for more columns eg apply(responses.utf8[,1:3], MARGIN=2, myConvUTF8)
stop() id= TBD correctly without mismatched dup YP etc.
stmBrowser(stmFit, data=responses.utf8,
  c("jeeadvqaType", "notrOtot3", "subject"), # vector of covariates you want to visualize.
  # data =/= out$meta coz stmBrowser() expects data for stm model. Instead, data= whose documents == responses[,"QOS"].
  text="QOS", # text: name of covariate where the text is held coz stmBrowser() refers data[, text].
  n=1000, # max documents to browse.
  labeltype=mySTMlabeltype(stmFit), # was: "prob" as per stmBrowser() 2015.
  id="idYPQ" # id=for user to locate original QOS for complete human readability.
  # was: id=NULL which translates to row numbers being used as ID for documents (QOS in this case).
  # was: directory=getwd().
)
#Remove files
unlink("stm-visualization", recursive=TRUE)

### --- Extract model estimates for further use:
print(str(stmFit$mu)) # [mu=The corpus mean of topic prevalence and coefficients.] Includes gamma for content covariate?
stmBeta <- stmFit$beta # str(stmbeta). [beta=List containing the log of the word probabilities for each topic.]
stmTheta <- stmFit$theta # [theta=Number of Documents by Number of Topics matrix of topic proportions.]
responses.stmTheta <- cbind(responses, stmTheta)
colnames(responses.stmTheta) <- c(colnames(responses), paste0("topicProp", 1:ncol(stmTheta)))
print(str(responses.stmTheta))
print(summary(responses.stmTheta))
suffix.restmt <- "InsightPlusSTM2020Feb27"
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
visNetworkCorrel(ctmFit, topicNames, topic)

# plot perspectives for researcher-selected topics:
plot(ctmFit, type="perspectives", labeltype="prob", n=30, topics = topics.cf[c(1,2)], text.cex=0.8)

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
