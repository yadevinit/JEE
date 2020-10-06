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
tStamp <- function(){
  return(format(Sys.time(), "%Y%m%dT%H%M%S")) # For unique filename, format timestamp similar to ISO 8601.
}
checkResidualsCustom <- function(stmFit, out.documents, tol=0.01){
  print(str(stmFit$time)) # [The time elapsed in seconds.]
  print(str(stmFit$convergence))
  stopifnot(stmFit$convergence$converged) # Upon failure, consider restarting.
  chkb <- checkBeta(stmFit) # [Looks for words that load exclusively onto a topic].
  stopifnot(chkb$check) # [boolean representing if the check was passed]. Nothing seems to be "overloaded"?
  chkr <- checkResiduals(stmFit, out.documents, tol=tol)
    # [Computes the multinomial dispersion of the STM residuals as in Taddy (2012)]
    # [tol=0.01. The tolerance parameter for calculating the degrees of freedom. Defaults to 1/100 as in Taddy (2012) ...
    # In estimating the degrees of freedom, we follow Taddy (2012) in approximating the parameter \hat{N} by the
    # number of expected counts exceeding a tolerance parameter. The default value of 1/100 given in the Taddy paper
    # can be changed by setting the tol argument.]
  print("checkResiduals:"); print(chkr)
    # Was: stopifnot((chkr$dispersion <= 1) && (chkr$pvalue <= 0.05))
    # Was till 2020Mar29: (chkr$pvalue <= 0.05)
  msgTaddy.dispersion.pValue <- c(
    ifelse((chkr$dispersion <= 1),
      "Binomial distribution 0 < VMR < 1 under-dispersed or Poisson distribution VMR = 1 might suffice.",
      "Negative binomial distribution VMR > 1 over-dispersed it seems. Consider topics >K & correlated phrase counts."),
      # http://proceedings.mlr.press/v22/taddy12/taddy12.pdf says:
      # [
      # In 1.b, the sample dispersion distribution is shown for K = 9, 10, 11 at each size specification. Of
      # primary interest, sigma^2 is almost always larger than one for K < 10 and less than one for K ≥ 10.
      # This pattern persists for un-plotted K, and separation across models increases with M. Estimated dispersion does
      # appear to be biased low by roughly 1-6% depending
      # on size, illustrating the difficulty of choosing effective
      # degrees of freedom. As a result, our chi^2 test of a moretopics-than-K alternative leads to p-values of p = 0
      # for K < 10 and p = 1 for K >= 10. ...
      # Model selection results are in Figure 3. The marginal likelihood surfaces, again expressed as Bayes factors,
      # are maximized at K = 20 for the we8there data and at K = 12 for congress109.
      # Interestingly, dispersion estimates remain larger than one for these chosen models, and we do not approach
      # sigma^2 = 1 even for K up to 200. This indicates alternative sources of overdispersion beyond topic-clustering,
      # such as correlation between counts across phrases in a given document.]
    ifelse((chkr$pvalue > 0.05),
      # https://stattrek.com/chi-square-test/goodness-of-fit.aspx says:
      # [The P-value is the probability of observing a sample statistic as extreme as the test statistic. ...
      # If the sample findings are unlikely, given the null hypothesis, the researcher rejects the null hypothesis.]
      "Multinomial dispersion of the STM residuals seems explained adequately by chosen topics.",
      "Null Hypothesis of sigma^2=1 is rejected. So, the more-topics-than-K alternative seems plausible."
    )
  )
  print(msgTaddy.dispersion.pValue)
  # Was till 2020Mar29:
  #  if((chkr$dispersion <= 1) && (chkr$pvalue > 0.05)){
  #    print("binomial distribution 0 < VMR < 1 under-dispersed or Poisson distribution VMR = 1 might suffice.")
  #  } else {
  #    print(
  #      "negative binomial distribution VMR > 1 over-dispersed seems warranted, rejecting Null Hypothesis of sigma2=1.")
  #  }
    # fails for K=7,13,36 when (prevalence =~ YP+subject, content =~ rOtotGrp) or
    #   (prevalence =~ YP*subject, content =~ rOtotGrp)
    # [If we calculate the sample dispersion and the value is greater than one, this implies that the number of topics
    # is set too low, because the latent topics are not able to account for the overdispersion.]
    # https://en.wikipedia.org/wiki/Index_of_dispersion says:
    # [
    # Distribution			VMR
    # constant random variable		VMR = 0	not dispersed
    # binomial distribution		0 < VMR < 1	under-dispersed
    # Poisson distribution		VMR = 1
    # negative binomial distribution	VMR > 1	over-dispersed
    # ]
  return(chkr)
}
# cKtopics <- 7; cMaxEMits=30; cEMtol=10*1e-05
#   # [emtol= Defaults to .00001. You can set it to 0 to have the algorithm run max.em.its number of steps]
# f.prevalence <- as.formula(paste0("~", "subject + rOtot3"))
# f.content <- as.formula(paste0("~", "rOtot3"))
stmCustom <- function(out, arg.prevalence=f.prevalence, arg.content=f.content,
  arg.K=cKtopics, arg.max.em.its=cMaxEMits, arg.emtol=cEMtol, # Increase to restart incomplete modeling from arg.model.
  arg.model=NULL, mustSave=TRUE){ # Incomplete model to restart stm() with.

  # [
  # stm::textProcessor().
  # stm::manyTopics() Does not work with models that have a content variable (at this point). ...
  # stm::searchK() Computes diagnostic values for models with different values of K (number of topics). ...
  # stm::selectModel() Assists the user in selecting the best STM model. plotModel().
  # stm::permutationTest() Permutation test of a binary covariate.??
  # ]

  cPkgs.0topicsSTM <- c("Rtsne", "rsvd", "geometry")
  if(FALSE){ # init
    install.packages(cPkgs.0topicsSTM)
  } # else continue.
  for(pkg in cPkgs.0topicsSTM){
    library(pkg, character.only=TRUE)
  }
  # [You can convert from quanteda's format directly to our native format using the quanteda function convert.
  # If documents is a sparse matrix or quanteda dfm object, then vocab should not (and must not) be supplied.
  # It is contained already inside the column names of the matrix.]
  date(); print(arg.prevalence); print(arg.content); print(paste("args.: ", arg.K, arg.max.em.its, arg.emtol, mustSave))
  stmFit <- stm(out$documents, out$vocab, K=arg.K,
    prevalence=arg.prevalence, content=arg.content,
      # Till 2020Mar18:   prevalence =~ subject + rOtot3, content =~ rOtot3,
      # Till 2020Mar11, it was: prevalence =~ subject + rOtotGrp + jeeadvqaType + jeeadvmarkScheme, content =~ notrOtot3
      # prefer: prevalence =~ jeeadvmarkScheme+subject+rOtotGrp, content =~ notrOtot3, # coz MultCorrAns explains notrOtot
      # alt: prevalence =~ subject + rOtotGrp, content =~ notrOtot3,
      # was: prevalence =~ subject + s(rOtot3), content =~ rOtot3
      # Later, replace with content=~ rOtot (or its binary factor), possibly in combo with subject as 1 var.
      # [Currently content can only contain one variable.]
    max.em.its=arg.max.em.its, emtol=arg.emtol, model=arg.model,
      # Was: model=stmFit, max.em.its=cMaxIter*2, # Uncomment to restart incomplete modeling stored in stmFit.
    data=out$meta, init.type="Spectral", seed=300, verbose=TRUE) # seed= for reproducibility.
      # Recommended "Spectral" algorithm has Reference to Dr Anima Anandkumar's publication: I feel proud!
      # was: max.em.its=75 for k=20 topics.
  stmFit.bak <<- stmFit # Just to avoid losing work done in case of error and crash ahead within this function.
  # Was: stop() # k <- 13? set k (and dependents) to what was last run.

  # https://raw.githubusercontent.com/bstewart/stm/master/vignettes/stmVignette.pdf (2019Oct) says:
  # [Topical content ...
  # It is important to note that this is a completely new model, and so the
  # actual topics may differ in both content and numbering compared to the previous example
  # where no content covariate was used.]
  chkr <- checkResidualsCustom(stmFit, out$documents, tol=0.01)
  if(mustSave){
#https://softwareengineering.stackexchange.com/questions/61683/standard-format-for-using-a-timestamp-as-part-of-a-filename

    file.STM <- paste0(cWorkDir, "/", "STMfit", "-q", length(out$documents), "t", arg.K, "covar", tStamp(), ".RData")
    save(stmFit, file=file.STM)
  } # else continue.

  return(stmFit)
}
getEstEffectTopicwisePlot.srOtot3 <- function(stmFit, out, kTopics=cKtopics, interactTopics=c(4)){
  par(ask=TRUE)
  myvar <- "rOtot3"; mymethod="continuous"

  prepEff.subject.srOtot3 <- list(); result.plot.prepEff <- list()
  fmla1 <- as.formula(paste0("1:", kTopics, " ~ subject * s(rOtot3)")) # b-spline transformed var through s(var)
  prepEff.subject.srOtot3 <- estimateEffect(fmla1,
    # Was: estimateEffect(1:kTopics ~ subject * s(rOtot3), # Was: fmla1,
    stmFit, meta=out$meta, uncertainty="Global")
  result.plot.prepEff <- plot(prepEff.subject.srOtot3, myvar, method=mymethod, verbose.labels=T)
  prepEff.subject.srOtot3 <- list(); result.plot.prepEff <- list()

  for(ti in 1:kTopics){
    # 2020Mar14:
    # Topics 2 (mostly C) more prevalent with higher notrOtot3.
    # Topic 6 (mostly C with mix of PM) prevalence moves the other way, while Topic 5 (mostly M) prevalence gradually
    # reduces with higher notrOtot3.
    fmla2 <- as.formula(paste0("c(", ti, ") ~ subject * s(rOtot3)")) # b-spline transformed var through s(var)
    prepEff.subject.srOtot3[[ti]] <- estimateEffect(fmla2, stmFit, meta=out$meta, uncertainty="Global")
    result.plot.prepEff[[ti]] <- plot(prepEff.subject.srOtot3[[ti]], myvar, method=mymethod, verbose.labels=T)
    if(ti %in% interactTopics){
      cColourVec <- c("orange", "green", "blue"); subject.levels <- c("C", "M", "P")
      for(isub in 1:length(subject.levels)){
        plot(prepEff.subject.srOtot3[[ti]], myvar, method=mymethod, verbose.labels=T,
          # covariate="day", model=poliblogInteraction,
          moderator="subject", moderator.value=subject.levels[isub], linecol=cColourVec[isub],
          add=TRUE, printlegend=FALSE
        )
      }
      legend(0, 0.06, subject.levels, lwd=2, col=cColourVec)
    } # else continue.
  }
  return(prepEff.subject.srOtot3)
}
prepDocumentsCustom <- function(arg.documents, arg.vocab, arg.meta, arg.lower.thresh=1){
  # stop() # why not lower.thresh=0 or original 5 with upper.thresh? # considering purpose is modeling for QOS difficulty?
  print("[If the vocabulary is in excess of 5000 entries inference can slow quite a bit.]")
  # Unlike the topicmodels packages, stm has built in features to help analysts reduce sparse terms (minDoc or minCount).
  plotRemoved(arg.documents, lower.thresh=seq(1, 100, by=10))

  out <- prepDocuments(documents=arg.documents, vocab=arg.vocab, meta=arg.meta,
    lower.thresh=arg.lower.thresh) # lower.thresh=1 default.
    # [Importantly, prepDocuments will also re-index all metadata/document relationships if any
    # changes occur due to processing].
  otrem <- out$tokens.removed; owrem <- out$words.removed; print(otrem); print(head(owrem))
  vocab.wrem <- arg.vocab[owrem]; print(glimpse(vocab.wrem))
  print(summary(out$meta)) # Confirm no NAs; else stmFit() fails.
  # 2020Mar13: removed 0.
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
  } # else continue.

  return(out)
}
textProcessorCustom <- function(arg.documents, arg.metadata=NULL, arg.striphtml=FALSE){ # before prepDocumentsCustom().
  tpout <- textProcessor(
    documents=arg.documents,
    metadata=arg.metadata, # default NULL,
    # lowercase = TRUE,
    # removestopwords = TRUE, removenumbers = TRUE,
    # removepunctuation = TRUE, ucp = FALSE, stem = TRUE,
    # wordLengths = c(3, Inf), sparselevel = 1, language = "en",
    # verbose = TRUE, onlycharacter = FALSE,
    striphtml=arg.striphtml, # default FALSE, but TRUE needed considering MathJax script. BEWARE if it consumes Math `<>`!
    customstopwords=c(stopwords("english"), cExtraStop.old), # customstopwords=NULL by default.
    # custompunctuation = NULL # eg c(".","?","!")
  )
  return(tpout) # tpout$vocab=Character vector of vocabulary.
}
corpusCustom <- function(text, meta.docnames=NULL, meta.docLevel=NULL, meta.corpusLevel=list()){
  library(quanteda)
  myCorpus <- quanteda::corpus(x=text,
    docnames=meta.docnames, # Default [docnames=NULL "text1", "text2", etc. are assigned automatically].
    docvars=meta.docLevel, # (QOS-)Document-level meta vars. Default docvars=NULL.
    meta=meta.corpusLevel # Corpus-level meta vars. Default meta=list().
    # , unique_docnames=TRUE # [if TRUE, enforce strict uniqueness in docnames].
    # , ...
  )
  # quanteda::tokens_skipgrams(x=text, n=2L, skip=0:1, concatenator="_")
    # Consistent with [skip-grams found in Guthrie et al (2006)].
  # quanteda::docvars(myCorpus, field=responses.covars) <- responses[, responses.covars]
    # add in the attributes about the responses
  # the document-feature matrix (DFM) includes the document-term matrix (counts of each term in each document)
  # along with the covariates (features) we just provided above.

  # Was: corpus.doc <- list(myCorpus, out); return(corpus.doc)
  return(myCorpus)
}
