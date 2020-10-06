# TBD 2020Feb14:
# - Consider readtext package as alt to pdftools, as per https://kenbenoit.net/pdfs/text_analysis_in_R.pdf. Maybe it
# gives superior quality, but still OCR reading capability (tesseract) might be indispensable.
#

stop() # run manually whatever's needed coz data files being created here.
### --- Code for creating data files in stages:
# (3a) prefer PCM documents that already have solutions, rather than just the questions with maybe the answer keys
# or separate document files for physics, chemistry, and maths.
if(FALSE){ # only once at start to generate RDS files.
  filesPDF <- myChoosePDFnoOCR()
  myPDFtoOCR(filesPDF, c(1:12)) # beware: it takes time.

  # Optionally, choose the OCR-processed files, eg, in case they have greater info
  # of formulae and images or they have half the "whitespace noise" (53977 vs 116235).
  # files <- myChooseFiles(filext=c("All"))
  # jads.OCR <- myreadRDS(files); glimpse(jads.OCR)
  # jads.OCR <- myunlist(jads.OCR); glimpse(jads.OCR); saveRDS(jads.OCR, file=cFile.OCR)
    # iff OCR-processed RDS files selected.
  # jads.noOCR <- myreadRDS(files); glimpse(jads.noOCR)
  # jads.noOCR <- myunlist(jads.noOCR); glimpse(jads.noOCR); saveRDS(jads.noOCR, file=cFile.noOCR)
    # iff noOCR-processed RDS files selected.
} # else continue
cFile.OCR <- paste0(cWorkDir, "/jads-ocr.rds")
  # Was: "C:\\Users\\SONY\\Documents/../Desktop/preBachelors/www/solnJEEAdvanced/jads-ocr.rds"
cFile.noOCR <- paste0(cWorkDir, "/jads-nocr.rds")
  # Was: "C:\\Users\\SONY\\Documents/../Desktop/preBachelors/www/solnJEEAdvanced/jads-nocr.rds"
jads.noOCR <- myunlist(myreadRDS(cFile.noOCR)); glimpse(jads.noOCR)
jads.OCR <- myunlist(myreadRDS(cFile.OCR)); glimpse(jads.OCR)

stop()
if(forOCR){ # jads.OCR or jads.noOCR to be chosen.
  jads <- jads.OCR
} else {
  jads <- jads.noOCR
}


  # (3b.2) consider Document attributes as {Year, Subject, Paper/Shift, Code (for mapping order)} as part of metamodel.
  # Then use question-options-solution (QOS) as Paragraph.
  # File jads[[iFile]] (across iPage), for example, has these salient elements:
  # "
  # JEE (ADVANCED) 2018 PAPER 1
  # CODE 5
  # PART-I PHYSICS
  # SECTION 1 (Maximum Marks
  # 1. question with maybe Options (A) to (D) fillowed by Soln
  # ...
  # Q.n question ...
  # SECTION m
  # PART p
  # ...
  # "
  #
locYPCSubSec <- locatePattern(jads, pattern=cYPCSubSec) # was: locateYPCSubSec(jads)
print(str(locYPCSubSec))
jads.YPCSubSec <- myregmatches(jads, locYPCSubSec,
  wantUniquePattern=c(cYPCSubSec.order[1], cYPCSubSec.order[2], cYPCSubSec.order[3])) # Y, P, C
print(str(jads.YPCSubSec))

locQOS <- locatePattern(jads, pattern=cPatQOS)
print(str(locQOS))
# try.QOS <- myregmatches(jads, locQOS, wantUniquePattern=c(1,2,3))
# str(try.QOS); tail(try.QOS[[12]][[1]][1])
jads.QOS <- myregmatches(jads, locQOS, wantUniquePattern=c())
  # regmatches(s, gregexpr(pattern, s))
  # start of Question to start of next Question (or earlier Section, Paper, or EOF) is what's a QOS.
print(str(jads.QOS))
# print(unique(jads.QOS[[5]][[2]][[1]])) # [1] "A"
# print(str(jads.QOS[[5]][[2]][[1]]))

# fname <- paste0(cWorkDir, "/jads-", ifelse(forOCR, "ocr", "nocr"), "-locQOS.rds")
# saveRDS(locQOS[[1]], file=fname, ascii=TRUE)
mylocQ.toCSV(locQOS[[1]], fname=paste0(cWorkDir, "/jads-", ifelse(forOCR, "ocr", "nocr"), "-locQOS.csv"))
# Improve QOS text directly, including any split and merge. Do any fresh QOS data entry referring to PDF
# only when considering mapping with outJEEinsight data (which has rOtot outcome), as that's most effort
# intensive and best done by working on extremes of rOtot minimally to develop a reasonable model of difficulty.
# TBD: correct iChar Part/Subject & Section before using them in metamodel with YPC.
# For now, proceed with topic modeling before reverting to do these detailed TBD works.

stop()
QOSs <- mySaveQOSs(jads, locQOS, forOCR=forOCR, mustWrite=FALSE)
# QOSs <- mySaveQOSs(jads, locQOS.ok, forOCR=forOCR, mustWrite=TRUE) # if overwriting with locQOS.ok
str(QOSs)
putFileQOSs(QOSs)
old.putFileQOSs(QOSs)
