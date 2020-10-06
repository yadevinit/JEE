# fromOCR=TRUE by default:
forOCR <- TRUE # by default
QOSs1c.ocr.rds <- getFiledQOSs()
QOSs1c.ocr.csv <- getFiledQOSs(prefix=paste0(cWorkDir, "/jads-"), fromOCR=TRUE, suffix="-QOSs", ext=".csv")
   # Beware: problematic .csv; prefer .rds.
# was: QOSs1c <- if(TRUE){ QOSs1c.ocr.rds } else { QOSs1c.ocr.csv }

### --- Iterate and develop the following code (including specifications) across PDF-filewise QOSs:
# till.imetaYPq <- 6+6; cYPtill <- "Y2018P2"
  # was: "Y2016P2". till specific index: whether into metaYPq or various files.


stop() # Beware: following row numbers and beyond must be consistent with till.imetaYPq! Else rows from one into another!
qoss <- updateQOS(till.imetaYPq, QOSs1c.ocr.csv, QOSs1c.ocr.rds)
chVec <- qoss; headCharVec(chVec)

# QOSs1c.use <- updateQOS(QOSs1c)
# QOSs1c <- if(till.imetaYPq %in% c(1:4)){ # for datafile indices in vector.
#     QOSs1c.ocr.csv
#   } else {
#     QOSs1c.ocr.rds
#   }
# startRowsTBD <- 1880; stopRowsTBD <- 1961
# chVec <- QOSs1c.use[1:stopRowsTBD, 1]; headCharVec(chVec[startRowsTBD:stopRowsTBD]) # Modify updateQOS() as needed.
# moreChVec(chVec, startRowsTBD:stopRowsTBD, n=350*4)

mergeSpecs.OCR <- list(
  # Y2012P1:
  c(1:17), # Drop Section intro later.
  c(23,24), c(26:28), 30+c(3:6), 30+c(12:13), 30+c(18:19), 30+c(33:34), 30+c(35:36), 30+c(38:39),
  30+c(47:48), 30+c(53:54), 30+c(55:56), 30+c(58:59),
  c(91:93), c(95:99), # Drop Answer Key table later.
  # Y2012P2:
  c(100:115), # Drop Section header later.
  c(117:118), c(123:183), c(184:272),
  c(273:311), # Drop Section header later.
  c(312:460), # Beware: Paragraph for Questions9 and 10, which might need to be replicated for those following Questions.
  c(461:527), c(528:593), # Q10 merge. Repeat earlier Para before Q9?
  c(594:763), # Beware: Paragraph for Questions 11 and 12.
  c(764:842), c(843:926), c(927:1172), # Beware: Paragraph for Questions 13-14.
  c(1173:1264), c(1265:1399), # Q14.
  c(1400:1433), # Section III starts.
  c(1434:1565), # Q15.
  c(1566:1672), c(1673:1700), # Q18,19 were earlier embedded within.
  c(1702:1719), c(1720:1760), # Q19 then Q20
    # Part II Chemistry starts
  c(1787:1788), c(1789:1790), c(1794:1795), c(1797:1799), c(1805:1806),
  c(1807:1808), # Drop later.
  # Y2013P1:
  # c(1809:1811) # Drop later. Use 1812 on for reading in q1 and splits.
  c(1815:1816), c(1824:1825), c(1847:1848), c(1850:1851),
  c(1872:1873), c(1874:1876), c(1877:1878),
  # Y2013P2 from 1880:
  c(1880:1881), # header
  c(1942:1961) # empty
  # Y2014P1:
  # Y2014P2:
  # Y2016P1:
  # Y2016P2:
  # Y2017P1:
  # Y2017P2:
  # Y2018P1:
  # Y2018P2:
)
mcv <- mergeCharVec(chVec, mergeSpecs.OCR); headCharVec(mcv)
# mcv.bak <- mcv # backup if needed.
mcv <- updateMergedCharVec(mcv); headCharVec(mcv)

metaYPq <- list( # Beware q here includes question-related media eg was including Paragraphs for (following) Questions.
  # Beware: mcv index here.
  Y2012P1=list(P=c(1:20), C=c(21:40), M=c(41:60), iMerged=c(2:61))
    # Drop iMerged==c(1,62) eg when mapping outJEEinsight.
  ,Y2012P2=list(P=c(1:20), # Was: coz +3 Paragraphs for Questions worth retaining.
    C=c(21:40), M=c(41:60), iMerged=setdiff(c(64:71,73:81,83:88, 89:108, 109:128), c(73,76,79)))
    # Drop iMerged=c(63,72,  73,76,79,  82,129).
  ,Y2013P1=list(P=c(1:20), C=c(21:40), M=c(41:60), iMerged=c(132:186, 188:192)) # Drop iMerged=c(130:131, 187).
  ,Y2013P2=list(P=c(1:20), C=c(21:40), M=c(41:60), iMerged=setdiff(193:(193+62-1), c(193, 254)))
  ,Y2014P1=list(P=c(1:20), C=c(21:40), M=c(41:60), iMerged=setdiff(255:315, c(255)))
  ,Y2014P2=list(P=c(1:20), C=c(21:40), M=c(41:60), iMerged=setdiff(316:376, c(316)))
  ,Y2016P1=list(P=c(1:18), C=c(19:36), M=c(37:54), iMerged=setdiff(377:432, c(377,432))) # Drop headers on ends.
  ,Y2016P2=list(P=c(1:18), C=c(19:36), M=c(37:54), iMerged=setdiff(433:(433+54+2-1), c(433,488))) # Drop headers on ends.
  ,Y2017P1=list(P=c(1:18), C=c(19:36), M=c(37:54), iMerged=setdiff(489:(489+54-1+3), c(489,489+19,489+56)))
  ,Y2017P2=list(P=c(1:18), C=c(19:36), M=c(37:54), iMerged=setdiff(546:(600), c(546)))
  ,Y2018P1=list(P=c(1:18), C=c(19:36), M=c(37:54), iMerged=setdiff(601:(656), c(601,638)))
  ,Y2018P2=list(P=c(1:18), C=c(19:36), M=c(37:54), iMerged=setdiff(657:710, c())) # or c(0).
)

  # Now, only after data is ready can we start writing to file:
  mmsqoss <- multMetaSelectedQOSs(till.imetaYPq, metaYPq, mcv)
  glimpse(mmsqoss)
  fname.mm <- paste0(cWorkDir, "/jads-", ifelse(forOCR, "ocr", "nocr"), "-till",
    names(metaYPq)[till.imetaYPq], ".rds")
  saveRDS(mmsqoss, file=fname.mm) # preferable over save() coz readRDS() "functional" and not attached
    # to earlier object names. default: ascii=FALSE which might be more portable but less efficient.
    # mmsqoss2 <- readRDS(file=fname.mm); identical(mmsqoss2, mmsqoss)
  fname.mm.tmp2read <- paste0(cWorkDir, "/tmpQOSjads-", ifelse(forOCR, "ocr", "nocr"), "-till",
    names(metaYPq)[till.imetaYPq], ".txt")
  mmsqoss.tmp2read <- as.data.frame(unlist(mmsqoss), stringsAsFactors=FALSE)
  colnames(mmsqoss.tmp2read) <- "QOS2020Mar12.1220" # was: "QOS2020Feb19.1025"
  write.table(mmsqoss.tmp2read, file=fname.mm.tmp2read, row.names=TRUE) # col.names=TRUE.
    # This is for human reading and mapping to other meta data eg from Project JEEinsight.

  fname.meta <- paste0(cWorkDir, "/jads-", ifelse(forOCR, "ocr", "nocr"), "-till",
    names(metaYPq)[till.imetaYPq], "metaVars", ".csv")
  mvarsdf <- metaToVars(metaYPq)
  write.csv(mvarsdf, file=fname.meta, row.names=FALSE) # col.names=TRUE.
  # saveRDS(mvarsdf, file=fname.meta)
  # was: saveRDS(metaYPq, file=fname.meta)
