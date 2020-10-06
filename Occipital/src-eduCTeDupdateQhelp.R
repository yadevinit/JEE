### --- Iterate and develop the following code (including specifications) across PDF-filewise QOSs:
updateQOS <- function(till.imetaYPq, QOSs1c.ocr.csv, QOSs1c.ocr.rds){ # till specific index into metaYPq.
  stopifnot(till.imetaYPq >= 4)
  qoss <- c() # init
  if(till.imetaYPq >= 4){
    qoss.csv <- updateQOS.fromcsv(QOSs1c.ocr.csv)
    argRowNums.csv.1to4 <- c(1:1961)
    qcsv.out <- qoss.csv[argRowNums.csv.1to4, 1]
    qoss <- c(qoss, qcsv.out)
  } # else continue
  if(till.imetaYPq >= 5){ # Y2014P1:
    zeroRowNum.rds <- 337
    argRowNums.rds <- c((zeroRowNum.rds + 1) : 449) # 656, 757, 848, 965, . list(f5=c(338:449), f6=c())
    qrds.in <- QOSs1c.ocr.rds[argRowNums.rds,] # was: [...,1]
    # print(headCharVec(qrds.in))
    # qoss.rds <- updateQOS.fromrds(qoss.in)
    qrds.out <- getAllQOS(absoluteRowNums=argRowNums.rds, zeroRowNum=zeroRowNum.rds, QOSs1c.ocr.rds, metaYP="Y2014P1")
    # print(headCharVec(qrds.out))
    qoss <- c(qoss, qrds.out)
  } # else continue
  if(till.imetaYPq >= 6){ # Y2014P2:
    zeroRowNum.rds <- 449
    argRowNums.rds <- c((zeroRowNum.rds + 1) : 656)
    qrds.in <- QOSs1c.ocr.rds[argRowNums.rds,]
    print(headCharVec(qrds.in))
    qrds.out <- getAllQOS(absoluteRowNums=argRowNums.rds, zeroRowNum=zeroRowNum.rds, QOSs1c.ocr.rds, metaYP="Y2014P2")
    print(headCharVec(qrds.out))
    qoss <- c(qoss, qrds.out)
  } # else continue

# stop()
  return(qoss)
}
Y2014P2.getNextQOS <- function(chrrNum, relativeRowNum, zeroRowNum, qoss.upd, metaYP){
  # print(paste("called", chrrNum)) # diagnostic troubleshooting.
  rowNum <- (relativeRowNum + zeroRowNum); qosslice <- qoss.upd
  nxtq <- switch(chrrNum,
    r1 = ans <- h1to17 <- myCollapseQ(rowNum, 18, qosslice), # merged header.
    r19 = ans <- q1 <- qosslice[rowNum,],
    r20 = ans <- q2 <- qosslice[rowNum,],
    r21 = ans <- q3 <- qosslice[rowNum,],
    r22 = ans <- q4 <- qosslice[rowNum,],
    r23 = ans <- q5 <- qosslice[rowNum,],
    r24 = ans <- q6 <- qosslice[rowNum,],
    r25 = ans <- q7 <- qosslice[rowNum,],
    r26 = ans <- q8 <- qosslice[rowNum,],
    r27 = ans <- q9 <- qosslice[rowNum,],
    r28 = ans <- q10 <- myCollapseQ(rowNum, 2, qosslice),
    r30 = ans <- q11 <- qosslice[rowNum,],
    r31 = ans <- q12 <- qosslice[rowNum,],
    r32 = ans <- q13 <- qosslice[rowNum,],
    r33 = ans <- q14 <- qosslice[rowNum,],
    r34 = ans <- q15 <- qosslice[rowNum,],
    r35 = ans <- q16 <- qosslice[rowNum,],
    r36 = ans <- q17 <- myCollapseQ(rowNum, 9, qosslice),
    r45 = ans <- q18 <- myCollapseQ(rowNum, 9, qosslice),
    r54 = ans <- q19 <- myCollapseQ(rowNum, 6, qosslice),
    r60 = ans <- q20 <- myCollapseQ(rowNum, 6, qosslice),
    r66 = ans <- q21 <- qosslice[rowNum,],
    r67 = ans <- q22 <- qosslice[rowNum,],
    r68 = ans <- q23 <- myCollapseQ(rowNum, 2, qosslice),
    r70 = ans <- q24 <- qosslice[rowNum,],
    r71 = ans <- q25 <- qosslice[rowNum,],
    r72 = ans <- q26 <- qosslice[rowNum,],
    r73 = ans <- q27 <- qosslice[rowNum,],
    r74 = ans <- q28 <- myCollapseQ(rowNum, 5, qosslice),
    r79 = ans <- q29 <- qosslice[rowNum,],
    r80 = ans <- q30 <- qosslice[rowNum,],
    r81 = ans <- q31 <- qosslice[rowNum,],
    r82 = ans <- q32 <- myCollapseQ(rowNum, 13, qosslice),
    r95 = ans <- q33 <- qosslice[rowNum,],
    r96 = ans <- q34 <- qosslice[rowNum,],
    r97 = ans <- q35 <- qosslice[rowNum,],
    r98 = {
      q36start <- qosslice[rowNum,]
      q36end.q37start <- unlist(strsplit(qosslice[rowNum+1,], split="3/[.]"))
      stopifnot(length(q36end.q37start) == 2)
      q36 <- paste0(q36start, q36end.q37start[1])
      q37start <- paste0("37.", q36end.q37start[2])
      q37end <- myCollapseQ(rowNum+2, 8, qosslice)
      q37 <- paste0(q37start, q37end)
      ans <- c(q36, q37)
    },
    r108 = ans <- q38 <- myCollapseQ(rowNum, 9, qosslice),
    r117 = ans <- q39 <- myCollapseQ(rowNum, 11, qosslice),
    r128 = ans <- q40 <- myCollapseQ(rowNum, 9, qosslice),
    r137 = ans <- q41 <- qosslice[rowNum,],
    r138 = ans <- q42 <- myCollapseQ(rowNum, 3, qosslice),
    r141 = ans <- q43 <- myCollapseQ(rowNum, 2, qosslice),
    r143 = ans <- q44 <- qosslice[rowNum,],
    r144 = ans <- q45 <- myCollapseQ(rowNum, 4, qosslice),
    r148 = ans <- q46 <- myCollapseQ(rowNum, 3, qosslice),
    r151 = ans <- q47 <- myCollapseQ(rowNum, 2, qosslice),
    r153 = ans <- q48 <- myCollapseQ(rowNum, 4, qosslice),
    r157 = ans <- q49 <- myCollapseQ(rowNum, 4, qosslice),
    r161 = ans <- q50 <- myCollapseQ(rowNum, 3, qosslice),
    r164 = ans <- q51 <- myCollapseQ(rowNum, 2, qosslice),
    r166 = ans <- q52 <- sub(pattern="^32", replacement="52", x=myCollapseQ(rowNum, 3, qosslice)),
    r169 = ans <- q53 <- qosslice[rowNum,],
    r170 = ans <- q54 <- qosslice[rowNum,],
    r171 = ans <- q55 <- qosslice[rowNum,],
    r172 = ans <- q56 <- myCollapseQ(rowNum, 2, qosslice),
    r174 = ans <- q57 <- myCollapseQ(rowNum, 9, qosslice),
    r183 = ans <- q58 <- myCollapseQ(rowNum, 9, qosslice),
    r192 = ans <- q59 <- myCollapseQ(rowNum, 9, qosslice),
    r201 = ans <- q60 <- myCollapseQ(rowNum, 7, qosslice),
    NA # by default if switched string is unmatched so far.
  )
  return(nxtq)
}
Y2014P1.getNextQOS <- function(chrrNum, relativeRowNum, zeroRowNum, qoss.upd, metaYP){
  # Beware: args row numbers pertain relatively to arg qosslice.
  rowNum <- (relativeRowNum + zeroRowNum); qosslice <- qoss.upd
  nxtq <- switch(chrrNum,
    r1 = ans <- h1to17 <- myCollapseQ(rowNum, 17, qosslice), # merged header.
    r20 = ans <- q1 <- myCollapseQ(rowNum, 2, qosslice),
    r22 = ans <- q2 <- qosslice[rowNum,],
    r23 = ans <- q3 <- qosslice[rowNum,],
    r24 = ans <- q4 <- qosslice[rowNum,],
    r25 = ans <- q5 <- qosslice[rowNum,],
    r26 = ans <- q6 <- qosslice[rowNum,],
    r27 = ans <- q7 <- qosslice[rowNum,],
    r28 = ans <- q8 <- qosslice[rowNum,],
    r29 = ans <- q9 <- myCollapseQ(rowNum, 2, qosslice),
    r31 = ans <- q10 <- myCollapseQ(rowNum, 3, qosslice),
    r34 = ans <- q11 <- qosslice[rowNum,],
    r35 = ans <- q12 <- qosslice[rowNum,],
    r36 = ans <- q13 <- qosslice[rowNum,],
    r37 = ans <- q14 <- myCollapseQ(rowNum, 2, qosslice),
    r39 = ans <- q15 <- qosslice[rowNum,],
    r40 = ans <- q16 <- myCollapseQ(rowNum, 2, qosslice),
    r42 = ans <- q17 <- qosslice[rowNum,],
    r43 = ans <- q18 <- qosslice[rowNum,],
    r44 = ans <- q19 <- qosslice[rowNum,],
    r45 = ans <- q20 <- myCollapseQ(rowNum, 2, qosslice),
    r47 = ans <- q21 <- myCollapseQ(rowNum, 2, qosslice),
    r49 = ans <- q22 <- qosslice[rowNum,],
    r50 = ans <- q23 <- qosslice[rowNum,],
    r51 = ans <- q24 <- qosslice[rowNum,],
    r52 = ans <- q25 <- qosslice[rowNum,],
    r53 = {
      q26.q27 <- unlist(strsplit(qosslice[rowNum,1], split="2/[.]"))
      stopifnot(length(q26.q27) == 2)
      q26 <- q26.q27[1]
      q27 <- paste0("27.", q26.q27[2])
      ans <- c(q26, q27)
    },
    r54 = ans <- q28 <- myCollapseQ(rowNum, 2, qosslice),
    r56 = ans <- q29 <- qosslice[rowNum,],
    r57 = ans <- q30 <- qosslice[rowNum,],
    r58 = ans <- q31 <- qosslice[rowNum,],
    r59 = ans <- q32 <- qosslice[rowNum,],
    r60 = ans <- q33 <- qosslice[rowNum,],
    r61 = ans <- q34 <- qosslice[rowNum,],
    r62 = ans <- q35 <- myCollapseQ(rowNum, 2, qosslice),
    r64 = ans <- q36 <- qosslice[rowNum,],
    r65 = ans <- q37 <- qosslice[rowNum,],
    r66 = ans <- q38 <- qosslice[rowNum,],
    r67 = ans <- q39 <- myCollapseQ(rowNum, 2, qosslice),
    r69 = ans <- q40 <- qosslice[rowNum,],
    r70 = ans <- q41 <- qosslice[rowNum,],
    r71 = ans <- q42 <- myCollapseQ(rowNum, 3, qosslice),
    r74 = ans <- q43 <- myCollapseQ(rowNum, 2, qosslice),
    r76 = ans <- q44 <- qosslice[rowNum,],
    r77 = ans <- q45 <- myCollapseQ(rowNum, 4, qosslice),
    r81 = ans <- q46 <- qosslice[rowNum,],
    r82 = ans <- q47 <- qosslice[rowNum,],
    r83 = ans <- q48 <- qosslice[rowNum,],
    r84 = ans <- q49 <- myCollapseQ(rowNum, 2, qosslice),
    r86 = ans <- q50 <- qosslice[rowNum,],
    r87 = ans <- q51 <- qosslice[rowNum,],
    r88 = ans <- q52 <- qosslice[rowNum,],
    r89 = ans <- q53 <- myCollapseQ(rowNum, 3, qosslice),
    r92 = {
      q54 <- myCollapseQ(rowNum, 4, qosslice)
      q54 <- sub(pattern="^4", replacement="54", x=q54)
      ans <- q54
    },
    r96 = ans <- q55 <- myCollapseQ(rowNum, 3, qosslice),
    r99 = {
      q56 <- myCollapseQ(rowNum, 4, qosslice)
      q56 <- sub(pattern="^36", replacement="56", x=q56)
      ans <- q56
    },
    r103 = ans <- q57 <- myCollapseQ(rowNum, 3, qosslice),
    r106 = ans <- q58 <- qosslice[rowNum,],
    r107 = ans <- q59 <- myCollapseQ(rowNum, 3, qosslice),
    r110 = ans <- q60 <- myCollapseQ(rowNum, 3, qosslice),
    NA # by default.
  )
  return(nxtq)
}
updateQOS.fromrds <- function(qoss){ # Beware: arg is relative slice of QOSs. So, index 1:length(qoss).
stop() # TBD
}
updateQOS.fromcsv <- function(qoss){
  print(date())
  qoss.upd <- qoss
  # Y2012P1:
  ss72  <- strsplit(qoss.upd[72,1], split="44,"); stopifnot(length(ss72[[1]]) == 2)
  qoss.upd[71,1] <- paste0(qoss.upd[71,1], ss72[[1]][1])
  qoss.upd[72,1] <- paste0("44.", ss72[[1]][2])
  qoss.upd[91,1] <- sub("^39\\.", "59\\.", qoss.upd[91,1]) # Coz incorrectly scanned.
  ss94 <- strsplit(qoss.upd[94,1], split="PN Resonance")
  qoss.upd[94,1] <- ss94[[1]][1] # Coz Answer Key was to be dropped.
  # Y2012P2:
  ss122 <- strsplit(qoss.upd[122,1], split="Tt\\."); stopifnot(length(ss122[[1]]) == 2)
  qoss.upd[122,1] <- ss122[[1]][1]
  qoss.upd[123,1] <- paste0("7.", ss122[[1]][2], qoss.upd[123,1])
  ss1700 <- strsplit(qoss.upd[1700,1], split="1[89]\\."); stopifnot(length(ss1700[[1]]) == 3)
  qoss.upd[1700,1] <- ss1700[[1]][1] # Last part of Q17.
  qoss.upd[1702,1] <- paste0("19.", ss1700[[1]][3], qoss.upd[1702,1]) # Q19.
  qoss.upd[1701,1] <- paste0("18.", ss1700[[1]][2], qoss.upd[1701,1]) # Q18.

    # Split qos48.1790 at PfQ. Then qos48.1790 <- qos48/1.
    qos48.1790 <- strsplit(qoss.upd[1790,1], split=cPattern.PfQ); stopifnot(length(qos48.1790[[1]]) == 2)
    qoss.upd[1790,1] <- qos48.1790[[1]][1]
    # Split qos50.1792 at PfQ. Then qos50.1792 <- qos50.1792/1.
    qos50.1792 <- strsplit(qoss.upd[1792,1], split=cPattern.PfQ); stopifnot(length(qos50.1792[[1]]) == 2)
    qoss.upd[1792,1] <- qos50.1792[[1]][1]
    # PrefixPostQnum qos50/2==PfQ.51.52 to qos51.1793 and qos52.1794.
    PfQ.51.52 <- paste0(cPattern.PfQ, " ", qos50.1792[[1]][2])
    qoss.upd[1793,1] <- myPrefixPostQnum(PfQ.51.52, qoss.upd[1793,1])
    qos52.1794 <- myPrefixPostQnum(PfQ.51.52, qoss.upd[1794,1])
    # Then PrefixPostQnum qos48/2==PfQ.49.50 to qos49.1791 and (earlier-updated) qos50.1792.
    PfQ.49.50 <- paste0(cPattern.PfQ, " ", qos48.1790[[1]][2])
    qoss.upd[1791,1] <- myPrefixPostQnum(PfQ.51.52, qoss.upd[1791,1])
    qoss.upd[1792,1] <- myPrefixPostQnum(PfQ.51.52, qoss.upd[1792,1]) # Beware: qos50.1792==strsplit(...)
    # Split qos52.1794 at PfQ. Then qos52.1794 <- qos52.1794/1.
    qos52.1794 <- strsplit(qoss.upd[1794,1], split=cPattern.PfQ); stopifnot(length(qos52.1794[[1]]) == 2)
    qoss.upd[1794,1] <- qos52.1794[[1]][1]
    # PrefixPostQnum qos52/2==PfQ.53.54 to qos53.1796 and qos54.1797.
    PfQ.53.54 <- paste0(cPattern.PfQ, " ", qos52.1794[[1]][2])
    qoss.upd[1796,1] <- myPrefixPostQnum(PfQ.53.54, qoss.upd[1796,1])
    qoss.upd[1797,1] <- myPrefixPostQnum(PfQ.53.54, qoss.upd[1797,1])
    # Suffix Solution53.1798 to qos53.1796. Then empty Solution53.1798.
    qoss.upd[1796,1] <- paste0(qoss.upd[1796,1], qoss.upd[1798,1])
    qoss.upd[1798,1] <- ""
    # Suffix Solution54.1799 to qos54.1797. Then empty Solution54.1799.
    qoss.upd[1797,1] <- paste0(qoss.upd[1797,1], qoss.upd[1799,1])
    qoss.upd[1799,1] <- ""
    # --- end dealing with what was being considered for merge earlier: c(1789:1790), c(1794:1795)
#   ss1794 <- strsplit(qoss.upd[1794,1], split=cPattern.PfQ); stopifnot(length(ss1794[[1]]) == 2)
#   qoss.upd[1794,1] <- ss1794[[1]][1]
#   qoss.upd[1795,1] <- paste0(cPattern.PfQ, ss1794[[1]][2], qoss.upd[1795,1])
# stop()
#   ss1792 <- strsplit(qoss.upd[1792,1], split=cPattern.PfQ); stopifnot(length(ss1794[[1]]) == 2)
#   qoss.upd[1794,1] <- ss1794[[1]][1]
#   qoss.upd[1795,1] <- paste0("Paragraph for Question", ss1794[[1]][2], qoss.upd[1795,1])
  qos60cont <- strsplit(qoss.upd[1806,1], split="PN Resonence"); stopifnot(length(qos60cont[[1]]) == 2)
  qoss.upd[1806,1] <- qos60cont[[1]][1]
  qoss.upd[1807,1] <- paste0(qos60cont[[1]][2], qoss.upd[1807,1])

  # Y2013P1:
  qoss.upd[1809,1] <- paste0(qoss.upd[1809:1813,1], collapse=" ")
  qoss.upd[1810:1813,1] <- ""
  # Missing q1. So, read it in:
  fname <- paste0(cWorkDir, "/Y2013P1q1.txt")
  qoss.upd[1812,1] <- read.table(file=fname, header=FALSE, stringsAsFactors=FALSE)[1,1]
  # 1814 split="3, Two":
  qos1814 <- strsplit(qoss.upd[1814,1], split="[*] 3, Two"); stopifnot(length(qos1814[[1]]) == 2)
  qoss.upd[1813,1] <- paste0(qoss.upd[1813,1], qos1814[[1]][1])
  qoss.upd[1814,1] <- paste0("* 3. Two", qos1814[[1]][2]) # was: "[*] 3, Two"
  # Shift up the QOSs to create space lower:
  qoss.upd[1810:1813,] <- qoss.upd[1811:1814,] # Even after this, 1810 might still be ""
  qoss.upd[1814,1] <- ""
  # Now, 1815 split="Ss." # split="PCM-3\\n""
  qos1815 <- strsplit(qoss.upd[1815,1], split="Ss."); stopifnot(length(qos1815[[1]]) == 2)
  qoss.upd[1814,1] <- qos1815[[1]][1]
  qoss.upd[1815,1] <- paste0("* 5. ", qos1815[[1]][2])
  # Now, prefix missing part of q6:
  qoss.upd[1817,1] <- paste0("* 6. Two non-reactive monoatomic ideal gases have their atomic masses in the ratio 2 : ",
    qoss.upd[1817,1])

  # Y2013P1:
  qoss.upd[1827,1] <- paste0(qoss.upd[1827,1], qoss.upd[1828,1]); qoss.upd[1828,1] <- ""
  qoss.upd[1828:1829,] <- qoss.upd[1829:1830,]; qoss.upd[1830,1] <- "" # Shift up the QOSs to create space lower.
  # 1831 split="19[,] "
  qos.18.19 <- strsplit(qoss.upd[1831,1], split="19[,] "); stopifnot(length(qos.18.19[[1]]) == 2)
  qoss.upd[1830,1] <- qos.18.19[[1]][1]
  qoss.upd[1831,1] <- paste0("19. ", qos.18.19[[1]][2])
  qos.8.9 <- strsplit(qoss.upd[1820,1], split="Q[,] A"); stopifnot(length(qos.8.9[[1]]) == 2)
  qoss.upd[1819,1] <- paste0(qoss.upd[1819,1], qos.8.9[[1]][1])
  qoss.upd[1820,1] <- paste0("9. A", qos.8.9[[1]][2])
  # qos.38.39 1853 split="39[,] "
  qos.38.39 <- strsplit(qoss.upd[1853,1], split="39[,] "); stopifnot(length(qos.38.39[[1]]) == 2)
  qoss.upd[1852,1] <- paste0(qoss.upd[1852,1], qos.38.39[[1]][1])
  qoss.upd[1853,1] <- paste0("39. ", qos.38.39[[1]][2])
  # qos.40.41 1854 split="Al\\. "
  qos.40.41 <- strsplit(qoss.upd[1854,1], split="Al\\. "); stopifnot(length(qos.40.41[[1]]) == 2)
  qoss.upd[1854,1] <- qos.40.41[[1]][1]
  qoss.upd[1854,1] <- sub(pattern="4\\.\\nAO\\. ", replacement="40\\. ", x=qoss.upd[1854,1])
  qoss.upd[1855,1] <- paste0("41. ", qos.40.41[[1]][2], qoss.upd[1855,1])
  qoss.upd[1860,1] <- sub(pattern="1\\. \\.\\n[*]AS\\. ", replacement="* 45\\. ", x=qoss.upd[1860,1])
  # q49 1863 split="*49, "
  qoss.upd[1858,1] <- paste0(qoss.upd[1858:1859,1], collapse=""); qoss.upd[1859,1] <- ""
  qoss.upd[1859:1862,] <- qoss.upd[1860:1863,] # Shift up to create space.
  qos.48.49 <- strsplit(qoss.upd[1863,1], split="*49, "); stopifnot(length(qos.48.49[[1]]) == 2)
  qoss.upd[1862,1] <- qos.48.49[[1]][1]
  qoss.upd[1863,1] <- paste0("* 49. ", qos.48.49[[1]][2])

  # Y2013P2 from 1880:
  # qoss.upd[1880,1] <- paste0(qoss.upd[1880:1888,1], collapse=" "); qoss.upd[1881:1888,1] <- ""
  #   # headers collapsed. 1881 on free to reuse.
  # qos.h.1 <- strsplit(qoss.upd[1889,1], split="[*]], "); stopifnot(length(qos.h.1[[1]]) == 2)
  # qoss.upd[1888,1] <- paste0(qoss.upd[1888,1], qos.h.1[[1]][1]) # Now has header.
  # qoss.upd[1889,1] <- paste0("* 1. ", qos.h.1[[1]][2]) # q1 ready.
  # # merge c(1890:1891) q2:
  # qoss.upd[1890,1] <- paste0(qoss.upd[1890:1891,1], collapse=" "); qoss.upd[1891,1] <- ""
  # # shift 1888:1890 (h q1 q2) up to create space, knowing 1881 on are free to reuse:
  # stopifnot(qoss.upd[1881:1883,] == "")
  # qoss.upd[1881:1883,1] <- qoss.upd[1888:1890,1]; qoss.upd[1888:1890,1] <- "" # 1884 on free now.
  # # next 1892 starts q3, and 1893 is q6:
  # qos.3.4.5 <- strsplit(qoss.upd[1892,1], split="[*]A, "); stopifnot(length(qos.3.4.5[[1]]) == 2)
  # qos.4.5 <- strsplit(qos.3.4.5[[1]][2], split="[*]5, "); stopifnot(length(qos.4.5[[1]]) == 2)
  # # TBD qos.4.5 use!!??
  # qos.6.7 <- strsplit(qoss.upd[1893,1], split="[*]7,"); stopifnot(length(qos.6.7[[1]]) == 2)
  # qoss.upd[1891,1] <- paste0(qoss.upd[1891,1], " ", qos.3.4.5[[1]][1]) # maybe qoss.upd[1891,1]=="" but safer this way.
  # qoss.upd[1892,1] <- qos.6.7[[1]][1]
  # qoss.upd[1893,1] <- paste0("*7.", qos.6.7[[1]][2]) # q7 ready.
  # # shift 1891:1893 up to create space, knowing 1884 on are free to reuse:
  # stopifnot(qoss.upd[1884:1886,] == "")
  # qoss.upd[1884:1886,] <- qoss.upd[1891:1893,]; qoss.upd[1891:1893,] <- "" # now, 1887 on are free to reuse.

  # Y2013P2 from 1880:
  argRowNums <- c(1880:1961)
  allqoss <- getAllQOS(absoluteRowNums=argRowNums, zeroRowNum=1880-1, qoss.upd, # was: qoss.upd
    metaYP="Y2013P2")
  # print(headCharVec(allqoss))
  # print(c(length(qoss.upd[argRowNums,]), length(allqoss)))
  padEmpty <- rep("", times=length(qoss.upd[argRowNums,]) - length(allqoss))
  qoss.upd[argRowNums,1] <- c(allqoss, padEmpty)

  return(qoss.upd)
}
Y2013P2.getNextQOS <- function(chrrNum, relativeRowNum, zeroRowNum, qoss, metaYP){
  # Beware: args row numbers pertain to arg qoss, not necessarily to the output of this function coz
  # rows might get added, modified, and deleted.
  absRowNum <- (relativeRowNum + zeroRowNum)
  nxtq <- switch(chrrNum, # Beware: arg if qoss.upd has already been partly processed in updateQOS().
    r1 = ans <- h1to9 <- myCollapseQ(absRowNum, 9, qoss), # merged header.
    r2 = NA, # header h2.
    r3 = NA,
    r4 = NA,
    r5 = NA,
    r6 = NA,
    r7 = NA,
    r8 = NA,
    r9 = NA, # header h9.
    r10 = {
      h10.q1 <- unlist(strsplit(qoss[absRowNum,1], split="[*]], "))
      stopifnot(length(h10.q1) == 2)
      ans <- c(h10.q1[1], paste0("* 1. ", h10.q1[2])) # (h10, q1) ready.
    },
    r11 = ans <- q2 <- myCollapseQ(absRowNum, 2, qoss), # merged q2.
    r12 = NA, # coz merged into q2 earlier.
    r13 = {
      q3.q4q5 <- unlist(strsplit(qoss[absRowNum,1], split="[*]A, "))
      stopifnot(length(q3.q4q5) == 2)
      q3 <- q3.q4q5[1]
      q4.q5 <- unlist(strsplit(q3.q4q5[2], split="[*]5, "))
      stopifnot(length(q4.q5) == 2)
      q4 <- paste0("*4. ", q4.q5[1])
      q5 <- paste0("*5. ", q4.q5[2])
      ans <- c(q3, q4, q5)
    },
    r14 = {
      q6.q7 <- unlist(strsplit(qoss[absRowNum,1], split="[*]7,"))
      stopifnot(length(q6.q7) == 2)
      ans <- c(q6.q7[1], paste0("*7.", q6.q7[2])) # q 6,7 ready.
    },
    r15 = {
      q8.para910.q9 <- unlist(strsplit(qoss[absRowNum,1], split=cPattern.PfQ))
      stopifnot(length(q8.para910.q9) == 2)
      ans <- c(q8.para910.q9[1], paste0(cPattern.PfQ, q8.para910.q9[2]))
    },
    r16 = {
      ans <- q10 <- myCollapseQ(absRowNum, 2, qoss)
    },
    r17 = NA, # suffixed earlier with r16.
    r19 = {
      ans <- q12 <- myCollapseQ(absRowNum, 2, qoss)
    },
    r20 = NA, # suffixed earlier with r19.
    r26 = { # TBD+1
      q18.q19 <- unlist(strsplit(qoss[absRowNum,1], split="[*]19,"))
      stopifnot(length(q18.q19) == 2)
      ans <- c(q18.q19[1], paste0("*19.", q18.q19[2], qoss[absRowNum+1,1], "2.")) # "2." from r29.
    },
    r27 = NA, # suffixed earlier with r26.
    r28 = NA, # suffixed earlier with r26.
    r35 = ans <- q26 <- sub(pattern="^20", replacement="26", x=qoss[absRowNum,1]),
    r39 = ans <- q30 <- myCollapseQ(absRowNum, 2, qoss),
    r40 = NA, # suffixed earlier with r39.
    r44 = ans <- q34 <- myCollapseQ(absRowNum, 2, qoss),
    r45 = NA, # suffixed earlier with r44.
    r49 = {
      q38.q39 <- unlist(strsplit(qoss[absRowNum,1], split="39,"))
      stopifnot(length(q38.q39) == 2)
      ans <- c(q38.q39[1], paste0("39.", q38.q39[2]))
    },
    r50 = {
      q40.sech.q41 <- unlist(strsplit(qoss[absRowNum,1], split="\\(I"))
      stopifnot(length(q40.sech.q41) == 2)
      q40 <- q40.sech.q41[1]
      q41 <- paste0("41. (I", q40.sech.q41[2])
      ans <- c(q40, q41)
    },
    r51 = ans <- q42 <- myCollapseQ(absRowNum, 2, qoss),
    r52 = NA, # suffixed earlier with r51.
    r54 = {
      q4445 <- sub(pattern="^4", replacement="44", x=qoss[absRowNum,1])
      q44.q45 <- unlist(strsplit(q4445, split="AS, "))
      stopifnot(length(q44.q45) == 2)
      ans <- c(q44.q45[1], paste0("45. ", q44.q45[2]))
    },
    r57 = {
      q48.para4950.q49 <- unlist(strsplit(qoss[absRowNum,1], split=cPattern.PfQ))
      stopifnot(length(q48.para4950.q49) == 2)
      ans <- c(q48.para4950.q49[1], paste0(cPattern.PfQ, q48.para4950.q49[2])) # para4950 is with q49.
    },
    r58 = {
      q50.para5152 <- unlist(strsplit(qoss[absRowNum,1], split=cPattern.PfQ))
      stopifnot(length(q50.para5152) == 2)
      q50 <- q50.para5152[1]
      para5152 <- paste0(cPattern.PfQ, q50.para5152[2])
      q51 <- myCollapseQ(absRowNum+1, 2, qoss) # from following rows.
      q51 <- sub(pattern="^1", replacement="51", x=q51)
      q51 <- myPrefixPostQnum(para5152, q51)
      q52para5354 <- sub(pattern="^2", replacement="52", x=qoss[absRowNum+3,1]) # from following rows.
      q52.para5354 <- unlist(strsplit(q52para5354, split=cPattern.PfQ))
      stopifnot(length(q52.para5354) == 2)
      q52 <- myPrefixPostQnum(para5152, q52.para5354[1])
      para5354 <- paste0(cPattern.PfQ, q52.para5354[2])
      q53 <- sub(pattern="^3", replacement="53", x=qoss[absRowNum+4,1]) # from following rows.
      q53 <- myPrefixPostQnum(para5354, q53)
      q54para5556 <- qoss[absRowNum+5,1] # from following rows.
      q54.para5556 <- unlist(strsplit(q54para5556, split=cPattern.PfQ))
      stopifnot(length(q54.para5556) == 2)
      q54 <- q54.para5556[1]
      q54 <- myPrefixPostQnum(para5354, q54)
      para5556 <- paste0(cPattern.PfQ, q54.para5556[2])
      q55 <- myPrefixPostQnum(para5556, qoss[absRowNum+6,1]) # from following rows.
      q56 <- myPrefixPostQnum(para5556, myCollapseQ(absRowNum+7, 2, qoss)) # from following rows.
      ans <- c(q50,  q51,q52,  q53,q54,  q55,q56)
    },
    r59 = NA, # suffixed earlier with r58.
    r60 = NA, # suffixed earlier with r58.
    r61 = NA, # suffixed earlier with r58.
    r62 = NA, # suffixed earlier with r58.
    r63 = NA, # suffixed earlier with r58.
    r64 = NA, # suffixed earlier with r58.
    r65 = NA, # suffixed earlier with r58.
    r66 = NA, # suffixed earlier with r58.
    r67 = {
      q57 <- myCollapseQ(absRowNum, 4, qoss) # from following rows.
      ans <- q57 <- sub(pattern="^7", replacement="57", x=q57)
    },
    r68 = NA, # suffixed earlier with r67.
    r69 = NA, # suffixed earlier with r67.
    r70 = NA, # suffixed earlier with r67.
    r71 = {
      q58 <- myCollapseQ(absRowNum, 2, qoss) # from following rows.
      ans <- q58 <- sub(pattern="^8", replacement="58", x=q58)
    },
    r72 = NA, # suffixed earlier with r71.
    r73 = ans <- q59 <- myCollapseQ(absRowNum, 8, qoss), # from following rows.
    r74 = NA, # suffixed earlier with r73.
    r75 = NA, # suffixed earlier with r73.
    r76 = NA, # suffixed earlier with r73.
    r77 = NA, # suffixed earlier with r73.
    r78 = NA, # suffixed earlier with r73.
    r79 = NA, # suffixed earlier with r73.
    r80 = NA, # suffixed earlier with r73.
    r81 = ans <- q60 <- myCollapseQ(absRowNum, 2, qoss), # from following rows.
    r82 = NA, # suffixed earlier with r81.

    # default case for relativeRowNum:
    ans <- qoss[absRowNum,1] # QOS as is.
  )
  return(nxtq)
}
