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
    # print(headCharVec(qrds.in))
    qrds.out <- getAllQOS(absoluteRowNums=argRowNums.rds, zeroRowNum=zeroRowNum.rds, QOSs1c.ocr.rds, metaYP="Y2014P2")
    # print(headCharVec(qrds.out))
    qoss <- c(qoss, qrds.out)
  } # else continue
  if(till.imetaYPq >= 7){ # Y2016P1:
    zeroRowNum.rds <- 656+0
    argRowNums.rds <- c((zeroRowNum.rds + 1) : (656+101))
    qrds.in <- QOSs1c.ocr.rds[argRowNums.rds,]
    # print(headCharVec(qrds.in))
    # qoss.upd <- qoss; zeroRowNum=zeroRowNum.rds; metaYP="Y2016P1"
    qrds.out <- getAllQOS(absoluteRowNums=argRowNums.rds, zeroRowNum=zeroRowNum.rds, QOSs1c.ocr.rds, metaYP="Y2016P1")
    # print(headCharVec(qrds.out))
    qoss <- c(qoss, qrds.out)
  } # else continue
  if(till.imetaYPq >= 8){ # Y2016P2:
    zeroRowNum.rds <- 757+0
    argRowNums.rds <- c((zeroRowNum.rds + 1) : (757+91))
    qrds.in <- QOSs1c.ocr.rds[argRowNums.rds,]
    # print(headCharVec(qrds.in))
    qrds.out <- getAllQOS(absoluteRowNums=argRowNums.rds, zeroRowNum=zeroRowNum.rds, QOSs1c.ocr.rds, metaYP="Y2016P2")
    # print(headCharVec(qrds.out))
    qoss <- c(qoss, qrds.out)
  } # else continue
  if(till.imetaYPq >= 9){ # Y2017P1:
    zeroRowNum.rds <- 757+91+0 # =848
    argRowNums.rds <- c((zeroRowNum.rds + 1) : (757+91+117)) # =965
    qrds.in <- QOSs1c.ocr.rds[argRowNums.rds,]
    # print(headCharVec(qrds.in))
    qrds.out <- getAllQOS(absoluteRowNums=argRowNums.rds, zeroRowNum=zeroRowNum.rds, QOSs1c.ocr.rds, metaYP="Y2017P1")
    # print(headCharVec(qrds.out))
    qoss <- c(qoss, qrds.out)
  } # else continue
  if(till.imetaYPq >= 10){ # Y2017P2:
    zeroRowNum.rds <- 965+0 #
    argRowNums.rds <- c((zeroRowNum.rds + 1) : (965+90)) # =?
    qrds.in <- QOSs1c.ocr.rds[argRowNums.rds,]
    # print(headCharVec(qrds.in))
    qrds.out <- getAllQOS(absoluteRowNums=argRowNums.rds, zeroRowNum=zeroRowNum.rds, QOSs1c.ocr.rds, metaYP="Y2017P2")
    # print(headCharVec(qrds.out))
    qoss <- c(qoss, qrds.out)
  } # else continue
  if(till.imetaYPq >= 11){ # Y2018P1:
    zeroRowNum.rds <- 965+90 # =1055
    argRowNums.rds <- c((zeroRowNum.rds + 1) : (965+90+81)) # =1136
    qrds.in <- QOSs1c.ocr.rds[argRowNums.rds,]
    # print(headCharVec(qrds.in))
    qrds.out <- getAllQOS(absoluteRowNums=argRowNums.rds, zeroRowNum=zeroRowNum.rds, QOSs1c.ocr.rds, metaYP="Y2018P1")
    # print(headCharVec(qrds.out))
    qoss <- c(qoss, qrds.out)
  } # else continue
  if(till.imetaYPq >= 12){ # Y2018P2:
    zeroRowNum.rds <- 1136+0
    argRowNums.rds <- c((zeroRowNum.rds + 1) : (1136+97)) # =?
    qrds.in <- QOSs1c.ocr.rds[argRowNums.rds,]
    # print(headCharVec(qrds.in))
    qrds.out <- getAllQOS(absoluteRowNums=argRowNums.rds, zeroRowNum=zeroRowNum.rds, QOSs1c.ocr.rds, metaYP="Y2018P2")
    # print(headCharVec(qrds.out))
    qoss <- c(qoss, qrds.out)
  } # else continue
  return(qoss)
}
Y2018P2.getNextQOS <- function(chrrNum, relativeRowNum, zeroRowNum, qoss.upd, metaYP){
  print(paste("called", chrrNum)) # diagnostic troubleshooting.
  rowNum <- (relativeRowNum + zeroRowNum); qosslice <- qoss.upd
  nxtq <- switch(chrrNum,
    r1 = ans <- q1P <- qosslice[rowNum,],
    r2 = ans <- q2P <- qosslice[rowNum,],
    r3 = ans <- q3P <- qosslice[rowNum,],
    r4 = ans <- q4P <- qosslice[rowNum,],
    r5 = ans <- q5P <- qosslice[rowNum,],
    r6 = ans <- q6P <- qosslice[rowNum,],
    r7 = ans <- q7P <- myCollapseQ(rowNum, 2, qosslice),
    r9 = ans <- q8P <- qosslice[rowNum,],
    r10 = ans <- q9P <- qosslice[rowNum,],
    r11 = ans <- q10P <- qosslice[rowNum,],
    r12 = ans <- q11P <- myCollapseQ(rowNum, 2, qosslice),
    r14 = ans <- q12P <- qosslice[rowNum,],
    r15 = ans <- q13P <- qosslice[rowNum,],
    # No need to differently extract q14P:q18P from r16:r34. That's coz each QOS is self-contained.
    r16 = ans <- q14P <- qosslice[rowNum,],
    r17 = ans <- q15P <- myCollapseQ(rowNum, 5, qosslice),
    r22 = ans <- q16P <- myCollapseQ(rowNum, 3, qosslice),
    r25 = ans <- q17P <- myCollapseQ(rowNum, 5, qosslice),
    r30 = ans <- q18P <- myCollapseQ(rowNum, 5, qosslice),
    r35 = ans <- q1C <- qosslice[rowNum,],
    r36 = ans <- q2C <- qosslice[rowNum,],
    r37 = ans <- q3C <- qosslice[rowNum,],
    r38 = ans <- q4C <- qosslice[rowNum,],
    r39 = { # q5C:q6C from r39:r40.
      patnQ6C <- "\nQ6 " # Missed a [.] and might fail other qNum pattern matching!
      q5C.q6CsansPatn <- unlist(strsplit(myCollapseQ(rowNum, 2, qosslice), split=patnQ6C))
      stopifnot(length(q5C.q6CsansPatn) == 2)
      q5C <- q5C.q6CsansPatn[1]
      q6C <- paste0("Q.6", q5C.q6CsansPatn[2])
      ans <- c(q5C, q6C)
    },
    r41 = ans <- q7C <- qosslice[rowNum,],
    r42 = ans <- q8C <- qosslice[rowNum,],
    r43 = ans <- q9C <- qosslice[rowNum,],
    r44 = ans <- q10C <- qosslice[rowNum,],
    r45 = ans <- q11C <- myCollapseQ(rowNum, 2, qosslice),
    r47 = ans <- q12C <- myCollapseQ(rowNum, 2, qosslice),
    r49 = ans <- q13C <- qosslice[rowNum,],
    # No need to differently extract q14C:q18C from r50:r71.
    r50 = ans <- q14C <- qosslice[rowNum,],
    r51 = ans <- q15C <- myCollapseQ(rowNum, 5, qosslice),
    r56 = ans <- q16C <- myCollapseQ(rowNum, 4, qosslice),
    r60 = ans <- q17C <- myCollapseQ(rowNum, 5, qosslice),
    r65 = ans <- q18C <- myCollapseQ(rowNum, 7, qosslice),
    r72 = ans <- q1M <- qosslice[rowNum,],
    r73 = ans <- q2M <- qosslice[rowNum,],
    r74 = ans <- q3M <- myCollapseQ(rowNum, 2, qosslice),
    r76 = ans <- q4M <- myCollapseQ(rowNum, 2, qosslice),
    r78 = ans <- q5M <- qosslice[rowNum,],
    r79 = ans <- q6M <- qosslice[rowNum,],
    r80 = ans <- q7M <- qosslice[rowNum,],
    r81 = ans <- q8M <- qosslice[rowNum,],
    r82 = ans <- q9M <- qosslice[rowNum,],
    r83 = ans <- q10M <- qosslice[rowNum,],
    r84 = ans <- q11M <- myCollapseQ(rowNum, 2, qosslice),
    r86 = ans <- q12M <- myCollapseQ(rowNum, 2, qosslice),
    r88 = ans <- q13M <- qosslice[rowNum,],
    # No need to differently extract q14M:q18M from r89:r97.
    r89 = ans <- q14M <- qosslice[rowNum,],
    r90 = ans <- q15M <- myCollapseQ(rowNum, 3, qosslice),
    r93 = ans <- q16M <- myCollapseQ(rowNum, 2, qosslice),
    r95 = ans <- q17M <- myCollapseQ(rowNum, 2, qosslice),
    r97 = ans <- q18M <- qosslice[rowNum,],
    NA # by default if switched string is unmatched so far.
  )
  return(nxtq)
}
Y2018P1.getNextQOS <- function(chrrNum, relativeRowNum, zeroRowNum, qoss.upd, metaYP){
  # print(paste("called", chrrNum)) # diagnostic troubleshooting.
  rowNum <- (relativeRowNum + zeroRowNum); qosslice <- qoss.upd
  nxtq <- switch(chrrNum,
    r1 = ans <- h1to5 <- myCollapseQ(rowNum, 5, qosslice), # merged header.
    r6 = ans <- q1 <- qosslice[rowNum,],
    r7 = ans <- q2 <- qosslice[rowNum,],
    r8 = ans <- q3 <- myCollapseQ(rowNum, 2, qosslice),
    r10 = ans <- q4 <- qosslice[rowNum,],
    r11 = ans <- q5 <- qosslice[rowNum,],
    r12 = ans <- q6 <- qosslice[rowNum,],
    r13 = ans <- q7 <- qosslice[rowNum,],
    r14 = ans <- q8 <- qosslice[rowNum,],
    r15 = ans <- q9 <- myCollapseQ(rowNum, 2, qosslice),
    r17 = ans <- q10 <- qosslice[rowNum,],
    r18 = ans <- q11 <- qosslice[rowNum,],
    r19 = ans <- q12 <- qosslice[rowNum,],
    r20 = ans <- q13 <- qosslice[rowNum,],
    # was:
    # r21 = ans <- q14 <- qosslice[rowNum,],
    # r22 = ans <- q15 <- qosslice[rowNum,],
    # r23 = ans <- q16 <- myCollapseQ(rowNum, 2, qosslice),
    # r25 = ans <- q17 <- myCollapseQ(rowNum, 2, qosslice),
    r21 = {
      # Prefer coding as per r72 block. In 2018 papers, patnPara precedes each applicable question.
      # From r21, retain q14, drop Section header, and move paraQ1516 into subsequent 2 questions q15,q16.
      # Similarly from r23:r24, retain q16 (with no Section header) and move paraQ1718 into subsequent 2 questions
      # q17,q18.
      patnSec <- "\nSECTION "; patnPara <- "\nPARAGRAPH"
      q14.secParaQ1516 <- unlist(strsplit(qosslice[rowNum+0,], split=patnSec)) # r21
      stopifnot(length(q14.secParaQ1516) == 2)
      sec.paraQ1516 <- unlist(strsplit(q14.secParaQ1516[2], split=patnPara))
      stopifnot(length(sec.paraQ1516) == 2)
      paraQ1516 <- paste0(patnPara, sec.paraQ1516[2]) # Drop Section header sec.paraQ1516[1].

      q16.paraQ1718 <- unlist(strsplit(myCollapseQ(rowNum+2, 2, qosslice), split=patnPara)) # r23:r24
      stopifnot(length(q16.paraQ1718) == 2)
      paraQ1718 <- paste0(patnPara, q16.paraQ1718[2])

      patnChem <- "\nPART II: CHEMISTRY\nSECTION 1"
      q18.footerChemHdr <- unlist(strsplit(myCollapseQ(rowNum+6, 2, qosslice), split=patnChem)) # r27:r28
      stopifnot(length(q18.footerChemHdr) == 2)
      q14 <- q14.secParaQ1516[1]
      q15 <- myPrefixPostQnum(paraQ1516, qosslice[rowNum+1,]) # r22
      q16 <- myPrefixPostQnum(paraQ1516, q16.paraQ1718[1])
      q17 <- myPrefixPostQnum(paraQ1718, myCollapseQ(rowNum+4, 2, qosslice)) # r25:r26
      q18 <- myPrefixPostQnum(paraQ1718, q18.footerChemHdr[1])
        # Drop/ignore subsequent footer and Chem header q18.footerChemHdr[2].
      ans <- c(q14,  q15,q16,  q17,q18)
    },
    r29 = ans <- q19 <- qosslice[rowNum,],
    r30 = ans <- q20 <- qosslice[rowNum,],
    r31 = ans <- q21 <- myCollapseQ(rowNum, 3, qosslice),
    r34 = ans <- q22 <- qosslice[rowNum,],
    r35 = ans <- q23 <- qosslice[rowNum,],
    r36 = ans <- q24 <- qosslice[rowNum,],
    r37 = ans <- q25 <- qosslice[rowNum,],
    r38 = ans <- q26 <- qosslice[rowNum,],
    r39 = ans <- q27 <- qosslice[rowNum,],
    r40 = ans <- q28 <- qosslice[rowNum,],
    r41 = ans <- q29 <- qosslice[rowNum,],
    r42 = ans <- q30 <- myCollapseQ(rowNum, 2, qosslice),
    r44 = ans <- q31 <- qosslice[rowNum,],
    r45 = { # r45:r49 appears to be q32.
      # Prefer coding as per r72 block. In 2018 papers, patnPara precedes each applicable question.
      patnSec <- "\nSECTION "; patnPara <- "\nPARAGRAPH"
      q32.secParaQ18plus1516 <- unlist(strsplit(myCollapseQ(rowNum+0, 5, qosslice), split=patnSec)) # r45:r49
      stopifnot(length(q32.secParaQ18plus1516) == 2)
      sec.paraQ18plus1516 <- unlist(strsplit(q32.secParaQ18plus1516[2], split=patnPara))
      stopifnot(length(sec.paraQ18plus1516) == 2)
      paraQ18plus1516 <- paste0(patnPara, sec.paraQ18plus1516[2]) # Ignore Section sec.paraQ18plus1516[1].

      q32 <- q32.secParaQ18plus1516[1]
      q33 <- myPrefixPostQnum(paraQ18plus1516, qosslice[rowNum+5,]) # r50
      q34.paraQ18plus1718 <- unlist(strsplit(qosslice[rowNum+6,], split=patnPara)) # r51
      stopifnot(length(q34.paraQ18plus1718) == 2)
      q34 <- myPrefixPostQnum(paraQ18plus1516, q34.paraQ18plus1718[1])
      paraQ18plus1718 <- paste0(patnPara, q34.paraQ18plus1718[2])
      q35 <- myPrefixPostQnum(paraQ18plus1718, qosslice[rowNum+7,]) # r52

      patnSecM <- "\nPART TIT: MATHEMATICS\nSECTION 1 "
      q36.secM <- unlist(strsplit(qosslice[rowNum+8,], split=patnSecM)) # r53
      q36 <- myPrefixPostQnum(paraQ18plus1718, q36.secM[1])
      secM <- paste0(patnSecM, q36.secM[2], qosslice[rowNum+9,]) # r54
      ans <- c(q32,  q33,q34,  q35,q36, secM) # 18plus c(q14,  q15,q16,  q17,q18)
    },
    r55 = ans <- q37 <- myCollapseQ(rowNum, 3, qosslice),
    r58 = ans <- q38 <- myCollapseQ(rowNum, 2, qosslice),
    r60 = ans <- q39 <- qosslice[rowNum,],
    r61 = ans <- q40 <- qosslice[rowNum,],
    r62 = ans <- q41 <- qosslice[rowNum,],
    r63 = ans <- q42 <- qosslice[rowNum,],
    r64 = ans <- q43 <- qosslice[rowNum,],
    r65 = ans <- q44 <- qosslice[rowNum,],
    r66 = ans <- q45 <- myCollapseQ(rowNum, 2, qosslice),
    r68 = ans <- q46 <- qosslice[rowNum,],
    r69 = ans <- q47 <- qosslice[rowNum,],
    r70 = ans <- q48 <- qosslice[rowNum,],
    r71 = ans <- q49 <- qosslice[rowNum,],

    r72 = {

q54sansPara <- "Q.18 For i = 1, 2, 3, 4, let Ti denote the event that the students Si and Si+1 do NOT sit adjacent to each other on
the day of the examination. Then, the probability of the event T1  T2  T3  T4 is
(A) 1
15
(B) 1
10
(C) 7
60
(D) 1
5
Sol. C
1 3 5 2 4
2 4 1 3 5
3 5 1 4 2
3 5 2 4 1
4 1 3 5 2
4 2 5 1 3
5 2 4 1 3
S S S S S
S S S S S
S S S S S
S S S S S
S S S S S
S S S S S
S S S S S
Same number of ways in reverse order
   
 
n E P E 7 2 7
n S 5! 60"

      # In 2018 papers, patnPara precedes each applicable question.
      patnPara <- "\nPARAGRAPH"
      q50.paraQ51 <- unlist(strsplit(myCollapseQ(rowNum+0, 3, qosslice), split=patnPara)) # r72:r74
      stopifnot(length(q50.paraQ51) == 2)
      q51.paraQ52 <- unlist(strsplit(myCollapseQ(rowNum+3, 3, qosslice), split=patnPara)) # r75:r77
      stopifnot(length(q51.paraQ52) == 2)
      q52.paraQ53 <- unlist(strsplit(myCollapseQ(rowNum+6, 2, qosslice), split=patnPara)) # r78:r79
      stopifnot(length(q52.paraQ53) == 2)
      # For Maths: r80 has q53paraQ5354.1of2. r81 has paraQ5354.2of2. q54 is MISSED!!
      q53.paraQ54 <- unlist(strsplit(myCollapseQ(rowNum+8, 2, qosslice), split=patnPara)) # r80:r81
      stopifnot(length(q53.paraQ54) == 2)

      q50 <- q50.paraQ51[1]
      q51 <- myPrefixPostQnum(paste0(patnPara, q50.paraQ51[2]), q51.paraQ52[1])
      q52 <- myPrefixPostQnum(paste0(patnPara, q51.paraQ52[2]), q52.paraQ53[1])
      q53 <- myPrefixPostQnum(paste0(patnPara, q52.paraQ53[2]), q53.paraQ54[1])
      q54 <- myPrefixPostQnum(paste0(patnPara, q53.paraQ54[2]), q54sansPara)
      ans <- c(q50,  q51,q52,  q53,q54)
    },
    NA # by default if switched string is unmatched so far.
  )
  return(nxtq)
}
Y2017P2.getNextQOS <- function(chrrNum, relativeRowNum, zeroRowNum, qoss.upd, metaYP){
  # print(paste("called", chrrNum)) # diagnostic troubleshooting.
  rowNum <- (relativeRowNum + zeroRowNum); qosslice <- qoss.upd
  nxtq <- switch(chrrNum,
    r1 = ans <- h1to16 <- myCollapseQ(rowNum, 16, qosslice), # merged header.
    r17 = ans <- q1 <- qosslice[rowNum,],
    r18 = ans <- q2 <- qosslice[rowNum,],
    r19 = ans <- q3 <- qosslice[rowNum,],
    r20 = ans <- q4 <- qosslice[rowNum,],
    r21 = ans <- q5 <- qosslice[rowNum,],
    r22 = ans <- q6 <- qosslice[rowNum,],
    r23 = ans <- q7 <- qosslice[rowNum,],
    r24 = {
      patn <- "9, A wheel"
      q8.q9 <- unlist(strsplit(qosslice[rowNum,], split=patn)); stopifnot(length(q8.q9) == 2)
      q8 <- q8.q9[1]; q9 <- paste0(patn, q8.q9[2])
      ans <- c(q8, q9)
    },
    r25 = ans <- q10 <- myCollapseQ(rowNum, 3, qosslice),
    r28 = ans <- q11 <- qosslice[rowNum,],
    r29 = ans <- q12 <- qosslice[rowNum,],
    r30 = ans <- q13 <- myCollapseQ(rowNum, 2, qosslice),

    # was:
    # r32 = ans <- q14 <- myCollapseQ(rowNum, 4, qosslice),
    # r36 = ans <- q15 <- qosslice[rowNum,],
    # r37 = ans <- q16 <- myCollapseQ(rowNum, 3, qosslice),
    # r40 = ans <- q17 <- qosslice[rowNum,],
    # r41 = ans <- q18 <- qosslice[rowNum,],
    r32 = {
      patnSec <- "\nSECTION "; patnPara <- "\nPARAGRAPH"
      # From r32 and r38, move para (patnPara on) to following 2 Qs respectively.
      q14.sectionParaQ1516 <- unlist(strsplit(myCollapseQ(rowNum+0, 4, qosslice), split=patnSec)) # r32
      stopifnot(length(q14.sectionParaQ1516) == 2)
      q14 <- q14.sectionParaQ1516[1]
      section.paraQ1516 <- unlist(strsplit(q14.sectionParaQ1516[2], split=patnPara))
      stopifnot(length(section.paraQ1516) == 2)
      paraQ1516 <- paste0(patnPara, section.paraQ1516[2]) # Drop section in section.paraQ1516[1].

      q15 <- qosslice[rowNum+4,]; q15 <- myPrefixPostQnum(paraQ1516, q15) # r36
      q16sansPara.paraQ1718 <- unlist(strsplit(myCollapseQ(rowNum+5, 3, qosslice), split=patnPara)) # r37:r39
      stopifnot(length(q16sansPara.paraQ1718) == 2)
      q16sansPara <- q16sansPara.paraQ1718[1]; paraQ1718 <- paste0(patnPara, q16sansPara.paraQ1718[2])

      q16 <- myPrefixPostQnum(paraQ1516, q16sansPara)
      q17 <- myPrefixPostQnum(paraQ1718, qosslice[rowNum+8,]) # r40
      q18 <- myPrefixPostQnum(paraQ1718, qosslice[rowNum+9,]) # r41
      ans <- c(q14,  q15,q16,  q17,q18)
    },
    r42 = ans <- q19 <- qosslice[rowNum,],
    r43 = ans <- q20 <- myCollapseQ(rowNum, 3, qosslice),
    r46 = ans <- q21 <- qosslice[rowNum,],
    r47 = ans <- q22 <- qosslice[rowNum,],
    r48 = ans <- q23 <- qosslice[rowNum,],
    r49 = ans <- q24 <- qosslice[rowNum,],
    r50 = ans <- q25 <- myCollapseQ(rowNum, 2, qosslice),
    r52 = ans <- q26 <- qosslice[rowNum,],
    r53 = ans <- q27 <- myCollapseQ(rowNum, 2, qosslice),
    r55 = ans <- q28 <- myCollapseQ(rowNum, 3, qosslice),
    r58 = ans <- q29 <- qosslice[rowNum,],
    r59 = ans <- q30 <- qosslice[rowNum,],
    r60 = ans <- q31 <- qosslice[rowNum,],
    # was:
    # r61 = ans <- q32 <- myCollapseQ(rowNum, 2, qosslice),
    # r63 = ans <- q33 <- qosslice[rowNum,],
    # r64 = ans <- q34 <- qosslice[rowNum,],
    # r65 = ans <- q35 <- qosslice[rowNum,],
    # r66 = ans <- q36 <- myCollapseQ(rowNum, 2, qosslice),
    r61 = { # r61:r62 has para that's to be moved into q33 and q34.  Similarly, r64 has para to be moved into q35 and q36.
      patnPara1 <- "\nPARAGRAPH ";  patnPara2 <- "\nParagraph "
      q32.paraQ3334 <- unlist(strsplit(myCollapseQ(rowNum+0, 2, qosslice), split=patnPara1)) # r61:r62
      stopifnot(length(q32.paraQ3334) == 2)
      q32 <- q32.paraQ3334[1]
      paraQ3334 <- paste0(patnPara1, q32.paraQ3334[2])
      q33 <- myPrefixPostQnum(paraQ3334, qosslice[rowNum+2,]) # r63

      q34.paraQ3536 <- unlist(strsplit(qosslice[rowNum+3,], split=patnPara2)) # r64
      stopifnot(length(q34.paraQ3536) == 2)
      q34 <- myPrefixPostQnum(paraQ3334, q34.paraQ3536[1])
      paraQ3536 <- paste0(patnPara2, q34.paraQ3536[2])

      q35 <- myPrefixPostQnum(paraQ3536, qosslice[rowNum+4,]) # r65
      q36 <- myPrefixPostQnum(paraQ3536, myCollapseQ(rowNum+5, 2, qosslice)) # r66:r67
      ans <- c(q32,  q33,q34,  q35,q36)
    },
    r68 = ans <- q37 <- qosslice[rowNum,],
    r69 = ans <- q38 <- qosslice[rowNum,],
    r70 = ans <- q39 <- qosslice[rowNum,],
    r71 = ans <- q40 <- myCollapseQ(rowNum, 2, qosslice),
    r73 = ans <- q41 <- qosslice[rowNum,],
    r74 = ans <- q42 <- qosslice[rowNum,],
    r75 = ans <- q43 <- qosslice[rowNum,],
    r76 = ans <- q44 <- qosslice[rowNum,],
    r77 = ans <- q45 <- qosslice[rowNum,],
    r78 = ans <- q46 <- qosslice[rowNum,],
    r79 = ans <- q47 <- myCollapseQ(rowNum, 3, qosslice),
    r82 = ans <- q48 <- myCollapseQ(rowNum, 2, qosslice),
    r84 = ans <- q49 <- qosslice[rowNum,],
    # was:
    # r85 = ans <- q50 <- qosslice[rowNum,],
    # r86 = ans <- q51 <- qosslice[rowNum,],
    # r87 = ans <- q52 <- qosslice[rowNum,],
    # r88 = ans <- q53 <- qosslice[rowNum,],
    # r89 = ans <- q54 <- qosslice[rowNum,],
    r85 = { # r85 has para that's to be moved into q51,q52.  Similarly, r87 has para to be moved into q53,q54.
      # row 537 para starts.
      patnSec <- "MATHEMATICS\nSECTION "; patnPara <- "\nPARAGRAPH " # eg "\nPARAGRAPH 1\nLet O be"
      q50.secParaQ5152 <- unlist(strsplit(qosslice[rowNum+0,], split=patnSec)) # r85
      stopifnot(length(q50.secParaQ5152) == 2)
      q50 <- q50.secParaQ5152[1]
      sec.paraQ5152 <- unlist(strsplit(q50.secParaQ5152[2], split=patnPara)); stopifnot(length(sec.paraQ5152) == 2)
      paraQ5152 <- paste0(patnPara, sec.paraQ5152[2]) # Drop and ignore section in sec.paraQ5152[1].

      q51 <- myPrefixPostQnum(paraQ5152, qosslice[rowNum+1,]) # r86
      q52.paraQ5354 <- unlist(strsplit(qosslice[rowNum+2,], split=patnPara)); stopifnot(length(q52.paraQ5354) == 2) # r87
      q52 <- myPrefixPostQnum(paraQ5152, q52.paraQ5354[1])
      paraQ5354 <- paste0(patnPara, q52.paraQ5354[2])
      q53 <- myPrefixPostQnum(paraQ5354, qosslice[rowNum+3,]) # r88
      q54 <- myPrefixPostQnum(paraQ5354, qosslice[rowNum+4,]) # r89

      ans <- c(q50,  q51,q52,  q53,q54)
    },
    NA # by default if switched string is unmatched so far.
  )
  return(nxtq)
}
Y2017P1.getNextQOS <- function(chrrNum, relativeRowNum, zeroRowNum, qoss.upd, metaYP){
  # print(paste("called", chrrNum)) # diagnostic troubleshooting.
  rowNum <- (relativeRowNum + zeroRowNum); qosslice <- qoss.upd
  nxtq <- switch(chrrNum,
    r1 = ans <- h1to16 <- myCollapseQ(rowNum, 16, qosslice), # merged header.
    r17 = ans <- q1 <- qosslice[rowNum,],
    r18 = ans <- q2 <- qosslice[rowNum,],
    r19 = ans <- q3 <- myCollapseQ(rowNum, 2, qosslice),
    r21 = ans <- q4 <- qosslice[rowNum,],
    r22 = ans <- q5 <- qosslice[rowNum,],
    r23 = ans <- q6 <- qosslice[rowNum,],
    r24 = ans <- q7 <- qosslice[rowNum,],
    r25 = ans <- q8 <- qosslice[rowNum,],
    r26 = ans <- q9 <- myCollapseQ(rowNum, 2, qosslice),
    r28 = ans <- q10 <- qosslice[rowNum,],
    r29 = ans <- q11 <- qosslice[rowNum,],
    r30 = { # Like PfQ, move patn from q12 into q131415.
      patn <- "Answer Q" # alt: "Answer Q[.]"
      q12.paratbl131415 <- unlist(strsplit(myCollapseQ(rowNum, 6, qosslice), split=patn))
      stopifnot(length(q12.paratbl131415) == 2)
      q12 <- q12.paratbl131415[1]; paratbl131415 <- paste0(patn, q12.paratbl131415[2])
      q13 <- myPrefixPostQnum(paratbl131415, qosslice[rowNum+6,]) # r36
      q14 <- myPrefixPostQnum(paratbl131415, qosslice[rowNum+7,]) # r37
      q15.paratbl161718 <- unlist(strsplit(myCollapseQ(rowNum+8, 6, qosslice), split=patn))
        # r38 qosslice[rowNum+8,] starts. BUT like PfQ, move patn from q15 end (r38:r43) into q161718.

      paratbl161718 <- paste0(patn, q15.paratbl161718[2])
      q15 <- myPrefixPostQnum(paratbl131415, q15.paratbl161718[1])
      q16 <- myPrefixPostQnum(paratbl161718, qosslice[rowNum+14,]) # r44
      q17 <- myPrefixPostQnum(paratbl161718, qosslice[rowNum+15,]) # r45
      # BUT in r46, "CHEMISTRY\nPART : ll CHEMISTR\nSECTION " onwards belongs to hdr r46.r47!
      patn.Chem <- "CHEMISTRY\nPART : ll CHEMISTR\nSECTION "
      q18.hdr46 <- unlist(strsplit(qosslice[rowNum+16,], split=patn.Chem)); stopifnot(length(q18.hdr46) == 2) # r46
      q18 <- myPrefixPostQnum(paratbl161718, q18.hdr46[1])
      hdr46.r47 <- paste0(patn.Chem, q18.hdr46[2], qosslice[rowNum+17,]) # Drop this later via merge.
      ans <- c(q12, q13, q14, q15,  q16, q17, q18,  hdr46.r47)
    },
    r48 = ans <- q19 <- myCollapseQ(rowNum, 2, qosslice),
    r50 = ans <- q20 <- qosslice[rowNum,],
    r51 = ans <- q21 <- qosslice[rowNum,],
    r52 = ans <- q22 <- qosslice[rowNum,],
    r53 = ans <- q23 <- qosslice[rowNum,],
    r54 = ans <- q24 <- qosslice[rowNum,],
    r55 = ans <- q25 <- qosslice[rowNum,],
    r56 = ans <- q26 <- qosslice[rowNum,],
    r57 = ans <- q27 <- qosslice[rowNum,],
    r58 = ans <- q28 <- qosslice[rowNum,],
    r59 = ans <- q29 <- qosslice[rowNum,],
    r60 = ans <- q30 <- myCollapseQ(rowNum, 2, qosslice),
    r62 = { # Include hdrq313233 into those Qs. Similarly, for hdrq343536.
      hdrq313233 <- paste0("Answer ", myCollapseQ(rowNum, 6, qosslice)) # r62:r67
      q31 <- myPrefixPostQnum(hdrq313233, qosslice[rowNum+6,]) # r68
      q32 <- myPrefixPostQnum(hdrq313233, qosslice[rowNum+7,]) # r69
      q33 <- myPrefixPostQnum(hdrq313233, qosslice[rowNum+8,]) # r70
      hdrq343536 <- paste0("Answer ", myCollapseQ(rowNum+9, 3, qosslice)) # r71:r73
      q34 <- myPrefixPostQnum(hdrq343536, qosslice[rowNum+12,]) # r74
      q35 <- myPrefixPostQnum(hdrq343536, qosslice[rowNum+13,]) # r75
      patnMathStart <- "MATHEMATICS\nSECTION" # r76 at end of q36.
      q36.hdrMath <- unlist(strsplit(qosslice[rowNum+14,], split=patnMathStart)); stopifnot(length(q36.hdrMath) == 2)
      q36 <- myPrefixPostQnum(hdrq343536, q36.hdrMath[1]) # And drop the Math header.
      ans <- c(q31, q32, q33,  q34, q35, q36)
    },
    # r77 = NA, # Just drop this Math header.
    r78 = ans <- q37 <- qosslice[rowNum,],
    r79 = ans <- q38 <- qosslice[rowNum,],
    r80 = ans <- q39 <- qosslice[rowNum,],
    r81 = ans <- q40 <- myCollapseQ(rowNum, 2, qosslice),
    r83 = ans <- q41 <- qosslice[rowNum,],
    r84 = ans <- q42 <- qosslice[rowNum,],
    r85 = ans <- q43 <- qosslice[rowNum,],
    r86 = ans <- q44 <- qosslice[rowNum,],
    r87 = ans <- q45 <- qosslice[rowNum,],
    r88 = ans <- q46 <- myCollapseQ(rowNum, 2, qosslice),
    r90 = ans <- q47 <- qosslice[rowNum,],
    r91 = ans <- q48 <- qosslice[rowNum,],
    r92 = {
      paratblQ495051 <- myCollapseQ(rowNum, 4, qosslice) # r92:r95
      q49 <- myPrefixPostQnum(paratblQ495051, qosslice[rowNum+4,]) # r96
      q50 <- myPrefixPostQnum(paratblQ495051, qosslice[rowNum+5,]) # r97
      q51 <- myPrefixPostQnum(paratblQ495051, myCollapseQ(rowNum+6, 5, qosslice)) # r98:r102
      paratblQ525354 <- myCollapseQ(rowNum+11, 3, qosslice) # r103:r105
      q52 <- myPrefixPostQnum(paratblQ525354, qosslice[rowNum+14,]) # r106
      q53 <- myPrefixPostQnum(paratblQ525354, qosslice[rowNum+15,]) # r107
      patnHdrStart <- "2[!]\\nResonance Eduventures Ltd.\\nCORPORATE" # Till r111 patnHdrStart.
      r111 <- qosslice[rowNum+19,]
      r111.hdr <- unlist(strsplit(r111, split=patnHdrStart)); stopifnot(length(r111.hdr) == 2)
      q54 <- paste0(myCollapseQ(rowNum+16, 3, qosslice), r111.hdr[1])
        # Create q54 from r108:r110 plus earlier part of r111.  Then prefix paratbl.
      q54 <- myPrefixPostQnum(paratblQ525354, sub(pattern="94[.]", replacement="54.", x=q54))
      hdr <- paste0(patnHdrStart, r111.hdr[2], myCollapseQ(rowNum+20, 6, qosslice)) # hdr (from r111) plus r112:r117.
      ans <- c(q49, q50, q51,  q52, q53, q54,  hdr)
    },
    NA # by default if switched string is unmatched so far.
  )
  return(nxtq)
}
Y2016P2.getNextQOS <- function(chrrNum, relativeRowNum, zeroRowNum, qoss.upd, metaYP){
  # print(paste("called", chrrNum)) # diagnostic troubleshooting.
  rowNum <- (relativeRowNum + zeroRowNum); qosslice <- qoss.upd
  nxtq <- switch(chrrNum,
    r1 = ans <- h1to15 <- myCollapseQ(rowNum, 15, qosslice), # merged header.
    r16 = ans <- q1 <- myCollapseQ(rowNum, 2, qosslice),
    r18 = ans <- q2 <- qosslice[rowNum,],
    r19 = ans <- q3 <- myCollapseQ(rowNum, 2, qosslice),
    r21 = ans <- q4 <- qosslice[rowNum,],
    r22 = ans <- q5 <- qosslice[rowNum,],
    r23 = ans <- q6 <- qosslice[rowNum,],
    r24 = ans <- q7 <- qosslice[rowNum,],
    r25 = ans <- q8 <- qosslice[rowNum,],
    r26 = ans <- q9 <- myCollapseQ(rowNum, 2, qosslice),
    r28 = ans <- q10 <- qosslice[rowNum,],
    r29 = ans <- q11 <- qosslice[rowNum,],
    r30 = ans <- q12 <- qosslice[rowNum,],
    r31 = ans <- q13 <- myCollapseQ(rowNum, 2, qosslice),
    # was:
    # r33 = ans <- q14 <- qosslice[rowNum,],
    # r34 = ans <- q15 <- qosslice[rowNum,],
    # r35 = ans <- q16 <- qosslice[rowNum,],
    # r36 = ans <- q17 <- qosslice[rowNum,],
    # r37 = ans <- q18 <- myCollapseQ(rowNum, 3, qosslice),
    r33 = {
      q14.PfQ1516 <- unlist(strsplit(qosslice[rowNum,], split=cPattern.PfQ)); stopifnot(length(q14.PfQ1516) == 2)
      q14 <- q14.PfQ1516[1]
      PfQ1516 <- paste0(cPattern.PfQ, " ", q14.PfQ1516[2])
      q15 <- myPrefixPostQnum(PfQ1516, qosslice[rowNum+1,])

      # Beware: q16 has PfQ for q17 q18; that's to be prefixed too!
      q16.PfQ1718 <- unlist(strsplit(qosslice[rowNum+2,], split=cPattern.PfQ)); stopifnot(length(q16.PfQ1718) == 2)
      q16 <- myPrefixPostQnum(PfQ1516, q16.PfQ1718[1])
      PfQ1718 <- paste0(cPattern.PfQ, " ", q16.PfQ1718[2])
      q17 <- myPrefixPostQnum(PfQ1718, qosslice[rowNum+3,])
      q18 <- myPrefixPostQnum(PfQ1718, myCollapseQ(rowNum+4, 3, qosslice))
      ans <- c(q14, q15, q16, q17, q18)
    },
    r40 = ans <- q19 <- qosslice[rowNum,],
    r41 = ans <- q20 <- qosslice[rowNum,],
    r42 = ans <- q21 <- qosslice[rowNum,],
    r43 = ans <- q22 <- qosslice[rowNum,],
    r44 = ans <- q23 <- qosslice[rowNum,],
    r45 = ans <- q24 <- qosslice[rowNum,],
    r46 = ans <- q25 <- qosslice[rowNum,],
    r47 = ans <- q26 <- qosslice[rowNum,],
    r48 = ans <- q27 <- qosslice[rowNum,],
    r49 = ans <- q28 <- qosslice[rowNum,],
    r50 = ans <- q29 <- qosslice[rowNum,],
    r51 = ans <- q30 <- qosslice[rowNum,],
    r52 = ans <- q31 <- qosslice[rowNum,],
    # was:
    # r53 = ans <- q32 <- qosslice[rowNum,],
    # r54 = ans <- q33 <- myCollapseQ(rowNum, 2, qosslice),
    # r56 = ans <- q34 <- myCollapseQ(rowNum, 2, qosslice),
    # r58 = ans <- q35 <- qosslice[rowNum,],
    # r59 = ans <- q36 <- qosslice[rowNum,],
    r53 = { # Beware: q32 and q34 have "Paragraph [1-9]" for subsequent 2 questions.
      patn <- "Paragraph [1-9]" # Beware: not cPattern.PfQ
      q32para3334 <- qosslice[rowNum,] # r53
      q32.para3334 <- unlist(strsplit(q32para3334, split=patn)); stopifnot(length(q32.para3334) == 2)
      q32 <- q32.para3334[1]; para3334 <- paste0(patn, q32.para3334[2])
      q33 <- myPrefixPostQnum(para3334, myCollapseQ(rowNum+1, 2, qosslice)) # prefix para to r54:55
      q34para3536 <- myCollapseQ(rowNum+3, 2, qosslice) # r56:57
      q34.para3536 <- unlist(strsplit(q34para3536, split=patn)); stopifnot(length(q34.para3536) == 2)
      q34 <- myPrefixPostQnum(para3334, q34.para3536[1]); para3536 <- paste0(patn, q34.para3536[2])
      q35 <- myPrefixPostQnum(para3536, qosslice[rowNum+5,]) # prefix para to r58
      q36 <- myPrefixPostQnum(para3536, qosslice[rowNum+6,]) # prefix para to r59
      ans <- c(q32, q33, q34, q35, q36)
    },
    r60 = ans <- q37 <- qosslice[rowNum,],
    r61 = ans <- q38 <- myCollapseQ(rowNum, 3, qosslice),
    r64 = ans <- q39 <- myCollapseQ(rowNum, 2, qosslice),
    r66 = ans <- q40 <- qosslice[rowNum,],
    r67 = ans <- q41 <- myCollapseQ(rowNum, 2, qosslice),
    r69 = ans <- q42 <- qosslice[rowNum,],
    r70 = ans <- q43 <- qosslice[rowNum,],
    r71 = ans <- q44 <- myCollapseQ(rowNum, 2, qosslice),
    r73 = ans <- q45 <- qosslice[rowNum,],
    r74 = ans <- q46 <- qosslice[rowNum,],
    r75 = ans <- q47 <- qosslice[rowNum,],
    r76 = ans <- q48 <- qosslice[rowNum,],
    r77 = ans <- q49 <- qosslice[rowNum,],
    # r78 = ans <- q50 <- myCollapseQ(rowNum, 3, qosslice),
    # r81 = ans <- q51 <- qosslice[rowNum,],
    # r82 = ans <- q52 <- myCollapseQ(rowNum, 2, qosslice),
    # r84 = ans <- q53 <- qosslice[rowNum,],
    # r85 = {
    #   hdrStartStr <- "DARKENING THE BUBBLES"
    #   q54.hdr <- unlist(strsplit(myCollapseQ(rowNum, 7, qosslice), split=hdrStartStr))
    #   stopifnot(length(q54.hdr) == 2)
    #   q54 <- sub(pattern="^94[.]", replacement="54.", x=q54.hdr[1])
    #   hdr <- paste0(hdrStartStr, q54.hdr[2])
    #   ans <- c(q54, hdr)
    # },
    r78 = {
      patn <- "\\nParagraph"; patn.fixed <- "Paragraph" # without regular-expression directives.
        # Beware: from end of q50 & q52, this has to be moved into consecutive 2 questions respectively.
        # Beware: patn=="^(Paragraph)" did not match.
      q50p5152 <- myCollapseQ(rowNum, 3, qosslice) # r78
      q50.p5152 <- unlist(strsplit(q50p5152, split=patn)); stopifnot(length(q50.p5152) == 2)
      q50 <- q50.p5152[1]; p5152 <- paste0(patn.fixed, q50.p5152[2]) # Beware: there's a "^" at start of patn, and more.

      q51 <- myPrefixPostQnum(p5152, qosslice[rowNum+3,]) # r81
      q52p5354 <- myCollapseQ(rowNum+4, 2, qosslice) # r82
      q52.p5354 <- unlist(strsplit(q52p5354, split=patn)); stopifnot(length(q52.p5354) == 2)
      q52 <- myPrefixPostQnum(p5152, q52.p5354[1])
      p5354 <- paste0(patn.fixed, q52.p5354[2])
      q53 <- myPrefixPostQnum(p5354, qosslice[rowNum+6,]) # r84

      hdrStartStr <- "DARKENING THE BUBBLES"
      q54.hdr <- unlist(strsplit(myCollapseQ(rowNum+7, 7, qosslice), split=hdrStartStr)) # r85
      stopifnot(length(q54.hdr) == 2)
      q54 <- myPrefixPostQnum(p5354, sub(pattern="^94[.]", replacement="54.", x=q54.hdr[1]))
      hdr <- paste0(hdrStartStr, q54.hdr[2])

      ans <- c(q50, q51, q52, q53, q54, hdr)
    },
    NA # by default if switched string is unmatched so far.
  )
  return(nxtq)
}
Y2016P1.getNextQOS <- function(chrrNum, relativeRowNum, zeroRowNum, qoss.upd, metaYP){
  # print(paste("called", chrrNum)) # diagnostic troubleshooting.
  rowNum <- (relativeRowNum + zeroRowNum); qosslice <- qoss.upd
  nxtq <- switch(chrrNum,
    r1 = ans <- h1to15 <- myCollapseQ(rowNum, 15, qosslice), # merged header.
    r16 = ans <- q1 <- myCollapseQ(rowNum, 2, qosslice),
    r18 = ans <- q2 <- qosslice[rowNum,],
    r19 = ans <- q3 <- qosslice[rowNum,],
    r20 = ans <- q4 <- myCollapseQ(rowNum, 2, qosslice),
    r22 = ans <- q5 <- qosslice[rowNum,],
    r23 = ans <- q6 <- myCollapseQ(rowNum, 2, qosslice),
    r25 = ans <- q7 <- qosslice[rowNum,],
    r26 = ans <- q8 <- qosslice[rowNum,],
    r27 = ans <- q9 <- myCollapseQ(rowNum, 3, qosslice),
    r30 = ans <- q10 <- qosslice[rowNum,],
    r31 = ans <- q11 <- qosslice[rowNum,],
    r32 = ans <- q12 <- qosslice[rowNum,],
    r33 = ans <- q13 <- qosslice[rowNum,],
    r34 = ans <- q14 <- myCollapseQ(rowNum, 2, qosslice),
    r36 = ans <- q15 <- qosslice[rowNum,],
    r37 = ans <- q16 <- qosslice[rowNum,],
    r38 = ans <- q17 <- myCollapseQ(rowNum, 2, qosslice),
    r40 = ans <- q18 <- myCollapseQ(rowNum, 2, qosslice),
    r42 = ans <- q19 <- qosslice[rowNum,],
    r43 = ans <- q20 <- qosslice[rowNum,],
    r44 = ans <- q21 <- qosslice[rowNum,],
    r45 = ans <- q22 <- myCollapseQ(rowNum, 2, qosslice),
    r47 = ans <- q23 <- qosslice[rowNum,],
    r48 = ans <- q24 <- qosslice[rowNum,],
    r49 = ans <- q25 <- myCollapseQ(rowNum, 2, qosslice),
    r51 = ans <- q26 <- qosslice[rowNum,],
    r52 = ans <- q27 <- qosslice[rowNum,],
    r53 = ans <- q28 <- qosslice[rowNum,],
    r54 = ans <- q29 <- qosslice[rowNum,],
    r55 = ans <- q30 <- qosslice[rowNum,],
    r56 = ans <- q31 <- qosslice[rowNum,],
    r57 = ans <- q32 <- myCollapseQ(rowNum, 2, qosslice),
    r59 = ans <- q33 <- qosslice[rowNum,],
    r60 = ans <- q34 <- qosslice[rowNum,],
    r61 = ans <- q35 <- qosslice[rowNum,],
    r62 = ans <- q36 <- qosslice[rowNum,],
    r63 = ans <- q37 <- qosslice[rowNum,],
    r64 = ans <- q38 <- qosslice[rowNum,],
    r65 = ans <- q39 <- myCollapseQ(rowNum, 3, qosslice),
    r68 = ans <- q40 <- qosslice[rowNum,],
    r69 = ans <- q41 <- qosslice[rowNum,],
    r70 = ans <- q42 <- myCollapseQ(rowNum, 4, qosslice),
    r74 = ans <- q43 <- myCollapseQ(rowNum, 2, qosslice),
    r76 = ans <- q44 <- myCollapseQ(rowNum, 4, qosslice),
    r80 = ans <- q45 <- myCollapseQ(rowNum, 2, qosslice),
    r82 = ans <- q46 <- qosslice[rowNum,],
    r83 = ans <- q47 <- myCollapseQ(rowNum, 2, qosslice),
    r85 = ans <- q48 <- qosslice[rowNum,],
    r86 = ans <- q49 <- qosslice[rowNum,],
    r87 = ans <- q50 <- qosslice[rowNum,],
    r88 = ans <- q51 <- myCollapseQ(rowNum, 2, qosslice),
    r90 = ans <- q52 <- qosslice[rowNum,],
    r91 = {
      # q53 = concat(r91,92,93,94pre5A).  q54 = concat(sub(r94post5A), r95preResonance).
      # hdr = concat(r95postResonance,r96:r101) # which you drop later.
      rows91to94 <- myCollapseQ(rowNum, 4, qosslice)
      rows91to94.prePost5A <- unlist(strsplit(rows91to94, split="5A[.]"))
      stopifnot(length(rows91to94.prePost5A) == 2)
      q53 <- rows91to94.prePost5A[1]
      row95.prePostResonance <- unlist(strsplit(qosslice[rowNum+4,], split="Resonance Eduventures"))
      stopifnot(length(row95.prePostResonance) == 2)
      q54 <- paste0("54.", rows91to94.prePost5A[2], row95.prePostResonance[1])
      hdr <- paste0("Resonance Eduventures", row95.prePostResonance[2], myCollapseQ(rowNum+5, 6, qosslice))
      ans <- c(q53, q54, hdr) # was: q53 <- qosslice[rowNum,]
    },
    NA # by default if switched string is unmatched so far.
  )
  return(nxtq)
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
