library(reshape)
library(dplyr)
library(ggplot2)
library(moments)

minObs = 10

width <- 5
height <- 5

widthFull <- 10
heightFull <- 10

width3 <- 3
height3 <- 3

stdGrey <- 'grey90'

readDataset <- function() { read.csv('dataset.csv', header=T) }

readAgg <- function() { 
  df <- readDataset()
  aggregate(. ~ Class, data=df, function(x) mean(x, na.rm=T), na.action=NULL)
}

aggByClass <- function(df) {
  aggregate(. ~ Class, data=df, function(x) mean(x, na.rm=T), na.action=NULL)
}

adjustCoverageNAs <- function(df) {
  df[is.na(df$BranchCoverage),"BranchCoverage"] <- 0
  df[is.na(df$MethodCoverage),"MethodCoverage"] <- 0
  df[is.na(df$LineCoverage),"LineCoverage"] <- 0
  df[is.na(df$BranchCovered),"BranchCovered"] <- 0
  df[is.na(df$MethodCovered),"MethodCovered"] <- 0
  df[is.na(df$LineCovered),"LineCovered"] <- 0
  df
}


colAdmin <- c(
  "Project",
  "Class",
  "Seed"
)

colRes <- c(
  "TestCnt",
  "TestLen",
  "LineGoals",
  "LineCovered",
  "LineCoverage",
  "MethodGoals",
  "MethodCovered",
  "MethodCoverage",
  "BranchGoals",
  "BranchCovered",
  "BranchCoverage"
)

colEvo <- c(
  "CR",
  "PIC",
  "NSC",
  "FF",
  "NSCFCS",
  "NumIter"
)

colMetric <- c(
  "AD",
  "CBO",
  "CBOI",
  "CC",
  "CCL",
  "CCO",
  "CD",
  "CI",
  "CLC",
  "CLLC",
  "CLOC",
  "DIT",
  "DLOC",
  "LCOM5",
  "LDC",
  "LLDC",
  "LLOC",
  "LOC",
  "NA.",
  "NG",
  "NII",
  "NL",
  "NLA",
  "NLE",
  "NLG",
  "NLM",
  "NLPA",
  "NLPM",
  "NLS",
  "NM",
  "NOA",
  "NOC",
  "NOD",
  "NOI",
  "NOP",
  "NOS",
  "NPA",
  "NPM",
  "NS",
  "PDA",
  "PUA",
  "RFC",
  "TCD",
  "TCLOC",
  "TLLOC",
  "TLOC",
  "TNA",
  "TNG",
  "TNLA",
  "TNLG",
  "TNLM",
  "TNLPA",
  "TNLPM",
  "TNLS",
  "TNM",
  "TNOS",
  "TNPA",
  "TNPM",
  "TNS",
  "WMC",
  "cWMC",
  "cDIT",
  "cNOC",
  "cCBO",
  "cRFC",
  "cLCOM",
  "cCa",
  "cNPM",
  "mMethodCnt",
  "mCMethodCnt",
  "mBranchCnt",
  "mBranchInstrCnt",
  "mPMethodCnt",
  "mPCMethodCnt",
  "mSMethodCnt",
  "mSCMethodCnt",
  "mSPMethodCnt",
  "mSPCMethodCnt",
  "mFieldCnt",
  "mPFieldCnt",
  "mIFEQs",
  "mIFNEs",
  "mIFLTs",
  "mIFLEs",
  "mIFGTs",
  "mIFGEs",
  "mICMPEQs",
  "mICMPNEs",
  "mICMPLTs",
  "mICMPGEs",
  "mICMPGTs",
  "mICMPLEs",
  "mACMPEQs",
  "mACMPNEs",
  "mIFNULLs",
  "mIFNONNULLs"
)

# limitedCorrPlot <- function(mat, keep, digits = 2) {
#   mat <- mat[!(rownames(mat) %in% keep), colnames(mat) %in% keep]
#   mat <- round(mat, digits = digits)
#   dat <- melt(mat)
#   
#   dat$X1 <- factor(dat$X1, levels=setdiff(rownames(mat), keep))
#   dat$X2 <- factor(dat$X2, levels=keep)
#   ggplot(dat, aes(X2, X1, fill = value)) +
#     geom_tile() +
#     coord_equal() +
#     theme(aspect.ratio = 1) +
#     geom_text(aes(X2, X1, label = value), color = "black", size = 4) +
#     labs(x = NULL, y = NULL) +
#     scale_fill_gradient2(
#       name = NULL,
#       low = "blue",
#       high = "red",
#       mid = "white",
#       midpoint = 0,
#       limit = c(-1, 1),
#       space = "Lab"
#     ) +
#     theme_minimal()
# }
# 
# keep <- c('TC', 'BC', 'MC', 'LC')
# 
# limitedCorrPlot(mPears, keep)
# ggsave('sizeMetricsVsResults-tab-pears.pdf', width=width, height=height)
# 
# limitedCorrPlot(mSpear, keep)
# ggsave('sizeMetricsVsResults-tab-spear.pdf', width=width, height=height)
