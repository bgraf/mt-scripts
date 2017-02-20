source('util.R')
library(corrplot)

df <- readDataset()
dfa <- aggByClass(df)

dfs <- subset(dfa, select=c(colRes, colMetric))

dfc <- dfs[complete.cases(dfs),]

mPears <- cor(dfc)
mSpear <- cor(dfc, method='spearman')

selectBest <- function(row, n=10) {
  ordered <- sort(abs(row))
  sub <- ordered[names(ordered) %in% colMetric]
  names(tail(sub, n=n))
}


renCols <- function(df) {
  rename(df, c("BranchCoverage"="BC", 
                   "MethodCoverage"="MC", 
                   "TestCnt"="TC",
                   "mPMethodCnt" = "PMCnt",
                   "mCMethodCnt"="CMCnt",
                   "mPCMethodCnt"="PCMCnt",
                   "mMethodCnt"="MCnt"))
}

pdf('softwareVsResultsCorr-bc.pdf', width=width, height=height)
bcFive <- selectBest(mSpear["BranchCoverage",], n=5)
ddf <- dfc[, c("BranchCoverage", bcFive)]
ddf <- renCols(ddf)
corrplot.mixed(cor(ddf))
dev.off()

pdf('softwareVsResultsCorr-mc.pdf', width=width, height=height)
bcFive <- selectBest(mSpear["MethodCoverage",], n=5)
ddf <- dfc[, c("MethodCoverage", bcFive)]
ddf <- renCols(ddf)
corrplot.mixed(cor(ddf))
dev.off()

pdf('softwareVsResultsCorr-tc.pdf', width=width, height=height)
bcFive <- selectBest(mSpear["TestCnt",], n=5)
ddf <- dfc[, c("TestCnt", bcFive)]
ddf <- renCols(ddf)
corrplot.mixed(cor(ddf))
dev.off()

