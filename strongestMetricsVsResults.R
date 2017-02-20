source('util.R')

selectStrongestMetrics <- function(mat, criterion) {
  row <- mat[criterion, colnames(mat) %in% colMetric]
  na.omit(row[order(abs(row), decreasing = T)])    
} 

df <- readDataset()
dfa <- aggByClass(df)

dfs <- subset(dfa, select=c(colMetric, colRes))
names(dfs)


dfs$NEs <- with(dfs, mIFNEs + mICMPNEs + mACMPNEs + mIFNONNULLs)
dfs$EQs <- with(dfs, mICMPEQs + mACMPEQs + mIFNULLs + mIFEQs)
dfs$EQNEs <- dfs$NEs + dfs$EQs


mPears <- cor(dfs, use='complete', method='pearson')
mSpear <- cor(dfs, use='complete', method='spearman')


a <- selectStrongestMetrics(mPears, 'BranchCoverage')
b <- selectStrongestMetrics(mPears, 'MethodCoverage')
c <- selectStrongestMetrics(mPears, 'LineCoverage')

namesOver <- function(a, limit=0.3) {
  names(a[abs(a) > limit])
}

mSpear['BranchCoverage',]

selectStrongestMetrics(mSpear, 'BranchCoverage')
