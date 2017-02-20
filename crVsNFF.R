source('util.R')

df = readDataset()
df$NFF = with(df, FF / (BranchGoals + LineGoals + MethodGoals))

#------------------------------------------------------------------------------
# CR vs. NFF for observations with incomplete coverage
#------------------------------------------------------------------------------

dfi = subset(
  df,
  BranchGoals > BranchCovered |
  MethodGoals > MethodCovered |
  LineGoals > LineCovered)

dfi = aggByClass(dfi)

cor(dfi$CR, dfi$NFF, use = 'complete', method='spearman')
cor(dfi$CR, dfi$NFF, use = 'complete', method='pearson')

cor(dfi$CR, dfi$BranchCoverage, use = 'complete', method='spearman')
cor(dfi$CR, dfi$BranchCoverage, use = 'complete', method='pearson')

#------------------------------------------------------------------------------
# CR vs. NFF for observations with incomplete coverage and more than one iter
#------------------------------------------------------------------------------

dfim = subset(dfi, NumIter > 1)
dfim = aggByClass(dfim)
cor(dfim$CR, dfim$NFF, use = 'complete', method='pearson')
cor(dfim$CR, dfim$NFF, use = 'complete', method='spearman')

cor(dfim$CR, dfim$BranchCoverage, use = 'complete', method='pearson')
cor(dfim$CR, dfim$BranchCoverage, use = 'complete', method='spearman')



#------------------------------------------------------------------------------
# CR vs. NFF for observations with more than one iteration
#------------------------------------------------------------------------------

dfm = subset(df, NumIter > 1)
dfm = aggByClass(dfm)
cor(dfm$CR, dfm$NFF, use = 'complete', method = 'pearson')
cor(dfm$CR, dfm$NFF, use = 'complete', method = 'spearman')

cor(dfm$CR, dfm$BranchCoverage, use = 'complete', method = 'pearson')
cor(dfm$CR, dfm$BranchCoverage, use = 'complete', method = 'spearman')


