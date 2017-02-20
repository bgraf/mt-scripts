source('util.R')
library(corrplot)
source('corr.R')

df = readDataset()
dfa = aggByClass(df)

cols = c("WMC","DIT","RFC","LCOM5","CBO")
res = c("BranchCoverage", "MethodCoverage", "LineCoverage")

dfs = subset(dfa, select=c(cols, res))

names(dfs) = c('WMC', 'DIT', 'RFC', 'LCOM5', 'CBO', 'BC', 'MC', 'LC')

# ----

mkggcorr(dfs, 'pearson')
ggsave('metricsVsResults-gg-pears.pdf', width = width, height = height)

mkggcorr(dfs, 'spearman')
ggsave('metricsVsResults-gg-spear.pdf', width = width, height = height)

#----

mPears = cor(dfs, use="complete")
mSpear = cor(dfs, use="complete", method='spearman')

pdf('metricsVsResults-mix-pears.pdf', width=width, height=height)
corrplot.mixed(mPears)
dev.off()

pdf('metricsVsResults-mix-spear.pdf', width=width, height=height)
corrplot.mixed(mSpear)
dev.off()


pdf('metricsVsResults-pears.pdf', width=width, height=height)
corrplot(mPears, type='upper')
dev.off()

pdf('metricsVsResults-spear.pdf', width=width, height=height)
corrplot(mSpear, type='upper')
dev.off()

