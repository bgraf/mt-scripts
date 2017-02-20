source('util.R')
library(corrplot)

df = readDataset()
df$NFF = with(df, FF / (BranchGoals + LineGoals + MethodGoals))
#df <- adjustCoverageNAs(df)

dfs = aggByClass(df) #aggregate(. ~ Class, data=df, function (c) mean(c, na.rm=T))

dfs = subset(dfs, select=c('CR', 'PIC', 'FF', 'NFF', 'NumIter', "BranchCoverage","MethodCoverage", "LineCoverage"))
names(dfs) = c("CR", "PIC", "FF", 'NFF', "NI", "BC", "MC", "LC")


print('Missing values by column:')
missings <- sapply(dfs, function(c) sum(is.na(c)))
print(missings)
print(paste('Used rows:', nrow(dfs) - max(missings)))

# ----

source('corr.R')

mkggcorr(dfs, 'pearson')
ggsave('evoResultCorr-gg-pears.pdf', width = width, height = height)

mkggcorr(dfs, 'spearman')
ggsave('evoResultCorr-gg-spear.pdf', width = width, height = height)

# ----


mPears <- cor(dfs, use="complete", method="pearson")
mSpear <- cor(dfs, use="complete", method="spearman")

pdf('evoResultCorr-agg-pears.pdf', width=width, height=height)
corrplot.mixed(mPears)
dev.off()

pdf('evoResultCorr-agg-spear.pdf', width=width, height=height)
corrplot.mixed(mSpear)
dev.off()

print('Pearson (agg):')
print(round(mPears, digits=2))

print('Spearman (agg):')
print(round(mSpear, digits=2))
