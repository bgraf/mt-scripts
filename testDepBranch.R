source('util.R')

df = read.csv('dataset_dep.csv', header=T)

dfa = aggByClass(df)

names(dfa)

dfs = subset(dfa, select=-c(NSC,NSCFCS,PIC,CR))

dfs$NEs <- with(dfs, mIFNEs + mICMPNEs + mACMPNEs + mIFNONNULLs)
dfs$EQs <- with(dfs, mICMPEQs + mACMPEQs + mIFNULLs + mIFEQs)
dfs$EQNEs <- dfs$NEs + dfs$EQs
dfs$CMPs <- dfs$mBranchInstrCnt - dfs$EQNEs

dfs$mdNEf = with(dfs, mdIFNEf + mdICMPNEf + mdACMPNEf + mdIFNONNULLf)
dfs$mdNEt = with(dfs, mdIFNEt + mdICMPNEt + mdACMPNEt + mdIFNONNULLt)
dfs$mdEQf <- with(dfs, mdICMPEQf + mdACMPEQf + mdIFNULLf + mdIFEQf)
dfs$mdEQt <- with(dfs, mdICMPEQt + mdACMPEQt + mdIFNULLt + mdIFEQt)

dfs$mdCMPNEf = with(dfs, mdICMPNEf + mdACMPNEf)
dfs$mdCMPNEt = with(dfs, mdICMPNEt + mdACMPNEt)
dfs$mdCMPEQf = with(dfs, mdICMPEQf + mdACMPEQf)
dfs$mdCMPEQt = with(dfs, mdICMPEQt + mdACMPEQt)

dfs$CMPNEs = with(dfs, mICMPNEs + mACMPNEs)
dfs$CMPEQs = with(dfs, mICMPEQs + mICMPEQs)


plot(density(with(dfs, (mdNEf) / NEs), na.rm = T))
plot(density(with(dfs, (mdEQt) / EQs), na.rm = T))

hist(dfs$mdNEf, n=1000, xlim=c(0,100))

ggplot(dfs, aes(mdEQf, mdEQt)) + geom_smooth(method = "lm") + geom_point() + lims(x=c(0,100), y=c(0,100))
ggplot(dfs, aes(mdNEf, mdNEt)) + geom_smooth(method = "lm") + geom_point() + lims(x=c(0,100), y=c(0,100))

ggplot(dfs, aes(mdCMPEQf, mdCMPEQt)) + geom_smooth(method = "lm") + geom_point() + lims(x=c(0,100), y=c(0,100))
ggplot(dfs, aes(mdCMPNEf, mdCMPNEt)) + geom_smooth(method = "lm") + geom_point() + lims(x=c(0,100), y=c(0,100))

ggplot(dfs, aes(mdCMPEQt - (mdCMPEQf))) + geom_histogram(bins = 30)
ggplot(dfs, aes(mdCMPNEt - (mdCMPNEf))) + geom_histogram(bins = 30)

dfs$CMPEQs = with(dfs, mICMPEQs + mACMPEQs)
dfs$CMPNEs = with(dfs, mICMPNEs + mACMPNEs)

# --- NE vs EQ in T vs. F

cor(dfs$CMPEQs, dfs$BranchCoverage, use = 'complete', method = 'spearman')
cor(dfs$CMPNEs, dfs$BranchCoverage, use = 'complete', method = 'spearman')

sum(dfs$mdCMPNEf, na.rm = T) / sum(dfs$NEs, na.rm = T)


plot(dfs$mdEQt, dfs$EQs)
plot(dfs$mdNEf, dfs$NEs)

qqplot(dfs$mdEQt, dfs$EQs)
qqplot(dfs$mdNEf, dfs$NEs)

## -----

# Only CMPs

cor(dfs$CMPEQs, dfs$BranchCoverage, use="complete", method="spearman")
cor(dfs$CMPNEs, dfs$BranchCoverage, use="complete", method="spearman")

hist(dfs$mdCMPEQt - dfs$mdCMPNEf)

a = dfs$mdCMPEQt / dfs$CMPEQs
b = dfs$mdCMPNEf / dfs$CMPNEs

hist(a - b)


## -----

cor(dfs$mIFEQs, dfs$BranchCoverage, use="complete", method="spearman")
cor(dfs$mIFNEs, dfs$BranchCoverage, use="complete", method="spearman")


a = (dfs$mdIFEQt + dfs$mdIFEQf) / dfs$mIFEQs
b = (dfs$mdIFNEt + dfs$mdIFNEf) / dfs$mIFNEs

hist(a-b)

# All
cor(dfs$EQs, dfs$BranchCoverage, use="complete", method="spearman")
cor(dfs$NEs, dfs$BranchCoverage, use="complete", method="spearman")

hist(dfs$mdEQt - dfs$mdCMPNEf)

a = dfs$mdEQt / dfs$EQs
b = dfs$mdNEf / dfs$NEs

hist(a-b)

## -----

dfx = subset(dfs, NEs > EQs)

hist(dfx$mdNEf - dfx$mdEQt)

hist(dfx$mdNEt - dfx$mdEQf)



hist(dfx$mdNEf + dfx$mdNEt - dfx$mdEQf - dfx$mdEQt)

cor(dfx$EQs, dfx$BranchCoverage, use="complete", method="spearman")
cor(dfx$NEs, dfx$BranchCoverage, use="complete", method="spearman")
