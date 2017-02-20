library(Rmisc)
source('util.R')
library(corrplot)
source('corr.R')

df = readDataset()
dfa = aggByClass(df)

dfs = subset(dfa, select=c(colMetric, colRes))

dfs$NEs = with(dfs, mIFNEs + mICMPNEs + mACMPNEs + mIFNONNULLs)
dfs$EQs = with(dfs, mICMPEQs + mACMPEQs + mIFNULLs + mIFEQs)
dfs$EQNEs = dfs$NEs + dfs$EQs
dfs$CMPs = dfs$mBranchInstrCnt - dfs$EQNEs

dfd = subset(dfs, select = c(
  'mBranchInstrCnt',
  'CMPs',
  'EQNEs',
  'NA.',
  'NLE',
  'NOS',
  'BranchCoverage',
  'MethodCoverage',
  'LineCoverage'
))

names(dfd) = c(
  'BIs', 'CMPs', 'EQNEs', 'NA.', 'NLE', 'NOS', 'BC', 'MC', 'LC'
)

# ----

mkggcorr(dfd, 'pearson')
ggsave('sizeMetricsVsResults-gg-pears.pdf', width = width, height = height)

mkggcorr(dfd, 'spearman')
ggsave('sizeMetricsVsResults-gg-spear.pdf', width = width, height = height)

# ----

mPears = cor(dfd, use='complete', method='pearson')
mSpear = cor(dfd, use='complete', method='spearman')

pdf('sizeMetricsVsResults-mix-pears.pdf', width=width, height=height)
corrplot.mixed(mPears)
dev.off()

pdf('sizeMetricsVsResults-mix-spear.pdf', width=width, height=height)
corrplot.mixed(mSpear)
dev.off()

## Branch instructions..

s = summarySE(dfd, measurevar = 'BC', groupvars = 'BIs', na.rm = T)
s = filter(s, N >= minObs)

ggplot(s, aes(BIs, BC)) +
  geom_point() +
  geom_errorbar(aes(ymin=BC-se, ymax=BC+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('BC') +
  xlab('BIs') +
  #scale_x_continuous(breaks = seq(0,10)) +
  theme_minimal() + ylim(0.0,1.0)
ggsave('sizeMetricsVsResults-bc-by-bis.pdf', width=width3, height=height3)

s = summarySE(dfd, measurevar = 'BC', groupvars = 'EQNEs', na.rm = T)
s = filter(s, N >= minObs)

ggplot(s, aes(EQNEs, BC)) +
  geom_point() +
  geom_errorbar(aes(ymin=BC-se, ymax=BC+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('BC') +
  xlab('EQNEs') +
  #scale_x_continuous(breaks = seq(0,10)) +
  theme_minimal() + ylim(0.0,1.0)
ggsave('sizeMetricsVsResults-bc-by-eqnes.pdf', width=width3, height=height3)

s = summarySE(dfd, measurevar = 'BC', groupvars = 'CMPs', na.rm = T)
s = filter(s, N >= minObs)

ggplot(s, aes(CMPs, BC)) +
  geom_point() +
  geom_errorbar(aes(ymin=BC-se, ymax=BC+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('BC') +
  xlab('CMPs') +
  #scale_x_continuous(breaks = seq(0,10)) +
  theme_minimal() + ylim(0.0,1.0)

## Nesting level if-else

s = summarySE(dfd, measurevar = 'BC', groupvars = 'NLE', na.rm = T)
s = filter(s, N >= minObs)

ggplot(s, aes(NLE, BC)) +
  geom_point() +
  geom_errorbar(aes(ymin=BC-se, ymax=BC+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('BC') +
  xlab('NLE') +
  theme_minimal() + ylim(0.0,1.0)
ggsave('sizeMetricsVsResults-bc-by-nle.pdf', width=width3, height=height3)


# Nesting level distribution

d = subset(dfa, NLE == 0)
ggplot(d, aes(BranchCoverage)) + geom_histogram(fill=stdGrey, color='black', bins = 10) + theme_minimal()

d = subset(dfa, NLE == 3)
ggplot(d, aes(BranchCoverage)) + geom_histogram(fill=stdGrey, color='black', bins = 10) + theme_minimal()

d = subset(dfa, NLE == 7)
ggplot(d, aes(BranchCoverage)) + geom_histogram(fill=stdGrey, color='black', bins = 10) + theme_minimal()

