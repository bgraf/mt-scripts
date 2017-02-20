library(Rmisc)
source('util.R')
library(corrplot)
source('corr.R')

df = readDataset()
dfa = aggByClass(df)

dfd = subset(
  dfa,
  select = c(
    'BranchGoals',
    'MethodGoals',
    'LineGoals',
    'BranchCoverage',
    'MethodCoverage',
    'LineCoverage'
  )
)

names(dfd) = c('BG', 'MG', 'LG', 'BC', 'MC', 'LC')

# ----

mkggcorr(dfd, 'pearson')
ggsave('goalMetricsVsResults-gg-pears.pdf', width = width, height = height)

mkggcorr(dfd, 'spearman')
ggsave('goalMetricsVsResults-gg-spear.pdf', width = width, height = height)

# ----

mPears = cor(dfd, use='complete', method='pearson')
mSpear = cor(dfd, use='complete', method='spearman')

pdf('goalMetricsVsResults-pears.pdf', width=width, height=height)
corrplot(mPears, type='lower')
dev.off()

pdf('goalMetricsVsResults-spear.pdf', width=width, height=height)
corrplot(mSpear, type='lower')
dev.off()

pdf('goalMetricsVsResults-mix-pears.pdf', width=width, height=height)
corrplot.mixed(mPears)
dev.off()

pdf('goalMetricsVsResults-mix-spear.pdf', width=width, height=height)
corrplot.mixed(mSpear)
dev.off()

# Method Coverage vs. Method Goals
cutBreaks = seq(0.0, 1.0, by=0.1)
cutLabels = seq(10, 100, by = 10)

dfa$cuts = cut(dfa$MethodCoverage, breaks=cutBreaks, include.lowest = T,
               labels = cutLabels)
s = summarySE(dfa, measurevar = 'MethodGoals', groupvars = c('cuts'))
s = s[complete.cases(s),]

ggplot(s, aes(cuts, MethodGoals)) +
  geom_point() +
  geom_errorbar(aes(ymin=MethodGoals-se, ymax=MethodGoals+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('MG') +
  xlab('MC [%]') +
  #scale_x_continuous(breaks = seq(0,10)) +
  theme_minimal()
ggsave('goalMetricsVsResults-mg-by-mc.pdf',width=width3, height=height3)

# Branch Coverage and Branch Goals

dfa$cuts = cut(dfa$BranchCoverage, breaks=cutBreaks, labels=cutLabels, include.lowest = T)
s = summarySE(dfa, measurevar = 'BranchGoals', groupvars = c('cuts'))
s = s[complete.cases(s),]

ggplot(s, aes(cuts, BranchGoals)) +
  geom_point() +
  geom_errorbar(aes(ymin=BranchGoals-se, ymax=BranchGoals+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('BG') +
  xlab('BC [%]') +
  #scale_x_continuous(breaks = seq(0,10)) +
  theme_minimal()
ggsave('goalMetricsVsResults-bg-by-bc.pdf',width=width3, height=height3)


# Line Coverage vs. Line Goals

dfa$cuts = cut(dfa$LineCoverage, breaks=cutBreaks, labels=cutLabels, include.lowest = T)
s = summarySE(dfa, measurevar = 'LineGoals', groupvars = c('cuts'))
s = s[complete.cases(s),]

ggplot(s, aes(cuts, LineGoals)) +
  geom_point() +
  geom_errorbar(aes(ymin=LineGoals-se, ymax=LineGoals+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('LG') +
  xlab('LC [%]') +
  #scale_x_continuous(breaks = seq(0,10)) +
  theme_minimal()
ggsave('goalMetricsVsResults-lg-by-lc.pdf',width=width3, height=height3)


# # Number of Testcases
# 
# s <- summarySE(dfa, measurevar = 'TestCnt', groupvars = c('MethodGoals'))
# s <- filter(s, N >= 30)
# 
# ggplot(s, aes(MethodGoals, TestCnt)) +
#   geom_point() +
#   geom_errorbar(aes(ymin=TestCnt-se, ymax=TestCnt+se), colour="black", width=0.1*nrow(s)/width) +
#   ylab('TC') +
#   xlab('MG') +
#   #scale_x_continuous(breaks = seq(0,10)) +
#   theme_minimal()
# ggsave('goalMetricsVsResults-tc-by-mg.pdf', width=width3, height=height3)
# 
# s <- summarySE(dfa, measurevar = 'TestCnt', groupvars = c('BranchGoals'))
# s <- filter(s, N >= 30)
# 
# ggplot(s, aes(BranchGoals, TestCnt)) +
#   geom_point() +
#   geom_errorbar(aes(ymin=TestCnt-se, ymax=TestCnt+se), colour="black", width=0.1*nrow(s)/width) +
#   ylab('TC') +
#   xlab('BG') +
#   #scale_x_continuous(breaks = seq(0,10)) +
#   theme_minimal()
# ggsave('goalMetricsVsResults-tc-by-bg.pdf', width=width3, height=height3)
# 
# s <- summarySE(dfa, measurevar = 'TestCnt', groupvars = c('LineGoals'))
# s <- filter(s, N >= 30)
# 
# ggplot(s, aes(LineGoals, TestCnt)) +
#   geom_point() +
#   geom_errorbar(aes(ymin=TestCnt-se, ymax=TestCnt+se), colour="black", width=0.1*nrow(s)/width) +
#   ylab('TC') +
#   xlab('LG') +
#   #scale_x_continuous(breaks = seq(0,10)) +
#   theme_minimal()
# ggsave('goalMetricsVsResults-tc-by-lg.pdf', width=width3, height=height3)


####

s = summarySE(dfa, measurevar = 'BranchCoverage', groupvars = c('BranchGoals'))
s = subset(s, N >= minObs)

ggplot(s, aes(BranchGoals, BranchCoverage)) +
  geom_point() +
  geom_errorbar(aes(ymin=BranchCoverage-se, ymax=BranchCoverage+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('BC') +
  xlab('BG') +
  ylim(0.0, 1.0) +
  theme_minimal()
ggsave('goalMetricsVsResults-bc-by-bg.pdf', width=width3, height=height3)

s = summarySE(dfa, measurevar = 'MethodCoverage', groupvars = c('MethodGoals'))
s = subset(s, N >= minObs)

ggplot(s, aes(MethodGoals, MethodCoverage)) +
  geom_point() +
  geom_errorbar(aes(ymin=MethodCoverage-se, ymax=MethodCoverage+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('MC') +
  xlab('MG') +
  ylim(0.0, 1.0) +
  theme_minimal()
ggsave('goalMetricsVsResults-mc-by-mg.pdf', width=width3, height=height3)


s = summarySE(dfa, measurevar = 'LineCoverage', groupvars = c('LineGoals'))
s = subset(s, N >= minObs)

ggplot(s, aes(LineGoals, LineCoverage)) +
  geom_point() +
  geom_errorbar(aes(ymin=LineCoverage-se, ymax=LineCoverage+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('LC') +
  xlab('LG') +
  ylim(0.0, 1.0) +
  theme_minimal()
ggsave('goalMetricsVsResults-lc-by-lg.pdf', width=width3, height=height3)
