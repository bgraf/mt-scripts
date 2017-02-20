library(Rmisc)
source('util.R')
library(corrplot)
source('corr.R')

df = readDataset()
df$NFF = with(df, FF / (BranchGoals + LineGoals + MethodGoals))
dfa = aggByClass(df)

dfa$NEs = with(dfa, mIFNEs + mICMPNEs + mACMPNEs + mIFNONNULLs)
dfa$EQs = with(dfa, mICMPEQs + mACMPEQs + mIFNULLs + mIFEQs)
dfa$EQNEs = with(dfa, NEs + EQs)
dfa$CMPs = with(dfa, mBranchInstrCnt - EQNEs)

dfa$BIs = dfa$mBranchInstrCnt
dfa$NI = dfa$NumIter

dfa$FBP = with(dfa, ifelse(BIs > 0, EQNEs / BIs, 0))

dfd = subset(
  dfa,
  select = c(
    'BIs', 'NA.', 'NLE', 'NOS',
    'CR', 'PIC', 'FF', 'NI'
  )
)

# ----

mkggcorr(dfd, 'pearson')
ggsave('otherMetricsVsEvolvability-gg-pears.pdf', width = width, height = height)

mkggcorr(dfd, 'spearman')
ggsave('otherMetricsVsEvolvability-gg-spear.pdf', width = width, height = height)

# ----

# --- correlations

mPears = cor(dfd, use = 'complete', method = 'pearson')
mSpear = cor(dfd, use = 'complete', method = 'spearman')

# --- plots

pdf('otherMetricsVsEvolvability-mix-pears.pdf', width = width, height = height)
corrplot.mixed(mPears)
dev.off()

pdf('otherMetricsVsEvolvability-mix-spear.pdf', width = width, height = height)
corrplot.mixed(mSpear)
dev.off()

# --- analysis

# metrics = c('BIs', 'FBP', 'NA.', 'NLE', 'NOS')
# 
# tail(abs(mPears) >= abs(mSpear), 4)[,metrics]
# 
# tail(abs(mSpear) > 0.8, 4)[,metrics]
# 
# tail(abs(mSpear) > 0.3, 4)[,metrics]

#------------------------------------------------------------------------------
# --- CR vs. {BIs, EQNEs, NLE}

crLim = ylim(0, 0.7)

sBI = summarySE(dfd, measurevar = 'CR', groupvars = 'BIs', na.rm = T)
sBI = filter(sBI, N >= minObs)

ggplot(sBI, aes(BIs, CR)) +
  geom_point() +
  geom_errorbar(aes(ymin=CR-se, ymax=CR+se), colour="black", width=0.1*nrow(sBI)/width) +
  ylab('CR') +
  xlab('BIs') +
  crLim +
  theme_minimal()
ggsave('otherMetricsVsEvolvability-cr-by-bi.pdf', width = width3, height = height3)




sNLE = summarySE(dfd, measurevar = 'CR', groupvars = 'NLE', na.rm = T)
sNLE = filter(sNLE, N >= minObs)

ggplot(sNLE, aes(NLE, CR)) +
  geom_point() +
  geom_errorbar(aes(ymin=CR-se, ymax=CR+se), colour="black", width=0.1*nrow(sNLE)/width) +
  ylab('CR') +
  xlab('NLE') +
  crLim +
  theme_minimal()
ggsave('otherMetricsVsEvolvability-cr-by-nle.pdf', width = width3, height = height3)

#------------------------------------------------------------------------------
# --- PIC vs. {BIs, EQNEs, NLE}

picLim = ylim(0, 0.7)

sBI = summarySE(dfd, measurevar = 'PIC', groupvars = 'BIs', na.rm = T)
sBI = filter(sBI, N >= minObs)

ggplot(sBI, aes(BIs, PIC)) +
  geom_point() +
  geom_errorbar(aes(ymin=PIC-se, ymax=PIC+se), colour="black", width=0.1*nrow(sBI)/width) +
  ylab('PIC') +
  xlab('BIs') +
  picLim +
  theme_minimal()
ggsave('otherMetricsVsEvolvability-pic-by-bi.pdf', width = width3, height = height3)


sNLE = summarySE(dfd, measurevar = 'PIC', groupvars = 'NLE', na.rm = T)
sNLE = filter(sNLE, N >= minObs)

ggplot(sNLE, aes(NLE, PIC)) +
  geom_point() +
  geom_errorbar(aes(ymin=PIC-se, ymax=PIC+se), colour="black", width=0.1*nrow(sNLE)/width) +
  ylab('PIC') +
  xlab('NLE') +
  picLim +
  theme_minimal()
ggsave('otherMetricsVsEvolvability-pic-by-nle.pdf', width = width3, height = height3)

#------------------------------------------------------------------------------
# --- FF vs. {BIs, EQNEs, NLE}

ffLim = ylim(0, max(dfd$FF))

sBI = summarySE(dfd, measurevar = 'FF', groupvars = 'BIs', na.rm = T)
sBI = filter(sBI, N >= minObs)

ggplot(sBI, aes(BIs, FF)) +
  geom_point() +
  geom_errorbar(aes(ymin=FF-se, ymax=FF+se), colour="black", width=0.1*nrow(sBI)/width) +
  ylab('FF') +
  xlab('BIs') +
  ffLim +
  theme_minimal()
ggsave('otherMetricsVsEvolvability-ff-by-bi.pdf', width = width3, height = height3)


sNLE = summarySE(dfd, measurevar = 'FF', groupvars = 'NLE', na.rm = T)
sNLE = filter(sNLE, N >= minObs)

ggplot(sNLE, aes(NLE, FF)) +
  geom_point() +
  geom_errorbar(aes(ymin=FF-se, ymax=FF+se), colour="black", width=0.1*nrow(sNLE)/width) +
  ylab('FF') +
  xlab('NLE') +
  ffLim +
  theme_minimal()
ggsave('otherMetricsVsEvolvability-ff-by-nle.pdf', width = width3, height = height3)

#------------------------------------------------------------------------------
# NA vs. {CR, PIC, FF}

sCR = summarySE(dfd, measurevar = 'CR', groupvars = 'NA.', na.rm = T)
sCR = filter(sCR, N >= minObs)

ggplot(sCR, aes(NA., CR)) +
  geom_point() +
  geom_errorbar(aes(ymin=CR-se, ymax=CR+se), colour="black", width=0.1*nrow(sCR)/width) +
  ylab('CR') +
  xlab('NA') +
  theme_minimal()
ggsave('otherMetricsVsEvolvability-cr-by-na.pdf', width = width3, height = height3)

# ---

sPIC = summarySE(dfd, measurevar = 'PIC', groupvars = 'NA.', na.rm = T)
sPIC = filter(sPIC, N >= minObs)

ggplot(sPIC, aes(NA., PIC)) +
  geom_point() +
  geom_errorbar(aes(ymin=PIC-se, ymax=PIC+se), colour="black", width=0.1*nrow(sPIC)/width) +
  ylab('PIC') +
  xlab('NA') +
  theme_minimal()
ggsave('otherMetricsVsEvolvability-pic-by-na.pdf', width = width3, height = height3)

# ---

sFF = summarySE(dfd, measurevar = 'FF', groupvars = 'NA.', na.rm = T)
sFF = filter(sFF, N >= minObs)

ggplot(sFF, aes(NA., FF)) +
  geom_point() +
  geom_errorbar(aes(ymin=FF-se, ymax=FF+se), colour="black", width=0.1*nrow(sFF)/width) +
  ylab('FF') +
  xlab('NA') +
  theme_minimal()
ggsave('otherMetricsVsEvolvability-ff-by-na.pdf', width = width3, height = height3)


#------------------------------------------------------------------------------

cutBreaks = seq(0.0, 1.0, by = 0.1)

dfbi = subset(dfa, BIs > 0)
dfbi$FBP = with(dfbi, EQNEs / BIs)
dfbi$FBPi = cut(dfbi$FBP, breaks=cutBreaks, include.lowest = T, labels = 10*seq(1, 10, by = 1))

sFBP = summarySE(dfbi, measurevar = 'CR', groupvars = 'FBPi', na.rm = T)
sFBP = filter(sFBP, N >= minObs)

ggplot(sFBP, aes(factor(FBPi), CR)) +
  geom_point() +
  geom_errorbar(aes(ymin=CR-se, ymax=CR+se), colour="black", width=0.1*nrow(sFBP)/width) +
  ylab('CR') +
  xlab('FBP [%]') +
  crLim +
  theme_minimal()
ggsave('otherMetricsVsEvolvability-cr-by-fbpi.pdf', width = width3, height = height3)

dfbi$FBPi = cut(dfbi$FBP, breaks=cutBreaks, include.lowest = T, labels = 10*seq(1, 10, by = 1))
sFBP = summarySE(dfbi, measurevar = 'NFF', groupvars = 'FBPi', na.rm = T)
sFBP = filter(sFBP, N >= minObs)

ggplot(sFBP, aes(factor(FBPi), NFF)) +
  geom_point() +
  geom_errorbar(aes(ymin=NFF-se, ymax=NFF+se), colour="black", width=0.1*nrow(sFBP)/width) +
  ylab('NFF') +
  xlab('FBP [%]') +
  ffLim +
  theme_minimal()
ggsave('otherMetricsVsEvolvability-nff-by-fbpi.pdf', width = width3, height = height3)

dfbi$FBPi = cut(dfbi$FBP, breaks=cutBreaks, include.lowest = T, labels = 10*seq(1, 10, by = 1))
sFBP = summarySE(dfbi, measurevar = 'PIC', groupvars = 'FBPi', na.rm = T)
sFBP = filter(sFBP, N >= minObs)

ggplot(sFBP, aes(factor(FBPi), PIC)) +
  geom_point() +
  geom_errorbar(aes(ymin=PIC-se, ymax=PIC+se), colour="black", width=0.1*nrow(sFBP)/width) +
  ylab('PIC') +
  xlab('FBP [%]') +
  picLim +
  theme_minimal()
ggsave('otherMetricsVsEvolvability-pic-by-fbpi.pdf', width = width3, height = height3)

#------------------------------------------------------------------------------

median(subset(dfbi, FBP > 0.9)$BIs)
median(subset(dfbi, FBP > 0.1 & FBP <= 0.9)$BIs)
median(subset(dfbi, FBP <= 0.1)$BIs)

median(subset(dfbi, FBP <= 0.1)$NLE)

ggplot(subset(dfbi, FBP <= 0.1), aes(BIs)) + stat_ecdf()
ggplot(subset(dfbi, FBP > 0.9), aes(BIs)) + stat_ecdf()

ggplot(NULL,  aes(BIs)) + 
  stat_ecdf(data=subset(dfbi, FBP <= 0.1)) + 
  stat_ecdf(data=subset(dfbi, FBP > 0.9), color='red') + 
  stat_ecdf(data=subset(dfbi, FBP <= 0.9 & FBP > 0.1), color='blue') + 
  scale_x_continuous(trans = "log10", breaks = c(1,10,100,1000))

ggplot(NULL,  aes(NLE)) + 
  stat_ecdf(data=subset(dfbi, FBP <= 0.1)) + 
  stat_ecdf(data=subset(dfbi, FBP > 0.9), color='red') + 
  stat_ecdf(data=subset(dfbi, FBP <= 0.9 & FBP > 0.1), color='blue') + 
  scale_x_continuous(trans = "log10", breaks = c(1,2,3,4,5)) + 
  theme_minimal()

dfbi$FBPgroup = ifelse(dfbi$FBP > 0.9, 'H', ifelse(dfbi$FBP <= 0.1, 'L', 'M'))

dfbi$FBPgroup = factor(dfbi$FBPgroup, levels = c('H', 'M', 'L'))

ggplot(dfbi, aes(BIs, color=FBPgroup)) + 
  stat_ecdf() + 
  theme_minimal() +
  scale_color_manual(
    values = c('H' = 'red', 'L' = 'blue', 'M' = 'grey10'),
    labels = c('(0.9, 1.0]', '(0.1, 0.9]', '[0, 0.1]'),
    name = NULL
    ) +
  scale_x_continuous(trans='log10', breaks=c(1, 10, 100, 1000))
