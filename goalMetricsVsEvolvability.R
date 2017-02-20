source('util.R')
library(corrplot)
source('corr.R')

df = readDataset()
df$NFF = df$FF / (df$BranchGoals + df$LineGoals + df$MethodGoals)
dfa = aggByClass(df)

# --- select variables

dfd = subset(
  dfa, 
  select = c('BranchGoals', 'MethodGoals', 'LineGoals',
             'NFF', 'CR', 'PIC', 'FF', 'NumIter'))
names(dfd) = c('BG', 'MG', 'LG',
               'NFF', 'CR', 'PIC', 'FF', 'NI')

# ----

mkggcorr(dfd, 'pearson')
ggsave('goalMetricsVsEvolvability-gg-pears.pdf', width = width, height = height)

mkggcorr(dfd, 'spearman')
ggsave('goalMetricsVsEvolvability-gg-spear.pdf', width = width, height = height)

# ----

# --- correlations

mPears = cor(dfd, use = 'complete', method = 'pearson')
mSpear = cor(dfd, use = 'complete', method = 'spearman')

# --- plots

pdf('goalMetricsVsEvolvability-mix-pears.pdf', width = width, height = height)
corrplot.mixed(mPears)
dev.off()

pdf('goalMetricsVsEvolvability-mix-spear.pdf', width = width, height = height)
corrplot.mixed(mSpear)
dev.off()

# --- analysis

metrics = c('BG', 'MG', 'LG')

tail(abs(mPears) >= abs(mSpear), 5)[,metrics]

tail(abs(mSpear) > 0.8, 5)[,metrics]

tail(abs(mSpear) > 0.3, 5)[,metrics]

# --- Pair-wise analysis
library(Rmisc)

# --- BG vs. {CR, PIC}

sCR = summarySE(dfd, measurevar = 'CR', groupvars = 'BG', na.rm = T)
sCR = filter(sCR, N >= minObs)

ggplot(sCR, aes(BG, CR)) +
  geom_point() +
  geom_errorbar(aes(ymin=CR-se, ymax=CR+se), colour="black", width=0.1*nrow(sCR)/width) +
  ylab('CR') +
  xlab('BG') +
  theme_minimal()
ggsave('goalMetricsVsEvolvability-cr-by-bg.pdf', width = width3, height = height3)


sPIC = summarySE(dfd, measurevar = 'PIC', groupvars = 'BG', na.rm = T)
sPIC = filter(sPIC, N >= minObs)

ggplot(sPIC, aes(BG, PIC)) +
  geom_point() +
  geom_errorbar(aes(ymin=PIC-se, ymax=PIC+se), colour="black", width=0.1*nrow(sPIC)/width) +
  ylab('PIC') +
  xlab('BG') +
  ylim(0,0.7) +
  theme_minimal()
ggsave('goalMetricsVsEvolvability-pic-by-bg.pdf', width = width3, height = height3)


sFF = summarySE(dfd, measurevar = 'FF', groupvars = 'BG', na.rm = T)
sFF = filter(sFF, N >= minObs)

ggplot(sFF, aes(BG, FF)) +
  geom_point() +
  geom_errorbar(aes(ymin=FF-se, ymax=FF+se), colour="black", width=0.1*nrow(sFF)/width) +
  ylab('FF') +
  xlab('BG') +
  theme_minimal()
ggsave('goalMetricsVsEvolvability-ff-by-bg.pdf', width = width3, height = height3)

# --- MG vs. {CR, PIC, FF}

sCR = summarySE(dfd, measurevar = 'CR', groupvars = 'MG', na.rm=T)
sCR = filter(sCR, N >= minObs)

ggplot(sCR, aes(MG, CR)) +
  geom_point() +
  geom_errorbar(aes(ymin=CR-se, ymax=CR+se), colour="black", width=0.1*nrow(sCR)/width) +
  ylab('CR') +
  xlab('MG') +
  theme_minimal()
ggsave('goalMetricsVsEvolvability-cr-by-mg.pdf', width = width3, height = height3)


sPIC = summarySE(dfd, measurevar = 'PIC', groupvars = 'MG', na.rm = T)
sPIC = filter(sPIC, N >= minObs)

ggplot(sPIC, aes(MG, PIC)) +
  geom_point() +
  geom_errorbar(aes(ymin=PIC-se, ymax=PIC+se), colour="black", width=0.1*nrow(sPIC)/width) +
  ylab('PIC') +
  xlab('MG') +
  ylim(0,0.7) +
  theme_minimal()
ggsave('goalMetricsVsEvolvability-pic-by-mg.pdf', width = width3, height = height3)


sFF = summarySE(dfd, measurevar = 'FF', groupvars = 'MG', na.rm = T)
sFF = filter(sFF, N >= minObs)

ggplot(sFF, aes(MG, FF)) +
  geom_point() +
  geom_errorbar(aes(ymin=FF-se, ymax=FF+se), colour="black", width=0.1*nrow(sFF)/width) +
  ylab('FF') +
  xlab('MG') +
  theme_minimal()
ggsave('goalMetricsVsEvolvability-ff-by-mg.pdf', width = width3, height = height3)

# --- BG and WMC_CC

paste('Pearson: ', cor(dfa$BranchGoals, dfa$WMC, use='complete', method = 'pearson'))
paste('Spearman:', cor(dfa$BranchGoals, dfa$WMC, use='complete', method = 'spearman'))

