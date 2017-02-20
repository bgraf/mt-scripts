source('util.R')
library(corrplot)
source('corr.R')

df = readDataset()
df$NFF = df$FF / (df$BranchGoals + df$LineGoals + df$MethodGoals)

dfa = aggByClass(df)
dfd = subset(dfa, select = c(colMetric, 'NFF', colEvo))
dfe = subset(dfd, select=-c(NSC, NSCFCS, cWMC, cDIT, cNOC, cCBO, cRFC, cLCOM, cCa, cNPM))

# Testability metrics

testabilityMetrics = c('WMC', 'DIT', 'RFC', 'LCOM5', 'CBO')

d = subset(dfe, select=c(testabilityMetrics, 'NFF', setdiff(colEvo, c('NSC', 'NSCFCS'))))
#d = rename(d, replace = c('NumIter' = 'NI'))
d$NI = d$NumIter
d$NumIter = NULL

# ----

mkggcorr(d, 'pearson')
ggsave('metricsVsEvolvability-testability-gg-pears.pdf', width = width, height = height)

mkggcorr(d, 'spearman')
ggsave('metricsVsEvolvability-testability-gg-spear.pdf', width = width, height = height)

# ----


mPears = cor(d, use='complete', method='pearson')
mSpear = cor(d, use='complete', method='spearman')

pdf('metricsVsEvolvability-testability-mix-pears.pdf', width=width, height=height)
corrplot.mixed(mPears)
dev.off()

pdf('metricsVsEvolvability-testability-mix-spear.pdf', width=width, height=height)
corrplot.mixed(mSpear)
dev.off()

## Description

# |r| < |p|
tail(abs(mPears) >= abs(mSpear), 5)[,testabilityMetrics]

# |p| > 0.3 AND |r| > 0.3
tail(abs(mSpear) > 0.3 & abs(mPears) > 0.3, 5)[,testabilityMetrics]

# |p| > 0.3
tail(abs(mSpear) > 0.3, 5)[,testabilityMetrics]

# |p| > 0.8
tail(abs(mSpear) > 0.8, 5)[,testabilityMetrics]


m = mSpear
m[abs(mSpear) <= 0.3] = NA

# |r| < |p| for |p| > 0.3
tail(abs(mPears) >= abs(m), 5)[,testabilityMetrics]

## ------------------------------------ 
## Analysis
## ------------------------------------ 

library(Rmisc)

# --- {WMC, RFC, CBO} vs. FF

s = summarySE(d, measurevar = 'FF', groupvars = 'WMC', na.rm = T)
s = filter(s, N >= minObs)

ggplot(s, aes(WMC, FF)) +
  geom_point() +
  geom_errorbar(aes(ymin=FF-se, ymax=FF+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('FF') +
  xlab('WMC') +
  theme_minimal()
ggsave('metricsVsEvolvability-ff-by-wmc.pdf', width = width3, height = height3)

sRFC = summarySE(d, measurevar = 'FF', groupvars = 'RFC', na.rm = T)
sRFC = filter(sRFC, N >= minObs)

ggplot(sRFC, aes(RFC, FF)) +
  geom_point() +
  geom_errorbar(aes(ymin=FF-se, ymax=FF+se), colour="black", width=0.1*nrow(sRFC)/width) +
  ylab('FF') +
  xlab('RFC') +
  theme_minimal()
ggsave('metricsVsEvolvability-ff-by-rfc.pdf', width = width3, height = height3)

sCBO = summarySE(d, measurevar = 'FF', groupvars = 'CBO', na.rm = T)
sCBO = filter(sCBO, N >= minObs)

ggplot(sCBO, aes(CBO, FF)) +
  geom_point() +
  geom_errorbar(aes(ymin=FF-se, ymax=FF+se), colour="black", width=0.1*nrow(sCBO)/width) +
  ylab('FF') +
  xlab('CBO') +
  theme_minimal()
ggsave('metricsVsEvolvability-ff-by-cbo.pdf', width = width3, height = height3)

# --- {WMC,RFC,CBO} vs. PIC

sWMC = summarySE(d, measurevar = 'PIC', groupvars = 'WMC', na.rm = T)
sWMC = filter(sWMC, N >= minObs)

ggplot(sWMC, aes(WMC, PIC)) +
  geom_point() +
  geom_errorbar(aes(ymin=PIC-se, ymax=PIC+se), colour="black", width=0.1*nrow(sWMC)/width) +
  ylab('PIC') +
  xlab('WMC') +
  ylim(0, 0.7) +
  theme_minimal()
ggsave('metricsVsEvolvability-pic-by-wmc.pdf', width = width3, height = height3)

sRFC = summarySE(d, measurevar = 'PIC', groupvars = 'RFC', na.rm = T)
sRFC = filter(sRFC, N >= minObs)

ggplot(sRFC, aes(RFC, PIC)) +
  geom_point() +
  geom_errorbar(aes(ymin=PIC-se, ymax=PIC+se), colour="black", width=0.1*nrow(sRFC)/width) +
  ylab('PIC') +
  xlab('RFC') +
  ylim(0, 0.7) +
  theme_minimal()
ggsave('metricsVsEvolvability-pic-by-rfc.pdf', width = width3, height = height3)

sCBO = summarySE(d, measurevar = 'PIC', groupvars = 'CBO', na.rm = T)
sCBO = filter(sCBO, N >= minObs)

ggplot(sCBO, aes(CBO, PIC)) +
  geom_point() +
  geom_errorbar(aes(ymin=PIC-se, ymax=PIC+se), colour="black", width=0.1*nrow(sCBO)/width) +
  ylab('PIC') +
  xlab('CBO') +  
  ylim(0, 0.7) +
  theme_minimal()
ggsave('metricsVsEvolvability-pic-by-cbo.pdf', width = width3, height = height3)


# --- {WMC,RFC,CBO} vs. CR

sWMCcr = summarySE(d, measurevar = 'CR', groupvars = 'WMC', na.rm = T)
sWMCcr = filter(sWMCcr, N >= minObs)

ggplot(sWMCcr, aes(WMC, CR)) +
  geom_point() +
  geom_errorbar(aes(ymin=CR-se, ymax=CR+se), colour="black", width=0.1*nrow(sWMCcr)/width) +
  ylab('CR') +
  xlab('WMC') +
  ylim(0, 1.0) +
  theme_minimal()
ggsave('metricsVsEvolvability-cr-by-wmc.pdf', width = width3, height = height3)

sRFCcr = summarySE(d, measurevar = 'CR', groupvars = 'RFC', na.rm = T)
sRFCcr = filter(sRFCcr, N >= minObs)

ggplot(sRFCcr, aes(RFC, CR)) +
  geom_point() +
  geom_errorbar(aes(ymin=CR-se, ymax=CR+se), colour="black", width=0.1*nrow(sRFCcr)/width) +
  ylab('CR') +
  xlab('RFC') +
  ylim(0, 1.0) +
  theme_minimal()
ggsave('metricsVsEvolvability-cr-by-rfc.pdf', width = width3, height = height3)

sCBOcr = summarySE(d, measurevar = 'CR', groupvars = 'CBO', na.rm = T)
sCBOcr = filter(sCBOcr, N >= minObs)

ggplot(sCBOcr, aes(CBO, CR)) +
  geom_point() +
  geom_errorbar(aes(ymin=CR-se, ymax=CR+se), colour="black", width=0.1*nrow(sCBOcr)/width) +
  ylab('CR') +
  xlab('CBO') +
  ylim(0, 1.0) +
  theme_minimal()
ggsave('metricsVsEvolvability-cr-by-cbo.pdf', width = width3, height = height3)

# --- PIC vs. CR for {WMC,RFC,CBO}


# --- LCOM vs. {CR, PIC, FF}

sCR = summarySE(d, measurevar = 'CR', groupvars = 'LCOM5', na.rm = T)
sCR = filter(sCR, N >= minObs)

ggplot(sCR, aes(LCOM5, CR)) +
  geom_point() +
  geom_errorbar(aes(ymin=CR-se, ymax=CR+se), colour="black", width=0.1*nrow(sCR)/width) +
  ylab('CR') +
  xlab('LCOM5') +
  theme_minimal()

sPIC = summarySE(d, measurevar = 'PIC', groupvars = 'LCOM5', na.rm = T)
sPIC = filter(sPIC, N >= minObs)

ggplot(sPIC, aes(LCOM5, PIC)) +
  geom_point() +
  geom_errorbar(aes(ymin=PIC-se, ymax=PIC+se), colour="black", width=0.1*nrow(sPIC)/width) +
  ylab('PIC') +
  xlab('LCOM5') +
  theme_minimal()

sFF = summarySE(d, measurevar = 'FF', groupvars = 'LCOM5', na.rm = T)
sFF = filter(sFF, N >= minObs)

ggplot(sFF, aes(LCOM5, FF)) +
  geom_point() +
  geom_errorbar(aes(ymin=FF-se, ymax=FF+se), colour="black", width=0.1*nrow(sFF)/width) +
  ylab('FF') +
  xlab('LCOM5') +
  theme_minimal()
