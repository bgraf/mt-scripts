#==============================================================================
# CR, PIC vs. BC, MC, LC
#==============================================================================

library(Rmisc)
source('util.R')

df = readDataset()
dfa = aggByClass(df)

#------------------------------------------------------------------------------
# Data preparation
#------------------------------------------------------------------------------

dfa$BC = dfa$BranchCoverage
dfa$MC = dfa$MethodCoverage
dfa$LC = dfa$LineCoverage

dfa = subset(dfa, !is.na(BC) & !is.na(MC) & !is.na(LC))

cutBreaks = seq(0.0, 1.0, by=0.1)
cutLabels = seq(10, 100, by = 10)

dfa$BCcut = cut(dfa$BC, breaks = cutBreaks, include.lowest = T, labels = cutLabels)
dfa$MCcut = cut(dfa$MC, breaks = cutBreaks, include.lowest = T, labels = cutLabels)
dfa$LCcut = cut(dfa$LC, breaks = cutBreaks, include.lowest = T, labels = cutLabels)

makeSummary = function(data, measure, group) {
  s = summarySE(data, measurevar = measure, groupvars = group, na.rm = T)
  s[!is.na(s[, group]), ]
}

#------------------------------------------------------------------------------
# ECDF of different cuts of MC
#------------------------------------------------------------------------------

ggplot(dfa, aes(MCcut, MethodCovered)) + 
  geom_boxplot(fill=stdGrey) + 
  coord_cartesian(ylim = c(0, 40)) +
  labs(x = 'MC [%]', y = 'überdeckte Methoden') +
  theme_minimal()
ggsave('evoVsCoverage-box-mc-to-methodcovered.pdf', width = width3, height = height3)

ggplot(dfa, aes(MCcut, BranchGoals)) + 
  geom_boxplot(fill=stdGrey) + 
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = 'MC [%]', y = 'Anzahl der Zweige') +
  theme_minimal()
ggsave('evoVsCoverage-box-mc-to-branchgoals.pdf', width = width3, height = height3)

ggplot(dfa, aes(MCcut, BranchGoals * MC)) + 
  geom_boxplot(fill=stdGrey) + 
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = 'MC [%]', y = 'Anzahl der Zweige * MC') +
  theme_minimal()
ggsave('evoVsCoverage-box-mc-to-branchgoalsadjusted.pdf', width = width3, height = height3)


#------------------------------------------------------------------------------
# CR by Coverage
#------------------------------------------------------------------------------

crLim = ylim(0, 0.7)

s = makeSummary(dfa, 'CR', 'BCcut')

ggplot(s, aes(factor(BCcut), CR)) +
  geom_point() +
  geom_errorbar(aes(ymin=CR-se, ymax=CR+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('CR') +
  xlab('BC [%]') +
  crLim +
  theme_minimal()

ggsave('evoVsCoverage-cr-by-bc.pdf', width = width3, height = height3)

#----------

s = makeSummary(dfa, 'CR', 'MCcut')

ggplot(s, aes(factor(MCcut), CR)) +
  geom_point() +
  geom_errorbar(aes(ymin=CR-se, ymax=CR+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('CR') +
  xlab('MC [%]') +
  crLim +
  theme_minimal()

ggsave('evoVsCoverage-cr-by-mc.pdf', width = width3, height = height3)

#----------

s = makeSummary(dfa, 'CR', 'LCcut')

ggplot(s, aes(factor(LCcut), CR)) +
  geom_point() +
  geom_errorbar(aes(ymin=CR-se, ymax=CR+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('CR') +
  xlab('LC [%]') +
  crLim +
  theme_minimal()

ggsave('evoVsCoverage-cr-by-lc.pdf', width = width3, height = height3)


#------------------------------------------------------------------------------
# PIC by Coverage
#------------------------------------------------------------------------------

picLim = ylim(0, 0.7)

s = makeSummary(dfa, 'PIC', 'BCcut')

ggplot(s, aes(factor(BCcut), PIC)) +
  geom_point() +
  geom_errorbar(aes(ymin=PIC-se, ymax=PIC+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('PIC') +
  xlab('BC [%]') +
  picLim +
  theme_minimal()

ggsave('evoVsCoverage-pic-by-bc.pdf', width = width3, height = height3)

#----------

s = makeSummary(dfa, 'PIC', 'MCcut')

ggplot(s, aes(factor(MCcut), PIC)) +
  geom_point() +
  geom_errorbar(aes(ymin=PIC-se, ymax=PIC+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('PIC') +
  xlab('MC [%]') +
  picLim +
  theme_minimal()

ggsave('evoVsCoverage-pic-by-mc.pdf', width = width3, height = height3)

#----------

s = makeSummary(dfa, 'PIC', 'LCcut')

ggplot(s, aes(factor(LCcut), PIC)) +
  geom_point() +
  geom_errorbar(aes(ymin=PIC-se, ymax=PIC+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('PIC') +
  xlab('LC [%]') +
  picLim +
  theme_minimal()

ggsave('evoVsCoverage-pic-by-lc.pdf', width = width3, height = height3)

#------------------------------------------------------------------------------
# Low PIC for small MC, why?
#------------------------------------------------------------------------------

dLowMc = subset(dfa, MC <= 0.5)
dHighMc = subset(dfa, MC > 0.5)


brs = seq(0, 10, by = 1)

ggplot(dfa, aes(MethodCovered, color = MC <= 0.5)) +
  stat_ecdf() +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  coord_cartesian(xlim = c(0, 10)) +
  scale_color_brewer(
    labels = c('MC > 0.5', 'MC <= 0.5'),
    palette = 'Set1',
    name = NULL
  ) +
  labs(x = 'überdeckte Methoden', y = 'ECDF [-]') +
  theme_minimal() +
  theme(legend.position=c(0.85,0.2), legend.background = element_rect(fill = "white"))

ggsave('evoVsCoverage-ecdf-methodcovered.pdf', width = width, height = height3)

ggplot(dfa, aes(BranchGoals*MC, color = MC <= 0.5)) +
  stat_ecdf() +
  scale_x_continuous(breaks = 10*seq(0, 10, by = 1)) +
  coord_cartesian(xlim = c(0, 100)) +
  scale_color_brewer(
    labels = c('MC > 0.5', 'MC <= 0.5'),
    palette = 'Set1',
    name = NULL
  ) +
  labs(x = 'Anzahl der Zweige', y = 'ECDF [-]') +
  theme_minimal() + 
  theme(legend.position=c(0.85,0.2), legend.background = element_rect(fill = "white"))

ggsave('evoVsCoverage-ecdf-branchgoals.pdf', width = width, height = height3)

# ----

dfaa = subset(dfa, (MC > 0.4 & MC <= 0.5) | (MC > 0.6) & (MC <= 0.7))

ggplot(dfaa, aes(MethodCovered, color = MC <= 0.5)) +
  stat_ecdf() +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  coord_cartesian(xlim = c(0, 10)) +
  scale_color_brewer(
    labels = c(expression({0.6 < MC} <= 0.7), expression({0.4 < MC} <= 0.5)),
    palette = 'Set1',
    name = NULL
  ) +
  labs(x = 'überdeckte Methoden', y = 'ECDF [-]') +
  theme_minimal() +
  theme(legend.position=c(0.85,0.2), legend.background = element_rect(fill = "white"))

ggsave('evoVsCoverage-ecdf2-methodcovered.pdf', width = width, height = height3)

ggplot(dfaa, aes(BranchGoals*MC, color = MC <= 0.5)) +
  stat_ecdf() +
  scale_x_continuous(breaks = 10*seq(0, 10, by = 1)) +
  coord_cartesian(xlim = c(0, 100)) +
  scale_color_brewer(
    labels = c(expression({0.6 < MC} <= 0.7), expression({0.4 < MC} <= 0.5)),
    palette = 'Set1',
    name = NULL
  ) +
  labs(x = 'Anzahl der Zweige', y = 'ECDF [-]') +
  theme_minimal() + 
  theme(legend.position=c(0.85,0.2), legend.background = element_rect(fill = "white"))

ggsave('evoVsCoverage-ecdf2-branchgoals.pdf', width = width, height = height3)

