#==============================================================================
# Consecutive Improvement and FBP
#==============================================================================

library(Rmisc)
source('util.R')

df = read.csv('dataset_cip2.csv', header=T)
df$NFF = with(df, FF / (BranchGoals + LineGoals + MethodGoals))

dfa = aggByClass(df)

dfa$NEs = with(dfa, mIFNEs + mICMPNEs + mACMPNEs + mIFNONNULLs)
dfa$EQs = with(dfa, mICMPEQs + mACMPEQs + mIFNULLs + mIFEQs)
dfa$EQNEs = with(dfa, NEs + EQs)

dfa$BIs = dfa$mBranchInstrCnt

dfa$FBP = with(dfa, ifelse(BIs > 0, EQNEs / BIs, 0))

dfa$FBPcut = cut(dfa$FBP, breaks = seq(0, 1, by = 0.1), labels = seq(10, 100, by = 10))
dfa$FBPcut2 = cut(dfa$FBP, breaks = seq(0, 1, by = 0.2), labels = seq(20, 100, by = 20))

#------------------------------------------------------------------------------
# Plots
#------------------------------------------------------------------------------

s = summarySE(dfa, measurevar = 'CIP', groupvars = 'FBPcut', na.rm = T)
s = s[!is.na(s$FBPcut), ]

ggplot(s, aes(factor(FBPcut), CIP)) +
  geom_point() +
  geom_errorbar(aes(ymin=CIP-se, ymax=CIP+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('CIP') +
  xlab('FBP [%]') +
  coord_cartesian(ylim = c(0, 0.7)) +
  theme_minimal()

ggsave('consecutiveImprovement-cip-by-fbp.pdf', width = width, height = height3)


#------------------------------------------------------------------------------
# Plots for fixed numbers of BIs
#------------------------------------------------------------------------------

plotCIPbyFBP = function(df) {
  s = summarySE(df, measurevar = 'CIP', groupvars = 'FBPcut2', na.rm = T)
  s = s[!is.na(s$FBPcut2), ]
  ggplot(s, aes(factor(FBPcut2), CIP)) +
    geom_point() +
    geom_errorbar(aes(ymin=CIP-se, ymax=CIP+se), colour="black", width=0.1*nrow(s)/width) +
    ylab('CIP') +
    xlab('FBP [%]') +
    coord_cartesian(ylim = c(0, 0.7)) +
    theme_minimal() 
}

makeS = function(df, lower) {
  df = subset(df, lower <= BIs & BIs < (lower+10))
  s = summarySE(df, measurevar = 'CIP', groupvars = 'FBPcut2', na.rm = T)
  s = s[!is.na(s$FBPcut2), ]
  s$lower = lower
  s
}

s = summarySE(subset(dfa, BIs >= 30), measurevar = 'CIP', groupvars = 'FBPcut2', na.rm = T)
s = s[!is.na(s$FBPcut2), ]
s$lower = "30+"


s.all = rbind(
  makeS(dfa, 0),
  makeS(dfa, 10),
  makeS(dfa, 20),
  s
)


ggplot(s.all, aes(factor(FBPcut2), CIP, color=factor(lower), group=factor(lower))) +
  geom_point() + geom_line() +
  ylab('CIP') +
  xlab('FBP [%]') +
  coord_cartesian(ylim = c(0, 0.7)) +
  scale_color_brewer(
    palette="Set1",
    labels=c(
      expression({0 <= BIs} < 10),
      expression({10 <= BIs} < 20),
      expression({20 <= BIs} < 30),
      expression(BIs > 30)),
    name=NULL
  ) +
  theme_minimal() 

ggsave('consecutiveImprovement-grouped-cip-by-fbp.pdf', width = width, height = height3)



#------------------------------------------------------------------------------
# Effects...
#------------------------------------------------------------------------------


dfa$CIPcut = cut(dfa$CIP, breaks = seq(0, 1, by = 0.1), labels = seq(1, 10, by = 1)/10)

s = summarySE(dfa, measurevar = 'PIC', groupvars = 'CIPcut')
s = s[!is.na(s$CIPcut), ]

ggplot(s, aes(factor(CIPcut), PIC)) +
  geom_point() +
  geom_errorbar(aes(ymin=PIC-se, ymax=PIC+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('PIC') +
  xlab('CIP') +
  ylim(0, 0.7) +
  theme_minimal()

ggsave('consecutiveImprovement-pic-by-cip.pdf', width = width, height = height3)

# ----

s = summarySE(dfa[dfa$BranchCoverage < 1.0, ], measurevar = 'CIP', groupvars = 'FBPcut', na.rm = T)
s = s[!is.na(s$FBPcut), ]

ggplot(s, aes(factor(FBPcut), CIP)) +
  geom_point() +
  geom_errorbar(aes(ymin=CIP-se, ymax=CIP+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('CIP') +
  xlab('FBP [%]') +
  coord_cartesian(ylim = c(0, 0.7)) +
    theme_minimal()

#------------------------------------------------------------------------------
# CIP vs. NLE: 
# CIP vs PIC and NLE vs PIC looked similar, so what's the connection?
#
# => unclear
#------------------------------------------------------------------------------

s = summarySE(dfa[dfa$BranchCoverage < 1.0, ], measurevar = 'CIP', groupvars = 'NLE', na.rm = T)
s = s[!is.na(s$CIP), ]

ggplot(s, aes(factor(NLE), CIP)) +
  geom_point() +
  geom_errorbar(aes(ymin=CIP-se, ymax=CIP+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('CIP') +
  xlab('NLE') +
  coord_cartesian(ylim = c(0, 0.7)) +
  theme_minimal()

s = summarySE(dfa[dfa$BranchCoverage < 1.0, ], measurevar = 'NLE', groupvars = 'CIPcut', na.rm = T)
s = s[!is.na(s$CIPcut), ]

ggplot(s, aes(factor(CIPcut), NLE)) +
  geom_point() +
  geom_errorbar(aes(ymin=NLE-se, ymax=NLE+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('NLE') +
  xlab('CIP') +
  theme_minimal()

s = summarySE(dfa, measurevar = 'CIP', groupvars = 'BIs', na.rm = T)
s = subset(s, N >= minObs)

ggplot(s, aes(BIs, CIP)) +
  geom_point() +
  geom_errorbar(aes(ymin=CIP-se, ymax=CIP+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('CIP') +
  xlab('BIs') +
  theme_minimal()
