library(Rmisc)
source('util.R')

df <- readDataset()
dfa <- aggByClass(df)

df$UBTestCnt <- ifelse(df$BranchCovered == df$BranchGoals,
       df$TestCnt, 
       df$TestCnt + df$BranchGoals - df$BranchCovered
       )

df$UBTestCntLC <- ifelse(df$LineCovered == df$LineGoals,
                         df$TestCnt,
                         df$TestCnt + df$LineGoals - df$LineCovered)

df$UBTestCntA <- with(
  df,
  TestCnt + 
    df$LineGoals - df$LineCovered +
    df$MethodGoals - df$MethodCovered +
    df$BranchGoals - df$BranchCovered)


dfs = aggByClass(df)

dfs = dfs[dfs$BranchCovered == dfs$BranchGoals,]

plot(ecdf(dfs$BranchGoals / dfs$TestCnt))

s <- summarySE(dfs, measurevar = 'UBTestCnt', groupvars = c('BranchGoals'))
s <- filter(s, N >= 30)

ggplot(s, aes(BranchGoals, UBTestCnt)) +
  geom_point() +
  geom_errorbar(aes(ymin=UBTestCnt-se, ymax=UBTestCnt+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('UBTestCnt') +
  xlab('BG') +
  theme_minimal()

s <- summarySE(dfs, measurevar = 'UBTestCntLC', groupvars = c('LineGoals'))
s <- filter(s, N >= 30)

ggplot(s, aes(LineGoals, UBTestCntLC)) +
  geom_point() +
  geom_errorbar(aes(ymin=UBTestCntLC-se, ymax=UBTestCntLC+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('UBTestCntLC') +
  xlab('BG') +
  theme_minimal()

## --- ALL ---
s <- summarySE(dfs, measurevar = 'UBTestCntA', groupvars = c('BranchGoals'))
s <- filter(s, N >= 10)

ggplot(s, aes(BranchGoals, UBTestCntA)) +
  geom_point() +
  geom_errorbar(aes(ymin=UBTestCntA-se, ymax=UBTestCntA+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('UBTestCntA') +
  xlab('BG') +
  theme_minimal()

s <- summarySE(dfs, measurevar = 'UBTestCntA', groupvars = c('LineGoals'))
s <- filter(s, N >= 10)

ggplot(s, aes(LineGoals, UBTestCntA)) +
  geom_point() +
  geom_errorbar(aes(ymin=UBTestCntA-se, ymax=UBTestCntA+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('UBTestCntLC') +
  xlab('BG') +
  theme_minimal()

## Missing goals by number of goals

dfa$BranchMissing = dfa$BranchGoals - dfa$BranchCovered
dfa$BranchMissingP = dfa$BranchMissing / dfa$BranchGoals

s <- summarySE(dfa, measurevar = 'BranchMissingP', groupvars = c('BranchGoals'))
s <- filter(s, N >= 30)

ggplot(s, aes(BranchGoals, BranchMissingP)) +
  geom_point() +
  geom_errorbar(aes(ymin=BranchMissingP-se, ymax=BranchMissingP+se), colour="black", width=0.1*nrow(s)/width) +
  ylab('BranchMissingP') +
  xlab('BG') +
  theme_minimal() + geom_smooth()

## ----------

plot(dfa$TestCnt, dfa$MethodCovered)
abline(0,1)
cor(dfa$BranchGoals, dfa$WMC, use="complete")
