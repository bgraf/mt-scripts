source('util.R')

df = readDataset()

df$NEs = with(df, mIFNEs + mICMPNEs + mACMPNEs + mIFNONNULLs)
df$EQs = with(df, mICMPEQs + mACMPEQs + mIFNULLs + mIFEQs)
df$EQNEs = with(df, NEs + EQs)
df$CMPs = with(df, mBranchInstrCnt - EQNEs)

fullCoverage = with(df, BranchCovered == BranchGoals &
                        MethodCovered == MethodGoals &
                        LineCovered == LineGoals)


dff = df[fullCoverage,]
dfn = df[!fullCoverage,]

#------------------------------------------------------------------------------
# Numbers and aggregation
#------------------------------------------------------------------------------

print(paste('Number of rows:', nrow(df)))
print(paste('Number of rows with full coverage:', nrow(dff)))
print(paste('Number of rows with non-full coverage:', nrow(dfn)))
print(paste('Full coverage in', 100 * nrow(dff) / nrow(df), '% of the runs'))


dffa = aggByClass(dff)
dfna = aggByClass(dfn)

print(paste('Classes with full-coverage:', nrow(dffa)))
print(paste('Classes with full-coverage (%):', nrow(dffa) / (nrow(dffa) + nrow(dfna))))

#------------------------------------------------------------------------------
# Full coverage correlations
#------------------------------------------------------------------------------

dfs = subset(dffa, select=c(colEvo, colRes, colMetric, 'CMPs', 'EQNEs'))
dfs = subset(
  dfs,
  select = -c(
    NSC,
    NSCFCS,
    BranchCoverage,
    MethodCoverage,
    LineCoverage,
    cWMC,
    cDIT,
    cNOC,
    cCBO,
    cRFC,
    cLCOM,
    cCa,
    cNPM
  )
)

mPears = cor(dfs, use='complete', method='pearson')
mSpear = cor(dfs, use='complete', method='spearman')

rPears = mPears["TestCnt",]
rSpear = mSpear["TestCnt",]

## --- Bar plots of correlations ---

selectedValues = c(
  "CR", "PIC", "FF", "NumIter",
  "WMC", "DIT", "RFC", "LCOM5", "CBO",
  "BranchGoals", "MethodGoals", "LineGoals",
  "mBranchInstrCnt", "CMPs", "EQNEs", "NA.", "NLE", "NOS"
)

namesOfValues = c(
  "CR", "PIC", "FF", "NI",
  "WMC", "DIT", "RFC", "LCOM5", "CBO",
  "BG", "MG", "LG",
  "BIs", "CMPs", "EQNEs", "NA.", "NLE", "NOS"
)

categoryOfValues = c(
  "Evolvierbarkeit", "Evolvierbarkeit", "Evolvierbarkeit", "Evolvierbarkeit",
  "Testbarkeit", "Testbarkeit", "Testbarkeit", "Testbarkeit", "Testbarkeit",
  "Ziele", "Ziele", "Ziele",
  "Andere", "Andere", "Andere", "Andere", "Andere", "Andere"
)

sPears = rPears[selectedValues]
sSpear = rSpear[selectedValues]

names(sPears) = namesOfValues
names(sSpear) = namesOfValues

dPears = as.data.frame(as.table(sPears))
dSpear = as.data.frame(as.table(sSpear))

dPears$Category = dSpear$Category = categoryOfValues

barPlot = function(df, level=0.3) {
  df$Var1 <- factor(df$Var1, levels=rev(df$Var1))
  ggplot(df, aes(Var1, Freq)) + 
    geom_bar(stat='identity', fill=stdGrey, color='black') + 
    geom_label(aes(y=-0.8, label=round(Freq, digits = 2)), hjust=0, fill='white') +
    facet_grid(Category ~ . , scales = "free_y", space = "free") +
    coord_flip() +
    ylim(-1,1) +
    theme_minimal() +
    labs(x = NULL, y=NULL) 
}

ord = order(dPears$Freq)

barPlot(dPears[ord, ])
ggsave('testcasesCorrelation-pears.pdf', width=width, height=height)

barPlot(dSpear[ord, ])
ggsave('testcasesCorrelation-spear.pdf', width=width, height=height)

#------------------------------------------------------------------------------
# Further analysis: testcases numbers vs. goals
#------------------------------------------------------------------------------

ggplot(dffa, aes(BranchGoals, TestCnt)) + geom_point() + geom_abline(slope = 1)

sum(dffa$BranchGoals > dffa$TestCnt) / nrow(dffa)

mean(dffa$BranchGoals / dffa$TestCnt)
mean(dffa$MethodGoals / dffa$TestCnt)
mean(dffa$LineGoals / dffa$TestCnt)

dffa$Goals = with(dffa, BranchGoals + MethodGoals + LineGoals)

mean(dffa$Goals / dffa$TestCnt)

ggplot(dffa, aes(Goals / TestCnt)) + 
  stat_ecdf() + 
  scale_x_continuous(
    trans="log10", 
    breaks=c(1, 10, 100), 
    limits = c(1, 200)) +
  labs(y = 'ECDF [-]', x = 'Ziele / TC') +
  theme_minimal()

cor(dfa$RFC, dfa$MethodGoals, use="complete", method="spearman")
cor(dffa$RFC, dffa$MethodGoals, use="complete", method="spearman")
cor(dffa$NOS, dffa$LineGoals, use="complete", method="spearman")

#------------------------------------------------------------------------------
# Numbers of EQNEs and CMPs
#------------------------------------------------------------------------------

sum(dffa$EQNEs) / sum(dffa$mBranchInstrCnt)
sum(dffa$CMPs) / sum(dffa$mBranchInstrCnt)


#------------------------------------------------------------------------------
# Equal numbers of CMPs and EQNEs
#------------------------------------------------------------------------------

equalBranchTypes = subset(dffa, CMPs == EQNEs)

cor(equalBranchTypes$TestCnt, equalBranchTypes$CMPs)
cor(equalBranchTypes$TestCnt, equalBranchTypes$EQNEs)

cor(dffa$TestCnt, dffa$CMPs)
cor(dffa$TestCnt, dffa$EQNEs)

plot(dffa$TestCnt, dffa$CMPs)

plot(dffa$TestCnt, dffa$EQNEs)

ggplot(dffa, aes(TestCnt, EQNEs)) + geom_point() + geom_smooth()
ggplot(dffa, aes(TestCnt, CMPs)) + geom_point() + geom_smooth()

ggplot(subset(dffa, CMPs==0), aes(EQNEs)) + stat_ecdf()
ggplot(subset(dffa, EQNEs==0), aes(CMPs)) + stat_ecdf()

subset(dffa, EQNEs==0)
subset(dffa, CMPs <= EQNEs)

