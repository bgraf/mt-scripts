source('util.R')

df = readDataset()
dfa = aggByClass(df)

#------------------------------------------------------------------------------
# How many with small iteration counts
#------------------------------------------------------------------------------

smallNI = subset(df, NumIter == 1)

mean(smallNI$BranchCoverage, na.rm = T)

print(paste(
  'Percentage of NI = 1:',
  100*nrow(smallNI) / nrow(df)))

## with two iterations

sNI2 = subset(df, NumIter == 2)
ni2fullCR = nrow(subset(sNI2, CR == 1)) / nrow(sNI2)
print(paste('Percentage of NI=2 with CR = 1:', ni2fullCR))

## without single-iterations

wo1 = subset(df, NumIter > 1)
cor(wo1$CR, wo1$FF, use="complete", method="spearman")
cor(wo1$CR, wo1$BranchCoverage, use="complete", method="spearman")

#dfa = dfa[!is.na(dfa$NSC), ]

dfa$CR2 = ifelse(dfa$NumIter == 1, 0, dfa$CR)
dfa$CR3 = ifelse(dfa$NumIter == 1, NA, dfa$CR)


cor(dfa$CR, dfa$FF, use="complete", method="pearson")
cor(dfa$CR2, dfa$FF, use="complete", method="pearson")
cor(dfa$CR3, dfa$FF, use="complete", method="pearson")

cor(dfa$CR, dfa$BranchCoverage, use="complete", method="pearson")
cor(dfa$CR2, dfa$BranchCoverage, use="complete", method="pearson")
cor(dfa$CR3, dfa$BranchCoverage, use="complete", method="pearson")

plot(dfa$CR, dfa$FF)
dfx = dfa[!is.na(dfa$NSC), ] #subset(dfa, FF < 3000)

cor(dfx$CR, dfx$BranchCoverage, use="complete", method="pearson")
cor(dfx$CR2, dfx$BranchCoverage, use="complete", method="pearson")
cor(dfx$CR3, dfx$BranchCoverage, use="complete", method="pearson")

