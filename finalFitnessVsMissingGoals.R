source('util.R')

df <- readDataset()

df$missingBranch <- with(df, BranchGoals - BranchCovered)
df$missingMethod <- with(df, MethodGoals - MethodCovered)
df$missingLine <- with(df, LineGoals - LineCovered)

df$missingTotal <- with(df, missingBranch + missingLine + missingMethod)

pearson <- cor(df$missingTotal, df$FF, use="complete")
spearman <- cor(df$missingTotal, df$FF, use="complete", method="spearman")

print(paste('r   =', pearson))
print(paste('rho =', spearman))

sub <- subset(df, select=c('missingTotal', 'FF'))
sub <- unique(sub)

dfp = data.frame(missingTotal = df$missingTotal,
                 FF = df$FF)

dfp = unique(dfp)

ggplot(df, aes(missingTotal, FF)) + 
  geom_point(data=sub, alpha=1, color='grey30', size=0.3) + 
  geom_smooth(data=df, method = "lm", color='black') + 
  ylab('beste Fitness') +
  xlab('Anzahl nicht erreichter Ziele') +
  theme_minimal()
ggsave('finalFitnessVsMissingGoals-scatter.pdf', width=width, height=height3)
ggsave('finalFitnessVsMissingGoals-scatter.png', width=width, height=height3, dpi = 600)

