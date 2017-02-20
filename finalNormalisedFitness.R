source('util.R')

df <- readDataset()

df$normalisedFF <- df$FF / (df$BranchGoals + df$LineGoals + df$MethodGoals)

pearson <- cor(df$normalisedFF, df$BranchCoverage, use="complete")
spearman <- cor(df$normalisedFF, df$BranchCoverage, use="complete", method="spearman")

print(paste('Pearsons r    =', pearson))
print(paste('Spearmans rho =', spearman))

sel <- select(df, c(BranchCoverage, normalisedFF))
uniques <- unique(sel)

ggplot(df, aes(BranchCoverage, normalisedFF)) + 
  geom_point(data=uniques, alpha=0.1, color='grey30', size=0.3) + 
  geom_smooth(data=df, method = "lm", color='black') + 
  ylab('normierte beste Fitness') +
  theme_minimal()
ggsave('finalNormalisedFitness-scatter.pdf', width=width, height=height3)
ggsave('finalNormalisedFitness-scatter.png', width=width, height=height3, dpi = 600)

print(summary(lm(normalisedFF ~ BranchCoverage, data=df)))