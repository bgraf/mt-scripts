library(dplyr)
library(ggplot2)

source('util.R')

df <- read.csv('dataset.csv', header=T)
sdf <- subset(df, select=c(Class, BranchCovered, BranchGoals, FF))
sdf <- sdf[complete.cases(sdf),]

sdf$MissingBranches <- sdf$BranchGoals - sdf$BranchCovered

print(paste("Pearson (all)  =", cor(sdf$MissingBranches, sdf$FF)))
print(paste("Spearman (all) =", cor(sdf$MissingBranches, sdf$FF, method="spearman")))

# make unique points to ease the printer's job
sdfUnique <- unique(sdf[c("MissingBranches", "FF")])

ggplot(sdfUnique, aes(MissingBranches, FF)) +
  geom_point(color='black', size=0.1) +
  scale_shape_discrete(solid=T) +
  geom_smooth(method = "lm", color='grey70') +
  xlab('Anzahl nicht überdeckter Zweige') +
  ylab('Beste Fitness') +
  theme_minimal()
ggsave('missingBranchesVsFF-scatter.pdf', width = width, height = height)

# calculate correlations over replications for each class

## agg <- sdf %>% group_by(Class) %>% summarise(
##                                      r = cor(MissingBranches, FF),
##                                      rho = cor(MissingBranches, FF, method = "spearman"))
##
## print(paste("Mean Pearson (reps)  =", mean(agg$r, na.rm = T)))
## print(paste("Mean Spearman (reps) =", mean(agg$rho, na.rm = T)))

