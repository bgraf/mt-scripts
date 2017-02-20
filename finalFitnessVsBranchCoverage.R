library(dplyr)
library(ggplot2)
library(reshape)

source('util.R')

df <- read.csv('dataset.csv', header=T)
sdf <- subset(df, select=c(Class, BranchCoverage, FF))
sdf <- sdf[complete.cases(sdf),]


# Calculate correlations over all data points
rAll <- with(sdf, cor(BranchCoverage, FF, use="complete"))
rhoAll <- with(sdf, cor(BranchCoverage, FF, use="complete", method="spearman"))

print(paste("Pearson's r (all) ='", rAll))
print(paste("Spearman's rho (all) ='", rhoAll))


# Correlation of means for each class
agg <- sdf %>% group_by(Class) %>% summarise(
                                     BranchCoverage = mean(BranchCoverage, na.rm=T),
                                     FF = mean(FF, na.rm=T))

rMean <- with(agg, cor(BranchCoverage, FF, use="complete"))
rhoMean <- with(agg, cor(BranchCoverage, FF, use="complete", method="spearman"))

print(paste("Pearson's r (mean) ='", rMean))
print(paste("Spearman's rho (mean) ='", rhoMean))


# Correlation for the replications of each class
# - Mean of these correlations
# - Histogram or ECDF of these correlations
agg <- sdf %>% group_by(Class) %>% summarise(
                                     r = cor(BranchCoverage, FF, use="complete"),
                                     rho = cor(BranchCoverage, FF, use="complete", method="spearman"))

print(summary(agg[,-1]))

rRepMean <- mean(agg$r, na.rm=T)
rhoRepMean <- mean(agg$rho, na.rm=T)

print(paste("Pearson's r (rep-mean) ='", rRepMean))
print(paste("Spearman's rho (rep-mean) ='", rhoRepMean))

ggplot(agg, aes(r)) +
  geom_histogram(bins=40, fill=stdGrey, color='black') +
  ylim(0,260) +
  xlab(expression(Pearsons ~ r)) +
  ylab('Anzahl') +
  theme_minimal()
ggsave('finalFitnessVsBranchCoverage-hist-pearson.pdf', width=width, height=height/2)

ggplot(agg, aes(rho)) +
  geom_histogram(bins=40, fill=stdGrey, color='black') +
  ylim(0,260) +
  xlab(expression(Spearmans ~ rho)) +
  ylab('Anzahl') +
  theme_minimal()
ggsave('finalFitnessVsBranchCoverage-hist-spearman.pdf', width=width, height=height/2)


pagg <- melt(as.data.frame(agg), id=c('Class'))

ggplot(pagg, aes(x=variable, y=value)) +
  stat_boxplot(geom='errorbar') +
  geom_boxplot(fill=stdGrey, color='black') +
  ylab('Korrelationskoeffizient') + xlab('Korrelationsmethode') +
  scale_x_discrete(labels=c(expression(Pearsons ~ r), expression(Spearmans ~ rho))) +
  theme_minimal()
ggsave('finalFitnessVsBranchCoverage-box.pdf', width=width, height=height)


