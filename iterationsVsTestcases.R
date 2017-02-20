library(Rmisc)
source('util.R')

df <- readDataset()
dfa <- aggByClass(df)

# For all iterations...

ggplot(dfa, aes(NumIter, TestCnt)) + 
  geom_point(alpha=0.2, color='grey50') + 
  geom_smooth(method="loess", color='black', se = F) + 
  xlab('Anzahl der Iterationen') +
  ylab('Anzahl der Testfälle') +
  theme_minimal()
ggsave('iterationsVsTestcases-scatter.pdf', width=width, height=height3)


# For smaller iterations..

dfa2 <- subset(dfa, NumIter < 200)
dfa2$NumIter <- dfa2$NumIter / 10
dfa2$breaks <- cut(dfa2$NumIter, breaks = seq(0, 20, length.out = 11), right = F)

ggplot(dfa2, aes(factor(breaks), TestCnt)) + 
  geom_boxplot() + 
  stat_summary(fun.y="mean", geom="point", shape=5) +
  xlab('Anzahl der Iterationen') +
  ylab('Anzahl der Testfälle') +
  theme_minimal()
ggsave('iterationsVsTestcases-box100.pdf', width=width, height=height3)

# Only mean for smaller iterations..

s <- summarySE(dfa2, measurevar = 'TestCnt', groupvars = c('breaks'), na.rm=T)

ggplot(s, aes(breaks, TestCnt)) +
  geom_point() +
  geom_errorbar(aes(ymin=TestCnt-se, ymax=TestCnt+se), colour="black", width=0.1*nrow(s)/width) +
  xlab(expression('Anzahl der Iterationen' ~ 10^-1)) +
  ylab('Anzahl der Testfälle') +
  theme_minimal()
ggsave('iterationsVsTestcases-small-means.pdf', width=width, height=height3)

