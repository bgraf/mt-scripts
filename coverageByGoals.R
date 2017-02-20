source('util.R')

df <- readDataset()

# Plot the generated coverage by the number of goals

agg <- df %>% group_by(Class, BranchGoals, LineGoals, MethodGoals) %>%
  summarise(
    bc = mean(BranchCoverage, na.rm=T),
    mc = mean(MethodCoverage, na.rm=T),
    lc = mean(LineCoverage, na.rm=T)
  )

agg <- agg[complete.cases(agg),]

print('BC vs. BranchGoals')
print(cor.test(agg$bc, agg$BranchGoals))
print(cor.test(agg$bc, agg$BranchGoals, method="spearman"))


agg$breaks <- cut(agg$bc, breaks = 10)
ggplot(agg, aes(breaks, BranchGoals)) + 
  geom_boxplot(fill=stdGrey) + 
  stat_summary(fun.y="mean", geom="point", shape=5) +
  ylim(0,100) + 
  scale_x_discrete(labels=c(seq(0.1,1.0,by=0.1))) +
  xlab("BranchCoverage") +
  ylab("Anzahl der Zweige") +
  theme_minimal()
ggsave('coverageByGoals-box-bc.pdf', width=widthFull, height=height3)

print('MC vs. BranchGoals')
print(cor.test(agg$mc, agg$MethodGoals))
print(cor.test(agg$mc, agg$MethodGoals, method="spearman"))

agg$breaks <- cut(agg$mc, breaks = 10)
ggplot(agg, aes(breaks, MethodGoals)) + 
  geom_boxplot(fill=stdGrey) + 
  stat_summary(fun.y="mean", geom="point", shape=5) +
  ylim(0,100) + 
  scale_x_discrete(labels=c(seq(0.1,1.0,by=0.1))) +
  xlab("MethodCoverage") +
  ylab("Anzahl der Methoden") +
  theme_minimal()
ggsave('coverageByGoals-box-mc.pdf', width=widthFull, height=height3)

print('LC vs. BranchGoals')
print(cor.test(agg$lc, agg$LineGoals))
print(cor.test(agg$lc, agg$LineGoals, method="spearman"))

agg$breaks <- cut(agg$lc, breaks = 10)
ggplot(agg, aes(breaks, LineGoals)) + 
  geom_boxplot(fill=stdGrey) + 
  stat_summary(fun.y="mean", geom="point", shape=5) +
  ylim(0,100) + 
  scale_x_discrete(labels=c(seq(0.1,1.0,by=0.1))) +
  xlab("LineCoverage") +
  ylab("Anzahl der Zeilen") +
  theme_minimal()
ggsave('coverageByGoals-box-lc.pdf', width=widthFull, height=height3)
