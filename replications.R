
source('util.R')

df <- readDataset()

# Target value standard deviations over the replications

agg <- df %>% group_by(Class) %>%
  summarise(SDBranchCoverage = sd(BranchCoverage, na.rm=T),
            SDLineCoverage = sd(LineCoverage, na.rm=T),
            SDMethodCoverage = sd(MethodCoverage, na.rm=T),
            SDTestCnt = sd(TestCnt, na.rm=T),
            SDTestLen = sd(TestLen, na.rm=T))

print(summary(agg[,-1]))

print(sapply(agg[,-1], function (x) mean(x, na.rm=T)))
print(sapply(agg[,-1], function (x) median(x, na.rm=T)))

counts <- agg %>% summarise(countBC = sum(SDBranchCoverage == 0, na.rm=T),
                  countLC = sum(SDLineCoverage == 0, na.rm=T),
                  countMC = sum(SDMethodCoverage == 0, na.rm=T),
                  countTC = sum(SDTestCnt == 0, na.rm=T),
                  countTL = sum(SDTestLen == 0, na.rm=T))
print(counts)

# Average standard deviation of test-case-length for classes with
# a branch coverage of 1.0 for all 

print('Average SD of TC and TL for classes with min(BC) = 1.0')
res <- df %>% group_by(Class) %>%
  summarise(bc = min(BranchCoverage, na.rm=T),
            sdTL = sd(TestLen, na.rm=T),
            sdTC = sd(TestCnt, na.rm=T)) %>%
  filter(bc == 1.0) %>%
  summarize(avgSdTC = median(sdTC, na.rm=T),
            avgSdTL = median(sdTL, na.rm=T))
print(res)


# Histograms


ggplot(agg, aes(SDBranchCoverage)) +
  geom_histogram(fill=stdGrey, color='black') +
  xlab(expression(sigma(BranchCoverage))) + ylab("Anzahl") +
  theme_minimal()
ggsave('replications-hist-bc.pdf', width=width3, height=height3)

ggplot(agg, aes(SDLineCoverage)) +
  geom_histogram(fill=stdGrey, color='black') +
  xlab(expression(sigma(LineCoverage))) + ylab("Anzahl") +
  theme_minimal()
ggsave('replications-hist-lc.pdf', width=width3, height=height3)

ggplot(agg, aes(SDMethodCoverage)) +
  geom_histogram(fill=stdGrey, color='black') +
  xlab(expression(sigma(MethodCoverage))) + ylab("Anzahl") +
  theme_minimal()
ggsave('replications-hist-mc.pdf', width=width3, height=height3)
