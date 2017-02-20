#==============================================================================
# Replications analysis:
#
# 1. Number of unique branch coverages per class
#==============================================================================

source('util.R')

df = readDataset()

#------------------------------------------------------------------------------
# unique branch coverages per class
#------------------------------------------------------------------------------

agg = df %>% group_by(Class) %>%
  summarise(
    uniques = length(unique(BranchCoverage))
  )

ggplot(agg, aes(uniques)) + 
  stat_ecdf() +
  labs(x = 'Anzahl verschiedener Zweigüberdeckungen', y = 'ECDF [-]') +
  theme_minimal()
ggsave('replicationsAnalysis-ecdf-unique-bc.pdf', width = width, height = height3)

## number of classes with more than 30 unique branch coverages

t = melt(table(agg$uniques))
numClasses = sum(t[t$Var.1 > 30, ]$value)

print(paste(
  'Number of classes with more than 30 unique coverages:', 
  numClasses, 100*numClasses / nrow(agg)
  ))

agg = df %>% group_by(Class) %>%
  summarise(
    uBC = length(unique(BranchCoverage)),
    uMC = length(unique(MethodCoverage)),
    uLC = length(unique(LineCoverage)),
    uTC = length(unique(TestCnt)),
    uTL = length(unique(TestLen))
  )

t = melt(as.data.frame(agg), measure.vars = c("uBC", "uMC", "uLC", "uTC", "uTL"))

ggplot(t, aes(value, color=variable)) + 
  stat_ecdf() + 
  scale_color_brewer(labels = c('BC', 'MC', 'LC', 'TC', 'TL'), palette = 'Set1', name = NULL) +
  labs(x = 'Anzahl unterschiedlicher Werte', y = 'ECDF [-]') +
  theme_minimal()

ggsave('replicationsAnalysis-ecdf-unique-all.pdf', width = widthFull, height = height3)
