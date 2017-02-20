
source('util.R')

df <- readDataset()

aggM <- df %>% group_by(Class) %>% summarise(meanBC = mean(BranchCoverage, na.rm=T),
                                             meanLC = mean(LineCoverage, na.rm=T),
                                             meanMC = mean(MethodCoverage, na.rm=T))

# ECDFs

ggplot() +
  stat_ecdf(data=aggM, aes(meanBC)) +
  ylab('ECDF [-]') +
  xlab('BC') +
  theme_minimal()
ggsave('coverageDistributions-ecdf-bc.pdf', width=width3, height=height3)

ggplot(aggM) +
  stat_ecdf(aes(meanMC)) +
  ylab('ECDF [-]') +
  xlab('MC') +
  theme_minimal()
ggsave('coverageDistributions-ecdf-mc.pdf', width=width3, height=height3)

ggplot(aggM) +
  stat_ecdf(aes(meanLC)) +
  ylab('ECDF [-]') +
  xlab('LC') +
  theme_minimal()
ggsave('coverageDistributions-ecdf-lc.pdf', width=width3, height=height3)


# Numbers


agg <- df %>% summarise(fullBC = sum(BranchCoverage == 1.0, na.rm=T) / nrow(df),
                        fullMC = sum(MethodCoverage == 1.0, na.rm=T) / nrow(df),
                        fullLC = sum(LineCoverage == 1.0, na.rm=T) / nrow(df))
print(agg)

# Coverages for the best by class

aggM <- df %>% group_by(Class) %>% summarise(maxBC = max(BranchCoverage, na.rm=T),
                                             maxLC = max(LineCoverage, na.rm=T),
                                             maxMC = max(MethodCoverage, na.rm=T))

agg <- aggM %>% summarise(fullBC = sum(maxBC == 1.0, na.rm=T) / nrow(aggM),
                          fullMC = sum(maxMC == 1.0, na.rm=T) / nrow(aggM),
                          fullLC = sum(maxLC == 1.0, na.rm=T) / nrow(aggM))
print(agg)

# No coverage at all

agg <- df %>% 
  group_by(Class)  %>%
  summarise(
    zero = max(BranchCovered, na.rm=T) == 0,
    zero = (is.na(zero) || zero),
    numGoals = max(BranchGoals, na.rm=T))
numberMissing <- nrow(filter(agg, zero == T))
missingPercent <- 100 * numberMissing / nrow(agg)
print(paste('Missing (BC) percent:', missingPercent))

agg <- df %>% 
  group_by(Class)  %>%
  summarise(
    zero = max(LineCovered, na.rm=T) == 0,
    zero = (is.na(zero) || zero),
    numGoals = max(LineGoals, na.rm=T))
numberMissing <- nrow(filter(agg, zero == T))
missingPercent <- 100 * numberMissing / nrow(agg)
print(paste('Missing (LC) percent:', missingPercent))


agg <- df %>% 
  group_by(Class)  %>%
  summarise(
    zero = max(MethodCovered, na.rm=T) == 0,
    zero = (is.na(zero) || zero),
    numGoals = max(MethodGoals, na.rm=T))
numberMissing <- nrow(filter(agg, zero == T))
missingPercent <- 100 * numberMissing / nrow(agg)
print(paste('Missing (MC) percent:', missingPercent))


