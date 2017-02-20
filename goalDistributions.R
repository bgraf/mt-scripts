source('util.R')

df <- readDataset()

agg <- df %>%
  group_by(Class) %>%
  summarise(bcGoals = max(BranchGoals, na.rm=T),
            mcGoals = max(MethodGoals, na.rm=T),
            lcGoals = max(LineGoals, na.rm=T))


print(summary(select(agg, -Class)))

s <- sapply(select(agg, -Class), function (x) {
  list(
    skewnewss = skewness(x, na.rm=T),
    kurtosis = kurtosis(x, na.rm=T)
  )
})
print(s)

# ECDFs

ggplot(agg) +
  stat_ecdf(aes(bcGoals)) +
  scale_x_continuous(trans='log10', breaks=c(1, 10, 100, 1000)) +
  theme_minimal() +
  xlab('Anzahl der Zweige') + ylab('ECDF [-]')
ggsave('goalDistributions-ecdf-bc.pdf', width=width3, height=height3)

ggplot(agg) +
  stat_ecdf(aes(mcGoals)) +
  scale_x_continuous(trans='log10', breaks=c(1,5, 10, 50, 100, 500, 1000)) +
  theme_minimal() +
  xlab('Anzahl der Methoden') + ylab('ECDF [-]')
ggsave('goalDistributions-ecdf-mc.pdf', width=width3, height=height3)

ggplot(agg) +
  stat_ecdf(aes(lcGoals)) +
  scale_x_continuous(trans='log10', breaks=c(1,10, 100, 1000)) +
  theme_minimal() +
  xlab('Anzahl der Zeilen') + ylab('ECDF [-]')
ggsave('goalDistributions-ecdf-lc.pdf', width=width3, height=height3)

# Densities

ggplot(agg) +
  geom_density(aes(bcGoals), fill=stdGrey) +
  scale_x_continuous(trans='log10') +
  theme_minimal() +
  xlab('Anzahl der Zweige') + ylab('Density')
ggsave('goalDistributions-dens-bc.pdf', width=width3, height=height3)


ggplot(agg) +
  geom_density(aes(mcGoals), fill=stdGrey) +
  scale_x_continuous(trans='log10') +
  theme_minimal() +
  xlab('Anzahl der Methoden') + ylab('Density')
ggsave('goalDistributions-dens-mc.pdf', width=width3, height=height3)

ggplot(agg) +
  geom_density(aes(lcGoals), fill=stdGrey) +
  scale_x_continuous(trans='log10') +
  theme_minimal() +
  xlab('Anzahl der Zeilen') + ylab('Density')
ggsave('goalDistributions-dens-lc.pdf', width=width3, height=height3)
