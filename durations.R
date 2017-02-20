source('util.R')

df = readDataset()

ggplot(df, aes(EsSeconds/60)) + 
  geom_vline(xintercept = 2, linetype='dashed', color='grey50') +
  stat_ecdf() + 
  scale_x_continuous(breaks = c(0, 0.1, 0.5, 1, 2, 5, 10, 30), trans='log10', limits = c(0.01, 40)) +
  xlab('Dauer [min]') + 
  ylab('ECDF [-]') +
  theme_minimal()
ggsave('durations-ecdf.pdf',width=width,height=height3)

print(summary(df$EsSeconds))

missing <- sum(is.na(df$EsSeconds))

print(paste('Missing: ', missing, ' => ', missing / nrow(df), '%'))

## Aggregate

dfa <- aggByClass(df)

ggplot(dfa, aes(EsSeconds/60)) + 
  geom_vline(xintercept = 2, linetype='dashed', color='grey50') +
  stat_ecdf() + 
  scale_x_continuous(breaks = c(0,0.1, 0.5, 1, 2, 5, 10, 30), trans="log10") +
  xlab('Dauer [min]') + 
  ylab('ECDF [-]') +
  theme_minimal()
ggsave('durations-ecdf-agg.pdf',width=width,height=height3)

#------------------------------------------------------------------------------
# Aggregate by class and show median
#------------------------------------------------------------------------------

medDurs = df %>% group_by(Class) %>%
  summarise(
    medianDur = median(EsSeconds, na.rm = T)
  )

ggplot(medDurs, aes(medianDur/60)) +
  geom_vline(xintercept = 2, linetype='dashed', color='grey50') +
  stat_ecdf() + 
  scale_x_continuous(breaks = c(0,0.1, 0.5, 1, 2, 5, 10), trans="log10", limits = c(0.1, 10)) +
  xlab('Dauer [min]') + 
  ylab('ECDF [-]') +
  theme_minimal()
ggsave('durations-ecdf-median.pdf',width=width,height=height3)

print(paste('Max median [min]:', max(medDurs$medianDur, na.rm = T) / 60))

print(paste('Number of classes with over 2min median:', sum(medDurs$medianDur > 240, na.rm = T)))
