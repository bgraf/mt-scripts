source('util.R')

df <- readDataset()

trials <- function(prob, n) {
  comp <- 1.0 - prob
  
  res <- 0
  for (i in 1:n) {
    res <- res + (comp^(i-1))*prob 
  }
  res
}

res <- c()
for (i in 1:30) {
  agg <- df %>%
    group_by(Class) %>%
    summarise(
      fullCnt = sum(BranchCoverage == 1.0, na.rm=T),
      cnt = sum(!is.na(BranchCoverage)),
      fullRatio = (fullCnt / cnt),
      nTrials = trials(fullRatio, i)
    ) %>% 
    filter(fullCnt > 0) %>% 
    summarise(m = mean(fullRatio),
              nt = mean(nTrials))
  
  res[i] <- agg$nt[[1]]
}

df.res <- data.frame(trials=1:length(res), meanProb=res)

ggplot(df.res, aes(trials, meanProb)) + 
  scale_x_continuous(breaks = 1:30, labels = ) +
  geom_hline(yintercept = 0.99, color='grey70') +
  geom_vline(xintercept = 13, color='grey70') +
  geom_point() + 
  ylab('mittlere Wahrscheinlichkeit') + xlab('Anzahl der Testfallerzeugungen') +
  theme_minimal()
ggsave('howManyRuns.pdf', width=widthFull, height=height3)

# res <- c()
# for (i in 1:150) {
#   agg <- df %>%
#     group_by(Class) %>%
#     summarise(
#       fullCnt = sum(BranchCoverage == 1.0, na.rm=T),
#       cnt = sum(!is.na(BranchCoverage)),
#       fullRatio = (fullCnt / cnt),
#       nTrials = trials(fullRatio, i)
#     ) %>%
#     summarise(m = mean(fullRatio),
#               nt = mean(nTrials, na.rm=T),
#               ntmed = median(nTrials, na.rm=T))
# 
#   res[i] <- agg$nt[[1]]
# }
# res
# df.res <- data.frame(trials=1:length(res), meanProb=res)
# 
# ggplot(df.res, aes(trials, meanProb)) +
#   geom_point() +
#   ylab('mittlere Wahrscheinlichkeit') + xlab('Anzahl der Testfallerzeugungen') +
#   theme_minimal()
# df.res
# ggsave('howManyRuns-all.pdf', width=widthFull, height=height3)


