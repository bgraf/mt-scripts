source('util.R')

set.seed(42)
x <- runif(1000)
y <- sort(x)+runif(1000)+sin(1:1000*0.01)*0.15*rnorm(1000, mean=1)

df <- data.frame(parent=sort(x)*max(y), offspring=y)


df$segment <- cut(df$parent, breaks = 10)

segmentBorders <- tail(seq(max(df$parent), length.out = 10, by=-(max(df$parent) - min(df$parent))/10), n = -1)

segments <- df %>% group_by(segment) %>%
  summarise(
    xm = mean(parent),
    my = mean(offspring)
  )

ggplot(df, aes(parent, offspring)) +
  geom_point(color='grey70') +
  geom_abline(slope=1, linetype='dotted') +
  xlab('Elternfitness') + ylab('Kindfitness') +
  theme_minimal()
ggsave('nscExample-fc.pdf', width=width, height=height3)


ggplot(df, aes(parent, offspring)) + 
  geom_point(color='grey70') + 
  geom_vline(xintercept = segmentBorders, linetype='dotted') +
  geom_line(data=segments, aes(xm, my), size=1) +
  geom_point(data=segments, aes(xm, my), size=1.1) +
  xlab('Elternfitness') + ylab('Kindfitness') +
  theme_minimal()
ggsave('nscExample-slope.pdf', width=width, height=height3)


