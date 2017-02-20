source('util.R')

set.seed(42)

n <- 50

# good progression

x  <- 1:n
y1 <- sort(exp(runif(n, min = 0, max=3)), decreasing = T)

ggplot(data.frame(i=x, f=y1), aes(i,f)) + 
  geom_line(color='grey70') +
  geom_point() +
  xlab('Iteration') + ylab('beste Fitness') +
  theme_minimal() +
  scale_y_continuous(labels = NULL)
ggsave('evolvabilityExample-good.pdf', width=width, height=height3)


# plateau progression

s <- sort(exp(runif(5, min=0, max=3)))
y2 <- sort(sample(s, n, replace = T), decreasing = T)

ggplot(data.frame(i=x, f=y2), aes(i,f)) + 
  geom_line(color='grey70') +
  geom_point() +
  xlab('Iteration') + ylab('beste Fitness') +
  theme_minimal() +
  scale_y_continuous(labels = NULL)
ggsave('evolvabilityExample-bad.pdf', width=width, height=height3)
