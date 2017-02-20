source('util.R')

sigEpsilon <- function(epsilon) {
  function(val) {
    if (val > epsilon) {
      1
    } else if (val < -epsilon) {
      -1
    } else {
      0
    }
  }
}

set.seed(6)

n <- 10
x <- runif(n, min=3, max=6)
d <- diff(x)



ggplot(data.frame(i = 1:n, x=x), aes(i, x)) + 
  geom_line() + 
  geom_point() +
  ylab(expression(f[i])) + 
  scale_x_continuous(breaks=seq(1, n, by=1)) +
  theme_minimal()
ggsave('picExample-f.pdf', width=width3, height=height3)


ggplot(data.frame(i=1:(n-1), x=sapply(d, sigEpsilon(0.5))), aes(i, x)) +
  geom_segment(aes(i, x, xend=i, yend=x, y=0)) +
  geom_point() +
  ylab(expression(b[i])) +
  scale_x_continuous(breaks=seq(1, n-1, by=1)) +
  scale_y_continuous(breaks=c(-1, 0, 1)) +
  theme_minimal()
ggsave('picExample-eps05.pdf', width=width3, height=height3)

ggplot(data.frame(i=1:(n-1), x=sapply(d, sigEpsilon(0.05))), aes(i, x)) +
  geom_segment(aes(i, x, xend=i, yend=x, y=0)) +
  geom_point() +
  ylab(expression(b[i])) +
  scale_x_continuous(breaks=seq(1, n-1, by=1)) +
  scale_y_continuous(breaks=c(-1, 0, 1)) +
  theme_minimal()
ggsave('picExample-eps005.pdf', width=width3, height=height3)
