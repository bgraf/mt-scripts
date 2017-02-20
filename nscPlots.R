source('util.R')


set.seed(42)

n = 500
x <- sort(runif(n))
y <- seq(0.0, 1.0, length.out=n) + rnorm(n, sd=0.1, mean=0.2)

plot(x,y)
abline(0,1)
