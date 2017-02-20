# This script generates plots to demonstrate various graphical
# representations of data.

source('util.R')

# Box- and violinplots

set.seed(42)

df <- data.frame(y = c(rnorm(1000), runif(min=0.0, max=1.0, 1000)))

ggplot(df, aes(factor(""), y)) + 
  geom_boxplot() + 
  stat_summary(fun.y = "mean", geom="point", shape=5) +
  xlab('') +
  theme_minimal()
ggsave('graphicalRepresentations-1box.pdf', width=width3, height=height3)

ggplot(df, aes(factor(""), y)) + 
  geom_violin(fill=stdGrey) + 
  xlab('') +
  theme_minimal()
ggsave('graphicalRepresentations-1violin.pdf', width=width3, height=height3)


ggplot(df, aes(y)) + 
  geom_histogram(color='black', fill=stdGrey, bins = 30) + 
  ylab('Anzahl') +
  theme_minimal()
ggsave('graphicalRepresentations-1hist.pdf', width=width3, height=height3)

# Bimodal

df <- data.frame(y = c(rnorm(1000, mean = -2.5), rnorm(1000, mean = 2.5)))

ggplot(df, aes(factor(""), y)) + 
  geom_boxplot() + 
  stat_summary(fun.y = "mean", geom="point", shape=5) +
  xlab('') +
  theme_minimal()
ggsave('graphicalRepresentations-2box.pdf', width=width3, height=height3)

ggplot(df, aes(factor(""), y)) + 
  geom_violin(fill=stdGrey) + 
  xlab('') +
  theme_minimal()
ggsave('graphicalRepresentations-2violin.pdf', width=width3, height=height3)


ggplot(df, aes(y)) + 
  geom_histogram(color='black', fill=stdGrey, bins = 30) + 
  ylab('Anzahl') +
  theme_minimal()
ggsave('graphicalRepresentations-2hist.pdf', width=width3, height=height3)


## ECDFs

num <- sum(df$y <= -3) / nrow(df)

ggplot(df, aes(y)) + 
  ylab('ECDF [-]') +
  geom_segment(data=NULL, aes(x=0, y=0.5, xend=-6,yend=0.5), color='grey50', linetype='dotted') +
  geom_segment(data=NULL, aes(x=0, y=0.5, xend=0,yend=0.0), color='grey50', linetype='dotted') +
  geom_segment(data=NULL, aes(x=-3, y=num, xend=-3, yend=0.0), color='grey50', linetype='dotted') +
  geom_segment(data=NULL, aes(x=-3, y=num, xend=-6,yend=num), color='grey50', linetype='dotted') +
  stat_ecdf() +
  theme_minimal()
ggsave('graphicalRepresentations-ecdf.pdf', width=width, height=height3)

# Korrelations

library(sigmoid)
x <- seq(-10, 10, length.out=100)
y <- sigmoid(x)

df1 <- data.frame(x=1:length(y), y=y)

ggplot(df1, aes(x,y)) + 
  geom_point() +
  theme_minimal()
ggsave('graphicalRepresentations-corr-sigmoid.pdf',width=width3, height=height3)

print(paste('sigmoid: r   = ', cor(df1$x, df1$y)))
print(paste('sigmoid: rho = ', cor(df1$x, df1$y, method='spearman')))

y2 <- c(y, rev(y))

df2 <- data.frame(x=1:length(y2), y=y2)

ggplot(df2, aes(x,y)) + 
  geom_point() +
  theme_minimal()
ggsave('graphicalRepresentations-corr-sigmoid2.pdf',width=width3, height=height3)


print(paste('sigmoid(2): r   = ', cor(df2$x, df2$y)))
print(paste('sigmoid(2): rho = ', cor(df2$x, df2$y, method='spearman')))

# exp

x <- seq(0,100, length.out = 100)
y <- exp(x)

df3 <- data.frame(x = x, y = y)
ggplot(df3, aes(x,y)) + 
  geom_point() +
  theme_minimal()
ggsave('graphicalRepresentations-corr-exp.pdf',width=width3, height=height3)

print(paste('exp: r   = ', cor(df3$x, df3$y)))
print(paste('exp: rho = ', cor(df3$x, df3$y, method='spearman')))

