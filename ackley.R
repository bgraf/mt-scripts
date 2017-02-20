source('util.R')

a = 20
b = 0.2
c = 2*pi

ackley2D <- function(x, y) {
  s1 = -b*sqrt((x^2 + y^2) / 2)
  s2 = (cos(c*x) + cos(c*y)) / 2
  
  -a*exp(s1) - exp(s2) + exp(1)
}


x <- seq(-3,3,length.out=50)
y <- seq(-3,3,length.out=50)
z <- outer(x^2, y^2, `+`)


pdf('ackley.pdf', width=width, height=height)
persp(x,y,z, theta = 30, phi = 35, col = 'grey100', expand=0.5, d =5000)
dev.off()

x <- seq(-3, 3, length.out=50)
y <- seq(-3, 3, length.out=50)
z <- outer(x, y, ackley2D)

pdf('ackley2.pdf', width=width, height=height)
persp(x,y,z, theta = 30, phi = 35,  d = 5000, expand = 0.5)
dev.off()

# one dimensioan unimodal fitness

x <- seq(1,5)
y <- c(0 ,2, 0.5,3, 2)
pdf('ackley-multimodal.pdf', width=width, height=height3)
plot(x,y, type="l", ylab = "f(x)")
abline(v=c(1.5, 3.5), lty=2)
text(1.7, 0.2, labels = "(a)")
text(3.7, 0.2, labels = "(b)")
dev.off()

ggplot(data.frame(x=x,y=y), aes(x,y)) + 
  geom_line() + 
  geom_vline(xintercept = c(1.5, 3.5), linetype='dotted') + 
  theme_minimal() +
  annotate("text", label='(a)', x = 1.7, y = 0.2) +
  annotate("text", label='(b)', x = 3.7, y = 0.2) +
  xlab('x') + ylab('f(x)')
ggsave('ackley-multimodal2.pdf', width=width, height=height3)


x <- seq(1,5)
y <- c(0.1,0.1,1,0.1,0.1)
pdf('ackley-bad-step.pdf',width=width, height=height3)
plot(x,y, type="s", ylab = "f(x)", ylim=c(0,1))
dev.off()

ggplot(data.frame(x=x, y=y), aes(x,y)) + geom_step() + theme_minimal() + ylab('f(x)') + ylim(0,1)
ggsave('ackley-bad-step2.pdf', width=width, height=height3)

set.seed(42)
x <- seq(1,5, length.out=30)
y <- runif(length(x),0,1)
pdf('ackley-bad-random.pdf',width=width, height=height3)
plot(x,y,type="l", ylab = "f(x)", ylim=c(0,1))
dev.off()

ggplot(data.frame(x=x, y=y), aes(x,y)) + geom_line() + theme_minimal() + ylab('f(x)') + ylim(0,1)
ggsave('ackley-bad-random2.pdf', width=width, height=height3)


