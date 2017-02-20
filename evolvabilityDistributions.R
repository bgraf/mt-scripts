source('util.R')
df <- readDataset()

# missing elements

missingCR <- sum(is.na(df$CR))
missingNSC <- sum(is.na(df$NSC))
missingPIC <- sum(is.na(df$PIC))
print(paste('Missing CR: ', missingCR, missingCR / nrow(df)))
print(paste('Missing NSC:', missingNSC, missingNSC / nrow(df)))
print(paste('Missing PIC:', missingPIC, missingPIC / nrow(df)))

# distributions

ggplot(df) + 
  geom_histogram(aes(CR), color="black", fill=stdGrey, binwidth = 0.1) + 
  ylab('Anzahl') +
  theme_minimal()
ggsave('evolvabilityDistributions-hist-cr.pdf', width=width3, height=height3)

ggplot(df) + 
  geom_histogram(aes(NSC), color="black", fill=stdGrey) + 
  ylab('Anzahl') +
  theme_minimal()
ggsave('evolvabilityDistributions-hist-nsc.pdf', width=width3, height=height3)


ggplot(df) + 
  geom_histogram(aes(PIC), color="black", fill=stdGrey, binwidth = 0.1) + 
  ylab('Anzahl') +
  theme_minimal()
ggsave('evolvabilityDistributions-hist-pic.pdf', width=width3, height=height3)

agg <- readAgg() #aggregate(. ~ Class, data=df, FUN=function(x) mean(x, na.rm=T))

ggplot(agg) + 
  geom_histogram(aes(CR), color="black", fill=stdGrey, binwidth = 0.1) + 
  ylab('Anzahl') +
  theme_minimal()
ggsave('evolvabilityDistributions-hist-agg-cr.pdf', width=width3, height=height3)


ggplot(agg) + 
  geom_histogram(aes(NSC), color="black", fill=stdGrey) + 
  ylab('Anzahl') +
  theme_minimal()
ggsave('evolvabilityDistributions-hist-agg-nsc.pdf', width=width3, height=height3)


ggplot(agg) + 
  geom_histogram(aes(PIC), color="black", fill=stdGrey, binwidth = 0.1) + 
  ylab('Anzahl') +
  theme_minimal()
ggsave('evolvabilityDistributions-hist-agg-pic.pdf', width=width3, height=height3)

# samples without CR = 0

nzero <- subset(df, CR > 0)

ggplot(nzero) + 
  geom_histogram(aes(CR), color="black", fill=stdGrey, binwidth = 0.1) + 
  ylab('Anzahl') +
  theme_minimal()

ggplot(nzero) + 
  geom_histogram(aes(NSC), color="black", fill=stdGrey) + 
  ylab('Anzahl') +
  theme_minimal()


ggplot(nzero) + 
  geom_histogram(aes(PIC), color="black", fill=stdGrey, binwidth = 0.1) + 
  ylab('Anzahl') +
  theme_minimal()

# CR vs. PIC:w

agg <- aggregate(df, by=list(df$Class), FUN=function(c) mean(c, na.rm=T))
crpeak <- select(agg, c(CR,PIC))

ggplot(agg) + 
  geom_point(data=unique(crpeak), aes(CR,PIC), alpha=0.5, color='grey30') + 
  #geom_smooth(aes(CR,PIC), color='black') +
  theme_minimal()
ggsave('evolvabilityDistributions-cr-vs-pic.pdf',width=widthFull,height=height3)

# NSCFS

ggplot(agg) + 
  stat_ecdf(aes(NSCFCS)) + 
  scale_x_continuous(trans='log10', breaks = c(1, 10, 100, 1000, 10000)) +
  ylab('ECDF [-]') + xlab('|FC|') +
  theme_minimal()
ggsave('evolvabilityDistributions-ecdf-nscfcs.pdf', width=width, height=height3)

nscs <- agg[!is.na(agg$NSC),]
hist(nscs$NSC)
summary(nscs$NSC)

ggplot(subset(nscs, NSC < 0), aes(abs(NSC))) + 
  stat_ecdf() + 
  scale_x_continuous(trans = "log10", breaks = c(0, 10^(-1), 10^0, 10^3, 10^6, 10^9),
                     labels = c(0, expression(10^{-1}),
                                expression(10^0),
                                expression(10^3),
                                expression(10^6),
                                expression(10^9))) + 
  ylab('ECDF [-]') + xlab('|NSC| > 0') +
  theme_minimal()
ggsave('evolvabilityDistributions-ecdf-nsc.pdf', width=width, height=height3)

# percent of zero NSCs

print(paste('Percent of zero NSCs:', sum(nscs$NSC >= 0.0) / nrow(nscs)))
