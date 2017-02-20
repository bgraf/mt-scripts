source('util.R')
df <- readDataset()

for (class in levels(df$Class)) {
  sub <- filter(df, Class == class)
  fileName <- paste0('crVsPicForEachClass-', class, '.pdf')
  print(fileName)

  ggplot(sub, aes(CR,PIC)) + 
    geom_hline(yintercept = mean(sub$PIC, na.rm=T), color='grey50') +
    geom_vline(xintercept = mean(sub$CR, na.rm=T), color='grey50') +
    geom_point(alpha=0.5) +
    theme_minimal() +
    xlim(0.0,1.0) + 
    ylim(0.0,1.2)
  ggsave(fileName, width=width3, height=height3)
}



for (class in levels(df$Class)) {
  sub <- subset(df, Class == class, select=c(PIC,CR))
  sub <- sub[complete.cases(sub),]
  
  meanCR <- mean(sub$CR, na.rm=T)
  meanPIC <- mean(sub$PIC, na.rm=T)

  if (!is.na(meanCR) && !is.na(meanPIC) && meanCR > 0.45 && meanCR < 0.55 && meanPIC < 0.2) {
    print(class)
  }
  
}


res <- data.frame(class=classes, mean=meanDists, med=medDists)

subset(res, mean > 0.5 & med > 0.5)

mdf <- data.frame(CR=mean(sub$CR, na.rm=T), PIC=mean(sub$PIC, na.rm=T))


