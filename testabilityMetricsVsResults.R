library(Rmisc)
source('util.R')

df = readDataset()
dfa = aggByClass(df)

cols <- c("WMC","DIT","RFC","LCOM5","CBO")

for (col in cols) {
  cat("===========", col, "===========", sep = "\n")
  
  s <- summarySE(dfa, measurevar = 'BranchCoverage', groupvars = c(col), na.rm=T)
  s <- filter(s, N >= minObs)
  
  
  ggplot(s, aes_string(col, 'BranchCoverage')) +
    geom_point() +
    geom_errorbar(aes(ymin=BranchCoverage-se, ymax=BranchCoverage+se), colour="black", width=0.1*nrow(s)/width) +
    ylab('BC') + 
    ylim(0.2, 1.0) +
    theme_minimal()

  fileName <- paste0('testabilityMetricsVsResults-', col, '.pdf')
  ggsave(fileName, width=width3, height=height3)
}


makeHist <- function(level) {
  ggplot(filter(dfa, RFC == level), aes(BranchCoverage)) + 
    geom_histogram(fill=stdGrey, color='black', bins = 10) +
    ylab('Anzahl') +
    xlab('BC') + 
    theme_minimal()
  
  fileName <- paste0('testabilityMetricsVsResults-hist-', level, '.pdf')
  ggsave(fileName, width=width3, height=height3)
}

dfa = subset(dfa, !is.na(dfa$BranchCoverage))

makeHist <- function(var, level) {
    dfa$cuts = cut(dfa$BranchCoverage, breaks = seq(0, 1.0, by = 0.1), labels = seq(10, 100, by = 10), include.lowest = T)
    dfaf = filter_(dfa, paste(var, "==", level))
    
    s = data.frame(
      i = seq(10, 100, by = 10),
      c = sapply(
        seq(10, 100, by = 10),
        function (i) { sum(dfaf$cuts == i) })
    )
    
    ggplot(s, aes(factor(i), c)) + 
      geom_bar(fill=stdGrey, color='black', stat='identity') +
      ylab('Anzahl') +
      xlab('BC [%]') +
      theme_minimal()
  
  fileName <- paste0('testabilityMetricsVsResults-hist-', var, '-', level, '.pdf')
  ggsave(fileName, width=width3, height=height3)
}

conf <- list(
  list("WMC", 0, 10),
  list("RFC", 0, 10),
  list("CBO", 0, 6)
)

for (c in conf) {
  for (i in seq(c[[2]], by=c[[3]], length.out=3)) {
    makeHist(c[[1]], i)
    print(paste(c[[1]], paste(i)))
  }
}

# Number of test cases..

for (col in cols) {
  cat("===========", col, "===========", sep = "\n")
  
  s <- summarySE(dfa, measurevar = 'TestCnt', groupvars = c(col), na.rm=T)
  s <- filter(s, N >= minObs)
  
  
  ggplot(s, aes_string(col, 'TestCnt')) +
    geom_point() +
    geom_errorbar(aes(ymin=TestCnt-se, ymax=TestCnt+se), colour="black", width=0.1*nrow(s)/width) +
    ylab('TC') +
    theme_minimal()
  
  fileName <- paste0('testabilityMetricsVsResults-tc-', col, '.pdf')
  ggsave(fileName, width=width3, height=height3)
}

makeHist2 <- function(var, level, xl) {
  ggplot(filter_(dfa, paste(var, "==", level)), aes(TestCnt)) + 
    geom_histogram(fill=stdGrey, color='black', binwidth =2.5) +
    ylab('Anzahl') +
    xlab('TC') +
    xlim(0, xl) +
    theme_minimal()
  
  fileName <- paste0('testabilityMetricsVsResults-hist-tc-', var, '-', level, '.pdf')
  ggsave(fileName, width=width3, height=height3)
}

conf2 <- list(
  list("LCOM5", 0, 2, 130),
  list("RFC", 0, 10, 70),
  list("WMC", 0, 10, 30)
)

for (c in conf2) {
  for (i in seq(c[[2]], by=c[[3]], length.out=3)) {
    makeHist2(c[[1]], i, c[[4]])
    print(paste(c[[1]], paste(i)))
  }
}

## Upper bound on testcase numbers

df$TestCntUB <- with(
  df, 
  TestCnt + 
    BranchGoals - BranchCovered +
    MethodGoals - MethodCovered +
    LineGoals - LineCovered)

dfa = aggByClass(df)

for (col in cols) {
  cat("===========", col, "===========", sep = "\n")
  
  s <- summarySE(dfa, measurevar = 'TestCntUB', groupvars = c(col), na.rm=T)
  s <- filter(s, N >= minObsRequired)
  
  s2 <- summarySE(dfa, measurevar = 'TestCnt', groupvars = c(col), na.rm=T)
  s2 <- filter(s2, N >= minObsRequired)
  
  
  ggplot(s, aes_string(col, 'TestCntUB')) +
    geom_point(data = s2, aes_string(col, 'TestCnt'), shape=18, color='grey30') +
    geom_point() +
    geom_errorbar(aes(ymin=TestCntUB-se, ymax=TestCntUB+se), colour="black", width=0.1*nrow(s)/width) +
    ylab(expression(TC[u])) +
    theme_minimal()
  
  fileName <- paste0('testabilityMetricsVsResults-tc-', col, '-ub.pdf')
  ggsave(fileName, width=width3, height=height3)
}