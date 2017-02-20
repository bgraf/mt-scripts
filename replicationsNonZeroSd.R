
source('util.R')

df <- readDataset()

# Target value standard deviations over the replications

agg <- df %>% group_by(Class) %>%
  summarise(SDBranchCoverage = sd(BranchCoverage, na.rm=T),
            SDLineCoverage = sd(LineCoverage, na.rm=T),
            SDMethodCoverage = sd(MethodCoverage, na.rm=T),
            SDTestCnt = sd(TestCnt, na.rm=T),
            SDTestLen = sd(TestLen, na.rm=T),
            DBranchCoverage = max(BranchCoverage) - min(BranchCoverage),
            UBranchCoverage = length(unique(BranchCoverage, na.rm=T))
            )

max(agg[agg$SDBranchCoverage > 0.0,]$UBranchCoverage, na.rm=T)

