
# 2016-11-21, bgraf

library(dplyr)

df <- read.csv('dataset.csv', header=T)

# general overview

print(paste('Number of rows:', nrow(df)))
print(paste('Number of cols:', ncol(df)))
print(paste('Number of classes:', length(levels(df$Class))))
print(paste('Number of projects:', length(unique(df$Project))))

# replications...

rowsByClass <- df %>% group_by(Class) %>% summarise(count = n())
print('Number of replications by class: counts')
print(summary(rowsByClass$count))
print(table(rowsByClass$count))

classesWithMissingReplications <- subset(rowsByClass, count < 50)

print('Classes with missing replications:')
print(classesWithMissingReplications)

# missing values

missingByColumn <- sapply(df, function(x) sum(is.na(x)))

totalMissing <- sum(missingByColumn)
totalPossible <- nrow(df) * ncol(df)

print(paste('Number of values missing:', totalMissing))
print(paste('Total number of values:', nrow(df)*ncol(df)))
print(paste('=>', round(totalMissing/totalPossible, digits=3), '%'))

# number of classes without CKJM

print(paste('Classes missing CKJM: ', length(unique(df[is.na(df$cCBO),]$Class))))


