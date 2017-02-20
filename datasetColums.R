source('util.R')

df = readDataset()

namesAndMissing = sapply(names(df), function (name) { sum(is.na(df[, name])) })

write.table(as.data.frame(namesAndMissing), file = stdout(), quote=F, col.names=F)
