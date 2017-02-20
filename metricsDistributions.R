source('util.R')

df <- readDataset()
dfa <- aggByClass(df)

cols <- c("NM","WMC","DIT","RFC","LCOM5","CBO")
labels <- c("NM", expression(WMC[CC]), "DIT", "RFC", "LCOM5", "CBO")

makePlot <- function(df, col) {
  ggplot(df, aes_string(col)) + 
    geom_histogram(fill=stdGrey,color='black', bins=10) +
    ylab('Anzahl') +
    theme_minimal()
}

makeEcdf <- function(df, col, lab) {
  ggplot(df, aes_string(col)) + 
    stat_ecdf() +
    ylab('ECDF [-]') + xlab(lab) +
    theme_minimal()
}

for (i in 1:length(cols)) {
  col <- cols[i]
  lab <- labels[i]
  makePlot(dfa, col)
  fileName <- paste0('metricsDistributions-',col,'.pdf')
  ggsave(fileName, width=width3, height=height3)
  
  makeEcdf(dfa, col, lab)
  fileName <- paste0('metricsDistributions-ecdf-',col,'.pdf')
  ggsave(fileName, width=width3, height=height3)
  
  print(fileName)
}

for (col in cols) {
  print(col)
  print(table(dfa[,col]))
}

