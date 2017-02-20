library(GGally)

mkggcorr = function(df, method) {
  ggcorr(df, label=T, method=c('complete', method), label_alpha=F, color='grey10', size=4, label_round = 2, label_size = 4)
}
