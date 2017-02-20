

# Specific CR is given as number of improving iterations I and a total
# number of iterations N.
# This yields the number of non-improving iterations (N-I)

i <- 5
n <- 10

crAndPic <- function(n, i) {
  m <- n - i

  if (i == m) {
    s01 <- i
    s10 <- i-1
  } else {
    s01 <- s10 <- min(i, m)  
  }
  
  p01 <- s01 / (n-1)
  p10 <- s10 / (n-1)
  
  t01 <- ifelse(p01 > 0.0, p01 * log2(p01), 0.0)
  t10 <- ifelse(p10 > 0.0, p10 * log2(p10), 0.0)
  
  pic <- - t01 - t10
    
  cr <- i / (n+1)
  
    
  list(pic=pic, cr=cr, s01, s10)
}

N <- 10000

pics <- c()
crs <- c()

for (i in 1:N) {
  res <- crAndPic(N, i)
  pics[i] <- res$pic
  crs[i] <- res$cr
}

plot(pics, type="l")

crAndPic(10,5)

crdf <- data.frame(PIC=pics, CR=crs)

ggplot(df, aes(CR,PIC)) + geom_point() + geom_line(data=crdf, aes(CR,PIC), color='blue')

a <- filter(df, PIC > 1.06)


plot(pics)

