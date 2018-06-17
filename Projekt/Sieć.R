uwiklana <- function(X){
  stopifnot(ncol(X)==2)
  g <- function(x, y){
    suma <- 0
    for(i in 1:nrow(X)){
      e1 <- y - X[i, 2]
      suma <- suma + e1/sqrt((x-X[i,1])^2+(y-X[i,2])^2)
    }
    suma
  }
  g
}

test <- matrix(c(1, 1, 2, 2, 3, 4), ncol = 2, byrow = TRUE)
k <- uwiklana(test)
p <- 0:999/(999/(max(test[,2])-min(test[,2])))+min(test[,2])
x <- ((-1000):1000/(1000/(6)))
y <- rep(0, times = length(x))
for(i in 1:length(x)){
  l <- abs(k(x[i], p))
  k(0, 0)
  l[is.nan(l)] <- Inf
  y[i] <- p[which(l==min(l))][1]
}
plot(x, y, typ="l", ylim=c(-0.1+min(test[,2]), max(test[,2])+0.1))
punkty <- cbind(x, y)
head(punkty)

blad_najmniejszy <- Inf
k_najlepsze <- rep(Inf, times = 6)
pb <- progress::progress_bar$new(total = 100000)
k <- runif(6, -5, 5)
for(i in 1:100000){
  k <- runif(6, -5, 5)
  pb$tick()
  y2 <- (k[1]*abs(x-1)+k[2]*abs(x-2)+k[3]*abs(x-3)+k[4]*abs(x^2-1)+k[5]*abs(x^2-2)+k[6]*abs(x^2-3))/(abs(x-1)+abs(x-2)+abs(x-3)+abs(x^2-1)+abs(x^2-2)+abs(x^2-3))
  blad <- abs(sum(abs(y-y2)))
  if(blad < blad_najmniejszy){
    blad_najmniejszy <- blad
    k_najlepsze <- k
    y_najlepsze <- y2
    X_naj <- cbind(x, y_najlepsze)
    }
  
}
blad_najmniejszy
par(mfrow = c(1, 2))
plot(X_naj, typ="l", ylim=c(-0.1+min(test[,2]), max(test[,2])+0.1))
plot(x, y, typ="l", ylim=c(-0.1+min(test[,2]), max(test[,2])+0.1))
