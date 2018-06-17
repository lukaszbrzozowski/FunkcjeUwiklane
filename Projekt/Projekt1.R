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
x <- (0:1000/(1000/(3*(max(test[,1])-min(test[,1])))))
y <- rep(0, times = length(x))
for(i in 1:length(x)){
  l <- abs(k(x[i], p))
  k(0, 0)
  l[is.nan(l)] <- Inf
  y[i] <- p[which(l==min(l))][1]
  }
plot(x, y, typ="l", ylim=c(-0.1+min(test[,2]), max(test[,2])+0.1))
points(test, col="red")

