uwiklana <- function(X){
  stopifnot(ncol(X)==2)
  g <- function(x, y){
      sum(sqrt((x-X[,1])^2+((y - X[, 2])^2)))
  }
  g
}
k <- uwiklana(matrix(c(1, 1, 2, 2, 3, 4), nrow = 3, byrow = TRUE))
Der <- Deriv(k, "y")
Der
Der_x <- Deriv(Der, "x")
Der_y <- Deriv(Der, "y")
Der_y_x <- function(x, y) {-Der_x(x, y)/Der_y(x, y)}
Der_y_x
x <- (1:100)/(100/5)
y <- rep(0, times = length(x))
p <- 0:999/(999/3) + 1
pb <- progress::progress_bar$new(total = 100000)
for(i in 1:length(x)){
  l <- rep(Inf, times = length(p))
  for(j in 1:length(p)){
  pb$tick()
  l[j] <- abs(Der_y_x(x[i], p[j]))
  y[i] <- p[which(l==min(l))][1]
  }
  l[is.nan(l)] <- Inf
  y[i] <- p[which(l==min(l))][1]
}
testX <- cbind(x, y)
plot(testX)
max(x)
