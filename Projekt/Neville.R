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
x <- ((-1000):1000/(1000/(6))) - 7
y <- rep(0, times = length(x))
for(i in 1:length(x)){
  l <- abs(k(x[i], p))
  k(0, 0)
  l[is.nan(l)] <- Inf
  y[i] <- p[which(l==min(l))][1]
}
plot(x, y, typ="l", ylim=c(-0.1+min(test[,2]), max(test[,2])+0.1))
punkty <- cbind(x, y)
T <- matrix(nrow = 3)

poly.neville <- function(x, y, x0) {
  
  n <- length(x)
  q <- matrix(data = 0, n, n)
  q[,1] <- y
  
  for (i in 2:n) {
    for (j in i:n) {
      q[j,i] <- ((x0 - x[j-i+1]) * q[j,i-1] - (x0 - x[j]) * q[j-1,i-1]) / (x[j] - x[j-i+1])
    }
  }
  
  res <- list('Approximated value'=q[n,n], 'Neville iterations table'=q)
  return(res)
}
k_najlepsze <- c(1, 2, 3)
blad_najlepszy <- Inf
wektor_wartosci <- rep(Inf, times=length(x))
pb <- progress::progress_bar$new(total = 100*length(x))
for(i in 1:100){
  k <- sample(1:length(x), 3)
  poly_nev <- rep(Inf, times=length(x))
  blad <- rep(Inf, times=length(x))
  wektor <- rep(Inf, times=length(x))
  for(j in 1:length(x)){
    pb$tick()
    if(j %in% k){
      blad[j] <- 0
    }else{
      wartosc <- poly.neville(c(punkty[k[1],1], punkty[k[2],1], punkty[k[3],1]), c(punkty[k[1],2], punkty[k[2],2], punkty[k[3],2]), punkty[j,1])[[1]]
      blad[j] <- abs(wartosc - punkty[j,2])
      wektor[j] <- wartosc   
      }    
  }
  if(sum(blad)<= blad_najlepszy){
    blad_najlepszy <- sum(blad)
    k_najlepsze <- k
    wektor_wartosci <- wektor
  }
}
k_najlepsze
blad_najlepszy
wektor_wartosci
najF <- cbind(x, wektor_wartosci)
par(mfrow=c(1, 2))
plot(x, y, type="l", ylim=c(-0.1+min(test[,2]), max(test[,2])+0.1))
plot(najF, type="l", ylim=c(-0.1+min(test[,2]), max(test[,2])+0.1))
warnings()
poly.neville(c(punkty[1,1], punkty[1000,1], punkty[2000,1]), c(punkty[1,2], punkty[1000,2], punkty[2000,2]), punkty[500,1])
