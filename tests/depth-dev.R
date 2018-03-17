f <- c(rgp(3), 10 + rgp(2))
plot(f, points = TRUE, pch = as.character(1:length(f)))
data <- as.matrix(f)
modbanddepth(data)

modbanddepth <- function(data, argvals = seq_len(ncol(data))) {
  ranks <- apply(data, 2, rank)
  weights <- {
    #assign half interval length to 2nd/nxt-to-last points to first and last point
    #assign other half intervals to intermediate points
    lengths <- diff(argvals)
    c(lengths[1]/2, (lengths[-1] + tail(lengths, -1))/2, tail(lengths, 1)/2) / 
      diff(range(argvals))
  }
  n <- nrow(data)
  tmp <- colSums(t((n - ranks ) * (ranks - 1)) * weights)
  (tmp + n - 1)/choose(n, 2) 
} 




"n.a", "n.b" and "match" are p by n matrices; "depth" is a vector of length n;
R[i,] = rank of M[i,], for each i=1,...,p;
n.a = n-R;
n.b = R-1;
match = n.a*n.b;
proportion = sum(match[,j])/p, for each j=1,...,n;
depth = (proportion+n-1)/nchoose2;
