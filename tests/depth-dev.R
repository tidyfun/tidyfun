
modbanddepth <- function(data, argvals) {
  # check if argvals are equidistant
  ranks <- apply(data, 2, rank)
  n <- nrow(data)
  
  
} 
"n.a", "n.b" and "match" are p by n matrices; "depth" is a vector of length n;
R[i,] = rank of M[i,], for each i=1,...,p;
n.a = n-R;
n.b = R-1;
match = n.a*n.b;
proportion = sum(match[,j])/p, for each j=1,...,n;
depth = (proportion+n-1)/nchoose2;
