# This function implements the Jackobi iteration

Jackobi = function(A, b, tol = 0.0001, K = 100) {
  d = diag(A)
  T = A
  diag(T) = 0
  
  n = nrow(T)
  x = numeric(n)
  k = 0; err = 1
  
  while(k < K & err > tol) {
    t = (b - T %*% x) / d
    err = max(abs(t - x))
    x = t
    k = k + 1
    #print(x[1:5])
  }
  print(k)
  return(x)
}