# This function implements the Jacobi iteration
# for finding the solution to the equation Ax = b

Jacobi = function(A, b, x0 = 0, tol = 0.0001, K = 100) {
  d = diag(A)
  T = A
  diag(T) = 0
  
  n = nrow(T)
  x = numeric(n) + x0
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

# The convergence of the Jacobi iteration depends on the principle eigenvalue of the matrix T,
# T = D^{-1}(L + U), where A is decomposed as A = D - L - U
 
max_eigen_jacobi = function(A){
  D = diag(diag(A))
  T = D - A
  T = solve(D) %*% T
  abs(eigen(T)$values[1])
}
