runi = function(xo,n){
  x =vector("numeric", n)
  x[1] = xo
  for (i in seq_len(n)){
    x[i+1] = (75*x[i] + 74) %% (2**16 + 1)
  }
  x/(2**16 + 1)
}   
