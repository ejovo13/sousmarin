library(pracma)

f = function(x){
  
  if (x>=0 && x<=1){
     z= 6*x*(1-x)
  } else {
    z =0
  }
  z
}

#on simule pour une fonction sur un compact [0,1]
#on majore par un rectangle

acceptationrejet = function(f,N,a,b,M){
  z = numeric(N)
  j=0
  while(j != N){
    X = runif(1)*(b-a) + a
    Y = runif(1)*M
    if(Y<=f(X)){
      z[j]=X
      j=j+1
    }
  }
  z
}

x=linspace(0,1)
y=f(x)
plot(x,y)
acceptationrejet(f,10,0,1,1.5)

