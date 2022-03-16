
box_muller = function(u1, u2){
  
  z1 = sqrt(-2*log(u1))*cos(2*pi*u2)
  z2 = sqrt(-2*log(u1))*sin(2*pi*u2)
  
  z = numeric(length(u1)*2)
  
  for (i in 1:length(u1)){
    z[i] = z1[i]
  }
  
  for (i in 1:length(u1)){
    z[i+length(u1)] = z2[i]
  }
  
  z
}

u1=c(0.1 ,0.5 ,0.3)
u2=c(0.7 ,0.9 ,0.1)

box_muller(u1,u2)

box_muller_gauss = function(n){
  u1 = r_unif(n/2)
  u2 = r_unif(n/2)
  box_muller(u1,u2)
}


box_muller_gauss_nous = function(n){
  u1 = r_unif(n/2,n, 2^16 +1 )
  u2 = r_unif(n/2,n, 2^16 +1 )
  box_muller(u1,u2)
}

hist(box_muller_gauss(1000000))