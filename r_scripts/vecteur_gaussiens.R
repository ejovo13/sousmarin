##on simule n réalisations d'un vecteur
##gaussien de moyenne µ et de variance ??

mvrgauss <- function(n,mu,Sigma) {
  d = length(mu)
  X = matrix(NA,n,d) # n réalisations d'un d vecteur
  Z = matrix(...,n,d)
  E = eigen(Sigma)
  S = ... # vecteurs propres
  D = ... # valeurs propres
  X = ...
  return(X)
}

##Simuler 500 réalisations un vecteur gaussien de dimension 3 de moyenne µ et de
##covariance ??

mu = c(1,0,-1)
sigma = rbind(c(1,7,-3),c(7,2,0),c(-3,0,3))
d=3
mu = matrix(c(1,0,-1),d,1)
Sigma = matrix(c(1,.7,-.3;.7,2,0,-.3,0,3),d,d)
n = 500
X = mvrgauss(n,mu,Sigma)
pairs(X,pch=20)