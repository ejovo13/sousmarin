##explication, data sur https://ericmarcon.github.io/Krigeage/Krigeage.html

##kringage 

##on plot 10 espèces différentes dans une grille

devtools::install_github("EricMarcon/SpatDiv")
library("SpatDiv")
autoplot(rSpCommunity(n = 1, size = 100, S = 10) -> spCommunity)

##on calcule la courbe d'accumulation des espèces
divAccum <- DivAccum(spCommunity, n.seq = 1:10, q.seq = 0, Individual = TRUE)


##on interpole les points
library("akima")
Interpole <- with(divAccum, interp(x = SpCommunity$x, y = SpCommunity$y,
                                   z = Neighborhoods["0", "10", ], xo = seq(from = 0, to = 1, by = 0.01),
                                   yo = seq(from = 0, to = 1, by = 0.01)))
image(Interpole, col = topo.colors(128, alpha = 1), asp = 1)
contour(Interpole, add = TRUE)
with(divAccum, points(x = SpCommunity$x, y = SpCommunity$y, pch = 20))


##en spatial 

library("spatial")
Carte <- with(divAccum, surf.gls(np = 3, covmod = expcov, x = SpCommunity$x,
                                 y = SpCommunity$y, z = Neighborhoods["0", "10", ], d = 0.5))
Krieg <- prmat(Carte, xl = 0, xu = 1, yl = 0, yu = 1, n = 128)
image(Krieg, col = topo.colors(128, alpha = 1), asp = 1)
contour(Krieg, add = TRUE)
with(divAccum, points(x = SpCommunity$x, y = SpCommunity$y, pch = 20))