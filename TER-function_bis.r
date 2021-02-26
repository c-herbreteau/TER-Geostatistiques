#### 17-02-21

library(plotly)
# Fonction Moyenne arithm?tique glissante

moy_gliss <- function(z, t){ # z matrice , t "rayon" fenetre
  #Grille de départ
  l1 <- dim(z)[1]
  l2 <- dim(z)[2]
  
  #Grille d'arrivée
  M = matrix(data = rep(0,(l1-2*t)*(l2-2*t)), nrow = (l1-2*t))
  
  for(i in (t+1):(l1-t)){
    for(j in (t+1):(l2-t)){
      fenetre = z[(i-t):(i+t), (j-t):(j+t)]
      M[i-t,j-t] <- mean(fenetre)
    }
  }
  return (M)
} 

affichage <- function(M){
  image(1:dim(M)[1],1:dim(M)[2], M, xlab = "X", ylab = "Y")
}

affichage_niveau <- function(M){
  fig <- plot_ly(x = 1:dim(t(M))[1], y = 1:dim(t(M))[2], z = t(M), type = "contour")
  fig
}

######## BRENOULLI ###########

#Bernoulli de p = 0.5, fenêtre de 5*5 (t = 2), dim(z)=14*14
z <- matrix(data =rbinom(14*14, 1, 0.5), nrow = 14)
M <- moy_gliss(z,2)
affichage(M) + title("Bernoulli de p = 0.5, fenêtre de 5*5 (t = 2), dim(z)=14*14")
affichage_niveau(M) 

#Bernoulli de p = 0.5, fenêtre de 11*11 (t = 5), dim(z)=14*14
z <- matrix(data =rbinom(14*14, 1, 0.5), nrow = 14)
M <- moy_gliss(z,5)
affichage(M) + title("Bernoulli de p = 0.5, fenêtre de 11*11 (t = 5), dim(z)=14*14")
affichage_niveau(M) 

#Bernoulli de p = 0.5, fenêtre de 21*21 (t = 10), dim(z)=200*150
z <- matrix(data =rbinom(200*150, 1, 0.5), nrow = 200)
M <- moy_gliss(z, 10)
affichage(M) + title("Bernoulli de p = 0.5, fenêtre de 21*21 (t = 10), dim(z)=200*150")
affichage_niveau(M) 

#Bernoulli de p = 0.5, fenêtre de 141*141 (t = 70), dim(z)=200*150
z <- matrix(data =rbinom(200*150, 1, 0.5), nrow = 200)
M <- moy_gliss(z, 70)
affichage(M) + title("Bernoulli de p = 0.5, fenêtre de 141*141 (t = 70), dim(z)=200*150")
affichage_niveau(M) 


######### NORMALE ##########

#Normale (0,1), fenêtre de 5*5 (t = 2), dim(z)=200*150
z <- matrix(data =rnorm(200*150), nrow = 200)
M <- moy_gliss(z, 2)
affichage(M) + title("Normale (0,1), fenêtre de 5*5 (t = 2), dim(z)=200*150")
affichage_niveau(M) 


######### POISSON ##########

#Poisson lambda = 3, fenêtre de 5*5 (t = 2), dim(z)=200*150
z <- matrix(data =rpois(200*150, 3), nrow = 200)
M <- moy_gliss(z, 2)
affichage(M) + title("#Poisson lambda = 3, fenêtre de 5*5 (t = 2), dim(z)=200*150")
affichage_niveau(M) 



