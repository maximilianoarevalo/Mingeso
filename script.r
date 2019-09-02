library(ggplot2)
library(ggpubr)
library(tidyr)

dir<-"/users/maximilianoarevalo/desktop"

tiempo.medio <- 60
tiempo.sd <- 20
N <- 18

set.seed(26*8)
datos1 <- rnorm(n = N, mean = tiempo.medio, sd = tiempo.sd)

set.seed(24*8)
datos2 <- rnorm(n = N, mean = tiempo.medio+10, sd = tiempo.sd-10)

df <- data.frame(cursoA = datos1, cursoB = datos2) #esta en formato ancho
#se lleva a formato largo
dfl <- gather( 
  data = df,
  key = "Curso",
  value = "Tiempo"
)

p<- gghistogram(
  data = dfl,
  x = "Tiempo",
  bins = 9, #barritas
  fill = "Curso"
)

z.obs <- (datos1 - tiempo.medio) / tiempo.sd #Z
#La distribucion Z tiene valores infinitos
#Se tiene un n-1 grado de libertad (valores que se pueden elegir para mantener la distribucion tal como esta)

grupos5.obs <- combn(z.obs,5) #datos 1 de tamaño 5 (5 grado de libertad)
#g5 <- grupos5.obs[,1:5]
g5sq <- grupos5.obs^2
chisq5 <- apply(g5sq, 2, sum)


#Bernoulli
p <- 4 / 18

x <- rbinom(100, 5, p) #binomial 
y <- rgeom(100, p)

B <- rep(0,18)
B[1:4] <- 1

Bi <- combn(B,5)
Bino <- apply(Bi, 2, sum) 


