library(dplyr)
library(rjags)
library(coda)
library(magrittr)

df = read.csv(choose.files())

# Matriz datos
X = df %>% select(male, mode_Bisexual:art_grupo_primer_3)

# Tiempos de supervivencia 
time = df$supvi_dias

# delta 
delta = df$death_y


######################## JAGS


K <- length(colnames(X)) #número de intervalo que escogemos 
a <- seq(min(time), max(time) + 0.001, length.out = K + 1) #vector de tiempos de censura 
int.obs <- matrix(data = NA, nrow = nrow(X), ncol = length(a) - 1)
D <- matrix(data = NA, nrow = nrow(X), ncol = length(a) - 1) 
for(i in 1:nrow(X)){
  for (k in 1:(length(a) - 1)){
    D[i, k] <- ifelse(time[i] - a[k] > 0, 1, 0) * ifelse(a[k + 1] - time[i] > 0, 1, 0)#indicadora del intervalo
    int.obs[i, k] <- D[i, k] * k #indica en que intervalo (1,2,3) cae la observación. 
  }
}

int.obs <- rowSums(int.obs) #vector que nos indica en que intervalo cae cada observación