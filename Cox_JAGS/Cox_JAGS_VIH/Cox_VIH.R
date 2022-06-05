library(dplyr)
library(rjags)
library(coda)
library(magrittr)

#Cargamos la base ya con las variables mudas incorporadas

DF_VIH <- read.csv("~/Documents/Modelo_Cox_Bayesiano/Cox_JAGS/Cox_JAGS_VIH/base_oficial_dummies.csv")

# Matriz diseño 
X <- DF_VIH %>% select(male, mode_Bisexual:art_grupo_primer_3)

# Tiempos de supervivencia 
time = DF_VIH$supvi_dias

# Censura/falla
delta = DF_VIH$death_y

######################## JAGS ###################################
K <- length(colnames(X)) #número de intervalo que escogemos 
a <- seq(min(time)-0.001, max(time) + 0.001, length.out = K + 1) #vector de tiempos de censura 
int.obs <- matrix(data = NA, nrow = nrow(X), ncol = length(a) - 1)
D <- matrix(data = NA, nrow = nrow(X), ncol = length(a) - 1) 
for(i in 1:nrow(X)){
  for (k in 1:(length(a) - 1)){
    D[i, k] <- ifelse(time[i] - a[k] > 0, 1, 0) * ifelse(a[k + 1] - time[i] > 0, 1, 0)#indicadora del intervalo
    int.obs[i, k] <- D[i, k] * k #indica en que intervalo cae la observación. 
  }
}

int.obs <- rowSums(int.obs) #vector que nos indica en que intervalo cae cada observación

#Datos con los cuales vamos a entrenar el modelo
data.jags <- list(n = nrow(X), m=length(a)-1, a = a, 
                  delta=delta, time=time, X=X, 
                  int.obs=int.obs, Nbetas=ncol(X), zeros=rep(0,nrow(X)))

#Función para inicializar el modelo 
init.jags <- function(){list(beta = rnorm(ncol(X)), lambda =runif(34,0.1))}

#Parámetros que vamos a monitorear   
param.jags <-c("beta", "lambda")  #paramteros a monitorear 





