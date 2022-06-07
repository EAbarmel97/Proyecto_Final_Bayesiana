library(dplyr)
library(rjags)
library(coda)
library(magrittr)

#Cargamos la base ya con las variables mudas incorporadas
DF_VIH <- read.csv("~/Documents/Modelo_Cox_Bayesiano/Cox_JAGS/Cox_JAGS_VIH/base_oficial_dummies.csv")

#Dado que son demasiadas observaciones tomamos una muestra de 30%
DF_VIH <- DF_VIH %>% sample_frac(0.30)

#creamos una variable llamada pais
DF_VIH$pais <- substr(DF_VIH$patient,1,2)

#agrupamos por pais y tomamos muestra de un 
DF_VIH<- DF_VIH %>%  group_by(pais) %>% sample_frac(0.3)

count(DF_VIH, pais)

#Matriz diseño 
X <- DF_VIH %>% select(male, mode_Bisexual:art_grupo_primer_3)

# Eliminamos la columna pais para poder usar la matriz diseño 
X <- X[,-1]

# Tiempos de supervivencia 
time = DF_VIH$supvi_dias

# Censura/falla
delta = DF_VIH$death_y

######################## JAGS ###################################
K <- length(colnames(X)) #número de intervalo que escogemos 
a <- seq(min(time), max(time) + 0.01, length.out = K + 1) #vector de tiempos de censura 
int.obs <- matrix(data = NA, nrow = nrow(X), ncol = length(a) - 1)
D <- matrix(data = NA, nrow = nrow(X), ncol = length(a) - 1) 
for(i in 1:nrow(X)){
  for (k in 1:(length(a) - 1)){
    D[i, k] <- ifelse(time[i] - a[k] >= 0, 1, 0) * ifelse(a[k + 1] - time[i] > 0, 1, 0)#indicadora del intervalo
    int.obs[i, k] <- D[i, k] * k #indica en que intervalo cae la observación. 
  }
}

int.obs <- rowSums(int.obs) #vector que nos indica en que intervalo cae cada observación

#Datos con los cuales vamos a entrenar el modelo
data.jags <- list(n = nrow(X), m=length(a)-1, a = a, 
                  delta=delta, time=time, X=X, 
                  int.obs=int.obs, Nbetas=ncol(X), zeros=rep(0,nrow(X)))

#Función para inicializar el modelo 
init.jags <- function(){list(beta = rnorm(ncol(X)), lambda = runif(34,0.1))}

#Parámetros que vamos a monitorear   
param.jags <- c("beta", "lambda")  #paramteros a monitorear 

############Compilamos el modelo 
Modelo_compilado <- jags.model(data = data.jags, file = "/Users/enki/Documents/Modelo_Cox_Bayesiano/Cox_JAGS/Cox_JAGS_VIH/m_VIH.txt", 
                               inits = init.jags, n.chains = 3)

#Mandamos llamar coda para tomar muestras para dist a posteriori 
update(Modelo_compilado, 1000)
res <- coda.samples(Modelo_compilado,variable.names=param.jags,n.iter=50000, n.thin=10)

#Unimos las 3 cadenas MCMC para hacer inferencia sobre los resultados de la simulación de 
#las dist posteriores 
results <- as.mcmc(do.call(rbind,res))

#Asiganamos a las entradas de results a beta y lambda 
lambda_names <- c()#vector para los hiperparams lambda 
beta_names <- c()#vector para los hiperparams beta 
for(i in 1:34){
  beta_names[i] = paste("beta",as.character(i), sep="")
  lambda_names[i] = paste("lambda",as.character(i), sep="")
}

#asignación segun los componentes de results 
for(i in 1:68){
  if(i <= 34){
    assign(beta_names[i], results[,i]) # los primeros 34 son los coefs de regresión 
  }else{
    assign(lambda_names[i], results[,i]) #los últimos 34 son los parámetros de la fun 
    #de riesgo base
  }
}

#Trazas 
par(mfrow=c(2,4))
traceplot(results)

#Info sobre las dist posteriores
summary(results)

