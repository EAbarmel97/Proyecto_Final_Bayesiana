library(dplyr)
library(rjags)
library(coda)
library(magrittr)
library(readr)
library(fastDummies)

#Función para contar el tiempo de ejecucción
#sleep_1_min <- function(){Sys.sleep(60)}

#Semilla aleatorea 
set.seed(8)

#Conjunto original
VIH_HON <- read.csv(file.choose())

#Tiempos de supervivencia 
time <- VIH_HON$supvi_dias

#Censura/falla
delta <- VIH_HON$death_y

#Matrix sin variables mudas 
df_sin <- VIH_HON %>% select(male,age, rna_v, cd4_v)

#Dummies para art_grupo_ultimo
#df_con <- dummy_cols(df_sin, select_columns = "art_grupo_ultimo", remove_selected_columns = TRUE)

#Matriz diseño 
X <- df_sin   

######################## JAGS ##################################

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

#medias paras las priors de los coefs de regresión 
mf <- c(0.442,0.03428,-0.000006402, -0.006125)

#Datos con los cuales vamos a entrenar el modelo
data.jags <- list(n = nrow(X), m=length(a)-1, a = a, mf = mf, 
                  delta=delta, time=time, X=X, 
                  int.obs=int.obs, Nbetas=ncol(X), zeros = rep(0,nrow(X)))

#Función para inicializar el modelo 
#init.jags <- function(){list(beta = rnorm(ncol(X)), lambda = runif(7,0.1))}

#Parámetros que vamos a monitorear   
param.jags <- c("beta", "lambda")  #paramteros a monitorear 

############Compilamos el modelo 

#compilación del modelo con tiempo 
start_time1 <- Sys.time()
#sleep_1_min()
Modelo_compilado <- jags.model(data = data.jags, file = file.choose(), n.chains = 3)
end_time1 <- Sys.time()

model_comp_time <- end_time1 - start_time1 #tiempo de excución 
  
  
#Mandamos llamar coda para tomar muestras para dist a posteriori 
start_time2 <- Sys.time()
#sleep_1_min()
update(Modelo_compilado, 5000)
res <- coda.samples(Modelo_compilado,variable.names=param.jags,n.iter=50000, n.thin=3)
end_time2 <- Sys.time()

coda_samp_time <- end_time2 - start_time2 #tiempo de excución 

#Unimos las 3 cadenas MCMC para hacer inferencia sobre los resultados de la simulación de 
#las dist posteriores 
results <- as.mcmc(do.call(rbind,res))

#Asiganamos a las entradas de results a beta y lambda 
lambda_names <- c()#vector para los hiperparams lambda 
beta_names <- c()#vector para los hiperparams beta 
for(i in 1:4){
  beta_names[i] = paste("beta",as.character(i), sep="")
  lambda_names[i] = paste("lambda",as.character(i), sep="")
}

#asignación segun los componentes de results 
for(i in 1:8){
  if(i <= 4){
    assign(beta_names[i], results[,i]) # los primeros 34 son los coefs de regresión 
  }else{
    assign(lambda_names[i], results[,i]) #los últimos 34 son los parámetros de la fun 
    #de riesgo base
  }
}

#Trazas 
par(mfrow=c(4,4))
traceplot(results)

#Info sobre las dist posteriores
summary(results)

#test de gelman
gelman.diag(res)
gelman.plot(res)

