library(dplyr)
library(rjags)
library(coda)
library(magrittr)
library(readr)
library(fastDummies)

# #Cargamos la base ya con las variables mudas incorporadas
# DF_VIH <- read.csv(choose.files())
# nrow(DF_VIH)
# 
# #Dado que son demasiadas observaciones tomamos una muestra de 30%
# #DF_VIH <- DF_VIH %>% sample_frac(0.30)
# DF_VIH = DF_VIH %>% filter(site_mexico == 1)
# 
# #creamos una variable llamada pais
# #DF_VIH$pais <- substr(DF_VIH$patient,1,2)
# 
# # IMPORTANTE: BORRAR VARIABLES NO SIGNIFICATIVAS SEGUN COX FRECUENTISTA
# 
# 
# #agrupamos por pais y tomamos muestra de un 
# set.seed(89)
# DF_VIH<- DF_VIH  %>% sample_frac(0.1)
# 
# count(DF_VIH, pais)
# 
# #Borrar la variable auxiliar pais
# DF_VIH$pais = NULL  
# 
# 
# 
# # Matriz diseño 
# X <- DF_VIH %>% select(male, mode_Bisexual:mode_Unknown, Edad_grupos_18...24.aÃ.os:art_grupo_primer_3)
# 
# # Tiempos de supervivencia 
# time = DF_VIH$supvi_dias
# 
# # Censura/falla
# delta = DF_VIH$death_y

# COnjunto original
df = readr::read_csv(file.choose())

# Tiempos de supervivencia 
time = df$supvi_dias

# Censura/falla
delta = df$death_y

# Matrix sin dummies 
df_sin = df %>% select(male,age, rna_v, cd4_v, art_grupo_ultimo)

# Dummies para art_grupo_ultimo
df_con <- dummy_cols(df_sin, select_columns = "art_grupo_ultimo", remove_selected_columns = TRUE)


# Matriz diseño 
X <- df_con

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
                  int.obs=int.obs, Nbetas=ncol(X), zeros = rep(0,nrow(X)))

#Función para inicializar el modelo 
init.jags <- function(){list(beta = rnorm(ncol(X)), lambda = runif(7,0.1))}

#Parámetros que vamos a monitorear   
param.jags <- c("beta", "lambda")  #paramteros a monitorear 

############Compilamos el modelo 
Modelo_compilado <- jags.model(data = data.jags, file = file.choose(), 
                               inits = init.jags, n.chains = 3)

#Mandamos llamar coda para tomar muestras para dist a posteriori 
update(Modelo_compilado, 5000)
res <- coda.samples(Modelo_compilado,variable.names=param.jags,n.iter=9000, n.thin=1)

#Unimos las 3 cadenas MCMC para hacer inferencia sobre los resultados de la simulación de 
#las dist posteriores 
results <- as.mcmc(do.call(rbind,res))

#Asiganamos a las entradas de results a beta y lambda 
lambda_names <- c()#vector para los hiperparams lambda 
beta_names <- c()#vector para los hiperparams beta 
for(i in 1:7){
  beta_names[i] = paste("beta",as.character(i), sep="")
  lambda_names[i] = paste("lambda",as.character(i), sep="")
}

#asignación segun los componentes de results 
for(i in 1:14){
  if(i <= 7){
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

