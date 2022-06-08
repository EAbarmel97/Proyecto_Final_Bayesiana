library(dplyr)
library(rjags)
library(coda)
library(magrittr)

#Cargamos la base ya con las variables mudas incorporadas
DF_VIH <- read.csv("~/Documents/Desktop/Proyecto_Final_Bayesiana/Cox_JAGS/Cox_JAGS_VIH/base_oficial_dummies.csv")
nrow(DF_VIH)

#Tiempos de supervivencia 
time = DF_VIH$supvi_dias

#Censura/falla
delta = DF_VIH$death_y

#creamos una variable llamada pais
DF_VIH$pais <- substr(DF_VIH$patient,1,2)

#IMPORTANTE: BORRAR VARIABLES NO SIGNIFICATIVAS SEGUN COX FRECUENTISTA

DF_VIH = DF_VIH %>% select(mode_Other..specify.in.mode_oth., site_argentina:site_peru,
                           Edad_grupos_25...44.años:Edad_grupos_60.años.o.más, 
                           rna_grupos_CV.indetectable, cd4_grupos_CD4.inicial...350, cd4_grupos_CD4.inicial.200...350, pais)

#agrupamos por pais y tomamos muestra de un 
set.seed(8)
DF_VIH<- DF_VIH %>%  group_by(pais) %>% sample_frac(0.50)

count(DF_VIH, pais)

#Borrar la variable auxiliar pais
DF_VIH$pais = NULL  

# Matriz diseño 
X <- DF_VIH 



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

#vector de medias para los coefs de rgresión que se encuentra en Cox frencuentista
mf <- c(0.514,2.87,2.501,1.92,2.147,2.095,0.511,0.860,1.19,0.261,-1.87,-0.926,0)

#Datos con los cuales vamos a entrenar el modelo
data.jags <- list(n = nrow(X), m=length(a)-1, a = a, 
                  delta=delta, time=time, X=X, mf= mf,
                  int.obs=int.obs, Nbetas=ncol(X), zeros=rep(0,nrow(X)))

#Función para inicializar el modelo 
init.jags <- function(){list(beta = rnorm(ncol(X)), lambda = runif(13,0.1))}

#Parámetros que vamos a monitorear   
param.jags <- c("beta", "lambda")  #paramteros a monitorear 

############Compilamos el modelo 
Modelo_compilado <- jags.model(data = data.jags, file = "~/Documents/Modelo_Cox_Bayesiano/Cox_JAGS/Cox_JAGS_VIH/m_VIH.txt", 
                               inits = init.jags, n.chains = 3)

#Mandamos llamar coda para tomar muestras para dist a posteriori 
update(Modelo_compilado, 1000)
res <- coda.samples(Modelo_compilado,variable.names=param.jags,n.iter=3000, n.thin=1)

#Unimos las 3 cadenas MCMC para hacer inferencia sobre los resultados de la simulación de 
#las dist posteriores 
results <- as.mcmc(do.call(rbind,res))

#Asiganamos a las entradas de results a beta y lambda 
lambda_names <- c()#vector para los hiperparams lambda 
beta_names <- c()#vector para los hiperparams beta 
for(i in 1:13){
  beta_names[i] = paste("beta",as.character(i), sep="")
  lambda_names[i] = paste("lambda",as.character(i), sep="")
}

#asignación segun los componentes de results 
for(i in 1:26){
  if(i <= 13){
    assign(beta_names[i], results[,i]) # los primeros 13 son los coefs de regresión 
  }else{
    assign(lambda_names[i], results[,i]) #los últimos 13 son los parámetros de la fun 
    #de riesgo base
  }
}

#Trazas 
par(mfrow=c(2,4))
traceplot(results)

#Info sobre las dist posteriores
summary(results)

#test de gelman
gelman.diag(res)


