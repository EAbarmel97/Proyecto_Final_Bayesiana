library(dyplyr)
library(tidyverse)
library(KMsurv)
#cargamos la bd larynx 
data(larynx)

#columna de apoyo 
dummy <- rep(NA,nrow(larynx))
X1 <- as.data.frame(dummy)

#hacemos one-hot-encoding 
X1$time <- larynx$time

X1$stage1 <- ifelse(larynx$stage == 1,1,0)  
  
X1$stage2 <- ifelse(larynx$stage == 2,1,0) 

X1$stage3 <- ifelse(larynx$stage == 3,1,0) 

X1$stage4 <- ifelse(larynx$stage == 4,1,0) 
  
X1$Age <-larynx$age
  
X1$Diagr <- larynx$diagyr 

#Borramos la variable de apoyo 

X1$dummy <- NULL

write.csv(X1,"feat_mat.csv", row.names = FALSE)
  

    
  


  