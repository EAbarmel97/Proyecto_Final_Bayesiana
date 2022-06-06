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

dummy_colnames <- c("stage1","stage2","stage","stage4")

for(i in 1:4){
  X1[dummy_colnames[i]] <- ifelse(larynx$stage == i,1,0)
}

X1$Age <-larynx$age
  
X1$Diagr <- larynx$diagyr 

#Borramos la variable de apoyo 

X1$dummy <- NULL

write.csv(X1,"feat_mat.csv", row.names = FALSE)
  

    
  


  