library(gdata)
library(DescTools)
library(survival)

b_o <- read.csv(file.choose())

# Eliminamos indicadores innecesarios
b_o$X.1 <- NULL
b_o$X <- NULL
# la fecha de enrrolamiento ya fue usada anteriormente y ya no la necesitaremos
b_o$baseline_d <- NULL
# el tipo de tratamiento ya fue usado para la agrupoacion asi que ya no necesitamos
b_o$art_id <- NULL
# ya agrupamos las cuentas de CD4 y de CV asi que por grupo las tenemos y no las usamos de forma individual
b_o$cd4_v <- NULL
b_o$rna_v <- NULL
# Eliminamos las edades como continuo pues ya fueron categorizadas
b_o$age <- NULL
# La fecha de nacimiento es irrelevante pues ya tenemos las edades
b_o$birth_d <- NULL
# La fecha de diagnostico de la enfermedad no es importante pues usamos como t_0 a la fecha de enrolamiento
b_o$hivdiagnosis_d <- NULL 
# La precision del como fue capturada la fecha de cumpleaños tampoco es necesaria para el estudio
b_o$birth_d_a <- NULL
# Las fechas de cuando se tomaron las cuentas de celulas cd4, la fecha de seguimiento y la CV ya fueron usadas y no son relevantes en este momento
b_o$visit_d <- NULL
b_o$rna_d <- NULL
b_o$cd4_d <- NULL  
# las fechas de cuando inicio el tratamiento y de cuando lo termino ya fueron usadas y son irrelevantes al momento
b_o$art_sd <- NULL
b_o$art_ed <- NULL
# los subgrupos de tratamiento ya fueron usados para indicar a que grupo pertenecen, en la columna "art_grupo_primer" encontramos al grupo que corresponde bajo el primer tratamiento asignado y en la columna "art_grupo_ultimo" encontramos al grupo de tratammiento al que corresponde y con el ultimo tratamiento registrado
b_o$NNRTI_grupo <- NULL
b_o$IP_grupo <- NULL
b_o$ITRAN_grupo <- NULL
# con esta variable identificavamos antiguamente al primer tratamiento asignado por grupo de tratamiento bajo principal activo
b_o$art_grupo <- NULL

# veamos las variables restantes que entran al modelo
View(b_o)

# Primero generaremos la variable que contendra a los datos de supervivencia
datos_superviv <- Surv(b_o$supvi_dias, b_o$death_y)

aj1 <- survfit(datos_superviv~1, type = "kaplan-meier", conf.type="log-log")

plot(aj1, mark="|", lwd=2, xlab="Día", ylab="Supervivencia S(t)", main="Kaplan-Meier Poblacion completa",
     col.main="darkgreen")

aj2 <- survfit(datos_superviv~factor(b_o$male), type = "kaplan-meier", conf.type="log-log")

plot(aj2, mark="|", lwd=2, xlab="Día", ylab="Supervivencia S(t)", main="Kaplan-Meier Poblacion completa",
     col.main="darkgreen")

cox_frec <- coxph(datos_superviv ~ factor(b_o$male) + b_o$mode + b_o$site + b_o$Edad_grupos + 
                    b_o$rna_grupos + b_o$cd4_grupos + factor(b_o$art_grupo_primer)+ factor(b_o$art_grupo_ultimo))
summary(cox_frec)