library(nlstools)
library(tidyverse)

setwd("G:/Unidades compartidas/Proy_ATERF/PT22107_I_LIDAR_Seforest_Burgos/202_Procesos/3_Cubicacion/")

base <- read.table("4_Arauzo_HD_pinaster.csv",header=TRUE, sep=";", na.strings="NA", dec=",", strip.white=TRUE)
base <- read.table("4_Arauzo_HD_silvestre.csv",header=TRUE, sep=";", na.strings="NA", dec=",", strip.white=TRUE)
base <- read.table("4_Arauzo_HD_nigra.csv",header=TRUE, sep=";", na.strings="NA", dec=",", strip.white=TRUE)
base <- read.table("4_Arauzo_HD_silvestre&nigra.csv",header=TRUE, sep=";", na.strings="NA", dec=",", strip.white=TRUE)
base <- read.table("4_Arauzo_HD_pinastersilvestre&nigra.csv",header=TRUE, sep=";", na.strings="NA", dec=",", strip.white=TRUE)

# base_2 <- base[-c(118,130,68),]


D<-base[,7] #cm
H<-base[,9] #m
Ho<-base[,13] #m
Dg<-base[,17] #Dg


data<-data.frame(D,H,Ho,Dg) %>% filter(H>0)
summary(data)

# ---------------- Hs --------------------------

Hs <-nls(H ~ 1.3+(a+b*Ho-c*Dg)*exp(d/D),data=data, start=list(a=5,b=0.95,c=0.05,d=-10.7))
summary(Hs)
plot(nlsResiduals(Hs))
plot(residuals(Hs) ~ D)

n <- nrow(data)
SS_total <- (n-1)*var(data$H)
SS_residual <- deviance(Hs)
R_cuadrado <- 1-(SS_residual/SS_total)

R_cuadrado

# ---------------- Hs2 --------------------------

Hs2 <-nls(H ~ 1.3+(a+b*Ho)*exp(d/D),data=data, start=list(a=5,b=0.95,d=-10.7))
summary(Hs2)
plot(residuals(Hs2))

n <- nrow(data)
SS_total <- (n-1)*var(data$H)
SS_residual <- deviance(Hs2)
R_cuadrado <- 1-(SS_residual/SS_total)

R_cuadrado

# ---------------- Hl --------------------------

Hl <-nls(H ~ exp(a+(b/D)),data=data, start=list(a=3,b=-15))
summary(Hl)
plot(residuals(Hl))

n <- nrow(data)
SS_total <- (n-1)*var(data$H)
SS_residual <- deviance(Hl)
R_cuadrado <- 1-(SS_residual/SS_total)

R_cuadrado



