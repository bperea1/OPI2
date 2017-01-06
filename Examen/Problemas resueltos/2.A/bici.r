#Autor: Benjamin Perea
#Fecha 5- Enero -2016
# Descripcion del Script: analisis de ecobici


setwd("/home/ben/Documentos/ben/Documentos/OPI/Problemas resueltos/2.A/Files")

# File names
files = list.files(pattern="*.csv")
# combinamos los archivos para tener uno solo
ecobici = do.call(rbind, lapply(files, function(x) read.csv(x,sep = ";",stringsAsFactors = FALSE)))
str(ecobici)
head(ecobici,20)



#es exactamente lo mismo con mas consumo de memoria
ecobici2<-ecobici


#pequeña prueba
ecobici2$Fecha_Retiro<-as.Date(ecobici2$Fecha_Retiro,format = '%d/%m/%Y')
ecobici2$Fecha_Arribo<-as.Date(ecobici2$Fecha_Arribo,format = '%d/%m/%Y')


ecobici2$Genero_Usuario<-factor(ecobici2$Genero_Usuario)

ecobici2$Fecha_Retiro<-factor(ecobici2$Fecha_Retiro)
ecobici2$Ciclo_Estacion_Retiro<-factor(ecobici2$Ciclo_Estacion_Retiro)
ecobici2$Ciclo_Estacion_Arribo<-factor(ecobici2$Ciclo_Estacion_Arribo)
ecobici2$Hora_Arribo<-factor(ecobici2$Hora_Arribo)

#Matriz de afluencia por horario
afluencia2<-table(estaciones=ecobici2$Ciclo_Estacion_Retiro, Horario=ecobici2$Hora_Retiro)
summary(afluencia2)
afluencia2
#para construir un data frame
write.csv(afluencia2,"afluencia.csv")
afluencia<-read.csv("afluencia.csv",header = TRUE,row.names = 1)

names(afluencia)[1:21]<-c("H0","H1","H5","H6","H7","H8","H9","H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23")





#ESTACIONES CON MAYORES AFLUENCIAS PARA CREAR UNA CLASIFICACION EN BASE AL USO PROMEDIO POR DIA
estaciones<-rowMeans(afluencia)
estaciones<-sort(estaciones,decreasing = TRUE)
head(estaciones,50)

summary(estaciones)

#clasificacion basada en dispersion (invetada)
clasificacion<-ifelse(estaciones<1242 & estaciones>320, "Muy alto",ifelse(estaciones<=320 & estaciones>238,"Alto",ifelse(estaciones<=238 & estaciones>190,"Media",ifelse(estaciones<=190 & estaciones>180,"Bajo","Muy Bajo"))))

#metodo grafico
pie(table(clasificacion),main="Clasificacion inventada por mi")

#Matriz de origen destino
ODMatrix2<-table(origen=ecobici2$Ciclo_Estacion_Retiro,destino=ecobici2$Ciclo_Estacion_Arribo)
ODMatrix2

#clasificacion por perfiles de uso

#cluster herarquico con distacia euclidiana
hm<-dist(scale(afluencia))
hmodel<-hclust(hm,method = "complete")

plot(hmodel,xlab = "", sub = "", main = "Euclidean Distance", ylab = "",yaxt = "n")

#clasificacion con distancia manhatan

hm2<-dist(scale(afluencia),method = "manhattan")
hmodel2<-hclust(hm,method = "complete")

plot(hmodel2,xlab = "", sub = "", main = "Manhattan distance", ylab = "",yaxt = "n")

#lo que podemos observar es que la clasificacion no depende de la distancia



#kmeans data
require(graphics,stats)
kmodel<-kmeans(afluencia,6,iter.max = 100)
kmodel
plot(afluencia[,c(12,13)], xlab = "Afluencia a las 14 horas", ylab = "Afluencia a las 15 horas",main="Con 6-klusters" ,pch = 16, col = kmodel$clust, cex = 2.5) 
text(afluencia[,14], afluencia[ ,15], labels = row.names(afluencia), pos = 2)
#para comparar con respecto a los horarios de mayor uso
pairs(afluencia[,c(6:8,12:14,16:18)],col=kmodel$cluster)

pie(table(kmodel$cluster),main="K-MODEL CLASS")

#pie en 3d
require(plotrix)
lbls <- c("Exesiva", "Muy Alta", "Alta", "Media", "Baja","Muy baja")
pie3D(table(kmodel$cluster),labels=lbls,explode=0.1, col=kmodel$cluster,main="K-means Class ")

#BORRAR TODAS LAS VARIABLES
rm(list=ls(all=TRUE))
