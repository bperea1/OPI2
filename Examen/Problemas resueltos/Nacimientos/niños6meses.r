#Autor: Benjamin Perea
#Fecha 5- Enero -2016
# Descripcion del Script: regresion lineal para el para el modelo


setwd("/home/ben/Documentos/ben/Documentos/OPI/Problemas resueltos/Nacimientos")
#leemos los datos
nacimientos<-read.csv("nacimiento.csv", header = TRUE, sep = ",", row.names = 1, stringsAsFactors = FALSE)

#vemos si existe un relacion lineal
plot(nacimientos$ID,nacimientos$o.1.años,col=3,pch=18, xlab ="años",ylab = "nacimientos", main="Bebes menores a 1 año en GAM")
axis(1,at=1:5, labels = c("1990","1995","2000","2005","2010"))

#CREAMOS UN MODELO LINEAL con tendencia de cada 5 años
modelo<-lm(o.1.años~ID,data=nacimientos)

#Vemos si los resultados son estadisticamente son significativos. Lo cual se puede apreciar en los resultados
summary(modelo)

#Graficamos el modelo y sus resultados
abline(modelo,col=2)

#predecimos el numero de recien nacidos habra (niños menores a un año) en la delegacion GAM
fit<-predict(modelo,data.frame(ID=6.2))

#con su intervalo de confianza
predict(modelo,data.frame(ID=6.2),interval = "confidence")

#ver tabla de indice de nacimientos por meses y frecuencias
resultado<-fit*.5126

#numero total de infantes menores a 6 meses en la delegacion Gustavo A. Madero
round(resultado,0) 

#BORRAR TODAS LAS VARIABLES
rm(list=ls(all=TRUE))

