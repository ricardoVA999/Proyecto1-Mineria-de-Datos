library(haven)
library(ggplot2)
library(dplyr)
library(nortest)
library(cluster)
library(fpc)
library(e1071)

setwd("C:/Users/Zephyrus/Documents/U/7mo Semestre/Mineria de Datos/Proyecto1-Mineria-de-Datos")

#Procecamiento de datos para los datos de Nacimientos
nac2009 = read_sav("./BasesDeDatos/nacimientos2009.sav")
names(nac2009)[names(nac2009) == "Gretnm"] <- "grupetma"
nac2009$Escolam<-9
nac2009$Escolap<-9
nac2009$Areag<-NULL
nac2009$Añoocu<-NULL
nac2009$Naciop<-NULL
nac2009$Naciom<-NULL
nac2009$Ocupam<-NULL
nac2009$Ocupap<-NULL
nac2009$grupetma[nac2009$grupetma %in% 1] <- 6
nac2009$grupetma[nac2009$grupetma %in% 2] <- 7

nac2010 = read_sav("./BasesDeDatos/nacimientos2010.sav")
names(nac2010)[names(nac2010) == "mupnam"] <- "Mupnam"
names(nac2010)[names(nac2010) == "mupnap"] <- "Mupnap"
names(nac2010)[names(nac2010) == "muprem"] <- "Muprem"
nac2010$Areag<-NULL
nac2010$Añoocu<-NULL
nac2010$Naciop<-NULL
nac2010$Naciom<-NULL
nac2010$Ocupam<-NULL
nac2010$Ocupap<-NULL
nac2010$Gretnp[nac2010$Gretnp %in% 1] <- 6
nac2010$Gretnp[nac2010$Gretnp %in% 2] <- 7
nac2010$grupetma[nac2010$grupetma %in% 1] <- 6
nac2010$grupetma[nac2010$grupetma %in% 2] <- 7
nac2010$Escolam <- ifelse(nac2010$Escolam !=9,nac2010$Escolam+1,nac2010$Escolam )
nac2010$Escolap <- ifelse(nac2010$Escolap !=9,nac2010$Escolap+1,nac2010$Escolap )

nac2011 = read_sav("./BasesDeDatos/nacimientos2011.sav")
names(nac2011)[names(nac2011) == "Mupreg"] <- "mupreg"
names(nac2011)[names(nac2011) == "Muprep"] <- "muprep"
names(nac2011)[names(nac2011) == "munnam"] <- "Mupnam"
nac2011$Areag<-NULL
nac2011$Añoocu<-NULL
nac2011$Naciop<-NULL
nac2011$Naciom<-NULL
nac2011$Ocupam<-NULL
nac2011$Ocupap<-NULL
nac2011$Gretnp[nac2011$Gretnp %in% 1] <- 6
nac2011$Gretnp[nac2011$Gretnp %in% 2] <- 7
nac2011$grupetma[nac2011$grupetma %in% 1] <- 6
nac2011$grupetma[nac2011$grupetma %in% 2] <- 7
nac2011$Escolam <- ifelse(nac2011$Escolam !=9,nac2011$Escolam+1,nac2011$Escolam )
nac2011$Escolap <- ifelse(nac2011$Escolap !=9,nac2011$Escolap+1,nac2011$Escolap )

nac2012 = read_sav("./BasesDeDatos/nacimientos2012.sav")
names(nac2012)[names(nac2012) == "Mupreg"] <- "mupreg"
names(nac2012)[names(nac2012) == "Muprep"] <- "muprep"
names(nac2012)[names(nac2012) == "munnam"] <- "Mupnam"
nac2012$Paisrep<-NULL
nac2012$Paisnacp<-NULL
nac2012$Paisrem<-NULL
nac2012$Paisnacm<-NULL
nac2012$Naciop<-NULL
nac2012$Naciom<-NULL
nac2012$Ocupam<-NULL
nac2012$Ocupap<-NULL
nac2012$Gretnp[nac2012$Gretnp %in% 1] <- 6
nac2012$Gretnp[nac2012$Gretnp %in% 2] <- 7
nac2012$grupetma[nac2012$grupetma %in% 1] <- 6
nac2012$grupetma[nac2012$grupetma %in% 2] <- 7
nac2012$Escolam <- ifelse(nac2012$Escolam !=9,nac2012$Escolam+1,nac2012$Escolam )
nac2012$Escolap <- ifelse(nac2012$Escolap !=9,nac2012$Escolap+1,nac2012$Escolap )

nac2013 = read_sav("./BasesDeDatos/nacimientos2013.sav")
names(nac2013)[names(nac2013) == "Mupreg"] <- "mupreg"
names(nac2013)[names(nac2013) == "Muprep"] <- "muprep"
names(nac2013)[names(nac2013) == "PuebloPP"] <- "Gretnp"
names(nac2013)[names(nac2013) == "PuebloPM"] <- "grupetma"
nac2013$Paisrep<-NULL
nac2013$Paisnacp<-NULL
nac2013$Paisrem<-NULL
nac2013$Paisnacm<-NULL
nac2013$Naciop<-NULL
nac2013$Naciom<-NULL
nac2013$Ciuomad<-NULL
nac2013$Ciuopad<-NULL

nac2014 = read_sav("./BasesDeDatos/nacimientos2014.sav")
names(nac2014)[names(nac2014) == "Mupreg"] <- "mupreg"
names(nac2014)[names(nac2014) == "Muprep"] <- "muprep"
names(nac2014)[names(nac2014) == "PuebloPP"] <- "Gretnp"
names(nac2014)[names(nac2014) == "PuebloPM"] <- "grupetma"
names(nac2014)[names(nac2014) == "Munpnap"] <- "Mupnap"
nac2014$Paisrep<-NULL
nac2014$Paisnacp<-NULL
nac2014$Paisrem<-NULL
nac2014$Paisnacm<-NULL
nac2014$Ocupap<-NULL
nac2014$ciuomad<-NULL
nac2014$Escolam <- ifelse(nac2014$Escolam !=9,nac2014$Escolam+1,nac2014$Escolam )
nac2014$Escolap <- ifelse(nac2014$Escolap !=9,nac2014$Escolap+1,nac2014$Escolap )

nac2015 = read_sav("./BasesDeDatos/nacimientos2015.sav")
names(nac2015)[names(nac2015) == "Mupreg"] <- "mupreg"
names(nac2015)[names(nac2015) == "Muprep"] <- "muprep"
names(nac2015)[names(nac2015) == "PuebloPP"] <- "Gretnp"
names(nac2015)[names(nac2015) == "Munpnap"] <- "Mupnap"
names(nac2015)[names(nac2015) == "PuebloPM"] <- "grupetma"
nac2015$Añoocu<-NULL
nac2015$TipoIns<-NULL
nac2015$Paisrep<-NULL
nac2015$Paisnacp<-NULL
nac2015$Paisrem<-NULL
nac2015$Paisnacm<-NULL
nac2015$ViaPar<-NULL
nac2015$Ocupap<-NULL
nac2015$Ocupam<-NULL

nac2016 = read_sav("./BasesDeDatos/nacimientos2016.sav")
names(nac2016)[names(nac2016) == "Mupreg"] <- "mupreg"
names(nac2016)[names(nac2016) == "Muprep"] <- "muprep"
names(nac2016)[names(nac2016) == "PuebloPP"] <- "Gretnp"
names(nac2016)[names(nac2016) == "Munpnap"] <- "Mupnap"
names(nac2016)[names(nac2016) == "PuebloPM"] <- "grupetma"
nac2016$Añoocu<-NULL
nac2016$TipoIns<-NULL
nac2016$Paisrep<-NULL
nac2016$Paisnacp<-NULL
nac2016$Paisrem<-NULL
nac2016$Paisnacm<-NULL
nac2016$ViaPar<-NULL
nac2016$Ocupap<-NULL
nac2016$Ocupam<-NULL
nac2016$Escolam <- ifelse(nac2016$Escolam !=9,nac2016$Escolam+1,nac2016$Escolam )
nac2016$Escolap <- ifelse(nac2016$Escolap !=9,nac2016$Escolap+1,nac2016$Escolap )

nac2017 = read_sav("./BasesDeDatos/nacimientos2017.sav")
names(nac2017)[names(nac2017) == "Mupreg"] <- "mupreg"
names(nac2017)[names(nac2017) == "Muprep"] <- "muprep"
names(nac2017)[names(nac2017) == "PuebloPP"] <- "Gretnp"
names(nac2017)[names(nac2017) == "Munpnap"] <- "Mupnap"
names(nac2017)[names(nac2017) == "PuebloPM"] <- "grupetma"
nac2017$Añoocu<-NULL
nac2017$TipoIns<-NULL
nac2017$Paisrep<-NULL
nac2017$Paisnacp<-NULL
nac2017$Paisrem<-NULL
nac2017$Paisnacm<-NULL
nac2017$ViaPar<-NULL
nac2017$Ocupap<-NULL
nac2017$Ocupam<-NULL

nac2018 = read_sav("./BasesDeDatos/nacimientos2018.sav")
names(nac2018)[names(nac2018) == "Mupreg"] <- "mupreg"
names(nac2018)[names(nac2018) == "Muprep"] <- "muprep"
names(nac2018)[names(nac2018) == "PuebloPP"] <- "Gretnp"
names(nac2018)[names(nac2018) == "Munpnap"] <- "Mupnap"
names(nac2018)[names(nac2018) == "PuebloPM"] <- "grupetma"
nac2018$Añoocu<-NULL
nac2018$TipoIns<-NULL
nac2018$Paisrep<-NULL
nac2018$Paisnacp<-NULL
nac2018$Paisrem<-NULL
nac2018$Paisnacm<-NULL
nac2018$ViaPar<-NULL
nac2018$Ocupap<-NULL
nac2018$Ocupam<-NULL

nac2019 = read_sav("./BasesDeDatos/nacimientos2019.sav")
names(nac2019)[names(nac2019) == "Mupreg"] <- "mupreg"
names(nac2019)[names(nac2019) == "Muprep"] <- "muprep"
names(nac2019)[names(nac2019) == "PuebloPP"] <- "Gretnp"
names(nac2019)[names(nac2019) == "Munpnap"] <- "Mupnap"
names(nac2019)[names(nac2019) == "PuebloPM"] <- "grupetma"
nac2019$Añoocu<-NULL
nac2019$TipoIns<-NULL
nac2019$Paisrep<-NULL
nac2019$Paisnacp<-NULL
nac2019$Paisrem<-NULL
nac2019$Paisnacm<-NULL
nac2019$ViaPar<-NULL
nac2019$Ocupap<-NULL
nac2019$Ocupam<-NULL


nac2009<-zap_labels(nac2009)
nac2010<-zap_labels(nac2010)
nac2011<-zap_labels(nac2011)
nac2012<-zap_labels(nac2012)
nac2013<-zap_labels(nac2013)
nac2014<-zap_labels(nac2014)
nac2015<-zap_labels(nac2015)
nac2016<-zap_labels(nac2016)
nac2017<-zap_labels(nac2017)
nac2018<-zap_labels(nac2018)
nac2019<-zap_labels(nac2019)


nacimientos <- rbind(nac2009, nac2010, nac2011, nac2012, nac2013, nac2014, nac2015, nac2016, nac2017, nac2018, nac2019)
nrow(nacimientos)
ncol(nacimientos)
colnames(nacimientos)

nacimientos$Añoreg[nacimientos$Añoreg %in% "9"] <- "2009"
nacimientos$Añoreg[nacimientos$Añoreg %in% "10"] <- "2010"

#Variables cuantitativas de nacimientos
summary(nacimientos$Libras[nacimientos$Libras != 99])
qqnorm(nacimientos$Libras[nacimientos$Libras != 99], pch = 19, col = "gray50")
qqline(nacimientos$Libras[nacimientos$Libras != 99])
lillie.test(nacimientos$Libras[nacimientos$Libras != 99])

summary(nacimientos$Onzas[nacimientos$Onzas != 99])
qqnorm(nacimientos$Onzas[nacimientos$Onzas != 99], pch = 19, col = "gray50")
qqline(nacimientos$Onzas[nacimientos$Onzas != 99])
lillie.test(nacimientos$Onzas[nacimientos$Onzas != 99])

table(nacimientos$Edadp)
barplot(table(nacimientos$Edadp), main="Frecuencia edad del Padre")
summary(nacimientos$Edadp[nacimientos$Edadp != 999])
qqnorm(nacimientos$Edadp[nacimientos$Edadp != 999], pch = 19, col = "gray50")
qqline(nacimientos$Edadp[nacimientos$Edadp != 999])
lillie.test(nacimientos$Edadp[nacimientos$Edadp != 999])

table(nacimientos$Edadm)
barplot(table(nacimientos$Edadm), main="Frecuencia edad de la Madre")
summary(nacimientos$Edadm[nacimientos$Edadm != 999])
qqnorm(nacimientos$Edadm[nacimientos$Edadm != 999], pch = 19, col = "gray50")
qqline(nacimientos$Edadm[nacimientos$Edadm != 999])
lillie.test(nacimientos$Edadm[nacimientos$Edadm != 999])

table(nacimientos$Tohite)
barplot(table(nacimientos$Tohite), main="Total de hijos tenidos")
summary(nacimientos$Tohite[nacimientos$Tohite != 99])
lillie.test(nacimientos$Tohite[nacimientos$Tohite != 99])

table(nacimientos$Tohinm)
barplot(table(nacimientos$Tohinm), main="Total de hijos nacidos muertos")
summary(nacimientos$Tohinm[nacimientos$Tohinm != 99])
lillie.test(nacimientos$Tohinm[nacimientos$Tohinm != 99])

table(nacimientos$Tohivi)
barplot(table(nacimientos$Tohivi), main="Total de hijos vivos")
summary(nacimientos$Tohivi[nacimientos$Tohivi != 99])
lillie.test(nacimientos$Tohivi[nacimientos$Tohivi != 99])

#Variables cualitativas de nacimientos
barplot(table(nacimientos$Añoreg), ylim=c(0,400000), col="blue")
barplot(table(nacimientos$Depocu),  ylim=c(0,800000),col="skyblue", main = "Departamento de ocurrencia")
barplot(table(nacimientos$Mesocu),ylim=c(0,400000), col="yellow", main="Mes de ocurrencia")
barplot(table(nacimientos$Sexo), ylim=c(0,2500000), col="red", main="Sexo del recien nacido", names=c("Masculino", "Feminino"))
barplot(table(nacimientos$Tipar), main="Tipo de parto", names=c("Simple", "Doble", "Triple", "Multiple"), col="pink")
barplot(table(nacimientos$Gretnp), ylim=c(0,1200000), col="purple", main="Grupo etnico del padre")
barplot(table(nacimientos$grupetma), ylim=c(0,1500000), col="green", main="Grupo etnico de la madre")
barplot(table(nacimientos$Escivp), ylim=c(0,2000000),col="chartreuse", main="Estado civil del padre", names=c("Casado", "Soltero", "Unido", "Ignorado"))
barplot(table(nacimientos$Escivm), ylim=c(0,2500000),col="brown", main="Estado civil de la madre", names=c("Casada", "Soltera", "Unida", "Ignorado"))
barplot(table(nacimientos$Escolap), ylim=c(0,1700000), col="darkgoldenrod", main="Escolaridad del padre")
barplot(table(nacimientos$Escolam), ylim=c(0,1700000), col="darkolivegreen", main="Escolaridad de la madre")
barplot(table(nacimientos$Asisrec), ylim=c(0,3000000), col="darkorange4", main="Asistencia medica recibida")
barplot(table(nacimientos$Sitioocu), ylim=c(0,2000000), col="goldenrod", main="Sitio de ocurrencia")

#Cruce de variables de Nacimientos
edades<-data.frame(cbind(nacimientos$Edadp, nacimientos$Edadm))
row_sub = apply(edades, 1, function(row) all(row !=999 ))
edades<-edades[row_sub,]
plot(edades$X1, edades$X2, main="Correlacion Edad del padre vs Edad de la madre", xlab = "Edad del padre", ylab = "Edad de la madre")
cor(edades$X1, edades$X2)

mediaLibrasDepa<-nacimientos%>%group_by(Depocu)%>% summarise(Mean=mean(Libras[Libras != 99]), Max=max(Libras[Libras != 99]), Min=min(Libras[Libras != 99]), Median=median(Libras[Libras != 99]), Std=sd(Libras[Libras != 99]))
plot(mediaLibrasDepa$Depocu, mediaLibrasDepa$Mean, type = "h", lwd=10, col="lightpink4", main = "Media de peso por departamento", xlab = "Departamento", ylab = "Media de peso")

mediaNaciDepa<-nacimientos%>%group_by(Depocu)%>% summarise(Mean=mean(Tohite[Tohite != 99]),Max=max(Tohite[Tohite != 99]), Min=min(Tohite[Tohite != 99]))
plot(mediaNaciDepa$Depocu, mediaNaciDepa$Mean, type = "h", lwd=10, col="lightslateblue", main = "Media de hijos tenido por departamento", xlab = "Departamento", ylab = "Media de hijos tenidos", ylim = c(0,4) )

mediaEdadEscop<-nacimientos%>%group_by(Escolap)%>% summarise(Mean=mean(Edadp[Edadp != 999]))
plot(mediaEdadEscop$Escolap, mediaEdadEscop$Mean, type = "h", lwd=10, col="orange", main = "Media de edad por escolaridad del Padre", xlab = "Escolaridad del padre", ylab = "Edad media del padre", ylim = c(0,50))

mediaEdadEscom<-nacimientos%>%group_by(Escolam)%>% summarise(Mean=mean(Edadm[Edadm != 999]))
plot(mediaEdadEscom$Escolam, mediaEdadEscom$Mean, type = "h", lwd=10, col="seagreen", main = "Media de edad por escolaridad de la Madre", xlab = "Escolaridad de la madre", ylab = "Edad media de la madre", ylim = c(0,50))

mediaHijoEscop<-nacimientos%>%group_by(Escolap)%>% summarise(Mean=mean(Tohite[Tohite != 99]))
plot(mediaHijoEscop$Escolap, mediaHijoEscop$Mean, type = "h", lwd=10, col="slateblue4", main = "Media de hijos tenidos por escolaridad del padre", xlab = "Escolaridad del padre", ylab = "Media de hijos tenidos", ylim = c(0,4))

mediaHijoEscom<-nacimientos%>%group_by(Escolam)%>% summarise(Mean=mean(Tohite[Tohite != 99]))
plot(mediaHijoEscom$Escolam, mediaHijoEscom$Mean, type = "h", lwd=10, col="yellow4", main = "Media de hijos tenidos por escolaridad de la madre", xlab = "Escolaridad de la madre", ylab = "Media de hijos tenidos", ylim = c(0,4))

#Clustering se realiza con aquellos de caracter cuantitativo
#Determinando el numero de clusters adecuados
cuantidata<-cbind(nacimientos$Libras, nacimientos$Onzas, nacimientos$Edadm, nacimientos$Edadp, nacimientos$Tohinm, nacimientos$Tohite, nacimientos$Tohivi)

wss <- (nrow(cuantidata-1)*sum(apply(cuantidata,2,var)))
for (i in 2:10) 
  wss[i] <- sum(kmeans(cuantidata, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

#Se utiliza cmeans y se determina si es un buen agrupamiento con el metodo de la silueta. Esto se hace con un grupo muestral ya que el tamanio del dataset origianal es muy grande
means<-data.frame()
for (i in 1:100){
  a<-((20000*(i-1))+1)
  b<-(i*20000)
  fcm<-cmeans(cuantidata[a:b,],2)
  silfcm<-silhouette(fcm$cluster,dist(cuantidata[a:b,]))
  actualmean<-mean(silfcm[,3])
  means<-rbind(means, data.frame(actualmean))}

mean(means$actualmean)

fcm<-cmeans(cuantidata,2)
nacimientos$FCGrupos<-fcm$cluster
#plotcluster(cuantidata,fcm$cluster)

g1<-nacimientos[nacimientos$FCGrupos==1,]
g2notfinish<-nacimientos[nacimientos$FCGrupos==2,]

cuantidata2<-cbind(g2notfinish$Libras, g2notfinish$Onzas, g2notfinish$Edadm, g2notfinish$Edadp, g2notfinish$Tohinm, g2notfinish$Tohite, g2notfinish$Tohivi)
wss2 <- (nrow(cuantidata2-1)*sum(apply(cuantidata2,2,var)))
for (i in 2:10) 
  wss2[i] <- sum(kmeans(cuantidata2, centers=i)$withinss)
plot(1:10, wss2, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

#Segundo metodo de la silueta
means2<-data.frame()
for (i in 1:100){
  a<-((20000*(i-1))+1)
  b<-(i*20000)
  fcm<-cmeans(cuantidata2[a:b,],2)
  silfcm<-silhouette(fcm$cluster,dist(cuantidata2[a:b,]))
  actualmean<-mean(silfcm[,3])
  means2<-rbind(means2, data.frame(actualmean))}

mean(means2$actualmean)

fcm2<-cmeans(cuantidata2,2)
g2notfinish$FCGrupos2<-fcm2$cluster
plotcluster(cuantidata2,fcm2$cluster)

g2<-g2notfinish[g2notfinish$FCGrupos2==1,]
g3<-g2notfinish[g2notfinish$FCGrupos2==2,]

nrow(g1)
nrow(g2)
nrow(g3)

#Analisis grupo 1
table(g1$Edadp)
barplot(table(g1$Edadm), main = "Edades de la madre del Grupo 1")
barplot(table(g1$Escivp), main = "Estado civil del padre")
barplot(table(g1$Escolap), main = "Escolaridad del padre")
barplot(table(g1$Gretnp), main = "Grupo etnico del padre")
barplot(table(g1$Deprep), main = "Departamento de residencia del padre")

barplot(table(g1$Escivm), main = "Estado civil de la madre")
barplot(table(g1$Escolam), main = "Escolaridad de la madre")
barplot(table(g1$grupetma), main = "Grupo etnico de la madre")

mediaHijoEtnm1<-g1%>%group_by(grupetma)%>% summarise(Mean=mean(Tohite[Tohite != 99]))
plot(mediaHijoEtnm1$grupetma, mediaHijoEtnm1$Mean, type = "h", lwd=10, col="yellow4", main = "Media de hijos tenidos por etnia de la madre en el grupo 1", xlab = "Grupo etnico de la madre", ylab = "Media de hijos tenidos", ylim=c(0,4))

mediaHijoEscom1<-g1%>%group_by(Escolam)%>% summarise(Mean=mean(Tohite[Tohite != 99]))
plot(mediaHijoEscom1$Escolam, mediaHijoEscom1$Mean, type = "h", lwd=10, col="yellow4", main = "Media de hijos tenidos por escolaridad de la madre en el grupo 1", xlab = "Escolaridad de la madre", ylab = "Media de hijos tenidos", ylim = c(0,4))
