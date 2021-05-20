library(haven)
library(ggplot2)
library(dplyr)
library(nortest)
library(cluster)
library(fpc)
library(e1071)
library(rpart)
library(rpart.plot)
library(caret)



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
nac2009$Gretnp[nac2009$Gretnp %in% 2] <- 4
nac2009$grupetma[nac2009$grupetma %in% 2] <- 4
nac2009$Asisrec[nac2009$Asisrec %in% 9] <- 5
nac2009$Sitioocu[nac2009$Sitioocu %in% 4] <- 5
nac2009$Sitioocu[nac2009$Sitioocu %in% 3] <- 4


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
nac2010$Gretnp[nac2010$Gretnp %in% 2] <- 4
nac2010$grupetma[nac2010$grupetma %in% 2] <- 4
nac2010$Escolam <- ifelse(nac2010$Escolam !=9,nac2010$Escolam+1,nac2010$Escolam )
nac2010$Escolap <- ifelse(nac2010$Escolap !=9,nac2010$Escolap+1,nac2010$Escolap )
nac2010$Sitioocu[nac2010$Sitioocu %in% 4] <- 5
nac2010$Sitioocu[nac2010$Sitioocu %in% 3] <- 4

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
nac2011$Gretnp[nac2011$Gretnp %in% 2] <- 4
nac2011$grupetma[nac2011$grupetma %in% 2] <- 4
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
nac2012$Gretnp[nac2012$Gretnp %in% 2] <- 4
nac2012$grupetma[nac2012$grupetma %in% 2] <- 4
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
nac2014$Asisrec[nac2014$Asisrec %in% 4] <- 2
nac2014$Asisrec[nac2014$Asisrec %in% 5] <- 4
nac2014$Asisrec[nac2014$Asisrec %in% 6] <- 5

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
nac2015$Asisrec[nac2015$Asisrec %in% 4] <- 2
nac2015$Asisrec[nac2015$Asisrec %in% 5] <- 4
nac2015$Asisrec[nac2015$Asisrec %in% 6] <- 5

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
nac2016$Asisrec[nac2016$Asisrec %in% 2] <- 3
nac2016$Asisrec[nac2016$Asisrec %in% 4] <- 2
nac2016$Asisrec[nac2016$Asisrec %in% 5] <- 4
nac2016$Asisrec[nac2016$Asisrec %in% 6] <- 5

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
nac2017$Asisrec[nac2017$Asisrec %in% 4] <- 2
nac2017$Asisrec[nac2017$Asisrec %in% 3] <- 4
nac2017$Asisrec[nac2017$Asisrec %in% 5] <- 3
nac2017$Asisrec[nac2017$Asisrec %in% 6] <- 5

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
nac2018$Asisrec[nac2018$Asisrec %in% 4] <- 2
nac2018$Asisrec[nac2018$Asisrec %in% 3] <- 4
nac2018$Asisrec[nac2018$Asisrec %in% 5] <- 3
nac2018$Asisrec[nac2018$Asisrec %in% 6] <- 5

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
nac2019$Asisrec[nac2019$Asisrec %in% 4] <- 2
nac2019$Asisrec[nac2019$Asisrec %in% 3] <- 4
nac2019$Asisrec[nac2019$Asisrec %in% 5] <- 3
nac2019$Asisrec[nac2019$Asisrec %in% 6] <- 5

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

nacimientos$Escolap[nacimientos$Escolap %in% "1"] <- "NoEdu"
nacimientos$Escolap[nacimientos$Escolap %in% "2"] <- "Primaria"
nacimientos$Escolap[nacimientos$Escolap %in% "3"] <- "Basica"
nacimientos$Escolap[nacimientos$Escolap %in% "4"] <- "Diversificado"
nacimientos$Escolap[nacimientos$Escolap %in% "5"] <- "Universitario"
nacimientos$Escolap[nacimientos$Escolap %in% "6"] <- "Universitario"
nacimientos$Escolap[nacimientos$Escolap %in% "7"] <- "Universitario"

nacimientos$Onzas[nacimientos$Onzas %in% 99] <- 0

nacimientos$Onzas<-nacimientos$Onzas*0.0625
nacimientos$Libras<-nacimientos$Libras+nacimientos$Onzas
nacimientos$Onzas<-NULL

#Evitando overfitting
nacimientos$Mesreg<-NULL
nacimientos$Depreg<-NULL
nacimientos$mupreg<-NULL
nacimientos$Onzas<-NULL

nacimientos$Mupocu<-NULL
nacimientos$muprep<-NULL
nacimientos$Muprem<-NULL
nacimientos$Mupnap<-NULL
nacimientos$Mupnam<-NULL

nacimientos$Añoreg<-NULL

nacimientos$Tohinm<-NULL
nacimientos$Tohivi<-NULL
#nacimientos$Tohite

#Predicciones padre

nacDatP<-nacimientos[!(nacimientos$Escolap==9),]

nacDatP$Escolam<-NULL
nacDatP$Edadm<-NULL
nacDatP$Deprem<-NULL
nacDatP$grupetma<-NULL
nacDatP$Escivm<-NULL
nacDatP$Depnam<-NULL


nacDatP$Escolap <- as.factor(nacDatP$Escolap)
nacDatP$Depocu <- as.factor(nacDatP$Depocu)
nacDatP$Sexo <- as.factor(nacDatP$Sexo)
nacDatP$Tipar <- as.factor(nacDatP$Tipar)
nacDatP$Deprep <- as.factor(nacDatP$Deprep)
nacDatP$Gretnp <- as.factor(nacDatP$Gretnp)
nacDatP$Escivp <- as.factor(nacDatP$Escivp)
nacDatP$Asisrec <- as.factor(nacDatP$Asisrec)
nacDatP$Sitioocu <- as.factor(nacDatP$Sitioocu)


table(nacDatP$Escolap)
nrow(nacDatP)
ncol(nacDatP)

set.seed(1234)

noEDPa<-nacDatP[nacDatP$Escolap=="NoEdu",]
priPa<-nacDatP[nacDatP$Escolap=="Primaria",]
basPa<-nacDatP[nacDatP$Escolap=="Basica",]
divPa<-nacDatP[nacDatP$Escolap=="Diversificado",]
uniPa<-nacDatP[nacDatP$Escolap=="Universitario",]

numFilasTrainNoEdPa<-sample(nrow(noEDPa), 0.6*nrow(noEDPa))#6
trainNoEdPa<-noEDPa[numFilasTrainNoEdPa,]

numFilasTrainPriPa<-sample(nrow(priPa), 0.29*nrow(priPa))#35
trainPriPa<-priPa[numFilasTrainPriPa,]

numFilasTrainBasPa<-sample(nrow(basPa), 0.8*nrow(basPa))#8
trainBasPa<-basPa[numFilasTrainBasPa,]

numFilasTrainDivPa<-sample(nrow(divPa), 0.7*nrow(divPa))#8
trainDivPa<-divPa[numFilasTrainDivPa,]

numFilasTrainUniPa<-sample(nrow(uniPa), 0.7*nrow(uniPa))#7
trainUniPa<-uniPa[numFilasTrainUniPa,]
trainUniPa<-rbind(trainUniPa, trainUniPa[rep(1:60000, 2), ])




testNoEdPa<-noEDPa[-numFilasTrainNoEdPa,]
numFilasTrainNoEdPa<-sample(nrow(testNoEdPa), 0.6*nrow(testNoEdPa))
testPriPa<-testNoEdPa[numFilasTrainNoEdPa,]

testPriPa<-priPa[-numFilasTrainPriPa,]
numFilasTrainPriPa<-sample(nrow(testPriPa), 0.15*nrow(testPriPa))
testPriPa<-testPriPa[numFilasTrainPriPa,]

testBasPa<-basPa[-numFilasTrainBasPa,]

testDivPa<-divPa[-numFilasTrainDivPa,]

testUniPa<-uniPa[-numFilasTrainUniPa,]




training<-rbind(trainNoEdPa, trainPriPa, trainBasPa, trainDivPa, trainUniPa)
test<-rbind(testNoEdPa, testPriPa, testBasPa, testDivPa, testUniPa)

table(training$Escolap)
table(test$Escolap)
table(nacDatP$Escolap)


#Arbol de Clasificacion Prediccion Educacion del padre

arbolModelo<-rpart(Escolap~.,training,method = "class")
rpart.plot(arbolModelo)

prediccion <- predict(arbolModelo, newdata = test[1:14])
columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
test$prediccion<-columnaMasAlta

test$prediccion <- as.factor(test$prediccion)
levels(test$prediccion) <- levels(test$Escolap)
cfm <- confusionMatrix(test$prediccion, test$Escolap)

cfm

#Bayes Ingenuo Prediccion Educacion del padre

training$Diaocu <- as.factor(training$Diaocu)
training$Mesocu <- as.factor(training$Mesocu)
training$Depnap <- as.factor(training$Depnap)

test$Diaocu <- as.factor(test$Diaocu)
test$Mesocu <- as.factor(test$Mesocu)
test$Depnap <- as.factor(test$Depnap)


modelo<-naiveBayes(as.factor(Escolap)~., data=training)

predTest<-predict(modelo, newdata = test[,1:14])
test$prediccion <- predTest
test$prediccion <- as.factor(test$prediccion)
levels(test$prediccion) <- levels(test$Escolap)
cfm <- confusionMatrix(test$prediccion, test$Escolap)
cfm
