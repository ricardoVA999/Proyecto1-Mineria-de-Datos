library(haven)

setwd("C:/Users/Zephyrus/Documents/U/7mo Semestre/Mineria de Datos/Proyecto1-Mineria-de-Datos")

nac2009 = read_sav("./BasesDeDatos/nacimientos2009.sav")
names(nac2009)[names(nac2009) == "Gretnm"] <- "grupetma"
nac2009$Areag<-NULL
nac2009$Añoocu<-NULL
nac2009$Naciop<-NULL
nac2009$Naciom<-NULL

nac2010 = read_sav("./BasesDeDatos/nacimientos2010.sav")
names(nac2010)[names(nac2010) == "mupnam"] <- "Mupnam"
names(nac2010)[names(nac2010) == "mupnap"] <- "Mupnap"
names(nac2010)[names(nac2010) == "muprem"] <- "Muprem"
nac2010$Escolam<-NULL
nac2010$Escolap<-NULL
nac2010$Areag<-NULL
nac2010$Añoocu<-NULL
nac2010$Naciop<-NULL
nac2010$Naciom<-NULL

nac2011 = read_sav("./BasesDeDatos/nacimientos2011.sav")
names(nac2011)[names(nac2011) == "Mupreg"] <- "mupreg"
names(nac2011)[names(nac2011) == "Muprep"] <- "muprep"
nac2011$Escolam<-NULL
nac2011$Escolap<-NULL
nac2011$Areag<-NULL
nac2011$Añoocu<-NULL
nac2011$Naciop<-NULL
nac2011$Naciom<-NULL

nac2012 = read_sav("./BasesDeDatos/nacimientos2012.sav")
names(nac2012)[names(nac2012) == "Mupreg"] <- "mupreg"
names(nac2012)[names(nac2012) == "Muprep"] <- "muprep"
names(nac2012)[names(nac2012) == "munnam"] <- "Mupnam"
nac2012$Escolam<-NULL
nac2012$Escolap<-NULL
nac2012$Paisrep<-NULL
nac2012$Paisnacp<-NULL
nac2012$Paisrem<-NULL
nac2012$Paisnacm<-NULL
nac2012$Naciop<-NULL
nac2012$Naciom<-NULL

nac2013 = read_sav("./BasesDeDatos/nacimientos2013.sav")
names(nac2013)[names(nac2013) == "Mupreg"] <- "mupreg"
names(nac2013)[names(nac2013) == "Muprep"] <- "muprep"
names(nac2013)[names(nac2013) == "PuebloPP"] <- "Gretnp"
names(nac2013)[names(nac2013) == "Ciuopad"] <- "Ocupap"
names(nac2013)[names(nac2013) == "PuebloPM"] <- "grupetma"
names(nac2013)[names(nac2013) == "Ciuomad"] <- "Ocupam"
nac2013$Paisrep<-NULL
nac2013$Paisnacp<-NULL
nac2013$Escolap<-NULL
nac2013$Paisrem<-NULL
nac2013$Paisnacm<-NULL
nac2013$Escolam<-NULL
nac2013$Naciop<-NULL
nac2013$Naciom<-NULL

nac2014 = read_sav("./BasesDeDatos/nacimientos2014.sav")
names(nac2014)[names(nac2014) == "Mupreg"] <- "mupreg"
names(nac2014)[names(nac2014) == "Muprep"] <- "muprep"
names(nac2014)[names(nac2014) == "PuebloPP"] <- "Gretnp"
names(nac2014)[names(nac2014) == "ciuopad"] <- "Ocupap"
names(nac2014)[names(nac2014) == "PuebloPM"] <- "grupetma"
names(nac2014)[names(nac2014) == "ciuomad"] <- "Ocupam"
names(nac2014)[names(nac2014) == "Munpnap"] <- "Mupnap"
nac2014$Paisrep<-NULL
nac2014$Paisnacp<-NULL
nac2014$Paisrem<-NULL
nac2014$Paisnacm<-NULL
nac2014$Escolam<-NULL
nac2014$Escolap<-NULL

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
nac2015$Escolam<-NULL
nac2015$Escolap<-NULL
nac2015$ViaPar<-NULL

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
nac2016$Escolam<-NULL
nac2016$Escolap<-NULL
nac2016$ViaPar<-NULL

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
nac2017$Escolam<-NULL
nac2017$Escolap<-NULL
nac2017$ViaPar<-NULL

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
nac2018$Escolam<-NULL
nac2018$Escolap<-NULL
nac2018$ViaPar<-NULL

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
nac2019$Escolam<-NULL
nac2019$Escolap<-NULL
nac2019$ViaPar<-NULL


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


nacimientos$Añoreg[nacimientos$Añoreg %in% "9"] <- "2009"
nacimientos$Añoreg[nacimientos$Añoreg %in% "10"] <- "2010"
nacimientos$Libras[nacimientos$Libras %in% "99"] <- "NA"
nacimientos$Onzas[nacimientos$Onzas %in% "99"] <- "NA"
nacimientos$Sexo[nacimientos$Sexo %in% "1"] <- "Hombre"
nacimientos$Sexo[nacimientos$Sexo %in% "2"] <- "Mujer"
nacimientos$Tipar[nacimientos$Tipar %in% "1"] <- "Simple"
nacimientos$Tipar[nacimientos$Tipar %in% "2"] <- "Doble"
nacimientos$Tipar[nacimientos$Tipar %in% "3"] <- "Triple"
nacimientos$Tipar[nacimientos$Tipar %in% "4"] <- "Multiple"

nacimientos$Edadp[nacimientos$Edadp %in% "999"] <- "Ignorado"
nacimientos$Deprep[nacimientos$Deprep %in% "99"] <- "Ignorado"
nacimientos$Deprep[nacimientos$Deprep %in% "23"] <- "Extranjero"
nacimientos$muprep[nacimientos$muprep %in% "9999"] <- "Ignorado"
nacimientos$muprep[nacimientos$muprep %in% "2300"] <- "Extranjero"
nacimientos$Gretnp[nacimientos$Gretnp %in% "9"] <- "Ignorado"
nacimientos$Escivp[nacimientos$Escivp %in% "9"] <- "Ignorado"
nacimientos$Escivp[nacimientos$Escivp %in% "1"] <- "Soltero"
nacimientos$Escivp[nacimientos$Escivp %in% "2"] <- "Casado"
nacimientos$Escivp[nacimientos$Escivp %in% "3"] <- "Unido"
nacimientos$Depnap[nacimientos$Depnap %in% "99"] <- "Ignorado"
nacimientos$Depnap[nacimientos$Depnap %in% "23"] <- "Extranjero"
nacimientos$Mupnap[nacimientos$Mupnap %in% "9999"] <- "Ignorado"

nacimientos$Edadm[nacimientos$Edadm %in% "999"] <- "Ignorado"
nacimientos$Deprem[nacimientos$Deprem %in% "99"] <- "Ignorado"
nacimientos$Deprem[nacimientos$Deprem %in% "23"] <- "Extranjero"
nacimientos$Muprem[nacimientos$Muprem %in% "9999"] <- "Ignorado"
nacimientos$Muprem[nacimientos$Muprem %in% "2300"] <- "Extranjero"
nacimientos$grupetma[nacimientos$grupetma %in% "9"] <- "Ignorado"
nacimientos$Escivm[nacimientos$Escivm %in% "9"] <- "Ignorado"
nacimientos$Escivm[nacimientos$Escivm %in% "1"] <- "Soltero"
nacimientos$Escivm[nacimientos$Escivm %in% "2"] <- "Casado"
nacimientos$Escivm[nacimientos$Escivm %in% "3"] <- "Unido"
nacimientos$Depnam[nacimientos$Depnam %in% "99"] <- "Ignorado"
nacimientos$Depnam[nacimientos$Depnam %in% "23"] <- "Extranjero"
nacimientos$Mupnam[nacimientos$Mupnam %in% "9999"] <- "Ignorado"

nacimientos$Asisrec[nacimientos$Asisrec %in% "9"] <- "Ignorado"
nacimientos$Sitioocu[nacimientos$Sitioocu %in% "9"] <- "Ignorado"
nacimientos$Tohite[nacimientos$Tohite %in% "99"] <- "Ignorado"
nacimientos$Tohinm[nacimientos$Tohinm %in% "99"] <- "Ignorado"
nacimientos$Tohivi[nacimientos$Tohivi %in% "99"] <- "Ignorado"
