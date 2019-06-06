####################################################################################################
#  global.R - UC3
######################################################################################################
library("dplyr")
library("zoo")
library("stringr")
library("lubridate")
####################################################################################################
# DONNEES CARTOGRAPHIQUES
####################################################################################################
# DONNEES CARTOGRAPHIQUES : BORNES
####################################################################################################
# Bornes avec donnees de ruralite
# 15 VARIABLES : 
# Station;Commune;Adresse;Identifiant;Type;Numero;CodeInsee;Connecteur;StationCode;ChargeBoxIdentity;CodePDC;PDL_IDC;lat;lon;Ruralite
# TOURTON;CHABOTTES;Parking Haute Plaine SO 05260 CHABOTTES (05);FR*XXX*XXXXX*A*B1*D;ACCELEREE;16322;5029;Droit;FR*XXX*XXXXX*A;FR*XXX*XXXXX*A-1;FR*XXX*XXXXX*A*B1;PDLXXXXXXXXXXX;44,6428413391113;6,16910123825073;400 - Com. isolées hors influence des pôles
df <- read.csv2("bornesV3WithRuralite.csv",encoding = "UTF-8")
df$DateConsuel <- lubridate::dmy(df$DateConsuel)
table(df$DateConsuel)
#2 latitude et longitude as factor => as numeric
df$lat <- as.numeric(as.character(df$lat))
df$lon <- as.numeric(as.character(df$lon))

#liste des stations
listStations <- levels(df$Station)
nbstations <- length(listStations)
#liste des PDC
listPDC <- levels(df$CodePDC)
nbPDC <- length(listPDC)
####################################################################################################
# DONNEES HISTORIQUES : TRANSACTIONS
####################################################################################################
# Borne;Type;Ville;DateDebut;HeureDebut;DureeChargemin;Consommationkwh;DateFin;HeureFin;TypeClient;Status;DateTimeDebut;DateTimeFin
# FR*XXX*XXXXX*A*B1;Rapide;ANCELLE (05);23/03/2017; 12:28;0;0;23/03/2017; 12:28;Individuel;CHARGE;2017-03-23 12:28:00;2017-03-23 12:30:00
# Borne = CodePDC
transactions <- read.csv2("transactions.csv",encoding = "UTF-8")

######################################################################################################
# Consommation nulle ou Duree de charge nulle => transactions supprimees
transactions$Borne <- droplevels(transactions$Borne)
transactions$DateDebut <- lubridate::dmy(transactions$DateDebut)
#Ajouter moisDebut et moisFin
#debut
transactions$MoisDebut <- lubridate::month(transactions$DateDebut)
transactions$MoisFactorDebut <- as.factor(transactions$MoisDebut)
transactions$MonthDebut <- as.Date(as.yearmon(transactions$DateTimeDebut))
levels(transactions$MoisFactorDebut)<-c("janvier","fevrier","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","decembre")

#Ajouter l'annee
transactions$an <- lubridate::year(transactions$DateDebut)
transactions2018 <- filter(transactions,an==2018)

# Mettre la ville au format transaction dans la cartographie des bornes
transVilles <- transactions2018 %>% group_by(Borne,Ville) %>% summarise(Consommationkwh=sum(Consommationkwh),
                                                                        DureeChargemin=sum(DureeChargemin),
                                                                        nbTransaction=n())
transVilles <- rename(transVilles,CodePDC=Borne)
transVilles$CodePDC<- as.character(transVilles$CodePDC)

#ajout du champ Ville 
df <- left_join(df,transVilles,by="CodePDC")
#suppression des NA
df$nbTransaction <- ifelse(is.na(df$nbTransaction),0,df$nbTransaction)
df$Consommationkwh <- ifelse(is.na(df$Consommationkwh),0,df$Consommationkwh)
df$DureeChargemin <- ifelse(is.na(df$DureeChargemin),0,df$DureeChargemin)
#calcul des moyennes
df$meanTrans <- mean(df$nbTransaction)
df$meanDuree <- mean(df$DureeChargemin)
df$meanConso <- mean(df$Consommationkwh)
#calcul des ecarts sur annee 2018
# df$Ecart <- df$nbTransaction - mean(df$nbTransaction)
df$EcartTrans <- round(df$nbTransaction - mean(df$nbTransaction),0)
df$EcartDuree <- round(df$DureeChargemin - mean(df$DureeChargemin),0)
df$EcartConso <- round(df$Consommationkwh - mean(df$Consommationkwh),0)

#liste des Villes / Communes ?
listVilles <- levels(df$Commune)
nbVilles <- length(listVilles)

# Type client
transactionsAvecClientPerJour <- transactions %>% group_by(DateDebut,TypeClient) %>% summarise(Consommationkwh=sum(Consommationkwh),
                                                                                DureeChargemin=sum(DureeChargemin),
                                                                                consoParTrans=sum(Consommationkwh)/n(),
                                                                               nbTransaction=n())
transactionsAvecClientPerMois <- transactions %>% group_by(MonthDebut,TypeClient) %>% summarise(MoisFactorDebut=unique(MoisFactorDebut),
                                                                           Consommationkwh=sum(Consommationkwh),
                                                                           DureeChargemin=sum(DureeChargemin),
                                                                           consoParTrans=sum(Consommationkwh)/n(),
                                                                           nbTransaction=n())
transactionsAvecClientPerMois$an <- lubridate::year(transactionsAvecClientPerMois$MonthDebut)
transactionsAvecClientPerMois2017 <- filter(transactionsAvecClientPerMois,an==2017)
transactionsAvecClientPerMois2018 <- filter(transactionsAvecClientPerMois,an==2018)

######################################################################################################
# CONSOLIDATION JOUR 
# PAR TYPE DE JOUR
# regroupement des data par jour en kWh
transactionsPerJour <- transactions %>% group_by(DateDebut,Borne) %>% summarise(Consommationkwh=sum(Consommationkwh),
                                                                          DureeChargemin=sum(DureeChargemin),
                                                                          nbTransaction=n())
transactionsPerJour$DateDebut <- lubridate::ymd(transactionsPerJour$DateDebut)
transactionsPerJour$typeJourDebut <- lubridate::wday(transactionsPerJour$DateDebut)
transactionsPerJour$typeJourFactorDebut <- as.factor(transactionsPerJour$typeJourDebut)
levels(transactionsPerJour$typeJourFactorDebut)<-c("Dimanche","Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi")

#Ajouter l'annee
transactionsPerJour$an <- lubridate::year(transactionsPerJour$DateDebut)
transactionsPerJour2017 <- filter(transactionsPerJour,an==2017)
transactionsPerJour2018 <- filter(transactionsPerJour,an==2018)

################################
# CONSOLIDATION MOIS 
# regroupement des data par mois en kWh 
transactionsPerMois <- transactions %>% group_by(MonthDebut) %>% summarise(MoisFactorDebut=unique(MoisFactorDebut),
                                                                           Consommationkwh=sum(Consommationkwh),
                                                                           DureeChargemin=sum(DureeChargemin),
                                                                           nbTransaction=n())
#regroupement par mois et par borne
transactionsPerMoisPerBorne <- transactions %>% group_by(MonthDebut,Borne) %>% summarise(MoisFactorDebut=unique(MoisFactorDebut),
                                                                           Consommationkwh=sum(Consommationkwh),
                                                                           DureeChargemin=sum(DureeChargemin),
                                                                           nbTransaction=n())
#choix de bornes
allBorne <- transactions %>% group_by(Borne) %>% summarise(nbTransaction=n())
allBorne <- allBorne[order(allBorne$nbTransaction, decreasing=TRUE),] # df <- df[order(df$ID),]
listBornes <- levels(allBorne$Borne)
nbornes <- length(listBornes)

#Ajouter l'annee
transactionsPerMois$an <- lubridate::year(transactionsPerMois$MonthDebut)
transactionsPerMois2017 <- filter(transactionsPerMois,an==2017)
transactionsPerMois2018 <- filter(transactionsPerMois,an==2018)
#Ajouter l'annee
transactionsPerMoisPerBorne$an <- lubridate::year(transactionsPerMoisPerBorne$MonthDebut)
transactionsPerMoisPerBorne2017 <- filter(transactionsPerMoisPerBorne,an==2017)
transactionsPerMoisPerBorne2018 <- filter(transactionsPerMoisPerBorne,an==2018)

#######################################################################################################
# CONSOLIDATION SEMAINE sur l'annee 2018
transactions2018$week <- lubridate::week(transactions2018$DateDebut)
# regroupement par semaine et par commune
transactionsPerSemainePerVille2018 <- transactions2018 %>% group_by(week,Ville) %>% 
  summarise(Consommationkwh=sum(Consommationkwh),
            DureeChargemin=sum(DureeChargemin),
            nbTransaction=n())
#les bornes par ville
BornePerVille2018 <- transactions2018 %>% group_by(Ville) %>% 
  summarise(nbBorne=n_distinct(Borne),
            nbTransaction=n())
# regroupement par semaine et par borne
transactionsPerSemainePerBorne2018 <- transactions2018 %>% group_by(week,Borne) %>% 
  summarise(Ville=unique(Ville),
            Consommationkwh=sum(Consommationkwh),
            DureeChargemin=sum(DureeChargemin),
            nbTransaction=n())
# Ajout de la ville a la Borne
transactionsPerSemainePerBorne2018 <- mutate(transactionsPerSemainePerBorne2018,VilleBorne=paste(str_sub(Ville,1,-5),Borne))

######################################################################################################
# CONSOLIDATION MOIS sur l'annee 2018
# regroupement par mois et par Ville
transactionsPerMoisPerVille2018 <- transactions2018 %>% group_by(MonthDebut,Ville) %>% 
  summarise(MoisFactorDebut=unique(MoisFactorDebut),
  Consommationkwh=sum(Consommationkwh),
  DureeChargemin=sum(DureeChargemin),
  nbTransaction=n())

# regroupement par mois et par borne
transactionsPerMoisPerBorne2018 <- transactions2018 %>% group_by(MonthDebut,Borne) %>% 
  summarise(Ville=unique(Ville),
            MoisFactorDebut=unique(MoisFactorDebut),
            Consommationkwh=sum(Consommationkwh),
            DureeChargemin=sum(DureeChargemin),
            nbTransaction=n())
# Ajout de la ville a la Borne => champ VilleBorne
transactionsPerMoisPerBorne2018 <- mutate(transactionsPerMoisPerBorne2018,VilleBorne=paste(str_sub(Ville,1,-5),Borne))

############################
# CALCUL DE LA MOYENNE PAR BORNE PAR MOIS sur l'annee 2018 (mais il n'y a pas toutes les bornes)
moyennePerMois2018 <- transactionsPerMoisPerBorne2018 %>% group_by(MonthDebut) %>% summarise(meanConso=mean(Consommationkwh),
                                                                                           meanCharge=mean(DureeChargemin),
                                                                                           meanTrans=mean(nbTransaction))

# summary(moyennePerMois2018)

#################################################################################################
# Consolidation heure et VISU HEURE
transactionsPerHeure <- transactions %>% group_by(DateTimeDebut,Borne) %>% summarise(Mois=unique(MoisDebut),
                                                                               Consommationkwh=sum(Consommationkwh),
                                                                               DureeChargemin=sum(DureeChargemin),
                                                                               nbTransaction=n())
# Ajouter HHDebut 
transactionsPerHeure$HH <- lubridate::hour(transactionsPerHeure$DateTimeDebut)
transactionsPerHeure$HHFactor <- as.factor(transactionsPerHeure$HH)
# Ajouter l'annee
transactionsPerHeure$an <- lubridate::year(transactionsPerHeure$DateTimeDebut)
transactionsPerHeure2017 <- filter(transactionsPerHeure,an==2017)
transactionsPerHeure2018 <- filter(transactionsPerHeure,an==2018)

# Ajouter le mois
transactionsPerHeure2018$Mois <- lubridate::month(transactionsPerHeure2018$DateTimeDebut)
transactionsPerHeure2018$MoisFactor <- as.factor(transactionsPerHeure2018$Mois)

# Ajouter une variable avec pleine/creuse
transactionsPerHeure2018 <- mutate(transactionsPerHeure2018,saison=as.factor(ifelse((Mois<=3|Mois>=10),"creuse","pleine")))

####################################################################################################
# DONNEES de production et de consommation des Alpes du SUD 2016 a 2018
# VARIABLES 
# Date;consommation;hydraulique;solaire;thermique;Jour;JourMois;Mois;MoisFactor;An;HH;HHFactor;TypeJourFactor;WEFactor;Month;autoConso
# 2016-01-01 00:00:00;430,593972861767;17,3523325771093;-0,0428333301097155;1,3138333261013;6;1;1;1;2016;0;0;6;Semaine;2016-01-01;-0,01
mesData <- read.csv2("prodConso2016-18.csv",encoding = "UTF-8")
# traitement des dates pour consolider a la journee
mesData$Date <- lubridate::ymd_hms(mesData$Date)
mesData$DateJour <- lubridate::date(mesData$Date)

solPerJour <- mesData %>% group_by(DateJour) %>% summarise(solaire=sum(solaire))
consoPerJour <- mesData %>% group_by(DateJour) %>% summarise(consommation=sum(consommation))
hydrauPerJour <- mesData %>% group_by(DateJour) %>% summarise(hydraulique=sum(hydraulique))
thermPerJour <- mesData %>% group_by(DateJour) %>% summarise(thermique=sum(thermique))
autoPerJour <- mesData %>% group_by(DateJour) %>% summarise(autoConso=mean(autoConso))

# VISU
data2018 <- filter(mesData,An==2018)
data2017 <- filter(mesData,An==2017)
data2016 <- filter(mesData,An==2016)
#Ajouter une variable avec pleine/creuse
mesData <- mutate(mesData,saison=as.factor(ifelse((Mois<=3|Mois>=10),"creuse","pleine")))
mesData$HHFactor <- as.factor(mesData$HHFactor)

autoPerJour <- mesData %>% group_by(DateJour) %>% summarise(autoConso=mean(autoConso))
solPerMois <- mesData %>% group_by(Month) %>% summarise(solaire=sum(solaire))
solPerMois <- mutate(solPerMois, GWh = solaire/1000)
consoPerMois <- mesData %>% group_by(Month) %>% summarise(consommation=sum(consommation))
consoPerMois <- mutate(consoPerMois, GWh = consommation/1000)
hydrauPerMois <- mesData %>% group_by(Month) %>% summarise(hydraulique=sum(hydraulique))
hydrauPerMois <- mutate(hydrauPerMois, GWh = hydraulique/1000)
thermPerMois <- mesData %>% group_by(Month) %>% summarise(thermique=sum(thermique))
thermPerMois <- mutate(thermPerMois, GWh = thermique/1000)
#autoconsommation est un %
autoPerMois <- mesData %>% group_by(Month) %>% summarise(autoConso=mean(autoConso))
########################################################################################################################################################
# Croisement des Donnees de production et consommation et/ des transactions Bornes
# periode du 1/01/2017 au 30/09/2018 LIMITE PAR LES DONNEES D ENERGIE (3T 2018)
# allProdTransPerJour.csv
# Date;Consommation;Hydrau;PV;autoConso;Thermique;Consommationkwh;DureeChargemin;nbTransaction;nbPDC
# 2017-01-01;10615,9770050049;16309,2286384683;619,694126838526;0,0583737254278502;32,0048335492611;0;0;0;16
# allProdTransPerMois.csv
# Month;Consommation;Hydrau;PV;autoConso;Thermique;Consommationkwh;DureeChargemin;nbTransaction;nbPDC
# 2017-01-01;336971,432861328;571552,005778764;21438,7883704533;0,0636219758702093;962,365994304419;31,437;145;4;17
prodTransPerJour <- read.csv2("allProdTransPerJour.csv",encoding = "UTF-8")
prodTransPerJour$Date <- lubridate::date(prodTransPerJour$Date)
prodTransPerJour$An <- lubridate::year(prodTransPerJour$Date)
#MOIS
prodTransPerMois <- read.csv2("allProdTransPerMois.csv",encoding = "UTF-8")
#uniformiser pour avoir toujours Date
prodTransPerMois$Date <- lubridate::date(prodTransPerMois$Month)
prodTransPerMois$An <- lubridate::year(prodTransPerMois$Date)
#SEMAINE A PARTIR DE JOUR
prodTrans <- prodTransPerJour
prodTrans$Week <- lubridate::week(prodTrans$Date)
prodTrans$An <- lubridate::year(prodTrans$Date)
prodTransPerSemaine <- prodTrans %>% group_by(Week,An) %>% summarise(Date=min(Date),
                                                                     Consommation=sum(Consommation),
                                                                     Hydrau=sum(Hydrau),
                                                                     PV=sum(PV),
                                                                     autoConso = sum(PV)/sum(Consommation),
                                                                     Thermique=sum(Thermique),
                                                                     Consommationkwh = sum(Consommationkwh),
                                                                     nbTransaction = sum(nbTransaction),
                                                                     DureeChargemin = sum(DureeChargemin))
########################################################################################################################################################
# METEO ET TRANSACTIONS
########################################################################################################################################################
# meteoTransJOUR.csv
# DateJour;temp;tempMin;tempMax;humidite;pointRosee;visu;pluie1;pluie3;Consommationkwh;DureeChargemin;nbTransaction
# 2017-01-01;2,75000000000004;-0,299999999999955;9,40000000000003;39,375;-10,0875;20000;0;0;0;0;0
meteoTransactionPerJour <- read.csv2("meteoTransJOUR.csv",encoding = "UTF-8")
meteoTransactionPerJour$Date <- as.Date(meteoTransactionPerJour$DateJour)
# variable=rapport entre la consommation et le nb de transactions
meteoTransactionPerJour <- mutate(meteoTransactionPerJour, ConsoPerTrans=Consommationkwh/nbTransaction)
# JOUR : ajout des donnees de production
meteoProdTransPerJour <- left_join(select(prodTransPerJour,c=(-An)),select(meteoTransactionPerJour,c(Date,ConsoPerTrans,temp,humidite,pointRosee,visu,pluie1,pluie3)),by=c("Date"))
# MOIS
# meteoTransMOIS.csv
# Month;temp;tempMin;tempMax;humidite;pointRosee;visu;pluie1;pluie3;MoisFactor;Consommationkwh;DureeChargemin;nbTransaction
# 2017-01-01;1,17056451612906;-10,1;12,6;50,125;-9,31008064516127;18910,0403225806;2,4;10,1;1;31,437;145;4
meteoTransactionPerMois <- read.csv2("meteoTransMOIS.csv",encoding = "UTF-8")
meteoTransactionPerMois <- mutate(meteoTransactionPerMois, ConsoPerTrans=Consommationkwh/nbTransaction)
meteoTransactionPerMois$Date <- as.Date(meteoTransactionPerMois$Month)
meteoTransactionsMois <- select(meteoTransactionPerMois,c(temp,humidite,pointRosee,visu,pluie1,pluie3,Consommationkwh,DureeChargemin,nbTransaction,ConsoPerTrans))
# ajout des donnees de production
# MOIS ajout des donnees de production
meteoProdTransPerMois <- left_join(select(prodTransPerMois,c=(-An)),select(meteoTransactionPerMois,c(Date,ConsoPerTrans,temp,humidite,pointRosee,visu,pluie1,pluie3)),by=c("Date"))
meteoProdTrans <- select(meteoProdTransPerMois,c(Consommation,Hydrau,PV,autoConso,Thermique,Consommationkwh,DureeChargemin,nbTransaction,nbPDC,temp,humidite,pointRosee,visu,pluie1,pluie3))
# premieres transactions le 27 janvier
jourfin=nrow(meteoTransactionPerJour)-27
tsmeteoTransactionPerJour <- meteoTransactionPerJour[27:jourfin,] 
# preferable s'il n y a pas l'annee entiere 2018
y <- ts(tsmeteoTransactionPerJour,frequency=365,start=c(2017,27),end=c(2018,365))