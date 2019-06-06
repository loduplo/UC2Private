# UC2Private
Smart Charging prototype privé
Pour fonctionner, ce site shiny doit avoir les fichiers de données suivants:
1. DONNEES CARTOGRAPHIQUES : BORNES - Bornes avec donnees de ruralite

bornesWithRuralite.csv avec les champs suivants : 
Station;Commune;Adresse;Identifiant;Type;Numero;CodeInsee;Connecteur;StationCode;ChargeBoxIdentity;CodePDC;PDL_IDC;lat;lon;Ruralite
TOURTON;CHABOTTES;Parking Haute Plaine SO 05260 CHABOTTES (05);FR*XXX*XXXXX*A*B1*D;ACCELEREE;16322;5029;Droit;FR*XXX*XXXXX*A;FR*XXX*XXXXX*A-1;FR*XXX*XXXXX*A*B1;PDLXXXXXXXXXXX;44,6428413391113;6,16910123825073;400 - Com. isolées hors influence des pôles

2. DONNEES HISTORIQUES : TRANSACTIONS - Les transactions de rechargement

transactions.csv Chaque transaction est décrite avec les champs suivants :
Borne;Type;Ville;DateDebut;HeureDebut;DureeChargemin;Consommationkwh;DateFin;HeureFin;TypeClient;Status;DateTimeDebut;DateTimeFin
FR*XXX*XXXXX*A*B1;Rapide;ANCELLE (05);23/03/2017; 12:28;0;0;23/03/2017; 12:28;Individuel;CHARGE;2017-03-23 12:28:00;2017-03-23 12:30:00
Le champ Borne du fichier de transactions.csv = le champ CodePDC du fichier bornesWithRuralite.csv

3. DONNEES de production et de consommation des Alpes du SUD 2016 a 2018

prodConso2016-18.csv avec les champs suivants :
Date;consommation;hydraulique;solaire;thermique;Jour;JourMois;Mois;MoisFactor;An;HH;HHFactor;TypeJourFactor;WEFactor;Month;autoConso
2016-01-01 00:00:00;430,593972861767;17,3523325771093;-0,0428333301097155;1,3138333261013;6;1;1;1;2016;0;0;6;Semaine;2016-01-01;-0,01

4. Croisement des Donnees de production et consommation et/ des transactions Bornes

allProdTransPerJour.csv vec les champs suivants :
Date;Consommation;Hydrau;PV;autoConso;Thermique;Consommationkwh;DureeChargemin;nbTransaction;nbPDC
2017-01-01;10615,9770050049;16309,2286384683;619,694126838526;0,0583737254278502;32,0048335492611;0;0;0;16

allProdTransPerMois.csv
Month;Consommation;Hydrau;PV;autoConso;Thermique;Consommationkwh;DureeChargemin;nbTransaction;nbPDC
2017-01-01;336971,432861328;571552,005778764;21438,7883704533;0,0636219758702093;962,365994304419;31,437;145;4;17

5. Croisement des données meteo et des transactions des bornes

meteoTransJOUR.csv
DateJour;temp;tempMin;tempMax;humidite;pointRosee;visu;pluie1;pluie3;Consommationkwh;DureeChargemin;nbTransaction
2017-01-01;2,75000000000004;-0,299999999999955;9,40000000000003;39,375;-10,0875;20000;0;0;0;0;0

meteoTransMOIS.csv
Month;temp;tempMin;tempMax;humidite;pointRosee;visu;pluie1;pluie3;MoisFactor;Consommationkwh;DureeChargemin;nbTransaction
2017-01-01;1,17056451612906;-10,1;12,6;50,125;-9,31008064516127;18910,0403225806;2,4;10,1;1;31,437;145;4
