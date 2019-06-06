## UI.R ##
library("shiny")
library("shinydashboard")
library("leaflet")
library("lubridate")
library("dplyr")
library("zoo")
library("ggplot2")
library("forecast")
library("openair") #pour afficher un calendrier qui est adapte a la pollution de l'air
source("global.R")
#palette de couleurs
library("RColorBrewer")
library("psy")
library("corrplot")
library("latticeExtra")

#################################################################################################
# 
# UI : carte interactive - Météo - Type Client - Production / Consommation - Production et Bornes
#      Historique 2018 - Histogrammes
#
#################################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "UC2: Bornes IRVE"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Carte interactive", tabName = "carto", icon = icon("map")),
      selectInput(inputId = "ville",
                  label = paste("Choisissez la ville (",nbVilles,")",""),
                  choice = listVilles),
      selectInput(inputId = "station",
                  label = paste("Choisissez la station (",nbstations,")",""),
                  choice = listStations),
      menuItem("Meteo", tabName = "meteo", icon = icon("cloud")),
      menuItem("Type Client", tabName = "client", icon = icon("user")),
      menuItem("Production / Consommation", tabName = "analyse", icon = icon("dashboard")),
      menuItem("Production et Bornes", tabName = "prodborne", icon = icon("plug")), 
      menuItem("Historique 2018", tabName = "histofacet", icon = icon("th")),
      menuItem("Histogrammes", tabName = "histo", icon = icon("th"))
    )
  ),
  dashboardBody(
    
    tabItems(
      # First tab content : CARTO
      tabItem(tabName = "carto",
              fluidRow(
                box(title="Carte interactive des bornes IRVE", width=12,status="primary",solidHeader=TRUE,
                  leafletOutput("map",height=500),
                  column(4,selectInput(inputId = "legend",
                                       label = h4("choix legende"),
                                       choice = c("TypeCharge","Ruralite","Consommationkwh","DureeChargemin","nbTransaction"))),
                  column(4,checkboxInput("quantile", "Quantile", FALSE)),
                  column(12,uiOutput("locationid"))
                ),
 
                box(title="Historique de la borne par mois, annee 2018",width=12,status="primary",solidHeader=TRUE,
                    fluidRow(
                      column(4,plotOutput("transbornemois")),
                      column(4,plotOutput("chargebornemois")),
                      column(4,plotOutput("consobornemois"))
                      )
                ),
                box(title="Historique de la ville par mois, annee 2018",width=12,status="primary",solidHeader=TRUE,
                    fluidRow(
                      column(4,plotOutput("transvillemois")),
                      column(4,plotOutput("chargevillemois")),
                      column(4,plotOutput("consovillemois"))
                    )
                ),
                box(title="Historique de la borne choisie", width=12,status="primary",solidHeader=TRUE,
                    fluidRow(
                      column(4,selectInput(inputId = "variable",
                                           label = h4("choix variable"),
                                           choice = c("Consommationkwh","DureeChargemin","nbTransaction")))
                      #column(8,uiOutput("locationid"))
                      #textOutput("locationid2")
                    )),
                box(title="Utilisation de la borne par mois",width=4,status="primary",solidHeader=TRUE,
                    plotOutput("histopermois")
                ),
                box(title="Utilisation de la borne par type de jour",width=4,status="primary",solidHeader=TRUE,
                    plotOutput("histoperjour")
                ),
                box(title="Utilisation de la borne par heure",width=4,status="primary",solidHeader=TRUE,
                    plotOutput("histoperheure")
                ),
                box(title="Historique des reservations",status="primary",solidHeader=TRUE,
                    plotOutput("resaggplot")
                )
               
              )
      ),      
      # TAB ITEM METEO
      tabItem(tabName = "meteo",
              fluidRow(
                #choisir la visu meteo
                box(title="Embrun 2017-18 : temperatures et transactions (jours)",width=6,status="primary",solidHeader=TRUE,
                    plotOutput("temptransjour")),
                box(title="Embrun 2017-18 : temperatures et consommation (jours)",width=6,status="primary",solidHeader=TRUE,
                    plotOutput("tempconsojour")),
                box(title="Embrun 2017-18 : temperatures et consommation par transaction (jours)",width=12,status="primary",solidHeader=TRUE,
                    plotOutput("tempconsotransjour")),
                box(title="Embrun 2017-18 : temperatures et transactions (mois)",width=6,status="primary",solidHeader=TRUE,
                    plotOutput("temptransmois")),
                box(title="Embrun 2017-18 : temperatures et consommation (mois)",width=6,status="primary",solidHeader=TRUE,
                    plotOutput("tempconsomois")),
                box(title="Embrun 2017-18 : temperatures et consommation par transactions (mois)",width=12,status="primary",solidHeader=TRUE,
                    plotOutput("tempconsotransmois")),
                
                box(title="Correlation",status="primary",solidHeader=TRUE,
                    plotOutput("corrmeteotrans")),
                box(title="Heatmap",status="primary",solidHeader=TRUE,
                    plotOutput("corrheatmap")),
                box(title="Pairs meteo1",status="primary",solidHeader=TRUE,
                    plotOutput("pairsmeteo1")),
                box(title="Pairs meteo2",status="primary",solidHeader=TRUE,
                    plotOutput("pairsmeteo2")),
                box(title="ACP focalisee : Nombre de transactions",status="primary",solidHeader=TRUE,
                    plotOutput("focaltrans")),
                box(title="ACP focalisee : Consommation",status="primary",solidHeader=TRUE,
                    plotOutput("focalconso"))
              ) #fluidRow
      ), # tabItem meteo #
      # Tab content Histogramme 
      tabItem(tabName = "client",
              fluidRow(
                column(4,selectInput(inputId = "varclient",
                                     label = h4("choix variable"),
                                     choice = c("Consommationkwh","DureeChargemin","nbTransaction","consoParTrans"))
                ),
                column(4,selectInput(inputId = "clientannee",
                                     label = h4("choix annee"),
                                     choice = c("2018","2017-2018","2017"))
                ),
                box(title="Utilisation des bornes par mois/type client",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("facetclientpermois",height = 800)
                ),
                box(title="Utilisation des bornes par mois : une courbe par type de client",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("clientpermoispoint")
                ),
                box(title="RÃ©partition de l'utilisation des bornes par mois par type client : empilÃ©s",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("clientpermoisfill")
                ),
                box(title="Utilisation des bornes par mois par type client : histogrammes empilÃ©s",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("clientpermoisstack")
                ),
                box(title="Utilisation des bornes par mois : histogrammes accolÃ©s par type client : ",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("clientpermoisdodge")
                )
              ) #fluidRow
      ), # tabItem client #
      # tabItem analyse : Production / Consommation
      tabItem(tabName = "analyse",
              fluidRow(
                #Analyse des donnees 
                box(background = "light-blue", width=12,
                    fluidRow(
                      column(4,selectInput(inputId = "production",
                                           label = h4("Choisissez la production"),
                                           choice = c("solaire","consommation","hydraulique", "thermique","autoConso"))
                      ),
                      column(4,selectInput(inputId = "anprod",
                                           label = h4("et l'annee"),
                                           choice = c("2016-2018","2018","2017","2016"))
                      )
                    )
                ),
                box(title="Production mensuelle",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("visuSeasonalprod")
                ),
                box(title="Production consolidee au jour",status="primary",solidHeader=TRUE,
                    plotOutput("prodjour")
                ),
                box(title="Production consolidee au mois",status="primary",solidHeader=TRUE,
                    plotOutput("prodmois")
                ),
                box(title="Boxplot de la Production horaire par mois",status="primary",solidHeader=TRUE,
                    plotOutput("visuBoxMois")
                ),
                box(title="Boxplot de la Production horaire par heure",status="primary",solidHeader=TRUE,
                    plotOutput("visuBoxHeure")
                ),
                box(title="Production horaire par mois",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("visuPlotMois")
                ),
                box(title="Boxplot de la Production horaire par mois",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("visuBoxJour")
                ),
                column(8,textOutput("param"))
              ) # fluidrow #
      ), # tabItem analyse
      # tabItem analyse : Production / Consommation
      tabItem(tabName = "prodborne",
              fluidRow(
                #Analyse des donnees 
                box(background = "light-blue", width=12,
                    fluidRow(
                      column(4,selectInput(inputId = "anprodborne",
                                           label = h4("choisissez l'annee"),
                                           choice = c("2017-2018","2018"))
                      ),
                      column(4,selectInput(inputId = "prodtemps",
                                           label = h4("choisissez la consolidation"),
                                           choice = c("mois","semaine","jour")))
                     )
                ),
                box(title="Production solaire et consommation des bornes",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("prodconsobornes")
                ),
                box(title="Correlation",status="primary",solidHeader=TRUE,
                    plotOutput("corrmeteoprod")),
                box(title="Heatmap",status="primary",solidHeader=TRUE,
                    plotOutput("corrheatmapprod")),
                box(title="ACP focalisee : Consommation des Bornes",status="primary",solidHeader=TRUE,
                    plotOutput("focalbornes")),
                box(title="ACP focalisee : Production hydraulique",status="primary",solidHeader=TRUE,
                    plotOutput("focalhydrau")),
                box(title="ACP focalisee : Production Solaire",status="primary",solidHeader=TRUE,
                    plotOutput("focalpv")),
                box(title="ACP focalisee : Production Thermique",status="primary",solidHeader=TRUE,
                    plotOutput("focalthermique")),
                box(title="Production solaire horaire par mois",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("prodSolaireMois")
                ),
                box(title="Utilisation des bornes par mois",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("bornespermois")
                ),
                box(title="Consommation horaire par mois",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("consommationMois")
                ),
                box(background = "light-blue", width=12,
                    fluidRow(
                      column(4,checkboxInput("saison","Saison creuse(oct-mars)/pleine(avril-sept)",FALSE)),
                      column(4,checkboxInput("boxplot","boxplot",FALSE))
                      )
                ),
                box(background = "light-blue", width=12,
                    fluidRow(
                      plotOutput("solaireHeure",height = 300),
                      plotOutput("bornesperheure",height = 300),
                      plotOutput("consommationHeure",height = 300)
                    )
                )
              ) # fluidrow #
      ), # tabItem analyse Production / consommation 
      # Tab Content Historique 2018 
      tabItem(tabName = "histofacet",
              fluidRow(
                column(4,selectInput(inputId = "varhist18",
                                     label = h4("choix variable"),
                                     choice = c("nbTransaction","Consommationkwh","DureeChargemin"))
                ),
                column(4,selectInput(inputId = "entite",
                                     label = h4("choix Borne / Ville"),
                                     choice = c("Ville","Borne"))
                ),
                column(4,selectInput(inputId = "temps",
                                     label = h4("choix Mois / Semaine"),
                                     choice = c("mois","semaine"))
                ),
                column(12,textOutput("villesbornes")),
                box(title="Historique Annee 2018",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("ggplotfacet",height = 1500)
                ),
                box(title="Historique Annee 2018, echelle y fixe",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("ggplotfacet2",height = 1500)
                )
              )
      ),
      # Tab content HISTORIQUE
      tabItem(tabName = "histo",
              fluidRow(
                column(4,selectInput(inputId = "varhisto",
                                     label = h4("choix variable"),
                                     choice = c("Consommationkwh","DureeChargemin","nbTransaction"))
                       ),
                column(4,selectInput(inputId = "annee",
                                      label = h4("choix annee"),
                                      choice = c("2017-2018","2018","2017"))
                        ),
                column(4,sliderInput(inputId = "nombre",
                                     label = h4("Nombre de bornes"),1,nbornes,30)
                 ),
                box(title="Saisonnalite mensuelle",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("visuseasonal")
                ),
                box(title="Boxplot d'utilisation des bornes par mois",status="primary",solidHeader=TRUE,
                    plotOutput("ggboxpermois")
                ),
                box(title="Utilisation des bornes par mois",status="primary",solidHeader=TRUE,
                    plotOutput("ggpermois")
                ),
                box(title="Boxplot d'utilisation des bornes par type de jour",status="primary",solidHeader=TRUE,
                    plotOutput("ggboxperjour")
                ),
                box(title="Utilisation des bornes par type de jour",status="primary",solidHeader=TRUE,
                    plotOutput("ggperjour")
                ),
                box(title="Boxplot d'utilisation des bornes par heure",status="primary",solidHeader=TRUE,
                    plotOutput("ggboxperheure")
                ),
                box(title="Utilisation des bornes par heure",status="primary",solidHeader=TRUE,
                    plotOutput("ggperheure")
                )
            )
      )
    )
  )
)