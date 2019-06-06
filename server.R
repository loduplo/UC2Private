## server.R ##
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
library("gridExtra")

#################################################################################################
# 
# SERVER
#
#################################################################################################

server <- function(input, output,session) {
  
  # Les bornes
  data <- reactive({
    x <- df
  })
  
  # les transactions
  trans <- reactive({
    t <- transdf
  })
  #les reservations
  # output$resaggplot <- renderPlot ({
  #   #Calendrier
  #   calendarPlot(resa05, pollutant="nbResa",year = 2018,
  #                month=6:12, cols=c("green","blue","orange","red"))
  # })
  #### CLIC sur la carte => recuperation de la Borne Point de charge PDC
  pdc <- reactive({
    validate(
      need(!is.null(input$map_marker_click), "Please select a location from the map above")
    )
    input$map_marker_click$id
  })
  ville <- reactive({
    validate(
      need(!is.null(input$map_marker_click), "Please select a location from the map above")
    )
    trans <- select(filter(df, CodePDC==pdc()),c("Ville"))
    return (trans[1]$Ville)
  })
  output$locationid <- renderUI({
    h4(paste("Vous avez choisi le point de charge :", pdc(), "de la ville", ville()))
    })
  
  
  ############################################################################################################
  ### DASHBOARD tabname=carto                                                                                #
  ### Carte interactive des bornes IRVE                                                                      #
  ############################################################################################################
  ### VISU contextuelles de la cartographie
  colorvar <- eventReactive(
    {
      input$variable
    }, 
    {
      if(input$variable == "Consommationkwh"){
        col <- "#589482"
      } else if(input$variable == "DureeChargemin"){
        col <- "#8C2423"
      } else if(input$variable == "nbTransaction"){
        col <- "#A6B06D"
      }
      return (col)
    })
  unitevar <- eventReactive(
    {
      input$variable
    }, 
    {
      if(input$variable == "Consommationkwh"){
        unite <- "kwh"
      } else if(input$variable == "DureeChargemin"){
        unite <- "minutes"
      } else if(input$variable == "nbTransaction"){
        unite <- "nombre"
      }
      return (unite)
    })
  #Utilisation des bornes par Mois
  output$histopermois <- renderPlot({
    input$variable
    if (!is.null(pdc()))
    {
    datapdc <- filter(transactionsPerMoisPerBorne,Borne==pdc())
    title = paste(pdc()," : ",input$variable," : consolidation mois", "2017-2018","")
    ggplot(datapdc, aes_string("MoisFactorDebut",input$variable,group="MoisFactorDebut")) + 
      geom_col(fill=colorvar())+ labs(title = title, x = "mois", y=unitevar())
    }
  })
  #Utilisation des bornes par Jour
  output$histoperjour <- renderPlot({
    input$variable
    if (!is.null(pdc()))
    {
    datapdc <- filter(transactionsPerJour,Borne==pdc())
    title = paste(pdc()," : ",input$variable," : consolidation jour", "2017-2018","")
    ggplot(datapdc, aes_string("typeJourFactorDebut",input$variable,group="typeJourFactorDebut")) + 
      geom_col(fill=colorvar())+ labs(title = title, x = "Type de jour", y=unitevar())
    }
  })
  #Utilisation des bornes par heure
  output$histoperheure <- renderPlot({
    input$variable
    if (!is.null(pdc()))
    {
    datapdc <- filter(transactionsPerHeure,Borne==pdc())
    title = paste(pdc()," : ",input$variable," : consolidation heure", "2017-2018","")
    ggplot(datapdc, aes_string("HHFactor",input$variable,group="HHFactor")) + 
      geom_col(fill=colorvar()) + labs(title = title, x = "Heure", y=unitevar())
    }
  })
  # Visu d'historique a partir de clic sur la carte
  output$histojour <- renderPlot ({
    #PLOT jour
    if (!is.null(pdc()))
    {
    datapdc <- filter(transactionsPerJour,Borne==pdc())
    
    #paste("La PDC choisie est :", pdc(), "de la station",borne$Station)
    title = paste(pdc()," : ",pdc()," : consolidation jour","2017/2018","")
    xlabtext = paste("jours annee(s)", "2017/2018","")
    plot(datapdc[["DateDebut"]],datapdc[["Consommationkwh"]], type ="h", ylab="kwh", xlab=xlabtext, main=title,col="blue")
    }
  })
  #PLOT mois
  output$histomois <- renderPlot({
    #PLOT mois
    if (!is.null(pdc()))
    {
    datapdc <- filter(transactionsPerMoisPerBorne,Borne==pdc())
    title = paste(pdc()," : ",input$variable," : consolidation mois", input$annee,"")
    xlabtext = paste("mois annee(s)", "2017/2018","")
    plot(datapdc[["MonthDebut"]],datapdc[["Consommationkwh"]], type ="h",ylab="kwh", xlab=xlabtext, main=title,col="blue")
    }
  })
  #HISTORIQUE BORNE : historique annee 2018 avec 3 variables 
  output$transbornemois <- renderPlot({
    #PLOT mois
    if (!is.null(pdc()))
    {
    databorne <- filter(transactionsPerMoisPerBorne2018,Borne==pdc())
    title = paste("Nombre de transactions",pdc())
    p <-ggplot(databorne, aes_string("MonthDebut","nbTransaction")) +
      geom_col(colour="red",fill="orange")+
      labs(title = title, x = "Mois", y="nombre")
    p #limits=c("1","5","10","15","20","25","30","35","40","45","50","60"))
    }
  })
  output$consobornemois <- renderPlot({
    #PLOT mois
    if (!is.null(pdc()))
    {
    databorne <- filter(transactionsPerMoisPerBorne2018,Borne==pdc())
    title = paste("Consommation",pdc())#,nrow(databorne))
    ggplot(databorne, aes_string("MonthDebut","Consommationkwh")) +
      geom_col(colour="blue",fill="turquoise")+
      labs(title = title, x = "Mois", y="kwh")
    }
  })
  output$chargebornemois <- renderPlot({
    #PLOT mois
    if (!is.null(pdc()))
    {
    databorne <- filter(transactionsPerMoisPerBorne2018,Borne==pdc())
    title = paste("Temps d'utilisation",pdc())#,nrow(databorne))
    ggplot(databorne, aes_string("MonthDebut","DureeChargemin")) +
      geom_col(colour="purple",fill="pink")+
      labs(title = title, x = "Mois", y="minutes")
    }
  })
  #HISTORIQUE VILLE : historique annee 2018 avec 3 variables 
  output$transvillemois <- renderPlot({
    #PLOT mois
    if (!is.null(pdc()))
    {
    dataville <- filter(transactionsPerMoisPerVille2018,Ville==ville())
    title = paste("Nombre de transactions",ville())#,nrow(dataville))
    ggplot(dataville, aes_string("MonthDebut","nbTransaction")) +
      geom_col(colour="red",fill="orange")+
      labs(title = title, x = "Mois", y="nombre")
    }
  })
  output$consovillemois <- renderPlot({
    #PLOT mois
    if (!is.null(pdc()))
    {
    dataville <- filter(transactionsPerMoisPerVille2018,Ville==ville())
    title = paste("Consommation :",ville())#,nrow(dataville))
    ggplot(dataville, aes_string("MonthDebut","Consommationkwh")) +
      geom_col(colour="blue",fill="turquoise")+
      labs(title = title, x = "Mois", y="kwh")
    }
  })
  output$chargevillemois <- renderPlot({
    #PLOT mois
    if (!is.null(pdc()))
    {
    dataville <- filter(transactionsPerMoisPerVille2018,Ville==ville())
    title = paste("Temps d'utilisation",ville())#,nrow(dataville))
    ggplot(dataville, aes_string("MonthDebut","DureeChargemin")) +
      geom_col(colour="purple",fill="pink")+
      labs(title = title, x = "Mois", y="minutes")
    }
  })
############################################################################################################
# Ajout de couleur au cercle :
#   rouge pour RAPIDE
#   bleu pour ACCELERE
# Ajout de couleur sur la borne en fonction de l'ecart la moyenne : mois d'octobre
# par quantile : 1er quantile vert, 2eme bleu, 3eme turquoise, 4eme jaune
 output$map <- renderLeaflet({
      df <- data()
      
      # Ajout de couleur au cercle :
      #   rouge pour RAPIDE
      #   bleu pour ACCELERE
      pal = leaflet::colorFactor(palette=c("blue","red"), domain=df$Type)
      
      m <- leaflet(data = df) %>%
        addTiles() %>%
        addLegend(position="topleft",pal=pal, values= ~Type,title = "Type de charge",opacity=2) %>%
        addCircleMarkers(layerId= ~CodePDC,
                         lng = ~lon,lat = ~lat,
                         color = ~ pal(Type),
                         label = paste(df$Commune,":",df$Station),
                         labelOptions = labelOptions(noHide=F,direction='top', textsize='15px'),
                         #clusterOptions = markerClusterOptions(),
                         # Station;Commune;Adresse;Identifiant;Type;StationCode;Connecteur;ChargeBoxIdentity;CodePDC;PDL_IDC;
                         # INT Numero;CodeInsee
                         popup = ~paste("<b>","Commune :","</b>",as.character(df$Commune),"</br>",
                                        "<b>","Ruralite :","</b>",as.character(df$Ruralite),"</br>",
                                        "<b>","Nom de la Station :","</b>",as.character(df$Station),"</br>",
                                        "<b>","Date Consuel :","</b>",as.character(df$DateConsuel),"</br>",
                                        "<b>","Code PDC :","</b>",as.character(df$CodePDC),"</br>",
                                        "<b>","ID station :","</b>",as.character(df$Identifiant),"</br>","</br>",
                                        "<b>","Code Station :","</b>",as.character(df$StationCode),"</br>",
                                        "<b>","Connecteur :","</b>",as.character(df$Connecteur),"</br>",
                                        "<b>","Type :","</b>",as.character(df$Type),"</br>",
                                        "<b>","Adresse de la Station :","</b>",as.character(df$Adresse),"</br>","</br>",
                                        "<b>","ChargeBoxIdentity :","</b>",as.character(df$ChargeBoxIdentity),"</br>",
                                        "<b>","PDL ou IDC :","</b>",as.character(df$PDL_IDC),"</br>","</br>",
                                        "<b>","Numero :","</b>",as.character(df$Numero),"</br>",
                                        "<b>","Code INSEE :","</b>",as.character(df$CodeInsee),"</br>")
        )
      m
    })
  # Use a separate observer to recreate the legend as needed.
  observe({
    input$legend
    input$quantile
    proxy <- leafletProxy("map", data = df)
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    proxy %>% clearMarkers() %>% clearControls() %>% clearShapes() 
    if (input$legend=="TypeCharge") {
      pal = leaflet::colorFactor(palette=c("blue","red"), domain=df$Type)
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~Type,title = "Type de borne",opacity=2)
      colorpal=~pal(Type)
      val=~Type
      cercle=10
    }
    else if (input$legend=="Ruralite")
    {
      pal = leaflet::colorFactor(palette=c("orange","purple","blue","green","light green","dark grey"), domain=df$Ruralite)
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~Ruralite,title = "Ruralite",opacity=2)
      colorpal=~pal(Ruralite)
      val=~Ruralite
      cercle=as.numeric(df$Type)*10
    }
    else if (input$legend=="Consommationkwh")
    {
      if(input$quantile == TRUE)
      {
        pal <- leaflet::colorQuantile(
          palette = "Blues", #Blues, Greens, Reds, Oranges, RdYlBu
          na.color = "#808080",
          domain = df$EcartConso)
      }
      else {
        pal <- leaflet::colorNumeric(
          palette = "Blues", #Blues, Greens, Reds, Oranges, RdYlBu
          na.color = "#808080",
          domain = df$EcartConso)
      }
      proxy %>% addLegend(position = "topleft",
                         pal = pal, values = ~EcartConso,title = "Ecart annuel : Consommation en kwh",opacity=2)
      colorpal=~pal(EcartConso)
      val=~EcartConso
      cercle=~sqrt(Consommationkwh)
    }
    else if (input$legend=="nbTransaction")
    {
      if(input$quantile == TRUE)
      {
        pal <- leaflet::colorQuantile(
          palette = "Oranges", 
          domain = df$EcartTrans)
      }
      else
      {
        pal <- leaflet::colorNumeric(
          palette = "Oranges", 
          domain = df$EcartTrans)
      }
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~EcartTrans,title = "Ecart annuel : nombre de transactions",opacity=2)
      colorpal=~pal(EcartTrans)
      val=~EcartTrans
      cercle=~sqrt(nbTransaction)*2
    }
    else if (input$legend=="DureeChargemin")
    {
      if(input$quantile == TRUE)
      {
        pal <- leaflet::colorQuantile(
          palette = "RdYlBu", #Blues, Greens, Reds, Oranges, RdYlBu
          na.color = "#808080",
          domain = df$EcartDuree)
      }
      else
      { 
        pal <- leaflet::colorNumeric(
          palette = "RdYlBu", #Blues, Greens, Reds, Oranges, RdYlBu
          na.color = "#808080",
          domain = df$EcartDuree)
      }
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~EcartDuree,title = "Ecart annuel :Duree en minutes",opacity=2)
      colorpal=~pal(EcartDuree)
      val=~EcartDuree
      cercle=~sqrt(DureeChargemin)/4
    }
    # Remove any existing circle
    proxy %>% addCircleMarkers(layerId= ~CodePDC,
                       lng = ~lon,lat = ~lat, fillOpacity=0.5,
                       radius = cercle,
                       color = colorpal, #Type
                       label = paste(df$Commune,":",df$Station), #Station
                       popup = ~paste("<b>","Commune :","</b>",as.character(df$Commune),"</br>",
                                      "<b>","Ruralite :","</b>",as.character(df$Ruralite),"</br>",
                                      "<b>","Nom de la Station :","</b>",as.character(df$Station),"</br>",
                                      "<b>","Date Consuel :","</b>",as.character(df$DateConsuel),"</br>",
                                      "<b>","Code PDC :","</b>",as.character(df$CodePDC),"</br>",
                                      "<b>","ID station :","</b>",as.character(df$Identifiant),"</br>","</br>",
                                      "<b>","Code Station :","</b>",as.character(df$StationCode),"</br>",
                                      "<b>","Connecteur :","</b>",as.character(df$Connecteur),"</br>",
                                      "<b>","Type :","</b>",as.character(df$Type),"</br>",
                                      "<b>","Adresse de la Station :","</b>",as.character(df$Adresse),"</br>","</br>",
                                      "<b>","ChargeBoxIdentity :","</b>",as.character(df$ChargeBoxIdentity),"</br>",
                                      "<b>","PDL ou IDC :","</b>",as.character(df$PDL_IDC),"</br>","</br>",
                                      "<b>","Numero :","</b>",as.character(df$Numero),"</br>",
                                      "<b>","Code INSEE :","</b>",as.character(df$CodeInsee),"</br>","</br>",
                                      "<b>","2018: ","</br>",
                                      "<b>","       Nombre de Transactions : ","</b>",as.character(df$nbTransaction),"</br>",
                                      "<b>","Ecart Nb Transactions : ","</b>",as.character(df$EcartTrans),"</br>",
                                      "<b>","Consommation : ","</b>",as.character(df$Consommationkwh),"kwh","</br>",
                                      "<b>","Ecart Consommation : ","</b>",as.character(df$EcartConso),"</br>",
                                      "<b>","Duree de charge : ","</b>",as.character(df$DureeChargemin),"minutes","</br>",
                                      "<b>","Ecart Duree de charge : ","</b>",as.character(df$EcartDuree),"</br>")
                        )
  })

  # A REUTILISER rectangle vert autour d'une ville
  #add a rectangle to display the selected City
  observe({
    input$ville
    laVille<-filter(df,Ville==input$ville)
    laVille <- laVille[1,]

    proxy <- leafletProxy("map", data = df)
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    proxy %>% clearShapes() %>%
      addRectangles(
        lng1=laVille$lon-0.03, lat1=laVille$lat-0.03,
        lng2=laVille$lon+0.03, lat2=laVille$lat+0.03,
        color="green",
        fill = FALSE
        #highlightOptions = highlightOptions(color = "grey", weight = 2,
        #                                                 bringToFront = TRUE)

      )
  })
  #add a rectangle to display the selected station
  observe({
    input$station
    laStation<-filter(df,Station==input$station)
    laStation <- laStation[1,]
    
    proxy <- leafletProxy("map", data = df)
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    proxy %>% clearShapes() %>%  
      addRectangles(
        lng1=laStation$lon-0.02, lat1=laStation$lat-0.02,
        lng2=laStation$lon+0.02, lat2=laStation$lat+0.02,
        color="green",
        fill = FALSE,
        highlightOptions = highlightOptions(color = "grey", weight = 2,
        bringToFront = TRUE)
      )
  })

  ############################################################################################################
  ### DASHBOARD tabname=meteo                                                                                #                                                                    #
  ############################################################################################################
  ### temperature et transactions : Embrun 2018 par jour
  output$temptransjour <- renderPlot({
    visu <- ggplot(meteoTransactionPerJour,aes(Date)) +
      geom_ribbon(aes(ymin=tempMin,ymax=tempMax),fill="lightblue",show.legend =TRUE) +
      geom_line(aes(y=temp,colour="Temperature Moyenne"))+
      geom_line(aes(y=nbTransaction,colour="Nombre Transactions"))+
      scale_colour_manual("", 
                          breaks = c("Temperature Moyenne", "Nombre Transactions", "TempMin"),
                          values = c("red", "blue","lightblue")) +
      xlab(" ") +
      scale_y_continuous("Temperature en Â°C", limits = c(-10,35)) + 
      scale_x_date(date_breaks="2 month",date_labels = "%m-%y") +
      theme(axis.text=element_text(size=10,face="bold"),
            axis.title=element_text(size=14,face="bold"),
            legend.text=element_text(size=12),
            legend.justification=c(1,1),legend.position="top"
      )
      #scale_x_date(date_breaks="2 month",date_labels = "%b %Y",limits= c(Sys.Date()-365,Sys.Date()-100))+
    return(visu)
  })
  #temperature et transactions : Embrun 2017-2018 par mois
  output$temptransmois <- renderPlot({
  ## construct separate plots for each series
  obj1 <- xyplot(temp ~ Date,meteoTransactionPerMois, type = c("p","l"), lwd = 2,ylab="Temperature en Â°C")
  obj2 <- xyplot(nbTransaction ~ Date, meteoTransactionPerMois, type = c("p","l"), lwd = 2,ylab="Nombre de transactions")
  visu <- update(doubleYScale(obj1, obj2, add.ylab2=TRUE, text = c("Temperature", "Nombre de transactions")),
         xlab="Mois",
         par.settings = simpleTheme(col = c('blue','red'), lty = 1:2))
  return(visu)
  })
  #temperature et consommation : Embrun 2017-2018 par jour
  output$tempconsojour <- renderPlot({
    ## construct separate plots for each series
    obj1 <- xyplot(temp ~ Date,meteoTransactionPerJour, type = c("p","l"), lwd = 2,ylab="Temperature en ÃÂ°C")
    obj2 <- xyplot(Consommationkwh ~ Date, meteoTransactionPerJour, type = c("p","l"), lwd = 2,ylab="Consommation en kwh")
    visu <- update(doubleYScale(obj1, obj2, add.ylab2=TRUE, text = c("Temperature", "Consommation en kwh")),
                   xlab="Jour",
                   par.settings = simpleTheme(col = c('blue','red'), lty = 1:2))
    return(visu)
  })
  #temperature et consommation/nb transaction : Embrun 2017-2018 par jour
  output$tempconsotransjour <- renderPlot({
    ## construct separate plots for each series
    obj1 <- xyplot(temp ~ Date,meteoTransactionPerJour, type = c("p","l"), lwd = 2,ylab="Temperature en Â°C")
    obj2 <- xyplot(ConsoPerTrans ~ Date, meteoTransactionPerJour, type = c("p","l"), lwd = 2,ylab="Consommation par transaction en kwh")
    visu <- update(doubleYScale(obj1, obj2, add.ylab2=TRUE, text = c("Temperature", "Consommation par transaction en kwh")),
                   xlab="Mois",
                   par.settings = simpleTheme(col = c('blue','red'), lty = 1:2))
    return(visu)
  })
  #temperature et consommation : Embrun 2017-2018 par mois
  output$tempconsomois <- renderPlot({
    ## construct separate plots for each series
    obj1 <- xyplot(temp ~ Date,meteoTransactionPerMois, type = c("p","l"), lwd = 2,ylab="Temperature en Â°C")
    obj2 <- xyplot(Consommationkwh ~ Date, meteoTransactionPerMois, type = c("p","l"), lwd = 2,ylab="Consommation en kwh")
    visu <- update(doubleYScale(obj1, obj2, add.ylab2=TRUE, text = c("Temperature", "Consommation en kwh")),
                   xlab="Mois",
                   par.settings = simpleTheme(col = c('blue','red'), lty = 1:2))
    return(visu)
  })
  #temperature et consommation/nb transaction : Embrun 2017-2018 par mois
  output$tempconsotransmois <- renderPlot({
    ## construct separate plots for each series
    obj1 <- xyplot(temp ~ Date,meteoTransactionPerMois, type = c("p","l"), lwd = 2,ylab="Temperature en ÃÂ°C")
    obj2 <- xyplot(ConsoPerTrans ~ Date, meteoTransactionPerMois, type = c("p","l"), lwd = 2,ylab="Consommation par transaction en kwh")
    visu <- update(doubleYScale(obj1, obj2, add.ylab2=TRUE, text = c("Temperature", "Consommation par transaction en kwh")),
                   xlab="Mois",
                   par.settings = simpleTheme(col = c('blue','red'), lty = 1:2))
    return(visu)
  })
  output$corrmeteotrans <- renderPlot({
    visu <- corrplot(cor(meteoTransactionsMois[,1:10],use="pairwise.complete.obs"),method="circle")
  })
  output$corrheatmap <- renderPlot({
    heatmap(abs(cor(meteoTransactionsMois[,1:10])),symm=T)
  })
  output$pairsmeteo1 <- renderPlot({
    pairs(ConsoPerTrans~temp+tempMin+tempMax+pointRosee,y)
  })
  output$pairsmeteo2 <- renderPlot({
    pairs(ConsoPerTrans~visu+humidite+pluie1+pluie3,y)
  })
  output$focaltrans <- renderPlot({
    # ACP focalisee
    # expliquer : le nombre de transactions / avec les variables explicatives
    expliquer <- "Consommationkwh"
    explicatives <- c("temp","humidite","pointRosee","visu","pluie3","nbTransaction","DureeChargemin")
    visu <- fpca(y=expliquer,x=explicatives,data=meteoTransactionsMois, partial="Yes")
      return(visu)
  })
  output$focalconso <- renderPlot({
    # ACP focalisee
    # expliquer : le nombre de transactions / avec les variables explicatives
    expliquer <- "ConsoPerTrans"
    explicatives <- c("temp","humidite","pointRosee","visu","pluie3","Consommationkwh","DureeChargemin", "nbTransaction")
    visu <- fpca(y=expliquer,x=explicatives,data=meteoTransactionsMois, partial="Yes")
    return(visu)
  })
  ############################################################################################################
  ### DASHBOARD tabname=client                                                                               #                                                                    #
  ############################################################################################################
  axeyclient <- eventReactive(
  {
      input$varclient
  },
  {
      if(input$varclient == "Consommationkwh"){
        unite <- "Consommation en kwh"
  } else if(input$varclient == "DureeChargemin"){
        unite <- "Duree de charge en minutes"
  } else if(input$varclient == "nbTransaction"){
        unite <- "nombre de transactions"
  } else if(input$varclient == "consoParTrans"){
        unite <- "consommation par transaction en kwh"
  }
      return (unite)
  })
  echelleclient <- eventReactive(
    {
      input$varclient
    },
    {
      if(input$varclient == "Consommationkwh"){
        echelle <- c(0,200,400,600,800,1000,1200,1400,1600,1800,2000)
      } else if(input$varclient == "DureeChargemin"){
        echelle <- c(0,5000,10000,15000,20000,25000,30000)
      } else if(input$varclient == "nbTransaction"){
        echelle <- c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150) 
      } else if(input$varclient == "consoParTrans"){
        echelle <- c(0,5,10,15,20,25,30,35)
      }
      return (echelle)
    })
  dataclient <- eventReactive(
  {
      input$clientannee
  }, 
  {
  if(input$clientannee == 2017){
      data <- transactionsAvecClientPerMois2017
  } else if(input$clientannee == 2018){
      data <- transactionsAvecClientPerMois2018
  } else {
      data <- transactionsAvecClientPerMois
  }
      return (data)
  })

  #Visu en 4 lignes : 1 ligne par type de client
  output$facetclientpermois <- renderPlot({
    input$varclient
    
    first<- ggplot(dataclient(), aes_string(x="MonthDebut",y=input$varclient))
    visu <- first + geom_col(aes(color=TypeClient,fill=TypeClient),position="dodge") + 
      scale_x_date(date_labels="%b%y",date_breaks = "1 month") + 
      scale_y_continuous(breaks=echelleclient())+ 
      labs(x = "mois", y=axeyclient(), title=paste(axeyclient(),"par type client")) +
      facet_wrap(~ TypeClient,nrow=4) 
    return(visu)
  })

  #Visu des points : 1 couleur et une forme par type de client
  output$clientpermoispoint <- renderPlot({
    input$varclient
    #geom_col(aes(color=TypeClient,fill=TypeClient),position="fill")
    visu <- ggplot(dataclient(), aes_string(x="MonthDebut",y=input$varclient)) +
      geom_line(aes(color=TypeClient),size=1)+
      geom_point(aes(color=TypeClient,shape=TypeClient,cex=1))+
      scale_x_date(date_labels="%b %y",date_breaks = "1 month") +
      labs(x = "mois", y=axeyclient(),title=paste(axeyclient(),"par type client"))
    #all <- grid.arrange(first,fill,stack,ncol=1,nrow=3)
    return (visu)
  })
  output$clientpermoisfill <- renderPlot({
    input$varclient

    #position=fill 
    visu <- ggplot(dataclient(), aes_string(x="MonthDebut",y=input$varclient)) +
      geom_col(aes(color=TypeClient,fill=TypeClient),position="fill")+
      #geom_point(aes(color=TypeClient,shape=TypeClient))+clientpermoisfill
      scale_x_date(date_labels="%m-%y",date_breaks = "1 month") +
      labs(x = "mois", y="pourcentage %",title=paste(axeyclient(),"% par type client"))

    return (visu)
  })
  output$clientpermoisstack <- renderPlot({
    input$varclient
 
    #position=stack
    visu <- ggplot(dataclient(), aes_string(x="MonthDebut",y=input$varclient)) +
      geom_col(aes(color=TypeClient,fill=TypeClient),position="stack")+
      scale_x_date(date_labels="%m-%y",date_breaks = "1 month") +
      labs(x = "mois", y=axeyclient(),title=paste(axeyclient(),"par type client"))
    
    return (visu)
  })
  output$clientpermoisdodge <- renderPlot({
    input$varclient
    
    #position=dodge
    visu <- ggplot(dataclient(), aes_string(x="MonthDebut",y=input$varclient)) +
      geom_col(aes(color=TypeClient,fill=TypeClient),position="dodge")+
      scale_x_date(date_labels="%m-%y",date_breaks = "1 month") +
      labs(x = "mois", y=axeyclient(),title=paste(axeyclient(),"par type client"))
    
    return (visu)
  })
  ############################################################################################################
  ### DASHBOARD tabname=analyse                                                                              #
  ### ANALYSE PRODUCTION ET CONSOMMATION                                                                     #
  ############################################################################################################
  #renderText
  output$param <- renderText({
    input$production
    paste("Vous avez choisi la production", input$production, "\n"," pour l'annee ", input$anprod)
  })
  
  dataAn <- eventReactive(
    {
      input$anprod
    }, 
    {
      if (input$anprod == 2017){
        data <- data2017
      } else if (input$anprod == 2018) {
        data <- data2018
      } else if (input$anprod == 2016) {
        data <- data2016
      } else 
      {
        data <- mesData
      }
      return (data)
    })
  
  prodjdf <- eventReactive(
    {
      input$production
    }, 
    {
      if(input$production == "solaire"){
        return (solPerJour)
      } else if(input$production == "consommation"){
        return (consoPerJour)
      } else if(input$production == "hydraulique"){
        return (hydrauPerJour)
      } else if(input$production == "autoConso"){
        return (autoPerJour)
      } else if(input$production == "thermique"){
        return (thermPerJour)
      }
      return (prod)
    })
  prodmdf <- eventReactive(
    {
      input$production
    }, 
    {
      if(input$production == "solaire"){
        prod <- solPerMois
      } else if(input$production == "consommation"){
        prod <- consoPerMois
      } else if(input$production == "hydraulique"){
        prod <- hydrauPerMois
      } else if(input$production == "autoConso"){
        prod <- autoPerMois
      } else if(input$production == "thermique"){
        prod <- thermPerMois
      }
      return (prod)
    })
  colProd <- eventReactive(
    {
      input$production
    }, 
    {
      if(input$production == "solaire"){
        col <- "red"
      } else if(input$production == "consommation"){
        col <- "blue"
      } else if(input$production == "hydraulique"){
        col <- "turquoise"
      } else if(input$production == "thermique"){
        col <- "yellow"
      } else if(input$production == "autoConso"){
        col <- "green"
      }
      return (col)
    })
  #production choisie
  output$prodjour <- renderPlot({
    input$production
    if (input$production == "autoConso")
    {
      axey <- "% d'auto consommation"
    }
    else
    {
      axey <- paste(input$production," en MWh")
    }
    title <- paste(input$production,"consolidation par jour",sep=", ")
    plot(prodjdf(), type ="l", ylab=axey, xlab="jours du 01/2016 au 09/2018", main=title,col=colProd())
  })
  
  output$prodmois <- renderPlot({
    input$production
    
    if (input$production == "autoConso")
    {
      axey <- "% d'auto consommation"
      plot(prodmdf()$Month,prodmdf()$autoConso, type ="p", ylab=axey, xlab="mois du 01/2016 au 09/2018", main="consolidation mois")
      lines(prodmdf()$Month,prodmdf()$autoConso, type ="h", ylab=axey, xlab="mois du 01/2016 au 09/2018", main="consolidation mois")
    }
    else 
    {
      axey <- paste(input$production," en GWh")
      plot(prodmdf()$Month,prodmdf()$GWh, type ="p", ylab=axey, xlab="mois du 01/2016 au 09/2018", main="consolidation mois")
      lines(prodmdf()$Month,prodmdf()$GWh, type ="h", ylab=axey, xlab="mois du 01/2016 au 09/2018", main="consolidation mois")
    }
  })
  #visu plot de production horaire par heure et par mois pour les annees 2016,2017 et 2018
  output$visuPlotMois <- renderPlot({
    input$production
    input$anprod
    
    if (input$production == "autoConso")
    {
      unite <- "pourcentage %"
    }
    else
    {
      unite <- "MWh"
    }
    
    title <- paste("Production", input$production,"horaire par mois annee", input$anprod)
    visu <- ggplot(dataAn(), aes_string("HH",input$production)) +
      geom_point(colour=colProd())+labs(x = "heure", y=unite)+ facet_wrap(~ Month) +
      labs(title = title, x = "Heures", y=unite)
    return (visu)
  })
  
  #Production horaire par Mois pour l'annee 2016,2017 ou 2018 : boxplot
  output$visuBoxJour <- renderPlot({ 
    input$production
    input$anprod
    
    if (input$production == "autoConso")
    {
      unite <- "pourcentage %"
    }
    else
    {
      unite <- "MWh"
    }
    title <- paste("Production", input$production,"horaire par mois annee", input$anprod)
    visu <- ggplot(dataAn(), aes_string("HHFactor",input$production,group="HHFactor")) + geom_boxplot(fill = colProd())+
      labs(title = title, x = "Heures", y=unite) + facet_wrap(~ Month) +  
      stat_summary(fun.y=mean, geom="point", shape=23, size=1,color="black")
    return (visu)
  })
  output$visuSeasonalprod <- renderPlot({
    input$production
    
    #create a ts object
    if (input$production == "autoConso")
    {
      myts <- ts(prodmdf()$autoConso, start=c(2016, 1), end=c(2018, 9), frequency=12)
      axey <- paste(input$production,"en %")
    } else {
      myts <- ts(prodmdf()$GWh, start=c(2016, 1), end=c(2018, 9), frequency=12)
      axey <- paste(input$production,"en GWh")
    }
    visu<-seasonplot(myts,year.labels =TRUE,col=1:6, year.labels.left=TRUE, ylab= axey,xlab="",main="")
    return (visu)
  })
  #boxplot de la production horaire par mois
  output$visuBoxMois <- renderPlot({
    input$production
    input$anprod
    if (input$production == "autoConso")
    {
      unite <- "pourcentage %"
    }
    else
    {
      unite <- "MWh"
    }
    title <- paste(input$production,"horaire par mois annee", input$anprod)
    
    visu<-ggplot(dataAn(), aes_string("MoisFactor",input$production,group="MoisFactor")) + 
      geom_boxplot(fill = colProd())+
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
      labs(title = title, x = "Mois", y=unite)+
      stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
    return (visu)
  })
  # Boxplot par heure
  output$visuBoxHeure <- renderPlot({
    input$production
    input$anprod
    if (input$production == "autoConso")
    {
      unite <- "pourcentage %"
    }
    else
    {
      unite <- "MWh"
    }
    title <- paste(input$production,"horaire par mois annee", input$anprod)
    
    visu<-ggplot(dataAn(), aes_string("HHFactor",input$production,group="HHFactor")) + 
        geom_boxplot(fill = colProd())+
        #scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)) +
        labs(title = title, x = "Heure", y=unite)+
        stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
    return (visu)
  })
  ############################################################################################################
  ### DASHBOARD tabname=prodborne                                                                            #
  ### ANALYSE PRODUCTION ET CONSOMMATION DES BORNES                                                          #
  ############################################################################################################
  dataprod <- eventReactive(
    {
      input$prodtemps
      input$anprodborne
    }, 
    {
      if (input$prodtemps == "mois"){
        data <- prodTransPerMois
      } else if (input$prodtemps == "semaine"){
        data <- prodTransPerSemaine
      } else if (input$prodtemps == "jour"){
        data <- prodTransPerJour
      } 
      if (input$anprodborne == 2018)
      {
        data <- filter(data,An=="2018")
      }
      return (data)
    })
  
  # Comparaison par jour, semaine ou mois
  
  output$prodconsobornes <- renderPlot({
    input$anprodborne
    input$prodtemps
    
    titlepv <- paste("Production solaire par",input$prodtemps,"en MWh")
    pv <- ggplot(dataprod(),aes(Date,PV)) +
      geom_col(aes(y=PV),colour="red",fill="red")+
      scale_x_date(date_labels="%b %y",date_breaks = "1 month") +
      labs(x = "Date", y="MWh",title=titlepv)
    titleconso <- paste("Evolution de la consommation par ",input$prodtemps,"en KWh")
    consoBornes <- ggplot(dataprod(),aes(Date,Consommationkwh)) +
      geom_col(aes(y=Consommationkwh),colour="green",fill="green")+
      scale_x_date(date_labels="%b %y",date_breaks = "1 month") +
      labs(x = "Date", y="KWh",title=titleconso)
    visu <- grid.arrange(pv,consoBornes,ncol=1,nrow=2)
    return (visu)
  })
  #Correlation sur meteoProdTransMois
  output$corrmeteoprod <- renderPlot({
    visu <- corrplot(cor(meteoProdTrans,use="pairwise.complete.obs"),method="circle")
  })
  output$corrheatmapprod <- renderPlot({
    heatmap(abs(cor(meteoProdTrans)),symm=T)
  })
  #BUT 4.3 : Trouver une corrÃÂ©lation entre production hydro & tempÃÂ©rature & humiditÃÂ©.
  output$focalbornes <- renderPlot({
    # ACP focalisee
    #Expliquer la production hydro avec meteo + autres productions
    expliquer <- "Consommationkwh"
    explicatives <- c("temp","humidite","pointRosee","visu","pluie3","nbTransaction","DureeChargemin","Hydrau","PV","Thermique","nbPDC","autoConso","Consommation")
    visu <- fpca(y=expliquer,x=explicatives,data=meteoProdTransPerMois, partial="Yes")
    return(visu)
  })
  #BUT 4.3 : Trouver une correlation entre production hydro & tempÃÂ©rature & humiditÃÂ©.
  output$focalhydrau <- renderPlot({
    # ACP focalisee
    #Expliquer la production hydro avec meteo + autres productions
    expliquer <- "Hydrau"
    explicatives <- c("temp","humidite","pointRosee","visu","pluie3","nbTransaction","DureeChargemin","Consommationkwh","PV","Thermique","nbPDC","autoConso","Consommation")
    visu <- fpca(y=expliquer,x=explicatives,data=meteoProdTransPerMois, partial="Yes")
    return(visu)
  })
  #BUT 4.4 : Trouver une correlation entre production solaire/hydro et les tempÃÂ©ratures.
  output$focalpv <- renderPlot({
    # ACP focalisee
    #Expliquer la production PV avec meteo + autres productions
    expliquer <- "PV"
    explicatives <- c("temp","humidite","pointRosee","visu","pluie3","nbTransaction","DureeChargemin","Consommationkwh","Hydrau","Thermique","nbPDC","autoConso","Consommation")
    visu <- fpca(y=expliquer,x=explicatives,data=meteoProdTransPerMois, partial="Yes")
    return(visu)
  })
  #BUT 4.5 : Trouver une correlation entre production thermique et tempÃÂ©rature et utilisation des bornes.
  output$focalthermique <- renderPlot({
    # ACP focalisee
    #Expliquer la production Thermique avec meteo + autres productions
    expliquer <- "Thermique"
    explicatives <- c("temp","humidite","pointRosee","visu","pluie3","nbTransaction","DureeChargemin","Consommationkwh","PV","Hydrau","nbPDC","autoConso","Consommation")
    visu <- fpca(y=expliquer,x=explicatives,data=meteoProdTransPerMois, partial="Yes")
    return(visu)
  })
  #boxplot de la production solaire horaire par mois
  output$prodSolaireMois <- renderPlot({
    
    plot1 <- ggplot(data2018, aes_string("MoisFactor","solaire",group="MoisFactor")) + 
      geom_col(fill="red")+ labs(title = "Production solaire par mois", x = "mois", y="MWh")+
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))
    box <-ggplot(mesData, aes_string("MoisFactor","solaire",group="MoisFactor")) + 
      geom_boxplot(fill = "red")+
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
      labs(title = "Production solaire horaire", x = "Mois", y="MWh")+
      stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
    visu <- grid.arrange(plot1,box,ncol=2,nrow=1)
    return (visu)
  })
  #boxplot de la consommation horaire par mois
  output$consommationMois <- renderPlot({
    
    plot1 <- ggplot(data2018, aes_string("MoisFactor","consommation",group="MoisFactor")) + 
      geom_col(fill="blue")+ labs(title = "Consommation par mois", x = "mois", y="MWh")+
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))
    box <-ggplot(mesData, aes_string("MoisFactor","consommation",group="MoisFactor")) + 
      geom_boxplot(fill = "blue")+
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
      labs(title = "Consommation horaire", x = "Mois", y="MWh")+
      stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
    visu <- grid.arrange(plot1,box,ncol=2,nrow=1)
    return (visu)
  })
  # Production solaire par heure
  # si saison=TRUE => separation en 2 graphiques
  output$solaireHeure <- renderPlot({
    input$saison
    input$boxplot
    
    if (input$saison==TRUE)
    {
      if (input$boxplot == FALSE)
      {
        visu <- ggplot(mesData, aes_string("HHFactor","solaire",group="HHFactor")) + 
          geom_col(fill="red") + labs(title = "Production solaire par heure", x = "Heure", y="MWh")+ 
          facet_wrap( ~saison,ncol=2) 
      }
      else
      {
        #Box Plot
        visu <-ggplot(mesData, aes_string("HHFactor","solaire",group="HHFactor")) + 
          geom_boxplot(fill = "red")+
          facet_wrap( ~saison,ncol=2)+
          labs(title = "production solaire horaire", x = "Heure", y="MWh")+
          stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
      }
    }
    else
    {
      plot1 <- ggplot(mesData, aes_string("HHFactor","solaire",group="HHFactor")) + 
        geom_col(fill="red") + labs(title = "Production solaire par heure", x = "Heure", y="MWh")
      box1 <-ggplot(mesData, aes_string("HHFactor","solaire",group="HHFactor")) + 
        geom_boxplot(fill = "red")+
        labs(title = "Production solaire horaire", x = "Heure", y="MWh")+
        stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
      visu <- grid.arrange(plot1,box1,ncol=2,nrow=1)
    }

    
    return (visu)
  })
  # Consommation electrique par heure
  output$consommationHeure <- renderPlot({
    input$saison
    input$boxplot
    
    if (input$saison==TRUE)
    {
      if (input$boxplot == FALSE)
      {
        visu <- ggplot(mesData, aes_string("HHFactor","consommation",group="HHFactor")) + 
        geom_col(fill="blue") + labs(title = "Consommation par heure", x = "Heure", y="MWh")+ 
        facet_wrap( ~saison,ncol=2) 
      }
      else
      {
        #Box Plot
        visu <-ggplot(mesData, aes_string("HHFactor","consommation",group="HHFactor")) + 
                geom_boxplot(fill = "blue")+
                 facet_wrap( ~saison,ncol=2)+
                labs(title = "Consommation horaire", x = "Heure", y="MWh")+
                stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
      }
    }
    else
    {
      plot1 <- ggplot(mesData, aes_string("HHFactor","consommation",group="HHFactor")) + 
        geom_col(fill="blue") + labs(title = "Consommation par heure", x = "Heure", y="MWh")
      box1 <-ggplot(mesData, aes_string("HHFactor","consommation",group="HHFactor")) + 
        geom_boxplot(fill = "blue")+
       labs(title = "Consommation horaire", x = "Heure", y="MWh")+
        stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
      visu <- grid.arrange(plot1,box1,ncol=2,nrow=1)
    }
    
    return (visu)
  })
  #Utilisation des bornes par Mois
  output$bornespermois <- renderPlot({
    transactionsPerHeure2018$MoisFactor <- as.factor(lubridate::month(transactionsPerHeure2018$DateTimeDebut))
    plot1 <- ggplot(transactionsPerHeure2018, aes_string("MoisFactor","Consommationkwh",group="MoisFactor")) + 
      geom_col(fill="green")+ labs(title = "Consommation des bornes par mois", x = "mois", y="kwh")
    
    #Boxplot d'utilisation des bornes par Mois
    box1 <- ggplot(transactionsPerHeure2018, aes_string("MoisFactor","Consommationkwh",group="MoisFactor")) + 
      geom_boxplot(fill="green")+ labs(title = "boxplot", x = "mois", y="kwh") +
      stat_summary(fun.y=mean, geom="point", shape=1, size=2,color="#191919")
    visu <- grid.arrange(plot1,box1,ncol=2,nrow=1)
    return (visu)
  })
  #Boxplot d'utilisation des bornes par Heure
  output$bornesperheure <- renderPlot({
    input$saison
    input$boxplot
    
    if (input$saison==TRUE)
    {
      if (input$boxplot==FALSE)
      {
        #Utilisation des bornes par heure
        visu <- ggplot(transactionsPerHeure2018, aes_string("HHFactor","Consommationkwh",group="HHFactor")) + 
                  geom_col(fill="green") + labs(title = "Consommation des bornes par heure", x = "Heure", y="kwh")+
                  facet_wrap( ~saison,ncol=2)
      } else
      {
        #Boxplot d'utilisation des bornes par heure
        visu <- ggplot(transactionsPerHeure2018, aes_string("HHFactor","Consommationkwh",group="HHFactor")) + 
          geom_boxplot(fill="green")+ labs(title = "boxplot", x = "heure", y="kwh") +
          facet_wrap( ~saison,ncol=2)+
          stat_summary(fun.y=mean, geom="point", shape=1, size=2,color="#191919")
      }
    }
    else
    {
        #Utilisation des bornes par heure
        plot1 <- ggplot(transactionsPerHeure2018, aes_string("HHFactor","Consommationkwh",group="HHFactor")) + 
          geom_col(fill="green") + labs(title = "Consommation des bornes par heure", x = "Heure", y="kwh")
        
        #Boxplot d'utilisation des bornes par heure
        box1 <- ggplot(transactionsPerHeure2018, aes_string("HHFactor","Consommationkwh",group="HHFactor")) + 
          geom_boxplot(fill="green")+ labs(title = "boxplot", x = "heure", y="kwh") +
          stat_summary(fun.y=mean, geom="point", shape=1, size=2,color="#191919")
        visu <- grid.arrange(plot1,box1,ncol=2,nrow=1)
    }
    
    return (visu)
  })
  ############################################################################################################
  ### DASHBOARD tabname=histofacet                                                                           #
  ### VISU FACET VILLES / BORNES                                                                             #
  ############################################################################################################
  output$villesbornes <- renderText({
    "Les villes qui ont plusieurs bornes sont les suivantes : 
    Embrun, Tallard, Guillestre, Laragne-Monteglin, Savines-Le-Lac, L'Argentiere-La-Bessee,
    Le Devoluy, Le Monetier-Les-Bains, Les Orres, St Firmin, Veynes"
  })
  
  unite18 <- eventReactive(
    {
      input$varhist18
    }, 
    {
      if(input$varhist18 == "Consommationkwh"){
        unite <- "kwh"
      } else if(input$varhist18 == "DureeChargemin"){
        unite <- "minutes"
      } else if(input$varhist18 == "nbTransaction"){
        unite <- "nombre"
      }
      return (unite)
    })
  tempshist18 <- eventReactive(
    {
      input$temps
    },
    {
      if(input$temps == "mois")
      {
        vartemps <- "MonthDebut"
      }
      else
      {
        vartemps <- "week"
      }
      return (vartemps)
    })
  # Nb transaction : colour="red",fill="orange"
  # Consommation : colour="blue",fill="turquoise"
  # Charge : colour="purple",fill="pink"
  colorfill <- eventReactive(
    {
      input$varhist18
    }, 
    {
      if(input$varhist18 == "Consommationkwh"){
        col <- "turquoise"
      } else if(input$varhist18 == "DureeChargemin"){
        col <- "red"
      } else if(input$varhist18 == "nbTransaction"){
        col <- "orange"
      }
      return (col)
  })
  datahisto18 <- eventReactive(
    {
      input$entite
      input$temps
    }, 
    {
      if(input$entite == "Ville"){
        if(input$temps == "mois")
        {
          data <- transactionsPerMoisPerVille2018
        }
        else
        {
          data <- transactionsPerSemainePerVille2018
        }
      } else if(input$entite == "Borne"){
        if(input$temps == "mois")
        {
          data <- transactionsPerMoisPerBorne2018
        }
        else
        {
          data <- transactionsPerSemainePerBorne2018
        }
      }
      return (data)
    })
  # Visualisation de toutes les entites
  # echelle Y adaptee 
  output$ggplotfacet <- renderPlot({
    input$varhist18 # nbTransaction - Consommationkwh - DureeChargemin
    input$entite #Borne ou Ville
    input$temps #Mois ou Semaine
    vtitle = paste(input$varhist18,": consolidation",input$temps,"par",input$entite)
    #facet_wrap(~ Borne,scales="free_y",ncol=5) l echelle est adaptee a chaque wrap
    if (input$entite == "Borne")
    {
      # VISU BORNE
      ggplot(datahisto18(), aes_string(tempshist18(),input$varhist18)) +
        geom_col(colour="black",fill=colorfill())+ 
        facet_wrap(~ VilleBorne,ncol=4,scales="free_y") +
        labs(title = vtitle, x = input$temps, y=unite18())
    } else {
      # VISU VILLE
      ggplot(datahisto18(), aes_string(tempshist18(),input$varhist18)) +
        geom_col(colour="black",fill=colorfill())+ 
        facet_wrap(~ Ville,ncol=6,scales="free_y") +
        labs(title = vtitle, x = input$temps, y=unite18())
    }
  })
  # Visualisation de toutes les entites
  # echelle Y FIXE 
  output$ggplotfacet2 <- renderPlot({
    input$varhist18 # nbTransaction - Consommationkwh - DureeChargemin
    input$entite #Borne ou Ville
    input$temps #Mois ou Semaine
    vtitle = paste(input$varhist18,": consolidation",input$temps,"par",input$entite)
    #facet_wrap(~ Borne,scales="free_y",ncol=5) l echelle est adaptee a chaque wrap
    if (input$entite == "Borne")
    {
      # VISU BORNE
      ggplot(datahisto18(), aes_string(tempshist18(),input$varhist18)) +
        geom_col(colour="black",fill=colorfill())+ 
        facet_wrap( ~VilleBorne,ncol=4) +
        labs(title = vtitle, x = input$temps, y=unite18())
    } else {
      # VISU VILLE
      ggplot(datahisto18(), aes_string(tempshist18(),input$varhist18)) +
        geom_col(colour="black",fill=colorfill())+ 
        facet_wrap(~ Ville,ncol=6) +
        labs(title = vtitle, x = input$temps, y=unite18())
    }
  })
  ############################################################################################################
  ### DASHBOARD tabname=histo                                                                                #
  ### VISU FACET VILLES / BORNES                                                                             #
  ############################################################################################################
  unitevarhisto <- eventReactive(
    {
      input$varhisto
    }, 
    {
      if(input$varhisto == "Consommationkwh"){
        unite <- "kwh"
      } else if(input$varhisto == "DureeChargemin"){
        unite <- "minutes"
      } else if(input$varhisto == "nbTransaction"){
        unite <- "nombre"
      }
      return (unite)
    })
  # Nb transaction : couleur #A6B06D ou #98DB9C
  # Consommation : couleur #589482
  # Charge : couleur #8C2423
  colorvarhisto <- eventReactive(
    {
      input$varhisto
    }, 
    {
      if(input$varhisto == "Consommationkwh"){
        col <- "#589482"
      } else if(input$varhisto == "DureeChargemin"){
        col <- "#8C2423"
      } else if(input$varhisto == "nbTransaction"){
        col <- "#A6B06D"
      }
      return (col)
    })
  datavarmois <- eventReactive(
    {
      input$annee
      input$nombre
    }, 
    {
      if(input$annee == 2017){
        data <- transactionsPerMoisPerBorne2017
      } else if(input$annee == 2018){
        data <- transactionsPerMoisPerBorne2018
      } else {
        data <- transactionsPerMoisPerBorne
      }
      if (input$nombre <= length(data$Borne))
      {
        listBornes <- listBornes[1:input$nombre]
        data <- filter(data, Borne %in% listBornes)
      }
      return (data)
    })
  datavarjour <- eventReactive(
    {
      input$annee
      input$nombre
    }, 
    {
      if(input$annee == 2017){
        data <- transactionsPerJour2017
      } else if(input$annee == 2018){
        data <- transactionsPerJour2018
      } else {
        data <- transactionsPerJour
      }
      if (input$nombre <= length(data$Borne))
      {
        listBornes <- listBornes[1:input$nombre]
        data <- filter(data, Borne %in% listBornes)
      }
      return (data)
    })
  datavarheure <- eventReactive(
    {
      input$annee
      input$nombre
    }, 
    {
      if(input$annee == 2017){
        data <- transactionsPerHeure2017
      } else if(input$annee == 2018){
        data <- transactionsPerHeure2018
      } else {
        data <- transactionsPerHeure
      }
      if (input$nombre <= length(data$Borne))
      {
        listBornes <- listBornes[1:input$nombre]
        data <- filter(data, Borne %in% listBornes)
      }
      return (data)
    })
  #VISU SEASONAL
  output$visuseasonal <- renderPlot({
    input$varhisto
    
    #create a ts object
    tsMois <- ts(transactionsPerMois[[input$varhisto]], start=c(2017, 1), end=c(2018, 11), frequency=12)
    axey <- paste(input$varhisto,"en",unitevarhisto(),"Toutes les bornes","")
    visu<-seasonplot(tsMois,year.labels =TRUE,col=1:6, year.labels.left=TRUE, ylab=unitevarhisto() ,xlab="mois de janvier 2017 - novembre 2018",main=axey)
    return (visu)
  })
  #PLOT jour
  output$plotperjour <- renderPlot({
    input$varhisto
    input$annee
    title = paste(input$varhisto," : consolidation jour",input$annee,"")
    xlabtext = paste("jours annee(s)", input$annee,"")
    plot(datavarjour()[["DateDebut"]],datavarjour()[[input$varhisto]], type ="h", ylab=unitevarhisto(), xlab=xlabtext, main=title,col=colorvarhisto())
   })
  #PLOT mois
  output$plotpermois <- renderPlot({
    input$varhisto
    input$annee
    title = paste(input$varhisto," : consolidation mois", input$annee,"")
    xlabtext = paste("mois annee(s)", input$annee,"")
    plot(datavarmois()[["MonthDebut"]],datavarmois()[[input$varhisto]], type ="h",ylab=unitevarhisto(), xlab=xlabtext, main=title,col=colorvarhisto())
    #plot(transactionsPerMois[["MonthDebut"]],transactionsPerMois[[input$varhisto]], type ="h",ylab=unitevarhisto(), xlab=xlabtext, main=title,col=colorvar())
  })
  #Utilisation des bornes par Mois
  output$ggpermois <- renderPlot({
    input$varhisto
    input$annee
    title = paste(input$varhisto," : consolidation mois", input$annee,"")
    ggplot(datavarmois(), aes_string("MoisFactorDebut",input$varhisto,group="MoisFactorDebut")) + 
        geom_col(fill=colorvarhisto())+ labs(title = title, x = "mois", y=unitevarhisto())
  })
  #Boxplot d'utilisation des bornes par Mois
  output$ggboxpermois <- renderPlot({
    input$varhisto
    input$annee
    title = paste(input$varhisto," : consolidation mois", input$annee,"")
    ggplot(datavarmois(), aes_string("MoisFactorDebut",input$varhisto,group="MoisFactorDebut")) + 
        geom_boxplot(fill=colorvarhisto())+ labs(title = title, x = "mois", y=unitevarhisto()) +
        stat_summary(fun.y=mean, geom="point", shape=1, size=2,color="#191919")
  })
  #Utilisation des bornes par Jour
  output$ggperjour <- renderPlot({
    input$varhisto
    input$annee
    title = paste(input$varhisto," : consolidation jour", input$annee,"")
    ggplot(datavarjour(), aes_string("typeJourFactorDebut",input$varhisto,group="typeJourFactorDebut")) + 
      geom_col(fill=colorvarhisto())+ labs(title = title, x = "Type de jour", y=unitevarhisto())
  })
  #Boxplot d'utilisation des bornes par Jour
  output$ggboxperjour <- renderPlot({
    input$varhisto
    input$annee
    title = paste(input$varhisto," : consolidation jour", input$annee,"")
    ggplot(datavarjour(), aes_string("typeJourFactorDebut",input$varhisto,group="typeJourFactorDebut")) + 
      geom_boxplot(fill=colorvarhisto())+ labs(title = title, x = "Type de jour", y=unitevarhisto()) +
      stat_summary(fun.y=mean, geom="point", shape=1, size=2,color="#191919")
  })
  #Utilisation des bornes par heure
  output$ggperheure <- renderPlot({
    input$varhisto
    title = paste(input$varhisto," : consolidation heure", input$annee,"")
    ggplot(datavarheure(), aes_string("HHFactor",input$varhisto,group="HHFactor")) + 
      geom_col(fill=colorvarhisto()) + labs(title = title, x = "Heure", y=unitevarhisto())
  })
  #Boxplot d'utilisation des bornes par heure
  output$ggboxperheure <- renderPlot({
    input$varhisto
    title = paste(input$varhisto," : consolidation heure", input$annee,"")
    ggplot(datavarheure(), aes_string("HHFactor",input$varhisto,group="HHFactor")) + 
      geom_boxplot(fill=colorvarhisto()) + labs(title = title, x = "Heure", y=unitevarhisto())+
      stat_summary(fun.y=mean, geom="point", shape=23, size=1,color="#191919")
  })
}
