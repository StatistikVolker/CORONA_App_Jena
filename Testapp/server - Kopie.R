## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Ben Phillips
##
## Date Created: 2020-03-12
##
## Email: phillipsb@unimelb.edu.au
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------
## load up the packages we will need 
library(shiny)
library(tidyverse)
## ---------------------------

## source files
#source("getRKIdata.R")

## ---------------------------
options(scipen=9999)


# Define server logic 
shinyServer(function(input, output) {

#  tabPanel "Aktulle Fallzahlen pro LK" ---------------------------------------------

# IARplot
  IARfootnote <- renderText({
    paste0("* geschätzt (",input$IARmodell," Szenario)")
  })
  
  weight <- eventReactive(input$IARmodell,{
    if (input$IARmodell == "pessimistisches") {weight <- c(rep(1,7),rep(0.810,7),rep(0.476,7),rep(0.238,7),rep(0.048,28));length(weight)} # PPW Jena
    if (input$IARmodell == "mittleres") {weight <- c(rep(1-0,7),rep(1-0.079,7),rep(1-0.416,7),rep(1-0.911,7),rep(1-0.980,28));length(weight)} # NPW Jena, 7 Tage bis first recovery
    if (input$IARmodell == "optimistisches") {weight <- c(rep(1-0,5),rep(1-0.079,7),rep(1-0.416,7),rep(1-0.911,7),rep(1-0.980,28));length(weight)} # NPW Jena, 5 Tage bis first recovery
    return(weight)
    
  })
        #weight by Npwvals
    #wj = length(weight)

  output$IARplot <- renderPlot({
      #Datensatz für LK
    df <- dftsI  %>% 
      filter(county == input$IARLK)  %>%
      #filter(county == "SK Jena")  %>%
      filter(if (infected[Datum == "2020-02-15"]==0) Datum >= "2020-02-15") %>%
      filter(if (infected[Datum == "2020-03-01"]==0) Datum >= "2020-03-01") %>%
      mutate(id = row_number()) %>% 
      rename(i = infected)
    
    wj = length(weight())
    # berechne active cases
    for (k in df$id){
      #  print(k)
      if (k + wj < dim(df)[1])   {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * weight()),rep(df$i[k] * 0,dim(df)[1]-(k+wj)))}
      if (k + wj == dim(df)[1])  {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * weight()))}
      if (k + wj > dim(df)[1] & k < dim(df)[1])   {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * weight()[1:(dim(df)[1]-k)]))}
      if (k == dim(df)[1])   {df[paste0("col",k)] <- rep(0,k)}
    }
    
    # Datensatz mit infected, actve und recovered Zahlen
    dfplot<-df %>% mutate(a = select(., contains("col")) %>% rowSums() + i) %>%
      mutate(i = cumsum(i),
             r = i-a) %>%
      select(Datum, i,a,r) %>%
      pivot_longer(-Datum,names_to = "status",values_to = "nsum") %>%
      mutate(status = factor(status,levels = c("i","a","r")))
    
     # Plotte
    mindat <- floor_date(as.Date(min(dfplot$Datum)),unit = "week");mindat
    maxdat <- ceiling_date(as.Date(max(dfplot$Datum)),unit = "week");maxdat
    poltweeks <- as.numeric(maxdat-mindat)/7
    ggplot() +
      geom_point(data = dfplot,aes(x = Datum,y = nsum, color = status, group = status),size = 3) +
      geom_step(data = dfplot,aes(x = Datum,y = nsum, color = status, group = status),size = 2) +
      scale_colour_manual(values = c("darkblue","forestgreen","orange"),labels =  c("infected","active*","recovered*")) + 
      theme_minimal() +
      theme(legend.position =  "bottom") +
      theme(legend.position =  "bottom",
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 16),
            legend.title = element_blank(),
            legend.text = element_text(size = 16),
            panel.grid.major.x = element_line(colour="red", size=0.75)) + 
      scale_y_continuous(breaks = seq(0,200,25),labels =seq(0,200,25)) +
      labs(y = "absolute Anzahl",x = "Datum") +
      scale_x_date(breaks = seq(mindat,maxdat,by="week"),
                   date_labels = "%b %d", minor_breaks = "1 day") +
      annotate("rect", xmin = mindat-1, xmax = maxdat+2, ymin = -10, ymax = -4,fill = "white") +
      annotate("text",y=-6,x = c(seq(mindat,maxdat,"day")),
               label = c(rep(c("S","M","D","M","D","F","S"),poltweeks),"S"), 
               color =c(rep(c("red",rep("blue",5),"orange"),poltweeks),"red")) + 
      labs(caption = IARfootnote())
           
    
  })
  
#  tabPanel "Verdopplungsate" ---------------------------------------------
  #Reactives:
  # Berechungsgrundlage er VDZ (5, 7, 10 Tage)

  footnote <- renderText({
    paste0("VDZ (bezogen auf ",as.numeric(input$vdzdays)," Tage) am ",format(as.Date(input$vdzdatum),"%d. %B %Y"))
  })
 # Deutschlandkarte
  output$VDZPlot <- renderPlot({
    vdz <- paste0("VDzeit",input$vdzdays)
    
    # DatensatzVDZ vorbereiten für Plot
#    if (input$vdzdays() == 10){
#      vdz <- paste0("VDzeit",input$vdzdays
#    } else {
#      vdz <- paste0("VDzeit0",input$vdzdays)
 #   }
    
    names(dfVDZall)<-namesdfVDZall
    names(dfVDZall)[names(dfVDZall)==vdz]<-"VDZlab"
    
    
    #Datensatz mit Geographie für Plot
    dfVDZgeo <-   merge(geo_lk,dfVDZall) %>%
      filter(datum == input$vdzdatum)  %>%
      full_join(dfVDZcol)
    
    # Deutschlandkarte
    ggplot()+
    geom_sf(data = dfVDZgeo,aes(fill = VDZlab)) +
    scale_fill_manual(values = VDZcol) +
    theme_void() +
    theme(legend.text = element_text(size = 12),
          title = element_text(size = 10,face = "bold"),
          plot.caption = element_text(hjust=0, size=rel(1.2))
          ) +
    labs(caption = footnote(),
         fill = "Verdopplungszeit\nin den SK und LK\nDeutschlands"
    )
  # Abspeichern
  #ggsave(paste0("VDZplots/",vdzmapdat," Verdopplungszeit in D_",vdz,".jpg"))
  })
  
  
  

#   titlePanel "Coronavirus 10-Tages Vorhersage auf Landkreisebene --------

  output$result1.1 <- renderPrint(dim(pDat))
  
  ##### Doubling time ##### 
  output$doubTime <- renderText({
    pDat <-tsSub(tsICountry, tsICountry$Country %in% input$LKfinder)
    
    dTime <- round(doubTime(pDat, dates), 1)
    #dTime
  })

  ##### Doubling time ##### 
  output$dtimetab <- renderTable({
    pDat <-tsSub(tsICountry, tsICountry$Country %in% input$LKfinder)
    dTime10 <- round(doubTime(pDat, dates,inWindow = 10), 1)
    dTime07 <- round(doubTime(pDat, dates,inWindow = 7), 1)
    dTime05 <- round(doubTime(pDat, dates,inWindow = 5), 1)
    if (dTime05 == 999) dTime05 = "keine Neuinfektion seit 5 Tagen"
    if (dTime07 == 999) dTime07 = "keine Neuinfektion seit 7 Tagen"
    if (dTime10 == 999) dTime10 = "keine Neuinfektion seit 10 Tagen"
    #dTtab <- format(as.integer(c(tail(yA[!is.na(yA)], 1), tail(lDat$y[,"lwr"],1), tail(lDat$y[,"upr"],1))), big.mark = ",")
    dTtab <- c(dTime10,dTime07,dTime05)
    dim(dTtab) <- c(1, 3)
    colnames(dTtab)<-c("10 Tage","7 Tage","5 Tage")
    dTtab
  }, rownames = FALSE)
  
  
 

    
 # ##### Doubling time ##### 
#  output$doubTime <- renderText({
#    pDat <- tsSub(tsICountry, tsICountry$Country %in% input$countryFinder)
#    dTime <- round(doubTime(pDat, dates), 1)
#  })
  
  
  
  #### Reactive expressions for forecast page ####
  yAfCast <-reactive({ # subset country for forecast page
    tsSub(tsI,tsI$Country.Region %in% input$LKfinder)
  })
  
  
  
  projfCast <- reactive({ # projection for forecast
    yA <- yAfCast()
    
    #yA <- tsI %>% filter(Country.Region == "Jena")
    projSimple(yA, dates)
  })
  
  ##### Raw stats #####  
  output$rawStats <- renderTable({
    yA <- yAfCast()
    yD <- tsSub(tsD,tsD$Country.Region %in% input$LKfinder)
    yI <- tsSub(tsI,tsI$Country.Region %in% input$LKfinder)
    #yR <- tsSub(tsR,tsR$Country.Region %in% input$LKfinder)
    nn <-length(yI)
    if (is.na(yA[nn])) nn <- nn-1
    out <- as.integer(c(yI[nn], yD[nn]))
    dim(out) <-c(1,2)
    colnames(out) <- c("Total", "Deaths")
    format(out, big.mark = ",")
  }, rownames = FALSE)
  
##### Raw plot #####  
  output$rawPlot <- renderPlot({
    yA <- yAfCast()
    lDat <- projfCast()
    yMax <- max(c(lDat$y[,"fit"], yA), na.rm = TRUE)
    yTxt <- "Confirmed active cases"
    plot(yA~dates, 
         xlim = c(min(dates), max(lDat$x)),
         ylim = c(0, yMax),
         pch = 21,
         cex = 2,
         bty = "u", 
         xlab = "Date", 
         ylab = yTxt,
         main = input$LKfinder,
         col = "black",
         bg = tsE$plotcol[tsE$county == input$LKfinder])
    axis(side = 4)
    lines(lDat$y[, "fit"]~lDat$x,col = "darkred")
    lines(lDat$y[, "lwr"]~lDat$x, lty = 2,col = "darkred")
    lines(lDat$y[, "upr"]~lDat$x, lty = 2,col = "darkred")
  })
  
##### Log plot #####    
  output$logPlot <- renderPlot({
    yA <- yAfCast()
    lDat <- projfCast()
    yMax <- max(c(lDat$y[,"fit"], yA), na.rm = TRUE)
    yTxt <- "Confirmed active cases (log scale)"
    plot((yA+0.1)~dates, 
         xlim = c(min(dates), max(lDat$x)),
         ylim = c(1, yMax),
         log = "y",
         pch = 21,
         cex = 2,
         bty = "u", 
         xlab = "Date", 
         ylab = yTxt,
         main = input$LKfinder,
         col = "black",
         bg = tsE$plotcol[tsE$county == input$LKfinder])
    axis(side=4)
    lines(lDat$y[, "fit"]~lDat$x,col = "darkred")
    lines(lDat$y[, "lwr"]~lDat$x, lty = 2,col = "darkred")
    lines(lDat$y[, "upr"]~lDat$x, lty = 2,col = "darkred")
  })
  
##### Detection rate #####    
  output$detRate <- renderText({
    yD <- tsSub(tsD,tsD$Country.Region %in% input$LKfinder)
    yI <- tsSub(tsI,tsI$Country.Region %in% input$LKfinder)
    dR<-round(detRate(yI, yD), 4)
    if (is.na(dR)) "Insufficient data for estimation" else dR
  })
  
##### Prediction table confirmed #####    
  output$tablePredConf <- renderTable({
    yA <- yAfCast()
    lDat <- projfCast()
    nowThen <- format(as.integer(c(tail(yA[!is.na(yA)], 1), tail(lDat$y[,"lwr"],1), tail(lDat$y[,"upr"],1))), big.mark = ",")
    nowThen <- c(nowThen[1], paste(nowThen[2], "-", nowThen[3]))
    dim(nowThen) <- c(1, 2)
    colnames(nowThen)<-c("Now", "In 10 days (min-max)")
    nowThen
  }, rownames = FALSE)
  
##### Prediction table true #####    
  output$tablePredTrue <- renderText({
    yA <- yAfCast()
    yD <- tsSub(tsD,tsD$Country.Region %in% input$LKfinder)
    yI <- tsSub(tsI,tsI$Country.Region %in% input$LKfinder)
    dRate <- detRate(yI, yD)
    lDat <- projfCast()
    now <- tail(yA[!is.na(yA)], 1)
    nowTrue <- format(round(now/dRate, 0), big.mark = ",")
    #nowThenTrue <- c(round(nowThenTrue[1],0), paste(round(nowThenTrue[2],0), "-", round(nowThenTrue[3],0)))
    #dim(nowThenTrue) <- c(1, 2)
    #colnames(nowThenTrue)<-c("Now", "In 10 days (min-max)")
    nowTrue
  })
  
#  tabPanel "Wachstumsrate und 'Flattening the Curve'-Index" --------------

  
    checkGroup <- reactive({
      c(input$cboxLK_TH,
        input$cboxLK_BW,
        input$cboxLK_BY,
        input$cboxLK_BB,
        input$cboxLK_HE,
        input$cboxLK_MV,
        input$cboxLK_NS,
        input$cboxLK_NRW,
        input$cboxLK_RP,
        input$cboxLK_SL,
        input$cboxLK_SN,
        input$cboxLK_SA,
        input$cboxLK_SH,
        input$cboxLK_SS,
        input$cboxJvgl_SK,
        input$cboxcity_SN,
        input$cboxJvgl_LK
      )
    })

  
  

  
  growthSub <- reactive({
    #subset(tsICountry, tsICountry$Country %in% checkGroup())
    tsICountry %>% filter(Country %in% checkGroup())
  })

  ##### Growth rate #####    
  output$growthRate <- renderPlot({
    
    pDatGR <- tsICountry %>% filter(Country %in% checkGroup()) %>%
    #pDatGR <- tsICountry %>% filter(Country %in% "SK Jena") %>%
      pivot_longer(-Country,names_to = "Datum",values_to = "cases") %>%
      filter(Datum > as.Date(max(Datum))-(inWindow+1)) %>%
      group_by(Country) %>%
      mutate(casebefore = dplyr::lag(cases, n = 1, default = NA),
             rate = 100 * (cases - casebefore)/casebefore) %>%
      ungroup() %>% 
      #mutate(county = as.factor(Country),
      #       county = droplevels(Country))  %>%
      rename(county = Country) %>%
      filter(!is.na(rate))
    print(pDatGR)
    #levels(pDat$county)
    # Grafik (ggplot)
    #names(pDat)
    mindat <- as.Date(min(pDatGR$Datum))
    maxdat <- as.Date(max(pDatGR$Datum))
    ggplot(data = pDatGR,aes(x = as.Date(Datum),y = rate, color = county, fill = county)) +
      geom_bar(stat="identity", position='dodge') +
      theme_minimal() +
      theme(legend.position =  "bottom",
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 16),
            legend.title = element_blank(),
            legend.text = element_text(size = 16),
            panel.grid.major.x = element_line(colour="white", size=0.75)
      ) + 
      scale_x_date(breaks = seq(mindat,maxdat,by="days"),
                   date_labels = "%b %d", minor_breaks = "2 days") +
      colfill + colcolor +
      labs(y = "Wachstumsrate (% pro Tag)",x = "Datum") +
      annotate("rect", xmin = mindat-1, xmax = maxdat+2, ymin = -0.3, ymax = -0.02,fill = "white") #+
    
    
    
  })
  
  
  ##### Curve-flattenning #####    
  output$cfi <- renderPlot({
    pDatCFI <- tsICountry %>% filter(Country %in% checkGroup()) %>%
      pivot_longer(-Country,names_to = "Datum",values_to = "cases") %>%
      rename(county = Country) %>%
      mutate(logcase = log(cases)) %>%
      group_by(county) %>%
      mutate(casebefore = dplyr::lag(logcase, n = 1, default = NA),
             diff = casebefore - logcase,
             diffbefore = dplyr::lag(diff, n = 1, default = NA),
             diffIND = diffbefore - diff,
             ind = diffIND/abs(diff),
             ind = if_else(abs(ind)>10,NA_real_,ind),
             ind = if_else(is.infinite(ind),0,ind),
             ind = if_else(is.na(ind),0,ind)
             ) %>%
      ungroup() 
    
    mindat <- ceiling_date(as.Date(min(pDatCFI$Datum)),unit = "week")
    maxdat <- floor_date(as.Date(max(pDatCFI$Datum)),unit = "week")
    
    ggplot(data = pDatCFI,aes(x = as.Date(Datum), y = ind, color = county, group = county)) +
      #geom_line(size = 2) +
      geom_hline(yintercept = 0,size = 2,linetype="dashed", color = "black") +
      geom_smooth(method = "loess",se=FALSE,size =2) +
      theme_minimal() +
      theme(legend.position =  "bottom",
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 16),
            legend.title = element_blank(),
            legend.text = element_text(size = 16),
            #panel.grid.minor.x = element_line(colour="blue", size=0.5), 
            panel.grid.major.x = element_line(colour="red", size=0.75)) + 
      
      colcolor +
      labs(y = "Curve-flatenning Index",x = "Datum") +
      scale_x_date(breaks = seq(mindat,maxdat,by="week"),
                   date_labels = "%b %d", minor_breaks = "1 week") 
  })

# tabPanel "Intensivbetten" -----------------------------------------------
  # GRAFIK KARTE THÜRINGEN
  
  # Übergaben - ausgewähltes Datum
  immapdatum <- renderText({
    format(as.Date(input$imdatum,format= ,"%d. %B %Y"),"%Y-%m-%d")
    #format(as.Date(input$imdatum),"%Y-%b-%d")
    #testdata<-format(Sys.Date()-1,"%d. %B %Y")
    #format(as.Date(testdata,format= ,"%d. %B %Y"),"%Y-%m-%d")
})
  
  
  # Angenommener Anteil intensivmedizinischer Patienten
  impatserious<-reactive({
    max(input$imPats) /100
  })
  # Bereite Daten für Plot auf
  
  
  source("getmapTHdata.R") # enthält das folgende Makro
  output$immapPlot <- renderPlot({
    
    
 
    dfplotgeo <- mkr.getmapTHdata(impatser = impatserious(),immapdat =  immapdatum() )
    
  # Erstelle Mapplot
    ggplot()+
      geom_sf(data = dfplotgeo,aes(fill = warnIntstat)) +
      scale_fill_manual(values = Intbedcol) +
      theme_void() +
      theme(#legend.position = "bottom",
            legend.text = element_text(size = 12),
            #title.text = element_text(size = 20),
            title = element_text(size = 16,face = "bold")
      ) +
      #guides(fill = guide_legend(title.position="top", title.hjust = 0,nrow=2,byrow=TRUE)      ) +
      labs(#title = "Landkreise mit potentiell zu wenigen Intensivmedizinische Betten",
        title = paste0("Prognose für den ",format(as.Date(immapdatum()),"%d. %B %Y")),
        fill = "Bedarf an\nIntensivbetten\n für CORONA"
      )
  })
  
  
  
  # Datensatz erstellen
  #input == "SK Jena"
  #output$result <- renderPrint(impatserious())
  #output$result2 <- renderPrint(max(input$imPats))
  output$impatPlot <- renderPlot({
    plotdf <- tsI %>% 
      rename(county = Country.Region) %>%
      filter(county %in% input$impatLK) %>%
      group_by(county) %>%
      pivot_longer(-county,names_to = "Datum",values_to = "cases") %>%
      mutate(seriouscases = if_else((cases*impatserious())>0.1,ceiling(cases*impatserious()),floor(cases*impatserious()))) %>%
      left_join(KKH_TH_Intbeds) %>%
      filter(case_when(cases[Datum == "2020-03-01"]==0 ~ Datum>="2020-02-29",
                       cases[Datum == "2020-02-15"]==0 ~ Datum>="2020-02-15",
                       TRUE ~ Datum>=min(Datum))
      )
    
    
    
    maxdat <- as.Date(max(plotdf$Datum))
    
    
    # Prognose 10 Tage
    yA <-tsSub(tsI,tsI$Country.Region %in% input$impatLK)
    proj <- projSimple(yA, dates)
    
    
    dfproj<- cbind.data.frame(proj$x,proj$y) %>%
      rename(Datum = 'proj$x') %>%
      mutate(projsc = if_else((fit*impatserious())>0.1,ceiling(fit*impatserious()),floor(fit*impatserious())),
             projscl = if_else((lwr*impatserious())>0.1,ceiling(lwr*impatserious()),floor(lwr*impatserious())),
             projscu = if_else((upr*impatserious())>0.1,ceiling(upr*impatserious()),floor(upr*impatserious())),
             county = input$impatLK
      ) %>%
      filter(Datum > maxdat)
    
  # Grafik attribute 
    Intbeds<-plotdf$IntensivBettenLK[1]
    yscmax<-max(Intbeds,max(dfproj$projscu))
    yInblab <- if_else(Intbeds < yscmax,yscmax,Intbeds)
    anottxt <- paste0("Intensivmedizinische Betten in ",plotdf$county[1]," (N = ",Intbeds,")")
  # Grafik (ggplot)
  mindat <- ceiling_date(as.Date(min(plotdf$Datum)),unit = "week")
  maxdat <- floor_date(as.Date(max(dfproj$Datum)),unit = "week")
    
  ggplot(data = plotdf,aes(x = as.Date(Datum),y = seriouscases, color = county, fill = county)) +
    geom_hline(yintercept = Intbeds,size = 2,linetype="solid", color = "black") +
    geom_label(x = mindat-2, y = yInblab+0.05*yInblab,label=anottxt,color = "black",fill = "white",hjust =0,label.padding = unit(0.4, "lines"),size = 5) +
    # 'REALE' DATEN
    geom_bar(stat="identity",width=.5) + #, position='dodge') +
    #pROGNOSEDATEN
    geom_crossbar(data = dfproj,aes(x = as.Date(Datum),y = projsc,ymin = projscl, ymax = projscu,alpha = 0.5), width = 0.5) +
    geom_point(data = dfproj,aes(x = as.Date(Datum),y = projsc),size = 3) + #, position='dodge') +
    theme_minimal() +
    theme(legend.position =  "none",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16),
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          #panel.grid.minor.x = element_line(colour="blue", size=0.5), 
          panel.grid.major.x = element_line(colour="red", size=0.75)
          ) + 
      scale_x_date(breaks = seq(mindat,maxdat,by="week"),
                   date_labels = "%b %d", minor_breaks = "1 week") +
      scale_y_continuous(limits = c(0,1.1*yscmax)) +
      colfill + colcolor +
      labs(y = "kritische CORONA Patienten (geschätzt)",x = "Datum")# +
  })   
  

# in Bearbeitung: ##### Doubling time #####  ------------------------------

  
  
##### Doubling time plot #####    
  output$doubTimePlot <- renderPlot({
    #pDat <- subset(tsICountry, tsICountry$Country %in% checkGroup())
    
    pDat <- tsICountry %>% filter(Country %in% checkGroup()) %>% left_join(tsE)
    dTime <- as.matrix(doubTime(pDat))
    dTime[!is.finite(dTime)]<-NA
    clrs<-pDat$plotcol
    dates10 <- dates[(length(pDat)-10+1):length(pDat)]
    counts <- table(dTime)
    barplot(dTime,
            main="Doubling time",
            xlab="Date", 
            ylab="Doubling time (days)",
            beside=TRUE,
            col = clrs,
            legend = checkGroup(),
            args.legend = list(bty = "n", x = "topleft"))
    
    
   
    
  })
})
