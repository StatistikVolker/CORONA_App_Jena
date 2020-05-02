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
  
  # Gewichtungsfaktoren aus Jena Daten:
  PPWweights<-c(rep(1,7),rep(0.810,7),rep(0.476,7),rep(0.238,7),rep(0.048,28))
  NPWweights<-c(rep(1-0,7),rep(1-0.079,7),rep(1-0.416,7),rep(1-0.911,7),rep(1-0.980,28))
  NPWweightsopt<-c(rep(1-0,5),rep(1-0.079,7),rep(1-0.416,7),rep(1-0.911,7),rep(1-0.980,28))
  
#  tabPanel "Aktulle Fallzahlen pro LK" ---------------------------------------------

  # ----------------------------------------------------------------------------------------------------
  # IARplot
  # ----------------------------------------------------------------------------------------------------

  IARfootnote <- renderText({
    paste0("* geschätzt (",input$IARmodell," Szenario)")
  })
  
  IARweight <- eventReactive(input$IARmodell,{
    if (input$IARmodell == "by PPW") {weight <- PPWweights;length(weight)} # PPW Jena
    if (input$IARmodell == "by NPW") {weight <- NPWweights;length(weight)} # NPW Jena, 7 Tage bis first recovery
    if (input$IARmodell == "by NPW optimistic") {weight <- NPWweightsopt;length(weight)} # NPW Jena, 5 Tage bis first recovery
    return(weight)
    
  })
  
  
  # ----------------------------------------------------------------------------------------------------
  # Daten IAR-Plot (dfLKstatus())
  # ----------------------------------------------------------------------------------------------------
  dfLKstatus <- reactive({
    df <- dftsI  %>% 
      filter(county == input$IARLK)#  %>%

    # Datumsfilter setzen
    if (df$infected[df$Datum == "2020-02-15"]==0) {df <- df %>% filter(Datum >= "2020-02-15")}
    if (df$infected[df$Datum == "2020-03-01"]==0) {df <- df %>% filter(Datum >= "2020-03-01")}
    # Zeilennzummer und kürzere Namen
    df <-df %>%
      mutate(id = row_number()) %>% 
      rename(i = infected)
    
    wj = length(IARweight())
    # berechne active cases
    for (k in df$id){
      #  print(k)
      if (k + wj < dim(df)[1])   {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * IARweight()),rep(df$i[k] * 0,dim(df)[1]-(k+wj)))}
      if (k + wj == dim(df)[1])  {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * IARweight()))}
      if (k + wj > dim(df)[1] & k < dim(df)[1])   {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * IARweight()[1:(dim(df)[1]-k)]))}
      if (k == dim(df)[1])   {df[paste0("col",k)] <- rep(0,k)}
    }
    
    # Datensatz mit infected, actve und recovered Zahlen
    dfplot<-df %>% mutate(a = select(., contains("col")) %>% rowSums() + i) %>%
      mutate(i = cumsum(i),
             r = i-a) %>%
      select(Datum, i,a,r) %>%
      pivot_longer(-Datum,names_to = "status",values_to = "nsum") %>%
      mutate(status = factor(status,levels = c("i","a","r")))
    #print(dfplot)
    return(dfplot)
  })
    
  
  # ----------------------------------------------------------------------------------------------------
  # Tabelle der Daten der letzten 7 Tage (tablast7days)
  # ----------------------------------------------------------------------------------------------------
  stat7days <- reactive({
    seldat <- max(dfLKstatus()$Datum)-6
    #print(seldat)
    tab <- dfLKstatus() %>%
      pivot_wider(names_from = "status",values_from = "nsum") %>%
      filter(Datum >= as.Date(seldat)) %>%
      mutate(i = format(round(i), nsmall = 0),
             a = format(round(a), nsmall = 0),
             r = format(round(r), nsmall = 0),
             Datum = as.Date(Datum)) %>%
      select(Datum,infected = i, active = a, recovered = r) %>%
      mutate(Datum = format(Datum,"%d. %B %Y")) %>%
      pivot_longer(-Datum,names_to = "status",values_to = "nsum") %>%
      pivot_wider(names_from = "Datum",values_from = "nsum")
    return(tab)
  })
  

  output$tablast7days <- renderTable(stat7days(),align = "lrrrrrrr")
  
  # ----------------------------------------------------------------------------------------------------
  # Grafik Status seit beginn (IARplot)
  # ----------------------------------------------------------------------------------------------------
  output$IARplot <- renderPlot({
      #Datensatz für LK
    
     # Plotte
    mindat <- floor_date(as.Date(min(dfLKstatus()$Datum)),unit = "week");mindat
    maxdat <- ceiling_date(as.Date(max(dfLKstatus()$Datum)),unit = "week");maxdat
    poltweeks <- as.numeric(maxdat-mindat)/7
    ggplot() +
      geom_point(data = dfLKstatus(),aes(x = Datum,y = nsum, color = status, group = status),size = 3) +
      geom_step(data = dfLKstatus(),aes(x = Datum,y = nsum, color = status, group = status),size = 2) +
      scale_colour_manual(values = c("darkblue","forestgreen","orange"),labels =  c("infected","active*","recovered*")) + 
      theme_minimal() +
      theme(legend.position =  "bottom") +
      theme(legend.position =  "bottom",
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 16),
            legend.title = element_blank(),
            legend.text = element_text(size = 16),
            panel.grid.major.x = element_line(colour="red", size=0.75)) + 
      #scale_y_continuous(breaks = seq(0,200,25),labels =seq(0,200,25)) +
      labs(y = "absolute Anzahl",x = "Datum") +
      scale_x_date(breaks = seq(mindat,maxdat,by="week"),
                   date_labels = "%b %d", minor_breaks = "1 day") +
      annotate("rect", xmin = mindat-1, xmax = maxdat+2, ymin = -10, ymax = -4,fill = "white") +
      annotate("text",y=-6,x = c(seq(mindat,maxdat,"day")),
               label = c(rep(c("S","M","D","M","D","F","S"),poltweeks),"S"), 
               color =c(rep(c("red",rep("blue",5),"orange"),poltweeks),"red")) + 
      labs(caption = IARfootnote())
           
    
  })
  

# Deutschlandkarte: Active Cases ------------------------------------------

  # ----------------------------------------------------------------------------------------------------
  # Einstellungen auslesen für später
  # ----------------------------------------------------------------------------------------------------

  # Übergaben - ausgewähltes Datum
  arpgdat <- renderText({
    format(as.Date(input$arpdat,format= ,"%d. %B %Y"),"%Y-%m-%d")
  })
  
  # Schätzszenario
  arpmodell <- renderText({
    input$arpmodell
  })

  # Fußnote für Plot
  arpfootnote <- renderText({
    paste0("* geschätzt (",arpmodell()," Szenario)")
  })
  
  # Gewichtungsfaktoren für Szenario bestimmen
  arpweight <- eventReactive(arpmodell(),{
    if (arpmodell() == "by PPW") {weight <- PPWweights;length(weight)} # PPW Jena
    if (arpmodell() == "by NPW") {weight <- NPWweights;length(weight)} # NPW Jena, 7 Tage bis first recovery
    if (arpmodell() == "by NPW optimistic") {weight <- NPWweightsopt;length(weight)} # NPW Jena, 5 Tage bis first recovery
    return(weight)
  })
  
  # Statuswahl: Was soll geplottet werden?
  arpstatus <- renderText({
    print(input$arpstatus)
    input$arpstatus
  })
  
  # Titel für Plot
  arpplottit <- renderText({
    paste("Heatmap für",arpstatus())
  })
  
  # Statuswahl: Was soll geplottet werden?
  arpfilter <- eventReactive(arpstatus(),{
    if (arpstatus() == "active cases") {filtercol == "a"}
    if (arpstatus() == "Anteil recovered") {filtercol == "rp"}
    return(filtercol)
  })
  
 
  
  
  # ----------------------------------------------------------------------------------------------------
  # Datensatz erstellen (alle SK/LK, letzte 10 Tage)
  # ----------------------------------------------------------------------------------------------------
  
  dfarpLK <- reactive({
    teller <- 0
    for (LK in unique(dftsI$county)){#[54:55]) {
      #LK <- unique(dftsI$county)[54]
      teller <- teller + 1
      print(paste(teller,"-",LK))
      
      df <- dftsI  %>% 
        filter(county == LK)
      # Datumsfilter setzen
      if (df$infected[df$Datum == "2020-02-15"]==0) {df <- df %>% filter(Datum >= "2020-02-15")}
      if (df$infected[df$Datum == "2020-03-01"]==0) {df <- df %>% filter(Datum >= "2020-03-01")}
      # Zeilennzummer und kürzere Namen
      df <-df %>%
        mutate(id = row_number()) %>% 
        rename(i = infected)
      
      wj = length(arpweight())
      # berechne active cases
      for (k in df$id){
        #  print(k)
        if (k + wj < dim(df)[1])   {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * arpweight()),rep(df$i[k] * 0,dim(df)[1]-(k+wj)))}
        if (k + wj == dim(df)[1])  {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * arpweight()))}
        if (k + wj > dim(df)[1] & k < dim(df)[1])   {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * arpweight()[1:(dim(df)[1]-k)]))}
        if (k == dim(df)[1])   {df[paste0("col",k)] <- rep(0,k)}
      }
      
      # Datensatz mit infected, actve und recovered Zahlen
      df<-df %>% mutate(a = select(., contains("col")) %>% rowSums() + i) %>%
        mutate(i = cumsum(i),
               r = i-a,
               rp = r/i,
               county = LK) %>%
        select(county,Datum,i,a,r,rp) %>%
        filter(Datum >= today()-10)
      if (teller == 1){
        dfLK <- df
      } else {
        dfLK <- bind_rows(dfLK,df)
      }
    }
  
  })
  
  # ----------------------------------------------------------------------------------------------------
  # Datensatz erstellen (alle SK/LK, letzte 10 Tage)
  # ----------------------------------------------------------------------------------------------------
  
  # für welchses Datum soll geplottet werden
  dfplot <- reactive({
    # Auf Datum filtern
    dfplot <- dfarpLK() %>% filter(Datum  == arpdat())
    #Datensatz mit Geographie für Plot mergen
    dfgeo <-   merge(geo_lk,dfplot) #%>%
  })
  # ----------------------------------------------------------------------------------------------------
  # Karte zeichnen
  # ----------------------------------------------------------------------------------------------------
  output$arpPlot <- renderPlot({
    print(arpfilter())
    if (arpfilter() == "Anteil recovered") {
      ggplot()+
        geom_sf(data = dfgeo,aes(fill = rp)) +
        scale_fill_gradient2(low = 'red',mid = "orange", high = 'yellowgreen',midpoint = 0.5) +
        #scale_fill_manual(values = VDZcol) +
        theme_void() +
        theme(legend.text = element_text(size = 12),
              title = element_text(size = 10,face = "bold"),
              plot.caption = element_text(hjust=0, size=rel(1.2))
        ) #+
      #labs(caption = footnote(),
      #     fill = "Verdopplungszeit\nin den SK und LK\nDeutschlands"
      #)
    }
    
    if (arpfilter() == "active cases") {
        ggplot()+
        geom_sf(data = dfgeo,aes(fill = a)) +
        scale_fill_gradient2(low = 'green',mid = "orange", high = 'darkred',midpoint = 100) +
        #scale_fill_manual(values = VDZcol) +
        theme_void() +
        theme(legend.text = element_text(size = 12),
              title = element_text(size = 10,face = "bold"),
              plot.caption = element_text(hjust=0, size=rel(1.2))
        ) #+
    }
    
  })

  # ----------------------------------------------------------------------------------------------------
  # Tabelle Situation Bundesländer
  # ----------------------------------------------------------------------------------------------------
    # für welchses Datum soll geplottet werden
    dfarptab <- reactive({
      dfgeoBL <-   merge(geo_lk,dfarpLK()) %>%
        select(Datum,Bundesland = NAME_1,i,a,r,rp) %>%
        select(-geometry) %>%
        group_by(Bundesland) %>%
        summarise_if(is.numeric,sum)
      print(dfgeoBL)
      return(dfgeoBL)
    })
    
    output$iartabBL <- renderTable(dfarptab(),align = 'llrrrr')
    
    # tabPanel "Intensivbetten" -----------------------------------------------
  # ----------------------------------------------------------------------------------------------------
  # Einstellungen auslesen für später
  # ----------------------------------------------------------------------------------------------------
  
  
  # Übergaben - ausgewähltes Datum
  immapregdat <- renderText({
    format(as.Date(input$imregdat,format= ,"%d. %B %Y"),"%Y-%m-%d")
    #format(as.Date(input$imdatum),"%Y-%b-%d")
    #testdata<-format(Sys.Date()-1,"%d. %B %Y")
    #format(as.Date(testdata,format= ,"%d. %B %Y"),"%Y-%m-%d")
  })
  
  
  # Angenommener Anteil intensivmedizinischer Patienten
  impatregserious<-reactive({
    max(input$imregPats) /100
  })
  
  
  # ----------------------------------------------------------------------------------------------------
  # Datensatz für Regionen Thüringen erstellen
  # ----------------------------------------------------------------------------------------------------
  
  
  Ibedsweight <- eventReactive(input$imregIARmodell,{
    if (input$imregIARmodell == "by PPW") {weight <- PPWweights;length(weight)} # PPW Jena
    if (input$imregIARmodell == "by NPW") {weight <- NPWweights;length(weight)} # NPW Jena, 7 Tage bis first recovery
    if (input$imregIARmodell == "by NPW optimistic") {weight <- NPWweightsopt;length(weight)} # NPW Jena, 5 Tage bis first recovery
    return(weight)
    
  })
  #weight by Npwvals
  #wj = length(weight)
  print(Ibedsweight)
  
  dfregTH <- reactive({
    
    #Datensatz für LK
    df <- dftsI_TH  %>% ungroup() %>%
      filter(Region == input$imregwahl)  %>%
      #filter(Region == "ost")  %>%
      mutate(id = row_number()) %>% 
      rename(i = infected)
    #print(head(df))
    wj = length(Ibedsweight())
    # berechne active cases
    for (k in df$id){
      #  print(k)
      if (k + wj < dim(df)[1])                    {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * Ibedsweight()),rep(df$i[k] * 0,dim(df)[1]-(k+wj)))}
      if (k + wj == dim(df)[1])                   {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * Ibedsweight()))}
      if (k + wj > dim(df)[1] & k < dim(df)[1])   {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * Ibedsweight()[1:(dim(df)[1]-k)]))}
      if (k == dim(df)[1])                        {df[paste0("col",k)] <-   rep(0,k)}
    }
    # Datensatz mit infected, actve und recovered Zahlen
    df <- df %>% ungroup() %>%
      mutate(a = select(., contains("col") ) %>% rowSums() + i) %>%
      mutate(i = cumsum(i),
             r = i-a) %>%
      select(Datum, i,a,r) 
    
    return(df)
  })  
  
  dfregscTH <- reactive({
    dfregTH() %>% 
      mutate(sc = if_else((i*impatregserious())>0.1,ceiling(i*impatregserious()),floor(i*impatregserious())))
    
  })
  # ----------------------------------------------------------------------------------------------------
  # Tabelle der Daten der letzten 7 Tage (imtabregpats)
  # ----------------------------------------------------------------------------------------------------
  tab <- reactive({
    seldat <- max(dfregscTH()$Datum)-6
    #print(seldat)
    tab <- dfregscTH() %>%
      filter(Datum >= as.Date(seldat)) %>%
      mutate(i = format(round(i), nsmall = 0),
             a = format(round(a), nsmall = 0),
             r = format(round(r), nsmall = 0),
             sc= format(round(sc),nsmall = 0),
             Datum = as.Date(Datum)) %>%
      select(Datum,infected = i, active = a, recovered = r,seriouscases = sc) %>%
      mutate(Datum = format(Datum,"%d. %B %Y")) %>%
      pivot_longer(-Datum,names_to = "status",values_to = "nsum") %>%
      pivot_wider(names_from = "Datum",values_from = "nsum")
    return(tab)
  })
  
  output$imtabregpats <- renderTable(tab(),align = 'lrrrrrrr')
  
  
  # ----------------------------------------------------------------------------------------------------
  # Berechne Daten der nächsten 7 Tage (PROGNOSE) (imtabregproj)
  # ----------------------------------------------------------------------------------------------------
  
  yA   <- reactive({
    yA <- dfregTH() %>% select(Datum,i) %>% pivot_wider(names_from = "Datum",values_from = "i") 
  })
  
  
  dfregscTHproj <- reactive({
    maxdat <- as.Date(max(dfregscTH()$Datum))
    yA   <- as.numeric(yA()[1,])
    dates <- as.Date(names(yA()))
    proj <- projSimple(yA,dates,inWindow=7)
    dfproj<- cbind.data.frame(proj$x,proj$y) %>%
      rename(Datum = 'proj$x') %>%
      mutate(projsc = if_else((fit*impatregserious())>0.1,ceiling(fit*impatregserious()),floor(fit*impatregserious())),
             projscl = if_else((lwr*impatregserious())>0.1,ceiling(lwr*impatregserious()),floor(lwr*impatregserious())),
             projscu = if_else((upr*impatregserious())>0.1,ceiling(upr*impatregserious()),floor(upr*impatregserious()))
      ) %>%
      filter(Datum > maxdat)
    return(dfproj)
  })
  # ----------------------------------------------------------------------------------------------------
  # Tabelle der Daten der nächsten 7 Tage (PROGNOSE) (imtabregproj)
  # ----------------------------------------------------------------------------------------------------
  
  tabproj <- reactive({
    tabproj<-dfregscTHproj() %>%
      mutate(projsc = format(round(projsc), nsmall = 0),
             projscl= format(round(projscl),nsmall = 0),
             projscu= format(round(projscu),nsmall = 0),
             Datum = as.Date(Datum)) %>%
      select(Datum,seriuoscases = projsc, upper = projscu, lower = projscl) %>%
      mutate(Datum = format(Datum,"%d. %B %Y")) %>%
      pivot_longer(-Datum,names_to = "status",values_to = "nsum") %>%
      pivot_wider(names_from = "Datum",values_from = "nsum")
    return(tabproj)
    
  })
  
  output$imtabregproj <- renderTable(tabproj(),align = 'lrrrrrrr')
  
  
  
  # ----------------------------------------------------------------------------------------------------
  # Status der Patienten (impatregplot)
  # ----------------------------------------------------------------------------------------------------
  
  
  # impatregplot
  IARregfootnote <- renderText({
    paste0("* geschätzt (",input$imregIARmodell," Szenario)")
  })
  
  output$impatstatregPlot <- renderPlot({
    
    dfplot <- dfregTH() %>%
      pivot_longer(-Datum,names_to = "status",values_to = "nsum") %>%
      mutate(status = factor(status,levels = c("i","a","r"))
      )
    
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
            panel.grid.major.x = element_line(colour="red", size=0.75)
      ) + 
      #scale_y_continuous(breaks = seq(0,200,25),labels =seq(0,200,25)
      #                   ) +
      labs(y = "absolute Anzahl",x = "Datum") +
      scale_x_date(breaks = seq(mindat,maxdat,by="week"),
                   date_labels = "%b %d", minor_breaks = "1 day"
      ) +
      annotate("rect", xmin = mindat-1, xmax = maxdat+2, ymin = -10, ymax = -4,fill = "white") +
      annotate("text",y=-6,x = c(seq(mindat,maxdat,"day")),
               label = c(rep(c("S","M","D","M","D","F","S"),poltweeks),"S"), 
               color =c(rep(c("red",rep("blue",5),"orange"),poltweeks),"red")
      ) + 
      labs(caption = IARregfootnote()
      )
  })
  
  
  # ----------------------------------------------------------------------------------------------------
  # GRAFIK Intensivbettenprognose THÜRINGEN (impatregPlot)
  # ----------------------------------------------------------------------------------------------------
  # Datensatz erstellen
  #input == "SK Jena"
  #output$result <- renderPrint(impatregserious())
  #output$result2 <- renderPrint(max(input$imPats))
  
  
  
  IntBeedsreg <- reactive({
    sdTH_Ibedsreg$Intensivbetten_Beatmung[sdTH_Ibedsreg$Region == input$imregwahl]
  })
  
  output$impatregPlot <- renderPlot({
    # Datensatz der gewählten Region
    # Prognose 10 Tage
    
    # Grafik attribute 
    Intbeds<-IntBeedsreg()
    yscmax<-max(Intbeds,max(dfregscTHproj()$projscu))
    yInblab <- if_else(Intbeds < yscmax,yscmax,Intbeds)
    anottxt <- paste0("Intensivmedizinische Betten in Th\u00FCringen ",input$imregwahl," (N = ",Intbeds,")")
    mindat <- ceiling_date(as.Date(min(dfregscTH()$Datum)),unit = "week")
    maxdat <- floor_date(as.Date(max(dfregscTHproj()$Datum)),unit = "week")
    #print("Grafik vorbereitet")
    # Grafik (ggplot)
    ggplot(data = dfregscTH(),aes(x = as.Date(Datum),y = sc)) + #, color = county, fill = county)) +
      geom_hline(yintercept = Intbeds,size = 2,linetype="solid", color = "black") +
      geom_label(x = mindat-2, y = yInblab+0.05*yInblab,label=anottxt,color = "black",fill = "white",hjust =0,label.padding = unit(0.4, "lines"),size = 5) +
      # 'REALE' DATEN
      geom_bar(stat="identity",width=.5) + #, position='dodge') +
      #pROGNOSEDATEN
      geom_crossbar(data = dfregscTHproj(),aes(x = as.Date(Datum),y = projsc,ymin = projscl, ymax = projscu,alpha = 0.5), width = 0.5) +
      geom_point(data = dfregscTHproj(),aes(x = as.Date(Datum),y = projsc),size = 3) + #, position='dodge') +
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
  
  
  # ----------------------------------------------------------------------------------------------------
  # GRAFIK KARTE THÜRINGEN (impapregplot)
  # ----------------------------------------------------------------------------------------------------
  
  #  source("getregmapTHdata.R") # enthält das folgende Makro
  #  output$immapregPlot <- renderPlot({
  #
  #
  #  dfplotgeo <- mkr.getmapTHregiondata(df = dfregTH(),impatser = impatregserious(),immapdat =  immapdatum() )
  #    
  #  # Erstelle Mapplot
  #    ggplot()+
  #      geom_sf(data = dfplotgeo,aes(fill = warnIntstat)) +
  #      scale_fill_manual(values = Intbedcol) +
  #      theme_void() +
  #      theme(#legend.position = "bottom",
  #            legend.text = element_text(size = 12),
  #            #title.text = element_text(size = 20),
  #            title = element_text(size = 16,face = "bold")
  #      ) +
  #      #guides(fill = guide_legend(title.position="top", title.hjust = 0,nrow=2,byrow=TRUE)      ) +
  #      labs(#title = "Landkreise mit potentiell zu wenigen Intensivmedizinische Betten",
  #        title = paste0("Prognose für den ",format(as.Date(immapdatum()),"%d. %B %Y")),
  #        fill = "Bedarf an\nIntensivbetten\n für CORONA"
  #      )
  #  })
  
  
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
    #print(pDatGR)
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

})
