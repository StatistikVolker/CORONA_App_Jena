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


# tabPanel "Intensivbetten" -----------------------------------------------
  # ----------------------------------------------------------------------------------------------------
  # GRAFIK KARTE THÜRINGEN (impapregplot)
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
  # Bereite Daten für Plot auf
  
  
  source("getregmapTHdata.R") # enthält das folgende Makro
  output$immapregPlot <- renderPlot({
    
    
 
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
  


  # ----------------------------------------------------------------------------------------------------
  # Status der Patienten (impatregplot)
  # ----------------------------------------------------------------------------------------------------
  
  
  # impatregplot
  IARregfootnote <- renderText({
    paste0("* geschätzt (",input$imregIARmodell," Szenario)")
  })
  
  weight <- eventReactive(input$imregIARmodell,{
    if (input$imregIARmodell == "pessimistisches") {weight <- c(rep(1,7),rep(0.810,7),rep(0.476,7),rep(0.238,7),rep(0.048,28));length(weight)} # PPW Jena
    if (input$imregIARmodell == "mittleres") {weight <- c(rep(1-0,7),rep(1-0.079,7),rep(1-0.416,7),rep(1-0.911,7),rep(1-0.980,28));length(weight)} # NPW Jena, 7 Tage bis first recovery
    if (input$imregIARmodell == "optimistisches") {weight <- c(rep(1-0,5),rep(1-0.079,7),rep(1-0.416,7),rep(1-0.911,7),rep(1-0.980,28));length(weight)} # NPW Jena, 5 Tage bis first recovery
    return(weight)
    
  })
  #weight by Npwvals
  #wj = length(weight)
  
  output$impatregplot <- renderPlot({
    #Datensatz für LK
    df <- dftsI_TH  %>% 
      filter(Region == input$imregwahl)  %>%
      #filter(Region == "ost")  %>%
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
      labs(caption = IARregfootnote())
    
    
  
  
  })
