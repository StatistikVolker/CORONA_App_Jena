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
  

# Deutschlandkarte: Active Cases ------------------------------------------

  # ----------------------------------------------------------------------------------------------------
  # Einstellungen auslesen für später
  # ----------------------------------------------------------------------------------------------------

  # Übergaben - ausgewähltes Datum
  rpzdat <- renderText({
    #print(input$rpzdat)
    format(as.Date(input$rpzdat,format="%d. %B %Y"),"%Y-%m-%d")
    #format(as.Date(input$rpzdat),"%Y-%m-%d")
  })
  
  

  # ----------------------------------------------------------------------------------------------------
  # Datensatz erstellen (alle SK/LK, letzte 10 Tage)
  # ----------------------------------------------------------------------------------------------------
  
  dfrpzLK <- reactive({
    teller <- 0
    for (LK in unique(dftsI$county)[1:5]) {
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
        rename(i = infected) %>%
        mutate(ip = i/sum(i)) #%>%
        #filter(datI > "2020-03-10"))

      T <- length(df$i)
      t_start <- seq(2, T-7) # starting at 2 as conditional on the past observations
      t_end <- t_start + 7 # adding 6 to get 7-day windows as bounds included in window
      
            
      rpzelement <- estimate_R(df$i, 
                               method="parametric_si",
                               config = make_config(list(
                               t_start = t_start,
                               t_end = t_end,
                               mean_si = mean(df$i), 
                               std_si = dsd(df$i)))
                               )
      
      print(names(rpzelement$R))
      #dftab <- rpzelement$R %>% select()
      
      
      
      if (teller == 1){
        dfLK <- df
      } else {
        dfLK <- bind_rows(dfLK,df)
      }
    }
    return(dfLK)
    
  })
  
  

  # ----------------------------------------------------------------------------------------------------
  # Datensatz erstellen (alle SK/LK, letzte 10 Tage)
  # ----------------------------------------------------------------------------------------------------
  
  # f?r welchses Datum soll geplottet werden
  #dfplot <- reactive({
    # Auf Datum filtern
    #dftemp <- dfrpzLK() %>% filter(Datum  == rpzdat())
    #Datensatz mit Geographie für Plot mergen
    #dfgeo <-   merge(geo_lk,dftemp) #%>%
    #st_geometry(dfgeo) <- NULL
    
    #return(dfgeo)
  #})
  # ----------------------------------------------------------------------------------------------------
  # Karte zeichnen
  # ----------------------------------------------------------------------------------------------------
  #  output$rpzplot <- renderPlot({
      #q <- ggplot()+
      #  geom_sf(data = dfplot,aes(fill = var)) +
      #  scale_fill_gradient2(low = colvec[1],mid = colvec[2], high = colvec[3],midpoint = as.numeric(colvec[4])) +
      #  #scale_fill_manual(values = VDZcol) +
      #  theme_void() +
      #  theme(legend.title = element_text(size = 20,face = "bold"),
      #        legend.text = element_text(size = 14),
      #        plot.title = element_text(size = 30,face = "bold"),
      #        plot.subtitle = element_text(size = 24,face = "bold"),
      #        plot.caption = element_text(hjust=0, size=rel(1.2))
      #  ) +
      #  labs(title = "Active Erkrankte in deutschen Landkreisen",
      #       subtitle = rpzplottit(),
      #       fill = "",
      #       caption = rpzfootnote())
      #q
      

  #})

  # ----------------------------------------------------------------------------------------------------
  # Tabelle Situation Bundesländer
  # ----------------------------------------------------------------------------------------------------
    # für welchses Datum soll geplottet werden
    dfrpztab <- reactive({
      dfgeoBL <-   merge(geo_lk,dfrpzLK()) %>%
        select(Bundesland = NAME_1,i,a,r) %>%
        #select(-geometry) %>%
        group_by(Bundesland) %>%
        summarise_if(is.numeric,sum) %>%
        mutate(rate_recovered = round(r/i*100,1),
               infected = round(i),
               active = round(a),
               recovered = round(r)) %>%
        arrange(desc(rate_recovered)) %>%
        select(Bundesland,infected,active,recovered,"rate recovered" = rate_recovered) #
      
      st_geometry(dfgeoBL) <- NULL
      
      #dfgeoBL <- dfgeoBL %>%
      #  arrange(desc(recovered))
      #print(dfgeoBL)
      return(dfgeoBL)
    })
    
    output$rpztabBL <- renderTable(dfrpztab())
    
    #output$rpztabtest <- renderTable(rpzstatus())
 
})
