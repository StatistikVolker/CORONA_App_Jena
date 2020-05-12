## ---------------------------
##
## Script name: ui.R
##
## Purpose of script:  user interface for CORONAinJena-App
##
## Author: Ben Phillips, Volker Holzendorf
##
## Date Created: 2020-03-12, 2020-03-29
##
## Email: statistik@jena.de
##
## ---------------------------
##
## Notes:
## Diese Datei übersetzt das Sktript von Ben Phillips ins Deutsche und 
## adaptiert dies für die Daten der Landkreise in Deutschland   
##
## --------------------------
## load up the packages we will need 
library(shiny)

## ---------------------------

## load up our functions into memory
## source files
lastdate <-Sys.Date()-1
source("functions.R")
source("getRKIdata.R")
source("getmapVDZdata.R")
source("getISbedsTHdata.R")

## ---------------------------
## ---------------------------
options(scipen=9999)

# Define UI
shinyUI(
  fluidPage(
  # Application title
  titlePanel("Coronavirus 10-Tages Vorhersage auf Landkreisebene in Deutschland"),
  p("Entwickelt von Volker Holzendorf, Stadtverwaltung Jena, Fachdienst Haushalt, Controlling und Organisationsentwicklung, Team Controlling und Statistik"),
  navbarPage(
    p(format(dates[length(dates)], "%d %b")),
    tabPanel("Deutschlandkarte: Reproduktionszahl",
             # Sidebar 
             sidebarLayout(
               sidebarPanel(
                 
                 titlePanel("Reproduktionszahl in Deutschlands Stadt- bzw. Landkreisen"),
                 h4("Deutschlandkarte als Heatmap der Reproduktionszahl der (bekannten) COVID 19 Patienten"),
                 p("Es kann bis zu 10 Tage in die Vergangenheit geschaut werden."),
                 dateInput("rpzdat", "Wahle ein Datum", value = lastdate,language = "de",format = "d. MM yyyy"),
             ),
               mainPanel(  
                 img(src='LichtstadtJenaneu.png', align = "right",height = 40),
                 plotOutput("rpzplot"),
                 hr(),
                 p(""),
                 h4("Reproduktionszahl in den Bundesländern:"),
                 tableOutput(outputId = "rpztabBL"),
                 hr()
               )
             )
    )
  )
)
)


