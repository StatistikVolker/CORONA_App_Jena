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
##### Intensiv Betten ##### 
tabPanel("Intensivbetten NEU",
         # Sidebar
         sidebarLayout(
           sidebarPanel(
             titlePanel("Intensivbettenbedarf in Thüringen nach Region"),
             h4("Thüringenkarte"),
             dateInput("imregdat", "Wahle ein Datum", value = lastdate,language = "de",format = "d. MM yyyy"),
             h4("Anteil intensivmedizinisch betreuter CORONA Patienten"),
             p("80% der CORONA Infektionen verlaufen milde. Nur 20% der CORONA Infektionen verlaufen mit ausgeprägten Symptomen und werden evtl. stationär aufgenommen."),
             p("Zwischen 2% und 10% der patienten benötien intensivmedizinische Betreuung. Je nachdem wie hoch dieser Anteil ist, kann es knapp werden mit der Versorgung der Patienten."),
             p("Stellen Sie hier ein, mit welchem Anteil intensivmedizinisch betreuter CORONA Patienten Sie rechnen und sehen sie dann in der Grafik, ob die Intensivpflegebetten im LK ausreichen."),
             #p("Eie sehen zudem eine Prognise für die nächsten 10 Tage."),
             sliderInput("imregPats", "Erwarteter Anteil (in %)",min = 0, max = 20, value = 2.5,step = 0.5),
             h4("Abschätzung Recovered"),
             selectInput(inputId = "imregIARmodell",
                         label = "Abschätzung der Reovered Zahlen",
                         choices = c('pessimistisch' = "pessimistisches",
                                     'mittig' = "mittleres",
                                     'optimistisch' = "optimistisches")
             ),
             h4("Potentieller Bedarf im einzelnen Stadt-/ Landkreis"),
             selectInput(inputId = "imregwahl",
                         label = "Wähle Region aus Thüringen",
                         choices = c('Thüringen nord' = "nord",
                                     'Thüringen südwest' = "s\u00Fcdwest",
                                     'Thüringen Mitte' = "mitte",
                                     'Thüringen ost' = "ost",
                         ),
                         selected = "ost"),
             h4("Methodik:"),
             p("")
           ),
           # Show a plot of the generated distribution
           mainPanel(
             img(src='LichtstadtJenaneu.png', align = "right",height = 40),
             #verbatimTextOutput(outputId = "result"),
             #verbatimTextOutput(outputId = "result2"),
             h4("Landkreise mit potentiell zu wenigen Intensivmedizinische Betten"),
             p("Die Karte zeigt für Thüringen, welche Landkreise erwartbar zu wenig Intensivbeten für CORONA Patienten haben. Die Fabre zeigt den Intensivbettenbedarf an."),
             p("Werte unter 1 können bewältigt werden. Werte über 1 zeigen einem Mehrbedarf im Landkreis an."),
             p("Beachte: im LK Sömmerda gibt es kein Krankenhaus. Die Patienten werden von den Krankenhäusern in den umliegenden Landkreisen mitversorgt."),
             plotOutput("immapregPlot",width = "100%",height = "600px"),
             hr(),
             h4("Detailierte situation in einer ausgewählten Region"),
             plotOutput("impatregPlot"),
             hr(),
             h4("Status der Patienten in ausgewählter Region"),
             plotOutput("impatstatregPlot"),
             hr(),
             h4("Einteilung der Thüringer Krankenhaäuser nach Region und Typ Region"),
             img(src='Krankenhausstandorte nach Regionen in TH.jpg'),
             hr(),
             p("")
           )
         )
),
##### Intensiv Betten ##### 
tabPanel("Intensivbetten alt",
         # Sidebar
         sidebarLayout(
           sidebarPanel(
               titlePanel("Intensivbettenbedarf im LK"),
               h4("Thüringenkarte"),
               dateInput("imdatum", "Wahle ein Datum", value = lastdate,language = "de",format = "d. MM yyyy"),
               h4("Anteil intensivmedizinisch betreuter CORONA Patienten"),
               p("80% der CORONA Infektionen verlaufen milde. Nur 20% der CORONA Infektionen verlaufen mit ausgeprägten Symptomen und werden evtl. stationär aufgenommen."),
               p("Zwischen 2% und 10% der patienten benötien intensivmedizinische Betreuung. Je nachdem wie hoch dieser Anteil ist, kann es knapp werden mit der Versorgung der Patienten."),
               p("Stellen Sie hier ein, mit welchem Anteil intensivmedizinisch betreuter CORONA Patienten Sie rechnen und sehen sie dann in der Grafik, ob die Intensivpflegebetten im LK ausreichen."),
               #p("Eie sehen zudem eine Prognise für die nächsten 10 Tage."),
               sliderInput("imPats", "Erwarteter Anteil (in %)",min = 0, max = 10, value = 2.5,step = 0.5),
               h4("Potentieller Bedarf im einzelnen Stadt-/ Landkreis"),
               selectInput(inputId = "impatLK",
                           label = "Wähle Stadt-/ Landkreis",
                           choices = list('Thüringen' = LK_TH#,
                                          #'Baden-Württemberg' = LK_BW,
                                          #'Bayern' = LK_BY,
                                          #'Brandenburg' = LK_BB,
                                          #'Hessen' = LK_HE,
                                          #'Mecklenburg-Vorpommern' = LK_MV,
                                          #'Niedersachsen' = LK_NS,
                                          #'Nordrhein-Westfalen' = LK_NRW,
                                          #'Rheinland-Pfalz' = LK_RP,
                                          #'Saarland' = LK_SL,
                                          #'Sachsen' = LK_SN,
                                          #'Sachsen-Anhalt' = LK_SA,
                                          #'Schleswig-Holstein' = LK_SH,
                                          #'Stadtstaaten' = LK_SS
                                          #'Berlin' = LK_BE,
                                          #'Bremen' = LK_HB,
                                          #'Hamburg' = LK_HH,
                           ),
                           selected = "SK Jena"),
               h4("Methodik:"),
               p("Die Intensivbetten für die Thüringer Landkreise sind eine Schätzung aus den Daten des Krankenhausatlases von 2017 des Thürigner Landesamtes für Statistik (TLS)."),
               p("Stand 31.12.2017 gibt es in Thüringen 730 Intensivmedizinische Betten (neuere Daten liegen noch nicht vor, können aber gerne an statistik@jena.de gesendet werden)"),
               p("Die Jenaer Statistik schätzt insgesamt 733 Intensivbetten und verteilt diese auf die Landkreise. Die Schätzmethode ist die folgende:"),
               p("1. Das TLS stellt für jeden Landkreis die Anzahl der Krankenhausbetten zu Verfügung. Pro Landkreis ist zudem angegeben, wie groß die einzelnen Kliniken im Landkreis sind."),
               p("2. Ebenfalls seitens des TLS ist pro Klinikgröße die Anzahl der Betten und Intensivbetten angegeben."),
               p("3. Für jede Klinikgröße ergibt sich dadurch ein Schätzwert für den Anteil an intensivmedizinischen Betten (SWIB)."),
               p("4. Für die einzelnen Landkreise wird geschaut, wieviele verschiedene KLinikgrößentypen (KGTs) im LK sind."),
               p("5. Der Landkreisspezifische Intensivbettenanteil (LKIBA) berechnet sich dann aus der Summe der SWIB im Anteil geteilt durch die Anzahl der KGTs."),
               p("6. Die Landkreisspezifischen Klinikbetten mit intensivmedizinischer Versorgung ergeben sich dann aus der Bettenanzahl im LK mal dem LKIBA.")
           ),
           # Show a plot of the generated distribution
           mainPanel(
             img(src='LichtstadtJenaneu.png', align = "right",height = 40),
             #verbatimTextOutput(outputId = "result"),
             #verbatimTextOutput(outputId = "result2"),
             h4("Landkreise mit potentiell zu wenigen Intensivmedizinische Betten"),
             p("Die Karte zeigt für Thüringen, welche Landkreise erwartbar zu wenig Intensivbeten für CORONA Patienten haben. Die Fabre zeigt den Intensivbettenbedarf an."),
             p("Werte unter 1 können bewältigt werden. Werte über 1 zeigen einem Mehrbedarf im Landkreis an."),
             p("Beachte: im LK Sömmerda gibt es kein Krankenhaus. Die Patienten werden von den Krankenhäusern in den umliegenden Landkreisen mitversorgt."),
             plotOutput("immapPlot",width = "100%",height = "600px"),
             hr(),
             h4("Detailierte situation in einem ausgewählten Landkreis"),
             plotOutput("impatPlot"),
             hr(),
             p("")
             )
           )
         )
)
)

