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
    # Infected, Active recovered in one Plot
    tabPanel("Aktulle Fallzahlen pro LK",
             # Sidebar 
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Stadt-/ Landkreis"),
                 selectInput(inputId = "IARLK",
                             label = "Wähle Stadt-/ Landkreis",
                             choices = list('Thüringen' = LK_TH,
                                            'Städtevergleich Jena' = Jvgl_SK,
                                            'Städtevergleich Sachsen' = city_SN,
                                            'ausgewählte weitere Landkreise' = Jvgl_LK
                                            
                             ),
                             selected = "SK Jena"),
                 h4("Abschätzung Recovered"),
                 selectInput(inputId = "IARmodell",
                             label = "Abschätzung der Reovered Zahlen",
                             choices = c('pessimistisch' = "pessimistisches",
                                         'mittig' = "mittleres",
                                         'optimistisch' = "optimistisches")
                 ),
                 h4("Fallzahlen der letzten 5 Tage:"),
                 tableOutput(outputId = "tablast5days"),
                 hr(),
                 p("Die Abschätzung der active bzw. recovered-Patienten erfolgte wie folgt:"),
                 p("1. Aus den vorliegenden des Jenaer Gesundheitsamtes wurden die Pärdiktiven Werte zur Überwindung der COVID19 Erkrankung nach 7,14,21 und 28 Tagen berechnet. Dabei ist zu beachten, dass die Datengrundlage auf Grund der geringen Fallzahl dünne ist. So berechnen sich die PPW us derzeit N = 20, die NPW aus N = 95 Fällen."),
                 p("2. Die Zahl der active Erkrankten berechnet sich daraus wie folgt:"),
                 p("2.a Ausgehend vom aktuellen Tag werden die prädiktiven Werte rückwirkend als Stfenfunktion angewendet"),
                 p("2.b Die ersten 7 Tage nach Diagnose wird davon ausgegangen, dass die COVID-19 beteht, für die jeweils folgenden 7 Tage werden für die active Erkranten aus den NPW bzw PPW errechnet."),
                 p("2.c Ab Tag 56 (= (Wochen nach Infektion) wird von der vollständigen Genesung ausgegangen"),
                 p("3. Es wurden drei Szenarien gerechnet:"),
                 p("3.a pessimistisch: floor(infected * ppwvals)"),
                 p("3.b realistisch: floor(infected - (infected * npwvals))"),
                 p("3.c optimistisch: floor(infected - (infected * npwvals))/1.2"),
                 p("Die pessimistische Variante unterschätzt auf Grund der verwendeten Stufenfunktion die Heilungsrate, die realistische Variante hat ein ähnliches Problem, kommt aber zu positiveren Ergebnissen, da mit dem stabileren NPW aus den Jenaer Daten gerechnet wird. Die Optimistische Variante wendet einen einfachen Korrektur Faktor auf die relaistische Variante an. Dieser errechnet sich aus dem Abgleich der geschätzten daten mit den veröffentlicheten und bekannten Danten aus Jena bzw. Bundesweit (letztere allerdings auch nichr geschätzt)"),
                 p("Dieses Vorgehen ist notwendig, da die vom RKI zu verfügung gestellten Daten unverständlicherweise keinen Datumstempel bezüglich recovered (und auch Tod) mitliefern."), 
               ),
               mainPanel(  
                 img(src='LichtstadtJenaneu.png', align = "right",height = 40),
                 plotOutput("IARplot")#,width = "100%",height = "1000px")
               )
             )
    ),
    
    #### Verdopplungszeit mit 10 Tages vorhersage ####
    tabPanel("Verdopplungsate",
             # Sidebar 
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Berechnung der Verdopplungszeit"),
                 selectInput(inputId = "vdzdays",
                             label = "Berechne die Verdopplungszeit bezogen auf die letzen",
                             choices = c('... 10 Tage' = "10",
                                         '... 7 Tage' = "07",
                                         '... 5 Tage' = "05")
                 ),
                 h4("Datumsauswahl"),
                 dateInput("vdzdatum", "Wahle ein Datum", value = lastdate,language = "de",format = "d. MM yyyy"),
                 h4("Methodik"),
                 p("Hier ein wenig Prosa für die Methodik")
               ),
               mainPanel(  
                 img(src='LichtstadtJenaneu.png', align = "right",height = 40),
                 plotOutput("VDZPlot",width = "100%",height = "1000px")
               )
             )
    ),
##### 10-day forecast #####             
      tabPanel("10 Tages Vorhersage",
             # Sidebar 
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Stadt-/ Landkreis"),
                 selectInput(inputId = "LKfinder",
                             label = "Wähle Stadt-/ Landkreis",
                             choices = list('Thüringen' = LK_TH,
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
                                            ##'Berlin' = LK_BE,
                                            ##'Bremen' = LK_HB,
                                            ##'Hamburg' = LK_HH,
                                            'Städtevergleich Jena' = Jvgl_SK,
                                            'Städtevergleich Sachsen' = city_SN,
                                            'ausgewählte weitere Landkreise' = Jvgl_LK
                                            
                                            ),
                             selected = "SK Jena"),
                 h4("Absolute Anzahl:"),
                 tableOutput(outputId = "rawStats"),
                 h3("Infizierte:"),
                 tableOutput(outputId = "tablePredConf"),
                 h4("Verdopplungsrate:"),
                 tableOutput(outputId = "dtimetab"),
                 titlePanel("Aufdeckungs- indikator"),
                 p("sehr grober Überschlag",style = "color:red"),
                 h4("Geschätzter Anteil entdeckter Fälle:"),
                 textOutput(outputId = "detRate"),
                 h4("Abschätzung der tatsächlichen Fallzahl:"),
                 textOutput(outputId = "tablePredTrue"),
                 hr(),
                 p("Take this last number with a grain of salt; it is rough.  But low detection indicates that there are many more deaths in the country than there should be given reported case numbers (so there must be more cases than are reported)."),
                 p("Active cases are total number of infections minus deaths and recoveries."),
                 p("For more information, see", 
                    a("here.", href = "https://blphillipsresearch.wordpress.com/2020/03/12/coronavirus-forecast/", target="_blank"))
                 ),
               # Show a plot of the generated distribution
               mainPanel(
                 img(src='LichtstadtJenaneu.png', align = "right",height = 40),
                 #img(src='hashtag_controlling_statistik.png', align = "right",height = 30),
                 #verbatimTextOutput(outputId = "result1.1"),
                 
                 plotOutput("rawPlot"),
                 plotOutput("logPlot")
                 )
               )
             ),
##### Growth Rate ##### 
      tabPanel("Wachstumsrate und 'Flattening the Curve'-Index",
               # Sidebar
               sidebarLayout(
                 sidebarPanel(
                   titlePanel("Auswahl der Stadt-/ Landkreise"),
                   #p("Nach Bundesländern geordnet, und alphabetisch sortiert (erst Landkreise (LK) dann Stadtkreise (SK))"),
                   checkboxGroupInput(inputId = 'cboxLK_TH',label = 'Thüringen',choices = LK_TH,selected = "SK Jena"),
                   #checkboxGroupInput(inputId = 'cboxLK_BW',label = 'Baden-Württemberg',choices = LK_BW),
                   #checkboxGroupInput(inputId = 'cboxLK_BY',label = 'Bayern',choices = LK_BY),
                   ##checkboxGroupInput(inputId = 'cboxLK_BE',label = 'Berlin',choices = LK_BE),
                   #checkboxGroupInput(inputId = 'cboxLK_BB',label = 'Brandenburg',choices = LK_BB),
                   ##checkboxGroupInput(inputId = 'cboxLK_HB',label = 'Bremen',choices = LK_HB),
                   ##checkboxGroupInput(inputId = 'cboxLK_HH',label = 'Hamburg',choices = LK_HH),
                   #checkboxGroupInput(inputId = 'cboxLK_HE',label = 'Hessen',choices = LK_HE),
                   #checkboxGroupInput(inputId = 'cboxLK_MV',label = 'Mecklenburg-Vorpommern',choices = LK_MV),
                   #checkboxGroupInput(inputId = 'cboxLK_NS',label = 'Niedersachsen',choices = LK_NS),
                   #checkboxGroupInput(inputId = 'cboxLK_NRW',label = 'Nordrhein-Westfalen',choices = LK_NRW),
                   #checkboxGroupInput(inputId = 'cboxLK_RP',label = 'Rheinland-Pfalz',choices = LK_RP),
                   #checkboxGroupInput(inputId = 'cboxLK_SL',label = 'Saarland',choices = LK_SL),
                   #checkboxGroupInput(inputId = 'cboxLK_SN',label = 'Sachsen',choices = LK_SN),
                   #checkboxGroupInput(inputId = 'cboxLK_SA',label = 'Sachsen-Anhalt',choices = LK_SA),
                   #checkboxGroupInput(inputId = 'cboxLK_SH',label = 'Schleswig-Holstein',choices = LK_SH),
                   #checkboxGroupInput(inputId = 'cboxLK_SS',label = 'Stadtstaaten',choices = LK_SS)
                   checkboxGroupInput(inputId = 'cboxJvgl_SK',label = 'Städtevergleich Jena',choices = Jvgl_SK),
                   checkboxGroupInput(inputId = 'cboxcity_SN',label = 'Städtevergleich Sachsen',choices = city_SN),
                   checkboxGroupInput(inputId = 'cboxJvgl_LK',label = 'ausgewählte weitere Landkreise',choices = Jvgl_LK)
                   
                   ),
                 mainPanel(
                   #img(src='hashtag_controlling_statistik.png', align = "right",height = 50),
                   img(src='LichtstadtJenaneu.png', align = "right",height = 40),
                   h3("Wachstumsrate"),
                   p("Es werden die Neuinfizierten der letzten 10 Tage dargestellt. Poitive Balken bedeuten mehr Neinfizierte im Vergleich zum Vortag, negative Balken weniger."),
                   p("Ziel ist es also möglichst viele Negative Balken zu sehen."),
                   p("Beachte: Folgen auf Tage mit geringem oder Null-Wachtum, Tage mit starken poitiven Anstieg, werden diese Tage in die folgenden Tage aggregiert übernommen."),
                   plotOutput("growthRate"),
                   hr(),
                   h3("'Flattening the Curve'-Index"),
                   p("Die Kurve zeigt auf, wie gut der jeweilie Stadt-/ Landkreis zu jedem Zeitpunkt in der Lage ist, die pandemische Kurve abzuflachen"),
                   p("Negative Werte weisen auf ein langsameres Wachstum zu diesem Zeitpunkt hin."),
                   plotOutput("cfi"),
                   p("Weitereführende Angaben:", 
                        a("here.", href = "https://blphillipsresearch.wordpress.com/2020/03/12/coronavirus-forecast/", target="_blank"))
                   )
                 )
               ),
##### Intensiv Betten ##### 
tabPanel("Intensivbetten NEU",
         # Sidebar
         sidebarLayout(
           sidebarPanel(
             titlePanel("Intensivbettenbedarf in Thüringen nach Region"),
             h4("Anteil intensivmedizinisch betreuter CORONA Patienten"),
             p("80% der CORONA Infektionen verlaufen milde. Nur 20% der CORONA Infektionen verlaufen mit ausgeprägten Symptomen und werden evtl. stationär aufgenommen."),
             p("Zwischen 2% und 10% der patienten benötien intensivmedizinische Betreuung. Je nachdem wie hoch dieser Anteil ist, kann es knapp werden mit der Versorgung der Patienten."),
             p("Stellen Sie hier ein, mit welchem Anteil intensivmedizinisch betreuter CORONA Patienten Sie rechnen und sehen sie dann in der Grafik, ob die Intensivpflegebetten im LK ausreichen."),
             #p("Eie sehen zudem eine Prognise für die nächsten 10 Tage."),
             sliderInput("imPats", "Erwarteter Anteil (in %)",min = 0, max = 20, value = 2.5,step = 0.5),
             h4("Potentieller Bedarf im einzelnen Stadt-/ Landkreis"),
             selectInput(inputId = "impatregion",
                         label = "Wähle REgion aus Thüringen",
                         choices = list('Thüringen' = LK_TH
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
             plotOutput("immapPlot",width = "100%",height = "600px"),
             hr(),
             h4("Detailierte situation in einem ausgewählten Landkreis"),
             plotOutput("impatPlot"),
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
),




             tabPanel("Infektionsrate pro 100.000 Einwohner",
                # Sidebar
                sidebarLayout(
                  sidebarPanel(
                    titlePanel("Auswahl der Stadt-/ Landkreise"),
                    #p("Nach Bundesländern geordnet, und alphabetisch sortiert (erst Landkreise (LK) dann Stadtkreise (SK))"),
                    checkboxGroupInput(inputId = 'cbox2LK_TH',label = 'Thüringen',choices = LK_TH,selected = "SK Jena"),
                    checkboxGroupInput(inputId = 'cbox2LK_BW',label = 'Baden-Württemberg',choices = LK_BW),
                    checkboxGroupInput(inputId = 'cbox2LK_BY',label = 'Bayern',choices = LK_BY),
                    ##checkboxGroupInput(inputId = 'cbox2LK_BE',label = 'Berlin',choices = LK_BE),
                    checkboxGroupInput(inputId = 'cbox2LK_BB',label = 'Brandenburg',choices = LK_BB),
                    ##checkboxGroupInput(inputId = 'cbox2LK_HB',label = 'Bremen',choices = LK_HB),
                    ##checkboxGroupInput(inputId = 'cbox2LK_HH',label = 'Hamburg',choices = LK_HH),
                    checkboxGroupInput(inputId = 'cbox2LK_HE',label = 'Hessen',choices = LK_HE),
                    checkboxGroupInput(inputId = 'cbox2LK_MV',label = 'Mecklenburg-Vorpommern',choices = LK_MV),
                    checkboxGroupInput(inputId = 'cbox2LK_NS',label = 'Niedersachsen',choices = LK_NS),
                    checkboxGroupInput(inputId = 'cbox2LK_NRW',label = 'Nordrhein-Westfalen',choices = LK_NRW),
                    checkboxGroupInput(inputId = 'cbox2LK_RP',label = 'Rheinland-Pfalz',choices = LK_RP),
                    checkboxGroupInput(inputId = 'cbox2LK_SL',label = 'Saarland',choices = LK_SL),
                    checkboxGroupInput(inputId = 'cbox2LK_SN',label = 'Sachsen',choices = LK_SN),
                    checkboxGroupInput(inputId = 'cbox2LK_SA',label = 'Sachsen-Anhalt',choices = LK_SA),
                    checkboxGroupInput(inputId = 'cbox2LK_SH',label = 'Schleswig-Holstein',choices = LK_SH),
                    checkboxGroupInput(inputId = 'cbox2LK_SS',label = 'Stadtstaaten',choices = LK_SS),
                    #checkboxGroupInput(inputId = 'cboxJvgl_SK',label = 'Städtevergleich Jena',choices = Jvgl_SK),
                    #checkboxGroupInput(inputId = 'cboxcity_SN',label = 'Städtevergleich Sachsen',choices = city_SN),
                    #checkboxGroupInput(inputId = 'cboxJvgl_LK',label = 'ausgewählte weitere Landkreise',choices = Jvgl_LK)
                  ),
                  mainPanel(
                    img(src='LichtstadtJenaneu.png', align = "right",height = 40),
                    #img(src='hashtag_controlling_statistik.png', align = "right",height = 30),
                    h3("Infektionsrate pro 100.000 Einwohner als Heatmap ab Tag 1 der Infektion in der jeweiligen Region"),
                    p("Die Heartmap zeigt die Infizierten in der jeweiligen Region ab Tag 1 des Auftretens in der Region an."),
                    p("Dunklere Farben bedeuten eine gringere Inzidenzrate, hellere Farbnen eine höhere Inzidenraten."),
                    p("Stadt-/ Landkreise mit ähnlicheren Farbverläufen verhalten sich ahnlicher zu einander."),
                    #plotOutput("pir100000"),
                    h3("Tabelle"),
                    p("Die Tabelle zeigt den Verlauf der Infektionsrate. Ab dem ersten Auftreten wird dann wöchentlich die Inzidenzrate angegeben."),
                    p("Zusätzlich ist der tagesaktuelle Wert angegeben.")
                    #tableOutput("tir100000")
                    )
                  )
                )
  )
  )
  )

