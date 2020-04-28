# getRKIdata.R

## ---------------------------
##
## Script name: getRKIdata.R
##
## Purpose of script: Zieht die tagesaktuellen  CORONA Daten von der RKI Seite und formatiert die Daten für Dashboard um
##
## Author: Ben Phillips, Volker Holzendorf
##
## Date Created: 2020-03-30
##
## Email: statistik@jena.de
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------
## load up the packages we will need:  

library("readxl")
library("tidyverse")
library("jsonlite")
library("lubridate")
## ---------------------------
## load up functions
source("functions.R")
lastdate <-Sys.Date()-2
#rm(dfRKII)
## ---------------------------

maxcases <-  0 # Nur LKs mit 50 Corona Fällen oder mehr werden ausgegeben!
inWindow <- 10 # Zeitspanne aus der geschätzt wird


LKcol <-read_excel("Daten/Landkreise plotcols 20200330.xlsx", sheet = "Landkreise") #%>% rename(GEN = 'Städte')

names(LKcol)
# Entweder Testdaten --------------------------------

load(file="Daten/Testdaten.Rdata")

#testLK <- c("LK Rems-Murr-Kreis","SK Erlangen","SK Lübeck","SK Braunschweig","SK Bremerhaven","LK Heinsberg","SK Kassel",
#            "SK Ludwigshafen","LK Sankt Wendel","SK Potsdam","LK Rostock","SK Leipzig","SK Halle","SK Jena")
#test_BL <-16
##names(RKIE_df)
#RKIE_df <- RKIE_df %>% filter(county %in% testLK  | (BL_ID ==16))

# oder livedaten -------------------------------------

### API Adresse aktuelle Einwohnerzahlen
##RKIE <- "https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.geojson"
####names(tsEold)
### Lade Aktuelle Daten mit allen Features
##RKIE_JSON<-fromJSON(RKIE)
##
### Aktueller Datensatz des RKI
##RKIE_df <- RKIE_JSON$features$properties
###names(RKIE_df)
##
###names(tsE)
### API Adresse der aktuellen CORONA Daten 
##tsRKI <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson"
##
### Lade Aktuelle Daten mit allen Features
##RKI_JSON<-fromJSON(tsRKI)
##
### Aktueller Datensatz des RKI
##RKI_df <- RKI_JSON$features$properties

##save(RKIE_df,RKI_df,file="Daten/Testdaten.Rdata")

## oder livedaten Ende -------------------------------------

##Stätde meta datensatz für Farben/ Einwohnerzahl usw.
tsE <-RKIE_df %>% select(GEN,BEZ,NUTS,EWZ,cases_per_100k,cases_per_population,BL,county) %>%
  left_join(LKcol)

# Aktuelle Fallzahlen und Todesfälle mit allen Subgruppen
tsIa<-mkr.makecoronaRKIallSubg(cases = AnzahlFall)
tsDa<-mkr.makecoronaRKIallSubg(cases = AnzahlTodesfall)

# Datensätze auf Landkreisebene
tsI<-mkr.makecoronaRKIgesLK(tsIa)
tsD<-mkr.makecoronaRKIgesLK(tsDa)

## get Date range
dCols<-dateCols(tsI)
dates<-as.Date(colnames(tsI)[dCols])

## Tidy up names
names(tsI)[!dCols] <- make.names(names(tsI)[!dCols])
names(tsD)[!dCols] <- make.names(names(tsD)[!dCols])
#names(tsT)[!dCols] <- make.names(names(tsT)[!dCols])

## add recovery lag -- assumes all cases recover at 22 days
matI<-as.matrix(tsI[, dCols])
matD<-as.matrix(tsD[, dCols])
matA<-matI-matD #remove deaths
matR <- cbind(matrix(0, nrow = nrow(matA), ncol = 22), matA[, -((ncol(matA)-21):ncol(matA))]) # recovered
matA <- matA - matR

tsA <- cbind.data.frame(Country.Region=pull(tsI[,!dCols]), matA) # active cases
names(tsA)

tsACountry <- countryAgg(tsA) # aggregated to country
tsACountry <- tsACountry[rev(order(tsACountry[[ncol(tsACountry)-1]])),] # ordered from most to least active cases

## Define menus
# get region names with 20 or more cases as of yesterday
ddNames <- tsACountry$Country[tsACountry[[ncol(tsACountry)-1]]>maxcases-1]

ddReg <- unique(ddNames)
names(ddReg) <- ddNames
#ddReg <- paste(ddReg, collapse = ", ") # menu specifier

# get region names with 20 or more cases as of yesterday
datcol <- as.character(lastdate)
tsACountry2 <- tsACountry %>% filter(!!as.name(datcol) > maxcases-1) %>%
  rename(county = Country) %>%
  left_join(tsE) %>%
  select(BL,BL_ID,county)

names(tsACountry2)
LK_BW <- tsACountry2 %>% filter(BL_ID == 8) %>% arrange(county) %>% pull(county) # BL_ID stattt Klaramen (BL) wegen Umlaut
LK_BY <- tsACountry2 %>% filter(BL == 'Bayern') %>% arrange(county) %>% pull(county)
LK_BE <- tsACountry2 %>% filter(BL == 'Berlin') %>% arrange(county) %>% pull(county)
LK_BB <- tsACountry2 %>% filter(BL == 'Brandenburg') %>% arrange(county) %>% pull(county)
LK_HB <- tsACountry2 %>% filter(BL == 'Bremen') %>% arrange(county) %>% pull(county)
LK_HH <- tsACountry2 %>% filter(BL == 'Hamburg') %>% arrange(county) %>% pull(county)
LK_HE <- tsACountry2 %>% filter(BL == 'Hessen') %>% arrange(county) %>% pull(county)
LK_MV <- tsACountry2 %>% filter(BL == 'Mecklenburg-Vorpommern') %>% arrange(county) %>% pull(county)
LK_NS <- tsACountry2 %>% filter(BL == 'Niedersachsen') %>% arrange(county) %>% pull(county)
LK_NRW <- tsACountry2 %>% filter(BL == 'Nordrhein-Westfalen') %>% arrange(county) %>% pull(county)
LK_RP <- tsACountry2 %>% filter(BL == 'Rheinland-Pfalz') %>% arrange(county) %>% pull(county)
LK_SL <- tsACountry2 %>% filter(BL == 'Saarland') %>% arrange(county) %>% pull(county)
LK_SN <- tsACountry2 %>% filter(BL == 'Sachsen') %>% arrange(county) %>% pull(county)
LK_SA <- tsACountry2 %>% filter(BL == 'Sachsen-Anhalt') %>% arrange(county) %>% pull(county)
LK_SH <- tsACountry2 %>% filter(BL == 'Schleswig-Holstein') %>% arrange(county) %>% pull(county)
LK_TH <- tsACountry2 %>% filter(BL_ID == 16) %>% arrange(county) %>% pull(county) # BL_ID stattt Klaramen (BL) wegen Umlaut
LK_SS <- tsACountry2 %>% filter(BL %in% c('Berlin','Bremen','Hamburg')) %>% arrange(county) %>% pull(county)

#ddReg <- LK_TH  
#ddNames <- tsACountry$Country[tsACountry[[ncol(tsACountry)-1]]>maxcases-1]

#ddReg <- unique(ddNames)
#names(ddReg) <- ddNames

# weitere Auswahllisten
Jvgl_SK <- c("SK Darmstadt","SK Kassel","SK Freiburg i.Breisgau","SK Flensburg","SK Bremerhaven","SK Salzgitter","SK Herne","SK Heilbronn","SK Erlangen","SK Halle","SK Ludwigshafen","SK Bielefeld","SK Cottbus","SK Regensburg","SK Braunschweig","SK Potsdam")
city_SN<- c("SK Chemnitz","SK Leipzig","SK Dresden")
Jvgl_LK <- c("LK Heinsberg","LK Oberspreewald-Lausitz","LK Rems-Murr-Kreis")

# Farben auslesen für ggplots
clrs<-LKcol %>% pull(plotcol)
names(clrs) <- LKcol  %>% pull(county)
colcolor <- scale_colour_manual(name = "grp",values = clrs)
colfill  <- scale_fill_manual(name = "grp",values = clrs)



#save(tsI, tsD, tsR, tsA, tsACountry, dates, ddNames, ddReg, file = paste0("dat/cacheData", format(Sys.Date(), format = "%m%d"), ".RData"))




