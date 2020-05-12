# getRKIdata.R

# ---------------------------
#
# Script name: getRKIdata.R#
#
# Purpose of script: Zieht die tagesaktuellen  CORONA Daten von der RKI Seite und formatiert die Daten für Dashboard um
#
# Author: Ben Phillips, Volker Holzendorf
#
# Date Created: 2020-03-30
#
# Email: statistik@jena.de
#
# ---------------------------
#
# Notes:
#  
#
# --------------------------
# load up the packages we will need:  

library("readxl")
library("tidyverse")
library("jsonlite")
library("lubridate")
library("sp")
library("sf")
library("measurements")

# ---------------------------
# load up functions
source("functions.R")
#Sys.setenv(htttps = "sqid.jena.de:3128")
#rm(dfRKII)
# ---------------------------

maxcases <-  0 # Nur LKs mit 50 Corona Fällen oder mehr werden ausgegeben!
inWindow <- 10 # Zeitspanne aus der geschätzt wird


LKcol <-read_excel("Daten/Landkreise plotcols 20200330.xlsx", sheet = "Landkreise") #%>% rename(GEN = 'Städte')


# Entweder Testdaten --------------------------------

##load(file="Daten/Testdaten.Rdata")

##testLK <- c("LK Rems-Murr-Kreis","SK Erlangen","SK Lübeck","SK Braunschweig","SK Bremerhaven","LK Heinsberg","SK Kassel",
##            "SK Ludwigshafen","LK Sankt Wendel","SK Potsdam","LK Rostock","SK Leipzig","SK Halle","SK Jena")
###test_BL <-16
##names(RKIE_df)
##RKIE_df <- RKIE_df %>% filter(county %in% testLK  | (BL_ID ==16))

# oder livedaten -------------------------------------

# API Adresse aktuelle Einwohnerzahlen
RKIE <- "https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.geojson"
####names(tsEold)
# Lade Aktuelle Daten mit allen Features
RKIE_JSON<-fromJSON(RKIE)

# Aktueller Datensatz des RKI
RKIE_df <- RKIE_JSON$features$properties

#names(tsE)
# API Adresse der aktuellen CORONA Daten 
#tsRKI <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson"
tsRKI <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson"
###tsRKI <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson?where=Bundesland%20%3D%20'Th%C3%BCringen'"  
### Lade Aktuelle Daten mit allen Features
RKI_JSON<-fromJSON(tsRKI)
##
#json_data_as_list <- readLines(tsRKI,skipNul = TRUE) %>% lapply(fromJSON)
##
# Aktueller Datensatz des RKI
RKI_df <- RKI_JSON$features$properties
###length(unique(RKI_df$Landkreis))
###length(unique(RKI_df$Bundesland))
##
##
save(RKIE_df,RKI_df,file=paste("Daten/Testdaten/",today(),"Testdaten.Rdata"))
##
## oder livedaten Ende -------------------------------------
print("RKI Daten geladen")

# Stätde meta datensatz für Farben/ Einwohnerzahl usw.
tsE <-RKIE_df %>% select(GEN,BEZ,NUTS,EWZ,cases_per_100k,cases_per_population,BL,county) %>%
  left_join(LKcol)

# Aktuelle Fallzahlen und Todesfälle mit allen Subgruppen
tsIa<-mkr.makecoronaRKIallSubg(cases = AnzahlFall)
tsDa<-mkr.makecoronaRKIallSubg(cases = AnzahlTodesfall)
#tsRa<-mkr.makecoronaRKIallSubg(cases = AnzahlGenesen)

# Datensätze auf Landkreisebene
tsI<-mkr.makecoronaRKIgesLK(tsIa)
tsD<-mkr.makecoronaRKIgesLK(tsDa)
#tsR<-mkr.makecoronaRKIgesLK(tsRa)

# get Date range
dCols<-dateCols(tsI)
dates<-as.Date(colnames(tsI)[dCols])

# Tidy up names
names(tsI)[!dCols] <- make.names(names(tsI)[!dCols])
names(tsD)[!dCols] <- make.names(names(tsD)[!dCols])
#names(tsR)[!dCols] <- make.names(names(tsR)[!dCols])
#names(tsT)[!dCols] <- make.names(names(tsT)[!dCols])

# add recovery lag -- assumes all cases recover at 22 days
matI<-as.matrix(tsI[, dCols])
matD<-as.matrix(tsD[, dCols])
#matR<-as.matrix(tsR[, dCols])
#matA <- matI - matR

# Active - Abschätzung aus den PÜrädiktiven Werten der Jenaer Daten, Stand 23.4.2020

lastdat <- as.Date(max(RKI_df$Meldedatum));lastdat
firstdat<- as.Date(min(RKI_df$Meldedatum));firstdat
dayswcovid <- ceiling(as.numeric(difftime(lastdat,firstdat,units = "days")))+1;dayswcovid
Datum <-  rev(seq.Date(firstdat, lastdat, by="day"))
tag <- c(0:(dayswcovid-1));length(tag)
ppwvals <- c(rep(1,7),rep(0.810,7),rep(0.476,7),rep(0.238,7),rep(0.048,28),rep(0,dayswcovid-56));length(ppwvals)
npwvals <- c(rep(0,7),rep(0.079,7),rep(0.416,7),rep(0.911,7),rep(0.980,28),rep(1,dayswcovid-56));length(npwvals)
tsAhelp <- cbind.data.frame(Datum,tag,ppwvals,npwvals)

# Datensatz mit Infected, Recovered, Active (inkl. death)
dfBLLK <- RKI_df %>% select(Bundesland,county = Landkreis) %>% distinct()


dftsI<-RKI_df %>% group_by(Meldedatum,Landkreis) %>% summarise(infected = sum(AnzahlFall)) %>%
  mutate(Datum = as.Date(Meldedatum),
         county = Landkreis
  ) %>%
  pivot_wider(id_cols = c(Datum),names_from = county,values_from = infected,values_fill = list(infected = 0)) %>%
  ungroup() %>%
  complete(Datum = seq.Date(min(Datum), max(Datum), by="day")) %>%
  pivot_longer(cols = -c(Datum),names_to = "county",values_to = "infected") %>%
  arrange(county,Datum) %>%
  mutate(infected = if_else(is.na(infected),"0",as.character(infected)),
         infected = as.numeric(infected)) %>%
  ungroup() %>%
  group_by(county) %>% 
  mutate(ip = infected/sum(infected),
         id  = row_number()) %>% 
  ungroup() %>%
  left_join(dfBLLK) %>% select(Bundesland, county,Datum,id,infected, ip)




#Infected:
names(tsI)

tsICountry <- countryAgg(tsI) # aggregated to country
tsICountry <- tsICountry[rev(order(tsICountry[[ncol(tsICountry)-1]])),] # ordered from most to least active cases


print("Daten aufbereitet")

# Landesheometrien mit LK
# Geometrie der Thüringer LK und SK
# get spatial data for Germany on county level
ger_lk <- st_read('Deutschlandkarte/gadm36_DEU_2.shp') %>% st_transform(crs = 4326) 
names(ger_lk)

# Define menus
# get region names with 20 or more cases as of yesterday
ddNames <- tsICountry$Country[tsICountry[[ncol(tsICountry)-1]]>maxcases-1]

ddReg <- unique(ddNames)
names(ddReg) <- ddNames
#ddReg <- paste(ddReg, collapse = ", ") # menu specifier

# get region names with 20 or more cases as of yesterday
datcol <- as.character(today()-1)
tsICountry2 <- tsICountry %>% #filter(!!as.name(datcol) > maxcases-1) %>%
  rename(county = Country) %>%
  left_join(tsE) %>%
  select(BL,BL_ID,county)

#names(tsICountry2)
LK_BW <- tsICountry2 %>% filter(BL_ID == 8) %>% arrange(county) %>% pull(county) # BL_ID stattt Klaramen (BL) wegen Umlaut
LK_BY <- tsICountry2 %>% filter(BL == 'Bayern') %>% arrange(county) %>% pull(county)
LK_BE <- tsICountry2 %>% filter(BL == 'Berlin') %>% arrange(county) %>% pull(county)
LK_BB <- tsICountry2 %>% filter(BL == 'Brandenburg') %>% arrange(county) %>% pull(county)
LK_HB <- tsICountry2 %>% filter(BL == 'Bremen') %>% arrange(county) %>% pull(county)
LK_HH <- tsICountry2 %>% filter(BL == 'Hamburg') %>% arrange(county) %>% pull(county)
LK_HE <- tsICountry2 %>% filter(BL == 'Hessen') %>% arrange(county) %>% pull(county)
LK_MV <- tsICountry2 %>% filter(BL == 'Mecklenburg-Vorpommern') %>% arrange(county) %>% pull(county)
LK_NS <- tsICountry2 %>% filter(BL == 'Niedersachsen') %>% arrange(county) %>% pull(county)
LK_NRW <- tsICountry2 %>% filter(BL == 'Nordrhein-Westfalen') %>% arrange(county) %>% pull(county)
LK_RP <- tsICountry2 %>% filter(BL == 'Rheinland-Pfalz') %>% arrange(county) %>% pull(county)
LK_SL <- tsICountry2 %>% filter(BL == 'Saarland') %>% arrange(county) %>% pull(county)
LK_SN <- tsICountry2 %>% filter(BL == 'Sachsen') %>% arrange(county) %>% pull(county)
LK_SA <- tsICountry2 %>% filter(BL == 'Sachsen-Anhalt') %>% arrange(county) %>% pull(county)
LK_SH <- tsICountry2 %>% filter(BL == 'Schleswig-Holstein') %>% arrange(county) %>% pull(county)
LK_TH <- tsICountry2 %>% filter(BL_ID == 16) %>% arrange(county) %>% pull(county) # BL_ID stattt Klaramen (BL) wegen Umlaut
LK_SS <- tsICountry2 %>% filter(BL %in% c('Berlin','Bremen','Hamburg')) %>% arrange(county) %>% pull(county)
#print(LK_TH)

# weitere Auswahllisten
Jvgl_SK <- c("SK Darmstadt","SK Kassel","SK Freiburg i.Breisgau","SK Flensburg","SK Bremerhaven","SK Salzgitter","SK Herne","SK Heilbronn","SK Erlangen","SK Halle","SK Ludwigshafen","SK Bielefeld","SK Cottbus","SK Regensburg","SK Braunschweig","SK Potsdam")
city_SN<- c("SK Chemnitz","SK Leipzig","SK Dresden")
Jvgl_LK <- c("LK Heinsberg","LK Oberspreewald-Lausitz","LK Rems-Murr-Kreis","LK M\u00E4rkischer Kreis","LK Calw")


#ddReg <- LK_TH  
#ddNames <- tsACountry$Country[tsACountry[[ncol(tsACountry)-1]]>maxcases-1]

#ddReg <- unique(ddNames)
#names(ddReg) <- ddNames


# Farben auslesen für ggplots
clrs<-LKcol %>% pull(plotcol)
names(clrs) <- LKcol  %>% pull(county)
colcolor <- scale_colour_manual(name = "grp",values = clrs)
colfill  <- scale_fill_manual(name = "grp",values = clrs)



print("Formatierungsdaten erstellt")

#save(tsI, tsD, tsR, tsA, tsACountry, dates, ddNames, ddReg, file = paste0("dat/cacheData", format(Sys.Date(), format = "%m%d"), ".RData"))





