## ---------------------------
##
## Script name: getISbedsTHdata.R

## Purpose of script: Lädt Daten für Intensivbetten in Thüringen
##  
## ACHTUNG: derzeit nur Thüringen
##
## Author: Volker Holzendorf
##
## Date Created: 2020-04-24
##
## Email: statistik@jena.de
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------
## load up the packages we will need 
library(tidyverse)
library(sp)
library(sf)
library(readxl)
#library(OurTools)
#library(Rdatacheck)
library(measurements)
library(lubridate)
#library()
## ---------------------------
#names(sdTH_Ibeds)


# -----------------------------------------------------------------------------------------------------------------------------------------------
# Krnakenhäuser in Thüringen
# -----------------------------------------------------------------------------------------------------------------------------------------------
sdTH_Ibeds <- read_xlsx("Intensivbetten/Thueringen_Intensivbetten.xlsx", sheet="KKHs") #%>% 
#replace(is.na(.), 0) %>%
#mutate( Stationen = if_else( Stationen == 0 & Kategorie == "Level2",1, Stationen))
#Table(sdTH_Ibeds$Region,sdTH_Ibeds$Kategorie)
class(sdTH_Ibeds$Intensivbetten_Beatmung)
# Anzahl Intensivbetten pro Region
sdTH_Ibeds %>% group_by(Region,Kategorie) %>% summarise_if(is.numeric,sum)
sdTH_Ibeds %>% group_by(Stadt) %>% count() %>% filter(n>1)
sdTH_Ibedsreg <- sdTH_Ibeds %>% group_by(Region) %>% summarise_if(is.numeric,sum)

# -----------------------------------------------------------------------------------------------------------------------------------------------
# Städtegeometrie Thüringen
# -----------------------------------------------------------------------------------------------------------------------------------------------
sdTH_geom <- read_xlsx("Intensivbetten/Thueringen_Gemarkungen_Koordinaten.xlsx") 

# -----------------------------------------------------------------------------------------------------------------------------------------------
# ZUsammenspiel: Krankenhäuser und Stadtgeometrie
# -----------------------------------------------------------------------------------------------------------------------------------------------
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
sdTH_Ibedsgeom<-sdTH_Ibeds %>% left_join(sdTH_geom) %>% 
  mutate(londms = str_replace_all(oeL_PD83,'. ', ' '),
         latdms = str_replace_all(nB_PD83,'. ', ' '),
         londms = str_replace_all(londms,'  ', ' '),
         latdms = str_replace_all(latdms,'  ', ' '),
         longitude = measurements::conv_unit(londms, "deg_min_sec", "dec_deg"),
         lattitude = conv_unit(latdms, "deg_min_sec", "dec_deg"),
  )
# Transormiere in SF-Object
sdTH_Ibedsgeom <- st_as_sf(x = sdTH_Ibedsgeom,                         
                           coords = c("longitude", "lattitude"),
                           crs = projcrs) %>%
  mutate(Kategorie = factor(Kategorie),
         Region = factor(Region))


#names(sdTH_Ibedsgeom)

# -----------------------------------------------------------------------------------------------------------------------------------------------
# LK undSK nur Thüringen
# -----------------------------------------------------------------------------------------------------------------------------------------------
geo_LKTH<- ger_lk %>% filter(NAME_1 == "Thüringen") 
#names(geo_LKTH)

# -----------------------------------------------------------------------------------------------------------------------------------------------
#Datensatz Infektionszahlen nach Regionen
# -----------------------------------------------------------------------------------------------------------------------------------------------
sdTH_LKRegion <- read_xlsx("Intensivbetten/Thueringen_Intensivbetten.xlsx", sheet="LK_Regionen") %>%
  mutate(NAME_2 = LK) %>%
  left_join(LKcol) %>%
  select(LK,Region,county, plotcol, county2)

#Datensatz für LK
#names(dftsI_TH)
#names(sdTH_LKRegion)
dftsI_TH <- dftsI %>% filter(Datum >= "2020-03-01") %>%
  left_join(sdTH_LKRegion) %>% filter(!is.na(Region)) %>% 
  group_by(Region,Datum) %>% summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  ungroup()

print("Daten Intensivbetten Th\u00FCringen erstellt")
