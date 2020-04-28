# getRKIdata.R

## ---------------------------
##
## Script name: getKKHdata.R
##
## Purpose of script: Wandelt die Kraknehausdaten um
##  
## ACHTUNG: derzeit nur Thüringen
##
## Author: Volker Holzendorf
##
## Date Created: 2020-03-31
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
library("sp")
library("sf")
## ---------------------------
## load up functions
#source("functions.R")
#rm(dfRKII)
## ---------------------------


# get spatial data for Germany on county level
ger_lk <- st_read('Deutschlandkarte/gadm36_DEU_2.shp') %>% st_transform(crs = 4326) 
ger_bl <- st_read('Deutschlandkarte/gadm36_DEU_1.shp') %>% st_transform(crs = 4326) 


# Krankenhausbetten 
KKH_TH     <-read_excel("Daten/Krankenhausbetten Thueringen 2017 roh.xlsx", sheet = "Kreise",skip = 4,
                        col_types = c("text",rep("numeric",6))) #%>% rename(GEN = 'Städte')
KKHbeds_TH <-read_excel("Daten/Krankenhausbetten Thueringen 2017 roh.xlsx", sheet = "Intensivbetten nach KKH",skip = 5) %>%
  group_by(KKHtyp2) %>%
  summarise_if(is.numeric,funs(sum(.))) %>%
  mutate(IntensivAnteil = IntensivBetten/Bettengesamt*100)



# Metadaten LK für Thüringen aufarbeiten. ---------------------------------


KKHLK_TH<- tsE %>%
  filter(BL_ID == 16) %>%
  #separate(county,into =c("drop","LK"),sep = "K ") %>%
  mutate(LK = county,
         LK = str_replace(LK, "LK ", ""),
         LK = str_replace(LK, "SK ", "Stadt ") 
         ) %>%
  select(county,LK,EWZ,cases_per_100k,cases_per_population)

tsA %>% rename(county = Country.Region)

#summary(KKHLK_TH$LK)
# Berechne Intensiv Betten Indikator --------------------------------------

names(df)

KKH_TH_Intbeds<-KKH_TH %>% 
  select(-c(BettenLK,KKHs)) %>%
  pivot_longer(cols = c(u100beds,b200beds,b500beds,mt500beds),names_to ="KKHtyp2",values_to = "KKHtypN") %>%
  left_join(KKHbeds_TH) %>%
  filter(KKHtypN >0) %>%
  mutate(KKHtyp2 = factor(KKHtyp2, levels = c("u100beds","b200beds","b500beds","mt500beds"))) %>%
  arrange(factor(KKHtyp2, levels = c("u100beds","b200beds","b500beds","mt500beds"))) %>%
  pivot_wider(id_cols = LK,names_from = KKHtyp2,names_prefix = "p",values_from = IntensivAnteil) %>%
  replace_na(list(pu100beds=0,pb200beds=0,pb500beds=0,pmt500beds=0)) %>%
  mutate(sumVar = select(., contains("beds")) %>% rowSums(),
         KKHtypN = as.numeric(pu100beds>0)+as.numeric(pb200beds>0)+as.numeric(pb500beds>0)+as.numeric(+pmt500beds>0),
         IntbdesInd = sumVar/KKHtypN) %>%
  full_join(KKH_TH) %>%
  mutate(IntbdesInd = if_else(is.na(IntbdesInd),0,IntbdesInd),
         IntensivBettenLKroh = BettenLK*IntbdesInd/100 ) %>% # geschätzte Intensivbettenzahl im LK. 
  separate(IntensivBettenLKroh, into = c("col1", "col2"),remove = FALSE) %>%
  mutate(col2 = if_else(is.na(col2),"0",col2),
         col2b = as.numeric(paste0("0.",col2)),
         IntensivBettenLK = if_else(col2b > 0.08,ceiling(BettenLK*IntbdesInd/100),floor(BettenLK*IntbdesInd/100))
         ) %>%
  select(LK,KKHs,IntbdesInd,BettenLK,IntensivBettenLK,IntensivBettenLKroh) %>%
  left_join(KKHLK_TH) %>%
  mutate(IntBeds_per_100k = IntensivBettenLK/EWZ*100000,
         IntBeds_per_population = IntensivBettenLK/EWZ) %>%
  select(county,LK,EWZ,KKHs,BettenLK,IntensivBettenLK,IntensivBettenLKroh,IntbdesInd,cases_per_100k,IntBeds_per_100k,cases_per_population,IntBeds_per_population) %>%
  filter(!is.na(county))


#sum(KKH_TH_Intbeds$IntensivBettenLK)-696

#KKH_TH_Intbeds$col2b[KKH_TH_Intbeds$col2b <0.1]

# Datensatz der Farben
Intbedbrk <- c(0,0.1,0.5,0.7,0.9,1,Inf)
Intbedcol <- c("greenyellow","chartreuse4","gold","orange","orangered3","red")
Intbedlab <- c("bis 10%","bis 50%","bis 70%\nWarnstufe1","bis 90%\nWarnstufe2","bis 100%\nWarnstufe3","Bedarf reicht im LK\nnicht mehr aus")

dfIndbedcol <-cbind.data.frame(warnIntstat = Intbedlab,Intbedcol) %>%
  mutate(warnIntstat = factor(warnIntstat, levels = Intbedlab))

