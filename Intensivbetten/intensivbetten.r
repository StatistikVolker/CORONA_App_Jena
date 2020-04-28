## ---------------------------
##
## Script name: intensivbetten.r
##
## Purpose of script: Lädt die Daten der Thüringer KLiniken mit Intensivbetten und plottet sie auf eine Karte
##
## Author: Volker Holzendorf
##
## Date Created: 2020-04-15
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


# Krnakenhäuser in Thüringen
sdTH_Ibeds <- read_xlsx("Intensivbetten/Th\u00FCringen_Intensivbetten.xlsx", sheet="KKHs") #%>% 
  #replace(is.na(.), 0) %>%
  #mutate( Stationen = if_else( Stationen == 0 & Kategorie == "Level2",1, Stationen))
Table(sdTH_Ibeds$Region,sdTH_Ibeds$Kategorie)
class(sdTH_Ibeds$Intensivbetten_Beatmung)
# Anzahl Intensivbetten pro Region
sdTH_Ibeds %>% group_by(Region,Kategorie) %>% summarise_if(is.numeric,funs(sum))
sdTH_Ibeds %>% group_by(Stadt) %>% count() %>% filter(n>1)

# Städtegeometrie
sdTH_geom <- read_xlsx("Intensivbetten/Th\u00FCringen_Gemarkungen_Koordinaten.xlsx") 



# ZUsammenspiel
names(sdTH_Ibeds)
names(sdTH_geom)
# Test ob 1 zu 1 Merge geklappt hat
#sdTH_Ibedsgeom <- sdTH_Ibeds %>% left_join(sdTH_geom)
#sdTH_Ibedsgeom %>% group_by(Krankenhaus) %>% count() %>% filter(n>1)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
sdTH_Ibedsgeom<-sdTH_Ibeds %>% left_join(sdTH_geom) %>% 
  mutate(londms = str_replace_all(öL_PD83,'. ', ' '),
         latdms = str_replace_all(nB_PD83,'. ', ' '),
         londms = str_replace_all(öL_PD83,'  ', ' '),
         latdms = str_replace_all(nB_PD83,'  ', ' '),
         longitude = measurements::conv_unit(londms, "deg_min_sec", "dec_deg"),
         lattitude = conv_unit(latdms, "deg_min_sec", "dec_deg"),
  )
# Transormiere in SF-Object
sdTH_Ibedsgeom <- st_as_sf(x = sdTH_Ibedsgeom,                         
               coords = c("longitude", "lattitude"),
               crs = projcrs) %>%
  mutate(Kategorie = factor(Kategorie),
         Region = factor(Region),
         regcol = case_when(Region == "nord" ~ "darkgreen",
                            Region == "mitte" ~ "darkred",
                            Region == "ost" ~ "darkblue",
                            Region == "s\u00FCdwest" ~ "orange"))


names(sdTH_Ibedsgeom)

# Landesheometrien mit LK
# Geometrie der Thüringer LK und SK
# get spatial data for Germany on county level
ger_lk <- st_read('Deutschlandkarte/gadm36_DEU_2.shp') %>% st_transform(crs = 4326) 
names(ger_lk)

# Nur Thüringen
geo_LKTH<- ger_lk %>% filter(NAME_1 == "Th\u00FCringen") 
                                          
names(geo_LKTH)


# -----------------------------------------------------------------------------------------------------------------------------------------------
# Erstelle Mapplot von Thüringen mit den Standorten der Krankenhäuser
# -----------------------------------------------------------------------------------------------------------------------------------------------
names(sdTH_Ibedsgeom)
class(geo_LKTH)
#class(df$Kategorie)

ggplot() +
  geom_sf(data = geo_LKTH,fill = "white") +
  geom_sf(data = sdTH_Ibedsgeom, aes(shape = Kategorie, color = Region, size = Kategorie,fill = Region, alpha = Kategorie), show.legend = "polygon") +
  geom_sf(data = sdTH_Ibedsgeom, aes(shape = Kategorie, color = Region, size = Kategorie,fill = Region, alpha = Kategorie), show.legend = "point") +
  scale_size_manual(values = c(6,6,4)) +
  scale_alpha_manual(values = c(0.25,0.5,1)) +
  scale_shape_manual(values = c(22,20,3)) +
  #scale_color_manual(labels = sdTH_Ibedsgeom$Krankenhaus,values = sdTH_Ibedsgeom$regcol) +
  guides(shape = guide_legend(override.aes = list(fill = "white", color = "black", alpha = 1))) +
  theme_void() +
  theme(#legend.position = "bottom",
        legend.title = element_text(face = "bold",size = 24),
        legend.text = element_text(size = 16)) 
ggsave("Intensivbetten/Krankenhausstandorte nach Regionen in TH.jpg",width = 12, height = 10)
# -----------------------------------------------------------------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------------------------------------------------------------
#Datensatz Infektionszahlen nach Regionen - in getISbedsTHdata.R übertragen
# -----------------------------------------------------------------------------------------------------------------------------------------------
#source("getRKIdata.r")


# Thüringe Landkreise in Regionen
names(LKcol)
sdTH_LKRegion <- read_xlsx("Intensivbetten/Thueringen_Intensivbetten.xlsx", sheet="LK_Regionen") %>%
  mutate(NAME_2 = LK) %>%
  left_join(LKcol) %>%
  select(LK,Region,county, plotcol, county2)

names(tsI)

tsI_TH <- tsI %>% mutate(county = Country.Region) %>% 
  left_join(sdTH_LKRegion) %>% filter(!is.na(Region)) %>% 
  group_by(Region) %>% summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  pivot_longer(contains("2020"),names_to = "Datum",values_to = "infected")
tsD_TH <- tsD %>% mutate(county = Country.Region) %>% 
  left_join(sdTH_LKRegion) %>% filter(!is.na(Region)) %>% 
  group_by(Region) %>% summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  pivot_longer(contains("2020"),names_to = "Datum",values_to = "deceased")

# Recovered nach Person, niht PÜerson und Datum im RKI Datensatz, deswegen unbrauchbra.
tsR_TH <- tsR %>% mutate(county = Country.Region) %>% 
  left_join(sdTH_LKRegion) %>% filter(!is.na(Region)) %>% 
  group_by(Region) %>% summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  pivot_longer(contains("2020"),names_to = "Datum",values_to = "recovered")


tsID_TH <- tsI_TH %>% left_join(tsD_TH) %>% filter(Datum > "2020-02-29") %>%
  pivot_longer(cols = c(infected,deceased),names_to = "status",values_to = "nsum" )

names(tsID_TH)
#names(sdTH_LKRegion)
# -----------------------------------------------------------------------------------------------------------------------------------------------
# O`Plot Infectionszahlen in Thüringen nach Region
# To Do: 
# Scalen anpassen --> aus CORONA JEna Bericht
# auch mindat, maxdat
# -----------------------------------------------------------------------------------------------------------------------------------------------

mindat<-as.Date(min(tsID_TH$Datum))
maxdat<-as.Date(max(tsID_TH$Datum))
class(maxdat)

p1 <- ggplot() +
  geom_point(data = tsID_TH,aes(x = Datum,y = nsum, color = Region, group = interaction(Region,status)),size = 3) +
  geom_step(data = tsID_TH, aes(x = Datum,y = nsum, color = Region, group = interaction(Region,status)),size = 2) +
  scale_colour_manual(values = rep(c("darkblue","forestgreen","orange","darkred"),2)) + 
  #geom_line(data = dffit,aes(x = datum,y = projn, group = progtyp),size = 0.8) +
  theme_minimal() +
  theme(legend.position =  "bottom") +
  theme(legend.position =  "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        #panel.grid.minor.x = element_line(colour="blue", size=0.5), 
        panel.grid.major.x = element_line(colour="red", size=0.75)) + 
  scale_y_continuous(breaks = seq(0,1000,100),labels =seq(0,1000,100)) +
  labs(y = "absolute Anzahl",x = "Datum") #+
  #scale_x_date(breaks = seq(as.Date(mindat),as.Date(maxdat),by="week"),
  #             date_labels = "%b %d", minor_breaks = "1 day") #+
  #annotate("rect", xmin = mindat-1, xmax = maxdat+2, ymin = -10, ymax = -4,fill = "white") +
  #annotate("text",y=-6,x = c(seq(mindat,maxdat,"day")),
  #         label = c(rep(c("S","M","D","M","D","F","S"),poltweeks),"S"), 
  #         color =c(rep(c("red",rep("blue",5),"orange"),poltweeks),"red"))
p1

# -----------------------------------------------------------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------------------------------------------------------
# Plot Prognose Intesivbetten
# -----------------------------------------------------------------------------------------------------------------------------------------------
names(tsI_TH)
impatRegion <- "mitte"
impatserious <- 0.025  # 2.5% aller Infizierten!

KKHTH_Intbeds <- sdTH_Ibedsgeom %>% select(Region,Intensivbetten_Beatmung,Stationen,Kategorie) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  distinct() %>%
  group_by(Region) %>%
  summarise(Intensivbetten_Beatmung = sum(Intensivbetten_Beatmung),
            Stationen = sum(Stationen)
  ) 

names(KKHTH_Intbeds)

plotdf <- tsI_TH %>% 
  #rename(county = Region) %>%
  #filter(Region %in% input$impatRegion) %>%
  filter(Region == impatRegion) %>%
  group_by(Region) %>%
  #pivot_longer(-county,names_to = "Datum",values_to = "cases") %>%
  #mutate(seriouscases = if_else((cases*impatserious())>0.1,ceiling(cases*impatserious()),floor(cases*impatserious()))) %>%
  mutate(seriouscases = if_else(infected*impatserious>0.1,ceiling(infected*impatserious),floor(infected*impatserious))) %>%
  left_join(KKHTH_Intbeds) %>%
  filter(case_when(infected[Datum == "2020-03-01"]==0 ~ Datum>="2020-02-29",
                   infected[Datum == "2020-02-15"]==0 ~ Datum>="2020-02-15",
                   TRUE ~ Datum>=min(Datum))
  )

names(KKHTH_Intbeds)

Table(plotdf$Region)


maxdat <- as.Date(max(plotdf$Datum))


# Prognose 10 Tage
#yA <-tsSub(tsI,tsI$Country.Region %in% input$impatRegion)
tsI_THw <- tsI_TH %>% pivot_wider(id_cols = "Region",names_from = "Datum", values_from = "infected")
yA <-tsSub(tsI_THw,tsI_THw$Region %in% impatRegion)
proj <- projSimple(yA, dates)


dfproj<- cbind.data.frame(proj$x,proj$y) %>%
  rename(Datum = 'proj$x') %>%
#  mutate(projsc = if_else((fit*impatserious())>0.1,ceiling(fit*impatserious()),floor(fit*impatserious())),
#         projscl = if_else((lwr*impatserious())>0.1,ceiling(lwr*impatserious()),floor(lwr*impatserious())),
#         projscu = if_else((upr*impatserious())>0.1,ceiling(upr*impatserious()),floor(upr*impatserious())),
#         county = input$impatLK
#  ) %>%
  mutate(projsc = if_else((fit*impatserious)>0.1,ceiling(fit*impatserious),floor(fit*impatserious)),
         projscl = if_else((lwr*impatserious)>0.1,ceiling(lwr*impatserious),floor(lwr*impatserious)),
         projscu = if_else((upr*impatserious)>0.1,ceiling(upr*impatserious),floor(upr*impatserious)),
         county = "mitte"
         ) %>%
  filter(Datum > maxdat) %>%
  rename(Region = county)

names(plotdf)
# Grafik attribute 
Intbeds<-plotdf$Intensivbetten_Beatmung[1]
yscmax<-max(Intbeds,max(dfproj$projscu))
yInblab <- if_else(Intbeds < yscmax,yscmax,Intbeds)
anottxt <- paste0("Intensivmedizinische Betten in Th\u00FCrigen ",plotdf$Region[1]," (N = ",Intbeds,")")
# Grafik (ggplot)
mindat <- ceiling_date(as.Date(min(plotdf$Datum)),unit = "week")
maxdat <- floor_date(as.Date(max(dfproj$Datum)),unit = "week")

ggplot(data = plotdf,aes(x = as.Date(Datum),y = seriouscases, color = Region, fill = Region)) +
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
  #colfill + colcolor +
  labs(y = "kritische CORONA Patienten (geschätzt)",x = "Datum")# +

names(dfproj)
