# getregmapTHdata.R

# Datesatz der Thüringer Kreise für Thürimngen Karte aufarbeiten

mkr.getmapTHregiondata <- function(df,impatser,immapdat){

# df = dfregTH()
# impatser = impatserious()
# immapdat =  immapdatum() 
  
plotdf <- df %>% 
  mutate(seriouscases = if_else((a*impatser)>0.1,ceiling(a*impatser),floor(a*impatser))) %>%
  left_join(sdTH_Ibedsreg)

# Stelle Mletztes Dateum der Daten fest
maxdat <- as.Date(max(plotdf$Datum))

dftsA_TH

# Prognose 10 Tage
yA <-dftsA_TH %>% select(Datum,a)%>% pivot_wider(names_from = "Datum",values_from = "a")
proj <- projSimple(yA, dates)
df<- cbind.data.frame(proj$x,proj$y) %>%
  rename(Datum = 'proj$x') %>%
  mutate(projsc = if_else((fit*impatser)>0.1,ceiling(fit*impatser),floor(fit*impatser)),
         projscl = if_else((lwr*impatser)>0.1,ceiling(lwr*impatser),floor(lwr*impatser)),
         projscu = if_else((upr*impatser)>0.1,ceiling(upr*impatser),floor(upr*impatser)),
         county = LK_TH[i]
  ) %>%
  filter(Datum > maxdat)
dfproj <- df
 
# Datensatz der Projektion: Nur ab Folgetag
dfproj <- dfproj %>% select(county,Datum,cases = fit,seriouscases = projsc) %>% mutate(typ = "projection")



#class(dfIndbedcol$warnIntstat)


# Erstelle Datensatz für Plot 
dfplot <- plotdf %>% select(county,Datum,cases,seriouscases) %>% mutate(typ = "real") %>% 
  bind_rows(dfproj) %>%  # füge Projektionsdaten an
  left_join(KKH_TH_Intbeds %>% select(county, IntensivBettenLK)) %>%
  mutate(warnIntstatc = if_else(IntensivBettenLK>0,seriouscases/IntensivBettenLK,5)) %>%  # Intensivbettenindikator
  mutate(warnIntstat = cut(warnIntstatc,breaks=Intbedbrk,include.lowest = TRUE,labels = Intbedlab)) 

#class(dfplot$warnIntstat)

# Geometrie der Thüringer LK und SK
geo_LKTH<- ger_lk %>% rename(BL = NAME_1) %>% left_join(LKcol) %>% filter(county %in% LK_TH)

# Merge geometry --> Daten satz für Plot
dfplotgeo <-   merge(geo_LKTH,dfplot) %>%
  filter(Datum == immapdat)  %>%
  full_join(dfIndbedcol) # full_join, um wirklich alle Kategorien in Datensatz zu haben

return(dfplotgeo)

}