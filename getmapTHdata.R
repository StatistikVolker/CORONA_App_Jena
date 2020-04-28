# getmapTHdata.R

# Datesatz der Thüringer Kreise für Thürimngen Karte aufarbeiten

mkr.getmapTHdata <- function(impatser,immapdat){

# impatser = impatserious()
# immapdat =  immapdatum() 
  
plotdf <- tsA %>% 
  rename(county = Country.Region) %>%
  filter(county %in% LK_TH) %>%
  group_by(county) %>%
  pivot_longer(-county,names_to = "Datum",values_to = "cases") %>%
  mutate(seriouscases = if_else((cases*impatser)>0.1,ceiling(cases*impatser),floor(cases*impatser))) %>%
  left_join(KKH_TH_Intbeds) %>%
  mutate(Datum = as.Date(Datum)) %>%
  filter(case_when(cases[Datum == "2020-03-01"]==0 ~ Datum>="2020-02-29",
                   cases[Datum == "2020-02-15"]==0 ~ Datum>="2020-02-15",
                   TRUE ~ Datum>=min(Datum))
  )

# Stelle Mletztes Dateum der Daten fest
maxdat <- as.Date(max(plotdf$Datum))


# Prognose 10 Tage
for (i in 1:length(LK_TH)){
  #i<-1
  yA <-tsSub(tsA,tsA$Country.Region %in% LK_TH[i]) #$LKfinder)
  proj <- projSimple(yA, dates)
  df<- cbind.data.frame(proj$x,proj$y) %>%
    rename(Datum = 'proj$x') %>%
    mutate(projsc = if_else((fit*impatser)>0.1,ceiling(fit*impatser),floor(fit*impatser)),
           projscl = if_else((lwr*impatser)>0.1,ceiling(lwr*impatser),floor(lwr*impatser)),
           projscu = if_else((upr*impatser)>0.1,ceiling(upr*impatser),floor(upr*impatser)),
           county = LK_TH[i]
    ) %>%
    filter(Datum > maxdat)
  #print(i)
  #print(dim(df))
  if (i==1) {
    dfproj <- df
  } else {
    dfproj <- rbind.data.frame(dfproj,df)
  }
}

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