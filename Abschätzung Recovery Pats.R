# Versuch der Abschätzung der Recovered Patienten.

# Idee über Prädiktive werte der Stadt Jena
# PPW für ist weiterhin Positiv
# NPW für ist neagativ.

#Sehr grob: immer Wochenweise:
#Tag 0-6, 7-13.14-20.21-27, 28+  
#folgende Werte (ermittelt aus den Daten vom 23.4.2020, 17:19)

#Berechne Tage seit 1.März
# letzter Tag in Datensatz
lastdat <- max(tsI_TH$Datum);lastdat
firstdat<- min(tsI_TH$Datum);firstdat
dayswcovid <- ceiling(as.numeric(difftime(lastdat,firstdat,units = "days")))+1;dayswcovid

tag <- c(0:(dayswcovid-1));length(tag)
ppwvals <- c(rep(1,7),rep(0.810,7),rep(0.476,7),rep(0.238,7),rep(0.048,dayswcovid-28));length(ppwvals)
npwvals <- c(rep(0,7),rep(0.079,7),rep(0.416,7),rep(0.911,7),rep(0.980,dayswcovid-28));length(npwvals)


tsI_THs <- tsI %>% mutate(county = Country.Region) %>% 
  left_join(sdTH_LKRegion) %>% filter(!is.na(Region)) %>% 
  group_by(Region)

tsR_TH <- tsI_THs %>% 
  pivot_wider(id_cols = "Datum",names_from = "Region",values_from = "infected") %>%
  mutate(tag = rev(tag),
         ppwvals = rev(ppwvals),
         npwvals = rev(npwvals))




class(dfTH_tsI$Datum)

names(RKI_df)
dfTH_tsI<-RKI_df %>% filter(Bundesland == "Thüringen") %>% group_by(Meldedatum,Landkreis) %>% summarise(infected = sum(AnzahlFall)) %>%
  mutate(Datum = as.Date(Meldedatum),
         county = Landkreis
         ) %>%
  left_join(sdTH_LKRegion) %>%
  group_by(Datum,Region) %>%
  summarise(infected = sum(infected)) %>%
  pivot_wider(id_cols = Datum,names_from = Region,values_from = infected,values_fill = list(infected = 0)) %>%
  ungroup() %>%
  complete(Datum = seq.Date(min(Datum), max(Datum), by="day"), fill = list(mitte = 0, nord = 0, ost = 0, südwest = 0)) 

  


lastdat <- as.Date(max(RKI_df$Meldedatum));lastdat
firstdat<- as.Date(min(RKI_df$Meldedatum));firstdat
dayswcovid <- ceiling(as.numeric(difftime(lastdat,firstdat,units = "days")))+1;dayswcovid

Datum <-  rev(seq.Date(firstdat, lastdat, by="day"))
tag <- c(0:(dayswcovid-1));length(tag)
ppwvals <- c(rep(1,7),rep(0.810,7),rep(0.476,7),rep(0.238,7),rep(0.048,28),rep(0,dayswcovid-56));length(ppwvals)
npwvals <- c(rep(0,7),rep(0.079,7),rep(0.416,7),rep(0.911,7),rep(0.980,28),rep(1,dayswcovid-56));length(npwvals)

tsRhelp <- cbind.data.frame(Datum,tag,ppwvals,npwvals)


dfJ_tsR <- dfTH_tsI %>%
  mutate(tag = rev(tag),
         ppwvals = rev(ppwvals),
         npwvals = rev(npwvals),
         ostactivef1 = floor(ost * ppwvals),
         ostactivef2 = floor(ost - (ost * npwvals)),
         ostactiver1 = round(ost * ppwvals),
         ostactiver2 = round(ost - (ost * npwvals)),
         ostactivec1 = ceiling(ost * ppwvals),
         ostactivec2 = ceiling(ost - (ost * npwvals))
  )
         


sum(dfJ_tsR$ost)
sum(dfJ_tsR$ostactivec1)
sum(dfJ_tsR$ostactivec2)
sum(dfJ_tsR$ostactiver1)
sum(dfJ_tsR$ostactiver2)
sum(dfJ_tsR$ostactivef1)
sum(dfJ_tsR$ostactivef2)

