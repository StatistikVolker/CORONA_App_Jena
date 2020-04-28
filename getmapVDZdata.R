# getmapVDZdata.R

# Datensatz der LKs für die Verdopplungszeitkarte aufarbeitet

#Alle LK Daten.
#tsI$Country.Region
tsIvdz <- tsI %>% mutate(county = as.character(Country.Region))
counties <- tsIvdz$county
#print(length(counties))
# Prognose 10 Tage
for (i in 1:length(counties)){
  #i<-49
  #tsIvdzm <-tsSub(tsIvdz,tsIvdz$county == counties[i]) #$LKfinder);tsIvdz
  #class(tsIvdzm)
  
  #print(i)
  #print(counties[i])
  
  tsIvdzvec <- tsIvdz %>% filter(county == counties[i]) %>%
    pivot_longer(-c(county,Country.Region),names_to ="datum",values_to = "cases") %>% 
    pull(cases)
  names(tsIvdzvec) <- dates
  #print(tsIvdzvec)
  j=0
  for (tag in seq(length(dates),length(dates)-10,-1)) {
    ###nur für aktuellen Tag
    #tag <- length(dates)
    j = j+1
    #print(j)
    #print(tag)
    #print(dates[tag])

    wtrate <- projSimpleSlope(tsIvdzvec[1:tag], dates[1:tag],inWindow = 5)[2];wtrate
    VDzeit05<-if_else(round(wtrate,10) > 0, log(2)/wtrate,999)
    wtrate <- projSimpleSlope(tsIvdzvec[1:tag], dates[1:tag],inWindow = 7)[2];wtrate
    VDzeit07<-if_else(round(wtrate,10) > 0, log(2)/wtrate,999)
    wtrate <- projSimpleSlope(tsIvdzvec[1:tag], dates[1:tag],inWindow = 10)[2];wtrate
    VDzeit10<-if_else(round(wtrate,10) > 0, log(2)/wtrate,999)
    VDzeitzeile <-c(counties[i],as.character(dates[tag]),VDzeit05,VDzeit07,VDzeit10)
    #print(VDzeitzeile)
    VDzeitzeile <-as.data.frame(t(VDzeitzeile))
    names(VDzeitzeile) <- c("county","datum","VDzeit05","VDzeit07","VDzeit10")
    
    #colnames(VDzeitzeile)<-dates[tag]
    #print(VDzeitzeile)
    if (j == 1){
      dfVDZ <- VDzeitzeile
      #print(dfVDZ)
    } else {
      dfVDZ <- rbind.data.frame(VDzeitzeile,dfVDZ)
    }
  }
  dfVDZ<-dfVDZ %>% mutate(datum = as.Date(datum),
                          VDzeit05 = as.numeric(as.character(VDzeit05)),
                          VDzeit07 = as.numeric(as.character(VDzeit07)),
                          VDzeit10 = as.numeric(as.character(VDzeit10))
  )
  #print(dates[tag])
  if (i==1) {
    dfVDZall <- dfVDZ
  } else {
    dfVDZall <- rbind.data.frame(dfVDZall,dfVDZ)
  }
  
}

## Datensatz der Farben für Verdopplungszeit in LKs
VDZbrk <- c(0,2,3,5,7,10,15,998,Inf)
VDZcol <- c("red","orangered3","darkorange3","orange","gold","chartreuse4","greenyellow","royalblue2")
VDZlab <- c("bis 2 Tage","bis 3 Tage","bis 5 Tage","bis 7 Tage","bis 10 Tage","bis 15 Tage",">15Tage","keine Neuinfektionen")

dfVDZcol <-cbind.data.frame(VDZlab,VDZcol) %>%
  mutate(VDZlab = factor(VDZlab, levels = VDZlab))

#barplot(1:7,col =VDZcol)

names(dfVDZall)
names(dfVDZcol)
dfVDZall<-   dfVDZall %>%   # Spiele geodaten zu
  mutate(VDzeit05 = cut(VDzeit05,breaks=VDZbrk,include.lowest = TRUE,labels = VDZlab),
         VDzeit07 = cut(VDzeit07,breaks=VDZbrk,include.lowest = TRUE,labels = VDZlab),
         VDzeit10 = cut(VDzeit10,breaks=VDZbrk,include.lowest = TRUE,labels = VDZlab)
  )  #%>%

# Namen des Datensatz festschreiben
namesdfVDZall<-names(dfVDZall)


#  full_join(dfVDZcol,by = c("VDzeit05" = "VDZlab")) %>% # full_join, um wirklich alle Kategorien in Datensatz zu haben
#  rename(VDZcol05 = VDZcol) %>%
#  full_join(dfVDZcol,by = c("VDzeit07" = "VDZlab")) %>% # full_join, um wirklich alle Kategorien in Datensatz zu haben
#  rename(VDZcol07 = VDZcol) %>%
#  full_join(dfVDZcol,by = c("VDzeit10" = "VDZlab")) %>% # full_join, um wirklich alle Kategorien in Datensatz zu haben
#  rename(VDZcol10 = VDZcol) 

# Geometrie der Landkreise mit merge für County
geo_lk<- ger_lk %>% 
  mutate(NAME_1 = as.character(NAME_1),
        NAME_2 = as.character(NAME_2)) %>% 
  left_join(LKcol)

print("Daten VDZ erstellt")

# Nam
