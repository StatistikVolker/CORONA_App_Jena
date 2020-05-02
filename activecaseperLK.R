activecaseperLK.R


#IARweight <- eventReactive(input$IARmodell,{
 
  if (input$IARmodell == "by PPW") {IARweight <- c(rep(1,7),rep(0.810,7),rep(0.476,7),rep(0.238,7),rep(0.048,28));length(weight)} # PPW Jena
  if (input$IARmodell == "by NPW") {weight <- c(rep(1-0,7),rep(1-0.079,7),rep(1-0.416,7),rep(1-0.911,7),rep(1-0.980,28));length(weight)} # NPW Jena, 7 Tage bis first recovery
  if (input$IARmodell == "by NPW optimistic") {weight <- c(rep(1-0,5),rep(1-0.079,7),rep(1-0.416,7),rep(1-0.911,7),rep(1-0.980,28));length(weight)} # NPW Jena, 5 Tage bis first recovery
  return(weight)
  
#})


# ----------------------------------------------------------------------------------------------------
# Daten IAR-Plot (dfLKstatus())
# ----------------------------------------------------------------------------------------------------
#dfLKstatus <- reactive({
  teller <- 0
  for (LK in unique(dftsI$county)){#[54:55]) {
    #LK <- unique(dftsI$county)[54]
    teller <- teller + 1
    print(paste(teller,"-",LK))
          
  df <- dftsI  %>% 
    filter(county == LK)  #%>%
    #filter(county == "SK Jena")  %>%
  if (df$infected[df$Datum == "2020-02-15"]==0) {df <- df %>% filter(Datum >= "2020-02-15")}
  if (df$infected[df$Datum == "2020-03-01"]==0) {df <- df %>% filter(Datum >= "2020-03-01")}
    df <-df %>%
      mutate(id = row_number()) %>% 
      rename(i = infected)
  
  #wj = length(IARweight())
  wj = length(IARweight)
  # berechne active cases
  for (k in df$id){
    #  print(k)
    #if (k + wj < dim(df)[1])   {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * IARweight()),rep(df$i[k] * 0,dim(df)[1]-(k+wj)))}
    #if (k + wj == dim(df)[1])  {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * IARweight()))}
    #if (k + wj > dim(df)[1] & k < dim(df)[1])   {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * IARweight()[1:(dim(df)[1]-k)]))}
    #if (k == dim(df)[1])   {df[paste0("col",k)] <- rep(0,k)}
    if (k + wj < dim(df)[1])   {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * IARweight),rep(df$i[k] * 0,dim(df)[1]-(k+wj)))}
    if (k + wj == dim(df)[1])  {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * IARweight))}
    if (k + wj > dim(df)[1] & k < dim(df)[1])   {df[paste0("col",k)] <- c(rep(0,k),floor(df$i[k] * IARweight[1:(dim(df)[1]-k)]))}
    if (k == dim(df)[1])   {df[paste0("col",k)] <- rep(0,k)}
  }
  
  # Datensatz mit infected, actve und recovered Zahlen
  df<-df %>% mutate(a = select(., contains("col")) %>% rowSums() + i) %>%
    mutate(i = cumsum(i),
           r = i-a,
           county = LK) %>%
    #select(Datum, i,a,r) %>%
    #pivot_longer(-Datum,names_to = "status",values_to = "nsum") %>%
    #mutate(status = factor(status,levels = c("i","a","r")),
    #       county = LK) %>%
    select(county,Datum,i,a,r) %>%
    filter(Datum >= today()-10)
  #print(dfplot)
  if (teller == 1){
    dfLK <- df
    } else {
    dfLK <- bind_rows(dfLK,df)
    }
  }
  
#})
  
  # Datumsabfrage
  dat <- "2020-04-30"
  # Abfrage Status
  status <- "a"
  dfplot <- dfLK %>% filter(Datum  == dat) %>% mutate(rp = r/i)
  
  geo_lk<- ger_lk %>% 
    mutate(NAME_1 = as.character(NAME_1),
           NAME_2 = as.character(NAME_2)) %>% 
    left_join(LKcol)
  
  
  #Datensatz mit Geographie für Plot
  dfgeo <-   merge(geo_lk,dfplot) #%>%
    #filter(datum == input$vdzdatum)  %>%
    #full_join(dfVDZcol)
  names(dfgeo)
  # Deutschlandkarte
  # Anteil recovered (bezogen auf jemals infected)
  ggplot()+
    geom_sf(data = dfgeo,aes(fill = rp)) +
    scale_fill_gradient2(low = 'red',mid = "orange", high = 'yellowgreen',midpoint = 0.5) +
    #scale_fill_manual(values = VDZcol) +
    theme_void() +
    theme(legend.text = element_text(size = 12),
          title = element_text(size = 10,face = "bold"),
          plot.caption = element_text(hjust=0, size=rel(1.2))
    ) #+
    #labs(caption = footnote(),
    #     fill = "Verdopplungszeit\nin den SK und LK\nDeutschlands"
    #)
  # A

  #  Active
  median(dfgeo$a)
  ggplot()+
    geom_sf(data = dfgeo,aes(fill = a)) +
    scale_fill_gradient2(low = 'green',mid = "orange", high = 'darkred',midpoint = 500) +
    #scale_fill_manual(values = VDZcol) +
    theme_void() +
    theme(legend.text = element_text(size = 12),
          title = element_text(size = 10,face = "bold"),
          plot.caption = element_text(hjust=0, size=rel(1.2))
    ) #+
names(dfgeo)
  mina <- min(dfgeo$a)  mina
  maxa <- max(dfgeo$a); maxa
  dfgeo%>% filter(NAME_1 == "Thüringen") %>% select(county,Datum,i,a,r,rp)
  
  
  