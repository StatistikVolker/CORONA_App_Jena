
testLK <- c("LK Rems-Murr-Kreis","SK Erlangen","SK Lübeck","SK Braunschweig","SK Bremerhaven","LK Heinsberg","SK Kassel",
            "SK Ludwigshafen","LK Sankt Wendel","SK Potsdam","LK Rostock","SK Leipzig","SK Halle","SK Jena")
length(testLK)

input <- testLK[c(1,7,14)];

dates
names(pDat)
names(tsE)
  
pDat <- tsACountry %>% filter(Country %in% input) #%>% left_join(tsE)
dTime <- as.matrix(doubTime(pDat,dates));dTime
dTime[!is.finite(dTime)]<-NA
clrs<-pDat$plotcol
dates10 <- dates[(length(pDat)-10+1):length(pDat)]
counts <- table(dTime)
barplot(dTime,
        main="Doubling time",
        xlab="Date", 
        ylab="Doubling time (days)",
        beside=TRUE,
        col = clrs,
        legend = input$countryGrowthRate,
        args.legend = list(bty = "n", x = "topleft"))


RKI_JSON$features$type


length(ddReg)


pDat <- tsSub(tsACountry, tsACountry$Country %in% input)
x<-tsACountry
subset<- tsACountry$Country  %in% input
xSub<-x[subset, dateCols(x)]
colSums(xSub)


dTime <- round(doubTime(pDat, dates), 1)
