## ---------------------------
##
## Script name: functions.R
##
## Purpose of script: Hold a bunch of functions for coronaRisk app
##
## Author: Ben Phillips
##
## Date Created: 2020-03-12
##
## Email: phillipsb@unimelb.edu.au
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------
## load up the packages we will need 

## ---------------------------

## function definitions


# calculates doubling time over the last inWindow days.
doubTime <- function(cases, time, inWindow = 10){
  r <- projSimpleSlope(cases, time, inWindow)[2]
  if_else(round(r,10) > 0, log(2)/r,999)

}


# growth rate
growthRate <- function(cases, inWindow=10){
  nn <- length(cases)
  ss <- (nn - inWindow + 1):nn
  rate <- numeric(length(ss))
  rate[ss] <- 100 * (cases[ss] - cases[ss-1]) / cases[ss-1]
}


# aggregates results to country
countryAgg<-function(x){
  #x<-tsA
  #x$Contry.Region
  xSelect<-x[, dCols]
  aggregate(xSelect, by = list(Country = x$Country.Region), FUN = sum)
}

# calculates the curve flatenning index.
  # it is the second derivative of logA wrt t (the change in growth rate) divided by first differential (the current growth rate).
cfi <- function(active){
  cfiInd <- -diff(diff(active))/abs(diff(active)[-1])#*ifelse(diff(active)[-1]>0, -1, 1)
  cfiInd[abs(cfiInd)>10]<-NA # remove crazy values associated with changed test/diagnosis
  cfiInd
}

# estimates detection rate based on assumptions about cfr, ttd
detRate<-function(infd, deaths, cfr = 0.033, ttd=17, window=5){
  obs<-c(rep(NA, window), diff(infd, window)) # observed new cases
  deathDiff<-diff(deaths, window) # observed new deaths
  expd<-deathDiff/cfr #expected new cases given cfr
  expd<-expd[-(1:(ttd-window))]
  expd<-c(expd, rep(NA, ttd))
  detRate<-obs/expd
  detRate[detRate==0]<-NA
  detRate[is.infinite(detRate)]<-NA
  out<-mean(detRate, na.rm = TRUE)
  if (is.nan(out)) return(NA)
  if (out>1) out<-1
  out
}

# Simple projection based on growth over last inWindow days
  # returns extended plotting data
projSimple<-function(rawN, rawTime, inWindow=10){
  
  #Test:
  #rawN <- yA
  #rawTime <- dates
  
  nn <- length(rawN)
  ss <- (nn-inWindow+1):nn  # ist ein s Seuenz mit start : ende
  x <- c(rawTime[ss], rawTime[nn]+1:inWindow) 
  lnN <- log(rawN[ss])
  lnN[is.infinite(lnN)]<-NA
  tIn <- rawTime[ss]
  mFit <- lm(lnN~tIn)
  extFit <- predict(mFit, newdata = list(tIn = x), interval = "confidence")
  y <- exp(extFit)
  list(x=x, y=y)
}

# Simple projection based on growth over last inWindow days


# returns coefficients
projSimpleSlope<-function(rawN, rawTime, inWindow=10){
  
  #test:
  #rawN <- dfLK
  #rawTime <-dates[1:tag]
  #inWindow<-7
  

# Bereich mit Tagesgrenze in windows rechnen!  
  nn <- length(rawN);nn
  ss <- (nn-inWindow+1);ss
  if (rawN[ss]!=0){
    #x <- c(rawTime[ss], rawTime[nn]+1:inWindow);x
    lnN <- log(rawN[nn:ss]);lnN
    lnN[is.infinite(lnN)]<-NA;lnN
    tIn <- rawTime[nn:ss];tIn
    mFit <- lm(lnN~tIn);mFit
    r<-coefficients(mFit)
  } else {
    r <-c(NA,NA)
  }
  return(r)
  

  }


# to identify the date columns in ts dataframes
dateCols<-function(x){
  grepl(pattern = "\\d", x = colnames(x))
}

# To subset time series data and aggregate totals
tsSub <- function(x, subset){
  #x<- tsACountry
  #subset<-tsACountry$Country %in% "SK Jena"
  xSub<-x[subset, dateCols(x)]
  colSums(xSub)
}


# Formatiere den Datensatz in richtiges Format
# Formatiere Datensätze um
mkr.makelongcoronadata_Zeit <- function(df,cases,what="Städte"){
  df <- df %>%
    pivot_longer(-c(ID,Datum),names_to = what,values_to = cases) %>%
    pivot_wider(-ID,names_from = Datum,values_from = cases) %>%
    rename(Country.Region = 'Städte')
}

# Formatiere den Datensatz des RKI in richtiges Format - alle Subgruppen
mkr.makecoronaRKIallSubg <- function(cases,df = RKI_df){
  
  cases <- enquo(cases)  
  dfout <- df %>% 
    select(Bundesland,Country.Region=Landkreis,!!cases,Datum=Meldedatum,Altersgruppe,Geschlecht) %>%
    mutate(Datum = as.Date(format(as.Date(Datum),"%Y-%m-%d"))) %>%
    group_by(Bundesland,Country.Region,Altersgruppe,Geschlecht,Datum)%>%
    summarize(cases = sum(!!cases)) %>%
    arrange(Datum) %>%
    pivot_wider(id_cols = c(Bundesland,Country.Region,Altersgruppe,Geschlecht),names_from = Datum,values_from = cases,
                values_fill=list(cases=0)) %>%
    ungroup()
  return(dfout)
  
}

# Formatiere den Datensatz des RKI in richtiges Format - Landkreisdaten
mkr.makecoronaRKIgesLK <- function(df){
  #df<-tsIa
  dfLK <- df %>%
    select(-c(Bundesland,Altersgruppe,Geschlecht)) %>%
    group_by(Country.Region) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)  %>% 
    pivot_longer(-Country.Region,names_to = "Datum",values_to = "cases") %>%
    arrange(Country.Region,Datum) %>%
    mutate(Datum = as.Date(Datum)) %>%
    complete(Datum = seq.Date(min(Datum), max(Datum), by="day"), Country.Region) %>%
    arrange(Country.Region,Datum) %>%
    mutate(cases = ifelse(is.na(cases),0,cases)) %>%
    group_by(Country.Region) %>%
    mutate(cumsum = cumsum(cases)) %>%
    pivot_wider(-cases,names_from = Datum,values_from = cumsum) 

      
#max(dfLK$Datum)      
#names(dfLK)      
          
  return(dfLK)
}
