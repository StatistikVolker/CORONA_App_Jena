## ---------------------------
##
## Script name: 600 CORONA JENA - Reproduktionszahl.r
##
## Purpose of script: Berechnet die CReproduktionszahl der COVID-19 Patienten in Jena
##
## Author: Volker Holzendorf
##
## Date Created: 2020-04-30
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
library(EpiEstim)
library(JenaR)

#library(OurTools)
## --------------------------




dfepiweeksdays <- dfepiweeks %>% 
  #group_by(row_number()) %>%
  complete(weekkstartdat = seq.Date(min(weekkstartdat), max(max(weekkstartdat,today())), by="day")) %>% 
  #  complete(weekkstartdat = seq.Date(min(weekkstartdat), max(weekkstartdat), by="day")) %>% 
  mutate(weeknr = na.locf0(weeknr),
         daygrp = rep(1:ceiling(length(weeknr)/4),each = 4)[1:length(weeknr)]
  ) 


dfJ_I <- dfJ_corona %>% dplyr::select(FALL,datI) %>% group_by(datI) %>% count() %>% rename(i = n)


Ipday <- dfepiweeksdays %>% rename(datI = weekkstartdat) %>% left_join(dfJ_I) %>% 
  mutate(i = if_else(is.na(i),"0",as.character(i)),
         i = as.numeric(i),
         ip = i/sum(i)) %>%
  filter(datI > "2020-03-10")

dim(Ipday)

# Reproduktionszahl für 4 Tage Intervalle
T <- length(Ipday$i)
t_start <- seq(2, T-4) # starting at 2 as conditional on the past observations
t_end <- t_start + 4 # adding 6 to get 7-day windows as bounds included in window

res_parametric_si <- estimate_R(Ipday$i, 
                                method="parametric_si",
                                config = make_config(list(
                                  t_start = t_start,
                                  t_end = t_end,
                                  mean_si = 10, 
                                  std_si = 4))
)

res_parametric_si$R
  
p<-plot(res_parametric_si, legend = FALSE,what = "incid", options_I = list(col = "darkblue"))
p

p + theme_minimal() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25,face = "bold")
        
        ) + 
  labs(y = "Anzahl Neuerkrankte pro Tag",
       title = "COVID-19 in Jena",
       x = "Tage seit erstem Fall")
mkr.saveplotCORONA("601 COVID-19 in Jena pro Tag",datendat)
print("601 COVID-19 in Jena pro Tag - done")


p<-plot(res_parametric_si, legend = FALSE,what = "R", options_R = list(col = "darkred",size = 2))

p + theme_minimal() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25,face = "bold")
  ) + 
  labs(y = "Reproduktionszahl, geschätzt",
       title = "Reproduktionszahl in Jena",
       x = "Tage seit erstem Fall")
mkr.saveplotCORONA("602 Kurve der Reproduktionszahl",datendat)
print("602 Kurve der Reproduktionszahl - done")


res_parametric_si$R
